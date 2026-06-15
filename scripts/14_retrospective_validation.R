## ============================================================
## PREIS Ebola RDC — VALIDATION RETROSPECTIVE de la detection de signaux
## ------------------------------------------------------------
## Objectif scientifique (pour l'article) :
##   Rejouer, jour apres jour, la detection de signaux comme si le
##   systeme tournait en temps reel, en n'utilisant QUE les donnees
##   disponibles jusqu'a chaque date (pas de fuite du futur).
##
##   Produit :
##    1) Un journal des signaux detectes avec leur DATE de premiere
##       detection (=> permet de mesurer la precocite).
##    2) Une comparaison "signal automatique" vs "ce qu'une lecture
##       manuelle periodique (tous les N jours) aurait vu".
##    3) Des tableaux/figures prets pour l'article.
##
## METHODE (defendable en revue) :
##   - Fenetre glissante : a la date t, on ne connait que les SitReps <= t.
##   - Memes seuils explicites que le module de production (13_).
##   - Pas de projection, pas de causalite : signaux + hypotheses.
##
## Entree : data INRB (national + par zone), cumlues par date.
## Sortie : outputs/validation/ (CSV + figures PNG).
## ============================================================

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(lubridate); library(ggplot2)
}))

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
# Source des donnees : depot INRB (local si present, sinon URL brute)
RAW_BASE <- paste0("https://raw.githubusercontent.com/INRB-UMIE/",
                   "BDBV2026-Data/main/data/insp_sitrep/processed/")
OUT_DIR  <- file.path(BASE_DIR, "outputs", "validation")
FINAL_DIR <- file.path(BASE_DIR, "data", "final")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FINAL_DIR, recursive = TRUE, showWarnings = FALSE)

AU_GREEN <- "#00843E"; AU_RED <- "#E31C23"; AU_GOLD <- "#F0B323"

# Seuils EXACTEMENT identiques au module de production 13_
TH <- list(min_cases_cfr = 10, cfr_jump_pts = 15, cfr_window = 7,
           accel_ratio = 2.0, accel_min_new = 5,
           emerging_zero_days = 7, emerging_min_burst = 5,
           high_cfr_abs = 50)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

local_or_url <- function(name) {
  lp <- file.path(BASE_DIR, "data", "raw", name)
  if (file.exists(lp)) lp else paste0(RAW_BASE, name)
}
read_ind <- function(name, valcol) {
  d <- tryCatch(readr::read_csv(local_or_url(name), show_col_types = FALSE),
                error = function(e) NULL)
  if (is.null(d)) return(NULL)
  names(d)[names(d) == valcol] <- "value"
  d$date <- as.Date(d$date)
  d$value <- suppressWarnings(as.numeric(d$value))
  d[!is.na(d$value), ]
}

msg("Chargement des donnees INRB (national + zones)...")
nat_cas <- read_ind("insp_sitrep__national_cumulative_confirmed_cases__daily.csv",
                    "national_cumulative_confirmed_cases")
nat_dec <- read_ind("insp_sitrep__national_cumulative_confirmed_deaths__daily.csv",
                    "national_cumulative_confirmed_deaths")
z_cas <- read_ind("insp_sitrep__cumulative_confirmed_cases__daily.csv",
                  "cumulative_confirmed_cases")
z_dec <- read_ind("insp_sitrep__cumulative_confirmed_deaths__daily.csv",
                  "cumulative_confirmed_deaths")

if (is.null(z_cas) || is.null(z_dec)) {
  msg("ERREUR : donnees par zone indisponibles. Validation interrompue.")
  quit(save = "no", status = 1)
}

# Harmonisation de quelques noms de zones (coherence INRB)
harm <- function(x) {
  x <- gsub("Mongbalu", "Mongbwalu", x)
  x <- gsub("^Gethy$", "Gety", x)
  x
}
z_cas$nom <- harm(z_cas$nom); z_dec$nom <- harm(z_dec$nom)

# Table zone-date : cas + deces + cfr + nouveaux + ma7
zone <- z_cas %>% select(zone = nom, date, cum_cases = value) %>%
  left_join(z_dec %>% select(zone = nom, date, cum_deaths = value),
            by = c("zone", "date")) %>%
  mutate(cum_deaths = ifelse(is.na(cum_deaths), 0, cum_deaths)) %>%
  arrange(zone, date) %>% group_by(zone) %>%
  mutate(new_cases = cum_cases - dplyr::lag(cum_cases),
         cfr = ifelse(cum_cases > 0, round(100 * cum_deaths / cum_cases, 1), NA)) %>%
  ungroup()

all_dates <- sort(unique(zone$date))
msg(sprintf("Periode : %s -> %s (%d dates).",
            min(all_dates), max(all_dates), length(all_dates)))

# ------------------------------------------------------------
# Fonction : detecter les signaux a une date t (donnees <= t seulement)
# ------------------------------------------------------------
detect_at <- function(t) {
  zt <- zone %>% filter(date <= t)
  if (nrow(zt) == 0) return(NULL)
  out <- list()
  # Helper : vrai seulement si x est une valeur unique non-NA satisfaisant cond
  ok_num <- function(x) length(x) == 1 && !is.na(x)
  for (z in unique(zt$zone)) {
    zz <- zt %>% filter(zone == z) %>% arrange(date)
    if (nrow(zz) == 0) next
    last <- tail(zz, 1)
    last_cfr <- last$cfr[1]; last_cases <- last$cum_cases[1]
    # ma7 a la date t
    recent <- zz %>% filter(date > t - 7)
    ma7_now <- if (nrow(recent)) mean(pmax(recent$new_cases, 0), na.rm = TRUE) else NA
    prev7 <- zz %>% filter(date <= t - 7, date > t - 14)
    ma7_old <- if (nrow(prev7)) mean(pmax(prev7$new_cases, 0), na.rm = TRUE) else NA

    # S1 hausse letalite
    if (ok_num(last_cfr) && ok_num(last_cases) && last_cases >= TH$min_cases_cfr) {
      ref <- zz %>% filter(date <= t - TH$cfr_window) %>% tail(1)
      ref_cfr <- if (nrow(ref)) ref$cfr[1] else NA
      if (ok_num(ref_cfr)) {
        jump <- last_cfr - ref_cfr
        if (ok_num(jump) && jump >= TH$cfr_jump_pts)
          out[[length(out)+1]] <- data.frame(date=t, zone=z, type="Hausse letalite",
            value=round(jump,1), detail=sprintf("CFR %.1f->%.1f", ref_cfr, last_cfr))
      }
    }
    # S2 acceleration
    if (ok_num(ma7_now) && ok_num(ma7_old) && ma7_old > 0 &&
        ma7_now >= TH$accel_min_new && ma7_now/ma7_old >= TH$accel_ratio)
      out[[length(out)+1]] <- data.frame(date=t, zone=z, type="Acceleration",
        value=round(ma7_now/ma7_old,1), detail=sprintf("ma7 %.1f->%.1f", ma7_old, ma7_now))
    # S4 letalite absolue elevee
    if (ok_num(last_cfr) && ok_num(last_cases) && last_cases >= TH$min_cases_cfr &&
        last_cfr >= TH$high_cfr_abs)
      out[[length(out)+1]] <- data.frame(date=t, zone=z, type="Letalite elevee",
        value=last_cfr, detail=sprintf("%d cas", as.integer(last_cases)))
  }
  if (length(out)) do.call(rbind, out) else NULL
}

# ------------------------------------------------------------
# Rejouer sur toutes les dates -> journal des signaux
# ------------------------------------------------------------
msg("Rejeu jour par jour (simulation temps reel)...")
all_signals <- list()
for (t in all_dates) {
  s <- detect_at(as.Date(t, origin = "1970-01-01"))
  if (!is.null(s)) all_signals[[length(all_signals)+1]] <- s
}
sig <- if (length(all_signals)) do.call(rbind, all_signals) else
  data.frame(date=as.Date(character()), zone=character(), type=character(),
             value=numeric(), detail=character())

# PREMIERE detection de chaque (zone,type) = date de signal precoce
first_detect <- sig %>% group_by(zone, type) %>%
  summarise(first_date = min(date), value_at_first = first(value),
            detail = first(detail), .groups = "drop") %>%
  arrange(first_date)

readr::write_csv(sig, file.path(OUT_DIR, "validation_all_signals.csv"))
readr::write_csv(first_detect, file.path(OUT_DIR, "validation_first_detection.csv"))
# Copie vers data/final pour l'onglet dashboard
readr::write_csv(first_detect, file.path(FINAL_DIR, "PREIS_validation_signals.csv"))
msg(sprintf("Signaux : %d occurrences, %d signaux distincts (zone x type).",
            nrow(sig), nrow(first_detect)))

# ------------------------------------------------------------
# Comparaison precocite : detection quotidienne vs lecture manuelle
# tous les N jours (ex. lecture humaine tous les 3 jours)
# ------------------------------------------------------------
manual_every <- 3
manual_dates <- all_dates[seq(1, length(all_dates), by = manual_every)]
precocity <- first_detect %>%
  rowwise() %>%
  mutate(manual_date = {
    later <- manual_dates[manual_dates >= first_date]
    if (length(later)) min(later) else NA
  }) %>%
  ungroup() %>%
  mutate(days_gained = as.integer(manual_date - first_date))
readr::write_csv(precocity, file.path(OUT_DIR, "validation_precocity.csv"))
mean_gain <- round(mean(precocity$days_gained, na.rm = TRUE), 1)
msg(sprintf("Gain de precocite moyen vs lecture tous les %d j : %.1f jour(s).",
            manual_every, mean_gain))

# ------------------------------------------------------------
# FIGURE 1 : chronologie des premieres detections (timeline)
# ------------------------------------------------------------
if (nrow(first_detect) > 0) {
  fd <- first_detect %>% mutate(lab = paste0(zone, " (", type, ")"))
  pal <- c("Hausse letalite" = AU_GOLD, "Acceleration" = AU_RED,
           "Letalite elevee" = AU_GREEN)
  g1 <- ggplot(fd, aes(x = first_date, y = reorder(lab, first_date), color = type)) +
    geom_segment(aes(x = min(all_dates), xend = first_date,
                     yend = reorder(lab, first_date)),
                 color = "grey85", linewidth = 0.4) +
    geom_point(size = 3) +
    scale_color_manual(values = pal, name = "Type de signal") +
    labs(title = "Chronologie des premières détections automatiques de signaux",
         subtitle = "Validation rétrospective — épidémie Ebola RDC 2026 (données INRB)",
         x = "Date de première détection", y = NULL,
         caption = "Létalité provisoire. Signaux à seuils explicites ; hypothèses à investiguer.") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  ggsave(file.path(OUT_DIR, "FIG_validation_timeline.png"), g1,
         width = 9, height = 6, dpi = 300)
  msg("Figure timeline ecrite.")
}

# ------------------------------------------------------------
# TABLEAU recapitulatif pour l'article
# ------------------------------------------------------------
summary_tab <- first_detect %>%
  count(type, name = "n_signaux") %>%
  arrange(desc(n_signaux))
readr::write_csv(summary_tab, file.path(OUT_DIR, "validation_summary_by_type.csv"))

msg("=== VALIDATION TERMINEE ===")
msg("Sorties dans :", OUT_DIR)
msg("  - validation_all_signals.csv      (toutes les detections)")
msg("  - validation_first_detection.csv  (premiere detection = precocite)")
msg("  - validation_precocity.csv        (gain vs lecture manuelle)")
msg("  - validation_summary_by_type.csv  (recap par type)")
msg("  - FIG_validation_timeline.png     (figure article)")
