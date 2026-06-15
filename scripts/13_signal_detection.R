## ============================================================
## PREIS Ebola RDC — 13. DETECTION DE SIGNAUX D'ALERTE PRECOCE
## ------------------------------------------------------------
## Objectif : repérer automatiquement, à chaque nouveau SitRep,
## des signaux épidémiologiques qui pourraient échapper à une
## lecture manuelle périodique.
##
## PRINCIPE METHODOLOGIQUE (important) :
##   - Le système SIGNALE des faits et propose des HYPOTHESES à
##     investiguer. Il ne pose PAS de diagnostic et n'émet PAS de
##     recommandation clinique/opérationnelle automatique.
##   - Tous les seuils sont EXPLICITES et documentés (pas de boîte
##     noire). Ils sont calibrés pour Ebola mais ajustables.
##   - Toute la létalité est PROVISOIRE.
##
## Entrée  : data/final/PREIS_daily_indicators.csv (produit par 11)
## Sortie  : data/final/PREIS_signals.csv  (un signal = une ligne)
##           + un texte prêt à insérer dans l'email d'alerte (04)
## ============================================================

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(lubridate)
}))

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
FINAL_DIR <- file.path(BASE_DIR, "data", "final")
IN_FP  <- file.path(FINAL_DIR, "PREIS_daily_indicators.csv")
OUT_FP <- file.path(FINAL_DIR, "PREIS_signals.csv")
TXT_FP <- file.path(FINAL_DIR, "PREIS_signals_text.txt")

# ------------------------------------------------------------
# SEUILS EXPLICITES (documentés, ajustables)
# ------------------------------------------------------------
TH <- list(
  min_cases_cfr      = 10,   # CFR fiable seulement si >= 10 cas cumulés
  cfr_jump_pts       = 15,   # hausse de létalité (points de %) sur la fenêtre
  cfr_window_days    = 7,    # fenêtre d'observation de la létalité
  accel_ratio        = 2.0,  # accélération : ma7 récent >= 2x ma7 précédent
  accel_min_new      = 5,    # ... et au moins 5 nouveaux cas/j récents
  emerging_zero_days = 7,    # zone "calme" = 0 nouveau cas pendant >= 7 j
  emerging_min_burst = 5,    # ... puis >= 5 nouveaux cas (réémergence)
  high_cfr_abs       = 50,   # létalité absolue élevée (%) à surveiller
  silence_days       = 4     # délai inhabituel entre deux points de données
)

log_sig <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "| [signaux]", ..., "\n")

if (!file.exists(IN_FP)) {
  log_sig("Indicateurs journaliers absents — détection sautée:", IN_FP)
  quit(save = "no", status = 0)
}

d <- readr::read_csv(IN_FP, show_col_types = FALSE)
need <- c("level","province","zone","date","cum_cases","cum_deaths",
          "new_cases","new_deaths","cfr")
if (!all(need %in% names(d))) {
  log_sig("Colonnes attendues manquantes — détection sautée.")
  quit(save = "no", status = 0)
}
d$date <- as.Date(d$date)
if (!"ma7_new_cases" %in% names(d)) d$ma7_new_cases <- NA_real_
if (!"revision" %in% names(d))
  d$revision <- !is.na(d$new_cases) & d$new_cases < 0

signals <- list()
add_signal <- function(type, level, zone, province, date, severity, detail, hypotheses) {
  signals[[length(signals) + 1]] <<- tibble(
    detected_on = Sys.Date(), type = type, level = level,
    zone = zone %||% NA_character_, province = province %||% NA_character_,
    date = date, severity = severity, detail = detail, hypotheses = hypotheses
  )
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ------------------------------------------------------------
# SIGNAL 1 — Hausse rapide de la létalité (par zone)
#   La CFR provisoire d'une zone bondit de >= cfr_jump_pts points
#   sur cfr_window_days jours (zones suffisamment grandes).
# ------------------------------------------------------------
zone_d <- d %>% filter(level == "Zone") %>% arrange(zone, date)
for (z in unique(zone_d$zone)) {
  zz <- zone_d %>% filter(zone == z, cum_cases >= TH$min_cases_cfr)
  if (nrow(zz) < 2) next
  last <- tail(zz, 1)
  past <- zz %>% filter(date <= last$date - TH$cfr_window_days)
  if (nrow(past) == 0) next
  ref <- tail(past, 1)
  jump <- last$cfr - ref$cfr
  if (!is.na(jump) && jump >= TH$cfr_jump_pts) {
    add_signal("Rising lethality", "Zone", z, last$province, last$date,
      severity = if (jump >= 25) "high" else "moderate",
      detail = sprintf("%s provisional lethality: %.1f%% \u2192 %.1f%% (+%.1f pts in ~%d days, %d cumulative cases)",
                       z, ref$cfr, last$cfr, jump, TH$cfr_window_days, last$cum_cases),
      hypotheses = "Late detection or care? Under-reporting of cases (underestimated denominator)? Difficult access to care? To investigate.")
  }
}

# ------------------------------------------------------------
# SIGNAL 2 — Accélération localisée (par zone)
#   La moyenne mobile 7j récente est >= accel_ratio fois la
#   précédente, avec un minimum de cas pour éviter le bruit.
# ------------------------------------------------------------
for (z in unique(zone_d$zone)) {
  zz <- zone_d %>% filter(zone == z) %>% arrange(date)
  if (nrow(zz) < 9) next
  last <- tail(zz, 1)
  prev <- zz %>% filter(date <= last$date - 7) %>% tail(1)
  if (nrow(prev) == 0) next
  r_now <- last$ma7_new_cases; r_old <- prev$ma7_new_cases
  if (!is.na(r_now) && !is.na(r_old) && r_old > 0 &&
      r_now >= TH$accel_min_new && r_now / r_old >= TH$accel_ratio) {
    add_signal("Localized acceleration", "Zone", z, last$province, last$date,
      severity = if (r_now / r_old >= 3) "high" else "moderate",
      detail = sprintf("%s: 7-day average of new cases %.1f \u2192 %.1f/day (x%.1f)",
                       z, r_old, r_now, r_now / r_old),
      hypotheses = "Active uncontrolled transmission chain? New super-spreading event (funerals, care)? Strengthen contact tracing. To investigate.")
  }
}

# ------------------------------------------------------------
# SIGNAL 3 — Réémergence dans une zone redevenue calme
#   Zone avec 0 nouveau cas pendant >= emerging_zero_days,
#   puis un sursaut >= emerging_min_burst.
# ------------------------------------------------------------
for (z in unique(zone_d$zone)) {
  zz <- zone_d %>% filter(zone == z) %>% arrange(date)
  if (nrow(zz) < TH$emerging_zero_days + 1) next
  last <- tail(zz, 1)
  window_before <- zz %>% filter(date < last$date,
                                 date >= last$date - TH$emerging_zero_days)
  if (nrow(window_before) == 0) next
  if (all(replace(window_before$new_cases, is.na(window_before$new_cases), 0) == 0) &&
      !is.na(last$new_cases) && last$new_cases >= TH$emerging_min_burst) {
    add_signal("Re-emergence", "Zone", z, last$province, last$date,
      severity = "high",
      detail = sprintf("%s: %d quiet day(s) then %d new case(s) on %s",
                       z, TH$emerging_zero_days, last$new_cases,
                       format(last$date, "%d/%m")),
      hypotheses = "New introduction from a neighbouring zone? Unidentified reservoir/source case? Check epidemiological links. To investigate.")
  }
}

# ------------------------------------------------------------
# SIGNAL 4 — Létalité absolue élevée (zones fiables)
# ------------------------------------------------------------
zlast <- zone_d %>% group_by(zone) %>% slice_tail(n = 1) %>% ungroup()
for (i in seq_len(nrow(zlast))) {
  r <- zlast[i, ]
  if (!is.na(r$cfr) && r$cum_cases >= TH$min_cases_cfr && r$cfr >= TH$high_cfr_abs) {
    add_signal("High lethality", "Zone", r$zone, r$province, r$date,
      severity = if (r$cfr >= 70) "high" else "moderate",
      detail = sprintf("%s: provisional lethality %.1f%% (%d cases)", r$zone, r$cfr, r$cum_cases),
      hypotheses = "Late or insufficient care? Particular severity? Under-detection of mild cases (biased denominator)? To investigate.")
  }
}

# ------------------------------------------------------------
# SIGNAL 5 — Révisions à la baisse de l'INRB (transparence)
#   On signale (sans alarmisme) les reclassifications récentes.
# ------------------------------------------------------------
rev_recent <- d %>% filter(level == "National", isTRUE(revision) | revision == TRUE) %>%
  arrange(desc(date)) %>% head(1)
if (nrow(rev_recent) == 1) {
  add_signal("Data revision", "National", NA, NA, rev_recent$date,
    severity = "info",
    detail = sprintf("Downward revision of the national cumulative total on %s (INRB reclassification).",
                     format(rev_recent$date, "%d/%m")),
    hypotheses = "Case harmonisation/reclassification by INRB - normal behaviour, flagged for transparency (trends should be read accounting for these adjustments).")
}

# ------------------------------------------------------------
# SIGNAL 6 — Délai inhabituel entre points de données (national)
# ------------------------------------------------------------
nat <- d %>% filter(level == "National") %>% arrange(date)
if (nrow(nat) >= 2) {
  gap <- as.integer(difftime(max(nat$date), nat$date[nrow(nat) - 1], units = "days"))
  if (!is.na(gap) && gap >= TH$silence_days) {
    add_signal("Data silence", "National", NA, NA, max(nat$date),
      severity = "info",
      detail = sprintf("Gap of %d days since the previous data point.", gap),
      hypotheses = "Delayed SitRep publication? Difficulties in field data reporting? To verify with the source.")
  }
}

# ------------------------------------------------------------
# CONSOLIDATION + SORTIES
# ------------------------------------------------------------
if (length(signals) == 0) {
  log_sig("No signal detected above thresholds.")
  readr::write_csv(tibble(detected_on = Sys.Date(), type = "No signal",
                          level = NA, zone = NA, province = NA, date = Sys.Date(),
                          severity = "info",
                          detail = "No signal above defined thresholds.",
                          hypotheses = NA), OUT_FP)
  writeLines("No alert signal above the defined thresholds for this SitRep.", TXT_FP)
  quit(save = "no", status = 0)
}

sig_df <- bind_rows(signals)
# Presentation order: high severity first
sev_rank <- c("high" = 1, "moderate" = 2, "info" = 3)
sig_df <- sig_df %>% mutate(rk = sev_rank[severity]) %>%
  arrange(rk, type) %>% select(-rk)

readr::write_csv(sig_df, OUT_FP)
log_sig(sprintf("%d signal(s) detected -> %s", nrow(sig_df), basename(OUT_FP)))

# Text ready for the alert email (04)
lines <- c("SIGNALS TO INVESTIGATE (automated detection, explicit thresholds)",
           "The system reports facts and proposes hypotheses; it does not make",
           "a diagnosis. All lethality is provisional.", "")
for (i in seq_len(nrow(sig_df))) {
  s <- sig_df[i, ]
  lines <- c(lines,
    sprintf("[%s | %s severity] %s", toupper(s$type), s$severity, s$detail),
    sprintf("   Hypotheses: %s", s$hypotheses), "")
}
writeLines(enc2utf8(lines), TXT_FP)
log_sig("Signal alert text written ->", basename(TXT_FP))
