# ============================================================
# 15_bed_occupancy_analysis_v2.R
#
# PREIS Ebola DRC — Taux d'Occupation des Lits (CTE/CT/CI)
#
# Auteur  : Dr R. Hyacinthe ZABRE — PREIS / Africa CDC
# Version : 2.1 — Juin 2026
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(tibble); library(ggplot2); library(scales)
  library(lubridate)
})

# ============================================================
# 0. CONFIGURATION
# ============================================================
BASE_DIR   <- Sys.getenv("PREIS_ROOT", unset = getwd())
DATA_FINAL <- file.path(BASE_DIR, "data", "final")
OUT_DIR    <- file.path(BASE_DIR, "outputs", "analyse")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

GEN_DATE <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Règle de date : SitRep 14 = 28 mai 2026, +1 j/SitRep
SNO_ANCHOR  <- 14L
DATE_ANCHOR <- as.Date("2026-05-28")
sitrep_date <- function(sno) DATE_ANCHOR + (as.integer(sno) - SNO_ANCHOR)

SEUIL_TENSION    <- 60
SEUIL_SATURATION <- 80
SEUIL_CRITIQUE   <- 100

# ============================================================
# 1. DONNÉES INTÉGRÉES (SitReps 31-38)
# ============================================================

# ---- 1a. National ------------------------------------------
nat_raw <- tibble::tribble(
  ~sitrep_no, ~lits_disponibles, ~patients_isoles, ~taux_occ_pct,
  31L,  390L,  295L,  NA_real_,
  32L,  390L,  308L,  NA_real_,
  33L,  400L,  320L,  NA_real_,
  34L,  400L,  335L,  NA_real_,
  35L,  410L,  342L,  NA_real_,
  36L,  420L,  350L,  NA_real_,
  37L,  430L,  358L,  NA_real_,
  38L,  441L,  371L,  84.1
) %>%
  dplyr::mutate(
    date     = sitrep_date(sitrep_no),
    level    = "National",
    province = NA_character_,
    zone     = NA_character_,
    taux_occ_pct = dplyr::case_when(
      !is.na(taux_occ_pct) ~ taux_occ_pct,
      lits_disponibles > 0 ~ 100 * patients_isoles / lits_disponibles,
      TRUE ~ NA_real_
    )
  )

# ---- 1b. Provinces SitRep 38 (Tableau 6, COUSP) -----------
prov_sr38 <- tibble::tribble(
  ~province,    ~lits_disponibles, ~patients_isoles, ~taux_occ_pct,
  "Ituri",       338L,              298L,              88.2,
  "Nord-Kivu",    71L,               63L,              88.7,
  "Sud-Kivu",     32L,               10L,              31.3
) %>%
  dplyr::mutate(
    sitrep_no = 38L,
    date      = sitrep_date(38L),
    level     = "Province",
    zone      = NA_character_
  )

# ---- 1c. Provinces SitReps antérieurs ----------------------
prov_hist <- tibble::tribble(
  ~sitrep_no, ~province,    ~lits_disponibles, ~patients_isoles,
  37L, "Ituri",     330L, 285L,
  37L, "Nord-Kivu",  68L,  58L,
  37L, "Sud-Kivu",   32L,  15L,
  36L, "Ituri",     320L, 268L,
  36L, "Nord-Kivu",  65L,  52L,
  36L, "Sud-Kivu",   32L,  30L,
  35L, "Ituri",     310L, 250L,
  35L, "Nord-Kivu",  62L,  48L,
  35L, "Sud-Kivu",   32L,  44L,
  34L, "Ituri",     305L, 240L,
  34L, "Nord-Kivu",  60L,  42L,
  34L, "Sud-Kivu",   32L,  53L,
  33L, "Ituri",     295L, 225L,
  33L, "Nord-Kivu",  58L,  38L,
  33L, "Sud-Kivu",   32L,  57L,
  32L, "Ituri",     285L, 210L,
  32L, "Nord-Kivu",  55L,  35L,
  32L, "Sud-Kivu",   32L,  63L,
  31L, "Ituri",     275L, 195L,
  31L, "Nord-Kivu",  52L,  30L,
  31L, "Sud-Kivu",   32L,  70L
) %>%
  dplyr::mutate(
    date         = sitrep_date(sitrep_no),
    level        = "Province",
    zone         = NA_character_,
    taux_occ_pct = 100 * patients_isoles / lits_disponibles
  )

# ---- 1d. Zones de santé SitRep 38 -------------------------
zone_sr38 <- tibble::tribble(
  ~province,    ~zone,           ~lits_disponibles, ~patients_isoles,
  "Ituri",      "Bunia",          120L,  108L,
  "Ituri",      "Rwampara",        80L,   72L,
  "Ituri",      "Mongbwalu",       70L,   65L,
  "Ituri",      "Nyankunde",       35L,   28L,
  "Ituri",      "Mangala",         12L,   10L,
  "Ituri",      "Lita",            10L,    8L,
  "Ituri",      "Tchomia",          8L,    3L,
  "Nord-Kivu",  "Katwa",           28L,   27L,
  "Nord-Kivu",  "Butembo",         25L,   22L,
  "Nord-Kivu",  "Beni",            18L,   14L,
  "Sud-Kivu",   "Miti-Murhesa",    32L,   10L
) %>%
  dplyr::mutate(
    sitrep_no    = 38L,
    date         = sitrep_date(38L),
    level        = "Zone",
    taux_occ_pct = 100 * patients_isoles / lits_disponibles
  )

# Colonnes communes exportées
COLS <- c("sitrep_no","date","level","province","zone",
          "lits_disponibles","patients_isoles","taux_occ_pct")

# ============================================================
# 2. CHARGEMENT BASE LONGITUDINALE (optionnel)
# ============================================================
fp_long <- file.path(DATA_FINAL, "PREIS_indicators_long.csv")

if (file.exists(fp_long)) {
  cat(">>> Base longitudinale détectée — fusion en cours...\n")

  ind <- suppressWarnings(
    readr::read_csv(fp_long, show_col_types = FALSE)
  ) %>%
    dplyr::mutate(
      sitrep_no      = suppressWarnings(as.integer(sitrep_no)),
      value          = suppressWarnings(as.numeric(value)),
      indicator_code = tolower(trimws(as.character(indicator_code)))
    ) %>%
    dplyr::filter(!is.na(sitrep_no), !is.na(indicator_code))

  if (!"level"    %in% names(ind)) ind$level    <- "National"
  if (!"province" %in% names(ind)) ind$province <- NA_character_
  if (!"zone"     %in% names(ind)) ind$zone     <- NA_character_

  present <- unique(ind$indicator_code)

  CODES_AVAIL <- c("beds_available","lits_disponibles","bed_capacity",
                   "lits_installes","treatment_beds_available")
  CODES_OCCUP <- c("beds_occupied","patients_isoles","lits_occupes",
                   "hospitalised","hospitalized","current_inpatients",
                   "patients_admitted")
  CODES_RATE  <- c("bed_occupancy_rate","taux_occupation_lits",
                   "occupancy_rate","taux_occ")

  find_code <- function(cands) {
    h <- intersect(tolower(cands), present)
    if (length(h) == 0L) NA_character_ else h[1L]
  }
  c_avail <- find_code(CODES_AVAIL)
  c_occup <- find_code(CODES_OCCUP)
  c_rate  <- find_code(CODES_RATE)

  cat("  Lits disponibles : ", ifelse(is.na(c_avail), "NON TROUVÉ", c_avail), "\n")
  cat("  Lits occupés     : ", ifelse(is.na(c_occup), "NON TROUVÉ", c_occup), "\n")
  cat("  Taux occupation  : ", ifelse(is.na(c_rate),  "NON TROUVÉ", c_rate),  "\n")

  KEY <- c("sitrep_no","level","province","zone")

  # Extraction sécurisée : retourne un data.frame, jamais NULL
  extract_safe <- function(code, col_name) {
    if (is.na(code)) {
      return(tibble::tibble(
        sitrep_no = integer(), level = character(),
        province  = character(), zone  = character()
      ) %>% dplyr::mutate(!!col_name := numeric()))
    }
    ind %>%
      dplyr::filter(indicator_code == code) %>%
      dplyr::select(dplyr::all_of(KEY), value) %>%
      dplyr::rename(!!col_name := value)
  }

  df_avail <- extract_safe(c_avail, "lits_disponibles")
  df_occup <- extract_safe(c_occup, "patients_isoles")
  df_rate  <- extract_safe(c_rate,  "taux_occ_pct")

  # Jointures successives — jamais de NULL car extract_safe garantit un df
  db <- df_avail %>%
    dplyr::full_join(df_occup, by = KEY) %>%
    dplyr::full_join(df_rate,  by = KEY) %>%
    dplyr::mutate(
      date = sitrep_date(sitrep_no),
      taux_occ_pct = dplyr::case_when(
        !is.na(taux_occ_pct) ~ taux_occ_pct,
        !is.na(patients_isoles) & !is.na(lits_disponibles) & lits_disponibles > 0
          ~ 100 * patients_isoles / lits_disponibles,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(dplyr::any_of(COLS))

  cat("  Lignes issues de la base longit. : ", nrow(db), "\n")

  all_data <- dplyr::bind_rows(
    nat_raw   %>% dplyr::select(dplyr::all_of(COLS)),
    prov_sr38 %>% dplyr::select(dplyr::all_of(COLS)),
    prov_hist %>% dplyr::select(dplyr::all_of(COLS)),
    zone_sr38 %>% dplyr::select(dplyr::all_of(COLS)),
    db
  ) %>%
    dplyr::distinct(sitrep_no, level, province, zone, .keep_all = TRUE)

} else {
  cat(">>> Pas de base longitudinale — données intégrées utilisées.\n")
  all_data <- dplyr::bind_rows(
    nat_raw   %>% dplyr::select(dplyr::all_of(COLS)),
    prov_sr38 %>% dplyr::select(dplyr::all_of(COLS)),
    prov_hist %>% dplyr::select(dplyr::all_of(COLS)),
    zone_sr38 %>% dplyr::select(dplyr::all_of(COLS))
  )
}

all_data <- all_data %>%
  dplyr::arrange(level, province, zone, sitrep_no)

last_no   <- max(all_data$sitrep_no, na.rm = TRUE)
last_date <- sitrep_date(last_no)

cat("====================================================\n")
cat("PREIS — Taux d'occupation des lits\n")
cat("Généré le  : ", GEN_DATE, "\n", sep = "")
cat("Dernier SitRep : N°", last_no, " (", format(last_date), ")\n", sep = "")
cat("====================================================\n")

# ============================================================
# 3. SÉPARATION PAR NIVEAU
# ============================================================
df_nat  <- all_data %>% dplyr::filter(level == "National")
df_prov <- all_data %>% dplyr::filter(level == "Province", !is.na(province))
df_zone <- all_data %>% dplyr::filter(level == "Zone",     !is.na(zone))

latest_nat  <- df_nat %>% dplyr::filter(sitrep_no == last_no)

latest_prov <- df_prov %>%
  dplyr::group_by(province) %>%
  dplyr::slice_max(sitrep_no, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(taux_occ_pct))

latest_zone <- df_zone %>%
  dplyr::group_by(province, zone) %>%
  dplyr::slice_max(sitrep_no, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(taux_occ_pct))

# ============================================================
# 4. EXPORTS CSV
# ============================================================
readr::write_csv(latest_nat,  file.path(OUT_DIR, "bed_occ_latest_national.csv"))
readr::write_csv(latest_prov, file.path(OUT_DIR, "bed_occ_latest_province.csv"))
readr::write_csv(latest_zone, file.path(OUT_DIR, "bed_occ_latest_zone.csv"))
readr::write_csv(df_nat,      file.path(OUT_DIR, "bed_occ_timeseries_national.csv"))
readr::write_csv(df_prov,     file.path(OUT_DIR, "bed_occ_timeseries_province.csv"))
readr::write_csv(df_zone,     file.path(OUT_DIR, "bed_occ_timeseries_zone.csv"))
cat(">>> CSV exportés.\n")

# ============================================================
# 5. THÈME & HELPERS GRAPHIQUES
# ============================================================
couleurs_province <- c(
  "Ituri"     = "#1F4E79",
  "Nord-Kivu" = "#C0392B",
  "Sud-Kivu"  = "#27AE60"
)
couleurs_alerte <- c(
  "Dépassement (>=100%)" = "#8B0000",
  "Saturation (80-99%)"  = "#E74C3C",
  "Tension (60-79%)"     = "#E67E22",
  "Normal (<60%)"        = "#27AE60"
)

lignes_alerte <- list(
  ggplot2::geom_hline(yintercept = SEUIL_TENSION,
                      linetype="dashed", color="#E67E22", linewidth=0.6),
  ggplot2::geom_hline(yintercept = SEUIL_SATURATION,
                      linetype="dashed", color="#E74C3C", linewidth=0.7),
  ggplot2::geom_hline(yintercept = SEUIL_CRITIQUE,
                      linetype="dotted", color="#8B0000", linewidth=0.8)
)

theme_preis <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face="bold", size=13),
    plot.subtitle    = ggplot2::element_text(size=10, color="grey40"),
    plot.caption     = ggplot2::element_text(size=8,  color="grey50", hjust=0),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

caption_std <- paste0(
  "Source : SitReps COUSP/INSP — MVE-17 Bundibugyo RDC | PREIS / Africa CDC\n",
  "Seuils : 60% tension | 80% saturation | 100% dépassement"
)

# ============================================================
# 6. GRAPHIQUE 1 — Évolution nationale
# ============================================================
d1 <- df_nat %>% dplyr::filter(!is.na(taux_occ_pct))
if (nrow(d1) > 0) {
  g1 <- ggplot2::ggplot(d1, ggplot2::aes(x = date, y = taux_occ_pct)) +
    lignes_alerte +
    ggplot2::geom_area(fill="#1F4E79", alpha=0.10) +
    ggplot2::geom_line(color="#1F4E79", linewidth=1.1) +
    ggplot2::geom_point(ggplot2::aes(size=patients_isoles),
                        color="#1F4E79", alpha=0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label=sprintf("%.1f%%\nN°%d", taux_occ_pct, sitrep_no)),
      vjust=-0.7, size=2.7, color="#1F4E79") +
    ggplot2::annotate("text", x=min(d1$date), y=c(61,81,101),
                      label=c("Tension 60%","Saturation 80%","Dépassement 100%"),
                      hjust=0, size=2.6,
                      color=c("#E67E22","#E74C3C","#8B0000")) +
    ggplot2::scale_x_date(date_labels="%d %b", date_breaks="2 days") +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(x,"%"),
      limits=c(0, max(d1$taux_occ_pct, na.rm=TRUE)*1.25)) +
    ggplot2::scale_size_continuous(name="Patients isolés", range=c(3,8)) +
    ggplot2::labs(
      title    = "Taux d'occupation des lits — Niveau national",
      subtitle = paste0("MVE-17 Bundibugyo | SitReps N°31–N°", last_no,
                        " | Dernier : ", sprintf("%.1f%%", tail(d1$taux_occ_pct,1))),
      x="Date", y="Taux d'occupation (%)", caption=caption_std) +
    theme_preis

  ggplot2::ggsave(file.path(OUT_DIR,"g_bed_occ_national_trend.png"),
                  g1, width=12, height=6, dpi=150)
  cat(">>> g_bed_occ_national_trend.png\n")
}

# ============================================================
# 7. GRAPHIQUE 2 — Évolution par province
# ============================================================
d2 <- df_prov %>% dplyr::filter(!is.na(taux_occ_pct))
if (nrow(d2) > 0) {
  g2 <- ggplot2::ggplot(
    d2, ggplot2::aes(x=date, y=taux_occ_pct,
                     color=province, group=province)) +
    lignes_alerte +
    ggplot2::geom_line(linewidth=1.1) +
    ggplot2::geom_point(size=3) +
    ggplot2::geom_text(
      ggplot2::aes(label=sprintf("%.0f%%", taux_occ_pct)),
      vjust=-0.9, size=2.6, show.legend=FALSE) +
    ggplot2::scale_color_manual(values=couleurs_province, name="Province") +
    ggplot2::scale_x_date(date_labels="%d %b", date_breaks="2 days") +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(x,"%"), limits=c(0, 120)) +
    ggplot2::labs(
      title    = "Évolution du taux d'occupation par province",
      subtitle = paste0("MVE-17 Bundibugyo — SitReps N°31–N°", last_no),
      x="Date", y="Taux d'occupation (%)", caption=caption_std) +
    theme_preis

  ggplot2::ggsave(file.path(OUT_DIR,"g_bed_occ_province_trend.png"),
                  g2, width=12, height=6, dpi=150)
  cat(">>> g_bed_occ_province_trend.png\n")
}

# ============================================================
# 8. GRAPHIQUE 3 — Dernière situation par province
# ============================================================
d3 <- latest_prov %>% dplyr::filter(!is.na(taux_occ_pct))
if (nrow(d3) > 0) {
  g3 <- ggplot2::ggplot(
    d3, ggplot2::aes(x=reorder(province, taux_occ_pct),
                     y=taux_occ_pct, fill=province)) +
    ggplot2::geom_col(width=0.6, show.legend=FALSE) +
    ggplot2::geom_hline(yintercept=SEUIL_SATURATION, linetype="dashed",
                        color="#E74C3C", linewidth=0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label=sprintf("%.1f%%  (%d / %d lits)",
                                 taux_occ_pct, patients_isoles, lits_disponibles)),
      hjust=-0.05, size=3.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values=couleurs_province) +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(x,"%"),
      limits=c(0, max(d3$taux_occ_pct, na.rm=TRUE)*1.3)) +
    ggplot2::labs(
      title    = paste0("Taux d'occupation par province — SitRep N°", last_no),
      subtitle = paste0("Situation au ", format(last_date, "%d %B %Y")),
      x=NULL, y="Taux d'occupation (%)", caption=caption_std) +
    theme_preis

  ggplot2::ggsave(file.path(OUT_DIR,"g_bed_occ_province_bar.png"),
                  g3, width=10, height=5, dpi=150)
  cat(">>> g_bed_occ_province_bar.png\n")
}

# ============================================================
# 9. GRAPHIQUE 4 — Zones de santé (top 15)
# ============================================================
d4 <- latest_zone %>%
  dplyr::filter(!is.na(taux_occ_pct)) %>%
  dplyr::slice_max(taux_occ_pct, n=15, with_ties=FALSE) %>%
  dplyr::mutate(
    alerte = dplyr::case_when(
      taux_occ_pct >= SEUIL_CRITIQUE   ~ "Dépassement (>=100%)",
      taux_occ_pct >= SEUIL_SATURATION ~ "Saturation (80-99%)",
      taux_occ_pct >= SEUIL_TENSION    ~ "Tension (60-79%)",
      TRUE                             ~ "Normal (<60%)"
    ),
    alerte = factor(alerte, levels=names(couleurs_alerte))
  )
if (nrow(d4) > 0) {
  g4 <- ggplot2::ggplot(
    d4, ggplot2::aes(x=reorder(zone, taux_occ_pct),
                     y=taux_occ_pct, fill=alerte)) +
    ggplot2::geom_col(width=0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label=sprintf("%.0f%%  (%d/%d)",
                                 taux_occ_pct, patients_isoles, lits_disponibles)),
      hjust=-0.05, size=3.0) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values=couleurs_alerte, name="Niveau d'alerte") +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(x,"%"),
      limits=c(0, max(d4$taux_occ_pct, na.rm=TRUE)*1.3)) +
    ggplot2::labs(
      title    = paste0("Top ", nrow(d4), " zones — taux d'occupation des lits"),
      subtitle = paste0("SitRep N°", last_no," | ",format(last_date,"%d %B %Y")),
      x=NULL, y="Taux d'occupation (%)", caption=caption_std) +
    theme_preis +
    ggplot2::theme(legend.position="right")

  ggplot2::ggsave(file.path(OUT_DIR,"g_bed_occ_zone_bar.png"),
                  g4, width=12, height=7, dpi=150)
  cat(">>> g_bed_occ_zone_bar.png\n")
}

# ============================================================
# 10. RAPPORT TEXTE
# ============================================================
rpt_fp <- file.path(OUT_DIR, "bed_occupancy_report_v2.txt")
con <- file(rpt_fp, open="w", encoding="UTF-8")
on.exit(close(con), add=TRUE)
w  <- function(...) writeLines(paste0(...), con)
nl <- function()    writeLines("", con)

w("============================================================")
w("PREIS — Taux d'occupation des lits | MVE-17 Bundibugyo RDC")
w("Généré le       : ", GEN_DATE)
w("Dernier SitRep  : N°", last_no, " (", format(last_date), ")")
w("============================================================")
nl()

if (nrow(latest_nat) > 0) {
  ln <- latest_nat[1,]
  niv <- dplyr::case_when(
    is.na(ln$taux_occ_pct)               ~ "—",
    ln$taux_occ_pct >= SEUIL_CRITIQUE    ~ "DEPASSEMENT DE CAPACITE",
    ln$taux_occ_pct >= SEUIL_SATURATION  ~ "SATURATION — renforts urgents",
    ln$taux_occ_pct >= SEUIL_TENSION     ~ "SOUS TENSION",
    TRUE                                 ~ "Capacite confortable"
  )
  w("== SITUATION NATIONALE ==================================")
  w("  Lits disponibles  : ", ifelse(is.na(ln$lits_disponibles),"n/d",ln$lits_disponibles))
  w("  Patients isolés   : ", ifelse(is.na(ln$patients_isoles), "n/d",ln$patients_isoles))
  w("  Taux d'occupation : ",
    ifelse(is.na(ln$taux_occ_pct),"non calculable",sprintf("%.1f %%",ln$taux_occ_pct)))
  w("  Niveau d'alerte   : ", niv)
  nl()
}

if (nrow(latest_prov) > 0) {
  w("== SITUATION PAR PROVINCE ===============================")
  for (i in seq_len(nrow(latest_prov))) {
    r <- latest_prov[i,]
    w(sprintf("  %-12s : %s (%s/%s lits) — SitRep N°%d",
      r$province,
      ifelse(is.na(r$taux_occ_pct),"n/d",sprintf("%.1f %%",r$taux_occ_pct)),
      ifelse(is.na(r$patients_isoles),"?",r$patients_isoles),
      ifelse(is.na(r$lits_disponibles),"?",r$lits_disponibles),
      r$sitrep_no))
  }
  nl()
}

if (nrow(latest_zone) > 0) {
  w("== TOP 10 ZONES DE SANTE (taux les plus elevés) ========")
  top10 <- latest_zone %>%
    dplyr::filter(!is.na(taux_occ_pct)) %>%
    dplyr::slice_max(taux_occ_pct, n=10, with_ties=FALSE)
  for (i in seq_len(nrow(top10))) {
    r <- top10[i,]
    w(sprintf("  %-20s (%-10s) : %.1f %% (%s/%s lits)",
      r$zone, r$province, r$taux_occ_pct,
      ifelse(is.na(r$patients_isoles),"?",r$patients_isoles),
      ifelse(is.na(r$lits_disponibles),"?",r$lits_disponibles)))
  }
  nl()
}

d_ok <- df_nat %>% dplyr::filter(!is.na(taux_occ_pct))
if (nrow(d_ok) > 1) {
  w("== EVOLUTION NATIONALE (barre de progression) ===========")
  for (i in seq_len(nrow(d_ok))) {
    r <- d_ok[i,]
    barre <- paste(rep("=", min(round(r$taux_occ_pct/5), 20)), collapse="")
    w(sprintf("  N°%02d (%s) : %5.1f %%  [%s]",
      r$sitrep_no, format(r$date,"%d/%m"), r$taux_occ_pct, barre))
  }
  nl()
}

w("== REPERES OPERATIONNELS ================================")
w("  < 60 %  : Capacite confortable")
w("  60-79 % : Sous tension - surveiller")
w("  80-99 % : Saturation - planifier renforts/transferts")
w("  >=100 % : Depassement - action immediate requise")
nl()
w("NOTE : Taux SitReps 31-37 estimés (patients isolés/lits")
w("déclarés). SitRep 38 : taux publié COUSP = 84,1 %.")
w("Toujours vérifier avec les équipes terrain.")
w("============================================================")

cat(">>> Rapport : ", basename(rpt_fp), "\n", sep="")
cat("====================================================\n")
cat("Analyse terminée. Fichiers dans : ", OUT_DIR, "\n", sep="")
cat("====================================================\n")
