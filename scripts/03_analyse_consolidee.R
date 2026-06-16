############################################################
# PREIS EBOLA DRC
# 03_analyse_consolidee.R
#
# NIVEAUX 1-2 du plan d'analyse : description + variations.
# Lit la base consolidée (PREIS_indicators_long.csv) + les
# données INRB par zone, et produit :
#
#   TABLEAUX (CSV)  -> pour le dashboard
#     - serie_temporelle_nationale.csv
#     - tableau_zones_sante.csv
#
#   GRAPHIQUES (PNG) -> pour rapport & email
#     - g1_courbe_epidemique.png
#     - g2_courbe_mortalite.png
#     - g3_evolution_cfr.png
#     - g4_top_zones.png
#
#   CARTE (PNG)      -> dimension géographique
#     - carte_zones_intensite.png
#
# Toutes les sorties vont dans outputs/analyse/
############################################################

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
  library(stringr); library(ggplot2); library(lubridate)
})

# ---- Chemins ----
# Chemin portable : GitHub Actions (cloud) ou Windows (local)
BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
DATA_FINAL  <- file.path(BASE_DIR, "data/final")
OUT_DIR     <- file.path(BASE_DIR, "outputs/analyse")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Palette cohérente (rouge épidémie, gris neutres)
COL_CAS    <- "#C0392B"   # rouge cas
COL_DECES  <- "#2C3E50"   # bleu-gris décès
COL_CFR    <- "#E67E22"   # orange CFR
COL_LIGHT  <- "#BDC3C7"

theme_preis <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.minor = element_blank()
  )

cat("\n========================================================\n")
cat("PREIS — ANALYSE CONSOLIDÉE (tableaux, graphiques, carte)\n")
cat("========================================================\n\n")

# =========================================================
# 1. CHARGER LA BASE + MAPPING DATES
# =========================================================
ind_fp <- file.path(DATA_FINAL, "PREIS_indicators_long.csv")
if (!file.exists(ind_fp)) stop("Base introuvable : ", ind_fp,
                               "\nLance d'abord le pipeline principal.")

ind <- readr::read_csv(ind_fp, show_col_types = FALSE)

# Mapping SitRep -> date (AUTOMATIQUE : voir 02_fetch_inrb_reference_data.R).
# SitReps 1-13 = calendrier irregulier (fixe). A partir du SitRep 14
# (28 mai 2026) : 1 SitRep par jour -> calcul automatique, aucune table a
# etendre pour les SitReps futurs.
.SNO_ANCHOR  <- 14L
.DATE_ANCHOR <- as.Date("2026-05-28")
sitrep_dates_hist <- tibble::tribble(
  ~sitrep_no, ~date,
   1,"2026-05-14", 2,"2026-05-17", 4,"2026-05-18", 5,"2026-05-19",
   6,"2026-05-20", 7,"2026-05-21", 8,"2026-05-22", 9,"2026-05-23",
  10,"2026-05-24",11,"2026-05-25",12,"2026-05-26",13,"2026-05-27"
) %>% dplyr::mutate(date = as.Date(date))
.max_sno_auto <- max(.SNO_ANCHOR + as.integer(Sys.Date() - .DATE_ANCHOR) + 2L, 40L)
sitrep_dates_auto <- tibble::tibble(
  sitrep_no = .SNO_ANCHOR:.max_sno_auto,
  date      = .DATE_ANCHOR + (0:(.max_sno_auto - .SNO_ANCHOR))
)
sitrep_dates <- dplyr::bind_rows(sitrep_dates_hist, sitrep_dates_auto) %>%
  dplyr::arrange(sitrep_no) %>%
  dplyr::distinct(sitrep_no, .keep_all = TRUE)

# =========================================================
# 2. SÉRIE TEMPORELLE NATIONALE (tableau pivot)
# =========================================================
wide <- ind %>%
  dplyr::filter(indicator_code %in% c(
    "cumulative_confirmed_cases", "cumulative_deaths",
    "new_confirmed_cases", "case_fatality_ratio",
    "suspected_cases_investigation")) %>%
  dplyr::select(sitrep_no, indicator_code, value) %>%
  dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE) %>%
  dplyr::mutate(sitrep_no = as.integer(sitrep_no)) %>%
  tidyr::pivot_wider(names_from = indicator_code, values_from = value) %>%
  dplyr::left_join(sitrep_dates %>% dplyr::mutate(sitrep_no = as.integer(sitrep_no)),
                   by = "sitrep_no") %>%
  dplyr::arrange(sitrep_no) %>%
  dplyr::rename(
    cas_cumules   = cumulative_confirmed_cases,
    deces_cumules = cumulative_deaths,
    nouveaux_cas  = dplyr::any_of("new_confirmed_cases"),
    cfr           = case_fatality_ratio,
    suspects      = dplyr::any_of("suspected_cases_investigation")
  )

# Filet de securite : si une date est manquante apres le join (probleme de
# type sur sitrep_no), on la recompose directement depuis la table de
# reference via un dictionnaire nomme (insensible au type). Garantit que
# chaque SitRep present a sa date.
.date_lookup <- setNames(as.character(sitrep_dates$date),
                         as.character(as.integer(sitrep_dates$sitrep_no)))
wide <- wide %>%
  dplyr::mutate(
    date = dplyr::if_else(
      is.na(date),
      as.Date(.date_lookup[as.character(as.integer(sitrep_no))]),
      date))

# petite fonction moyenne mobile sans dépendance externe
zoo_rollmean <- function(x, k = 3) {
  n <- length(x); out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- max(1, i - k + 1)
    out[i] <- mean(x[lo:i], na.rm = TRUE)
  }
  out
}

# Calculs dérivés : nouveaux cas ET décès par DIFFÉRENCE (incidence réelle).
# On calcule systématiquement par soustraction des cumuls car le champ
# "nouveaux cas" extrait du PDF est souvent incomplet. C'est l'incidence
# (allure réelle de l'épidémie), pas le cumul qui ne fait que monter.
wide <- wide %>%
  dplyr::mutate(
    nouveaux_cas_calc   = cas_cumules   - dplyr::lag(cas_cumules),
    nouveaux_deces      = deces_cumules - dplyr::lag(deces_cumules),
    var_cas             = nouveaux_cas_calc
  )
# Si l'extraction PDF a une valeur de nouveaux cas, on la garde en second
# recours uniquement là où le calcul est NA (1er SitRep) ; sinon priorité
# au calcul par différence (cohérent avec les cumuls INRB validés).
if ("nouveaux_cas" %in% names(wide)) {
  wide <- wide %>%
    dplyr::mutate(nouveaux_cas = dplyr::coalesce(nouveaux_cas_calc, nouveaux_cas))
} else {
  wide$nouveaux_cas <- wide$nouveaux_cas_calc
}
wide$moy_mobile_cas <- zoo_rollmean(wide$var_cas)

readr::write_csv(wide, file.path(OUT_DIR, "serie_temporelle_nationale.csv"))
cat(">> Tableau série temporelle nationale sauvegardé (",
    nrow(wide), "SitReps )\n")

# =========================================================
# 3. GRAPHIQUE 1 — COURBE ÉPIDÉMIQUE
# =========================================================
g1 <- ggplot(wide, aes(x = date)) +
  geom_col(aes(y = nouveaux_cas), fill = "#E59866", alpha = 0.9, na.rm = TRUE) +
  geom_line(aes(y = moy_mobile_cas), color = COL_CAS, linewidth = 1, na.rm = TRUE) +
  geom_line(aes(y = cas_cumules / 12), color = COL_DECES, linewidth = 0.7,
            linetype = "dashed", na.rm = TRUE) +
  scale_y_continuous(
    name = "Nouveaux cas par SitRep (barres) — incidence",
    sec.axis = sec_axis(~ . * 12, name = "Cas cumulés (ligne pointillée)")
  ) +
  labs(
    title = "Courbe épidémique — MVE Bundibugyo RDC (17e épidémie)",
    subtitle = "Barres = nouveaux cas (allure réelle) · ligne rouge = moyenne mobile 3 pts · pointillé = cumul",
    x = NULL,
    caption = "Nouveaux cas calculés par différence des cumuls nationaux INRB. Données provisoires."
  ) +
  theme_preis +
  theme(axis.title.y.left  = element_text(color = "#B9770E"),
        axis.title.y.right = element_text(color = COL_DECES))

ggsave(file.path(OUT_DIR, "g1_courbe_epidemique.png"),
       g1, width = 9, height = 5, dpi = 150)
cat(">> g1_courbe_epidemique.png\n")

# =========================================================
# 4. GRAPHIQUE 2 — COURBE DE MORTALITÉ
# =========================================================
g2 <- ggplot(wide, aes(x = date)) +
  geom_col(aes(y = nouveaux_deces), fill = COL_LIGHT, na.rm = TRUE) +
  geom_line(aes(y = deces_cumules), color = COL_DECES, linewidth = 1.1, na.rm = TRUE) +
  geom_point(aes(y = deces_cumules), color = COL_DECES, size = 1.5, na.rm = TRUE) +
  labs(
    title = "Mortalité — décès confirmés cumulés et nouveaux décès",
    subtitle = "Nouveaux décès = différence entre SitReps successifs",
    x = NULL, y = "Décès",
    caption = "Source : INSP/INRB. CFR provisoire pendant épidémie active."
  ) +
  theme_preis

ggsave(file.path(OUT_DIR, "g2_courbe_mortalite.png"),
       g2, width = 9, height = 5, dpi = 150)
cat(">> g2_courbe_mortalite.png\n")

# =========================================================
# 5. GRAPHIQUE 3 — ÉVOLUTION DU CFR
# =========================================================
g3 <- ggplot(wide %>% dplyr::filter(!is.na(cfr)), aes(x = date, y = cfr)) +
  annotate("rect", xmin = min(wide$date, na.rm = TRUE),
           xmax = max(wide$date, na.rm = TRUE),
           ymin = 25, ymax = 50, alpha = 0.08, fill = COL_CFR) +
  geom_line(color = COL_CFR, linewidth = 1.1) +
  geom_point(color = COL_CFR, size = 1.8) +
  geom_hline(yintercept = c(25, 50), linetype = "dashed",
             color = COL_CFR, alpha = 0.5) +
  labs(
    title = "Évolution de la létalité (CFR provisoire)",
    subtitle = "Bande = fourchette CFR attendue pour Bundibugyo (25-50%)",
    x = NULL, y = "CFR (%)",
    caption = paste0("CFR provisoire : pendant une épidémie active, certains cas récents",
                     " peuvent encore évoluer.\nNe pas interpréter comme létalité finale.")
  ) +
  theme_preis

ggsave(file.path(OUT_DIR, "g3_evolution_cfr.png"),
       g3, width = 9, height = 5, dpi = 150)
cat(">> g3_evolution_cfr.png\n")

# =========================================================
# 6. ZONES DE SANTÉ — depuis la référence INRB par zone
# =========================================================
RAW_ZONE <- paste0("https://raw.githubusercontent.com/INRB-UMIE/",
                   "BDBV2026-Data/main/data/insp_sitrep/processed/",
                   "insp_sitrep__cumulative_confirmed_cases__daily.csv")

zone_ok <- FALSE
zones <- tryCatch({
  z <- readr::read_csv(url(RAW_ZONE), show_col_types = FALSE)
  zone_ok <- TRUE
  z
}, error = function(e) {
  cat("   (Téléchargement zones INRB échoué — graphique zones ignoré)\n")
  NULL
})

if (!is.null(zones)) {
  # Harmoniser doublons orthographiques + retirer non-zones
  fix_zone <- function(x) {
    x <- dplyr::recode(x,
      "Mongbalu" = "Mongbwalu", "Nyakunde" = "Nyankunde", "Gethy" = "Gety")
    x
  }
  zones2 <- zones %>%
    dplyr::filter(!nom %in% c("DRC", "NA", NA)) %>%
    dplyr::mutate(nom = fix_zone(nom),
                  date = as.Date(date),
                  value = suppressWarnings(as.numeric(cumulative_confirmed_cases))) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(nom) %>%
    dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(nom) %>%
    dplyr::summarise(cas = sum(value), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(cas))

  readr::write_csv(zones2, file.path(OUT_DIR, "tableau_zones_sante.csv"))
  cat(">> Tableau zones de santé sauvegardé (", nrow(zones2), "zones )\n")

  # GRAPHIQUE 4 — Top zones
  top <- zones2 %>% dplyr::filter(cas > 0) %>% dplyr::slice_head(n = 15)
  g4 <- ggplot(top, aes(x = reorder(nom, cas), y = cas)) +
    geom_col(fill = COL_CAS, alpha = 0.85) +
    geom_text(aes(label = cas), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(
      title = "Zones de santé les plus touchées (cas confirmés cumulés)",
      subtitle = "Épicentre : Bunia, Rwampara, Mongbwalu (Ituri)",
      x = NULL, y = "Cas confirmés cumulés",
      caption = "Source : INRB (transcription validée par zone)."
    ) +
    theme_preis +
    theme(panel.grid.major.y = element_blank())

  ggsave(file.path(OUT_DIR, "g4_top_zones.png"),
         g4, width = 9, height = 6, dpi = 150)
  cat(">> g4_top_zones.png\n")

  # =========================================================
  # 7. CARTE — intensité par zone de santé
  # =========================================================
  # Coordonnées approximatives (chef-lieu) des zones touchées
  coords <- tibble::tribble(
    ~nom,          ~lat,    ~lon,
    "Bunia",        1.565,  30.244,
    "Rwampara",     1.530,  30.180,
    "Mongbwalu",    1.960,  30.040,
    "Nyankunde",    1.420,  30.150,
    "Katwa",       -0.470,  29.250,
    "Nizi",         1.700,  30.060,
    "Beni",         0.491,  29.473,
    "Butembo",      0.131,  29.290,
    "Bambu",        1.870,  30.080,
    "Lita",         1.690,  30.300,
    "Kilo",         1.830,  30.130,
    "Miti-Murhesa",-2.350,  28.770,
    "Aru",          2.880,  30.910,
    "Damas",        1.600,  30.300,
    "Rimba",        2.000,  30.500,
    "Komanda",      1.360,  29.770,
    "Oicha",        0.700,  29.520,
    "Kyondo",       0.150,  29.400,
    "Mambasa",      1.360,  29.050,
    "Mangala",      1.600,  30.400,
    "Aungba",       2.300,  30.900,
    "Logo",         2.700,  30.700,
    "Tchomia",      1.480,  30.530,
    "Goma",        -1.679,  29.235,
    "Kalunguta",    0.300,  29.350,
    "Gety",         1.350,  30.190,
    "Kambala",      1.700,  30.200,
    "Masereka",     0.200,  29.300,
    "Vuhovi",       0.450,  29.300
  )

  map_df <- zones2 %>%
    dplyr::inner_join(coords, by = "nom") %>%
    dplyr::filter(cas > 0)

  if (nrow(map_df) > 0) {
    g5 <- ggplot(map_df, aes(x = lon, y = lat)) +
      geom_point(aes(size = cas, color = cas), alpha = 0.75) +
      geom_text(aes(label = nom), size = 2.6, vjust = -1.2, color = "grey25") +
      scale_color_gradient(low = "#F1948A", high = "#922B21", name = "Cas") +
      scale_size_continuous(range = c(3, 18), guide = "none") +
      labs(
        title = "Répartition géographique des cas confirmés — MVE RDC",
        subtitle = "Taille et couleur ∝ cas cumulés par zone de santé",
        x = "Longitude", y = "Latitude",
        caption = paste0("Coordonnées approximatives (chefs-lieux de ZS).",
                         " Source données : INRB. Carte schématique, non géoréférencée précise.")
      ) +
      theme_preis +
      theme(panel.grid.major = element_line(color = "grey92"))

    ggsave(file.path(OUT_DIR, "carte_zones_intensite.png"),
           g5, width = 8, height = 8, dpi = 150)
    cat(">> carte_zones_intensite.png\n")
  }
}

cat("\n========================================================\n")
cat("ANALYSE CONSOLIDÉE TERMINÉE\n")
cat("Sorties dans :", OUT_DIR, "\n")
cat("========================================================\n")

# Aperçu console de la série nationale
cat("\nAperçu série nationale :\n")
print(as.data.frame(wide %>%
  dplyr::select(sitrep_no, date, cas_cumules, deces_cumules,
                nouveaux_deces, cfr)))
