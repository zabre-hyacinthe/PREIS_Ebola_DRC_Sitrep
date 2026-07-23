## ============================================================
## PREIS Ebola RDC — Préparation des données du dashboard
## Copie la série temporelle + le tableau zones depuis l'analyse
## vers le dossier dashboard (pour exécution locale ou déploiement
## shinyapps.io). À lancer après 03_analyse_consolidee.R.
## ============================================================

BASE_DIR    <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
ANALYSE_DIR <- file.path(BASE_DIR, "outputs", "analyse")
DASH_DIR    <- file.path(BASE_DIR, "dashboard_ebola")
DASH_DATA   <- file.path(DASH_DIR, "data")
DASH_CURATED<- file.path(DASH_DATA, "curated")

dir.create(DASH_DATA,    recursive = TRUE, showWarnings = FALSE)
dir.create(DASH_CURATED, recursive = TRUE, showWarnings = FALSE)

# 1. Série temporelle + zones (depuis l'analyse)
copy_if <- function(from, to) {
  if (file.exists(from)) { file.copy(from, to, overwrite = TRUE)
    cat("  copié :", basename(from), "\n") }
  else cat("  MANQUANT :", from, "\n")
}
cat("Préparation des données du dashboard Ebola...\n")
copy_if(file.path(ANALYSE_DIR, "serie_temporelle_nationale.csv"),
        file.path(DASH_DATA, "serie_temporelle_nationale.csv"))
copy_if(file.path(ANALYSE_DIR, "tableau_zones_sante.csv"),
        file.path(DASH_DATA, "tableau_zones_sante.csv"))
# Base longue complète (tous les indicateurs) pour la vue évolution KPI
copy_if(file.path(BASE_DIR, "data", "final", "PREIS_indicators_long.csv"),
        file.path(DASH_DATA, "PREIS_indicators_long.csv"))
# Série journalière (national + province) pour l'onglet Suivi journalier
copy_if(file.path(BASE_DIR, "data", "final", "PREIS_daily_indicators.csv"),
        file.path(DASH_DATA, "PREIS_daily_indicators.csv"))
# Couche choroplèthe : zones de santé réelles (Est RDC, simplifiée)
copy_if(file.path(BASE_DIR, "data", "curated", "rdc_zones_sante_est.geojson"),
        file.path(DASH_DATA, "curated", "rdc_zones_sante_est.geojson"))
# Module de synthèse narrative (réutilisé par le dashboard)
copy_if(file.path(BASE_DIR, "scripts", "05_synthese_narrative.R"),
        file.path(DASH_DIR, "05_synthese_narrative.R"))

# 2. Fond de carte Afrique (depuis curated existant, sinon à placer manuellement)
src_africa <- file.path(BASE_DIR, "data", "curated", "africa_countries_rcc.geojson")
copy_if(src_africa, file.path(DASH_CURATED, "africa_countries_rcc.geojson"))


# --- DHIS2 Situation Room : copie des 4 CSV AGREGES (0 donnee individuelle) ---
DHIS2_PUB <- file.path(DASH_DATA, 'dhis2_public')
dir.create(DHIS2_PUB, recursive = TRUE, showWarnings = FALSE)
.agg_names <- c('dhis2_mve_situation_room.csv','dhis2_mve_situation_room_zones.csv',
                'dhis2_mve_situation_room_priority_zones.csv','dhis2_mve_situation_room_epi_curve.csv')
.agg_src_dirs <- c(file.path(BASE_DIR,'source_line_list','data'),
                   file.path(BASE_DIR,'source_line_list'),
                   file.path(BASE_DIR,'outputs','dhis2'),
                   file.path(BASE_DIR,'data','dhis2_public'))
for (.nm in .agg_names) {
  .hit <- NA_character_
  for (.d in .agg_src_dirs) { .p <- file.path(.d, .nm); if (file.exists(.p)) { .hit <- .p; break } }
  if (!is.na(.hit)) copy_if(.hit, file.path(DHIS2_PUB, .nm)) else cat('  MANQUANT (agrege):', .nm, '\n')
}
cat("\nDashboard prêt. Pour le lancer localement :\n")
cat('  shiny::runApp("', DASH_DIR, '")\n', sep = "")
