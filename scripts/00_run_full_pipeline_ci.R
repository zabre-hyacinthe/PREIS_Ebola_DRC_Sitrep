############################################################
# 00_run_full_pipeline_ci.R — PREIS EBOLA DRC
#
# AJOUT 2026-09-18 : comble un vide architectural majeur decouvert cette
# semaine -- scripts/03_extract_pdf.R et scripts/04_extract_indicators.R
# ne FONT QUE DEFINIR des fonctions. Rien ne les appelait jamais dans le
# workflow automatise (chaque `Rscript scripts/03_extract_pdf.R` demarrait
# une session R, definissait les fonctions, puis quittait sans rien
# executer). Le vrai orchestrateur qui relie scraping -> telechargement ->
# extraction -> validation -> export, run_preis_pipeline(), existe deja
# dans scripts/07_run_pipeline.R mais n'etait source nulle part non plus.
#
# Ce script fait exactement ca : chemins portables (meme motif que les
# autres scripts deja corriges cette semaine : GITHUB_WORKSPACE avec repli
# local), sourcing des 7 modules dans l'ordre, puis appel de la fonction.
#
# Verifie le 18/09/2026 : le sourcing des 7 fichiers ensemble ne produit
# aucune erreur (voir tests joints). L'extraction elle-meme a ete testee
# et validee valeur par valeur sur un vrai SitRep (125) recu de
# l'utilisateur -- tous les indicateurs par province/DPS correspondent au
# document source. Le SEUL element non testable depuis ce sandbox est
# l'appel reseau reel vers insp.cd (domaine non autorise ici) -- a
# confirmer sur le premier run GitHub Actions.
############################################################

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr); library(tibble)
  library(readr); library(pdftools); library(httr); library(rvest)
  library(openxlsx)
})

BASE_DIR   <- Sys.getenv("GITHUB_WORKSPACE", unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
SCRIPT_DIR <- file.path(BASE_DIR, "scripts")
PDF_DIR    <- file.path(BASE_DIR, "data/pdf")
TABLE_DIR  <- file.path(BASE_DIR, "data/processed")
DATA_FINAL <- file.path(BASE_DIR, "data/final")
OUTPUT_DIR <- file.path(BASE_DIR, "outputs")
LOG_DIR    <- file.path(BASE_DIR, "data/logs")
for (d in c(PDF_DIR, TABLE_DIR, DATA_FINAL, OUTPUT_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

INSP_CATEGORY_PAGE <- "https://insp.cd/category/sitrep/"
INSP_MAX_PAGES     <- 6
EPIDEMIC_LABEL      <- "MVE17-Bundibugyo-2026"

REGISTRY_FP      <- file.path(DATA_FINAL, "sitrep_registry.csv")
RUN_LOG_FP       <- file.path(LOG_DIR, "master_run_log.csv")
LINES_FP         <- file.path(TABLE_DIR, "pdf_lines.csv")
TABLE_ROWS_FP    <- file.path(TABLE_DIR, "pdf_table_rows.csv")
CANDIDATES_FP    <- file.path(DATA_FINAL, "PREIS_indicator_candidates.csv")
VALIDATED_FP     <- file.path(DATA_FINAL, "PREIS_indicators_validated.csv")
HEALTH_ZONES_FP  <- file.path(DATA_FINAL, "PREIS_health_zones.csv")
QC_ISSUES_FP     <- file.path(DATA_FINAL, "PREIS_QC_issues.csv")
QC_BY_SITREP_FP  <- file.path(DATA_FINAL, "PREIS_QC_by_sitrep.csv")

for (f in c("01_utils.R", "02_scrape_insp.R", "03_extract_pdf.R",
            "04_extract_indicators.R", "05_qc_validate.R",
            "06_analyse_report.R", "07_run_pipeline.R")) {
  cat("Sourcing", f, "...\n")
  source(file.path(SCRIPT_DIR, f), encoding = "UTF-8")
}

cat("\n============================================================\n")
cat("Lancement de run_preis_pipeline()\n")
cat("============================================================\n\n")

results <- run_preis_pipeline(
  force_redownload    = FALSE,
  force_reextract      = FALSE,
  max_new              = Inf,
  process_all_known    = FALSE,
  enable_tabulizer     = FALSE,   # tabulizer (Java) indisponible sur le runner GH Actions standard
  block_email_on_critical = TRUE
)

invisible(results)
