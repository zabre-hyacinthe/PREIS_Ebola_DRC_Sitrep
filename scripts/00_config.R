############################################################
# 00_config.R — PREIS EBOLA DRC
############################################################

if (!exists("BASE_DIR")) {
  BASE_DIR <- Sys.getenv("PREIS_BASE_DIR", unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
}

SCRIPT_DIR         <- file.path(BASE_DIR, "scripts")
DATA_RAW_DIR       <- file.path(BASE_DIR, "data/raw")
DATA_PROCESSED_DIR <- file.path(BASE_DIR, "data/processed")
DATA_FINAL_DIR     <- file.path(BASE_DIR, "data/final")
PDF_DIR            <- file.path(BASE_DIR, "data/pdf")
OUTPUT_DIR         <- file.path(BASE_DIR, "outputs")
DOC_DIR            <- file.path(BASE_DIR, "documentation")
LOG_DIR            <- file.path(BASE_DIR, "logs")
TABLE_DIR          <- file.path(DATA_PROCESSED_DIR, "tables")

for (d in c(SCRIPT_DIR, DATA_RAW_DIR, DATA_PROCESSED_DIR, DATA_FINAL_DIR,
            PDF_DIR, OUTPUT_DIR, DOC_DIR, LOG_DIR, TABLE_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

INSP_CATEGORY_PAGE <- "https://insp.cd/category/sitrep/"
INSP_MAX_PAGES     <- 6L
EPIDEMIC_LABEL     <- "MVB_2026_Ituri_Bundibugyo"

REGISTRY_FP          <- file.path(DATA_FINAL_DIR, "sitrep_registry.csv")
RUN_LOG_FP           <- file.path(LOG_DIR, "master_run_log.csv")
LINES_FP             <- file.path(DATA_PROCESSED_DIR, paste0("PREIS_lines_extracted_", format(Sys.Date(), "%Y%m%d"), ".csv"))
TABLE_ROWS_FP        <- file.path(DATA_PROCESSED_DIR, paste0("PREIS_table_rows_", format(Sys.Date(), "%Y%m%d"), ".csv"))
CANDIDATES_FP        <- file.path(DATA_FINAL_DIR, "PREIS_indicator_candidates.csv")
VALIDATED_FP         <- file.path(DATA_FINAL_DIR, "PREIS_indicators_validated.csv")
HEALTH_ZONES_FP      <- file.path(DATA_FINAL_DIR, "PREIS_health_zones.csv")
QC_BY_SITREP_FP      <- file.path(DATA_FINAL_DIR, "PREIS_QC_by_sitrep.csv")
QC_ISSUES_FP         <- file.path(DATA_FINAL_DIR, "PREIS_QC_issues.csv")

packages_required <- c(
  "dplyr", "readr", "stringr", "tibble", "tidyr", "purrr",
  "openxlsx", "glue", "lubridate", "rvest", "httr",
  "pdftools", "base64enc", "xml2", "jsonlite"
)

missing_required <- packages_required[!vapply(packages_required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_required) > 0) install.packages(missing_required)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(openxlsx)
  library(glue)
  library(lubridate)
  library(rvest)
  library(httr)
  library(pdftools)
  library(base64enc)
  library(xml2)
  library(jsonlite)
})

KNOWN_HZ_DICT <- c(
  "Aru", "Aungba", "Bambu", "Bunia", "Damas", "Gety", "Gethy",
  "Kilo", "Komanda", "Lita", "Logo", "Mambasa", "Mangala",
  "Mongbwalu", "Nizi", "Nyankunde", "Rimba", "Rwampara",
  "Beni", "Butembo", "Goma", "Kalunguta", "Katwa", "Kyondo", "Oicha",
  "Miti-Murhesa"
)
