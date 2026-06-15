############################################################
# PREIS EBOLA DRC
# 02_fetch_inrb_reference_data.R
#
# Télécharge les données NATIONALES déjà transcrites et
# validées par l'INRB (dépôt BDBV2026-Data) et les intègre
# comme :
#   (1) SOURCE pour les SitReps scannés illisibles (7-12, 14)
#   (2) RÉFÉRENCE de validation croisée pour les autres
#
# Source : github.com/INRB-UMIE/BDBV2026-Data
#          data/insp_sitrep/processed/*.csv  (non-LFS, texte)
#
# Sortie : data/final/INRB_reference_national.csv
############################################################

suppressPackageStartupMessages({
  library(httr); library(readr); library(dplyr)
  library(stringr); library(tidyr); library(lubridate)
})

BASE_DIR     <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
DATA_FINAL   <- file.path(BASE_DIR, "data/final")
dir.create(DATA_FINAL, recursive = TRUE, showWarnings = FALSE)

# raw.githubusercontent.com sert les CSV en texte (pas LFS)
RAW_BASE <- "https://raw.githubusercontent.com/INRB-UMIE/BDBV2026-Data/main/data/insp_sitrep/processed/"

cat("\n============================================================\n")
cat("DONNÉES DE RÉFÉRENCE INRB (validées) — National par date\n")
cat("============================================================\n\n")

# ---- Indicateurs nationaux à récupérer (nom_fichier -> indicator_code) ----
national_files <- c(
  national_cumulative_confirmed_cases   = "insp_sitrep__national_cumulative_confirmed_cases__daily.csv",
  national_cumulative_confirmed_deaths  = "insp_sitrep__national_cumulative_confirmed_deaths__daily.csv",
  national_cumulative_suspected_cases   = "insp_sitrep__national_cumulative_suspected_cases__daily.csv",
  national_suspected_cases_in_isolation = "insp_sitrep__national_suspected_cases_in_isolation__daily.csv",
  national_suspected_under_investigation= "insp_sitrep__national_suspected_cases_under_investigation__daily.csv"
)

fetch_csv <- function(fname) {
  url <- paste0(RAW_BASE, fname)
  resp <- tryCatch(GET(url, timeout(60),
                       add_headers("User-Agent" = "PREIS-Bot")),
                   error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) {
    cat("   ÉCHEC:", fname, "\n"); return(NULL)
  }
  readr::read_csv(content(resp, "text", encoding = "UTF-8"),
                  show_col_types = FALSE)
}

# ---- Télécharger et empiler ----
ref_long <- list()
for (code in names(national_files)) {
  cat(">> ", code, "\n")
  df <- fetch_csv(national_files[[code]])
  if (is.null(df)) next
  # garder DRC (national) ; colonne valeur = 3e colonne
  val_col <- names(df)[3]
  df2 <- df %>%
    dplyr::filter(toupper(nom) == "DRC") %>%
    dplyr::transmute(
      date = as.Date(date),
      indicator_code = code,
      value = suppressWarnings(as.numeric(.data[[val_col]]))
    ) %>%
    dplyr::filter(!is.na(value))
  ref_long[[code]] <- df2
}

ref_national <- dplyr::bind_rows(ref_long)

# ---- Mapping SitRep N° -> date de rapportage (depuis rapports INRB) ----
sitrep_dates <- tibble::tribble(
  ~sitrep_no, ~sitrep_date,
   1, "2026-05-14",  2, "2026-05-17",  4, "2026-05-18",  5, "2026-05-19",
   6, "2026-05-20",  7, "2026-05-21",  8, "2026-05-22",  9, "2026-05-23",
  10, "2026-05-24", 11, "2026-05-25", 12, "2026-05-26", 13, "2026-05-27",
  14, "2026-05-28", 15, "2026-05-29", 16, "2026-05-30", 17, "2026-05-31",
  18, "2026-06-01", 19, "2026-06-02", 20, "2026-06-03", 21, "2026-06-04",
  22, "2026-06-05", 23, "2026-06-06", 24, "2026-06-07", 25, "2026-06-08",
  26, "2026-06-09", 27, "2026-06-10", 28, "2026-06-11", 29, "2026-06-12",
  30, "2026-06-13", 31, "2026-06-14", 32, "2026-06-15", 33, "2026-06-16",
  34, "2026-06-17", 35, "2026-06-18"
) %>% dplyr::mutate(sitrep_date = as.Date(sitrep_date))

# ---- Joindre : pour chaque SitRep, les valeurs nationales INRB ----
ref_by_sitrep <- sitrep_dates %>%
  dplyr::left_join(ref_national, by = c("sitrep_date" = "date")) %>%
  dplyr::filter(!is.na(indicator_code)) %>%
  dplyr::mutate(
    source = "INRB_reference",
    supervisor_validated = TRUE
  ) %>%
  dplyr::select(sitrep_no, sitrep_date, indicator_code, value,
                source, supervisor_validated)

# ---- Sauvegarder ----
out_fp <- file.path(DATA_FINAL, "INRB_reference_national.csv")
readr::write_csv(ref_by_sitrep, out_fp)

cat("\n============================================================\n")
cat("Référence INRB sauvegardée :", out_fp, "\n")
cat("Lignes :", nrow(ref_by_sitrep), "\n")
cat("SitReps couverts :",
    paste(sort(unique(ref_by_sitrep$sitrep_no)), collapse = ", "), "\n")
cat("============================================================\n")

# Aperçu cas + décès
cat("\nAperçu (cas confirmés cumulés & décès par SitRep) :\n")
preview <- ref_by_sitrep %>%
  dplyr::filter(indicator_code %in%
                c("national_cumulative_confirmed_cases",
                  "national_cumulative_confirmed_deaths")) %>%
  tidyr::pivot_wider(id_cols = c(sitrep_no, sitrep_date),
                     names_from = indicator_code, values_from = value) %>%
  dplyr::arrange(sitrep_no)
print(as.data.frame(preview))
