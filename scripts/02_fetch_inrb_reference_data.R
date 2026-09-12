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
#
# CORRECTIF 2026-09-12 :
#  (1) Chemin en dur "D:/PREIS_..." remplace par le meme motif
#      portable que les autres scripts du workflow (BASE_DIR via
#      GITHUB_WORKSPACE, repli sur le chemin Windows local).
#  (2) Le tableau sitrep_dates code en dur (arrete au SitRep 35,
#      2026-06-18 -- meme categorie de bug que celui deja corrige
#      dans 03_analyse_consolidee.R) est supprime. A la place,
#      jointure PAR DATE sur outputs/analyse/serie_temporelle_
#      nationale.csv, deja produit et tenu a jour par
#      03_analyse_consolidee.R (colonnes sitrep_no/date/
#      date_quality) -- une seule source de verite pour le
#      mapping SitRep <-> date, jamais une deuxieme copie figee.
#      Seules les dates de qualite "observed" sont utilisees pour
#      la jointure, afin de ne pas valider une reference externe
#      fiable contre une date interpolee/extrapolee incertaine.
############################################################

suppressPackageStartupMessages({
  library(httr); library(readr); library(dplyr)
  library(stringr); library(tidyr); library(lubridate)
})

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
DATA_FINAL   <- file.path(BASE_DIR, "data/final")
ANALYSE_DIR  <- file.path(BASE_DIR, "outputs/analyse")
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

if (nrow(ref_national) == 0) {
  cat("\nAUCUNE donnee INRB recuperee (echec reseau ou source indisponible) -- on s'arrete sans ecraser un fichier de sortie existant.\n")
  quit(save = "no", status = 0)
}

# ---- Mapping SitRep N° <-> date : source unique = le pipeline lui-meme ----
# (plus de tableau code en dur ici -- voir note de correctif en tete de fichier)
serie_fp <- file.path(ANALYSE_DIR, "serie_temporelle_nationale.csv")
if (!file.exists(serie_fp)) {
  cat("\nATTENTION: ", serie_fp, " introuvable -- ce script doit tourner APRES 03_analyse_consolidee.R dans le workflow. Abandon sans ecraser la sortie existante.\n")
  quit(save = "no", status = 0)
}
sitrep_dates <- readr::read_csv(serie_fp, show_col_types = FALSE) %>%
  dplyr::filter(date_quality == "observed") %>%
  dplyr::transmute(sitrep_no = as.integer(sitrep_no), sitrep_date = as.Date(date)) %>%
  dplyr::distinct()

# ---- Joindre : pour chaque SitRep (date observee), les valeurs nationales INRB ----
# relationship="many-to-many" : deux SitReps publies le meme jour calendaire
# (cas reel observe, ex. SitRep 49 et 50 le 2026-07-02) doivent chacun recevoir
# la meme valeur de reference pour cette date -- ce n'est pas un doublon a corriger.
ref_by_sitrep <- sitrep_dates %>%
  dplyr::inner_join(ref_national, by = c("sitrep_date" = "date"),
                     relationship = "many-to-many") %>%
  dplyr::mutate(
    source = "INRB_reference",
    supervisor_validated = TRUE
  ) %>%
  dplyr::select(sitrep_no, sitrep_date, indicator_code, value,
                source, supervisor_validated) %>%
  dplyr::arrange(sitrep_no, indicator_code)

if (nrow(ref_by_sitrep) == 0) {
  cat("\nAucune correspondance de date entre la reference INRB et les SitReps observes -- pas d'ecriture (evite de vider le fichier existant).\n")
  quit(save = "no", status = 0)
}

# ---- Sauvegarder ----
out_fp <- file.path(DATA_FINAL, "INRB_reference_national.csv")
readr::write_csv(ref_by_sitrep, out_fp)

cat("\n============================================================\n")
cat("Référence INRB sauvegardée :", out_fp, "\n")
cat("Lignes :", nrow(ref_by_sitrep), "\n")
cat("SitReps couverts :",
    paste(range(ref_by_sitrep$sitrep_no), collapse = " a "), "\n")
cat("============================================================\n")

# Aperçu cas + décès (les plus recents)
cat("\nAperçu (cas confirmés cumulés & décès par SitRep, 10 plus recents) :\n")
preview <- ref_by_sitrep %>%
  dplyr::filter(indicator_code %in%
                c("national_cumulative_confirmed_cases",
                  "national_cumulative_confirmed_deaths")) %>%
  tidyr::pivot_wider(id_cols = c(sitrep_no, sitrep_date),
                     names_from = indicator_code, values_from = value) %>%
  dplyr::arrange(dplyr::desc(sitrep_no)) %>%
  utils::head(10) %>%
  dplyr::arrange(sitrep_no)
print(as.data.frame(preview))
