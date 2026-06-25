## ============================================================
## PREIS EBOLA DRC
## 93_reextract_offline.R   (v2 — corrige le bug <<-)
##
## Re-extrait TOUS les PDFs locaux SANS scraping ni internet.
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/93_reextract_offline.R")
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(tibble)
  library(purrr); library(readr); library(tidyr)
})

cat("============================================================\n")
cat("PREIS — RE-EXTRACTION OFFLINE v2 (sans scraping)\n")
cat("Demarre :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

ROOT      <- getwd()
PDF_DIR   <- file.path(ROOT, "data/pdf")
FINAL_DIR <- file.path(ROOT, "data/final")
dir.create(FINAL_DIR, showWarnings = FALSE, recursive = TRUE)

# -- 1. Charger les fonctions --
cat("\n[1] Chargement des fonctions...\n")
need <- c("00_config.R","01_utils.R","03_extract_pdf.R",
          "04_extract_indicators.R","05_qc_validate.R","10_sitrep_identity.R")
for (s in need) {
  fp <- file.path(ROOT, "scripts", s)
  if (file.exists(fp)) {
    tryCatch({ source(fp); cat("   [OK]", s, "\n") },
             error = function(e) cat("   [WARN]", s, ":", conditionMessage(e), "\n"))
  } else cat("   [ABSENT]", s, "\n")
}

if (!exists("safe_num")) {
  safe_num <- function(x) {
    v <- suppressWarnings(as.numeric(gsub("[^0-9.,-]", "", gsub(",", ".", as.character(x)))))
    if (length(v) == 0) NA_real_ else v[1]
  }
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# -- 2. Lister les PDFs locaux valides --
cat("\n[2] Recensement des PDFs locaux...\n")
all_pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
cat("   PDFs trouves :", length(all_pdfs), "\n")

pdf_no   <- integer(0)
pdf_path <- character(0)
for (fp in all_pdfs) {
  no <- tryCatch(sitrep_no_from_filename(fp), error = function(e) NA_integer_)
  if (is.na(no) || no < 1 || no > 60) next
  if (no %in% pdf_no) next   # dedup
  pdf_no   <- c(pdf_no, as.integer(no))
  pdf_path <- c(pdf_path, fp)
}
ord <- order(pdf_no)
pdf_no <- pdf_no[ord]; pdf_path <- pdf_path[ord]
cat("   SitReps uniques a traiter :", length(pdf_no), "\n")
if (length(pdf_no) == 0) stop("Aucun PDF SitRep valide trouve dans ", PDF_DIR)

# -- 3. Fonction d'extraction d'UN pdf (retourne une liste, pas de <<-) --
extract_one <- function(no, path) {
  bundle <- extract_pdf_bundle(path, no, enable_tabulizer = FALSE)
  lt <- bundle$lines
  tr <- if (!is.null(bundle$tables)) bundle$tables else tibble::tibble()
  if (is.null(lt) || nrow(lt) == 0) {
    return(list(ok = FALSE, lines = NULL, cand = tibble::tibble(), hz = tibble::tibble()))
  }
  cand <- extract_indicator_candidates(lt, tr)
  hz   <- extract_hz_from_lines(lt)
  list(ok = TRUE, lines = lt, cand = cand, hz = hz)
}

# -- 4. Boucle d'extraction (accumulation par listes locales) --
cat("\n[3] Extraction (1-3 min)...\n")
lines_acc <- list(); cand_acc <- list(); hz_acc <- list()
ok_count  <- 0L

for (i in seq_along(pdf_no)) {
  no   <- pdf_no[i]
  path <- pdf_path[i]
  res <- tryCatch(extract_one(no, path),
                  error = function(e) {
                    cat(sprintf("   [ERR] SR%02d : %s\n", no, conditionMessage(e)))
                    NULL
                  })
  if (is.null(res)) next
  if (!isTRUE(res$ok)) {
    cat(sprintf("   [VIDE] SR%02d — PDF scanne (passe par INRB)\n", no))
    next
  }
  lines_acc[[length(lines_acc) + 1]] <- res$lines
  if (nrow(res$cand) > 0) cand_acc[[length(cand_acc) + 1]] <- res$cand
  if (nrow(res$hz)   > 0) hz_acc[[length(hz_acc) + 1]]     <- res$hz
  ok_count <- ok_count + 1L
  cat(sprintf("   [OK] SR%02d — %d candidats, %d zones\n",
              no, nrow(res$cand), nrow(res$hz)))
}
cat("\n   PDFs traites avec succes :", ok_count, "/", length(pdf_no), "\n")

# -- 5. Consolidation --
cat("\n[4] Consolidation...\n")
candidates_all <- if (length(cand_acc) > 0) {
  dplyr::bind_rows(cand_acc) %>% dplyr::mutate(extracted_at = as.character(extracted_at))
} else tibble::tibble()

if (nrow(candidates_all) == 0) {
  stop("Aucun candidat extrait. Les PDFs lisibles n'ont produit aucun indicateur.")
}
cat("   Total candidats bruts :", nrow(candidates_all), "\n")

observed <- select_best_observed_indicators(candidates_all)
cat("   Indicateurs observes selectionnes :", nrow(observed), "\n")

# -- 6. Validation QC + derivation lab_positivity_rate --
cat("\n[5] Validation QC...\n")
qc_res <- validate_and_derive_indicators(observed, candidates_all)
validated <- qc_res$validated
cat("   Indicateurs valides :", nrow(validated), "\n")

# -- 7. Ecriture type-coherente --
cat("\n[6] Ecriture des CSV...\n")
norm_dt <- function(df) {
  if ("extracted_at" %in% names(df)) df$extracted_at <- as.character(df$extracted_at)
  df
}
write_csv(norm_dt(candidates_all), file.path(FINAL_DIR, "PREIS_indicator_candidates.csv"))
cat("   [OK] PREIS_indicator_candidates.csv\n")
write_csv(norm_dt(validated), file.path(FINAL_DIR, "PREIS_indicators_validated.csv"))
cat("   [OK] PREIS_indicators_validated.csv\n")

long <- validated %>%
  dplyr::filter(qc_valid == TRUE) %>%
  dplyr::transmute(sitrep_no, indicator_code, domain, value, extraction_rule)
write_csv(long, file.path(FINAL_DIR, "PREIS_indicators_long.csv"))
cat("   [OK] PREIS_indicators_long.csv\n")

if (length(hz_acc) > 0) {
  hz_all <- dplyr::bind_rows(hz_acc)
  write_csv(hz_all, file.path(FINAL_DIR, "PREIS_health_zones.csv"))
  cat("   [OK] PREIS_health_zones.csv —", nrow(hz_all), "lignes\n")
}
if (!is.null(qc_res$qc_issues) && nrow(qc_res$qc_issues) > 0) {
  write_csv(norm_dt(qc_res$qc_issues), file.path(FINAL_DIR, "PREIS_QC_issues.csv"))
  cat("   [OK] PREIS_QC_issues.csv\n")
}
if (!is.null(qc_res$qc_by_sitrep) && nrow(qc_res$qc_by_sitrep) > 0) {
  write_csv(qc_res$qc_by_sitrep, file.path(FINAL_DIR, "PREIS_QC_by_sitrep.csv"))
  cat("   [OK] PREIS_QC_by_sitrep.csv\n")
}

# -- 8. Verifier les NOUVEAUX indicateurs --
cat("\n[7] Nouveaux indicateurs (patch 04/05) :\n")
new_inds <- c("doses_vaccine_administered","hcw_vaccinated","ring_vaccination_n",
              "contacts_followed_up","deaths_community",
              "hz_affected_ituri","hz_affected_nordkivu","hz_affected_sudkivu",
              "lab_positivity_rate")
found <- intersect(new_inds, unique(validated$indicator_code))
cat("   Trouves :", length(found), "/", length(new_inds), "\n")
if (length(found) > 0) {
  for (ind in found) {
    cat("     +", ind, ":", sum(validated$indicator_code == ind), "valeur(s)\n")
  }
} else {
  cat("   [INFO] Aucun nouveau indicateur. Voir diagnostic [8].\n")
}

# -- 9. Diagnostic texte brut --
cat("\n[8] Diagnostic (mots-cles dans un SitRep recent) :\n")
last_i <- length(pdf_no)
cat("   PDF analyse : SR", pdf_no[last_i], "\n")
diag_lines <- tryCatch({
  b <- extract_pdf_bundle(pdf_path[last_i], pdf_no[last_i], enable_tabulizer = FALSE)
  b$lines$line_text
}, error = function(e) character(0))

for (kw in c("vaccin","anneau","contact","communaut","Ituri","Nord-Kivu","positiv")) {
  hits <- grep(kw, diag_lines, value = TRUE, ignore.case = TRUE)
  if (length(hits) > 0) {
    cat("   [", kw, "] ", length(hits), " ligne(s). Ex: \"",
        substr(hits[1], 1, 85), "\"\n", sep = "")
  } else {
    cat("   [", kw, "] aucune ligne\n", sep = "")
  }
}

cat("\n============================================================\n")
cat("TERMINE :", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Etape suivante : source('scripts/92_qc_tableau_indicateurs.R')\n")
cat("============================================================\n")
