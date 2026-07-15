############################################################
# PREIS Ebola RDC — 06_normalize_pdf_names.R
#
# Objectif :
#   Uniformiser tous les noms de fichiers PDFs dans data/pdf/
#   au format canonique  SitRep_NN_2026.pdf  (NN sur 2 chiffres).
#
#   Historique du probleme :
#     - SR 01-40 sont nommes    SitRep_XX_2026.pdf         (OK)
#     - SR 44-55 sont nommes    PREIS_DRC_Ebola_SitRep_0XX.pdf
#     - Nombreux legacy         SITREP-MVE-NUM-XX-1.pdf, etc.
#   Sans normalisation, les regex des scripts d'extraction
#   ratent les fichiers avec le "mauvais" pattern.
#
# Sortie :
#   - Renommage in-place dans data/pdf/
#   - Log CSV dans data/logs/normalize_pdf_YYYYMMDD.csv
#   - Doublons detectes : on garde le plus gros, l'autre passe
#     en .dupe.bak (bloque par .gitignore)
#
# Ne modifie aucun contenu de PDF. Aucune extraction ici.
############################################################

suppressPackageStartupMessages({
  library(stringr); library(readr); library(tibble); library(dplyr)
})

ROOT   <- Sys.getenv("GITHUB_WORKSPACE", unset = getwd())
ROOT   <- normalizePath(ROOT, winslash = "/", mustWork = FALSE)
PDF_DIR <- file.path(ROOT, "data", "pdf")
LOG_DIR <- file.path(ROOT, "data", "logs")
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

LOG_FP <- file.path(LOG_DIR,
                    paste0("normalize_pdf_", format(Sys.Date(), "%Y%m%d"), ".csv"))

log_msg <- function(...) {
  cat("[normalize_pdf] ",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ",
      paste0(..., collapse = ""), "\n", sep = "")
}

if (!dir.exists(PDF_DIR)) {
  log_msg("data/pdf/ absent — rien a normaliser.")
  quit(save = "no", status = 0)
}

pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = FALSE,
                   ignore.case = TRUE)
log_msg(length(pdfs), " PDFs candidats.")

# ---- Extraction du numero de SitRep ----
# Ordre des patterns : du plus specifique au plus permissif.
extract_sitrep_no <- function(fn) {
  patterns <- c(
    # PREIS_DRC_Ebola_SitRep_055.pdf
    "(?i)PREIS[_ -]?DRC[_ -]?Ebola[_ -]?SitRep[_ -]?0*([0-9]+)",
    # Draft-Final_SitRep_MVE_RDC_N°030_...
    "(?i)SitRep[_ ]?MVE[_ -]?RDC[_ -]?N?[°o]?0*([0-9]+)",
    # SitRep_40_2026.pdf
    "(?i)SitRep[_ ]0*([0-9]+)[_ -]?2026",
    # SITREP-MVE-NUM-20-1.pdf, SITREP-MVE-N-04.pdf
    "(?i)SITREP[_ -]?MVE[_ -]?(?:NUM[_ -]?)?N?[°o]?0*([0-9]+)",
    # SITREP36_MVE16.pdf, SITREP38_MVE161.pdf : le PREMIER nombre est le SR
    "(?i)^SITREP0*([0-9]+)[_ -]?MVE",
    # SitRep_N°01_04_09_2025-Version-part.pdf
    "(?i)SitRep[_ ]?N?[°o]?0*([0-9]+)[_ -]",
    # SITREP-NUM-13-.pdf
    "(?i)SITREP[_ -]?NUM[_ -]?0*([0-9]+)",
    # Fallback tres permissif : SitRep suivi d'un nombre
    "(?i)SitRep[_ -]?0*([0-9]+)"
  )
  for (p in patterns) {
    m <- str_match(fn, p)[, 2]
    if (!is.na(m)) {
      n <- suppressWarnings(as.integer(m))
      if (!is.na(n) && n >= 1 && n <= 999) return(n)
    }
  }
  NA_integer_
}

log_rows <- tibble(orig = character(), target = character(),
                   action = character(), size_before = double(),
                   size_after = double())

for (fn in pdfs) {
  sno <- extract_sitrep_no(fn)
  src <- file.path(PDF_DIR, fn)
  sz_src <- tryCatch(file.info(src)$size, error = function(e) NA_real_)

  if (is.na(sno)) {
    log_rows <- bind_rows(log_rows, tibble(
      orig = fn, target = NA_character_, action = "skip_no_match",
      size_before = sz_src, size_after = NA_real_))
    next
  }

  target <- sprintf("SitRep_%02d_2026.pdf", sno)
  dst <- file.path(PDF_DIR, target)

  if (fn == target) {
    log_rows <- bind_rows(log_rows, tibble(
      orig = fn, target = target, action = "already_ok",
      size_before = sz_src, size_after = sz_src))
    next
  }

  if (file.exists(dst)) {
    sz_dst <- file.info(dst)$size
    if (!is.na(sz_src) && !is.na(sz_dst) && sz_src > sz_dst) {
      # source plus grosse : on remplace
      bak <- paste0(dst, ".dupe.bak")
      file.rename(dst, bak)
      file.rename(src, dst)
      log_rows <- bind_rows(log_rows, tibble(
        orig = fn, target = target, action = "replaced_larger",
        size_before = sz_src, size_after = sz_src))
      log_msg("REMPLACE ", fn, " -> ", target, " (source plus grosse)")
    } else {
      # cible plus grosse ou egale : on garde, source passee en .dupe.bak
      bak <- paste0(src, ".dupe.bak")
      file.rename(src, bak)
      log_rows <- bind_rows(log_rows, tibble(
        orig = fn, target = target, action = "kept_existing",
        size_before = sz_src, size_after = sz_dst))
      log_msg("DOUBLON ", fn, " -> .dupe.bak (", target, " deja plus grosse)")
    }
  } else {
    file.rename(src, dst)
    log_rows <- bind_rows(log_rows, tibble(
      orig = fn, target = target, action = "renamed",
      size_before = sz_src, size_after = sz_src))
    log_msg("RENOMMAGE ", fn, " -> ", target)
  }
}

write_csv(log_rows, LOG_FP)
log_msg("Log ecrit : ", LOG_FP)
log_msg("Total : ",
        sum(log_rows$action == "renamed"),        " renommes, ",
        sum(log_rows$action == "already_ok"),     " deja OK, ",
        sum(log_rows$action == "replaced_larger"),  " remplaces, ",
        sum(log_rows$action == "kept_existing"),  " doublons ecartes, ",
        sum(log_rows$action == "skip_no_match"),  " sans match.")
