# =============================================================================
# 12_finalize_pdf_naming.R
# -----------------------------------------------------------------------------
# CORRECTION DEFINITIVE : normalise les noms de PDFs pour le master automation.
#
# Problème constaté : dans data/pdf/, 3 conventions coexistent :
#   A) SitRep_N063_2026-07-16.pdf        (v4/v5/v5.1)
#   B) PREIS_DRC_Ebola_SitRep_063.pdf    (master automation - reconnu)
#   C) SitRep_63_2026.pdf                (ancien - reconnu)
#
# Le master ne reconnaît que B et C. D'où 5 SR "manquants" du registre alors
# qu'ils sont sur disque.
#
# Ce script :
#   1. Détecte tous les PDFs (toutes conventions)
#   2. Renomme les PDFs "convention A" au format B (PREIS_DRC_Ebola_SitRep_NNN.pdf)
#   3. Supprime les doublons (garde la version la plus récente/complète)
#   4. Nettoie les fichiers _ND et _unknown quand une version datée existe
#   5. Log complet des renommages/suppressions
# =============================================================================

suppressPackageStartupMessages({
  library(stringr); library(dplyr); library(readr); library(tibble)
})

PDF_DIR <- "data/pdf"
LOG_DIR <- "data/logs"
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
log_msg <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), tz = "UTC"), m))

log_msg("=== Finalisation nommage PDFs ===")

# --- 1. Inventaire complet ---
all_pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = TRUE)
log_msg(sprintf("PDFs détectés : %d", length(all_pdfs)))

# --- 2. Extraction du numéro SitRep pour chaque fichier ---
extract_sitrep_num <- function(fname) {
  bn <- basename(fname)
  # Convention A : SitRep_NXXX_YYYY-MM-DD.pdf ou SitRep_NXXX_ND.pdf ou _unknown.pdf
  m <- stringr::str_match(bn, "SitRep_N(\\d{2,4})_")
  if (!is.na(m[1, 2])) return(as.integer(m[1, 2]))
  # Convention B : PREIS_DRC_Ebola_SitRep_XXX.pdf
  m <- stringr::str_match(bn, "PREIS_DRC_Ebola_SitRep_(\\d{2,4})\\.pdf")
  if (!is.na(m[1, 2])) return(as.integer(m[1, 2]))
  # Convention C : SitRep_XX_YYYY.pdf (X sans "N")
  m <- stringr::str_match(bn, "^SitRep_(\\d{2,4})_\\d{4}\\.pdf$")
  if (!is.na(m[1, 2])) return(as.integer(m[1, 2]))
  # Anciens formats : SITREP-MVE-NUM-XX.pdf, etc.
  m <- stringr::str_match(bn, "(?:NUM-|N°|Num-|N_|MVE_)(\\d{2,4})")
  if (!is.na(m[1, 2])) return(as.integer(m[1, 2]))
  m <- stringr::str_match(bn, "SITREP(\\d{2,4})")
  if (!is.na(m[1, 2])) return(as.integer(m[1, 2]))
  NA_integer_
}

# --- 3. Détection convention + priorité ---
detect_convention <- function(fname) {
  bn <- basename(fname)
  if (stringr::str_detect(bn, "^PREIS_DRC_Ebola_SitRep_\\d+\\.pdf$")) return("B_master")
  if (stringr::str_detect(bn, "^SitRep_\\d+_\\d{4}\\.pdf$")) return("C_year")
  if (stringr::str_detect(bn, "^SitRep_N\\d+_\\d{4}-\\d{2}-\\d{2}\\.pdf$")) return("A_v51_dated")
  if (stringr::str_detect(bn, "^SitRep_N\\d+_(ND|unknown)\\.pdf$")) return("A_v51_nodate")
  "Z_legacy"
}

inv <- tibble::tibble(
  path       = all_pdfs,
  fname      = basename(all_pdfs),
  size       = file.info(all_pdfs)$size,
  mtime      = file.info(all_pdfs)$mtime,
  num        = vapply(all_pdfs, extract_sitrep_num, integer(1)),
  convention = vapply(all_pdfs, detect_convention, character(1))
) %>% dplyr::filter(!is.na(num))

log_msg(sprintf("PDFs avec numéro SitRep identifié : %d", nrow(inv)))
log_msg(sprintf("N° uniques présents : %d", length(unique(inv$num))))

# --- 4. Résolution des doublons (priorité : B > A_dated > C > A_nodate > Z) ---
priority_order <- c("B_master" = 1, "A_v51_dated" = 2, "C_year" = 3,
                    "A_v51_nodate" = 4, "Z_legacy" = 5)

resolved <- inv %>%
  dplyr::mutate(priority = priority_order[convention]) %>%
  dplyr::arrange(num, priority, dplyr::desc(size), dplyr::desc(mtime)) %>%
  dplyr::group_by(num) %>%
  dplyr::mutate(keep = dplyr::row_number() == 1L) %>%
  dplyr::ungroup()

n_keep   <- sum(resolved$keep)
n_delete <- sum(!resolved$keep)
log_msg(sprintf("À conserver : %d | À supprimer (doublons) : %d", n_keep, n_delete))

# --- 5. Actions : rename + delete ---
audit <- tibble::tibble(
  timestamp = character(0), action = character(0),
  num = integer(0), from = character(0), to = character(0)
)

# Suppressions
for (i in which(!resolved$keep)) {
  old <- resolved$path[i]
  if (file.exists(old)) {
    file.remove(old)
    audit <- dplyr::bind_rows(audit, tibble::tibble(
      timestamp = format(Sys.time(), tz = "UTC"),
      action = "DELETE_DUPLICATE",
      num = resolved$num[i],
      from = resolved$fname[i], to = NA_character_))
    log_msg(sprintf("SUPPR   N%03d : %s", resolved$num[i], resolved$fname[i]))
  }
}

# Renommages : tout ce qui n'est pas convention B → convention B
kept <- resolved %>% dplyr::filter(keep)
for (i in seq_len(nrow(kept))) {
  n <- kept$num[i]
  target_name <- sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", n)
  target_path <- file.path(PDF_DIR, target_name)
  if (kept$fname[i] == target_name) next  # déjà au bon format
  if (file.exists(target_path)) next       # une version existe déjà (rare)
  file.rename(kept$path[i], target_path)
  audit <- dplyr::bind_rows(audit, tibble::tibble(
    timestamp = format(Sys.time(), tz = "UTC"),
    action = "RENAME",
    num = n,
    from = kept$fname[i], to = target_name))
  log_msg(sprintf("RENAME  N%03d : %s → %s", n, kept$fname[i], target_name))
}

# --- 6. Écriture audit ---
log_path <- file.path(LOG_DIR,
                       sprintf("pdf_naming_finalization_%s.csv",
                               format(Sys.Date(), "%Y-%m-%d")))
readr::write_csv(audit, log_path, na = "ND")
log_msg(sprintf("Audit : %s", log_path))

# --- 7. Vérification finale ---
final_files <- list.files(PDF_DIR, pattern = "^PREIS_DRC_Ebola_SitRep_\\d+\\.pdf$",
                          full.names = FALSE)
m <- stringr::str_match(final_files, "SitRep_(\\d+)\\.pdf$")
final_nums <- sort(unique(as.integer(m[, 2])))
final_nums <- final_nums[!is.na(final_nums)]

cat("\n=== Résumé final ===\n")
cat(sprintf("PDFs au format master : %d\n", length(final_files)))
cat(sprintf("SR uniques : %d | Range : N%03d - N%03d\n",
            length(final_nums), min(final_nums), max(final_nums)))
trous <- setdiff(seq(min(final_nums), max(final_nums)), final_nums)
cat(sprintf("Trous : %s\n",
            if (length(trous) == 0) "AUCUN" else paste(sprintf("N%03d", trous), collapse = " ")))
log_msg("=== Finalisation terminée ===")
log_msg("Étape suivante : Rscript scripts/00_PREIS_MASTER_AUTOMATION.R")
