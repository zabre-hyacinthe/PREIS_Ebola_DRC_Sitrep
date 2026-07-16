############################################################
# PREIS EBOLA RDC
# INSTALLATION SECURISEE DU SCRIPT EMAIL/PDF CORRIGE
############################################################

project_dir <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
target_file <- file.path(project_dir, "scripts", "preis_safe_scientific_email.R")
candidate_file <- file.path(project_dir, "scripts", "preis_safe_scientific_email_fixed.R")

if (!file.exists(target_file)) {
  stop("Fichier cible introuvable : ", target_file)
}
if (!file.exists(candidate_file)) {
  stop(
    "Fichier corrige introuvable : ", candidate_file,
    "\nPlace d'abord preis_safe_scientific_email_fixed.R dans le dossier scripts."
  )
}

# Validation syntaxique AVANT toute modification
parse_error <- tryCatch({
  invisible(parse(file = candidate_file))
  NULL
}, error = function(e) conditionMessage(e))

if (!is.null(parse_error)) {
  stop(
    "Le fichier corrige contient une erreur de syntaxe. Le fichier actuel n'a pas ete touche.\n",
    parse_error
  )
}

candidate_text <- paste(readLines(candidate_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
required_markers <- c(
  "sitrep_no <- as.integer(sitrep_number)",
  "PREIS_PDF_ATTACHED",
  "pdf_path.txt",
  "preis_safe_probe_pdf_url",
  "preis_safe_wp_media_urls"
)
missing_markers <- required_markers[!vapply(required_markers, grepl, logical(1), x = candidate_text, fixed = TRUE)]
if (length(missing_markers) > 0) {
  stop(
    "Le fichier candidat est incomplet. Marqueurs manquants : ",
    paste(missing_markers, collapse = ", ")
  )
}
if (grepl("safe_log\\s*\\(", candidate_text, perl = TRUE)) {
  stop("Le fichier candidat contient encore safe_log().")
}
if (length(gregexpr("if \\(already && !force_send\\)", candidate_text, perl = TRUE)[[1]]) != 1L) {
  stop("Le controle anti-doublon n'apparait pas exactement une fois.")
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_file <- paste0(target_file, ".BACKUP_BEFORE_FULL_FIX_", stamp)
if (!file.copy(target_file, backup_file, overwrite = FALSE)) {
  stop("Impossible de creer la sauvegarde : ", backup_file)
}

if (!file.copy(candidate_file, target_file, overwrite = TRUE)) {
  file.copy(backup_file, target_file, overwrite = TRUE)
  stop("Echec du remplacement. L'ancienne version a ete restauree.")
}

final_error <- tryCatch({
  invisible(parse(file = target_file))
  NULL
}, error = function(e) conditionMessage(e))

if (!is.null(final_error)) {
  file.copy(backup_file, target_file, overwrite = TRUE)
  stop(
    "Validation finale echouee. L'ancienne version a ete restauree.\n",
    final_error
  )
}

cat("\nINSTALLATION REUSSIE\n")
cat("Fichier installe :", target_file, "\n")
cat("Sauvegarde :", backup_file, "\n")
cat("Syntaxe R : OK\n")
cat("Aucun email envoye.\n")
