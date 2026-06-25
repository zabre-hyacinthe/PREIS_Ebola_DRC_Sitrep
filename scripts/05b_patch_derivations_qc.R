## ============================================================
## PREIS EBOLA DRC
## 05b_patch_derivations_qc.R   (v2 — calibre sur le vrai fichier)
##
## Script AUTONOME — modifie 05_qc_validate.R automatiquement
##
## AJOUTE UNIQUEMENT : lab_positivity_rate
##   = samples_positive / samples_analyzed x 100
##
## NOTE IMPORTANTE :
##   Le CFR, new_confirmed_cases et new_deaths sont DEJA derives
##   par ton code existant. Ce patch NE LES TOUCHE PAS (zero doublon).
##   Il insere lab_positivity_rate juste apres la derivation du CFR.
##
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/05b_patch_derivations_qc.R")
## ============================================================

cat("============================================================\n")
cat("PATCH 05 (v2) — Derivation lab_positivity_rate\n")
cat("Demarre :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

TARGET <- file.path(getwd(), "scripts/05_qc_validate.R")

if (!file.exists(TARGET)) {
  stop("Fichier non trouve : ", TARGET)
}

# -- 1. Sauvegarde --
backup <- paste0(tools::file_path_sans_ext(TARGET),
                 "_BACKUP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
file.copy(TARGET, backup)
cat("[OK] Sauvegarde :", basename(backup), "\n")

lines <- readLines(TARGET, warn = FALSE, encoding = "UTF-8")
cat("[OK] Script lu —", length(lines), "lignes\n")

# -- 2. Deja applique ? --
if (any(grepl("PATCH_05B_APPLIED", lines))) {
  cat("[INFO] Patch deja applique. Rien a faire.\n")
  message("[INFO] Patch 05 deja applique — arret propre.")
  return(invisible(NULL))
}

# -- 3. Point d'insertion --
# Juste AVANT la ligne qui filtre les new_* indicators.
# A cet endroit, le CFR est derive, les new cases pas encore.
insertion <- which(grepl(
  'validated\\s*<-\\s*validated\\s*%>%\\s*dplyr::filter\\(!indicator_code\\s*%in%\\s*c\\("new_confirmed_cases"',
  lines))

if (length(insertion) == 0) {
  stop("Point d'insertion (filtre new_*) non trouve.\n",
       "La structure de 05_qc_validate.R a peut-etre change.")
}

insert_at <- insertion[1]
cat("[OK] Point d'insertion trouve : ligne", insert_at,
    "(apres derivation CFR, avant filtre new_*)\n")

# -- 4. Bloc lab_positivity_rate --
new_block <- c(
  "",
  "  ## ---- PATCH_05B_APPLIED ---- NE PAS SUPPRIMER CETTE LIGNE ----",
  "  ## Derivation lab_positivity_rate ajoutee par 05b_patch_derivations_qc.R",
  "  ## = samples_positive / samples_analyzed x 100 (si absent)",
  "  for (s in snos) {",
  "    has_lpr <- any(validated$sitrep_no == s &",
  "                   validated$indicator_code == 'lab_positivity_rate' &",
  "                   validated$qc_valid == TRUE)",
  "    if (!has_lpr) {",
  "      spos  <- get_indicator_value(validated, s, 'samples_positive')",
  "      sanal <- get_indicator_value(validated, s, 'samples_analyzed')",
  "      if (!is.na(spos) && !is.na(sanal) && sanal > 0 && spos <= sanal) {",
  "        lpr <- round(100 * spos / sanal, 1)",
  "        validated <- dplyr::bind_rows(validated, tibble::tibble(",
  "          sitrep_no = s, indicator_code = 'lab_positivity_rate', domain = 'laboratory',",
  "          value = lpr, value_source = 'derived', source_type = 'qc_derivation',",
  "          extraction_rule = 'computed_positivity_from_samples_positive_analyzed',",
  "          priority = 50L, evidence = paste0('100*', spos, '/', sanal),",
  "          extracted_at = as.character(Sys.time()),",
  "          candidate_flag = 'candidate_ok', qc_valid = TRUE, qc_note = 'derived_after_qc'",
  "        ))",
  "      }",
  "    }",
  "  }",
  "  ## ---- FIN PATCH_05B ----------------------------------------",
  ""
)

# -- 5. Insertion --
lines_new <- c(
  lines[1:(insert_at - 1)],
  new_block,
  lines[insert_at:length(lines)]
)

# -- 6. Verification syntaxe --
tmp <- tempfile(fileext = ".R")
writeLines(lines_new, tmp, useBytes = TRUE)
check <- tryCatch(parse(file = tmp), error = function(e) e)
unlink(tmp)

if (inherits(check, "error")) {
  cat("[ERREUR] Syntaxe invalide :\n", conditionMessage(check), "\n")
  cat("Fichier original NON modifie. Sauvegarde :", basename(backup), "\n")
  stop("Patch 05 non applique.")
}
cat("[OK] Syntaxe R verifiee sans erreur\n")

# -- 7. Ecriture --
writeLines(lines_new, TARGET, useBytes = TRUE)
cat("[OK] 05_qc_validate.R mis a jour —", length(lines_new), "lignes\n")
cat("[OK] Derivation ajoutee : lab_positivity_rate\n")
cat("\nNOTE : CFR, new_confirmed_cases, new_deaths etaient deja derives\n")
cat("       par ton code — non touches (zero doublon).\n")
cat("============================================================\n")
cat("PATCH 05 termine :", format(Sys.time(), "%H:%M:%S"), "\n")
cat("============================================================\n")
