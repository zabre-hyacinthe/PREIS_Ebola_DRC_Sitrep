## ============================================================
## PREIS EBOLA DRC
## 00b_patch_gros_pipeline.R
##
## Ajoute les memes regles (vaccination, contacts, zones par
## province) dans extract_indicators() du GROS pipeline
## 00_PREIS_MASTER_AUTOMATION_CORRIGE.R — celui que le CLOUD utilise.
##
## Utilise SA structure : add(code,val,rule,domain), g(pattern), flat.
## Insere juste AVANT le bloc CFR derive, dans extract_indicators().
##
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/00b_patch_gros_pipeline.R")
## ============================================================

cat("============================================================\n")
cat("PATCH 00 — Gros pipeline cloud (extract_indicators)\n")
cat("Demarre :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

# Cibler les DEUX gros scripts si presents (corrige + non corrige)
# pour garantir la coherence quel que soit celui charge par le cloud.
targets <- c("00_PREIS_MASTER_AUTOMATION_CORRIGE.R",
             "00_PREIS_MASTER_AUTOMATION.R")

patch_one <- function(rel) {
  TARGET <- file.path(getwd(), "scripts", rel)
  if (!file.exists(TARGET)) {
    cat("[skip]", rel, "absent\n"); return(invisible(FALSE))
  }
  cat("\n>>> Traitement :", rel, "\n")

  backup <- paste0(tools::file_path_sans_ext(TARGET),
                   "_BACKUP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
  file.copy(TARGET, backup)
  cat("   [OK] Sauvegarde :", basename(backup), "\n")

  lines <- readLines(TARGET, warn = FALSE, encoding = "UTF-8")

  if (any(grepl("PATCH_00B_APPLIED", lines))) {
    cat("   [INFO] Deja patche. Ignore.\n"); return(invisible(TRUE))
  }

  # Point d'insertion : la ligne du bloc CFR derive DANS extract_indicators
  # '  if (!("case_fatality_ratio" %in% seen)) {'
  ins <- which(grepl('if \\(!\\("case_fatality_ratio" %in% seen\\)\\)', lines))
  if (length(ins) == 0) {
    cat("   [ERREUR] Point d'insertion (bloc CFR) non trouve. Patch annule.\n")
    return(invisible(FALSE))
  }
  insert_at <- ins[1]
  cat("   [OK] Point d'insertion : ligne", insert_at, "(avant bloc CFR)\n")

  new_block <- c(
    "  ## ---- PATCH_00B_APPLIED ---- regles ajoutees (cloud) ----",
    "  ## vaccination, contacts suivis, zones par province",
    "  ## Utilise add() et g() locaux, et 'flat' (texte du SitRep).",
    "  add('doses_vaccine_administered', g('doses?\\\\s+(?:de\\\\s+vaccin\\\\s+)?administr\\\\w+\\\\s+(\\\\d{1,7})'), 'regex_vaccine_doses', 'vaccination')",
    "  add('doses_vaccine_administered', g('(\\\\d{1,7})\\\\s+personnes?\\\\s+(?:ont\\\\s+[e\u00e9]t[e\u00e9]\\\\s+)?vaccin'), 'regex_persons_vaccinated', 'vaccination')",
    "  add('hcw_vaccinated', g('(\\\\d{1,6})\\\\s+agents?\\\\s+de\\\\s+sant[e\u00e9]\\\\s+(?:ont\\\\s+[e\u00e9]t[e\u00e9]\\\\s+)?vaccin'), 'regex_hcw_vaccinated', 'vaccination')",
    "  add('ring_vaccination_n', g('vaccination\\\\s+en\\\\s+anneau\\\\s*:?\\\\s*(\\\\d{1,7})'), 'regex_ring_vaccination', 'vaccination')",
    "  add('contacts_followed_up', g('(\\\\d{1,7})\\\\s+contacts?\\\\s+(?:sous\\\\s+|en\\\\s+)?suivi'), 'regex_contacts_followup', 'contacts')",
    "  add('contacts_followed_up', g('contacts?\\\\s+(?:sous\\\\s+|en\\\\s+)?suivi\\\\s*:?\\\\s*(\\\\d{1,7})'), 'regex_contacts_followup2', 'contacts')",
    "  add('deaths_community', g('(\\\\d{1,5})\\\\s+d[e\u00e9]c[e\u00e8]s\\\\s+(?:en\\\\s+|dans\\\\s+la\\\\s+)?communaut'), 'regex_deaths_community', 'deaths')",
    "  add('hz_affected_ituri',    g('Ituri\\\\s*\\\\(?\\\\s*(\\\\d{1,2})\\\\s*/\\\\s*\\\\d{1,3}'), 'regex_hz_ituri', 'geography')",
    "  add('hz_affected_nordkivu', g('Nord[- ]?Kivu\\\\s*\\\\(?\\\\s*(\\\\d{1,2})\\\\s*/\\\\s*\\\\d{1,3}'), 'regex_hz_nordkivu', 'geography')",
    "  add('hz_affected_sudkivu',  g('Sud[- ]?Kivu\\\\s*\\\\(?\\\\s*(\\\\d{1,2})\\\\s*/\\\\s*\\\\d{1,3}'), 'regex_hz_sudkivu', 'geography')",
    "  ## ---- FIN PATCH_00B ----",
    ""
  )

  lines_new <- c(lines[1:(insert_at - 1)], new_block, lines[insert_at:length(lines)])

  tmp <- tempfile(fileext = ".R")
  writeLines(lines_new, tmp, useBytes = TRUE)
  chk <- tryCatch(parse(file = tmp), error = function(e) e)
  unlink(tmp)
  if (inherits(chk, "error")) {
    cat("   [ERREUR] Syntaxe invalide :", conditionMessage(chk), "\n")
    cat("   Fichier NON modifie. Sauvegarde :", basename(backup), "\n")
    return(invisible(FALSE))
  }

  writeLines(lines_new, TARGET, useBytes = TRUE)
  cat("   [OK]", rel, "patche —", length(lines_new), "lignes\n")
  invisible(TRUE)
}

for (t in targets) patch_one(t)

cat("\n============================================================\n")
cat("PATCH 00 termine :", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Le gros pipeline (cloud) extrait maintenant les memes indicateurs.\n")
cat("============================================================\n")
