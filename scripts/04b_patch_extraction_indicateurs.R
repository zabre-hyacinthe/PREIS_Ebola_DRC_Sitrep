## ============================================================
## PREIS EBOLA DRC
## 04b_patch_extraction_indicateurs.R   (v2 — calibre sur le vrai fichier)
##
## Script AUTONOME — modifie 04_extract_indicators.R automatiquement
## Ajoute les regles manquantes DANS extract_candidates_from_row_text()
## en utilisant la fonction add() et les helpers g_first/g_two existants.
##
## Indicateurs ajoutes :
##   - doses_vaccine_administered, hcw_vaccinated, ring_vaccination_n
##   - contacts_followed_up
##   - deaths_community
##   - hz_affected_ituri / nordkivu / sudkivu
##
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/04b_patch_extraction_indicateurs.R")
## ============================================================

cat("============================================================\n")
cat("PATCH 04 (v2) — Extraction indicateurs manquants\n")
cat("Demarre :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

TARGET <- file.path(getwd(), "scripts/04_extract_indicators.R")

if (!file.exists(TARGET)) {
  stop("Fichier non trouve : ", TARGET,
       "\nVerifier que setwd() pointe vers la racine du projet.")
}

# -- 1. Sauvegarde --
backup <- paste0(tools::file_path_sans_ext(TARGET),
                 "_BACKUP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
file.copy(TARGET, backup)
cat("[OK] Sauvegarde creee :", basename(backup), "\n")

lines <- readLines(TARGET, warn = FALSE, encoding = "UTF-8")
cat("[OK] Script lu —", length(lines), "lignes\n")

# -- 2. Deja applique ? --
if (any(grepl("PATCH_04B_APPLIED", lines))) {
  cat("[INFO] Patch deja applique. Rien a faire.\n")
  message("[INFO] Patch 04 deja applique — arret propre.")
  return(invisible(NULL))
}

# -- 3. Point d'insertion --
# La fonction extract_candidates_from_row_text() se termine par
# "  dplyr::bind_rows(out)" — on insere JUSTE AVANT cette ligne.
insertion <- which(grepl("^\\s*dplyr::bind_rows\\(out\\)\\s*$", lines))

if (length(insertion) == 0) {
  stop("Point d'insertion 'dplyr::bind_rows(out)' non trouve.\n",
       "La structure de 04_extract_indicators.R a peut-etre change.")
}

insert_at <- insertion[1]
cat("[OK] Point d'insertion trouve : ligne", insert_at,
    "(avant bind_rows(out))\n")

# -- 4. Bloc de regles a inserer --
new_block <- c(
  "",
  "  ## ---- PATCH_04B_APPLIED ---- NE PAS SUPPRIMER CETTE LIGNE ----",
  "  ## Regles ajoutees par 04b_patch_extraction_indicateurs.R",
  "  ## Reutilise add() et g_first() de la portee locale.",
  "",
  "  # Vaccination rVSV-ZEBOV : doses administrees",
  "  add('doses_vaccine_administered', g_first('doses?\\\\s+(?:de\\\\s+vaccin\\\\s+)?administr\\\\w+\\\\s+(\\\\d{1,7})', txt), 'vaccination', 'narr_vaccine_doses', 3)",
  "  add('doses_vaccine_administered', g_first('(\\\\d{1,7})\\\\s+personnes?\\\\s+(?:ont\\\\s+[e\u00e9]t[e\u00e9]\\\\s+)?vaccin', txt), 'vaccination', 'narr_persons_vaccinated', 4)",
  "  add('doses_vaccine_administered', g_first('total\\\\s+(?:de\\\\s+)?(\\\\d{1,7})\\\\s+(?:personnes?\\\\s+)?vaccin', txt), 'vaccination', 'narr_total_vaccinated', 4)",
  "",
  "  # Agents de sante vaccines (HCW)",
  "  add('hcw_vaccinated', g_first('(\\\\d{1,6})\\\\s+agents?\\\\s+de\\\\s+sant[e\u00e9]\\\\s+(?:ont\\\\s+[e\u00e9]t[e\u00e9]\\\\s+)?vaccin', txt), 'vaccination', 'narr_hcw_vaccinated', 4)",
  "  add('hcw_vaccinated', g_first('agents?\\\\s+de\\\\s+sant[e\u00e9]\\\\s+vaccin\\\\w+\\\\s*:?\\\\s*(\\\\d{1,6})', txt), 'vaccination', 'tbl_hcw_vaccinated', 3)",
  "",
  "  # Vaccination en anneau",
  "  add('ring_vaccination_n', g_first('vaccination\\\\s+en\\\\s+anneau\\\\s*:?\\\\s*(\\\\d{1,7})', txt), 'vaccination', 'tbl_ring_vaccination', 3)",
  "  add('ring_vaccination_n', g_first('(\\\\d{1,7})\\\\s+contacts?\\\\s+(?:et\\\\s+contacts?\\\\s+de\\\\s+contacts?\\\\s+)?vaccin', txt), 'vaccination', 'narr_ring_vaccination', 4)",
  "",
  "  # Contacts suivis (tracage)",
  "  add('contacts_followed_up', g_first('(\\\\d{1,7})\\\\s+contacts?\\\\s+(?:sous\\\\s+|en\\\\s+)?suivi', txt), 'contacts', 'narr_contacts_followed', 4)",
  "  add('contacts_followed_up', g_first('contacts?\\\\s+(?:sous\\\\s+|en\\\\s+)?suivi\\\\s*:?\\\\s*(\\\\d{1,7})', txt), 'contacts', 'tbl_contacts_followed', 3)",
  "  add('contacts_followed_up', g_first('(\\\\d{1,7})\\\\s+contacts?\\\\s+(?:ont\\\\s+[e\u00e9]t[e\u00e9]\\\\s+)?suivis', txt), 'contacts', 'narr_contacts_suivis', 4)",
  "",
  "  # Deces en communaute",
  "  add('deaths_community', g_first('(\\\\d{1,5})\\\\s+d[e\u00e9]c[e\u00e8]s\\\\s+(?:en\\\\s+|dans\\\\s+la\\\\s+)?communaut', txt), 'deaths', 'narr_deaths_community', 4)",
  "  add('deaths_community', g_first('d[e\u00e9]c[e\u00e8]s\\\\s+(?:en\\\\s+|dans\\\\s+la\\\\s+)?communaut\\\\w*\\\\s*:?\\\\s*(\\\\d{1,5})', txt), 'deaths', 'tbl_deaths_community', 3)",
  "",
  "  # Zones de sante affectees par province : format 'Ituri (19/36)'",
  "  add('hz_affected_ituri',    g_first('Ituri\\\\s*\\\\(?\\\\s*(\\\\d{1,2})\\\\s*/\\\\s*\\\\d{1,3}', txt), 'geography', 'hz_affected_ituri', 2)",
  "  add('hz_affected_nordkivu', g_first('Nord[- ]?Kivu\\\\s*\\\\(?\\\\s*(\\\\d{1,2})\\\\s*/\\\\s*\\\\d{1,3}', txt), 'geography', 'hz_affected_nordkivu', 2)",
  "  add('hz_affected_sudkivu',  g_first('Sud[- ]?Kivu\\\\s*\\\\(?\\\\s*(\\\\d{1,2})\\\\s*/\\\\s*\\\\d{1,3}', txt), 'geography', 'hz_affected_sudkivu', 2)",
  "  ## ---- FIN PATCH_04B ----------------------------------------",
  ""
)

# -- 5. Insertion --
lines_new <- c(
  lines[1:(insert_at - 1)],
  new_block,
  lines[insert_at:length(lines)]
)

# -- 6. Verification syntaxe R --
tmp <- tempfile(fileext = ".R")
writeLines(lines_new, tmp, useBytes = TRUE)
check <- tryCatch(parse(file = tmp), error = function(e) e)
unlink(tmp)

if (inherits(check, "error")) {
  cat("[ERREUR] Syntaxe invalide apres patch :\n", conditionMessage(check), "\n")
  cat("Fichier original NON modifie. Sauvegarde :", basename(backup), "\n")
  stop("Patch 04 non applique.")
}
cat("[OK] Syntaxe R verifiee sans erreur\n")

# -- 7. Ecriture --
writeLines(lines_new, TARGET, useBytes = TRUE)
cat("[OK] 04_extract_indicators.R mis a jour —", length(lines_new), "lignes\n")
cat("[OK] Bloc ajoute :", length(new_block), "lignes\n")

cat("\nIndicateurs maintenant extraits :\n")
for (ind in c("doses_vaccine_administered","hcw_vaccinated","ring_vaccination_n",
              "contacts_followed_up","deaths_community",
              "hz_affected_ituri","hz_affected_nordkivu","hz_affected_sudkivu")) {
  cat("  +", ind, "\n")
}
cat("\n============================================================\n")
cat("PATCH 04 termine :", format(Sys.time(), "%H:%M:%S"), "\n")
cat("============================================================\n")
