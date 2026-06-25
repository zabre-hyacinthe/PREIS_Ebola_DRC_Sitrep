## ============================================================
## PREIS EBOLA RDC — INSTALLATION DES CORRECTIONS
## INSTALL_corrections.R
##
## Automatise les étapes de mise en place du système fiabilisé :
##   1+2. Copie 10_sitrep_identity.R et 08 corrigé dans scripts/
##        (sauvegarde des anciennes versions dans scripts/_backup_AAAAMMJJ/)
##   3.   Désactive la tâche planifiée locale (anti-doublon)
##   5.   Lance l'auto-test du module d'identité
##
## L'étape 4 (vérifier le .yml sur GitHub) reste manuelle : ce fichier
## est sur le dépôt distant, pas sur ta machine.
##
## USAGE :
##   1. Place ce script + 10_sitrep_identity.R + 08_cloud_sitrep_monitor.R
##      (versions corrigées) dans un dossier, ex. le dossier de téléchargement.
##   2. Adapte SOURCE_DIR ci-dessous vers ce dossier.
##   3. source("INSTALL_corrections.R")
## ============================================================

# ---- À ADAPTER ----
PROJET_DIR <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"          # racine du projet
SOURCE_DIR <- file.path(Sys.getenv("USERPROFILE"), "Downloads") # où sont les fichiers corrigés
TASK_NAME  <- "PREIS_Ebola_SitRep_Monitor"                      # tâche locale à désactiver

SCRIPTS_DIR <- file.path(PROJET_DIR, "scripts")

cat("\n============================================================\n")
cat("PREIS Ebola — Installation des corrections\n")
cat("============================================================\n")
cat("Projet :", PROJET_DIR, "\n")
cat("Source :", SOURCE_DIR, "\n\n")

stopifnot(dir.exists(PROJET_DIR))
if (!dir.exists(SCRIPTS_DIR)) dir.create(SCRIPTS_DIR, recursive = TRUE)

# ------------------------------------------------------------
# Sauvegarde des versions actuelles
# ------------------------------------------------------------
backup_dir <- file.path(SCRIPTS_DIR, paste0("_backup_", format(Sys.Date(), "%Y%m%d")))
dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)

files_to_install <- c("10_sitrep_identity.R", "08_cloud_sitrep_monitor.R",
                      "03_analyse_consolidee.R", "04_send_sitrep_alerts_conditional.R",
                      "05_synthese_narrative.R", "00_PREIS_MASTER_AUTOMATION.R")

# Étapes 1 + 2 : installation avec sauvegarde
for (f in files_to_install) {
  src <- file.path(SOURCE_DIR, f)
  dst <- file.path(SCRIPTS_DIR, f)
  
  if (!file.exists(src)) {
    cat(sprintf("[ATTENTION] Introuvable dans Source : %s — étape ignorée.\n", f))
    next
  }
  # Sauvegarde de l'ancienne version si elle existe
  if (file.exists(dst)) {
    file.copy(dst, file.path(backup_dir, f), overwrite = TRUE)
    cat(sprintf("[backup] Ancienne version sauvegardée : %s\n", f))
  }
  ok <- file.copy(src, dst, overwrite = TRUE)
  cat(sprintf("[%s] Installé : scripts/%s\n", if (ok) "OK" else "ECHEC", f))
}

# ------------------------------------------------------------
# Étape 3 : désactiver la tâche planifiée locale (Windows)
# ------------------------------------------------------------
cat("\n--- Désactivation de la tâche locale (anti-doublon) ---\n")
if (.Platform$OS.type == "windows") {
  # Vérifie d'abord si la tâche existe
  q <- tryCatch(
    suppressWarnings(system2("schtasks", c("/Query", "/TN", shQuote(TASK_NAME)),
                             stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0))
  exists_task <- !any(grepl("ERROR|introuvable|cannot find", q, ignore.case = TRUE)) && length(q) > 0
  
  if (exists_task) {
    r <- tryCatch(
      suppressWarnings(system2("schtasks", c("/Change", "/TN", shQuote(TASK_NAME), "/DISABLE"),
                               stdout = TRUE, stderr = TRUE)),
      error = function(e) conditionMessage(e))
    cat("[OK] Tâche désactivée :", TASK_NAME, "\n")
    cat("    (réactivable via : schtasks /Change /TN \"", TASK_NAME, "\" /ENABLE)\n", sep="")
  } else {
    cat("[info] Tâche", TASK_NAME, "non trouvée — rien à désactiver.\n")
  }
} else {
  cat("[info] Pas Windows — désactivation de tâche ignorée.\n")
}

# ------------------------------------------------------------
# Étape 5 : auto-test du module d'identité
# ------------------------------------------------------------
cat("\n--- Auto-test du module d'identité ---\n")
identity_fp <- file.path(SCRIPTS_DIR, "10_sitrep_identity.R")
if (file.exists(identity_fp)) {
  # Exécute le fichier comme script principal pour déclencher son auto-test
  res <- tryCatch(
    system2(file.path(R.home("bin"), "Rscript"), shQuote(identity_fp),
            stdout = TRUE, stderr = TRUE),
    error = function(e) paste("Erreur:", conditionMessage(e)))
  cat(paste(res, collapse = "\n"), "\n")
} else {
  cat("[ATTENTION] Module d'identité non installé — test impossible.\n")
}

# ------------------------------------------------------------
# Rappel étape 4 (manuelle)
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("INSTALLATION TERMINÉE\n")
cat("============================================================\n")
cat("\nÉtape 4 (MANUELLE, sur GitHub) :\n")
cat("  - Mets à jour .github/workflows/preis_sitrep_monitor.yml\n")
cat("    (cron 30 min + packages sf/ggplot2/tidyr/lubridate)\n")
cat("  - Pousse scripts/10_sitrep_identity.R et 08 corrigé sur le dépôt\n")
cat("  - Déclenche le workflow (onglet Actions -> Run workflow)\n\n")
cat("Sauvegardes des anciennes versions :", backup_dir, "\n")