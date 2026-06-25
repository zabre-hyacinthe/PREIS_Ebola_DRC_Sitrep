## ============================================================
## PREIS EBOLA DRC
## 00_RUN_PATCHES.R   (v2 — robuste, continue meme si une etape echoue)
##
## Script MAITRE — applique tous les patchs + genere le tableau QC.
##
## UNE SEULE COMMANDE :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/00_RUN_PATCHES.R")
## ============================================================

cat("\n")
cat("============================================================\n")
cat("  PREIS EBOLA DRC — APPLICATION DES PATCHS + QC COMPLET\n")
cat("  Demarre :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n\n")

ROOT <- getwd()
if (!file.exists(file.path(ROOT, "scripts/00_config.R"))) {
  stop(
    "ERREUR : setwd() ne pointe pas vers la racine du projet.\n",
    "Faire d'abord : setwd('D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26')\n"
  )
}

# Compteur de resultats
.results <- list()

.step <- function(n, titre) {
  cat("\n------------------------------------------------------------\n")
  cat(sprintf("ETAPE %d / 5 — %s\n", n, titre))
  cat("------------------------------------------------------------\n")
}

# Execute un patch de facon isolee : un echec n'arrete pas le reste
.run_patch <- function(label, script_name) {
  fp <- file.path(ROOT, "scripts", script_name)
  if (!file.exists(fp)) {
    cat("[ERREUR]", label, ": script non trouve —", script_name, "\n")
    .results[[label]] <<- "ABSENT"
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    # Environnement isole pour eviter les collisions de variables
    env <- new.env(parent = globalenv())
    sys.source(fp, envir = env)
    .results[[label]] <<- "OK"
    TRUE
  }, error = function(e) {
    cat("[ERREUR]", label, ":", conditionMessage(e), "\n")
    .results[[label]] <<- paste0("ECHEC: ", conditionMessage(e))
    FALSE
  })
  invisible(ok)
}

# -- ETAPE 1 — Patch extraction indicateurs --
.step(1, "Patch 04 — extraction vaccination + contacts + zones")
.run_patch("Patch 04", "04b_patch_extraction_indicateurs.R")

# -- ETAPE 2 — Patch derivations QC --
.step(2, "Patch 05 — derivations automatiques (lab_rate, new_cases...)")
.run_patch("Patch 05", "05b_patch_derivations_qc.R")

# -- ETAPE 3 — Patch moniteur faux positif --
.step(3, "Patch 08 — correction faux positif SitRep N752")
.run_patch("Patch 08", "08b_patch_moniteur_faux_positif.R")

# -- ETAPE 4 — Relance pipeline complet --
.step(4, "Pipeline complet sur tous les SitReps (force_reextract = TRUE)")
pipeline_ok <- tryCatch({
  cat("Chargement de la configuration...\n")
  source(file.path(ROOT, "scripts/00_config.R"))
  source(file.path(ROOT, "scripts/07_run_pipeline.R"))
  cat("Lancement du pipeline (2-5 min)...\n\n")
  run_preis_pipeline(force_redownload = FALSE, force_reextract = TRUE)
  .results[["Pipeline"]] <- "OK"
  cat("[OK] Pipeline termine\n")
  TRUE
}, error = function(e) {
  cat("[ERREUR] Pipeline :", conditionMessage(e), "\n")
  cat("Le tableau QC sera quand meme genere avec les donnees existantes.\n")
  .results[["Pipeline"]] <- paste0("ECHEC: ", conditionMessage(e))
  FALSE
})

# -- ETAPE 5 — Tableau QC Excel --
.step(5, "Generation tableau QC Excel complet")
tryCatch({
  source(file.path(ROOT, "scripts/92_qc_tableau_indicateurs.R"))
  .results[["Tableau QC"]] <- "OK"
}, error = function(e) {
  cat("[ERREUR] Generation Excel :", conditionMessage(e), "\n")
  .results[["Tableau QC"]] <- paste0("ECHEC: ", conditionMessage(e))
})

# -- Resume final --
cat("\n")
cat("============================================================\n")
cat("  RESUME FINAL\n")
cat("============================================================\n")
for (label in names(.results)) {
  status <- .results[[label]]
  tag <- if (status == "OK") "[OK]   " else "[ECHEC]"
  cat(sprintf("%s %-14s %s\n", tag, label,
              if (status == "OK") "" else paste0("— ", status)))
}
cat("\n")
cat("Resultats dans :\n")
cat("  outputs/audit/PREIS_QC_indicateurs_*.xlsx\n")
cat("  data/final/PREIS_indicator_candidates.csv\n")
cat("  data/final/PREIS_indicators_validated.csv\n")
cat("\nTermine :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")
