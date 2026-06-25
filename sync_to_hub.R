# =============================================================================
# PREIS — SYNCHRONISATION VERS LE HUB CENTRAL
# Fichier : sync_to_hub.R
# Auteur  : Dr R. Hyacinthe ZABRE — Africa CDC
# Version : 1.0 — 17 juin 2026
#
# RÔLE
# ----
# Constitue la BASE DE DONNÉES UNIFIÉE PREIS ("unified DB").
# Chaque module produit ses 4 fichiers communs dans SON propre dossier.
# Ce script les RASSEMBLE dans un dépôt central unique (le HUB), préfixés
# par module, après vérification d'intégrité.
#
# PRINCIPE D'ISOLATION (non négociable)
# -------------------------------------
# - Un module n'écrit JAMAIS dans le dossier d'un autre.
# - Ce script COPIE (ne déplace pas) : les dossiers sources restent intacts.
# - Chaque fichier est préfixé par son module : aucune collision possible.
# - Si un module est absent/incomplet, les AUTRES ne sont pas affectés.
#
# CHAÎNE COMPLÈTE
# ---------------
#   adapter_ebola.R  -> D:/PREIS_Ebola.../data/final/preis_common/
#   adapter_polio.R  -> D:/PREIS_Polio_FV/data/final/preis_common/
#        |
#        v  (ce script)
#   D:/PREIS_HUB/data/            <- base unifiée (préfixée par module)
#        |
#        v
#   dashboards, alertes, analyses  <- lisent le HUB
#
# MIGRATION GITHUB (plus tard)
# ----------------------------
# Quand la chaîne locale est validée, décommenter la section "PUSH GITHUB"
# en bas : une seule commande git transforme le HUB local en dépôt cloud.
# =============================================================================


# --------------------------------------------------------------------------- #
# 0. CONFIGURATION — adapter ces chemins à votre PC si besoin
# --------------------------------------------------------------------------- #

suppressPackageStartupMessages({
  library(jsonlite)
})

# Le HUB central (créé automatiquement s'il n'existe pas)
HUB_DIR      <- Sys.getenv("PREIS_HUB", "D:/PREIS_HUB")
HUB_DATA_DIR <- file.path(HUB_DIR, "data")

# Registre des modules : id + chemin de leur dossier preis_common
# Pour ajouter un module : ajouter une ligne ici. RIEN d'autre à changer.
MODULES_SOURCES <- list(
  ebola = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26/data/final/preis_common",
  polio = "D:/PREIS_Polio_FV/data/final/preis_common"
  # mpox = "D:/PREIS_Mpox_FV/data/final/preis_common"
)

# Les 4 fichiers du format commun attendus par module
COMMON_FILES <- c("preis_series.csv", "preis_zones.csv",
                  "preis_signals.csv", "preis_meta.json")

cat("=== PREIS — Synchronisation vers le HUB central ===\n")
cat("HUB :", HUB_DIR, "\n")
cat("Timestamp :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# --------------------------------------------------------------------------- #
# 1. PRÉPARATION DU HUB
# --------------------------------------------------------------------------- #

if (!dir.exists(HUB_DATA_DIR)) {
  dir.create(HUB_DATA_DIR, recursive = TRUE)
  cat("Dossier HUB créé :", HUB_DATA_DIR, "\n")
}


# --------------------------------------------------------------------------- #
# 2. FONCTION DE SYNCHRO D'UN MODULE
#    Copie les 4 fichiers d'un module vers le HUB, préfixés, après contrôle.
#    Retourne un résumé du statut du module.
# --------------------------------------------------------------------------- #

sync_module <- function(module_id, source_dir) {
  cat("-- Module :", module_id, "--\n")
  
  result <- list(
    module       = module_id,
    source_dir   = source_dir,
    files_copied = character(0),
    files_missing= character(0),
    n_rows       = list(),
    status       = "ok",
    synced_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  
  # Le dossier source existe-t-il ?
  if (!dir.exists(source_dir)) {
    cat("   AVERTISSEMENT : dossier source introuvable. Module ignoré.\n")
    cat("   ", source_dir, "\n")
    result$status <- "source_missing"
    return(result)
  }
  
  # Copier chaque fichier commun, préfixé par module
  for (f in COMMON_FILES) {
    src <- file.path(source_dir, f)
    dst <- file.path(HUB_DATA_DIR, paste0(module_id, "_", f))
    
    if (!file.exists(src)) {
      cat("   MANQUANT :", f, "\n")
      result$files_missing <- c(result$files_missing, f)
      next
    }
    
    # Contrôle d'intégrité minimal : fichier non vide
    if (file.size(src) == 0) {
      cat("   VIDE (ignoré) :", f, "\n")
      result$files_missing <- c(result$files_missing, f)
      next
    }
    
    # Compter les lignes pour les CSV (contrôle de cohérence)
    if (grepl("\\.csv$", f)) {
      n <- tryCatch(length(readLines(src)) - 1, error = function(e) NA)
      result$n_rows[[f]] <- n
    }
    
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (ok) {
      cat("   OK :", f, "->", basename(dst),
          if (!is.null(result$n_rows[[f]])) paste0(" (", result$n_rows[[f]], " lignes)") else "",
          "\n")
      result$files_copied <- c(result$files_copied, basename(dst))
    } else {
      cat("   ÉCHEC COPIE :", f, "\n")
      result$files_missing <- c(result$files_missing, f)
    }
  }
  
  # Statut final du module
  if (length(result$files_missing) == length(COMMON_FILES)) {
    result$status <- "empty"
  } else if (length(result$files_missing) > 0) {
    result$status <- "partial"
  } else {
    result$status <- "complete"
  }
  
  cat("   Statut :", toupper(result$status), "\n\n")
  result
}


# --------------------------------------------------------------------------- #
# 3. SYNCHRO DE TOUS LES MODULES
# --------------------------------------------------------------------------- #

all_results <- list()
for (mid in names(MODULES_SOURCES)) {
  all_results[[mid]] <- sync_module(mid, MODULES_SOURCES[[mid]])
}


# --------------------------------------------------------------------------- #
# 4. REGISTRE CENTRAL DES MODULES (manifest du HUB)
#    Un fichier JSON qui décrit l'état de la base unifiée.
#    Les consommateurs (dashboard) le lisent pour savoir quels modules
#    sont disponibles, sans avoir à scanner le dossier.
# --------------------------------------------------------------------------- #

cat("-- Construction du registre central --\n")

# Lire chaque meta de module pour enrichir le registre
modules_registry <- lapply(names(all_results), function(mid) {
  r <- all_results[[mid]]
  meta_file <- file.path(HUB_DATA_DIR, paste0(mid, "_preis_meta.json"))
  meta <- if (file.exists(meta_file))
    tryCatch(fromJSON(meta_file, simplifyVector = TRUE), error = function(e) NULL)
  else NULL
  
  list(
    module           = mid,
    label            = if (!is.null(meta)) meta$module_label else mid,
    sync_status      = r$status,
    system_status    = if (!is.null(meta)) meta$system_status else "no_data",
    last_report_date = if (!is.null(meta)) meta$last_report_date else NA,
    n_signals        = if (!is.null(meta)) meta$n_signals else 0,
    files            = r$files_copied,
    synced_at        = r$synced_at
  )
})
names(modules_registry) <- names(all_results)

hub_manifest <- list(
  hub_version     = "1.0",
  generated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  n_modules       = length(modules_registry),
  modules         = modules_registry,
  common_files    = COMMON_FILES,
  note            = "Base de données unifiée PREIS. Chaque module préfixe ses fichiers."
)

manifest_path <- file.path(HUB_DATA_DIR, "preis_hub_manifest.json")
write_json(hub_manifest, manifest_path, pretty = TRUE,
           auto_unbox = TRUE, null = "null")
cat("   Registre écrit :", basename(manifest_path), "\n\n")


# --------------------------------------------------------------------------- #
# 5. RAPPORT FINAL
# --------------------------------------------------------------------------- #

cat(strrep("=", 60), "\n")
cat("RÉSUMÉ — Base unifiée PREIS\n")
cat(strrep("=", 60), "\n")

for (mid in names(all_results)) {
  r <- all_results[[mid]]
  st <- switch(r$status,
               complete = "COMPLET   ✓",
               partial  = "PARTIEL   ~",
               empty    = "VIDE      ✗",
               source_missing = "ABSENT    ✗",
               r$status)
  cat(sprintf("  %-8s %s  (%d/%d fichiers)\n",
              mid, st, length(r$files_copied), length(COMMON_FILES)))
}

cat("\nFichiers dans le HUB :\n")
hub_files <- list.files(HUB_DATA_DIR)
for (f in hub_files) cat("  ", f, "\n")

cat("\nHUB prêt :", HUB_DATA_DIR, "\n")
cat(strrep("=", 60), "\n")


# --------------------------------------------------------------------------- #
# 6. PUSH GITHUB (désactivé — à activer quand la chaîne locale est validée)
# --------------------------------------------------------------------------- #
# Quand vous voudrez publier le HUB sur GitHub, créez d'abord le dépôt
# PREIS_Common_DB sur github.com, puis dans D:/PREIS_HUB lancez une fois :
#   git init
#   git remote add origin https://github.com/VOTRE_COMPTE/PREIS_Common_DB.git
# Ensuite, décommentez le bloc ci-dessous pour pousser à chaque synchro :
#
# if (Sys.getenv("PREIS_HUB_PUSH", "0") == "1") {
#   cat("\n-- Push GitHub --\n")
#   old_wd <- getwd(); setwd(HUB_DIR)
#   system('git add -A')
#   system(sprintf('git commit -m "sync HUB %s"',
#                  format(Sys.time(), "%Y-%m-%d %H:%M")))
#   system('git push origin main')
#   setwd(old_wd)
#   cat("   HUB poussé sur GitHub.\n")
# }

cat("\nSynchro terminée :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

invisible(hub_manifest)

# FIN : sync_to_hub.R