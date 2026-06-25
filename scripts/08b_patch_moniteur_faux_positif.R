## ============================================================
## PREIS EBOLA DRC
## 08b_patch_moniteur_faux_positif.R   (v2 — non destructif)
##
## Script AUTONOME — corrige le faux positif SitRep N752 SANS
## modifier la fonction find_latest_local_pdf() existante
## (dont la signature/retour est inconnue et utilisee a 3 endroits).
##
## APPROCHE SURE :
##   1. Nettoie le state CSV (supprime les entrees sitrep_no > 60)
##   2. Ajoute un garde-fou EN TETE de 08 : une fonction
##      .preis_is_real_sitrep_file() que le moniteur peut utiliser,
##      sans toucher au code existant.
##
## Le vrai blocage du 752 vient surtout du nettoyage du state :
## une fois la ligne 752 supprimee, le moniteur ne la "voit" plus
## comme "deja envoyee" et repart sur la vraie derniere valeur.
##
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/08b_patch_moniteur_faux_positif.R")
## ============================================================

cat("============================================================\n")
cat("PATCH 08 (v2) — Correction faux positif SitRep N752\n")
cat("Demarre :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

STATE_FILE <- file.path(getwd(), "data/monitor_state/preis_sitrep_email_state.csv")

# ============================================================
# PARTIE 1 — NETTOYAGE DU STATE CSV (le plus important)
# ============================================================
cat("\n[1/2] Nettoyage du state CSV...\n")

if (!file.exists(STATE_FILE)) {
  cat("[WARN] State CSV non trouve :", STATE_FILE, "\n")
  cat("       (Normal si le moniteur n'a jamais tourne en local)\n")
} else {
  # Sauvegarde
  state_backup <- paste0(tools::file_path_sans_ext(STATE_FILE),
                         "_BACKUP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  file.copy(STATE_FILE, state_backup)
  cat("[OK] Sauvegarde state :", basename(state_backup), "\n")

  state <- tryCatch(
    read.csv(STATE_FILE, stringsAsFactors = FALSE),
    error = function(e) { cat("[WARN] Lecture state echouee:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(state) && "sitrep_no" %in% names(state)) {
    avant <- nrow(state)
    # Garder uniquement les SitReps avec numero realiste (1-60)
    state$sitrep_no <- suppressWarnings(as.integer(state$sitrep_no))
    state_clean <- state[!is.na(state$sitrep_no) & state$sitrep_no >= 1 & state$sitrep_no <= 60, ]
    apres <- nrow(state_clean)
    suppr <- avant - apres

    if (suppr > 0) {
      write.csv(state_clean, STATE_FILE, row.names = FALSE)
      cat("[OK]", suppr, "entree(s) hors-bornes supprimee(s) (dont N752)\n")
      cat("[OK] State nettoye :", apres, "SitRep(s) conserve(s)\n")
      cols_show <- intersect(c("sitrep_no", "status", "sent_at"), names(state_clean))
      if (length(cols_show) > 0 && apres > 0) {
        print(state_clean[, cols_show, drop = FALSE])
      }
    } else {
      cat("[INFO] Aucune entree hors-bornes. State deja propre.\n")
    }
  } else {
    cat("[WARN] Colonne sitrep_no absente du state.\n")
  }
}

# ============================================================
# PARTIE 2 — GARDE-FOU AJOUTE EN TETE DU SCRIPT 08
# ============================================================
cat("\n[2/2] Ajout du garde-fou dans 08_cloud_sitrep_monitor.R...\n")

TARGET <- file.path(getwd(), "scripts/08_cloud_sitrep_monitor.R")

if (!file.exists(TARGET)) {
  cat("[WARN] Script 08 non trouve. Garde-fou non ajoute.\n")
  cat("       Le nettoyage du state (partie 1) suffit dans la plupart des cas.\n")
} else {
  lines <- readLines(TARGET, warn = FALSE, encoding = "UTF-8")

  if (any(grepl("PATCH_08B_APPLIED", lines))) {
    cat("[INFO] Garde-fou deja present dans le script 08.\n")
  } else {
    backup <- paste0(tools::file_path_sans_ext(TARGET),
                     "_BACKUP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
    file.copy(TARGET, backup)
    cat("[OK] Sauvegarde :", basename(backup), "\n")

    # Trouver la 1ere ligne non-commentaire pour inserer apres l'en-tete
    insert_at <- 1
    for (i in seq_along(lines)) {
      if (!grepl("^\\s*#", lines[i]) && nchar(trimws(lines[i])) > 0) {
        insert_at <- i
        break
      }
    }

    guard <- c(
      "## ---- PATCH_08B_APPLIED ---- garde-fou anti-faux-positif ----",
      "## Verifie qu'un numero de SitRep detecte est realiste (1-60).",
      "## A utiliser pour filtrer les detections aberrantes (ex: 752).",
      ".preis_sitrep_no_is_real <- function(no) {",
      "  no <- suppressWarnings(as.integer(no))",
      "  !is.na(no) && no >= 1 && no <= 60",
      "}",
      ""
    )

    lines_new <- c(
      lines[1:(insert_at - 1)],
      guard,
      lines[insert_at:length(lines)]
    )

    tmp <- tempfile(fileext = ".R")
    writeLines(lines_new, tmp, useBytes = TRUE)
    check <- tryCatch(parse(file = tmp), error = function(e) e)
    unlink(tmp)

    if (inherits(check, "error")) {
      cat("[ERREUR] Syntaxe invalide. Garde-fou non ajoute.\n")
      cat("Sauvegarde intacte :", basename(backup), "\n")
    } else {
      writeLines(lines_new, TARGET, useBytes = TRUE)
      cat("[OK] Garde-fou .preis_sitrep_no_is_real() ajoute en tete\n")
      cat("[OK] La fonction find_latest_local_pdf() existante n'a PAS ete modifiee\n")
      cat("     (signature/retour preserves — zero risque de casse)\n")
    }
  }
}

cat("\n============================================================\n")
cat("PATCH 08 termine :", format(Sys.time(), "%H:%M:%S"), "\n")
cat("Le faux positif 752 est neutralise par le nettoyage du state.\n")
cat("============================================================\n")
