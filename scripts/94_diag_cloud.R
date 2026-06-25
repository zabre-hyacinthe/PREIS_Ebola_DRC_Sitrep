## ============================================================
## PREIS EBOLA DRC
## 94_diag_cloud.R
##
## DIAGNOSTIC DEFINITIF avant automatisation cloud.
## Repond a 4 questions en un seul run :
##   1. Quel pipeline le cloud (08) lance-t-il reellement ?
##   2. Ce pipeline appelle-t-il les fonctions patchees (04/05) ?
##   3. Le pipeline regenere-t-il les CSV du tableau Excel ?
##   4. openxlsx est-il installable sur ce systeme ?
##
## Ne MODIFIE rien. Lecture seule. Aucun risque.
##
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/94_diag_cloud.R")
## ============================================================

cat("============================================================\n")
cat("PREIS — DIAGNOSTIC CLOUD (lecture seule)\n")
cat("============================================================\n")

ROOT <- getwd()
sep <- function() cat(paste(rep("-", 60), collapse=""), "\n")

# ── Q1 : quels pipelines candidats existent ? ─────────────────
cat("\n[Q1] Pipelines de production presents :\n"); sep()
candidates <- c("00_PREIS_MASTER_AUTOMATION_CORRIGE.R",
                "00_PREIS_MASTER_AUTOMATION.R",
                "00_RUN_ALL_PRODUCTION.R")
pipeline_found <- NA_character_
for (nm in candidates) {
  fp <- file.path(ROOT, "scripts", nm)
  if (file.exists(fp)) {
    sz <- round(file.info(fp)$size/1024, 1)
    n  <- length(readLines(fp, warn=FALSE))
    cat(sprintf("  [PRESENT] %-42s %5d lignes  %s Ko\n", nm, n, sz))
    if (is.na(pipeline_found)) pipeline_found <- fp
  } else {
    cat(sprintf("  [absent]  %s\n", nm))
  }
}
cat("\n  >> Le cloud chargera EN PREMIER :",
    if (!is.na(pipeline_found)) basename(pipeline_found) else "AUCUN", "\n")

# ── Q2 : ce pipeline appelle-t-il les fonctions patchees ? ────
cat("\n[Q2] Le pipeline utilise-t-il les fonctions externes (04/05) ?\n"); sep()
if (!is.na(pipeline_found)) {
  code <- readLines(pipeline_found, warn=FALSE)

  # Cherche s'il source 04 et 05, ou s'il a sa PROPRE copie des regles
  src04 <- any(grepl("04_extract_indicators", code))
  src05 <- any(grepl("05_qc_validate", code))
  own_extract <- any(grepl("extract_candidates_from_row_text|candidate_row|extract_indicator_candidates", code))
  own_qc      <- any(grepl("validate_and_derive_indicators", code))

  cat("  source('04_extract_indicators.R') present :", src04, "\n")
  cat("  source('05_qc_validate.R') present        :", src05, "\n")
  cat("  Definit SES PROPRES fonctions extraction  :", own_extract, "\n")
  cat("  Definit SA PROPRE validation QC           :", own_qc, "\n\n")

  if (src04 && src05) {
    cat("  >> VERDICT : le pipeline SOURCE tes fichiers 04/05.\n")
    cat("     => Tes patchs s'appliqueront automatiquement. PARFAIT.\n")
  } else if (own_extract || own_qc) {
    cat("  >> VERDICT : le pipeline a sa PROPRE copie des regles.\n")
    cat("     => ATTENTION : tes patchs 04/05 ne s'appliqueront PAS ici.\n")
    cat("     => Il faudra patcher ce gros script aussi.\n")
  } else {
    cat("  >> VERDICT : indetermine — montrer les source() ci-dessous.\n")
  }

  cat("\n  Lignes source() dans le pipeline :\n")
  src_lines <- grep("source\\(", code, value=TRUE)
  src_lines <- src_lines[!grepl("^\\s*#", src_lines)]
  if (length(src_lines) > 0) {
    cat(paste("   ", head(src_lines, 25)), sep="\n")
  } else {
    cat("    (aucun source() — le pipeline est autonome/monolithique)\n")
  }
} else {
  cat("  Aucun pipeline trouve — diagnostic Q2 impossible.\n")
}

# ── Q3 : quels CSV le pipeline ecrit-il ? ─────────────────────
cat("\n\n[Q3] Le pipeline ecrit-il les CSV du tableau Excel ?\n"); sep()
if (!is.na(pipeline_found)) {
  code <- readLines(pipeline_found, warn=FALSE)
  targets <- c("PREIS_indicators_long", "PREIS_indicators_validated",
               "PREIS_indicator_candidates", "PREIS_health_zones",
               "PREIS_daily_indicators", "PREIS_signals")
  for (t in targets) {
    writes <- any(grepl(paste0(t, ".*csv|write.*", t), code))
    cat(sprintf("  %-32s %s\n", t,
                if (writes) "[ECRIT par le pipeline]" else "[non ecrit ici]"))
  }
}

# ── Q4 : openxlsx installable ? ───────────────────────────────
cat("\n[Q4] openxlsx disponible sur ce systeme ?\n"); sep()
has_oxl <- requireNamespace("openxlsx", quietly = TRUE)
cat("  openxlsx installe :", has_oxl, "\n")
if (has_oxl) {
  cat("  Version :", as.character(packageVersion("openxlsx")), "\n")
} else {
  cat("  >> A installer : install.packages('openxlsx')\n")
}

# ── Q5 : 08 lance-t-il bien le pipeline a chaque nouveau SR ? ──
cat("\n[Q5] Logique de 08 : extraction a chaque nouveau SR ?\n"); sep()
code08 <- readLines(file.path(ROOT, "scripts/08_cloud_sitrep_monitor.R"), warn=FALSE)
runs_pipeline <- any(grepl("Running production pipeline|source\\(pipeline_file", code08))
cat("  08 lance le pipeline de production :", runs_pipeline, "\n")
# Est-ce conditionnel a un nouveau SR, ou systematique ?
cond_new <- any(grepl("new.*sitrep|nouveau.*sitrep|latest.*>.*sent|new_or_pending", code08, ignore.case=TRUE))
cat("  Conditionne a un nouveau SR detecte :", cond_new, "\n")

cat("\n============================================================\n")
cat("RESUME POUR L'AUTOMATISATION :\n")
cat("============================================================\n")
if (!is.na(pipeline_found)) {
  code <- readLines(pipeline_found, warn=FALSE)
  src_both <- any(grepl("04_extract_indicators", code)) && any(grepl("05_qc_validate", code))
  cat("Pipeline cloud      :", basename(pipeline_found), "\n")
  cat("Utilise patchs 04/05:", if (src_both) "OUI (auto)" else "NON (a patcher)", "\n")
  cat("openxlsx pret       :", if (has_oxl) "OUI" else "a installer", "\n")
  cat("\nPROCHAINE ETAPE selon ce resume :\n")
  if (src_both) {
    cat("  -> Scenario A (simple) : ajouter le tableau Excel au workflow.\n")
  } else {
    cat("  -> Scenario B : patcher aussi le gros pipeline 00, puis Excel.\n")
  }
}
cat("============================================================\n")
