## ============================================================
## PREIS EBOLA DRC
## 92_qc_tableau_indicateurs.R
##
## Script AUTONOME — génère le tableau Excel de contrôle qualité
## Couvre TOUS les SitReps disponibles (SR1 → dernier SR)
##
## UTILISATION :
##   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
##   source("scripts/92_qc_tableau_indicateurs.R")
##
## SORTIE :
##   outputs/audit/PREIS_QC_indicateurs_[date].xlsx
##
## ONGLETS :
##   1_MATRICE    — indicateurs × SitRep (vert=ok, rouge=absent)
##   2_DETAIL     — chaque valeur avec règle, evidence, QC
##   3_SERIE      — série temporelle nationale complète
##   4_ZONES      — zones de santé par SitRep
##   5_SIGNAUX    — signaux d'alerte actifs + validation INRB
##   6_BACKLOG    — indicateurs à extraire (lacunes prioritaires)
##   7_GUIDE      — légende et guide lecture
## ============================================================

## ---- AUTO_INSTALL_92 : auto-installation packages (cloud-safe) ----
.pkgs_92 <- c("openxlsx","dplyr","tidyr","readr","stringr")
.missing_92 <- .pkgs_92[!vapply(.pkgs_92, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing_92) > 0) {
  install.packages(.missing_92, repos = "https://cloud.r-project.org")
}
## ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

cat("============================================================\n")
cat("PREIS QC — Tableau indicateurs par source\n")
cat("Démarré :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

# ── Chemins ────────────────────────────────────────────────────
ROOT      <- getwd()
FINAL     <- file.path(ROOT, "data/final")
ANALYSE   <- file.path(ROOT, "outputs/analyse")
OUT_DIR   <- file.path(ROOT, "outputs/audit")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_FILE  <- file.path(OUT_DIR,
  paste0("PREIS_QC_indicateurs_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"))

# ── Helpers lecture ────────────────────────────────────────────
rd <- function(fname, dir = FINAL) {
  fp <- file.path(dir, fname)
  if (!file.exists(fp)) { cat("[ABSENT]", fname, "\n"); return(NULL) }
  tryCatch(read_csv(fp, show_col_types = FALSE),
           error = function(e) { cat("[ERR]", fname, ":", e$message, "\n"); NULL })
}

# ── Chargement données ─────────────────────────────────────────
cat("\nChargement des données...\n")
long    <- rd("PREIS_indicators_long.csv")
valid   <- rd("PREIS_indicators_validated.csv")
cand    <- rd("PREIS_indicator_candidates.csv")
daily   <- rd("PREIS_daily_indicators.csv")
inrb    <- rd("INRB_reference_national.csv")
zones   <- rd("PREIS_health_zones.csv")
signals <- rd("PREIS_signals.csv")
val_sig <- rd("PREIS_validation_signals.csv")
val_inrb<- rd("PREIS_validation_vs_INRB.csv")
qc_iss  <- rd("PREIS_QC_issues.csv")
qc_by   <- rd("PREIS_QC_by_sitrep.csv")
serie   <- rd("serie_temporelle_nationale.csv", dir = ANALYSE)
registry<- rd("sitrep_registry.csv")

# ── Registre indicateurs attendus ─────────────────────────────
# Définition complète : code, domaine, source, priorité OMS, statut
IND_DEF <- tribble(
  ~indicator_code,                ~domaine,          ~source,       ~priorite, ~statut,
  "cumulative_confirmed_cases",   "Cas",             "INRB+PDF",    "P1",      "actif",
  "cumulative_deaths",            "Décès",           "INRB+PDF",    "P1",      "actif",
  "case_fatality_ratio",          "CFR",             "INRB+PDF",    "P1",      "actif",
  "new_confirmed_cases",          "Cas",             "PDF/dérivé",  "P1",      "actif",
  "new_deaths",                   "Décès",           "PDF/dérivé",  "P1",      "actif",
  "suspected_cases_investigation","Cas-suspects",    "INRB+PDF",    "P2",      "actif",
  "cases_ituri",                  "Cas-Ituri",       "PDF",         "P2",      "actif",
  "cases_nordkivu",               "Cas-NordKivu",    "PDF",         "P2",      "actif",
  "cases_sudkivu",                "Cas-SudKivu",     "PDF",         "P2",      "actif",
  "deaths_ituri",                 "Décès-Ituri",     "PDF",         "P2",      "actif",
  "deaths_nordkivu",              "Décès-NordKivu",  "PDF",         "P2",      "actif",
  "deaths_sudkivu",               "Décès-SudKivu",   "PDF",         "P2",      "actif",
  "hz_affected_national",         "Géographie",      "PDF",         "P2",      "actif",
  "hz_affected_ituri",            "Géographie-Ituri","PDF",         "P2",      "patch",
  "hz_affected_nordkivu",         "Géographie-NK",   "PDF",         "P2",      "patch",
  "hz_affected_sudkivu",          "Géographie-SK",   "PDF",         "P2",      "patch",
  "alerts_reported",              "Alertes",         "PDF",         "P2",      "actif",
  "alerts_validated",             "Alertes",         "PDF",         "P2",      "actif",
  "alerts_investigated",          "Alertes",         "PDF",         "P3",      "actif",
  "alerts_investigation_rate",    "Alertes",         "PDF",         "P3",      "actif",
  "samples_collected",            "Labo",            "PDF",         "P2",      "actif",
  "samples_analyzed",             "Labo",            "PDF",         "P2",      "actif",
  "samples_positive",             "Labo",            "PDF",         "P2",      "actif",
  "samples_received",             "Labo",            "PDF",         "P2",      "actif",
  "lab_positivity_rate",          "Labo",            "PDF/dérivé",  "P2",      "actif",
  "contacts_listed",              "Contacts",        "PDF",         "P1",      "actif",
  "contacts_followed_up",         "Contacts",        "PDF",         "P1",      "patch",
  "contacts_followup_rate",       "Contacts",        "PDF",         "P1",      "patch",
  "patients_in_isolation",        "Isolement",       "PDF",         "P2",      "actif",
  "recovered",                    "Guérison",        "PDF",         "P2",      "actif",
  "recovered_today",              "Guérison",        "PDF",         "P3",      "actif",
  "doses_vaccine_administered",   "Vaccination",     "PDF",         "P1",      "patch",
  "hcw_vaccinated",               "Vaccination",     "PDF",         "P1",      "patch",
  "ring_vaccination_n",           "Vaccination",     "PDF",         "P2",      "patch",
  "deaths_community",             "Décès-lieu",      "PDF",         "P2",      "patch",
  "travellers_total",             "Voyageurs",       "PDF",         "P3",      "actif",
  "bed_occupancy_rate",           "Capacité-CTE",    "PDF/DHIS2",   "P2",      "futur"
)

# ── Construction base unifiée ──────────────────────────────────
cat("Construction base unifiée...\n")

all_vals <- bind_rows(
  if (!is.null(long) && "indicator_code" %in% names(long))
    long %>% select(sitrep_no, indicator_code,
                    value = any_of(c("value","valeur")),
                    rule  = any_of(c("extraction_rule","rule","domain"))) %>%
             mutate(src_type = "long", qc_ok = NA)
  else NULL,

  if (!is.null(valid) && "indicator_code" %in% names(valid))
    valid %>% select(sitrep_no, indicator_code, value,
                     rule = any_of(c("extraction_rule","rule")),
                     qc_ok = any_of("qc_valid")) %>%
              mutate(src_type = "validated")
  else NULL,

  if (!is.null(inrb) && "indicator_code" %in% names(inrb))
    inrb %>% select(sitrep_no, indicator_code, value) %>%
             mutate(src_type = "INRB", rule = "INRB_reference", qc_ok = TRUE)
  else NULL
) %>%
  filter(!is.na(value)) %>%
  mutate(
    src_prio = case_when(
      src_type == "validated" ~ 1L,
      src_type == "INRB"      ~ 2L,
      TRUE                    ~ 3L
    )
  ) %>%
  arrange(sitrep_no, indicator_code, src_prio) %>%
  group_by(sitrep_no, indicator_code) %>%
  slice(1) %>%
  ungroup()

all_sr  <- sort(unique(all_vals$sitrep_no))
all_ind <- sort(unique(IND_DEF$indicator_code))
cat("  SitReps trouvés :", length(all_sr), "\n")
cat("  Indicateurs définis :", nrow(IND_DEF), "\n")

## Garde cloud-safe : si aucune donnée, sortir proprement sans planter
if (length(all_sr) == 0 || nrow(all_vals) == 0) {
  cat("\n[INFO] Aucune donnée d'indicateur disponible pour le moment.\n")
  cat("       Le tableau Excel n'est pas généré (rien à afficher).\n")
  cat("       Ce n'est pas une erreur — relancer après extraction.\n")
  if (!interactive()) quit(save = "no", status = 0) else {
    stop("__PREIS_NO_DATA__", call. = FALSE)
  }
}

# ── Styles openpyxl via openxlsx ──────────────────────────────
S_HDR   <- createStyle(fontName="Arial", fontSize=9, textDecoration="bold",
                        fontColour="white", fgFill="#1F4E79",
                        halign="center", valign="center",
                        wrapText=TRUE, border="TopBottomLeftRight",
                        borderColour="#FFFFFF")
S_HDR2  <- createStyle(fontName="Arial", fontSize=9, textDecoration="bold",
                        fontColour="white", fgFill="#2E75B6",
                        halign="center", valign="center",
                        wrapText=TRUE)
S_META  <- createStyle(fontName="Arial", fontSize=9,
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_META_L<- createStyle(fontName="Arial", fontSize=9, halign="left",
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_OK    <- createStyle(fontName="Arial", fontSize=9, halign="center",
                        fgFill="#E2EFDA", fontColour="#375623",
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_WARN  <- createStyle(fontName="Arial", fontSize=9, halign="center",
                        fgFill="#FFF2CC", fontColour="#7F6000",
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_MISS  <- createStyle(fontName="Arial", fontSize=9, halign="center",
                        fgFill="#FCE4D6", fontColour="#843C0C",
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_PATCH <- createStyle(fontName="Arial", fontSize=9, halign="center",
                        fgFill="#EDE7F6", fontColour="#4A148C",
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_ALT   <- createStyle(fontName="Arial", fontSize=9,
                        fgFill="#F2F2F2",
                        border="TopBottomLeftRight", borderColour="#D9D9D9")
S_TITLE <- createStyle(fontName="Arial", fontSize=12, textDecoration="bold",
                        fontColour="white", fgFill="#1F4E79",
                        halign="left", valign="center")

wb <- createWorkbook()

# ============================================================
# ONGLET 1 — MATRICE INDICATEURS × SITREP
# ============================================================
cat("\n[1/7] Matrice indicateurs × SitRep...\n")
ws <- "1_MATRICE"
addWorksheet(wb, ws, zoom = 90)

# Titre
n_cols_total <- 4 + length(all_sr) + 2  # meta + SR + couverture + statut patch
mergeCells(wb, ws, rows=1, cols=1:n_cols_total)
writeData(wb, ws,
  paste0("PREIS Ebola DRC — QC Indicateurs × SitRep (SR",
         min(all_sr), "→SR", max(all_sr), ") | Généré : ",
         format(Sys.time(), "%Y-%m-%d %H:%M")),
  startRow=1, startCol=1)
addStyle(wb, ws, S_TITLE, rows=1, cols=1, stack=TRUE)
setRowHeights(wb, ws, rows=1, heights=22)

# Headers
hdr_row <- data.frame(
  Indicateur = "Indicateur",
  Domaine    = "Domaine",
  Source     = "Source",
  Priorite   = "Priorité",
  stringsAsFactors = FALSE
)
for (sr in all_sr) hdr_row[[paste0("SR", sr)]] <- paste0("SR", sr)
hdr_row[["Couverture"]] <- paste0("Couvert\n/", length(all_sr), " SR")
hdr_row[["Statut"]] <- "Statut"

writeData(wb, ws, hdr_row, startRow=2, startCol=1, colNames=FALSE)
addStyle(wb, ws, S_HDR, rows=2, cols=1:4, gridExpand=TRUE, stack=FALSE)
addStyle(wb, ws, S_HDR2, rows=2, cols=5:(4+length(all_sr)), gridExpand=TRUE, stack=FALSE)
addStyle(wb, ws, S_HDR, rows=2, cols=(5+length(all_sr)):(6+length(all_sr)),
         gridExpand=TRUE, stack=FALSE)
setRowHeights(wb, ws, rows=2, heights=30)

# Données ligne par ligne
for (r in seq_len(nrow(IND_DEF))) {
  ind   <- IND_DEF$indicator_code[r]
  dom   <- IND_DEF$domaine[r]
  src   <- IND_DEF$source[r]
  prio  <- IND_DEF$priorite[r]
  stat  <- IND_DEF$statut[r]
  row_e <- r + 2

  # Colonnes méta
  writeData(wb, ws, ind,  startRow=row_e, startCol=1)
  writeData(wb, ws, dom,  startRow=row_e, startCol=2)
  writeData(wb, ws, src,  startRow=row_e, startCol=3)
  writeData(wb, ws, prio, startRow=row_e, startCol=4)
  addStyle(wb, ws, S_META_L, rows=row_e, cols=1, stack=FALSE)
  addStyle(wb, ws, S_META,   rows=row_e, cols=2:4, gridExpand=TRUE, stack=FALSE)

  # Colonnes SR
  n_ok <- 0L
  for (ci in seq_along(all_sr)) {
    sr    <- all_sr[ci]
    col_e <- 4 + ci
    row_data <- all_vals %>%
      filter(sitrep_no == sr, indicator_code == ind)

    if (nrow(row_data) > 0) {
      val <- round(row_data$value[1], 2)
      writeData(wb, ws, val, startRow=row_e, startCol=col_e)
      addStyle(wb, ws, S_OK, rows=row_e, cols=col_e, stack=FALSE)
      n_ok <- n_ok + 1L
    } else {
      writeData(wb, ws, "—", startRow=row_e, startCol=col_e)
      s_use <- if (stat == "patch") S_PATCH else S_MISS
      addStyle(wb, ws, s_use, rows=row_e, cols=col_e, stack=FALSE)
    }
  }

  # Colonne couverture
  col_cov <- 5 + length(all_sr)
  cov_txt <- paste0(n_ok, "/", length(all_sr))
  writeData(wb, ws, cov_txt, startRow=row_e, startCol=col_cov)
  s_cov <- if (n_ok == length(all_sr)) S_OK else if (n_ok >= length(all_sr)/2) S_WARN else S_MISS
  addStyle(wb, ws, s_cov, rows=row_e, cols=col_cov, stack=FALSE)

  # Colonne statut
  col_stat <- 6 + length(all_sr)
  stat_label <- switch(stat,
    "actif"  = "Actif",
    "patch"  = "Patch 04/05",
    "futur"  = "DHIS2",
    stat
  )
  writeData(wb, ws, stat_label, startRow=row_e, startCol=col_stat)
  s_stat <- switch(stat, "actif"=S_OK, "patch"=S_PATCH, "futur"=S_WARN, S_META)
  addStyle(wb, ws, s_stat, rows=row_e, cols=col_stat, stack=FALSE)
}

# Largeurs colonnes
setColWidths(wb, ws, cols=1, widths=32)
setColWidths(wb, ws, cols=2, widths=14)
setColWidths(wb, ws, cols=3, widths=12)
setColWidths(wb, ws, cols=4, widths=9)
if (length(all_sr) > 0)
  setColWidths(wb, ws, cols=5:(4+length(all_sr)), widths=7)
setColWidths(wb, ws, cols=5+length(all_sr), widths=10)
setColWidths(wb, ws, cols=6+length(all_sr), widths=12)
freezePane(wb, ws, firstActiveRow=3, firstActiveCol=5)

# Légende couleurs sous la matrice
leg_row <- nrow(IND_DEF) + 5
writeData(wb, ws, "Légende :", startRow=leg_row, startCol=1)
leg_items <- list(
  list("Valeur présente (extraite ou dérivée)", S_OK),
  list("Absent — non extrait du PDF", S_MISS),
  list("Patch appliqué — à vérifier sur prochains SR", S_PATCH),
  list("Donnée future (DHIS2 / module en cours)", S_WARN)
)
for (li in seq_along(leg_items)) {
  writeData(wb, ws, leg_items[[li]][[1]], startRow=leg_row+1, startCol=li)
  addStyle(wb, ws, leg_items[[li]][[2]], rows=leg_row+1, cols=li, stack=FALSE)
}
cat("  [OK] Matrice :", nrow(IND_DEF), "indicateurs ×", length(all_sr), "SitReps\n")

# ============================================================
# ONGLET 2 — DÉTAIL EXTRACTION
# ============================================================
cat("[2/7] Détail extraction...\n")
ws2 <- "2_DETAIL"
addWorksheet(wb, ws2, zoom=90)

detail_src <- if (!is.null(valid)) valid else if (!is.null(cand)) cand else NULL

if (!is.null(detail_src) && nrow(detail_src) > 0) {
  cols_keep <- intersect(
    c("sitrep_no","indicator_code","domain","value","value_source",
      "source_type","extraction_rule","priority","evidence",
      "qc_valid","qc_note","extracted_at"),
    names(detail_src))
  detail <- detail_src %>%
    select(all_of(cols_keep)) %>%
    arrange(sitrep_no, indicator_code)

  mergeCells(wb, ws2, rows=1, cols=1:length(cols_keep))
  writeData(wb, ws2,
    paste0("Détail extraction — tous SitReps validés | Lignes : ", nrow(detail)),
    startRow=1, startCol=1)
  addStyle(wb, ws2, S_TITLE, rows=1, cols=1)
  setRowHeights(wb, ws2, rows=1, heights=20)

  writeData(wb, ws2, detail, startRow=2, headerStyle=S_HDR,
            borders="all", borderColour="#D9D9D9")

  # Coloration QC
  if ("qc_valid" %in% names(detail)) {
    qc_col <- which(names(detail) == "qc_valid")
    for (r in seq_len(nrow(detail))) {
      s <- if (isTRUE(detail$qc_valid[r])) S_OK else S_WARN
      addStyle(wb, ws2, s, rows=r+2, cols=1:length(cols_keep),
               gridExpand=TRUE, stack=FALSE)
    }
  }

  setColWidths(wb, ws2, cols=1, widths=9)
  setColWidths(wb, ws2, cols=2, widths=32)
  setColWidths(wb, ws2, cols=3:6, widths=13)
  setColWidths(wb, ws2, cols=7, widths=32)
  setColWidths(wb, ws2, cols=8, widths=8)
  if (length(cols_keep) >= 9)
    setColWidths(wb, ws2, cols=9, widths=50)
  freezePane(wb, ws2, firstActiveRow=3, firstActiveCol=3)

  # Section QC issues
  if (!is.null(qc_iss) && nrow(qc_iss) > 0) {
    qc_start <- nrow(detail) + 5
    mergeCells(wb, ws2, rows=qc_start, cols=1:6)
    writeData(wb, ws2,
      paste0("=== PROBLÈMES QC (", nrow(qc_iss), " issues détectées) ==="),
      startRow=qc_start, startCol=1)
    addStyle(wb, ws2, S_WARN, rows=qc_start, cols=1:6, gridExpand=TRUE)
    writeData(wb, ws2, qc_iss, startRow=qc_start+1, headerStyle=S_HDR2)
    for (r in seq_len(nrow(qc_iss))) {
      s <- if (grepl("CRITICAL", qc_iss$severity[r], ignore.case=TRUE)) S_MISS else S_WARN
      addStyle(wb, ws2, s, rows=r+qc_start+1,
               cols=1:ncol(qc_iss), gridExpand=TRUE, stack=FALSE)
    }
  }
  cat("  [OK]", nrow(detail), "lignes détail\n")
} else {
  writeData(wb, ws2, data.frame(INFO="Aucun fichier validé trouvé."))
  cat("  [WARN] Pas de données détail disponibles\n")
}

# ============================================================
# ONGLET 3 — SÉRIE TEMPORELLE
# ============================================================
cat("[3/7] Série temporelle...\n")
ws3 <- "3_SERIE"
addWorksheet(wb, ws3, zoom=90)

serie_data <- if (!is.null(serie) && nrow(serie) > 0) serie else {
  if (!is.null(daily) && "level" %in% names(daily))
    daily %>% filter(level == "National") else NULL
}

if (!is.null(serie_data) && nrow(serie_data) > 0) {
  # Nettoyage et enrichissement
  if (!"date" %in% names(serie_data) && "sitrep_no" %in% names(serie_data)) {
    serie_data <- serie_data %>% mutate(date = NA)
  }
  if ("cum_cases" %in% names(serie_data) && !"cas_cumules" %in% names(serie_data)) {
    serie_data <- serie_data %>% rename(
      cas_cumules  = any_of("cum_cases"),
      deces_cumules = any_of("cum_deaths"),
      nouveaux_cas  = any_of("new_cases"),
      nouveaux_deces = any_of("new_deaths")
    )
  }
  serie_data <- serie_data %>%
    mutate(
      alerte_revision = if ("nouveaux_cas" %in% names(serie_data))
                          ifelse(!is.na(nouveaux_cas) & nouveaux_cas < 0, "REVISION INRB", "")
                        else "",
      cfr_calc = if (all(c("cas_cumules","deces_cumules") %in% names(serie_data)))
                   ifelse(!is.na(cas_cumules) & cas_cumules > 0,
                          round(deces_cumules / cas_cumules * 100, 1), NA)
                 else NA
    )

  mergeCells(wb, ws3, rows=1, cols=1:ncol(serie_data))
  writeData(wb, ws3,
    paste0("Série temporelle nationale | ",
           nrow(serie_data), " points | ",
           min(serie_data$date, na.rm=TRUE), " → ",
           max(serie_data$date, na.rm=TRUE)),
    startRow=1, startCol=1)
  addStyle(wb, ws3, S_TITLE, rows=1, cols=1)
  setRowHeights(wb, ws3, rows=1, heights=20)

  writeData(wb, ws3, serie_data, startRow=2, headerStyle=S_HDR,
            borders="all", borderColour="#D9D9D9")

  # Surligner révisions
  if ("alerte_revision" %in% names(serie_data)) {
    rev_rows <- which(serie_data$alerte_revision != "")
    for (r in rev_rows) {
      addStyle(wb, ws3, S_WARN, rows=r+2, cols=1:ncol(serie_data),
               gridExpand=TRUE, stack=FALSE)
    }
  }

  setColWidths(wb, ws3, cols=1:ncol(serie_data), widths=14)
  freezePane(wb, ws3, firstActiveRow=3, firstActiveCol=2)
  cat("  [OK]", nrow(serie_data), "lignes série temporelle\n")
} else {
  writeData(wb, ws3, data.frame(INFO="Série temporelle non disponible."))
  cat("  [WARN] Pas de série temporelle\n")
}

# ============================================================
# ONGLET 4 — ZONES DE SANTÉ
# ============================================================
cat("[4/7] Zones de santé...\n")
ws4 <- "4_ZONES"
addWorksheet(wb, ws4, zoom=90)

if (!is.null(zones) && nrow(zones) > 0) {
  # Detecter la colonne de regle/preuve disponible (varie selon la version)
  rule_col <- intersect(c("rule","extraction_rule","confidence","evidence_line"),
                        names(zones))
  rule_col <- if (length(rule_col) > 0) rule_col[1] else NA_character_

  zones_agg <- zones %>%
    group_by(health_zone) %>%
    summarise(
      n_sitreps     = n_distinct(sitrep_no),
      premier_sr    = min(sitrep_no),
      dernier_sr    = max(sitrep_no),
      liste_sitreps = paste(sort(unique(sitrep_no)), collapse=", "),
      source_info   = if (!is.na(rule_col))
                        paste(unique(.data[[rule_col]]), collapse="|")
                      else "extraction_zones",
      .groups = "drop"
    ) %>%
    arrange(desc(n_sitreps), health_zone)

  # Joindre signaux si disponibles
  if (!is.null(val_sig) && "zone" %in% names(val_sig)) {
    zones_agg <- zones_agg %>%
      left_join(
        val_sig %>% group_by(zone) %>%
          summarise(signaux = paste(type, collapse=" | "), .groups="drop"),
        by = c("health_zone" = "zone")
      ) %>%
      mutate(signaux = ifelse(is.na(signaux), "—", signaux))
  }

  mergeCells(wb, ws4, rows=1, cols=1:ncol(zones_agg))
  writeData(wb, ws4,
    paste0("Zones de santé touchées — ", nrow(zones_agg), " zones | ",
           length(unique(zones$sitrep_no)), " SitReps"),
    startRow=1, startCol=1)
  addStyle(wb, ws4, S_TITLE, rows=1, cols=1)
  setRowHeights(wb, ws4, rows=1, heights=20)

  writeData(wb, ws4, zones_agg, startRow=2, headerStyle=S_HDR,
            borders="all", borderColour="#D9D9D9")

  # Surligner zones avec signaux
  if ("signaux" %in% names(zones_agg)) {
    sig_rows <- which(zones_agg$signaux != "—")
    for (r in sig_rows) {
      addStyle(wb, ws4, S_MISS, rows=r+2, cols=1:ncol(zones_agg),
               gridExpand=TRUE, stack=FALSE)
    }
    # Alternance sur les autres
    for (r in setdiff(seq_len(nrow(zones_agg)), sig_rows)) {
      if (r %% 2 == 0)
        addStyle(wb, ws4, S_ALT, rows=r+2, cols=1:ncol(zones_agg),
                 gridExpand=TRUE, stack=FALSE)
    }
  }

  setColWidths(wb, ws4, cols=1, widths=20)
  setColWidths(wb, ws4, cols=2:4, widths=10)
  setColWidths(wb, ws4, cols=5, widths=40)
  setColWidths(wb, ws4, cols=6, widths=22)
  if (ncol(zones_agg) >= 7) setColWidths(wb, ws4, cols=7, widths=35)
  freezePane(wb, ws4, firstActiveRow=3, firstActiveCol=2)
  cat("  [OK]", nrow(zones_agg), "zones\n")
} else {
  writeData(wb, ws4, data.frame(INFO="Données zones non disponibles."))
}

# ============================================================
# ONGLET 5 — SIGNAUX + VALIDATION
# ============================================================
cat("[5/7] Signaux...\n")
ws5 <- "5_SIGNAUX"
addWorksheet(wb, ws5, zoom=90)
row_cur <- 2

sections <- list(
  list(title = paste0("SIGNAUX ACTIFS (", if (!is.null(signals)) nrow(signals) else 0, ")"),
       data  = signals,
       sev_col = "severity",
       sev_map = list(high=S_MISS, moderate=S_WARN, info=S_OK)),
  list(title = "VALIDATION SIGNAUX HISTORIQUES",
       data  = val_sig, sev_col=NULL, sev_map=NULL),
  list(title = "VALIDATION vs INRB",
       data  = val_inrb, sev_col="match",
       sev_map=list(OK=S_OK)),
  list(title = paste0("QC PAR SITREP"),
       data  = qc_by, sev_col="qc_status",
       sev_map=list(PASS_WITH_WARNINGS=S_WARN, OK=S_OK, CRITICAL=S_MISS))
)

for (sec in sections) {
  if (is.null(sec$data) || nrow(sec$data) == 0) next
  mergeCells(wb, ws5, rows=row_cur, cols=1:ncol(sec$data))
  writeData(wb, ws5, paste0("=== ", sec$title, " ==="),
            startRow=row_cur, startCol=1)
  addStyle(wb, ws5, S_HDR, rows=row_cur, cols=1:ncol(sec$data), gridExpand=TRUE)
  row_cur <- row_cur + 1
  writeData(wb, ws5, sec$data, startRow=row_cur,
            headerStyle=S_HDR2, borders="all", borderColour="#D9D9D9")
  if (!is.null(sec$sev_col) && sec$sev_col %in% names(sec$data)) {
    sev_idx <- which(names(sec$data) == sec$sev_col)
    for (r in seq_len(nrow(sec$data))) {
      sev_val <- as.character(sec$data[[sev_idx]][r])
      s <- sec$sev_map[[sev_val]]
      if (!is.null(s))
        addStyle(wb, ws5, s, rows=r+row_cur, cols=1:ncol(sec$data),
                 gridExpand=TRUE, stack=FALSE)
    }
  }
  setColWidths(wb, ws5, cols=1:ncol(sec$data), widths=18)
  row_cur <- row_cur + nrow(sec$data) + 3
}
cat("  [OK] Signaux\n")

# ============================================================
# ONGLET 6 — BACKLOG LACUNES PRIORITAIRES
# ============================================================
cat("[6/7] Backlog lacunes...\n")
ws6 <- "6_BACKLOG"
addWorksheet(wb, ws6, zoom=90)

# Calculer les lacunes à partir des données réelles
backlog <- IND_DEF %>%
  mutate(
    n_valeurs = sapply(indicator_code, function(cd)
      sum(all_vals$indicator_code == cd, na.rm=TRUE)),
    couverture_pct = round(n_valeurs / max(length(all_sr), 1) * 100),
    lacune = length(all_sr) - n_valeurs,
    priorite_action = case_when(
      priorite == "P1" & n_valeurs == 0  ~ "1_CRITIQUE_ABSENT",
      priorite == "P1" & couverture_pct < 50 ~ "2_CRITIQUE_PARTIEL",
      priorite == "P2" & n_valeurs == 0  ~ "3_IMPORTANT_ABSENT",
      priorite == "P2" & couverture_pct < 50 ~ "4_IMPORTANT_PARTIEL",
      TRUE                               ~ "5_OK_OU_OPTIONNEL"
    ),
    action = case_when(
      statut == "patch"  ~ "Relancer pipeline apres patch 04b",
      statut == "futur"  ~ "Integrer module DHIS2",
      n_valeurs == 0     ~ "Verifier PDF source + regex extraction",
      couverture_pct < 50~ "Verifier PDF source des SR manquants",
      TRUE               ~ "OK"
    )
  ) %>%
  filter(priorite_action != "5_OK_OU_OPTIONNEL" | statut == "patch") %>%
  arrange(priorite_action, desc(lacune)) %>%
  select(priorite_action, indicateur=indicator_code, domaine,
         priorite, statut, n_valeurs, couverture_pct, lacune, action)

mergeCells(wb, ws6, rows=1, cols=1:ncol(backlog))
writeData(wb, ws6,
  paste0("Backlog lacunes prioritaires — ", nrow(backlog),
         " indicateurs à compléter | Généré : ",
         format(Sys.time(), "%Y-%m-%d %H:%M")),
  startRow=1, startCol=1)
addStyle(wb, ws6, S_TITLE, rows=1, cols=1)
setRowHeights(wb, ws6, rows=1, heights=20)

writeData(wb, ws6, backlog, startRow=2, headerStyle=S_HDR,
          borders="all", borderColour="#D9D9D9")

for (r in seq_len(nrow(backlog))) {
  s <- case_when(
    grepl("CRITIQUE_ABSENT", backlog$priorite_action[r]) ~ list(S_MISS),
    grepl("CRITIQUE_PARTIEL", backlog$priorite_action[r]) ~ list(S_WARN),
    grepl("IMPORTANT_ABSENT", backlog$priorite_action[r]) ~ list(S_PATCH),
    TRUE ~ list(S_ALT)
  )[[1]]
  addStyle(wb, ws6, s, rows=r+2, cols=1:ncol(backlog),
           gridExpand=TRUE, stack=FALSE)
}

setColWidths(wb, ws6, cols=1, widths=22)
setColWidths(wb, ws6, cols=2, widths=32)
setColWidths(wb, ws6, cols=3:8, widths=13)
setColWidths(wb, ws6, cols=9, widths=40)
freezePane(wb, ws6, firstActiveRow=3, firstActiveCol=3)
cat("  [OK] Backlog:", nrow(backlog), "lacunes\n")

# ============================================================
# ONGLET 7 — GUIDE LECTURE
# ============================================================
cat("[7/7] Guide lecture...\n")
ws7 <- "7_GUIDE"
addWorksheet(wb, ws7, zoom=100)

guide <- tribble(
  ~Element, ~Signification, ~Statut, ~Action,
  "Vert (cellule SR)",    "Valeur extraite et présente pour ce SitRep","OK","Aucune",
  "Rouge (cellule SR)",   "Valeur absente — non extraite du PDF","Lacune","Vérifier PDF source + regex",
  "Violet (cellule SR)",  "Indicateur ajouté par patch — à vérifier sur SR suivants","Nouveau","Relancer pipeline force_reextract=TRUE",
  "Orange (cellule SR)",  "Indicateur futur (DHIS2) ou partiellement couvert","En cours","Intégrer module DHIS2",
  "Jaune (ligne entière)","Révision INRB à la baisse — comportement normal","OK documenté","Conserver flag 'revision'",
  "P1 (Priorité OMS)",    "Indicateur critique — doit être présent à chaque SitRep","Surveiller","Extraire en priorité",
  "P2",                   "Indicateur important — extraire si disponible dans le PDF","Standard","Amélioration continue",
  "P3",                   "Indicateur complémentaire","Optionnel","Extraire si présent",
  "Source INRB+PDF",      "Valeur INRB validée superviseur (GitHub INRB-UMIE/BDBV2026-Data)","Gold standard","Aucune",
  "Source PDF",           "Valeur extraite par regex depuis PDF INSP.cd","Variable","Vérifier règles si absent",
  "Source PDF/dérivé",    "Calculé automatiquement depuis d'autres indicateurs","Dérivé","Vérifier cohérence",
  "Statut 'patch'",       "Indicateur ajouté par le patch 04b/05b — peut être vide sur anciens SR","Nouveau","OK après relance pipeline",
  "Statut 'futur'",       "Nécessite module DHIS2 non encore intégré","À venir","Créer app_dhis2_module.R",
  "SR N°752",             "FAUX POSITIF corrigé (Visa_A3286866_TfjYN752.pdf)","Corrigé","Patch 08b appliqué",
  "Révision 30 mai 2026", "INRB a révisé à la baisse (263→238). Normal, flaggé.","OK","Conserver flag revision",
  "lab_positivity_rate",  "= samples_positive / samples_analyzed × 100. Calculé auto.","Patch 05b","Vérifier sur SR suivants",
  "contacts_followed_up", "Souvent absent des PDF — nouvelles règles regex dans patch 04b","Patch 04b","Vérifier SR suivants",
  "doses_vaccine_adm.",   "Vaccination Ebola — règles regex ajoutées dans patch 04b","Patch 04b","Vérifier SR suivants",
  "Rt (absent)",          "Nombre de reproduction effectif — standard OMS","À implémenter","EpiEstim : IS Ebola mean=15.3j SD=9.3j"
)

mergeCells(wb, ws7, rows=1, cols=1:4)
writeData(wb, ws7,
  paste0("Guide de lecture — PREIS QC Indicateurs | v", format(Sys.Date(), "%Y-%m-%d")),
  startRow=1, startCol=1)
addStyle(wb, ws7, S_TITLE, rows=1, cols=1)
setRowHeights(wb, ws7, rows=1, heights=20)

writeData(wb, ws7, guide, startRow=2, headerStyle=S_HDR,
          borders="all", borderColour="#D9D9D9")

for (r in seq_len(nrow(guide))) {
  s <- if (r %% 2 == 0) S_ALT else S_META
  addStyle(wb, ws7, s, rows=r+2, cols=1:4,
           gridExpand=TRUE, stack=FALSE)
}

# Infos système
info_start <- nrow(guide) + 5
writeData(wb, ws7,
  data.frame(
    Champ = c("Généré le","Racine projet","Scripts utilisés","Contact"),
    Valeur = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      ROOT,
      "PREIS 04_extract, 05_qc, 07_pipeline, 08_monitor, 92_qc_tableau",
      "Dr R. Hyacinthe ZABRE — PREIS / Africa CDC"
    )
  ),
  startRow=info_start, headerStyle=S_HDR2)

setColWidths(wb, ws7, cols=1, widths=24)
setColWidths(wb, ws7, cols=2, widths=52)
setColWidths(wb, ws7, cols=3, widths=18)
setColWidths(wb, ws7, cols=4, widths=46)
for (r in 3:(nrow(guide)+2)) setRowHeights(wb, ws7, rows=r, heights=22)

# ── Ordre onglets ──────────────────────────────────────────────
activeSheet(wb) <- 1

# ── Sauvegarde ─────────────────────────────────────────────────
cat("\nSauvegarde...\n")
saveWorkbook(wb, OUT_FILE, overwrite=TRUE)

if (file.exists(OUT_FILE)) {
  sz <- round(file.info(OUT_FILE)$size / 1024, 0)
  cat("============================================================\n")
  cat("[OK] Fichier Excel créé :\n")
  cat("     ", OUT_FILE, "\n")
  cat("     Taille :", sz, "Ko | 7 onglets\n")
  cat("     SitReps couverts :", length(all_sr), "\n")
  cat("     Indicateurs :", nrow(IND_DEF), "\n")
  cat("============================================================\n")
  cat("Pour régénérer après un nouveau SitRep :\n")
  cat("  source('scripts/92_qc_tableau_indicateurs.R')\n")
  cat("============================================================\n")
} else {
  stop("[ERREUR] Fichier non créé. Vérifier les droits sur outputs/audit/")
}
