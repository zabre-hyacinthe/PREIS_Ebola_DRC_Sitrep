# ============================================================
# PREIS EBOLA DRC — AUDIT PROFOND DU SYSTÈME
# But : extraire TOUT le contenu utile pour révision complète
#       sources, indicateurs, logique pipeline, dashboard
# Lancer depuis la racine du projet
# ============================================================

racine <- getwd()
cat("Racine :", racine, "\n")
cat("Généré le :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

output_file <- file.path(racine, "outputs/audit/AUDIT_DEEP_PREIS.txt")
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
sink(output_file, split = TRUE)

sep  <- function() cat(paste(rep("=", 70), collapse=""), "\n")
sep2 <- function() cat(paste(rep("-", 70), collapse=""), "\n")

# ============================================================
sep(); cat("PREIS EBOLA DRC — AUDIT PROFOND\n")
cat("Généré le :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Racine    :", racine, "\n"); sep()

# ============================================================
cat("\n\n1. SOURCES DE DONNÉES — IDENTIFICATION\n"); sep2()
# ============================================================

# 1a. SitReps registry
f_reg <- file.path(racine, "data/final/sitrep_registry.csv")
if (file.exists(f_reg)) {
  reg <- read.csv(f_reg, stringsAsFactors = FALSE)
  cat("\n[SITREP REGISTRY] Colonnes disponibles :\n")
  cat(paste(" -", names(reg), collapse="\n"), "\n")
  cat("\nNombre de SitReps :", nrow(reg), "\n")
  cat("\nAperçu (10 premières lignes, colonnes clés) :\n")
  cols_show <- intersect(c("sitrep_number","date","source","url","filename","title","scraped","extracted"), names(reg))
  if (length(cols_show) > 0) print(head(reg[, cols_show, drop=FALSE], 10))
  cat("\nValeurs manquantes par colonne :\n")
  print(sapply(reg, function(x) sum(is.na(x) | x == "")))
} else {
  cat("[ABSENT] sitrep_registry.csv non trouvé\n")
}

# 1b. État moniteur DHIS2 / cloud
f_state <- list.files(file.path(racine, "data/monitor_state"), full.names=TRUE)
cat("\n\n[MONITOR STATE] Fichiers :\n")
if (length(f_state) > 0) {
  for (f in f_state) {
    cat(" -", basename(f), "\n")
    tryCatch({
      d <- read.csv(f, stringsAsFactors=FALSE)
      cat("   Colonnes:", paste(names(d), collapse=", "), "\n")
      print(head(d, 5))
    }, error=function(e) cat("   [erreur lecture]\n"))
  }
} else cat("  [vide]\n")

# 1c. Données PDF disponibles
f_pdf <- list.files(file.path(racine, "data/pdf"), pattern="\\.(pdf|PDF)$", full.names=FALSE)
cat("\n\n[PDFs DISPONIBLES] Nombre :", length(f_pdf), "\n")
cat(paste(" -", head(f_pdf, 20), collapse="\n"), "\n")
if (length(f_pdf) > 20) cat(" ... et", length(f_pdf)-20, "autres\n")

# 1d. INRB référence
f_inrb <- file.path(racine, "data/final/INRB_reference_national.csv")
if (file.exists(f_inrb)) {
  cat("\n\n[INRB REFERENCE] Colonnes et aperçu :\n")
  d <- read.csv(f_inrb, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  print(head(d, 10))
}

# ============================================================
cat("\n\n2. INDICATEURS — ANALYSE COMPLÈTE PAR SOURCE\n"); sep2()
# ============================================================

# 2a. Indicateurs long format
f_long <- file.path(racine, "data/final/PREIS_indicators_long.csv")
if (file.exists(f_long)) {
  long <- read.csv(f_long, stringsAsFactors=FALSE)
  cat("\n[INDICATORS LONG] Structure :\n")
  cat("Colonnes:", paste(names(long), collapse=", "), "\n")
  cat("Lignes:", nrow(long), "\n")
  print(head(long, 5))

  cat("\n\nCOUVERTURE PAR INDICATEUR (sitreps avec valeur non-NA) :\n")
  col_val <- intersect(c("value","valeur","val"), names(long))[1]
  col_ind <- intersect(c("indicator","indicateur","variable","name"), names(long))[1]
  col_sr  <- intersect(c("sitrep","sitrep_number","sr","week"), names(long))[1]
  if (!is.na(col_ind) && !is.na(col_val)) {
    cov <- tapply(!is.na(long[[col_val]]) & long[[col_val]] != "",
                  long[[col_ind]], sum, na.rm=TRUE)
    cov_df <- data.frame(indicateur=names(cov), n_sitreps=as.integer(cov))
    cov_df <- cov_df[order(-cov_df$n_sitreps),]
    print(cov_df)
  }
}

# 2b. Indicateurs validés
f_val <- file.path(racine, "data/final/PREIS_indicators_validated.csv")
if (file.exists(f_val)) {
  cat("\n\n[INDICATORS VALIDATED] :\n")
  d <- read.csv(f_val, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  print(d)
}

# 2c. Daily indicators
f_daily <- file.path(racine, "data/final/PREIS_daily_indicators.csv")
if (file.exists(f_daily)) {
  cat("\n\n[DAILY INDICATORS] :\n")
  d <- read.csv(f_daily, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  cat("Lignes:", nrow(d), "| Plage dates :")
  col_date <- intersect(c("date","Date","week","Week"), names(d))[1]
  if (!is.na(col_date)) cat(min(d[[col_date]], na.rm=TRUE), "->", max(d[[col_date]], na.rm=TRUE))
  cat("\n")
  print(head(d, 10))
}

# 2d. Candidates (avant validation)
f_cand <- file.path(racine, "data/final/PREIS_indicator_candidates.csv")
if (file.exists(f_cand)) {
  cat("\n\n[INDICATOR CANDIDATES - bruts extraits PDF] :\n")
  d <- read.csv(f_cand, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  print(head(d, 15))
}

# ============================================================
cat("\n\n3. SCRIPTS — CONTENU ET LOGIQUE\n"); sep2()
# ============================================================

scripts_dir <- file.path(racine, "scripts")
scripts_canon <- c(
  "00_config.R",
  "01_fetch_sitreps_from_github.R",
  "02_scrape_insp.R",
  "02_fetch_inrb_reference_data.R",
  "03_extract_pdf.R",
  "03_analyse_consolidee.R",
  "04_extract_indicators.R",
  "05_qc_validate.R",
  "05_synthese_narrative.R",
  "06_analyse_report.R",
  "07_run_pipeline.R",
  "08_cloud_sitrep_monitor.R",
  "10_sitrep_identity.R",
  "11_daily_indicators.R",
  "13_signal_detection.R",
  "14_cfr_scatter_health_zone_english_PPT.R",
  "15_bed_occupancy_analysis_v2.R",
  "60_email.R"
)

for (sc in scripts_canon) {
  fp <- file.path(scripts_dir, sc)
  if (!file.exists(fp)) next

  cat("\n\n>>>", sc, "<<<\n")
  sep2()
  lines <- readLines(fp, warn=FALSE)
  cat("Lignes totales:", length(lines), "\n")

  # Commentaires de tête (documentation)
  head_lines <- head(lines, 30)
  head_comments <- head_lines[grepl("^#", head_lines)]
  if (length(head_comments) > 0) {
    cat("\nDocumentation en-tête :\n")
    cat(paste(head_comments, collapse="\n"), "\n")
  }

  # Packages utilisés
  pkgs <- unique(regmatches(lines, regexpr("(?<=library\\()[^)]+", lines, perl=TRUE)))
  pkgs2 <- unique(regmatches(lines, regexpr("(?<=require\\()[^)]+", lines, perl=TRUE)))
  all_pkgs <- unique(c(pkgs, pkgs2))
  all_pkgs <- all_pkgs[nchar(all_pkgs) > 0]
  if (length(all_pkgs) > 0) cat("\nPackages:", paste(all_pkgs, collapse=", "), "\n")

  # Sources externes (URLs, fichiers lus)
  urls <- unique(grep("https?://", lines, value=TRUE))
  urls <- gsub(".*\"(https?://[^\"]+)\".*", "\\1", urls)
  urls <- urls[grepl("^https?://", urls)]
  if (length(urls) > 0) {
    cat("\nURLs/sources externes :\n")
    cat(paste(" -", unique(urls), collapse="\n"), "\n")
  }

  # Fichiers lus (read.csv, read_csv, readRDS, etc.)
  reads <- grep("read\\.csv|read_csv|readRDS|read_excel|readLines|fromJSON|pdf_text|pdftools", lines, value=TRUE)
  reads <- reads[!grepl("^\\s*#", reads)]
  if (length(reads) > 0) {
    cat("\nFichiers/données lus :\n")
    cat(paste(" ", head(reads, 15), collapse="\n"), "\n")
  }

  # Fichiers écrits
  writes <- grep("write\\.csv|write_csv|saveRDS|ggsave|png\\(|pdf\\(|openxlsx|writexl", lines, value=TRUE)
  writes <- writes[!grepl("^\\s*#", writes)]
  if (length(writes) > 0) {
    cat("\nFichiers/sorties écrits :\n")
    cat(paste(" ", head(writes, 10), collapse="\n"), "\n")
  }

  # Indicateurs / variables nommées
  vars <- unique(regmatches(lines,
    gregexpr("(?<=[\"'])[a-z_]{4,40}(?=[\"'])", lines, perl=TRUE)))
  vars <- unique(unlist(vars))
  vars_epid <- vars[grepl("case|death|cfr|alert|contact|vaccin|lab|isolat|bed|recover|sample|positiv|suspect|confirm|rt_|taux|zone|hospitaliz|admission", vars, ignore.case=TRUE)]
  if (length(vars_epid) > 0) {
    cat("\nVariables épidémiologiques détectées :\n")
    cat(paste(" -", vars_epid, collapse="\n"), "\n")
  }

  # Fonctions définies
  fns <- grep("^[a-zA-Z_\\.]+\\s*<-\\s*function\\(", lines, value=TRUE)
  if (length(fns) > 0) {
    cat("\nFonctions définies :\n")
    cat(paste(" ", head(fns, 10), collapse="\n"), "\n")
  }
}

# ============================================================
cat("\n\n4. DASHBOARD — ANALYSE DÉTAILLÉE\n"); sep2()
# ============================================================

f_app <- file.path(racine, "dashboard_ebola/app.R")
if (file.exists(f_app)) {
  lines <- readLines(f_app, warn=FALSE)
  cat("Lignes totales:", length(lines), "\n")

  # Packages
  pkgs <- unique(regmatches(lines, regexpr("(?<=library\\()[^)]+", lines, perl=TRUE)))
  cat("\nPackages dashboard:", paste(pkgs[nchar(pkgs)>0], collapse=", "), "\n")

  # Sources de données référencées
  data_refs <- grep("read\\.csv|read_csv|readRDS|url\\(|raw\\.githubusercontent", lines, value=TRUE)
  data_refs <- data_refs[!grepl("^\\s*#", data_refs)]
  cat("\nSources de données :\n")
  cat(paste(" ", head(data_refs, 30), collapse="\n"), "\n")

  # Onglets / tabPanel
  tabs <- grep("tabPanel|tabItem|nav_panel|bs4TabItem|menuItem", lines, value=TRUE)
  cat("\nOnglets détectés :\n")
  cat(paste(" ", head(tabs, 30), collapse="\n"), "\n")

  # Indicateurs / KPI affichés
  kpis <- grep("valueBox|infoBox|renderValueBox|renderText.*total|renderText.*cas|renderText.*deces|renderText.*cfr", lines, value=TRUE, ignore.case=TRUE)
  cat("\nKPI / valueBox :\n")
  cat(paste(" ", head(kpis, 20), collapse="\n"), "\n")

  # Variables réactives principales
  reactives <- grep("reactive\\(|reactiveVal|observe\\(|observeEvent", lines, value=TRUE)
  reactives <- reactives[!grepl("^\\s*#", reactives)]
  cat("\nRéactifs principaux :\n")
  cat(paste(" ", head(reactives, 20), collapse="\n"), "\n")

  # DHIS2 mentions
  dhis2 <- grep("dhis2|DHIS2|dhis", lines, value=TRUE, ignore.case=TRUE)
  if (length(dhis2) > 0) {
    cat("\nRéférences DHIS2 dans app.R :\n")
    cat(paste(" ", head(dhis2, 20), collapse="\n"), "\n")
  }

  # i18n / traductions
  i18n <- grep("i18n|translate|lang|FR|EN|fr\\(|en\\(", lines, value=TRUE)
  cat("\nMultilingue i18n (extraits) :\n")
  cat(paste(" ", head(i18n, 10), collapse="\n"), "\n")
}

# ============================================================
cat("\n\n5. QC & VALIDATION\n"); sep2()
# ============================================================

f_qc <- file.path(racine, "data/final/PREIS_QC_issues.csv")
if (file.exists(f_qc)) {
  cat("\n[QC ISSUES] :\n")
  d <- read.csv(f_qc, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  print(d)
}

f_qc2 <- file.path(racine, "data/final/PREIS_QC_by_sitrep.csv")
if (file.exists(f_qc2)) {
  cat("\n[QC BY SITREP] :\n")
  d <- read.csv(f_qc2, stringsAsFactors=FALSE)
  print(d)
}

f_vinrb <- file.path(racine, "data/final/PREIS_validation_vs_INRB.csv")
if (file.exists(f_vinrb)) {
  cat("\n[VALIDATION vs INRB] :\n")
  d <- read.csv(f_vinrb, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  print(d)
}

f_vsig <- file.path(racine, "data/final/PREIS_validation_signals.csv")
if (file.exists(f_vsig)) {
  cat("\n[VALIDATION SIGNALS] :\n")
  d <- read.csv(f_vsig, stringsAsFactors=FALSE)
  print(d)
}

# ============================================================
cat("\n\n6. SIGNAUX DÉTECTÉS\n"); sep2()
# ============================================================

f_sig <- file.path(racine, "data/final/PREIS_signals.csv")
if (file.exists(f_sig)) {
  cat("\n[SIGNALS] :\n")
  d <- read.csv(f_sig, stringsAsFactors=FALSE)
  cat("Colonnes:", paste(names(d), collapse=", "), "\n")
  print(d)
}

# ============================================================
cat("\n\n7. ZONES DE SANTÉ\n"); sep2()
# ============================================================

f_hz <- file.path(racine, "data/final/PREIS_health_zones.csv")
if (file.exists(f_hz)) {
  d <- read.csv(f_hz, stringsAsFactors=FALSE)
  cat("\n[HEALTH ZONES] Colonnes:", paste(names(d), collapse=", "), "\n")
  cat("Total zones:", nrow(d), "\n")
  cat("Zones touchées (si flag) :\n")
  col_aff <- intersect(c("affected","touchee","cases","cas"), names(d))[1]
  if (!is.na(col_aff)) {
    print(subset(d, d[[col_aff]] > 0))
  } else {
    print(head(d, 20))
  }
}

# ============================================================
cat("\n\n8. SÉRIE TEMPORELLE NATIONALE\n"); sep2()
# ============================================================

f_ts <- file.path(racine, "outputs/analyse/serie_temporelle_nationale.csv")
if (file.exists(f_ts)) {
  d <- read.csv(f_ts, stringsAsFactors=FALSE)
  cat("\n[SÉRIE TEMPORELLE] Colonnes:", paste(names(d), collapse=", "), "\n")
  cat("Lignes:", nrow(d), "\n")
  print(head(d, 20))
}

# ============================================================
cat("\n\n9. FICHIERS CURATED (référentiels)\n"); sep2()
# ============================================================

curated <- list.files(file.path(racine, "data/curated"), full.names=FALSE)
cat("\nFichiers dans data/curated/ :\n")
cat(paste(" -", curated, collapse="\n"), "\n")

for (f in curated[grepl("\\.csv$", curated)]) {
  fp <- file.path(racine, "data/curated", f)
  d <- tryCatch(read.csv(fp, stringsAsFactors=FALSE), error=function(e) NULL)
  if (!is.null(d)) {
    cat("\n[", f, "] Colonnes:", paste(names(d), collapse=", "), "| Lignes:", nrow(d), "\n")
    print(head(d, 5))
  }
}

# ============================================================
cat("\n\n10. GITHUB ACTIONS — LOGIQUE COMPLÈTE\n"); sep2()
# ============================================================

wf_dir <- file.path(racine, ".github/workflows")
wf_files <- list.files(wf_dir, pattern="\\.yml$", full.names=TRUE)
for (f in wf_files) {
  cat("\n>>>", basename(f), "<<<\n")
  lines <- readLines(f, warn=FALSE)
  cat(paste(lines, collapse="\n"), "\n")
}

# ============================================================
cat("\n\n11. LOGS — DERNIÈRES ENTRÉES\n"); sep2()
# ============================================================

log_files <- list.files(file.path(racine, "logs"), full.names=TRUE)
cat("Fichiers de logs :", length(log_files), "\n")
for (f in head(log_files, 5)) {
  cat("\n[", basename(f), "]\n")
  lines <- readLines(f, warn=FALSE)
  cat(paste(tail(lines, 20), collapse="\n"), "\n")
}

# ============================================================
cat("\n\n12. SENT LOG & ÉTAT EMAIL\n"); sep2()
# ============================================================

f_sent <- file.path(racine, "data/final/sent_log_sitrep.csv")
if (file.exists(f_sent)) {
  d <- read.csv(f_sent, stringsAsFactors=FALSE)
  cat("[SENT LOG] Colonnes:", paste(names(d), collapse=", "), "\n")
  print(d)
}

f_email_state <- list.files(file.path(racine, "data"), pattern="email_state|sitrep_state", full.names=TRUE, recursive=TRUE)
for (f in f_email_state) {
  cat("\n[", basename(f), "] :\n")
  d <- tryCatch(read.csv(f, stringsAsFactors=FALSE), error=function(e) NULL)
  if (!is.null(d)) print(d)
}

# ============================================================
sep()
cat("FIN AUDIT PROFOND\n")
cat("Rapport écrit :", output_file, "\n")
sep()

sink()
cat("\n>> Rapport écrit :", output_file, "\n")
