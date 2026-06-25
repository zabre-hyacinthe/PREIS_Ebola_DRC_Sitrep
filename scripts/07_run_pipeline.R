############################################################
# 07_run_pipeline.R — PREIS EBOLA DRC
############################################################

run_preis_pipeline <- function(force_redownload = FALSE, force_reextract = FALSE,
                               max_new = Inf, process_all_known = FALSE,
                               enable_tabulizer = TRUE,
                               block_email_on_critical = TRUE) {

  cat("\n============================================================\n")
  cat("PREIS EBOLA DRC — MASTER AUTOMATION PIPELINE PRODUCTION\n")
  cat("Run time:", as.character(Sys.time()), "\n")
  cat("============================================================\n\n")

  cat("--- ÉTAPE 1: Scraping INSP ---\n")
  scraped <- scrape_insp_sitrep_list()
  if (nrow(scraped) == 0) {
    cat("Impossible de scraper la page. Pipeline arrêté.\n")
    return(invisible(NULL))
  }

  registry <- load_registry()
  merged <- merge_registry(scraped, registry)
  registry <- merged$registry
  new_or_pending <- merged$new_or_pending
  save_registry(registry)

  if (process_all_known) {
    to_process <- registry %>% dplyr::filter(pdf_url %in% scraped$pdf_url)
  } else {
    to_process <- registry %>%
      dplyr::filter(pdf_url %in% scraped$pdf_url, is.na(extracted) | extracted == FALSE | force_reextract)
  }
  to_process <- to_process %>% dplyr::arrange(dplyr::desc(sitrep_no))
  if (!is.infinite(max_new)) to_process <- to_process %>% dplyr::slice_head(n = max_new)

  all_lines <- list(); all_tables <- list(); all_candidates <- list(); all_hz <- list()
  processed_success <- integer(0)

  if (nrow(to_process) > 0) {
    cat("\n--- ÉTAPE 2: Téléchargement, extraction texte/tableaux ---\n")
    for (i in seq_len(nrow(to_process))) {
      row <- to_process[i, ]
      sno <- row$sitrep_no
      purl <- row$pdf_url
      cat("\n>> SitRep", sno, ":", paste0("SitRep_", sprintf("%02d", sno), "_2026.pdf"), "\n")

      local_pdf <- download_sitrep_pdf(purl, sno, force_redownload = force_redownload)
      registry$downloaded[registry$pdf_url == purl] <- !is.na(local_pdf)
      registry$local_pdf[registry$pdf_url == purl] <- local_pdf %||% NA_character_
      if (is.na(local_pdf)) next

      bundle <- extract_pdf_bundle(local_pdf, sno, enable_tabulizer = enable_tabulizer)
      lines <- bundle$lines
      tables <- bundle$tables
      if (nrow(lines) == 0) next

      all_lines[[as.character(sno)]] <- lines
      if (nrow(tables) > 0) all_tables[[as.character(sno)]] <- tables

      cand <- extract_indicator_candidates(lines, tables)
      cat("   Indicator candidates:", nrow(cand), "\n")
      if (nrow(cand) > 0) all_candidates[[as.character(sno)]] <- cand

      hz <- extract_hz_from_lines(lines)
      n_hz <- if (nrow(hz) > 0) dplyr::n_distinct(hz$health_zone) else 0
      n_hz_conf <- if (nrow(hz) > 0) dplyr::n_distinct(hz$health_zone[hz$confidence %in% c("high", "medium")]) else 0
      cat("   Health zones found:", n_hz, "| high/medium confidence:", n_hz_conf, "\n")
      if (nrow(hz) > 0) all_hz[[as.character(sno)]] <- hz

      registry$extracted[registry$pdf_url == purl] <- TRUE
      registry$analysed[registry$pdf_url == purl] <- TRUE
      registry$last_updated[registry$pdf_url == purl] <- as.character(Sys.time())
      processed_success <- c(processed_success, sno)
    }
    save_registry(registry)
  } else {
    cat("\nAucun nouveau SitRep à extraire. Re-génération QC/rapport avec les données existantes.\n")
  }

  cat("\n--- ÉTAPE 3: Consolidation, validation QC & export ---\n")

  lines_df <- dplyr::bind_rows(all_lines)
  tables_df <- dplyr::bind_rows(all_tables)
  cand_new <- dplyr::bind_rows(all_candidates)
  hz_new <- dplyr::bind_rows(all_hz)

  if (nrow(lines_df) > 0) {
    safe_write_csv(lines_df, LINES_FP)
    cat("   Extracted lines saved:", basename(LINES_FP), "\n")
  }
  if (nrow(tables_df) > 0) {
    safe_write_csv(tables_df, TABLE_ROWS_FP)
    cat("   Table rows saved:", basename(TABLE_ROWS_FP), "\n")
  }

  candidates_all <- if (nrow(cand_new) > 0) {
    append_distinct_csv(cand_new, CANDIDATES_FP, c("sitrep_no", "indicator_code", "value", "extraction_rule", "evidence"))
  } else read_csv_if_exists(CANDIDATES_FP)

  hz_all <- if (nrow(hz_new) > 0) {
    append_distinct_csv(hz_new, HEALTH_ZONES_FP, c("sitrep_no", "health_zone"))
  } else read_csv_if_exists(HEALTH_ZONES_FP)

  observed_selected <- select_best_observed_indicators(candidates_all)
  qc_res <- validate_and_derive_indicators(observed_selected, candidates_all)
  validated <- qc_res$validated
  qc_issues <- qc_res$qc_issues
  qc_by <- qc_res$qc_by_sitrep

  safe_write_csv(validated, VALIDATED_FP)
  safe_write_csv(qc_issues, QC_ISSUES_FP)
  safe_write_csv(qc_by, QC_BY_SITREP_FP)

  cat("   Candidates saved:", nrow(candidates_all), "rows\n")
  cat("   Validated indicators saved:", nrow(validated), "rows\n")
  cat("   Health zones saved:", nrow(hz_all), "rows\n")
  cat("   QC critical:", sum(qc_issues$severity == "CRITICAL", na.rm = TRUE), "\n")
  cat("   QC warning:", sum(qc_issues$severity == "WARNING", na.rm = TRUE), "\n")

  # Generate reports for all SitReps with validated or QC data, not only this run.
  report_snos <- sort(unique(c(validated$sitrep_no, qc_by$sitrep_no)))
  all_reports <- list()
  for (s in report_snos) {
    cat("   Analysing SitRep", s, "\n")
    analysis <- analyse_sitrep(validated, hz_all, qc_issues, s)
    report <- generate_report(analysis)
    txt_fp <- file.path(OUTPUT_DIR, paste0("PREIS_Report_SitRep_", s, "_", Sys.Date(), ".txt"))
    writeLines(report, txt_fp, useBytes = TRUE)
    all_reports[[as.character(s)]] <- list(sitrep_no = s, analysis = analysis, report = report, txt_fp = txt_fp)
    cat("   Report saved:", basename(txt_fp), "\n")
  }

  latest_sitrep <- if (length(report_snos) > 0) max(report_snos, na.rm = TRUE) else NA_integer_
  latest_fp <- NA_character_
  if (!is.na(latest_sitrep) && as.character(latest_sitrep) %in% names(all_reports)) {
    latest_report <- all_reports[[as.character(latest_sitrep)]]$report
    latest_fp <- file.path(OUTPUT_DIR, paste0("PREIS_Report_LATEST_SitRep_", latest_sitrep, ".txt"))
    writeLines(latest_report, latest_fp, useBytes = TRUE)
    cat("   Latest report copy saved:", basename(latest_fp), "\n")
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "indicator_candidates")
  openxlsx::writeData(wb, "indicator_candidates", candidates_all)
  openxlsx::addWorksheet(wb, "indicators_validated")
  openxlsx::writeData(wb, "indicators_validated", validated)
  openxlsx::addWorksheet(wb, "health_zones")
  openxlsx::writeData(wb, "health_zones", hz_all)
  openxlsx::addWorksheet(wb, "qc_by_sitrep")
  openxlsx::writeData(wb, "qc_by_sitrep", qc_by)
  openxlsx::addWorksheet(wb, "qc_issues")
  openxlsx::writeData(wb, "qc_issues", qc_issues)
  openxlsx::addWorksheet(wb, "registry")
  openxlsx::writeData(wb, "registry", registry)
  xl_fp <- file.path(OUTPUT_DIR, paste0("PREIS_Output_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
  openxlsx::saveWorkbook(wb, xl_fp, overwrite = TRUE)
  cat("   Excel saved:", basename(xl_fp), "\n")

  log_entry <- tibble::tibble(
    run_time = as.character(Sys.time()),
    n_scraped = nrow(scraped),
    n_new_or_pending = nrow(new_or_pending),
    n_requested_this_run = nrow(to_process),
    n_processed_success = length(processed_success),
    n_candidates_total = nrow(candidates_all),
    n_validated_indicators = nrow(validated),
    n_health_zones = if (nrow(hz_all) > 0) dplyr::n_distinct(hz_all$health_zone) else 0,
    n_qc_critical = sum(qc_issues$severity == "CRITICAL", na.rm = TRUE),
    n_qc_warning = sum(qc_issues$severity == "WARNING", na.rm = TRUE),
    latest_report_sitrep = latest_sitrep,
    latest_report_file = latest_fp
  )
  if (file.exists(RUN_LOG_FP)) {
    old_log <- read_csv_if_exists(RUN_LOG_FP)
    log_entry <- ({
  # PREIS PATCH 07 LOG TYPE FIX START
  # Purpose: make run log append stable across readr datetime/character guesses.
  if (exists("old_log") && is.data.frame(old_log) && "run_time" %in% names(old_log)) {
    old_log$run_time <- as.character(old_log$run_time)
  }
  if (exists("log_entry") && is.data.frame(log_entry) && "run_time" %in% names(log_entry)) {
    log_entry$run_time <- as.character(log_entry$run_time)
  }
  dplyr::bind_rows(old_log, log_entry)
  # PREIS PATCH 07 LOG TYPE FIX END
})
  }
  safe_write_csv(log_entry, RUN_LOG_FP)

  cat("\n============================================================\n")
  cat("PIPELINE TERMINÉ —", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("SitReps scrapés       :", nrow(scraped), "\n")
  cat("Nouveaux/pending      :", nrow(new_or_pending), "\n")
  cat("Demandés ce run       :", nrow(to_process), "\n")
  cat("Traités avec succès   :", length(processed_success), "\n")
  cat("Indicateurs validés   :", nrow(validated), "\n")
  cat("Zones de santé        :", if (nrow(hz_all) > 0) dplyr::n_distinct(hz_all$health_zone) else 0, "\n")
  cat("QC critical           :", sum(qc_issues$severity == "CRITICAL", na.rm = TRUE), "\n")
  cat("QC warning            :", sum(qc_issues$severity == "WARNING", na.rm = TRUE), "\n")
  cat("Latest report         :", basename(latest_fp), "\n")
  cat("============================================================\n\n")

  invisible(list(registry = registry, candidates = candidates_all, validated = validated,
                 health_zones = hz_all, qc_issues = qc_issues, qc_by_sitrep = qc_by,
                 reports = all_reports, latest_report = latest_fp, excel = xl_fp))
}
