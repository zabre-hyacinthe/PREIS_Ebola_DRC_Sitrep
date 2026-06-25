############################################################
# 04_extract_indicators.R — PREIS EBOLA DRC
############################################################

candidate_row <- function(sitrep_no, indicator_code, value, domain, source_type, extraction_rule,
                          priority = 99, evidence = NA_character_) {
  if (length(value) == 0 || is.na(value) || !is.finite(value)) return(tibble::tibble())
  tibble::tibble(
    sitrep_no = as.integer(sitrep_no),
    indicator_code = as.character(indicator_code),
    domain = as.character(domain),
    value = as.numeric(value),
    value_source = "observed",
    source_type = as.character(source_type),
    extraction_rule = as.character(extraction_rule),
    priority = as.integer(priority),
    evidence = as.character(evidence),
    extracted_at = as.character(Sys.time())
  )
}

g_first <- function(pattern, src) {
  m <- stringr::str_match(src, stringr::regex(pattern, ignore_case = TRUE))[, 2]
  if (is.na(m)) NA_real_ else safe_num(m)
}

g_two <- function(pattern, src) {
  m <- stringr::str_match(src, stringr::regex(pattern, ignore_case = TRUE))
  if (is.na(m[1, 2])) return(c(NA_real_, NA_real_))
  c(safe_num(m[1, 2]), safe_num(m[1, 3]))
}

extract_candidates_from_row_text <- function(row_text, sitrep_no, source_type = "text") {
  if (is.na(row_text) || stringr::str_squish(row_text) == "") return(tibble::tibble())
  txt <- stringr::str_squish(row_text)
  out <- list()
  add <- function(code, val, domain, rule, priority) {
    cand <- candidate_row(sitrep_no, code, val, domain, source_type, rule, priority, txt)
    if (nrow(cand) > 0) out[[length(out) + 1]] <<- cand
  }

  # Highest priority: total rows in tables or text-aligned table rows.
  mt <- stringr::str_match(txt, stringr::regex("^\\s*Total\\s+(\\d{2,5})\\s+(\\d{1,5})\\s+(\\d+(?:[.,]\\d+)?)\\s*%?\\s+(\\d+)\\s+sur", ignore_case = TRUE))
  if (!is.na(mt[1, 2])) {
    add("cumulative_confirmed_cases", safe_num(mt[1, 2]), "cases", "total_row_cases_deaths_cfr_hz", 1)
    add("cumulative_deaths", safe_num(mt[1, 3]), "deaths", "total_row_cases_deaths_cfr_hz", 1)
    add("case_fatality_ratio", safe_num(mt[1, 4]), "deaths", "total_row_cases_deaths_cfr_hz", 1)
    add("hz_affected_national", safe_num(mt[1, 5]), "geography", "total_row_cases_deaths_cfr_hz", 1)
  }

  # Alternative total table row where columns are tighter.
  mt2 <- stringr::str_match(txt, stringr::regex("^\\s*Total\\s+(\\d{2,5})\\s+(\\d{1,5})\\s+(\\d+(?:[.,]\\d+)?)", ignore_case = TRUE))
  if (!is.na(mt2[1, 2])) {
    add("cumulative_confirmed_cases", safe_num(mt2[1, 2]), "cases", "total_row_cases_deaths_cfr", 2)
    add("cumulative_deaths", safe_num(mt2[1, 3]), "deaths", "total_row_cases_deaths_cfr", 2)
    add("case_fatality_ratio", safe_num(mt2[1, 4]), "deaths", "total_row_cases_deaths_cfr", 2)
  }

  mi <- stringr::str_match(txt, stringr::regex("^\\s*Ituri\\s+(\\d{1,5})\\s+(\\d{1,5})\\s+(\\d+(?:[.,]\\d+)?)", ignore_case = TRUE))
  if (!is.na(mi[1, 2])) {
    add("cases_ituri", safe_num(mi[1, 2]), "cases", "province_row_ituri", 2)
    add("deaths_ituri", safe_num(mi[1, 3]), "deaths", "province_row_ituri", 2)
  }
  mn <- stringr::str_match(txt, stringr::regex("^\\s*Nord[- ]?Kivu\\s+(\\d{1,5})\\s+(\\d{1,5})\\s+(\\d+(?:[.,]\\d+)?)", ignore_case = TRUE))
  if (!is.na(mn[1, 2])) {
    add("cases_nordkivu", safe_num(mn[1, 2]), "cases", "province_row_nordkivu", 2)
    add("deaths_nordkivu", safe_num(mn[1, 3]), "deaths", "province_row_nordkivu", 2)
  }
  ms <- stringr::str_match(txt, stringr::regex("^\\s*Sud[- ]?Kivu\\s+(\\d{1,5})\\s+(\\d{1,5})\\s+(\\d+(?:[.,]\\d+)?)", ignore_case = TRUE))
  if (!is.na(ms[1, 2])) {
    add("cases_sudkivu", safe_num(ms[1, 2]), "cases", "province_row_sudkivu", 2)
    add("deaths_sudkivu", safe_num(ms[1, 3]), "deaths", "province_row_sudkivu", 2)
  }

  # Narrative and indicators.
  add("new_confirmed_cases", g_first("(\\d+)\\s+nouveaux?\\s+cas\\s+confirm", txt), "cases", "narr_new_confirmed", 4)
  add("new_confirmed_cases", g_first("(\\d+)\\s+Nouveaux?\\s+cas\\s+confirm\\w+\\s+en\\s+date", txt), "cases", "narr_new_confirmed_date", 4)
  add("cumulative_confirmed_cases", g_first("cumul\\s+des\\s+cas\\s+confirm\\w*\\s+s.?[eé]l[eè]ve\\s+[aà]\\s+(\\d{2,5})", txt), "cases", "narr_cumulative_cases_eleve", 3)
  add("cumulative_confirmed_cases", g_first("Cumul\\s+cas\\s+confirm\\w*\\s*:?\\s*(\\d{2,5})", txt), "cases", "narr_cumulative_cases_colon", 3)
  add("cumulative_confirmed_cases", g_first("cumul\\s+de\\s+(\\d{2,5})\\s+cas\\s+confirm", txt), "cases", "narr_cumulative_cases_de", 3)
  add("cumulative_confirmed_cases", g_first("total\\s+de\\s+(\\d{2,5})\\s+cas\\s+ont\\s+[eé]t[eé]\\s+notifi", txt), "cases", "narr_total_cases_notified", 3)
  add("cumulative_deaths", g_first("cumul\\s+des\\s+d[eé]c[eè]s\\s+confirm\\w*\\s+s.?[eé]l[eè]ve\\s+[aà]\\s+(\\d{1,5})", txt), "deaths", "narr_cumulative_deaths_eleve", 3)
  add("cumulative_deaths", g_first("Cumul\\s+d[eé]c[eè]s\\s+confirm\\w*\\s*:?\\s*(\\d{1,5})", txt), "deaths", "narr_cumulative_deaths_colon", 3)

  # Safer horizontal box rules: require at least 2 digits for cumulative cases to avoid "2 Cas confirmés" errors.
  add("cumulative_confirmed_cases", g_first("\\b(\\d{2,5})\\s+Cas\\s+confirm[eé]s\\b", txt), "cases", "box_horizontal_cases_safe", 6)
  add("cumulative_deaths", g_first("\\b(\\d{1,5})\\s+D[eé]c[eè]s\\s+confirm[eé]s\\b", txt), "deaths", "box_horizontal_deaths", 6)
  add("contacts_listed", g_first("\\b(\\d{1,6})\\s+Contacts?\\s+list[eé]s", txt), "contacts", "box_contacts_listed", 6)

  # Alerts and surveillance.
  add("alerts_reported", g_first("Alertes\\s+remont\\w+\\s+(\\d+)", txt), "surveillance", "tbl_alerts_reported", 3)
  ai <- g_two("Alertes\\s+investigu\\w+\\s+(\\d+)\\s*\\((\\d+(?:[.,]\\d+)?)", txt)
  add("alerts_investigated", ai[1], "surveillance", "tbl_alerts_investigated", 3)
  add("alerts_investigation_rate", ai[2], "surveillance", "tbl_alerts_investigation_rate", 3)
  add("alerts_validated", g_first("Alertes\\s+valid\\w+\\s+(\\d+)", txt), "surveillance", "tbl_alerts_validated", 3)
  add("alerts_reported", g_first("(\\d+)\\s+alertes?\\s+ont\\s+[eé]t[eé]\\s+remont", txt), "surveillance", "narr_alerts_reported", 4)
  add("alerts_investigated", g_first("dont\\s+(\\d+)\\s*\\([\\d,.]+\\s*%?\\)\\s*investigu", txt), "surveillance", "narr_alerts_investigated", 4)

  # Laboratory.
  add("samples_collected", g_first("[EÉ]chantillons?\\s+collect\\w+\\s+(\\d+)", txt), "laboratory", "tbl_samples_collected", 3)
  add("samples_analyzed", g_first("[EÉ]chantillons?\\s+analys\\w+\\s+(\\d+)", txt), "laboratory", "tbl_samples_analyzed", 3)
  lp <- g_two("[EÉ]chantillons?\\s+positifs?\\s+(\\d+)\\s+Taux\\s+de\\s+positivit\\w+\\s+(\\d+(?:[.,]\\d+)?)", txt)
  add("samples_positive", lp[1], "laboratory", "tbl_samples_positive", 3)
  add("lab_positivity_rate", lp[2], "laboratory", "tbl_lab_positivity", 3)
  add("samples_received", g_first("[EÉ]chantillons?\\s+re[cç]us\\s*:?\\s*(\\d+)", txt), "laboratory", "narr_samples_received", 4)
  add("samples_collected", g_first("(\\d+)\\s+nouveaux\\s+[eé]chantillons\\s+ont\\s+[eé]t[eé]\\s+collect", txt), "laboratory", "narr_samples_collected", 4)
  add("samples_positive", g_first("(\\d+)\\s+sont\\s+revenus?\\s+positifs?", txt), "laboratory", "narr_samples_positive", 4)

  # Contacts follow-up.
  add("contacts_followup_rate", g_first("(\\d+(?:[.,]\\d+)?)\\s*%\\s*Taux\\s+de\\s+suivi\\s+de\\s*contacts", txt), "contacts", "followup_rate_before_label", 3)
  add("contacts_followup_rate", g_first("Taux\\s+de\\s+suivi\\s+de\\s*contacts\\s*(\\d+(?:[.,]\\d+)?)\\s*%", txt), "contacts", "followup_rate_after_label", 3)
  add("contacts_followup_rate", g_first("taux\\s+global\\s+(\\d+(?:[.,]\\d+)?)\\s*%", txt), "contacts", "followup_rate_global", 4)

  # Care.
  add("patients_in_isolation", g_first("Patients?\\s+en\\s+isolement\\s+(\\d+)", txt), "care", "patients_isolation", 3)
  add("recovered_today", g_first("Gu[eé]ris\\s+du\\s+jour\\s+(\\d+)", txt), "care", "recovered_today", 3)


  ## ---- PATCH_04B_APPLIED ---- NE PAS SUPPRIMER CETTE LIGNE ----
  ## Regles ajoutees par 04b_patch_extraction_indicateurs.R
  ## Reutilise add() et g_first() de la portee locale.

  # Vaccination rVSV-ZEBOV : doses administrees
  add('doses_vaccine_administered', g_first('doses?\\s+(?:de\\s+vaccin\\s+)?administr\\w+\\s+(\\d{1,7})', txt), 'vaccination', 'narr_vaccine_doses', 3)
  add('doses_vaccine_administered', g_first('(\\d{1,7})\\s+personnes?\\s+(?:ont\\s+[eé]t[eé]\\s+)?vaccin', txt), 'vaccination', 'narr_persons_vaccinated', 4)
  add('doses_vaccine_administered', g_first('total\\s+(?:de\\s+)?(\\d{1,7})\\s+(?:personnes?\\s+)?vaccin', txt), 'vaccination', 'narr_total_vaccinated', 4)

  # Agents de sante vaccines (HCW)
  add('hcw_vaccinated', g_first('(\\d{1,6})\\s+agents?\\s+de\\s+sant[eé]\\s+(?:ont\\s+[eé]t[eé]\\s+)?vaccin', txt), 'vaccination', 'narr_hcw_vaccinated', 4)
  add('hcw_vaccinated', g_first('agents?\\s+de\\s+sant[eé]\\s+vaccin\\w+\\s*:?\\s*(\\d{1,6})', txt), 'vaccination', 'tbl_hcw_vaccinated', 3)

  # Vaccination en anneau
  add('ring_vaccination_n', g_first('vaccination\\s+en\\s+anneau\\s*:?\\s*(\\d{1,7})', txt), 'vaccination', 'tbl_ring_vaccination', 3)
  add('ring_vaccination_n', g_first('(\\d{1,7})\\s+contacts?\\s+(?:et\\s+contacts?\\s+de\\s+contacts?\\s+)?vaccin', txt), 'vaccination', 'narr_ring_vaccination', 4)

  # Contacts suivis (tracage)
  add('contacts_followed_up', g_first('(\\d{1,7})\\s+contacts?\\s+(?:sous\\s+|en\\s+)?suivi', txt), 'contacts', 'narr_contacts_followed', 4)
  add('contacts_followed_up', g_first('contacts?\\s+(?:sous\\s+|en\\s+)?suivi\\s*:?\\s*(\\d{1,7})', txt), 'contacts', 'tbl_contacts_followed', 3)
  add('contacts_followed_up', g_first('(\\d{1,7})\\s+contacts?\\s+(?:ont\\s+[eé]t[eé]\\s+)?suivis', txt), 'contacts', 'narr_contacts_suivis', 4)

  # Deces en communaute
  add('deaths_community', g_first('(\\d{1,5})\\s+d[eé]c[eè]s\\s+(?:en\\s+|dans\\s+la\\s+)?communaut', txt), 'deaths', 'narr_deaths_community', 4)
  add('deaths_community', g_first('d[eé]c[eè]s\\s+(?:en\\s+|dans\\s+la\\s+)?communaut\\w*\\s*:?\\s*(\\d{1,5})', txt), 'deaths', 'tbl_deaths_community', 3)

  # Zones de sante affectees par province : format 'Ituri (19/36)'
  add('hz_affected_ituri',    g_first('Ituri\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}', txt), 'geography', 'hz_affected_ituri', 2)
  add('hz_affected_nordkivu', g_first('Nord[- ]?Kivu\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}', txt), 'geography', 'hz_affected_nordkivu', 2)
  add('hz_affected_sudkivu',  g_first('Sud[- ]?Kivu\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}', txt), 'geography', 'hz_affected_sudkivu', 2)
  ## ---- FIN PATCH_04B ----------------------------------------


  ## ---- PATCH_04B_APPLIED ---- NE PAS SUPPRIMER CETTE LIGNE ----
  ## Regles ajoutees par 04b_patch_extraction_indicateurs.R
  ## Reutilise add() et g_first() de la portee locale.

  # Vaccination rVSV-ZEBOV : doses administrees
  add('doses_vaccine_administered', g_first('doses?\\s+(?:de\\s+vaccin\\s+)?administr\\w+\\s+(\\d{1,7})', txt), 'vaccination', 'narr_vaccine_doses', 3)
  add('doses_vaccine_administered', g_first('(\\d{1,7})\\s+personnes?\\s+(?:ont\\s+[eé]t[eé]\\s+)?vaccin', txt), 'vaccination', 'narr_persons_vaccinated', 4)
  add('doses_vaccine_administered', g_first('total\\s+(?:de\\s+)?(\\d{1,7})\\s+(?:personnes?\\s+)?vaccin', txt), 'vaccination', 'narr_total_vaccinated', 4)

  # Agents de sante vaccines (HCW)
  add('hcw_vaccinated', g_first('(\\d{1,6})\\s+agents?\\s+de\\s+sant[eé]\\s+(?:ont\\s+[eé]t[eé]\\s+)?vaccin', txt), 'vaccination', 'narr_hcw_vaccinated', 4)
  add('hcw_vaccinated', g_first('agents?\\s+de\\s+sant[eé]\\s+vaccin\\w+\\s*:?\\s*(\\d{1,6})', txt), 'vaccination', 'tbl_hcw_vaccinated', 3)

  # Vaccination en anneau
  add('ring_vaccination_n', g_first('vaccination\\s+en\\s+anneau\\s*:?\\s*(\\d{1,7})', txt), 'vaccination', 'tbl_ring_vaccination', 3)
  add('ring_vaccination_n', g_first('(\\d{1,7})\\s+contacts?\\s+(?:et\\s+contacts?\\s+de\\s+contacts?\\s+)?vaccin', txt), 'vaccination', 'narr_ring_vaccination', 4)

  # Contacts suivis (tracage)
  add('contacts_followed_up', g_first('(\\d{1,7})\\s+contacts?\\s+(?:sous\\s+|en\\s+)?suivi', txt), 'contacts', 'narr_contacts_followed', 4)
  add('contacts_followed_up', g_first('contacts?\\s+(?:sous\\s+|en\\s+)?suivi\\s*:?\\s*(\\d{1,7})', txt), 'contacts', 'tbl_contacts_followed', 3)
  add('contacts_followed_up', g_first('(\\d{1,7})\\s+contacts?\\s+(?:ont\\s+[eé]t[eé]\\s+)?suivis', txt), 'contacts', 'narr_contacts_suivis', 4)

  # Deces en communaute
  add('deaths_community', g_first('(\\d{1,5})\\s+d[eé]c[eè]s\\s+(?:en\\s+|dans\\s+la\\s+)?communaut', txt), 'deaths', 'narr_deaths_community', 4)
  add('deaths_community', g_first('d[eé]c[eè]s\\s+(?:en\\s+|dans\\s+la\\s+)?communaut\\w*\\s*:?\\s*(\\d{1,5})', txt), 'deaths', 'tbl_deaths_community', 3)

  # Zones de sante affectees par province : format 'Ituri (19/36)'
  add('hz_affected_ituri',    g_first('Ituri\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}', txt), 'geography', 'hz_affected_ituri', 2)
  add('hz_affected_nordkivu', g_first('Nord[- ]?Kivu\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}', txt), 'geography', 'hz_affected_nordkivu', 2)
  add('hz_affected_sudkivu',  g_first('Sud[- ]?Kivu\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}', txt), 'geography', 'hz_affected_sudkivu', 2)
  ## ---- FIN PATCH_04B ----------------------------------------

  dplyr::bind_rows(out)
}

extract_indicator_candidates <- function(line_table, table_rows = tibble::tibble()) {
  if ((is.null(line_table) || nrow(line_table) == 0) && (is.null(table_rows) || nrow(table_rows) == 0)) return(tibble::tibble())
  sitrep_no <- if (!is.null(line_table) && nrow(line_table) > 0) line_table$sitrep_no[1] else table_rows$sitrep_no[1]

  text_rows <- if (!is.null(line_table) && nrow(line_table) > 0) {
    line_table %>%
      dplyr::transmute(sitrep_no, source_type = "text_line", source_id = paste0("p", page, "_l", line_no), row_text = line_text)
  } else tibble::tibble()

  table_text_rows <- if (!is.null(table_rows) && nrow(table_rows) > 0 && "row_text" %in% names(table_rows)) {
    table_rows %>%
      dplyr::transmute(sitrep_no, source_type = "tabulizer_table", source_id = paste0("t", table_id, "_r", row_id), row_text = row_text)
  } else tibble::tibble()

  # Add a full-text row for narrative patterns spanning lines.
  full_text <- if (nrow(text_rows) > 0) {
    tibble::tibble(sitrep_no = sitrep_no, source_type = "full_text", source_id = "full_text",
                   row_text = stringr::str_squish(paste(text_rows$row_text, collapse = " ")))
  } else tibble::tibble()

  all_sources <- dplyr::bind_rows(table_text_rows, text_rows, full_text)
  candidates <- purrr::pmap_dfr(
    list(all_sources$row_text, all_sources$sitrep_no, all_sources$source_type),
    function(row_text, sitrep_no, source_type) extract_candidates_from_row_text(row_text, sitrep_no, source_type)
  )

  if (nrow(candidates) == 0) return(tibble::tibble())

  candidates %>%
    dplyr::filter(!is.na(value), is.finite(value)) %>%
    dplyr::mutate(
      # Extra protection: cumulative confirmed cases below 10 are not acceptable for this epidemic once SitRep >= 16.
      candidate_flag = dplyr::case_when(
        indicator_code == "cumulative_confirmed_cases" & sitrep_no >= 16 & value < 10 ~ "reject_too_small_for_cumulative_cases",
        indicator_code == "case_fatality_ratio" & (value < 0 | value > 100) ~ "reject_invalid_percent",
        indicator_code == "contacts_followup_rate" & (value < 0 | value > 100) ~ "reject_invalid_percent",
        indicator_code == "alerts_investigation_rate" & (value < 0 | value > 100) ~ "reject_invalid_percent",
        indicator_code == "lab_positivity_rate" & (value < 0 | value > 100) ~ "reject_invalid_percent",
        TRUE ~ "candidate_ok"
      )
    )
}

select_best_observed_indicators <- function(candidates) {
  if (is.null(candidates) || nrow(candidates) == 0) return(tibble::tibble())
  if (!"candidate_flag" %in% names(candidates)) candidates$candidate_flag <- "candidate_ok"
  if (!"priority" %in% names(candidates)) candidates$priority <- 99L
  if (!"value_source" %in% names(candidates)) candidates$value_source <- "observed"
  if (!"source_type" %in% names(candidates)) candidates$source_type <- "unknown"
  if (!"evidence" %in% names(candidates)) candidates$evidence <- NA_character_
  candidates %>%
    dplyr::filter(candidate_flag == "candidate_ok") %>%
    dplyr::arrange(sitrep_no, indicator_code, priority, dplyr::desc(value)) %>%
    dplyr::group_by(sitrep_no, indicator_code) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(qc_valid = TRUE, qc_note = "selected_observed_candidate")
}

extract_hz_from_lines <- function(line_table) {
  if (is.null(line_table) || nrow(line_table) == 0) return(tibble::tibble())
  hz_norm <- normalize_text(stringr::str_to_lower(KNOWN_HZ_DICT))
  results <- list()

  for (i in seq_len(nrow(line_table))) {
    txt <- line_table$line_text[i]
    txt_low <- normalize_text(stringr::str_to_lower(txt))
    found <- character()
    for (k in seq_along(KNOWN_HZ_DICT)) {
      zn <- hz_norm[k]
      pat <- paste0("(^|[^a-z])", stringr::str_replace_all(zn, "([\\-])", "\\\\\\1"), "([^a-z]|$)")
      if (stringr::str_detect(txt_low, stringr::regex(pat))) found <- c(found, KNOWN_HZ_DICT[k])
    }
    if (length(found) > 0) {
      context <- normalize_text(stringr::str_to_lower(txt))
      confidence <- dplyr::case_when(
        stringr::str_detect(context, "zone|zones|sante|touch|notif|cas|deces|confirm|province") ~ "high",
        stringr::str_detect(context, "liste|carte|legende|figure") ~ "low_generic_list",
        TRUE ~ "medium"
      )
      results[[length(results) + 1]] <- tibble::tibble(
        sitrep_no = line_table$sitrep_no[i], page = line_table$page[i], line_no = line_table$line_no[i],
        health_zone = unique(found), confidence = confidence, evidence_line = txt
      )
    }
  }

  if (length(results) == 0) return(tibble::tibble())
  dplyr::bind_rows(results) %>%
    dplyr::distinct(sitrep_no, health_zone, .keep_all = TRUE)
}
