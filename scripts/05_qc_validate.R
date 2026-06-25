############################################################
# 05_qc_validate.R — PREIS EBOLA DRC
############################################################

get_indicator_value <- function(df, sitrep_no, code) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  v <- df %>%
    dplyr::filter(.data$sitrep_no == .env$sitrep_no, indicator_code == .env$code, qc_valid == TRUE) %>%
    dplyr::slice(1) %>% dplyr::pull(value)
  if (length(v) == 0) NA_real_ else v[1]
}

make_issue <- function(sitrep_no, severity, issue_code, message, indicator_code = NA_character_) {
  tibble::tibble(
    sitrep_no = as.integer(sitrep_no), severity = severity, issue_code = issue_code,
    indicator_code = indicator_code, message = message, detected_at = as.character(Sys.time())
  )
}

validate_and_derive_indicators <- function(observed_indicators, candidates = tibble::tibble()) {
  if (is.null(observed_indicators) || nrow(observed_indicators) == 0) {
    return(list(validated = tibble::tibble(), qc_issues = tibble::tibble(), qc_by_sitrep = tibble::tibble()))
  }

  validated <- observed_indicators %>%
    dplyr::mutate(qc_valid = TRUE, qc_note = qc_note %||% "selected_observed_candidate")

  issues <- list()
  snos <- sort(unique(validated$sitrep_no))

  # Reject candidate-level bad flags as QC information.
  if (!is.null(candidates) && nrow(candidates) > 0 && "candidate_flag" %in% names(candidates)) {
    bad_cand <- candidates %>% dplyr::filter(candidate_flag != "candidate_ok")
    if (nrow(bad_cand) > 0) {
      issues[[length(issues) + 1]] <- bad_cand %>%
        dplyr::transmute(sitrep_no, severity = "WARNING", issue_code = candidate_flag,
                         indicator_code, message = paste0("Candidate rejected: ", indicator_code, " = ", value, " via ", extraction_rule),
                         detected_at = as.character(Sys.time()))
    }
  }

  # Monotonic validation for cumulative cases/deaths.
  for (code in c("cumulative_confirmed_cases", "cumulative_deaths")) {
    last_valid_value <- NA_real_
    last_valid_sno <- NA_integer_
    for (s in snos) {
      idx <- which(validated$sitrep_no == s & validated$indicator_code == code & validated$qc_valid == TRUE)
      if (length(idx) == 0) next
      val <- validated$value[idx[1]]
      if (!is.na(last_valid_value) && !is.na(val) && val < last_valid_value) {
        validated$qc_valid[idx] <- FALSE
        validated$qc_note[idx] <- paste0("invalidated_decreased_vs_sitrep_", last_valid_sno, "_value_", last_valid_value)
        issues[[length(issues) + 1]] <- make_issue(
          s, "CRITICAL", paste0(code, "_decreased"),
          paste0(code, " baisse de ", last_valid_value, " à ", val,
                 " — valeur invalidée, non utilisée pour dérivations."),
          code
        )
      } else {
        last_valid_value <- val
        last_valid_sno <- s
      }
    }
  }

  # CFR recomputation if missing and cases/deaths valid.
  current_valid <- validated %>% dplyr::filter(qc_valid == TRUE)
  for (s in snos) {
    has_cfr <- any(current_valid$sitrep_no == s & current_valid$indicator_code == "case_fatality_ratio")
    cc <- get_indicator_value(validated, s, "cumulative_confirmed_cases")
    cd <- get_indicator_value(validated, s, "cumulative_deaths")
    if (!has_cfr && !is.na(cc) && !is.na(cd) && cc > 0 && cd <= cc) {
      validated <- dplyr::bind_rows(validated, tibble::tibble(
        sitrep_no = s, indicator_code = "case_fatality_ratio", domain = "deaths",
        value = round(100 * cd / cc, 1), value_source = "derived", source_type = "qc_derivation",
        extraction_rule = "computed_cfr_from_valid_cumulative_cases_deaths", priority = 50L,
        evidence = paste0("100*", cd, "/", cc), extracted_at = as.character(Sys.time()),
        candidate_flag = "candidate_ok", qc_valid = TRUE, qc_note = "derived_after_qc"
      ))
    }
  }

  # Derive new cases/deaths only from immediately previous valid SitRep.
  derive_one <- function(s, cum_code, new_code, domain) {
    cur <- get_indicator_value(validated, s, cum_code)
    if (is.na(cur)) return(NULL)
    prev_s <- s - 1L
    prev <- get_indicator_value(validated, prev_s, cum_code)
    if (is.na(prev)) {
      issues[[length(issues) + 1]] <<- make_issue(
        s, "WARNING", paste0(new_code, "_not_derived_nonconsecutive_or_missing_previous"),
        paste0(new_code, " non dérivé : SitRep précédent valide manquant ou invalide."), new_code
      )
      return(NULL)
    }
    d <- cur - prev
    if (!is.finite(d) || d < 0) {
      issues[[length(issues) + 1]] <<- make_issue(
        s, "CRITICAL", paste0(new_code, "_negative_after_qc"),
        paste0(new_code, " négatif après QC : ", cur, " - ", prev, "."), new_code
      )
      return(NULL)
    }
    tibble::tibble(
      sitrep_no = s, indicator_code = new_code, domain = domain,
      value = as.numeric(d), value_source = "derived", source_type = "qc_derivation",
      extraction_rule = paste0("derived_from_valid_cumulative_difference_vs_sitrep_", prev_s),
      priority = 40L, evidence = paste0(cur, " - ", prev), extracted_at = as.character(Sys.time()),
      candidate_flag = "candidate_ok", qc_valid = TRUE, qc_note = "derived_after_qc_consecutive_previous"
    )
  }

  # Remove any earlier observed new_* then derive cleanly if possible.

  ## ---- PATCH_05B_APPLIED ---- NE PAS SUPPRIMER CETTE LIGNE ----
  ## Derivation lab_positivity_rate ajoutee par 05b_patch_derivations_qc.R
  ## = samples_positive / samples_analyzed x 100 (si absent)
  for (s in snos) {
    has_lpr <- any(validated$sitrep_no == s &
                   validated$indicator_code == 'lab_positivity_rate' &
                   validated$qc_valid == TRUE)
    if (!has_lpr) {
      spos  <- get_indicator_value(validated, s, 'samples_positive')
      sanal <- get_indicator_value(validated, s, 'samples_analyzed')
      if (!is.na(spos) && !is.na(sanal) && sanal > 0 && spos <= sanal) {
        lpr <- round(100 * spos / sanal, 1)
        validated <- dplyr::bind_rows(validated, tibble::tibble(
          sitrep_no = s, indicator_code = 'lab_positivity_rate', domain = 'laboratory',
          value = lpr, value_source = 'derived', source_type = 'qc_derivation',
          extraction_rule = 'computed_positivity_from_samples_positive_analyzed',
          priority = 50L, evidence = paste0('100*', spos, '/', sanal),
          extracted_at = as.character(Sys.time()),
          candidate_flag = 'candidate_ok', qc_valid = TRUE, qc_note = 'derived_after_qc'
        ))
      }
    }
  }
  ## ---- FIN PATCH_05B ----------------------------------------

  validated <- validated %>% dplyr::filter(!indicator_code %in% c("new_confirmed_cases", "new_deaths"))
  derived_rows <- list()
  for (s in snos) {
    dc <- derive_one(s, "cumulative_confirmed_cases", "new_confirmed_cases", "cases")
    dd <- derive_one(s, "cumulative_deaths", "new_deaths", "deaths")
    if (!is.null(dc)) derived_rows[[length(derived_rows) + 1]] <- dc
    if (!is.null(dd)) derived_rows[[length(derived_rows) + 1]] <- dd
  }
  if (length(derived_rows) > 0) validated <- dplyr::bind_rows(validated, dplyr::bind_rows(derived_rows))

  # Missing essential indicators after validation.
  essential <- c("cumulative_confirmed_cases", "cumulative_deaths", "case_fatality_ratio")
  validated_ok <- validated %>% dplyr::filter(qc_valid == TRUE)
  for (s in snos) {
    for (code in essential) {
      if (!any(validated_ok$sitrep_no == s & validated_ok$indicator_code == code)) {
        sev <- ifelse(code %in% c("cumulative_confirmed_cases", "cumulative_deaths"), "CRITICAL", "WARNING")
        issues[[length(issues) + 1]] <- make_issue(s, sev, paste0("missing_", code),
                                                   paste0("Indicateur absent/invalide après QC : ", code), code)
      }
    }
    for (code in c("new_confirmed_cases", "new_deaths", "contacts_followup_rate", "alerts_investigation_rate")) {
      if (!any(validated_ok$sitrep_no == s & validated_ok$indicator_code == code)) {
        issues[[length(issues) + 1]] <- make_issue(s, "WARNING", paste0("missing_", code),
                                                   paste0("Indicateur absent/non dérivé après QC : ", code), code)
      }
    }
  }

  qc_issues <- dplyr::bind_rows(issues)
  if (nrow(qc_issues) == 0) {
    qc_issues <- tibble::tibble(sitrep_no = integer(), severity = character(), issue_code = character(),
                                indicator_code = character(), message = character(), detected_at = character())
  }

  qc_counts <- if (nrow(qc_issues) > 0) {
    qc_issues %>%
      dplyr::count(sitrep_no, severity, name = "n") %>%
      tidyr::pivot_wider(names_from = severity, values_from = n, values_fill = 0)
  } else {
    tibble::tibble(sitrep_no = integer())
  }

  qc_by_sitrep <- tibble::tibble(sitrep_no = snos) %>%
    dplyr::left_join(qc_counts, by = "sitrep_no")

  if (!"CRITICAL" %in% names(qc_by_sitrep)) qc_by_sitrep$CRITICAL <- 0L
  if (!"WARNING" %in% names(qc_by_sitrep)) qc_by_sitrep$WARNING <- 0L
  qc_by_sitrep <- qc_by_sitrep %>%
    dplyr::mutate(
      CRITICAL = tidyr::replace_na(as.integer(CRITICAL), 0L),
      WARNING = tidyr::replace_na(as.integer(WARNING), 0L),
      qc_status = dplyr::case_when(
        CRITICAL > 0 ~ "BLOCKED_CRITICAL",
        WARNING > 0 ~ "PASS_WITH_WARNINGS",
        TRUE ~ "PASS"
      )
    )

  list(validated = validated, qc_issues = qc_issues, qc_by_sitrep = qc_by_sitrep)
}
