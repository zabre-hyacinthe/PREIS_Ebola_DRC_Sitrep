############################################################
# 06_analyse_report.R — PREIS EBOLA DRC
############################################################

getv <- function(indicators, sitrep_no, code) {
  if (is.null(indicators) || nrow(indicators) == 0) return(NA_real_)
  v <- indicators %>%
    dplyr::filter(sitrep_no == .env$sitrep_no, indicator_code == .env$code, qc_valid == TRUE) %>%
    dplyr::arrange(priority) %>%
    dplyr::slice(1) %>%
    dplyr::pull(value)
  if (length(v) == 0) NA_real_ else v[1]
}

gets <- function(indicators, sitrep_no, code) {
  if (is.null(indicators) || nrow(indicators) == 0) return("missing")
  row <- indicators %>%
    dplyr::filter(sitrep_no == .env$sitrep_no, indicator_code == .env$code, qc_valid == TRUE) %>%
    dplyr::arrange(priority) %>%
    dplyr::slice(1)
  if (nrow(row) == 0) return("missing")
  paste0(row$value_source, " / ", row$extraction_rule)
}

analyse_sitrep <- function(validated_indicators, hz_mentions, qc_issues, sitrep_no) {
  cumul_cases   <- getv(validated_indicators, sitrep_no, "cumulative_confirmed_cases")
  new_cases     <- getv(validated_indicators, sitrep_no, "new_confirmed_cases")
  cumul_deaths  <- getv(validated_indicators, sitrep_no, "cumulative_deaths")
  new_deaths    <- getv(validated_indicators, sitrep_no, "new_deaths")
  cfr           <- getv(validated_indicators, sitrep_no, "case_fatality_ratio")
  followup_rate <- getv(validated_indicators, sitrep_no, "contacts_followup_rate")
  inv_rate      <- getv(validated_indicators, sitrep_no, "alerts_investigation_rate")
  positivity    <- getv(validated_indicators, sitrep_no, "lab_positivity_rate")

  hz_s <- if (!is.null(hz_mentions) && nrow(hz_mentions) > 0) {
    hz_mentions %>%
      dplyr::filter(sitrep_no == .env$sitrep_no, confidence %in% c("high", "medium")) %>%
      dplyr::distinct(health_zone) %>% dplyr::pull(health_zone)
  } else character()

  signals <- tibble::tibble(
    indicator_code = c("new_confirmed_cases", "new_deaths", "case_fatality_ratio",
                       "contacts_followup_rate", "alerts_investigation_rate", "lab_positivity_rate"),
    value = c(new_cases, new_deaths, cfr, followup_rate, inv_rate, positivity),
    source = c(gets(validated_indicators, sitrep_no, "new_confirmed_cases"),
               gets(validated_indicators, sitrep_no, "new_deaths"),
               gets(validated_indicators, sitrep_no, "case_fatality_ratio"),
               gets(validated_indicators, sitrep_no, "contacts_followup_rate"),
               gets(validated_indicators, sitrep_no, "alerts_investigation_rate"),
               gets(validated_indicators, sitrep_no, "lab_positivity_rate"))
  ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      signal_level = dplyr::case_when(
        indicator_code == "new_deaths" & value > 0 ~ "RED",
        indicator_code == "case_fatality_ratio" & value >= 15 ~ "RED",
        indicator_code == "contacts_followup_rate" & value < 80 ~ "RED",
        indicator_code == "new_confirmed_cases" & value > 0 ~ "ORANGE",
        indicator_code == "alerts_investigation_rate" & value < 90 ~ "ORANGE",
        indicator_code == "lab_positivity_rate" & value >= 20 ~ "ORANGE",
        indicator_code %in% c("alerts_investigation_rate", "contacts_followup_rate") & value >= 90 ~ "GREEN",
        TRUE ~ "MONITOR"
      ),
      probable_driver = dplyr::case_when(
        indicator_code == "new_deaths" ~ "Présentation tardive, référence tardive, décès communautaires, gaps de prise en charge.",
        indicator_code == "case_fatality_ratio" & value >= 15 ~ "Sous-détection des cas bénins, sévérité clinique, retard de soins ou accès tardif au CTE.",
        indicator_code == "new_confirmed_cases" ~ "Transmission active, isolement incomplet, identification incomplète des contacts ou amélioration de la détection.",
        indicator_code == "contacts_followup_rate" & value < 90 ~ "Capacité insuffisante, mobilité des contacts, réticence communautaire ou retard de mise à jour.",
        indicator_code == "alerts_investigation_rate" & value < 90 ~ "Délai d'investigation, contraintes transport, charge de travail ou remontée tardive.",
        indicator_code == "lab_positivity_rate" & value >= 20 ~ "Transmission active probable, ciblage des prélèvements ou retard dans la rupture des chaînes.",
        TRUE ~ "Monitoring continu requis."
      )
    )

  qc_s <- if (!is.null(qc_issues) && nrow(qc_issues) > 0) qc_issues %>% dplyr::filter(sitrep_no == .env$sitrep_no) else tibble::tibble()

  list(sitrep_no = sitrep_no, cumul_cases = cumul_cases, new_cases = new_cases,
       cumul_deaths = cumul_deaths, new_deaths = new_deaths, cfr = cfr,
       followup_rate = followup_rate, inv_rate = inv_rate, positivity = positivity,
       hz_list = hz_s, signals = signals, qc_issues = qc_s,
       sources = tibble::tibble(
         item = c("Cas cumulés", "Nouveaux cas", "Décès cumulés", "Nouveaux décès", "CFR"),
         source = c(gets(validated_indicators, sitrep_no, "cumulative_confirmed_cases"),
                    gets(validated_indicators, sitrep_no, "new_confirmed_cases"),
                    gets(validated_indicators, sitrep_no, "cumulative_deaths"),
                    gets(validated_indicators, sitrep_no, "new_deaths"),
                    gets(validated_indicators, sitrep_no, "case_fatality_ratio"))
       ))
}

generate_report <- function(analysis) {
  fmt <- function(x) if (is.na(x)) "non disponible" else format(round(x, 1), big.mark = ",", trim = TRUE)
  pct <- function(x) if (is.na(x)) "non disponible" else paste0(round(x, 1), "%")

  n_red <- sum(analysis$signals$signal_level == "RED", na.rm = TRUE)
  n_orange <- sum(analysis$signals$signal_level == "ORANGE", na.rm = TRUE)
  n_green <- sum(analysis$signals$signal_level == "GREEN", na.rm = TRUE)
  hz_str <- if (length(analysis$hz_list) > 0) paste(analysis$hz_list, collapse = ", ") else "non identifiées avec confiance suffisante"

  qc_str <- if (!is.null(analysis$qc_issues) && nrow(analysis$qc_issues) > 0) {
    paste0("[", analysis$qc_issues$severity, "] ", analysis$qc_issues$issue_code, " — ", analysis$qc_issues$message, collapse = "\n")
  } else "[OK] Aucun blocage critique détecté pour ce SitRep."

  sig <- analysis$signals %>% dplyr::filter(signal_level %in% c("RED", "ORANGE", "GREEN"))
  sig_str <- if (nrow(sig) > 0) {
    paste0("[", sig$signal_level, "] ", sig$indicator_code, " = ", round(sig$value, 1),
           " | Source: ", sig$source,
           "\n   Driver probable: ", sig$probable_driver, collapse = "\n")
  } else "Aucun signal critique ou important détecté avec les indicateurs disponibles."

  sources_str <- paste0(analysis$sources$item, " : ", analysis$sources$source, collapse = "\n")

  glue::glue(
    "RAPPORT OPÉRATIONNEL AUTOMATISÉ — PREIS EBOLA RDC\n",
    "SitRep N°{analysis$sitrep_no} | Généré le {Sys.Date()}\n",
    "============================================================\n\n",
    "RÉSUMÉ EXÉCUTIF\n",
    "Signaux critiques (ROUGE): {n_red} | Signaux importants (ORANGE): {n_orange} | Positifs (VERT): {n_green}\n\n",
    "DONNÉES CLÉS\n",
    "Cas confirmés cumulés      : {fmt(analysis$cumul_cases)}\n",
    "Nouveaux cas               : {fmt(analysis$new_cases)}\n",
    "Décès cumulés              : {fmt(analysis$cumul_deaths)}\n",
    "Nouveaux décès             : {fmt(analysis$new_deaths)}\n",
    "Létalité (CFR)             : {pct(analysis$cfr)}\n",
    "Taux de suivi contacts     : {pct(analysis$followup_rate)}\n",
    "Taux d'investigation alertes: {pct(analysis$inv_rate)}\n",
    "Positivité laboratoire     : {pct(analysis$positivity)}\n\n",
    "SOURCES DES VALEURS CLÉS\n",
    "{sources_str}\n\n",
    "ZONES DE SANTÉ DÉTECTÉES\n",
    "{hz_str}\n\n",
    "CONTRÔLE QUALITÉ\n",
    "{qc_str}\n\n",
    "SIGNAUX OPÉRATIONNELS\n",
    "{sig_str}\n\n",
    "NOTE : Drivers probables uniquement — pas de causalité établie.\n",
    "Valider avec ligne-liste officielle, registre contacts, laboratoire et données CTE.\n"
  )
}
