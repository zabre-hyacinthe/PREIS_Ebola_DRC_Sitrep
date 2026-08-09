############################################################
# preis_email_enrichi.R
#
# Notification PREIS unique, envoyee apres l'extraction et
# l'analyse consolidee du nouveau SitRep.
#
# Contenu :
#   - cas confirmes cumules, deces et letalite ;
#   - variation du cumul depuis le SitRep precedent ;
#   - cinq zones de sante les plus touchees ;
#   - points saillants calcules a partir des donnees analysees ;
#   - PDF officiel uniquement en piece jointe.
#
# Securites (INCHANGEES) :
#   - aucun lien dashboard ;
#   - aucun graphique ni carte joints ;
#   - validation du numero inscrit dans le PDF ;
#   - blocage si un PDF officiel plus recent que la serie existe ;
#   - anti-doublon : data/preis_email_enrichi_state.csv.
#
# CHANGE 2026-07-29 : email bilingue (anglais par defaut, comme le
# reste du dashboard -- I18N app.R). Controle par la variable
# d'environnement PREIS_EMAIL_LANG ("en" ou "fr", "en" par defaut).
# Aucune logique metier modifiee -- uniquement le texte affiche.
# Le document Africa CDC (docx) N'EST PAS joint ici : cette
# securite est deliberee et reste intacte. Il part dans un email
# SEPARE -- voir 05_send_africacdc_sitrep_email.R.
#
# Usage interactif :
#   source("preis_email_enrichi.R")
#   preis_email_enrichi(force = TRUE)
#   preis_email_enrichi(force = TRUE, lang = "fr")  # pour forcer le francais
############################################################

.env_get <- function(names, default = "") {
  for (n in names) {
    v <- Sys.getenv(n, "")
    if (nzchar(v)) return(v)
  }
  default
}

.fmt <- function(x, lang = "en") {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) == 0 || is.na(x)) return(if (lang == "fr") "n/d" else "n/a")
  formatC(round(x), format = "d", big.mark = if (lang == "fr") " " else ",")
}

.html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# ---- Bilingual string table (same pattern as I18N in app.R) --------
# Falls back to English if a key is somehow missing for the chosen
# language, same fallback convention as the dashboard's tr().
.EMAIL_L <- list(
  en = c(
    title             = "PREIS \u2014 Ebola DRC",
    subtitle          = "Automatic summary \u2014 SitRep No.",
    kpi_cases         = "Cumulative confirmed cases",
    kpi_deaths        = "Cumulative deaths",
    kpi_cfr           = "Case fatality (CFR)",
    evolution_since   = "Change since SitRep No.",
    evolution_note    = "The change in cumulative totals may include new notifications and retrospective consolidations.",
    zones_title       = "Most affected health zones",
    zone_col_name     = "Health zone",
    zone_col_cases    = "Cumulative cases",
    highlights_title  = "Key highlights",
    unit_cases        = "cases",
    unit_deaths       = "deaths",
    in_national_total = "in the national cumulative total",
    change_cases_na   = "case change: n/a",
    change_deaths_na  = "death change: n/a",
    zone_top_lead     = "Among the reported zones, ",
    zone_top_mid      = " has the highest cumulative total, with ",
    zone_top_tail     = " cases. ",
    zone_unavailable  = "The health-zone breakdown is not available in the analysed file. ",
    evo_lead          = "Since SitRep No. ",
    evo_mid1          = ", the national cumulative total changed by ",
    evo_mid2          = " and ",
    evo_tail          = ".",
    evo_unavailable   = "The change since the previous SitRep is not available.",
    footer_text       = "The complete official report, validated as SitRep No. %s, is attached as a PDF. Provisional data, source: MoH DRC (INSP/INRB).",
    plain_fallback    = "See the PREIS summary and the attached official report.",
    subject_fmt       = "PREIS Ebola DRC - SitRep No.%03d: %s cases / %s deaths (CFR %s)"
  ),
  fr = c(
    title             = "PREIS \u2014 MVE Ebola RDC",
    subtitle          = "Synth\u00e8se automatique \u2014 SitRep N\u00b0",
    kpi_cases         = "Cas confirm\u00e9s cumul\u00e9s",
    kpi_deaths        = "D\u00e9c\u00e8s cumul\u00e9s",
    kpi_cfr           = "L\u00e9talit\u00e9 (CFR)",
    evolution_since   = "\u00c9volution depuis le SitRep N\u00b0",
    evolution_note    = "La variation du cumul peut inclure les nouvelles notifications et des consolidations r\u00e9trospectives.",
    zones_title       = "Zones les plus touch\u00e9es",
    zone_col_name     = "Zone de sant\u00e9",
    zone_col_cases    = "Cas cumul\u00e9s",
    highlights_title  = "Points saillants",
    unit_cases        = "cas",
    unit_deaths       = "d\u00e9c\u00e8s",
    in_national_total = "dans le cumul national",
    change_cases_na   = "variation des cas : n/d",
    change_deaths_na  = "variation des d\u00e9c\u00e8s : n/d",
    zone_top_lead     = "Parmi les zones renseign\u00e9es, ",
    zone_top_mid      = " pr\u00e9sente le cumul le plus \u00e9lev\u00e9, avec ",
    zone_top_tail     = " cas. ",
    zone_unavailable  = "La r\u00e9partition par zone de sant\u00e9 n'est pas disponible dans le fichier analys\u00e9. ",
    evo_lead          = "Depuis le SitRep N\u00b0",
    evo_mid1          = ", le cumul national a vari\u00e9 de ",
    evo_mid2          = " et de ",
    evo_tail          = ".",
    evo_unavailable   = "La variation depuis le SitRep pr\u00e9c\u00e9dent n'est pas disponible.",
    footer_text       = "Le rapport officiel complet, valid\u00e9 comme SitRep N\u00b0%s, est joint au format PDF. Donn\u00e9es provisoires, source INSP/INRB RDC.",
    plain_fallback    = "Consultez la synth\u00e8se PREIS et le rapport officiel joint.",
    subject_fmt       = "PREIS MVE RDC - SitRep N%03d : %s cas / %s deces (CFR %s)"
  )
)
.tr <- function(key, lang) {
  v <- .EMAIL_L[[lang]][[key]]
  if (is.null(v) || is.na(v)) v <- .EMAIL_L[["en"]][[key]]
  v
}

.pdf_internal_numbers <- function(pdf_file, max_pages = 2L) {
  if (!file.exists(pdf_file) || file.info(pdf_file)$size < 5000) {
    return(integer())
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package R 'pdftools' manquant : validation du PDF impossible.")
  }

  pages <- tryCatch(
    pdftools::pdf_text(pdf_file),
    error = function(e) character()
  )

  if (length(pages) == 0) return(integer())

  header_text <- paste(pages[seq_len(min(2, length(pages)))], collapse = " ")
  matches <- stringr::str_match_all(
    header_text,
    stringr::regex("SitRep\\s*N\\s*[\u00b0\u00bao]?\\s*0*([0-9]{1,3})", ignore_case = TRUE)
  )[[1]][, 2]

  suppressWarnings(as.integer(unique(matches)))
}

.latest_valid_higher_pdf <- function(pdf_dir, sno) {
  files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
  if (length(files) == 0) return(NULL)

  nums <- suppressWarnings(as.integer(
    stringr::str_match(basename(files), "SitRep_(\\d+)_2026\\.pdf")[, 2]
  ))
  nums2 <- suppressWarnings(as.integer(
    stringr::str_match(basename(files), "PREIS_DRC_Ebola_SitRep_(\\d+)\\.pdf")[, 2]
  ))
  nums <- ifelse(is.na(nums), nums2, nums)

  higher <- which(!is.na(nums) & nums > sno)
  if (length(higher) == 0) return(NULL)

  for (i in higher[order(-nums[higher])]) {
    internal <- tryCatch(.pdf_internal_numbers(files[i]), error = function(e) integer())
    if (nums[i] %in% internal) {
      return(list(number = nums[i], file = files[i]))
    }
  }
  NULL
}

preis_email_enrichi <- function(root = NULL, force = FALSE,
                                 lang = Sys.getenv("PREIS_EMAIL_LANG", "en")) {
  lang <- if (tolower(lang) %in% c("fr", "francais", "french")) "fr" else "en"

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  if (is.null(root)) {
    root <- if (file.exists(file.path(
      getwd(), "outputs", "analyse", "serie_temporelle_nationale.csv"
    ))) {
      getwd()
    } else {
      "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
    }
  }

  if (!dir.exists(root)) {
    cat("Dossier racine introuvable :", root, "\n")
    return(invisible(FALSE))
  }

  setwd(root)

  suppressPackageStartupMessages({
    library(readr)
    library(jsonlite)
    library(stringr)
  })

  serie_fp <- "outputs/analyse/serie_temporelle_nationale.csv"
  zones_fp <- "outputs/analyse/tableau_zones_sante.csv"
  pdf_dir <- "data/pdf"

  if (!file.exists(serie_fp)) {
    cat("Serie introuvable :", serie_fp, "\n")
    return(invisible(FALSE))
  }

  serie <- tryCatch(
    readr::read_csv(serie_fp, show_col_types = FALSE),
    error = function(e) NULL
  )

  required_columns <- c("sitrep_no", "date", "cas_cumules", "deces_cumules")

  if (is.null(serie) || !all(required_columns %in% names(serie))) {
    cat(
      "Serie invalide. Colonnes requises :",
      paste(required_columns, collapse = ", "),
      "\n"
    )
    return(invisible(FALSE))
  }

  serie$sitrep_no <- suppressWarnings(as.integer(serie$sitrep_no))
  serie <- serie[!is.na(serie$sitrep_no), , drop = FALSE]
  serie <- serie[order(serie$sitrep_no), , drop = FALSE]
  serie <- serie[!duplicated(serie$sitrep_no, fromLast = TRUE), , drop = FALSE]

  n <- nrow(serie)

  if (n == 0) {
    cat("Serie vide.\n")
    return(invisible(FALSE))
  }

  last <- serie[n, , drop = FALSE]
  prev <- if (n >= 2) serie[n - 1, , drop = FALSE] else last

  sno <- as.integer(last$sitrep_no[[1]])
  prev_sno <- as.integer(prev$sitrep_no[[1]])
  d_date <- as.character(last$date[[1]])
  cas <- suppressWarnings(as.numeric(last$cas_cumules[[1]]))
  dec <- suppressWarnings(as.numeric(last$deces_cumules[[1]]))

  if (is.na(sno) || is.na(cas) || is.na(dec) || cas <= 0 || dec < 0) {
    cat("Indicateurs nationaux invalides pour le dernier SitRep.\n")
    return(invisible(FALSE))
  }

  cfr_raw <- if ("cfr" %in% names(last)) {
    suppressWarnings(as.numeric(last$cfr[[1]]))
  } else {
    NA_real_
  }

  cfr_pct <- if (is.na(cfr_raw)) {
    100 * dec / cas
  } else if (cfr_raw >= 0 && cfr_raw <= 1.5) {
    100 * cfr_raw
  } else {
    cfr_raw
  }

  nvx_cas <- suppressWarnings(
    as.numeric(last$cas_cumules[[1]]) - as.numeric(prev$cas_cumules[[1]])
  )
  nvx_dec <- suppressWarnings(
    as.numeric(last$deces_cumules[[1]]) - as.numeric(prev$deces_cumules[[1]])
  )

  # Ne jamais envoyer une synthese ancienne si un PDF officiel plus recent
  # est deja present et porte correctement son propre numero.
  higher_pdf <- .latest_valid_higher_pdf(pdf_dir, sno)

  if (!is.null(higher_pdf)) {
    cat(
      "Envoi bloque : la serie s'arrete au SitRep", sno,
      "mais un PDF officiel valide N", higher_pdf$number,
      "est deja present :", higher_pdf$file, "\n"
    )
    return(invisible(FALSE))
  }

 pdf_candidates <- file.path(pdf_dir, c(
    sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", sno),
    sprintf("PREIS_DRC_Ebola_SitRep_%d.pdf",   sno),
    sprintf("SitRep_%02d_2026.pdf",            sno),
    sprintf("SitRep_%d_2026.pdf",              sno)
  ))
  pdf_fp <- pdf_candidates[file.exists(pdf_candidates)][1]

  if (is.na(pdf_fp)) {
    cat("Envoi bloque : PDF officiel introuvable. Cherche :",
        paste(basename(pdf_candidates), collapse = ", "), "\n")
    return(invisible(FALSE))
  }

  pdf_numbers <- tryCatch(
    .pdf_internal_numbers(pdf_fp),
    error = function(e) {
      cat("Validation PDF impossible :", conditionMessage(e), "\n")
      integer()
    }
  )

  if (!sno %in% pdf_numbers) {
    cat(
      "Envoi bloque : le PDF", basename(pdf_fp),
      "ne contient pas la mention SitRep N", sprintf("%03d", sno), "\n"
    )
    return(invisible(FALSE))
  }

  cat(
    "Validation PDF OK : SitRep N", sprintf("%03d", sno),
    "confirme dans", basename(pdf_fp), "\n"
  )

  z <- NULL
  top_html <- ""

  if (file.exists(zones_fp)) {
    z <- tryCatch(
      readr::read_csv(zones_fp, show_col_types = FALSE),
      error = function(e) NULL
    )

    if (!is.null(z) && all(c("nom", "cas") %in% names(z)) && nrow(z) > 0) {
      z$cas <- suppressWarnings(as.numeric(z$cas))
      z <- z[!is.na(z$cas) & !is.na(z$nom) & nzchar(trimws(z$nom)), , drop = FALSE]
      z <- z[order(-z$cas, z$nom), , drop = FALSE]
      z <- head(z, 5)

      rows <- paste0(
        "<tr>",
        "<td style='padding:6px 10px;border-bottom:1px solid #e6e6e6'>",
        .html_escape(z$nom),
        "</td>",
        "<td style='padding:6px 10px;border-bottom:1px solid #e6e6e6;text-align:right'><b>",
        vapply(z$cas, .fmt, character(1), lang = lang),
        "</b></td></tr>",
        collapse = ""
      )

      top_html <- paste0(
        "<table role='presentation' style='border-collapse:collapse;margin-top:6px;min-width:360px'>",
        "<tr><th style='text-align:left;padding:6px 10px;background:#f3f5f4'>", .tr("zone_col_name", lang), "</th>",
        "<th style='text-align:right;padding:6px 10px;background:#f3f5f4'>", .tr("zone_col_cases", lang), "</th></tr>",
        rows,
        "</table>"
      )
    }
  }

  state_fp <- "data/preis_email_enrichi_state.csv"
  dir.create(dirname(state_fp), recursive = TRUE, showWarnings = FALSE)

  state <- if (file.exists(state_fp)) {
    tryCatch(
      readr::read_csv(state_fp, show_col_types = FALSE),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  already <- !is.null(state) &&
    "sitrep_no" %in% names(state) &&
    sno %in% suppressWarnings(as.integer(state$sitrep_no))

  if (already && !isTRUE(force)) {
    cat(
      "SitRep", sno,
      "deja notifie par l'e-mail enrichi. Aucun nouvel envoi.\n"
    )
    return(invisible(TRUE))
  }

  atts <- pdf_fp
  cfr_txt <- sprintf("%.1f %%", cfr_pct)

  kpi <- function(val, lab, col) {
    paste0(
      "<td style='padding:14px 18px;background:", col,
      ";color:#fff;border-radius:10px'>",
      "<div style='font-size:26px;font-weight:700'>", val, "</div>",
      "<div style='font-size:12px;margin-top:2px'>", lab, "</div>",
      "</td>"
    )
  }

  variation_cases <- if (!is.na(nvx_cas)) {
    paste0(
      if (nvx_cas >= 0) "+" else "",
      .fmt(nvx_cas, lang),
      " ", .tr("unit_cases", lang), " ", .tr("in_national_total", lang)
    )
  } else {
    .tr("change_cases_na", lang)
  }

  variation_deaths <- if (!is.na(nvx_dec)) {
    paste0(
      if (nvx_dec >= 0) "+" else "",
      .fmt(nvx_dec, lang),
      " ", .tr("unit_deaths", lang), " ", .tr("in_national_total", lang)
    )
  } else {
    .tr("change_deaths_na", lang)
  }

  point_zone <- if (!is.null(z) && nrow(z) > 0) {
    paste0(
      .tr("zone_top_lead", lang),
      "<b>", .html_escape(z$nom[1]), "</b>",
      .tr("zone_top_mid", lang),
      "<b>", .fmt(z$cas[1], lang), .tr("zone_top_tail", lang), "</b>"
    )
  } else {
    .tr("zone_unavailable", lang)
  }

  point_evolution <- if (!is.na(nvx_cas) && !is.na(nvx_dec)) {
    paste0(
      .tr("evo_lead", lang), sprintf("%03d", prev_sno),
      .tr("evo_mid1", lang),
      "<b>", if (nvx_cas >= 0) "+" else "", .fmt(nvx_cas, lang), " ", .tr("unit_cases", lang), "</b>",
      .tr("evo_mid2", lang),
      "<b>", if (nvx_dec >= 0) "+" else "", .fmt(nvx_dec, lang), " ", .tr("unit_deaths", lang), "</b>",
      .tr("evo_tail", lang)
    )
  } else {
    .tr("evo_unavailable", lang)
  }

  html <- paste0(
    "<div style='font-family:Segoe UI,Arial,sans-serif;color:#252525;max-width:700px;line-height:1.45'>",

    "<div style='background:#0B4F3C;color:#fff;padding:18px 22px;border-radius:10px'>",
    "<div style='font-size:19px;font-weight:700'>", .tr("title", lang), "</div>",
    "<div style='font-size:14px;margin-top:3px'>", .tr("subtitle", lang), " ",
    sprintf("%03d", sno),
    if (nzchar(d_date) && d_date != "NA") paste0(" (", .html_escape(d_date), ")") else "",
    "</div></div>",

    "<table role='presentation' style='border-spacing:9px 0;margin:16px 0;width:100%'><tr>",
    kpi(.fmt(cas, lang), .tr("kpi_cases", lang), "#C83A2D"),
    kpi(.fmt(dec, lang), .tr("kpi_deaths", lang), "#33485D"),
    kpi(cfr_txt, .tr("kpi_cfr", lang), "#EF7F1A"),
    "</tr></table>",

    "<div style='background:#F4F6F7;border-left:4px solid #0B4F3C;padding:12px 14px;margin:14px 0'>",
    "<b>", .tr("evolution_since", lang), sprintf("%03d", prev_sno), " :</b> ",
    variation_cases, ", ", variation_deaths, ".",
    "<div style='font-size:11px;color:#666;margin-top:5px'>",
    .tr("evolution_note", lang),
    "</div></div>",

    if (nzchar(top_html)) {
      paste0(
        "<div style='margin-top:18px'>",
        "<div style='font-size:16px;font-weight:700;margin-bottom:6px'>", .tr("zones_title", lang), "</div>",
        top_html,
        "</div>"
      )
    } else {
      ""
    },

    "<div style='background:#FFF8E8;border-left:4px solid #EF7F1A;padding:12px 14px;margin-top:18px'>",
    "<div style='font-weight:700;margin-bottom:4px'>", .tr("highlights_title", lang), "</div>",
    point_zone,
    point_evolution,
    "</div>",

    "<p style='font-size:12px;color:#666;margin-top:18px'>",
    sprintf(.tr("footer_text", lang), sprintf("%03d", sno)),
    "</p>",

    "</div>"
  )

  subject <- sprintf(
    .tr("subject_fmt", lang),
    sno, .fmt(cas, lang), .fmt(dec, lang), cfr_txt
  )

  smtp_user <- .env_get(c("SMTP_USER", "SMTP_USERNAME"))
  smtp_pass <- .env_get(c("SMTP_PASS", "SMTP_PASSWORD"))

  from <- .env_get(
    c("ALERT_FROM", "EMAIL_FROM", "SMTP_FROM", "MAIL_FROM"),
    smtp_user
  )

  to <- .env_get(
    c(
      "ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO", "PREIS_EMAIL_TO",
      "SMTP_TO", "MAIL_TO"
    ),
    from
  )

  if (!nzchar(smtp_user) || !nzchar(smtp_pass)) {
    cat("Identifiants SMTP manquants (SMTP_USER / SMTP_PASS).\n")
    return(invisible(FALSE))
  }

  if (!nzchar(from) || !nzchar(to)) {
    cat("Expediteur ou destinataires manquants.\n")
    return(invisible(FALSE))
  }

  to_vec <- unique(trimws(unlist(strsplit(to, "[,;]"))))
  to_vec <- to_vec[nzchar(to_vec)]

  if (length(to_vec) == 0) {
    cat("Liste des destinataires vide.\n")
    return(invisible(FALSE))
  }

  cfg_dir <- file.path(tempdir(), "preis_enrich")
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)

  cfg <- list(
    from = from,
    to = as.list(to_vec),
    subject = subject,
    html = html,
    plain = .tr("plain_fallback", lang),
    attachments = as.list(atts)
  )

  cfg_fp <- file.path(cfg_dir, "cfg.json")
  writeLines(
    jsonlite::toJSON(cfg, auto_unbox = TRUE),
    cfg_fp,
    useBytes = TRUE
  )

  py <- c(
    "import sys, json, os, ssl, smtplib",
    "from email.message import EmailMessage",
    "from pathlib import Path",
    "cfg = json.load(open(sys.argv[1], encoding='utf-8'))",
    "host = os.environ.get('SMTP_HOST') or 'smtp.gmail.com'",
    "port = int(os.environ.get('SMTP_PORT') or '465')",
    "user = os.environ.get('SMTP_USER') or os.environ.get('SMTP_USERNAME') or ''",
    "pw = os.environ.get('SMTP_PASS') or os.environ.get('SMTP_PASSWORD') or ''",
    "use_ssl = (str(os.environ.get('SMTP_SSL','')).lower() in ('1','true','yes')) or port == 465",
    "msg = EmailMessage()",
    "msg['From'] = cfg['from']",
    "msg['To'] = ', '.join(cfg['to'])",
    "msg['Subject'] = cfg['subject']",
    "msg.set_content(cfg.get('plain', 'See the attached report.'))",
    "msg.add_alternative(cfg['html'], subtype='html')",
    "for a in cfg.get('attachments', []):",
    "    p = Path(a)",
    "    if p.is_file():",
    "        ext = p.suffix.lower()",
    "        if ext == '.pdf': mt, st = 'application', 'pdf'",
    "        else: mt, st = 'application', 'octet-stream'",
    "        msg.add_attachment(p.read_bytes(), maintype=mt, subtype=st, filename=p.name)",
    "ctx = ssl.create_default_context()",
    "if use_ssl:",
    "    with smtplib.SMTP_SSL(host, port, timeout=120, context=ctx) as s:",
    "        s.login(user, pw)",
    "        s.send_message(msg)",
    "else:",
    "    with smtplib.SMTP(host, port, timeout=120) as s:",
    "        s.ehlo()",
    "        s.starttls(context=ctx)",
    "        s.ehlo()",
    "        s.login(user, pw)",
    "        s.send_message(msg)",
    "print('SENT_OK')"
  )

  py_fp <- file.path(cfg_dir, "send.py")
  writeLines(py, py_fp, useBytes = TRUE)

  py_bin <- Sys.which("python3")
  if (!nzchar(py_bin)) py_bin <- Sys.which("python")

  if (!nzchar(py_bin)) {
    cat("Python introuvable.\n")
    return(invisible(FALSE))
  }

  out <- tryCatch(
    system2(
      py_bin,
      c(shQuote(py_fp), shQuote(cfg_fp)),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) conditionMessage(e)
  )

  cat(paste(out, collapse = "\n"), "\n")

  if (!any(grepl("SENT_OK", out, fixed = TRUE))) {
    cat("Echec de l'envoi de l'e-mail enrichi.\n")
    return(invisible(FALSE))
  }

  new_row <- data.frame(
    sitrep_no = sno,
    sent_utc = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(state) || !all(names(new_row) %in% names(state))) {
    state2 <- new_row
  } else {
    state$sitrep_no <- suppressWarnings(as.integer(state$sitrep_no))
    state <- state[state$sitrep_no != sno | is.na(state$sitrep_no), names(new_row), drop = FALSE]
    state2 <- rbind(state, new_row)
  }

  state_written <- tryCatch(
    {
      readr::write_csv(state2, state_fp)
      TRUE
    },
    error = function(e) {
      cat("Avertissement : etat anti-doublon non enregistre :", conditionMessage(e), "\n")
      FALSE
    }
  )

  cat(
    "\nE-mail enrichi ENVOYE pour SitRep", sno,
    "->", paste(to_vec, collapse = ", "), "\n"
  )
  cat("Langue :", lang, "\n")
  cat("Piece jointe :", basename(pdf_fp), "\n")
  cat("Etat anti-doublon enregistre :", state_written, "\n")

  invisible(TRUE)
}

if (!interactive()) {
  ok <- preis_email_enrichi(
    force = isTRUE(as.logical(Sys.getenv("PREIS_FORCE_SEND", "FALSE")))
  )

  if (!isTRUE(ok)) {
    quit(save = "no", status = 2)
  }
}
