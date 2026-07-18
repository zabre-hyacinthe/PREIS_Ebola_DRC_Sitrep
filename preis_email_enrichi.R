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
# Securites :
#   - aucun lien dashboard ;
#   - aucun graphique ni carte joints ;
#   - validation du numero inscrit dans le PDF ;
#   - blocage si un PDF officiel plus recent que la serie existe ;
#   - anti-doublon : data/preis_email_enrichi_state.csv.
#
# Usage interactif :
#   source("preis_email_enrichi.R")
#   preis_email_enrichi(force = TRUE)
############################################################

.env_get <- function(names, default = "") {
  for (n in names) {
    v <- Sys.getenv(n, "")
    if (nzchar(v)) return(v)
  }
  default
}

.fmt <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) == 0 || is.na(x)) return("n/d")
  formatC(round(x), format = "d", big.mark = " ")
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

  txt <- paste(head(pages, max_pages), collapse = "\n")
  txt <- tolower(enc2utf8(txt))

  patterns <- c(
    "sitrep\\s*n\\s*[°ºo]?\\s*0*([0-9]{1,3})",
    "sitrep[^0-9]{0,20}0*([0-9]{1,3})"
  )

  nums <- integer()

  for (pattern in patterns) {
    m <- gregexpr(pattern, txt, perl = TRUE, ignore.case = TRUE)
    hits <- regmatches(txt, m)[[1]]

    if (length(hits) > 0 && !identical(hits, character(0))) {
      extracted <- suppressWarnings(
        as.integer(sub(pattern, "\\1", hits, perl = TRUE, ignore.case = TRUE))
      )
      nums <- c(nums, extracted)
    }
  }

  unique(nums[!is.na(nums)])
}

.latest_valid_higher_pdf <- function(pdf_dir, current_sitrep) {
  if (!dir.exists(pdf_dir)) return(NULL)

  files <- list.files(
    pdf_dir,
    pattern = "^PREIS_DRC_Ebola_SitRep_[0-9]{3}\\.pdf$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) return(NULL)

  file_numbers <- suppressWarnings(
    as.integer(sub(
      ".*_([0-9]{3})\\.pdf$",
      "\\1",
      basename(files),
      ignore.case = TRUE
    ))
  )

  keep <- !is.na(file_numbers) & file_numbers > current_sitrep
  files <- files[keep]
  file_numbers <- file_numbers[keep]

  if (length(files) == 0) return(NULL)

  ord <- order(file_numbers, decreasing = TRUE)
  files <- files[ord]
  file_numbers <- file_numbers[ord]

  for (i in seq_along(files)) {
    internal <- tryCatch(
      .pdf_internal_numbers(files[i]),
      error = function(e) integer()
    )

    if (file_numbers[i] %in% internal) {
      return(list(number = file_numbers[i], file = files[i]))
    }
  }

  NULL
}

preis_email_enrichi <- function(root = NULL, force = FALSE) {
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

  pdf_fp <- file.path(
    pdf_dir,
    sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", sno)
  )

  if (!file.exists(pdf_fp)) {
    cat("Envoi bloque : PDF officiel introuvable :", pdf_fp, "\n")
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
        vapply(z$cas, .fmt, character(1)),
        "</b></td></tr>",
        collapse = ""
      )

      top_html <- paste0(
        "<table role='presentation' style='border-collapse:collapse;margin-top:6px;min-width:360px'>",
        "<tr><th style='text-align:left;padding:6px 10px;background:#f3f5f4'>Zone de sant&eacute;</th>",
        "<th style='text-align:right;padding:6px 10px;background:#f3f5f4'>Cas cumul&eacute;s</th></tr>",
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
      "<div style='font-size:12px;opacity:.95'>", lab, "</div></td>"
    )
  }

  variation_cases <- if (!is.na(nvx_cas)) {
    paste0(
      if (nvx_cas >= 0) "+" else "",
      .fmt(nvx_cas),
      " cas dans le cumul national"
    )
  } else {
    "variation des cas : n/d"
  }

  variation_deaths <- if (!is.na(nvx_dec)) {
    paste0(
      if (nvx_dec >= 0) "+" else "",
      .fmt(nvx_dec),
      " d&eacute;c&egrave;s dans le cumul national"
    )
  } else {
    "variation des d&eacute;c&egrave;s : n/d"
  }

  point_zone <- if (!is.null(z) && nrow(z) > 0) {
    paste0(
      "Parmi les zones renseign&eacute;es, <b>",
      .html_escape(z$nom[1]),
      "</b> pr&eacute;sente le cumul le plus &eacute;lev&eacute;, avec <b>",
      .fmt(z$cas[1]),
      " cas</b>. "
    )
  } else {
    paste0(
      "La r&eacute;partition par zone de sant&eacute; n'est pas disponible ",
      "dans le fichier analys&eacute;. "
    )
  }

  point_evolution <- if (!is.na(nvx_cas) && !is.na(nvx_dec)) {
    paste0(
      "Depuis le SitRep N&deg;", sprintf("%03d", prev_sno),
      ", le cumul national a vari&eacute; de <b>",
      if (nvx_cas >= 0) "+" else "",
      .fmt(nvx_cas),
      " cas</b> et de <b>",
      if (nvx_dec >= 0) "+" else "",
      .fmt(nvx_dec),
      " d&eacute;c&egrave;s</b>."
    )
  } else {
    "La variation depuis le SitRep pr&eacute;c&eacute;dent n'est pas disponible."
  }

  html <- paste0(
    "<div style='font-family:Segoe UI,Arial,sans-serif;color:#252525;max-width:700px;line-height:1.45'>",

    "<div style='background:#0B4F3C;color:#fff;padding:18px 22px;border-radius:10px'>",
    "<div style='font-size:19px;font-weight:700'>PREIS &mdash; MVE Ebola RDC</div>",
    "<div style='font-size:14px;margin-top:3px'>Synth&egrave;se automatique &mdash; SitRep N&deg;",
    sprintf("%03d", sno),
    if (nzchar(d_date) && d_date != "NA") paste0(" (", .html_escape(d_date), ")") else "",
    "</div></div>",

    "<table role='presentation' style='border-spacing:9px 0;margin:16px 0;width:100%'><tr>",
    kpi(.fmt(cas), "Cas confirm&eacute;s cumul&eacute;s", "#C83A2D"),
    kpi(.fmt(dec), "D&eacute;c&egrave;s cumul&eacute;s", "#33485D"),
    kpi(cfr_txt, "L&eacute;talit&eacute; (CFR)", "#EF7F1A"),
    "</tr></table>",

    "<div style='background:#F4F6F7;border-left:4px solid #0B4F3C;padding:12px 14px;margin:14px 0'>",
    "<b>&Eacute;volution depuis le SitRep N&deg;", sprintf("%03d", prev_sno), " :</b> ",
    variation_cases, ", ", variation_deaths, ".",
    "<div style='font-size:11px;color:#666;margin-top:5px'>",
    "La variation du cumul peut inclure les nouvelles notifications et des consolidations r&eacute;trospectives.",
    "</div></div>",

    if (nzchar(top_html)) {
      paste0(
        "<div style='margin-top:18px'>",
        "<div style='font-size:16px;font-weight:700;margin-bottom:6px'>Zones les plus touch&eacute;es</div>",
        top_html,
        "</div>"
      )
    } else {
      ""
    },

    "<div style='background:#FFF8E8;border-left:4px solid #EF7F1A;padding:12px 14px;margin-top:18px'>",
    "<div style='font-weight:700;margin-bottom:4px'>Points saillants</div>",
    point_zone,
    point_evolution,
    "</div>",

    "<p style='font-size:12px;color:#666;margin-top:18px'>",
    "Le rapport officiel complet, valid&eacute; comme SitRep N&deg;", sprintf("%03d", sno),
    ", est joint au format PDF. Donn&eacute;es provisoires, source INSP/INRB RDC.",
    "</p>",

    "</div>"
  )

  subject <- sprintf(
    "PREIS MVE RDC - SitRep N%03d : %s cas / %s deces (CFR %s)",
    sno, .fmt(cas), .fmt(dec), cfr_txt
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
    "msg.set_content('Consultez la synthese PREIS et le rapport officiel joint.')",
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
