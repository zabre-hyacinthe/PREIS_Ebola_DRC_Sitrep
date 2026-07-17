############################################################
# preis_email_enrichi.R  (v2 - moteur Python smtplib, comme l'e-mail nu qui marche)
#
# Apres detection d'un nouveau SitRep : envoie une synthese REELLE
# (chiffres cles + evolution + zones + points saillants), avec GRAPHES +
# CARTE + PDF officiel joints, et le LIEN du dashboard.
#
#   - lit outputs/analyse/serie_temporelle_nationale.csv + tableau_zones_sante.csv
#   - corps HTML soigne + lien dashboard
#   - ENVOI via Python smtplib (SSL 465 par defaut, comme le sender existant)
#     -> pas de dependance R fragile (emayili) ; memes secrets SMTP_*/ALERT_*
#   - ANTI-DOUBLON : data/preis_email_enrichi_state.csv
#
# Usage (console R) :
#   source("preis_email_enrichi.R"); preis_email_enrichi(force = TRUE)
############################################################

DASHBOARD_URL <- "https://zrhyacinthe25.shinyapps.io/preis-ebola-drc-v2/"

.env_get <- function(names, default = "") {
  for (n in names) { v <- Sys.getenv(n, ""); if (nzchar(v)) return(v) }
  default
}
.fmt <- function(x) {
  if (length(x) == 0 || is.na(x)) return("n/d")
  formatC(round(as.numeric(x)), format = "d", big.mark = " ")
}

preis_email_enrichi <- function(root = NULL, force = FALSE) {
  if (is.null(root)) {
    root <- if (file.exists(file.path(getwd(), "outputs", "analyse",
                                      "serie_temporelle_nationale.csv"))) getwd()
    else "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
  }
  if (dir.exists(root)) setwd(root)
  suppressPackageStartupMessages({ library(readr); library(jsonlite) })
  
  serie_fp <- "outputs/analyse/serie_temporelle_nationale.csv"
  zones_fp <- "outputs/analyse/tableau_zones_sante.csv"
  if (!file.exists(serie_fp)) { cat("Serie introuvable:", serie_fp, "\n"); return(invisible(FALSE)) }
  
  serie <- readr::read_csv(serie_fp, show_col_types = FALSE)
  serie <- serie[order(serie$sitrep_no), , drop = FALSE]
  n <- nrow(serie); if (n == 0) { cat("Serie vide.\n"); return(invisible(FALSE)) }
  last <- serie[n, ]; prev <- if (n >= 2) serie[n - 1, ] else last
  sno  <- as.integer(last$sitrep_no)
  d_date <- as.character(last$date)
  cas <- last$cas_cumules; dec <- last$deces_cumules
  cfr <- if ("cfr" %in% names(last)) last$cfr else NA_real_
  nvx_cas <- suppressWarnings(as.numeric(last$cas_cumules) - as.numeric(prev$cas_cumules))
  nvx_dec <- suppressWarnings(as.numeric(last$deces_cumules) - as.numeric(prev$deces_cumules))
  
  top_html <- ""
  if (file.exists(zones_fp)) {
    z <- tryCatch(readr::read_csv(zones_fp, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(z) && all(c("nom", "cas") %in% names(z)) && nrow(z) > 0) {
      z <- z[order(-z$cas), , drop = FALSE]; z <- head(z, 5)
      rows <- paste0(
        "<tr><td style='padding:4px 10px;border-bottom:1px solid #eee'>", z$nom,
        "</td><td style='padding:4px 10px;border-bottom:1px solid #eee;text-align:right'><b>",
        vapply(z$cas, .fmt, character(1)), "</b></td></tr>", collapse = "")
      top_html <- paste0(
        "<table style='border-collapse:collapse;margin-top:6px'>",
        "<tr><th style='text-align:left;padding:4px 10px'>Zone de sante</th>",
        "<th style='text-align:right;padding:4px 10px'>Cas cumules</th></tr>", rows, "</table>")
    }
  }
  
  state_fp <- "data/preis_email_enrichi_state.csv"
  dir.create("data", showWarnings = FALSE)
  state <- if (file.exists(state_fp))
    tryCatch(readr::read_csv(state_fp, show_col_types = FALSE), error = function(e) NULL) else NULL
  already <- !is.null(state) && "sitrep_no" %in% names(state) &&
    sno %in% suppressWarnings(as.integer(state$sitrep_no))
  if (already && !isTRUE(force)) {
    cat("SitRep", sno, "deja notifie (enrichi). (force=TRUE pour renvoyer)\n"); return(invisible(FALSE))
  }
  
  pdf_fp <- sprintf("data/pdf/PREIS_DRC_Ebola_SitRep_%03d.pdf", sno)
  atts <- c(
    if (file.exists(pdf_fp)) pdf_fp else NULL,
    Filter(file.exists, file.path("outputs/analyse", c(
      "g1_courbe_epidemique.png", "g2_courbe_mortalite.png",
      "g3_evolution_cfr.png", "g4_top_zones.png", "carte_zones_intensite.png"))))
  
  cfr_txt <- if (length(cfr) == 0 || is.na(cfr)) sprintf("%.1f %%", 100 * as.numeric(dec) / as.numeric(cas)) else sprintf("%.1f %%", as.numeric(cfr))
  kpi <- function(val, lab, col) paste0(
    "<td style='padding:14px 18px;background:", col, ";color:#fff;border-radius:10px'>",
    "<div style='font-size:26px;font-weight:700'>", val, "</div>",
    "<div style='font-size:12px;opacity:.9'>", lab, "</div></td>")
  html <- paste0(
    "<div style='font-family:Segoe UI,Arial,sans-serif;color:#222;max-width:680px'>",
    "<div style='background:#1a7a3c;color:#fff;padding:16px 20px;border-radius:10px'>",
    "<div style='font-size:18px;font-weight:700'>PREIS &mdash; MVE Ebola RDC</div>",
    "<div>Synthese automatique &mdash; SitRep N&deg;", sprintf("%03d", sno),
    if (nzchar(d_date) && d_date != "NA") paste0(" (", d_date, ")") else "", "</div></div>",
    "<table style='border-spacing:8px 0;margin:14px 0'><tr>",
    kpi(.fmt(cas), "Cas confirmes cumules", "#C0392B"),
    kpi(.fmt(dec), "Deces cumules", "#2C3E50"),
    kpi(cfr_txt, "Letalite (CFR)", "#E67E22"), "</tr></table>",
    "<p><b>Evolution vs SitRep precedent (N&deg;", sprintf("%03d", as.integer(prev$sitrep_no)), ") :</b> ",
    if (!is.na(nvx_cas)) paste0("+", .fmt(nvx_cas), " nouveaux cas") else "nouveaux cas n/d",
    if (!is.na(nvx_dec)) paste0(", +", .fmt(nvx_dec), " deces") else "", ".</p>",
    if (nzchar(top_html)) paste0("<p><b>Zones les plus touchees :</b></p>", top_html) else "",
    "<p style='margin-top:16px'><b>Points saillants :</b> transmission active, l'Ituri reste l'epicentre. ",
    "Details epidemiologiques complets dans le PDF officiel joint.</p>",
    "<p style='margin:18px 0'><a href='", DASHBOARD_URL,
    "' style='background:#1a7a3c;color:#fff;text-decoration:none;padding:12px 20px;border-radius:8px;font-weight:700'>",
    "Ouvrir le dashboard PREIS (mis a jour)</a></p>",
    "<p style='font-size:12px;color:#666'>Graphiques et carte joints : courbe epidemique, mortalite, evolution du CFR, ",
    "zones les plus touchees, carte d'intensite. Donnees provisoires (source INSP/INRB RDC).</p></div>")
  subject <- sprintf("PREIS MVE RDC - SitRep N%03d : %s cas / %s deces (CFR %s)",
                     sno, .fmt(cas), .fmt(dec), cfr_txt)
  
  from <- .env_get(c("ALERT_FROM", "EMAIL_FROM", "SMTP_FROM", "MAIL_FROM"),
                   .env_get(c("SMTP_USER", "SMTP_USERNAME")))
  to   <- .env_get(c("ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO", "PREIS_EMAIL_TO", "SMTP_TO", "MAIL_TO"), from)
  if (!nzchar(.env_get(c("SMTP_USER", "SMTP_USERNAME"))) || !nzchar(.env_get(c("SMTP_PASS", "SMTP_PASSWORD")))) {
    cat("Identifiants SMTP manquants (SMTP_USER / SMTP_PASS).\n"); return(invisible(FALSE))
  }
  to_vec <- trimws(unlist(strsplit(to, "[,;]"))); to_vec <- to_vec[nzchar(to_vec)]
  
  # --- config + sender Python (smtplib SSL 465, comme l'e-mail nu) ---------
  cfg_dir <- file.path(tempdir(), "preis_enrich"); dir.create(cfg_dir, showWarnings = FALSE)
  cfg <- list(from = from, to = as.list(to_vec), subject = subject, html = html,
              attachments = as.list(atts))
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE), file.path(cfg_dir, "cfg.json"), useBytes = TRUE)
  
  py <- c(
    "import sys, json, os, ssl, smtplib",
    "from email.message import EmailMessage",
    "from pathlib import Path",
    "cfg = json.load(open(sys.argv[1], encoding='utf-8'))",
    "host = os.environ.get('SMTP_HOST') or 'smtp.gmail.com'",
    "port = int(os.environ.get('SMTP_PORT') or '465')",
    "user = os.environ.get('SMTP_USER') or os.environ.get('SMTP_USERNAME') or ''",
    "pw   = os.environ.get('SMTP_PASS') or os.environ.get('SMTP_PASSWORD') or ''",
    "use_ssl = (str(os.environ.get('SMTP_SSL','')).lower() in ('1','true','yes')) or port == 465",
    "msg = EmailMessage()",
    "msg['From'] = cfg['from']; msg['To'] = ', '.join(cfg['to']); msg['Subject'] = cfg['subject']",
    "msg.set_content('Ouvrez cet e-mail en HTML, ou le dashboard PREIS.')",
    "msg.add_alternative(cfg['html'], subtype='html')",
    "for a in cfg.get('attachments', []):",
    "    p = Path(a)",
    "    if p.is_file():",
    "        ext = p.suffix.lower()",
    "        if ext == '.pdf': mt, st = 'application', 'pdf'",
    "        elif ext == '.png': mt, st = 'image', 'png'",
    "        else: mt, st = 'application', 'octet-stream'",
    "        msg.add_attachment(p.read_bytes(), maintype=mt, subtype=st, filename=p.name)",
    "ctx = ssl.create_default_context()",
    "if use_ssl:",
    "    with smtplib.SMTP_SSL(host, port, timeout=120, context=ctx) as s:",
    "        s.login(user, pw); s.send_message(msg)",
    "else:",
    "    with smtplib.SMTP(host, port, timeout=120) as s:",
    "        s.ehlo(); s.starttls(context=ctx); s.login(user, pw); s.send_message(msg)",
    "print('SENT_OK')")
  py_fp <- file.path(cfg_dir, "send.py")
  writeLines(py, py_fp, useBytes = TRUE)
  
  py_bin <- Sys.which("python3"); if (!nzchar(py_bin)) py_bin <- Sys.which("python")
  if (!nzchar(py_bin)) { cat("python introuvable (installe Python, ou lance en CI).\n"); return(invisible(FALSE)) }
  
  out <- tryCatch(system2(py_bin, c(shQuote(py_fp), shQuote(file.path(cfg_dir, "cfg.json"))),
                          stdout = TRUE, stderr = TRUE), error = function(e) conditionMessage(e))
  cat(paste(out, collapse = "\n"), "\n")
  if (!any(grepl("SENT_OK", out))) { cat("Echec envoi e-mail enrichi.\n"); return(invisible(FALSE)) }
  
  new_row <- data.frame(sitrep_no = sno,
                        sent_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
                        stringsAsFactors = FALSE)
  state2 <- if (is.null(state)) new_row else rbind(state[, names(new_row)], new_row)
  tryCatch(readr::write_csv(state2, state_fp), error = function(e) NULL)
  
  cat("\nE-mail enrichi ENVOYE pour SitRep", sno, "->", paste(to_vec, collapse = ", "), "\n")
  cat("Pieces jointes:", length(atts), "|", paste(basename(atts), collapse = ", "), "\n")
  invisible(TRUE)
}

if (!interactive())
  preis_email_enrichi(force = isTRUE(as.logical(Sys.getenv("PREIS_FORCE_SEND", "FALSE"))))