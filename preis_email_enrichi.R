############################################################
# preis_email_enrichi.R
#
# E-MAIL ENRICHI PREIS : apres detection d'un nouveau SitRep, envoie une
# synthese REELLE (chiffres cles + evolution + zones + points saillants),
# avec les GRAPHES + la CARTE + le PDF officiel joints, et le LIEN du
# dashboard.
#
#   - lit outputs/analyse/serie_temporelle_nationale.csv + tableau_zones_sante.csv
#   - calcule cumul national, CFR, nouveaux cas/deces vs SitRep precedent, top zones
#   - corps HTML soigne + lien dashboard
#   - joint : PDF du SitRep + g1..g4 + carte (ceux qui existent)
#   - SMTP via les memes secrets que l'existant (SMTP_*, ALERT_*)
#   - ANTI-DOUBLON SEPARE : data/preis_email_enrichi_state.csv
#   - envoi via 'emayili' (installe au besoin)
#
# Usage (console R, racine du projet) :
#   source("preis_email_enrichi.R")
#   preis_email_enrichi(force = TRUE)   # test : renvoie meme si deja notifie
#   preis_email_enrichi()               # prod : envoie seulement si nouveau
############################################################

DASHBOARD_URL <- "https://zrhyacinthe25.shinyapps.io/preis-ebola-drc-v2/"

.env_get <- function(names, default = "") {
  for (n in names) { v <- Sys.getenv(n, ""); if (nzchar(v)) return(v) }
  default
}

.fmt <- function(x) {
  if (is.na(x)) return("n/d")
  formatC(round(as.numeric(x)), format = "d", big.mark = " ")
}

preis_email_enrichi <- function(root = NULL, force = FALSE) {
  if (is.null(root)) {
    root <- if (file.exists(file.path(getwd(), "outputs", "analyse",
                                      "serie_temporelle_nationale.csv"))) getwd()
    else "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
  }
  setwd(root)
  suppressPackageStartupMessages(library(readr))
  
  serie_fp <- "outputs/analyse/serie_temporelle_nationale.csv"
  zones_fp <- "outputs/analyse/tableau_zones_sante.csv"
  if (!file.exists(serie_fp)) { cat("Serie introuvable:", serie_fp, "\n"); return(invisible(FALSE)) }
  
  serie <- readr::read_csv(serie_fp, show_col_types = FALSE)
  serie <- serie[order(serie$sitrep_no), , drop = FALSE]
  n <- nrow(serie); if (n == 0) { cat("Serie vide.\n"); return(invisible(FALSE)) }
  last <- serie[n, ]; prev <- if (n >= 2) serie[n - 1, ] else last
  sno  <- as.integer(last$sitrep_no)
  d_date <- as.character(last$date)
  cas  <- last$cas_cumules; dec <- last$deces_cumules
  cfr  <- if ("cfr" %in% names(last)) last$cfr else NA_real_
  nvx_cas <- suppressWarnings(as.numeric(last$cas_cumules) - as.numeric(prev$cas_cumules))
  nvx_dec <- suppressWarnings(as.numeric(last$deces_cumules) - as.numeric(prev$deces_cumules))
  
  # zones (top 5)
  top_html <- ""
  if (file.exists(zones_fp)) {
    z <- tryCatch(readr::read_csv(zones_fp, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(z) && all(c("nom", "cas") %in% names(z)) && nrow(z) > 0) {
      z <- z[order(-z$cas), , drop = FALSE]; z <- head(z, 5)
      tot <- sum(z$cas, na.rm = TRUE)
      rows <- paste0(
        "<tr><td style='padding:4px 10px;border-bottom:1px solid #eee'>", z$nom,
        "</td><td style='padding:4px 10px;border-bottom:1px solid #eee;text-align:right'><b>",
        vapply(z$cas, .fmt, character(1)), "</b></td></tr>", collapse = "")
      top_html <- paste0(
        "<table style='border-collapse:collapse;margin-top:6px'>",
        "<tr><th style='text-align:left;padding:4px 10px'>Zone de sante</th>",
        "<th style='text-align:right;padding:4px 10px'>Cas cumules</th></tr>",
        rows, "</table>")
    }
  }
  
  # anti-doublon
  state_fp <- "data/preis_email_enrichi_state.csv"
  dir.create("data", showWarnings = FALSE)
  state <- if (file.exists(state_fp))
    tryCatch(readr::read_csv(state_fp, show_col_types = FALSE), error = function(e) NULL) else NULL
  already <- !is.null(state) && "sitrep_no" %in% names(state) &&
    sno %in% suppressWarnings(as.integer(state$sitrep_no))
  if (already && !isTRUE(force)) {
    cat("SitRep", sno, "deja notifie (enrichi). Rien a faire. (force=TRUE pour renvoyer)\n")
    return(invisible(FALSE))
  }
  
  # pieces jointes existantes
  pdf_fp <- sprintf("data/pdf/PREIS_DRC_Ebola_SitRep_%03d.pdf", sno)
  atts <- c(
    if (file.exists(pdf_fp)) pdf_fp else NULL,
    Filter(file.exists, file.path("outputs/analyse", c(
      "g1_courbe_epidemique.png", "g2_courbe_mortalite.png",
      "g3_evolution_cfr.png", "g4_top_zones.png", "carte_zones_intensite.png"))))
  
  # corps HTML
  cfr_txt <- if (is.na(cfr)) sprintf("%.1f %%", 100 * as.numeric(dec) / as.numeric(cas)) else sprintf("%.1f %%", as.numeric(cfr))
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
    kpi(cfr_txt, "Letalite (CFR)", "#E67E22"),
    "</tr></table>",
    "<p><b>Evolution vs SitRep precedent (N&deg;", sprintf("%03d", as.integer(prev$sitrep_no)), ") :</b> ",
    if (!is.na(nvx_cas)) paste0("+", .fmt(nvx_cas), " nouveaux cas") else "nouveaux cas n/d",
    if (!is.na(nvx_dec)) paste0(", +", .fmt(nvx_dec), " deces") else "", ".</p>",
    if (nzchar(top_html)) paste0("<p><b>Zones les plus touchees :</b></p>", top_html) else "",
    "<p style='margin-top:16px'><b>Points saillants :</b> transmission active, l'Ituri reste l'epicentre. ",
    "Details epidemiologiques complets dans le PDF officiel joint.</p>",
    "<p style='margin:18px 0'><a href='", DASHBOARD_URL, "' ",
    "style='background:#1a7a3c;color:#fff;text-decoration:none;padding:12px 20px;border-radius:8px;font-weight:700'>",
    "Ouvrir le dashboard PREIS (mis a jour)</a></p>",
    "<p style='font-size:12px;color:#666'>Graphiques et carte joints : courbe epidemique, mortalite, evolution du CFR, ",
    "zones les plus touchees, carte d'intensite. Donnees provisoires (source INSP/INRB RDC).</p>",
    "</div>")
  subject <- sprintf("PREIS MVE RDC - SitRep N%03d : %s cas / %s deces (CFR %s)",
                     sno, .fmt(cas), .fmt(dec), cfr_txt)
  
  # SMTP (emayili -> Gmail : STARTTLS 587 fiable ; le 465/SSL du .Renviron sert au sender Python de la CI)
  host <- .env_get(c("SMTP_HOST", "EMAIL_SMTP_HOST", "PREIS_SMTP_HOST"), "smtp.gmail.com")
  port <- 587L
  user <- .env_get(c("SMTP_USERNAME", "SMTP_USER", "EMAIL_USER", "GMAIL_USER", "MAIL_USERNAME"))
  pass <- .env_get(c("SMTP_PASSWORD", "SMTP_PASS", "EMAIL_PASSWORD", "GMAIL_APP_PASSWORD", "MAIL_PASSWORD"))
  from <- .env_get(c("ALERT_FROM", "EMAIL_FROM", "SMTP_FROM", "MAIL_FROM"), user)
  # destinataire : ALERT_TO si defini, sinon on envoie a l'expediteur (soi-meme) -> pratique pour tester
  to   <- .env_get(c("ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO", "PREIS_EMAIL_TO", "SMTP_TO", "MAIL_TO"), from)
  if (!nzchar(user) || !nzchar(pass)) {
    cat("Identifiants SMTP manquants (SMTP_USER / SMTP_PASS).\n"); return(invisible(FALSE))
  }
  if (!nzchar(.env_get(c("ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO", "PREIS_EMAIL_TO", "SMTP_TO", "MAIL_TO"))))
    cat("Note: ALERT_TO non defini -> envoi a l'expediteur (", from, ") pour le test.\n", sep = "")
  to_vec <- trimws(unlist(strsplit(to, "[,;]")))
  
  if (!requireNamespace("emayili", quietly = TRUE)) {
    cat("Installation du paquet emayili...\n")
    install.packages("emayili", repos = "https://cloud.r-project.org")
  }
  
  msg <- emayili::envelope()
  msg <- emayili::from(msg, from)
  msg <- emayili::to(msg, to_vec)
  msg <- emayili::subject(msg, subject)
  msg <- emayili::html(msg, html)
  for (a in atts) msg <- emayili::attachment(msg, a)
  
  smtp <- emayili::server(host = host, port = port, username = user, password = pass)
  ok <- tryCatch({ smtp(msg, verbose = TRUE); TRUE },
                 error = function(e) { cat("Erreur envoi:", conditionMessage(e), "\n"); FALSE })
  if (!ok) return(invisible(FALSE))
  
  new_row <- data.frame(sitrep_no = sno,
                        sent_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
                        stringsAsFactors = FALSE)
  state2 <- if (is.null(state)) new_row else rbind(state[, names(new_row)], new_row)
  tryCatch(readr::write_csv(state2, state_fp), error = function(e) NULL)
  
  cat("\nE-mail enrichi ENVOYE pour SitRep", sno, "->", paste(to_vec, collapse = ", "), "\n")
  cat("Pieces jointes:", length(atts), "|", paste(basename(atts), collapse = ", "), "\n")
  invisible(TRUE)
}

# Execution directe : mode prod (anti-doublon actif). Pour tester : force=TRUE.
if (!interactive()) preis_email_enrichi(force = isTRUE(as.logical(Sys.getenv("PREIS_FORCE_SEND", "FALSE"))))