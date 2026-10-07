############################################################
# 05b_send_africacdc_sitrep_brief_full_email.R
#
# Trois fonctions, dans cet ordre, toutes non bloquantes pour le reste
# du pipeline (continue-on-error cote workflow) :
#
#  1. ALERTE OPERATEUR : si 06b a echoue de facon durable pour un SitRep
#     (PREIS_AFRICACDC_MAX_ATTEMPTS echecs, ou probleme de configuration),
#     envoie UN e-mail en francais a ALERT_TO avec la cause et la marche a
#     suivre. Un seul e-mail par SitRep (trace : alert_sent_utc dans
#     data/africa_cdc_brief/attempts.csv).
#
#  2. MODE TEST (PREIS_AFRICACDC_MODE = test) : envoie a ALERT_TO UNIQUEMENT
#     le resultat du test de 06b (les 2 documents, ou la cause de l'echec).
#     Rien n'est enregistre, aucun destinataire Africa CDC n'est contacte.
#
#  3. ENVOI NORMAL : envoie les 2 documents (SitRep + Executive Brief,
#     versions finales, sans suivi des modifications) de la derniere Issue generee,
#     une seule fois par Issue (anti-doublon :
#     data/africa_cdc_brief/email_sent_state.csv). Les documents sont lus
#     dans outputs/rapports/ (commites par le workflow) : un echec SMTP est
#     donc rejoue automatiquement au cycle suivant, sans regenerer.
#
# Canal independant de 05_send_africacdc_sitrep_email.R (supplement
# quantitatif) : aucun des canaux existants n'est modifie.
#
# Usage interactif :
#   source("scripts/05b_send_africacdc_sitrep_brief_full_email.R")
#   send_africacdc_sitrep_brief_full_email(force = TRUE)
############################################################

# --- Signature commune des e-mails (scripts/preis_signature.R ; repli integre si le fichier est absent) ---
for (.sig_fp in c("scripts/preis_signature.R", file.path(Sys.getenv("GITHUB_WORKSPACE", "."), "scripts", "preis_signature.R"),
                  "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26/scripts/preis_signature.R")) {
  if (file.exists(.sig_fp)) { try(source(.sig_fp, encoding = "UTF-8"), silent = TRUE); break }
}
if (!exists("preis_signature_html", mode = "function")) {
  preis_signature_text <- function() "\n\n-- \nDr R. Hyacinthe ZABRE\nEpidemio-Biostat, PREIS developer\nEmail: zrhyacinthe@gmail.com\nWhatsApp: +22678088770\n"
  preis_signature_html <- function() paste0("<div style='margin-top:22px;padding-top:10px;border-top:1px solid #d0d0d0;font-size:13px;line-height:1.5'>",
    "<b>Dr R. Hyacinthe ZABRE</b><br>Epidemio-Biostat, PREIS developer<br>Email: zrhyacinthe@gmail.com<br>WhatsApp: +22678088770</div>")
}

.awb_env_get <- function(names, default = "") {
  for (n in names) {
    v <- Sys.getenv(n, "")
    if (nzchar(v)) return(v)
  }
  default
}

.awb_hint <- function(category) {
  if (is.null(category) || length(category) == 0 || is.na(category[1])) category <- "x"
  switch(as.character(category[1]),
    auth       = "Cle API refusee (invalide, revoquee ou sans droits). Creer une nouvelle cle sur console.anthropic.com > API keys, puis remplacer le secret GitHub ANTHROPIC_API_KEY.",
    request    = "Requete refusee par l'API. Cause la plus frequente : credit API insuffisant. Verifier console.anthropic.com > Settings > Billing (le credit API est distinct de l'abonnement Claude.ai).",
    config     = "Configuration incomplete (secret ANTHROPIC_API_KEY, gabarits ou scripts manquants). Corriger puis relancer avec africa_cdc_mode = retry.",
    input      = "Le PDF du SitRep est illisible ou trop volumineux. Verifier data/pdf/ ; si le PDF est corrompu, relancer le telechargement du SitRep.",
    model      = "Aucun modele reconnu. Definir la variable ANTHROPIC_MODEL avec un identifiant de modele valide (voir docs.claude.com > Models).",
    truncated  = "La reponse du modele a ete tronquee : relancer (retry). Si cela se repete, signaler le cas.",
    overloaded = "API temporairement saturee ou limitee en debit : relancer plus tard (retry).",
    network    = "Probleme reseau transitoire avec l'API : relancer plus tard (retry).",
    timeout    = "Delai depasse : relancer plus tard (retry).",
    output     = "La reponse du modele n'a pas passe les controles qualite (JSON, index, reponse trop pauvre ou numero d'Issue). Aucun document n'a ete envoye. La reponse brute du modele est jointe a ce mail (last_response.txt) pour diagnostic. Relancer (retry) ; si cela se repete, transmettre ce fichier.",
    build      = "Construction ou verification des .docx echouee (gabarit incompatible ?). Consulter le journal du step 06b dans l'onglet Actions.",
    template   = "Lecture des gabarits impossible. Verifier data/africa_cdc_brief/templates/.",
    "Cause inattendue : consulter le journal du step 06b dans l'onglet Actions.")
}

.awb_send_mail <- function(py_bin, cfg) {
  cfg_dir <- file.path(tempdir(), paste0("preis_africacdc_brief_mail_", format(Sys.time(), "%H%M%S"), sample.int(9999, 1)))
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
  cfg_fp <- file.path(cfg_dir, "cfg.json")
  writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE), cfg_fp, useBytes = TRUE)
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
    "msg.set_content(cfg.get('plain', 'See attached documents.'))",
    "msg.add_alternative(cfg['html'], subtype='html')",
    "for a in cfg.get('attachments', []):",
    "    p = Path(a)",
    "    if p.is_file():",
    "        if p.suffix.lower() == '.docx':",
    "            mt, st = 'application', 'vnd.openxmlformats-officedocument.wordprocessingml.document'",
    "        else:",
    "            mt, st = 'application', 'octet-stream'",
    "        msg.add_attachment(p.read_bytes(), maintype=mt, subtype=st, filename=p.name)",
    "ctx = ssl.create_default_context()",
    "if use_ssl:",
    "    with smtplib.SMTP_SSL(host, port, timeout=120, context=ctx) as s:",
    "        s.login(user, pw)",
    "        s.send_message(msg)",
    "else:",
    "    with smtplib.SMTP(host, port, timeout=120) as s:",
    "        s.ehlo(); s.starttls(context=ctx); s.ehlo()",
    "        s.login(user, pw)",
    "        s.send_message(msg)",
    "print('SENT_OK')"
  )
  py_fp <- file.path(cfg_dir, "send.py")
  writeLines(py, py_fp, useBytes = TRUE)
  out <- tryCatch(system2(py_bin, c(shQuote(py_fp), shQuote(cfg_fp)), stdout = TRUE, stderr = TRUE),
                  error = function(e) conditionMessage(e))
  cat(paste(out, collapse = "\n"), "\n")
  any(grepl("SENT_OK", out, fixed = TRUE))
}

.awb_split <- function(x) {
  v <- unique(trimws(unlist(strsplit(x, "[,;]")))); v[nzchar(v)]
}

.awb_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}

send_africacdc_sitrep_brief_full_email <- function(root = NULL, force = FALSE) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  if (is.null(root)) {
    root <- if (file.exists(file.path(getwd(), "data", "africa_cdc_brief", "state.csv"))) getwd()
            else Sys.getenv("PREIS_BASE_DIR", "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
  }
  if (!dir.exists(root)) { cat("Dossier racine introuvable :", root, "\n"); return(invisible(FALSE)) }
  setwd(root)
  suppressPackageStartupMessages({ library(readr); library(jsonlite) })

  mode <- tolower(trimws(Sys.getenv("PREIS_AFRICACDC_MODE", "normal")))
  max_attempts <- suppressWarnings(as.integer(Sys.getenv("PREIS_AFRICACDC_MAX_ATTEMPTS", "3")))
  if (is.na(max_attempts) || max_attempts < 1) max_attempts <- 3L

  state_fp <- "data/africa_cdc_brief/state.csv"
  attempts_fp <- "data/africa_cdc_brief/attempts.csv"
  test_fp <- "outputs/rapports/africa_cdc_test/test_run.json"
  rapports_dir <- "outputs/rapports"
  sent_state_fp <- "data/africa_cdc_brief/email_sent_state.csv"

  # ---- Parametres SMTP (memes identifiants que les autres e-mails PREIS) ----
  smtp_user <- .awb_env_get(c("SMTP_USER", "SMTP_USERNAME"))
  smtp_pass <- .awb_env_get(c("SMTP_PASS", "SMTP_PASSWORD"))
  from <- .awb_env_get(c("ALERT_FROM", "EMAIL_FROM", "SMTP_FROM", "MAIL_FROM"), smtp_user)
  ops_to <- .awb_split(.awb_env_get(c("ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO", "PREIS_EMAIL_TO", "SMTP_TO", "MAIL_TO"), from))
  pub_to <- .awb_split(.awb_env_get(c("PREIS_AFRICACDC_TO", "ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO", "PREIS_EMAIL_TO", "SMTP_TO", "MAIL_TO"), from))
  if (!nzchar(smtp_user) || !nzchar(smtp_pass)) { cat("Identifiants SMTP manquants (SMTP_USER / SMTP_PASS).\n"); return(invisible(FALSE)) }
  if (!nzchar(from)) { cat("Expediteur manquant (ALERT_FROM).\n"); return(invisible(FALSE)) }
  py_bin <- Sys.which("python3"); if (!nzchar(py_bin)) py_bin <- Sys.which("python")
  if (!nzchar(py_bin)) { cat("Python introuvable.\n"); return(invisible(FALSE)) }

  all_ok <- TRUE
  wrap <- function(inner) paste0(
    "<div style='font-family:Segoe UI,Arial,sans-serif;color:#252525;max-width:640px;line-height:1.5'>", inner, preis_signature_html(), "</div>")

  # =====================================================================
  # 1. ALERTE OPERATEUR (modes normal / retry)
  # =====================================================================
  if (mode != "test" && file.exists(attempts_fp) && length(ops_to) > 0) {
    a <- tryCatch(readr::read_csv(attempts_fp, show_col_types = FALSE,
                                  col_types = readr::cols(.default = readr::col_character())), error = function(e) NULL)
    if (!is.null(a) && nrow(a) > 0 && all(c("sitrep_no", "attempts", "last_category", "last_error", "alert_sent_utc") %in% names(a))) {
      a[is.na(a)] <- ""
      done <- if (file.exists(state_fp)) {
        st <- tryCatch(readr::read_csv(state_fp, show_col_types = FALSE), error = function(e) NULL)
        if (!is.null(st) && "sitrep_no_source" %in% names(st)) suppressWarnings(max(as.integer(st$sitrep_no_source), na.rm = TRUE)) else -Inf
      } else -Inf
      changed <- FALSE
      for (i in seq_len(nrow(a))) {
        sno <- suppressWarnings(as.integer(a$sitrep_no[i])); n <- suppressWarnings(as.integer(a$attempts[i]))
        if (is.na(sno) || is.na(n) || nzchar(a$alert_sent_utc[i])) next
        if (is.finite(done) && sno <= done) next            # deja traite entre-temps
        needs <- identical(a$last_category[i], "config") || n >= max_attempts
        if (!needs) next
        cause <- a$last_error[i]
        html <- wrap(paste0(
          "<div style='background:#9F2241;color:#fff;padding:14px 18px;border-radius:8px'>",
          "<div style='font-size:16px;font-weight:700'>PREIS &mdash; generation Africa CDC SitRep + Brief en echec</div>",
          "<div style='font-size:13px;margin-top:3px'>SitRep RDC No. ", sno, "</div></div>",
          "<p style='margin-top:14px'>Le SitRep RDC No. <strong>", sno, "</strong> a ete detecte, mais le SitRep et le Brief ",
          "Africa CDC complets <strong>n'ont pas pu etre generes</strong>", if (n > 0) paste0(" (", n, " tentative(s))") else "", ". ",
          "<strong>Aucun document partiel n'a ete envoye</strong> ; le supplement quantitatif habituel n'est pas affecte.</p>",
          "<p><strong>Cause :</strong> ", .awb_esc(cause), "<br><strong>Que faire :</strong> ", .awb_esc(.awb_hint(a$last_category[i])), "</p>",
          "<p style='font-size:12px;color:#666'>Apres correction : GitHub &gt; onglet Actions &gt; <em>PREIS Ebola DRC SitRep Monitor V2</em> ",
          "&gt; Run workflow &gt; <em>africa_cdc_mode</em> = <strong>retry</strong>. Detail technique : journal du step ",
          "&laquo;&nbsp;Generer SitRep+Brief Africa CDC complet&nbsp;&raquo;.</p>"))
        plain <- sprintf("PREIS - generation Africa CDC SitRep+Brief en echec (SitRep RDC No. %d). Cause : %s. Que faire : %s",
                         sno, cause, .awb_hint(a$last_category[i]))
        ok <- .awb_send_mail(py_bin, list(from = from, to = as.list(ops_to),
              subject = sprintf("PREIS ALERTE - generation Africa CDC SitRep+Brief en echec (SitRep RDC No.%d)", sno),
              html = html, plain = plain, attachments = as.list(Sys.glob("outputs/rapports/africa_cdc_test/last_response.txt"))))
        if (ok) { a$alert_sent_utc[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"); changed <- TRUE
                  cat("Alerte operateur envoyee pour le SitRep", sno, "\n")
        } else { all_ok <- FALSE; cat("Echec d'envoi de l'alerte operateur pour le SitRep", sno, "\n") }
      }
      if (changed) tryCatch(readr::write_csv(a, attempts_fp, na = ""), error = function(e) cat("Avertissement : attempts.csv non mis a jour.\n"))
    }
  }

  # =====================================================================
  # 2. MODE TEST : resultat envoye a l'operateur seulement
  # =====================================================================
  if (mode == "test") {
    if (length(ops_to) == 0) { cat("Aucun destinataire ALERT_TO pour le test.\n"); return(invisible(FALSE)) }
    tr <- if (file.exists(test_fp)) tryCatch(jsonlite::fromJSON(test_fp, simplifyVector = FALSE), error = function(e) NULL) else NULL
    if (is.null(tr)) {
      subject <- "[TEST] PREIS Africa CDC SitRep + Brief - aucun resultat de test"
      html <- wrap("<p>Le test n'a produit aucun resultat. Consulter le journal du workflow (onglet Actions).</p>")
      files <- character(0); plain <- "Le test n'a produit aucun resultat. Consulter le journal du workflow."
    } else if (identical(tr$status, "ok")) {
      files <- unlist(tr$files); files <- files[file.exists(files)]
      subject <- sprintf("[TEST] PREIS Africa CDC SitRep + Executive Brief - chaine de generation VERIFIEE (SitRep RDC No.%03d)", as.integer(tr$sitrep_no))
      warn <- unlist(tr$warnings)
      html <- wrap(paste0(
        "<div style='background:#2E7D32;color:#fff;padding:14px 18px;border-radius:8px'><div style='font-size:16px;font-weight:700'>",
        "TEST reussi &mdash; toute la chaine fonctionne</div></div>",
        "<p style='margin-top:14px'>Cle API valide, PDF lu, reponse du modele validee, 2 documents construits et verifies. ",
        "Ils sont joints pour que tu juges la qualite. <strong>Rien n'a ete enregistre</strong> (etat et gabarits intacts) et ",
        "<strong>aucun destinataire Africa CDC n'a ete contacte</strong>.</p>",
        "<p>Source : SitRep RDC No. ", tr$sitrep_no, " (le meme que celui deja utilise pour le gabarit : le test ne montre donc ",
        "que peu de differences, ce qui est normal ; il valide la plomberie, pas l'evolution des chiffres). ",
        "Anomalies signalees : ", tr$anomalies_count, ".</p>",
        if (length(unlist(tr$anomalies)) > 0) paste0("<ul style='font-size:12px'>", paste0("<li>", vapply(unlist(tr$anomalies), .awb_esc, character(1)), "</li>", collapse = ""), "</ul>") else "",
        if (length(warn) > 0) paste0("<p><strong>Avertissements :</strong> ", .awb_esc(paste(warn, collapse = " ; ")), "</p>") else "",
        if (!is.null(tr$summary_fr) && nzchar(tr$summary_fr)) paste0("<p style='font-size:12px;color:#666'>Resume du modele : ", .awb_esc(tr$summary_fr), "</p>") else ""))
      plain <- "TEST reussi : toute la chaine fonctionne. Documents joints ; rien n'a ete enregistre."
    } else {
      files <- Sys.glob("outputs/rapports/africa_cdc_test/last_response.txt")   # reponse brute du modele, pour diagnostic
      subject <- "[TEST] PREIS Africa CDC SitRep + Brief - ECHEC du test"
      html <- wrap(paste0(
        "<div style='background:#9F2241;color:#fff;padding:14px 18px;border-radius:8px'><div style='font-size:16px;font-weight:700'>",
        "TEST en echec</div></div>",
        "<p style='margin-top:14px'><strong>Cause :</strong> ", .awb_esc(tr$message), "<br><strong>Que faire :</strong> ",
        .awb_esc(.awb_hint(tr$category)), "</p>",
        "<p style='font-size:12px;color:#666'>Rien n'a ete enregistre ni envoye aux destinataires Africa CDC.</p>"))
      plain <- sprintf("TEST en echec. Cause : %s. Que faire : %s", tr$message, .awb_hint(tr$category))
    }
    ok <- .awb_send_mail(py_bin, list(from = from, to = as.list(ops_to), subject = subject, html = html, plain = plain,
                                      attachments = as.list(files)))
    cat(if (ok) "E-mail de TEST envoye a" else "Echec d'envoi de l'e-mail de TEST a", paste(ops_to, collapse = ", "), "\n")
    return(invisible(ok && all_ok))
  }

  # =====================================================================
  # 3. ENVOI NORMAL
  # =====================================================================
  if (!file.exists(state_fp)) { cat("Aucun etat Africa CDC SitRep+Brief trouve :", state_fp, "\n"); return(invisible(all_ok)) }
  state <- tryCatch(readr::read_csv(state_fp, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(state) || nrow(state) == 0 || !"issue_no" %in% names(state)) {
    cat("Etat Africa CDC SitRep+Brief invalide ou vide.\n"); return(invisible(FALSE))
  }
  last <- state[which.max(suppressWarnings(as.integer(state$issue_no))), ]
  issue_no <- as.integer(last$issue_no[1]); sno <- as.integer(last$sitrep_no_source[1])
  anomalies_count <- suppressWarnings(as.integer(last$anomalies_count[1])); if (is.na(anomalies_count)) anomalies_count <- 0L

  sitrep_propre_fp <- file.path(rapports_dir, as.character(last$sitrep_propre_file[1]))
  brief_propre_fp  <- file.path(rapports_dir, as.character(last$brief_propre_file[1]))
  missing <- Filter(function(p) !file.exists(p), c(sitrep_propre_fp, brief_propre_fp))

  sent_state <- if (file.exists(sent_state_fp)) tryCatch(readr::read_csv(sent_state_fp, show_col_types = FALSE), error = function(e) NULL) else NULL
  already <- !is.null(sent_state) && "issue_no" %in% names(sent_state) && issue_no %in% suppressWarnings(as.integer(sent_state$issue_no))
  if (already && !isTRUE(force)) {
    cat("Issue Africa CDC No.", issue_no, "deja notifiee (SitRep+Brief complets). Aucun nouvel envoi.\n")
    return(invisible(all_ok))
  }
  if (length(missing) > 0) {
    cat("Document(s) manquant(s) pour l'Issue", issue_no, ":\n"); for (m in missing) cat(" -", m, "\n")
    return(invisible(FALSE))
  }
  if (length(pub_to) == 0) { cat("Liste des destinataires vide.\n"); return(invisible(FALSE)) }

  subject <- sprintf("PREIS Africa CDC SitRep + Executive Brief - Issue No.%d (DRC SitRep No.%03d)", issue_no, sno)
  anomaly_items <- tryCatch(readLines(file.path(rapports_dir, "africa_cdc_anomalies_latest.txt"), warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  anomaly_items <- anomaly_items[nzchar(trimws(anomaly_items))]
  anomaly_line <- if (anomalies_count > 0) {
    paste0("<p style='font-size:12px;color:#9F2241;margin-bottom:4px'><strong>", anomalies_count,
           " data-reliability note(s)</strong> flagged in this cycle (nothing was silently corrected or invented):</p>",
           if (length(anomaly_items) > 0) paste0("<ul style='font-size:12px;color:#444;margin-top:0'>",
             paste0("<li>", vapply(anomaly_items, .awb_esc, character(1)), "</li>", collapse = ""), "</ul>") else "")
  } else ""
  html <- paste0(
    "<div style='font-family:Segoe UI,Arial,sans-serif;color:#252525;max-width:640px;line-height:1.5'>",
    "<div style='background:#9F2241;color:#fff;padding:16px 20px;border-radius:8px'>",
    "<div style='font-size:17px;font-weight:700'>PREIS &mdash; Africa CDC SitRep + Executive Brief</div>",
    "<div style='font-size:13px;margin-top:3px'>Issue No. ", issue_no, " &middot; DRC SitRep No. ", sprintf("%03d", sno), "</div></div>",
    "<p style='margin-top:16px'>Please find attached the full Africa CDC-format SitRep and Executive Brief for Issue No. ",
    issue_no, ", generated automatically by PREIS from DRC SitRep No. ", sprintf("%03d", sno),
    ". Figures are consistent between the two documents (automatic cross-check). ",
    "Values not present in the DRC SitRep (e.g. Uganda, health-worker figures) are carried over from the previous cycle.</p>", anomaly_line,
    "<p style='font-size:12px;color:#666'>Unlike the DRC quantitative supplement (separate e-mail), these documents follow ",
    "the official Africa CDC template exactly, including Uganda figures and the full operational narrative by pillar. ",
    "They are generated automatically and should be reviewed before onward distribution.</p>", preis_signature_html(), "</div>")
  plain <- paste0(sprintf("PREIS Africa CDC SitRep + Executive Brief - Issue No.%d (DRC SitRep No.%03d). See attached documents.", issue_no, sno), preis_signature_text())

  ok <- .awb_send_mail(py_bin, list(from = from, to = as.list(pub_to), subject = subject, html = html, plain = plain,
        attachments = list(sitrep_propre_fp, brief_propre_fp)))
  if (!ok) { cat("Echec de l'envoi de l'e-mail Africa CDC SitRep+Brief complet (nouvel essai au prochain cycle).\n"); return(invisible(FALSE)) }

  new_row <- data.frame(issue_no = issue_no, sitrep_no_source = sno,
                        sent_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"), stringsAsFactors = FALSE)
  sent_state2 <- if (is.null(sent_state) || !all(names(new_row) %in% names(sent_state))) new_row else {
    sent_state$issue_no <- suppressWarnings(as.integer(sent_state$issue_no))
    sent_state <- sent_state[sent_state$issue_no != issue_no | is.na(sent_state$issue_no), names(new_row), drop = FALSE]
    rbind(sent_state, new_row)
  }
  tryCatch(readr::write_csv(sent_state2, sent_state_fp), error = function(e) cat("Avertissement : etat anti-doublon non enregistre.\n"))
  cat("\nE-mail Africa CDC SitRep+Brief (complet) ENVOYE pour Issue No.", issue_no, "->", paste(pub_to, collapse = ", "), "\n")
  cat("Pieces jointes :", paste(basename(c(sitrep_propre_fp, brief_propre_fp)), collapse = ", "), "\n")
  invisible(all_ok)
}

if (!interactive()) {
  # Volontairement PAS PREIS_FORCE_SEND : ce drapeau (vrai par defaut en lancement manuel) sert aux e-mails
  # habituels ; le reutiliser renverrait une Issue deja livree aux destinataires Africa CDC.
  ok <- send_africacdc_sitrep_brief_full_email(force = isTRUE(as.logical(Sys.getenv("PREIS_AFRICACDC_FORCE_SEND", "FALSE"))))
  if (!isTRUE(ok)) quit(save = "no", status = 2)
}
