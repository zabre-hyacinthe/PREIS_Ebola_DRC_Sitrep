############################################################
# PREIS EBOLA DRC
# 04_send_sitrep_alerts_conditional.R
#
# Adapté de 63_send_alerts_conditional.R (logique PREIS V10).
# Envoi conditionnel par SitRep, avec déduplication.
#
# LOGIQUE :
#   - sent_log_sitrep.csv = source de vérité (jamais 2x le même SitRep)
#   - Pour chaque destinataire actif : envoie uniquement les SitReps
#     non encore envoyés à CETTE adresse
#   - Réutilise preis_send_email() de R/60_email.R
#   - Corps = résumé + signaux + recommandations + lien dashboard
#
# Destinataires : data/final/alert_recipients.csv
#   colonnes : active, type, name, email
############################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tibble)
})

ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Chemins (cohérents avec le pipeline PREIS Ebola)
BASE_DIR    <- Sys.getenv("GITHUB_WORKSPACE",
                          unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
DATA_FINAL  <- file.path(BASE_DIR, "data/final")
OUT_DIR     <- file.path(BASE_DIR, "outputs/analyse")
SERIE_FP    <- file.path(OUT_DIR, "serie_temporelle_nationale.csv")
RECIP_FP    <- file.path(BASE_DIR, "data", "alert_recipients.csv")
SENT_LOG_FP <- file.path(DATA_FINAL, "sent_log_sitrep.csv")

if (!file.exists(SERIE_FP)) stop("Série introuvable : ", SERIE_FP,
                                 "\nLance d'abord 03_analyse_consolidee.R.")
if (!file.exists(RECIP_FP)) stop("Destinataires introuvables : ", RECIP_FP)

# Fonction d'envoi : on privilégie un envoi Python robuste (pas de segfault
# en cloud). 60_email.R reste utilisé s'il est présent ET si on n'est pas
# en cloud, pour compatibilité locale.
email_candidates <- c(
  file.path(BASE_DIR, "scripts", "60_email.R"),
  file.path(BASE_DIR, "scripts", "60_email_FV.R"),
  file.path(ROOT, "scripts", "60_email.R"),
  file.path(ROOT, "R", "60_email.R")
)
email_fn <- email_candidates[file.exists(email_candidates)][1]
has_local_email <- !is.na(email_fn) && length(email_fn) > 0
if (has_local_email) {
  source(email_fn)
  cat("[alerts] Fonction email locale chargée depuis :", email_fn, "\n")
} else {
  cat("[alerts] 60_email.R absent : envoi via Python smtplib (robuste cloud).\n")
}

# --- Envoi email robuste via Python (identique a celui de 08) ---
withr_env <- function(vars, expr) {
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(vars))
  on.exit({
    set_back <- old[!is.na(old)]
    if (length(set_back)) do.call(Sys.setenv, as.list(set_back))
    unset <- names(old)[is.na(old)]
    if (length(unset)) Sys.unsetenv(unset)
  })
  force(expr)
}

send_alert_python <- function(to, subject, body, attachments = character()) {
  smtp_host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  smtp_port <- as.integer(Sys.getenv("SMTP_PORT", "587"))
  smtp_user <- Sys.getenv("SMTP_USER")
  smtp_pass <- Sys.getenv("SMTP_PASS")
  from_addr <- Sys.getenv("ALERT_FROM", smtp_user)
  if (!nzchar(smtp_user) || !nzchar(smtp_pass)) {
    cat("[alerts] SMTP_USER/PASS manquants : envoi Python impossible.\n"); return(FALSE)
  }
  att <- attachments[file.exists(attachments)]
  py <- '
import os, sys, smtplib, ssl
from email.message import EmailMessage
host=os.environ["PY_SMTP_HOST"]; port=int(os.environ["PY_SMTP_PORT"])
user=os.environ["PY_SMTP_USER"]; pwd=os.environ["PY_SMTP_PASS"]; frm=os.environ["PY_FROM"]
to=[x for x in os.environ.get("PY_TO","").split(",") if x]
subject=os.environ.get("PY_SUBJECT",""); body=os.environ.get("PY_BODY","")
atts=[x for x in os.environ.get("PY_ATT","").split("|") if x]
msg=EmailMessage(); msg["From"]=frm; msg["To"]=", ".join(to); msg["Subject"]=subject
msg.set_content(body)
for a in atts:
    if os.path.exists(a):
        with open(a,"rb") as f: data=f.read()
        sub="png" if a.lower().endswith(".png") else "octet-stream"
        maintype="image" if sub=="png" else "application"
        msg.add_attachment(data, maintype=maintype, subtype=sub, filename=os.path.basename(a))
try:
    if port==465:
        with smtplib.SMTP_SSL(host,port,context=ssl.create_default_context(),timeout=60) as s:
            s.login(user,pwd); s.send_message(msg)
    else:
        with smtplib.SMTP(host,port,timeout=60) as s:
            s.ehlo(); s.starttls(context=ssl.create_default_context()); s.ehlo()
            s.login(user,pwd); s.send_message(msg)
    print("PYEMAIL_OK")
except Exception as e:
    print("PYEMAIL_ERROR:", e, file=sys.stderr); sys.exit(1)
'
  tmp_py <- tempfile(fileext = ".py"); writeLines(py, tmp_py)
  res <- withr_env(
    c(PY_SMTP_HOST=smtp_host, PY_SMTP_PORT=as.character(smtp_port),
      PY_SMTP_USER=smtp_user, PY_SMTP_PASS=smtp_pass, PY_FROM=from_addr,
      PY_TO=paste(to,collapse=","), PY_SUBJECT=subject, PY_BODY=body,
      PY_ATT=paste(att,collapse="|")),
    {
      pin <- Sys.which("python3"); if (!nzchar(pin)) pin <- Sys.which("python")
      if (!nzchar(pin)) { cat("[alerts] Python introuvable.\n"); return(FALSE) }
      out <- suppressWarnings(system2(pin, shQuote(tmp_py), stdout=TRUE, stderr=TRUE))
      any(grepl("PYEMAIL_OK", out))
    })
  unlink(tmp_py); isTRUE(res)
}

dash_url <- Sys.getenv("PREIS_DASHBOARD_URL", "")

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble())
  tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) tibble())
}
fmt <- function(x) if (is.na(x)) "non disponible" else format(x, big.mark = " ")

# ------------------------------------------------------------
# 1) Charger la série + destinataires + log
# ------------------------------------------------------------
serie <- read_csv(SERIE_FP, show_col_types = FALSE) %>% arrange(sitrep_no)

recips <- safe_read_csv(RECIP_FP)
for (nm in c("active","type","name","email")) {
  if (!nm %in% names(recips)) recips[[nm]] <- NA_character_
}
recips <- recips %>%
  mutate(
    active = toupper(as.character(active)),
    type   = ifelse(is.na(type) | type == "", "to", tolower(type)),
    name   = as.character(name),
    email  = as.character(email)
  ) %>%
  filter(active %in% c("TRUE","T","1","YES","OUI"),
         !is.na(email), email != "")

if (nrow(recips) == 0) stop("Aucun destinataire actif dans ", RECIP_FP)

sent_log <- safe_read_csv(SENT_LOG_FP)
log_cols <- c("date","recipient_name","recipient_email",
              "message_type","sitrep_no","status")
if (nrow(sent_log) == 0) {
  sent_log <- tibble(date=character(), recipient_name=character(),
                     recipient_email=character(), message_type=character(),
                     sitrep_no=character(), status=character())
} else {
  for (nm in log_cols) if (!nm %in% names(sent_log)) sent_log[[nm]] <- NA_character_
  sent_log <- sent_log %>% mutate(across(all_of(log_cols), as.character))
}

# ------------------------------------------------------------
# 2) Construire le corps (résumé + signaux + recommandations)
# ------------------------------------------------------------
build_sitrep_body <- function(rec_name, sno) {
  row  <- serie %>% filter(sitrep_no == sno)
  prev <- serie %>% filter(sitrep_no < sno) %>% slice_tail(n = 1)
  if (nrow(row) == 0) return(NULL)

  d_cas  <- if (nrow(prev)) row$cas_cumules   - prev$cas_cumules   else NA
  d_dec  <- if (nrow(prev)) row$deces_cumules - prev$deces_cumules else NA
  ndeces <- if ("nouveaux_deces" %in% names(row)) row$nouveaux_deces else NA
  ncas   <- if ("nouveaux_cas"   %in% names(row)) row$nouveaux_cas   else NA

  signaux <- character(); recos <- character()
  if (!is.na(row$cfr) && row$cfr >= 15) {
    signaux <- c(signaux, sprintf("[RED] High lethality: provisional CFR %.1f%%", row$cfr))
    recos <- c(recos, paste0(
      "High CFR - review time from presentation to care, the share of community ",
      "deaths, and treatment capacity in active zones."))
  }
  if (!is.na(ndeces) && ndeces > 0) {
    signaux <- c(signaux, sprintf("[RED] %d new death(s)", as.integer(ndeces)))
    recos <- c(recos, paste0(
      "New deaths - investigate each death (location, delay, known-contact status) ",
      "to distinguish active transmission from community deaths."))
  }
  if (!is.na(ncas) && ncas > 0) {
    signaux <- c(signaux, sprintf("[ORANGE] %d new confirmed case(s)", as.integer(ncas)))
    recos <- c(recos, paste0(
      "New cases - confirm the share arising from listed contacts (controlled ",
      "transmission) versus off-list cases (unresolved chains)."))
  }
  if (!length(signaux)) signaux <- "[GREEN] No critical signal on this SitRep."
  if (!length(recos))   recos   <- "Continue routine surveillance."

  delta_txt <- function(d) if (is.na(d)) "" else sprintf(" (%+d vs previous)", as.integer(d))

  # Advanced signals (module 13_signal_detection.R) - explicit-threshold
  # detection with hypotheses to investigate. Read from the text file.
  adv_signal_block <- character()
  adv_fp <- file.path(DATA_FINAL, "PREIS_signals_text.txt")
  if (file.exists(adv_fp)) {
    adv_txt <- tryCatch(readLines(adv_fp, warn = FALSE, encoding = "UTF-8"),
                        error = function(e) character())
    if (length(adv_txt)) adv_signal_block <- c("", adv_txt)
  }

  # Reconstruct the INSP SitRep page URL from number + date when possible.
  sitrep_link <- ""
  d <- tryCatch(as.Date(row$date), error = function(e) NA)
  if (!is.na(d)) {
    sitrep_link <- sprintf("https://insp.cd/sitrep-n%d-mvb_%s/",
                           sno, format(d, "%d-%m-%Y"))
  }

  lines <- c(
    sprintf("PREIS Ebola DRC - Alert | SitRep No. %d", sno),
    sprintf("Report date: %s", row$date),
    "17th outbreak (Bundibugyo virus - Ituri / North Kivu / South Kivu)",
    "==============================================================",
    "",
    "KEY FIGURES (national cumulative totals, INRB-validated source):",
    sprintf("  - Confirmed cases (cumulative): %s%s", fmt(row$cas_cumules), delta_txt(d_cas)),
    sprintf("  - Deaths (cumulative)         : %s%s", fmt(row$deces_cumules), delta_txt(d_dec)),
    sprintf("  - New deaths                  : %s", fmt(ndeces)),
    sprintf("  - Lethality (provisional CFR) : %s",
            ifelse(is.na(row$cfr),"n/a",paste0(row$cfr,"%"))),
    "",
    "--------------------------------------------------------------",
    "OPERATIONAL SIGNALS:",
    paste0("  ", signaux),
    adv_signal_block,
    "",
    "--------------------------------------------------------------",
    "SUGGESTED ACTIONS (to investigate, not directives):",
    paste0("  - ", recos),
    "",
    "--------------------------------------------------------------",
    "LINKS:",
    if (nzchar(sitrep_link)) paste0("  - Source SitRep (INSP): ", sitrep_link) else "",
    if (nzchar(dash_url))    paste0("  - Interactive dashboard: ", dash_url) else
      "  - Interactive dashboard: (set PREIS_DASHBOARD_URL to display)",
    "",
    "==============================================================",
    "NOTES:",
    "  - Provisional CFR: recent cases may still evolve; not the final lethality.",
    "  - Signals are facts plus hypotheses to investigate - no diagnosis, no",
    "    established causality.",
    "  - National totals are INRB-validated; zone-level detail is extracted from",
    "    the PDF and should be validated.",
    sprintf("  - Generated automatically on %s by the PREIS pipeline.", Sys.Date()),
    "",
    "--",
    "Generated by the PREIS automated surveillance system",
    "Developed by Dr Hyacinthe ZABRE, Epidemiologist-Biostatistician",
    "Email: raogoz@africacdc.org | WhatsApp: +226 78 08 87 70"
  )
  paste(lines[nzchar(lines) | TRUE], collapse = "\n")
}

# ------------------------------------------------------------
# 3) Boucle destinataires : envoyer les SitReps non encore envoyés
# ------------------------------------------------------------
all_snos <- sort(unique(serie$sitrep_no))
new_log <- list()

# Détecter le support des pièces jointes par ta fonction
send_formals <- tryCatch(names(formals(preis_send_email)), error = function(e) character(0))
supports_attach <- "attachments" %in% send_formals

# Pièces jointes par défaut : les graphiques d'analyse
default_attach <- file.path(OUT_DIR, c(
  "g1_courbe_epidemique.png","g2_courbe_mortalite.png",
  "g3_evolution_cfr.png","carte_zones_intensite.png"))
default_attach <- default_attach[file.exists(default_attach)]

for (i in seq_len(nrow(recips))) {
  addr   <- recips$email[i]
  nm_rec <- recips$name[i]

  # SitReps déjà envoyés à CETTE adresse
  done <- sent_log %>%
    filter(recipient_email == addr,
           toupper(message_type) == "SITREP",
           status == "sent") %>%
    pull(sitrep_no) %>% as.integer() %>% unique()

  # MODE TEST : si PREIS_TEST_ALERT=true, on force le renvoi du dernier
  # SitRep (ignore l'anti-doublon) pour vérifier l'email d'alerte.
  test_alert <- tolower(Sys.getenv("PREIS_TEST_ALERT","false")) %in% c("true","1","yes")
  if (test_alert) done <- integer(0)

  to_send <- setdiff(all_snos, done)

  # Par défaut : envoyer SEULEMENT le dernier SitRep non envoyé
  # (évite d'inonder à la première exécution). Pour tout envoyer,
  # mettre PREIS_SEND_ALL_SITREPS=true dans .Renviron.
  send_all <- tolower(Sys.getenv("PREIS_SEND_ALL_SITREPS","false")) %in% c("true","1","yes")
  if (!send_all && length(to_send) > 0) to_send <- max(to_send)

  if (length(to_send) == 0) {
    cat(sprintf("[alerts] Aucun nouveau SitRep pour %s (%s)\n", nm_rec, addr))
    next
  }

  for (sno in sort(to_send)) {
    body <- build_sitrep_body(nm_rec, sno)
    if (is.null(body)) next

    row <- serie %>% filter(sitrep_no == sno)
    subject <- sprintf("[PREIS Ebola DRC] SitRep No. %d - %s cases, %s deaths (CFR %s%%)",
                       sno, fmt(row$cas_cumules), fmt(row$deces_cumules),
                       ifelse(is.na(row$cfr),"n/a",row$cfr))

    ok <- TRUE
    tryCatch({
      # Cloud (ou 60_email.R absent) : envoi Python robuste en priorité.
      use_python <- !has_local_email || nzchar(Sys.getenv("GITHUB_WORKSPACE"))
      if (use_python) {
        att <- if (length(default_attach) > 0) default_attach else character()
        sent <- send_alert_python(to = addr, subject = subject,
                                  body = body, attachments = att)
        if (!sent) stop("envoi Python a echoue")
      } else if (supports_attach && length(default_attach) > 0) {
        preis_send_email(to = addr, subject = subject,
                         body_text = body, attachments = default_attach,
                         dry_run = FALSE)
      } else {
        preis_send_email(to = addr, subject = subject,
                         body_text = body, dry_run = FALSE)
      }
      cat(sprintf("[alerts] SitRep %d envoye a %s (%s)\n", sno, nm_rec, addr))
    }, error = function(e) {
      ok <<- FALSE
      cat(sprintf("[alerts] ECHEC SitRep %d pour %s : %s\n",
                  sno, addr, conditionMessage(e)))
    })

    if (ok) {
      new_log[[length(new_log)+1]] <- tibble(
        date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        recipient_name = nm_rec, recipient_email = addr,
        message_type = "SITREP", sitrep_no = as.character(sno),
        status = "sent")
    }
  }
}

# ------------------------------------------------------------
# 4) Mettre à jour le log (sauf en MODE TEST, pour ne pas polluer l'historique)
# ------------------------------------------------------------
test_alert_mode <- tolower(Sys.getenv("PREIS_TEST_ALERT","false")) %in% c("true","1","yes")
if (test_alert_mode) {
  cat("[alerts] MODE TEST : sent_log NON modifié (historique préservé).\n")
} else if (length(new_log) > 0) {
  add <- bind_rows(new_log)
  sent_log <- if (nrow(sent_log) > 0) bind_rows(sent_log, add) else add
  write_csv(sent_log, SENT_LOG_FP, na = "")
  cat(sprintf("[alerts] sent_log mis a jour : %d nouvelle(s) ligne(s)\n", nrow(add)))
} else {
  cat("[alerts] Aucun envoi effectue.\n")
}
