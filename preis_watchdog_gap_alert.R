############################################################
# preis_watchdog_gap_alert.R
#
# GARDE-FOU PREIS — alerte "SitRep sauté".
#
# Probleme resolu : l'email principal (preis_email_enrichi.R) tourne
# en `continue-on-error` et se bloque volontairement (fail-safe) quand
# la serie n'a pas encore integre le dernier SitRep. Resultat : un
# SitRep peut etre TELECHARGE mais jamais envoye, SANS que personne
# ne soit prevenu (ex. SR75, SR76, SR82).
#
# Ce script, lance en fin de workflow (if: always()), compare :
#   - les SitReps deja notifies  (data/preis_email_enrichi_state.csv)
#   - les SitReps reellement disponibles (serie + registre + PDF)
# et, s'il detecte un ecart, envoie UN email d'alerte listant les
# SitReps manquants + la cause probable (extraction bloquee vs run non
# execute). Anti-spam : n'alerte pas deux fois le meme ecart.
#
# 100 % autonome : reutilise vos secrets SMTP existants et la meme
# methode d'envoi Python que preis_email_enrichi.R. Aucune dependance
# R supplementaire (base R uniquement). N'envoie jamais de donnees
# epidemiologiques — seulement un signal technique a l'exploitant.
#
# Variables d'environnement :
#   SMTP_USER/SMTP_PASS (+ SMTP_HOST, SMTP_PORT, ALERT_FROM, ALERT_TO)
#   PREIS_WATCHDOG_TO      (optionnel) destinataire dedie ; sinon ALERT_TO
#   PREIS_WATCHDOG_FORCE   "true" pour forcer un envoi meme sans nouvel ecart
#
# Usage manuel :  Rscript --vanilla preis_watchdog_gap_alert.R
############################################################

options(warn = 1)

.env_get <- function(names, default = "") {
  for (n in names) {
    v <- Sys.getenv(n, "")
    if (nzchar(v)) return(v)
  }
  default
}

.read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE,
                    fileEncoding = "UTF-8"),
    error = function(e) tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE),
      error = function(e2) NULL
    )
  )
}

.as_int <- function(x) suppressWarnings(as.integer(as.character(x)))

.is_true <- function(x) toupper(trimws(as.character(x))) %in%
  c("TRUE", "T", "1", "YES", "OUI", "Y")

# ------------------------------------------------------------
# 0) Racine du depot
# ------------------------------------------------------------
root <- if (file.exists(file.path(getwd(),
             "data", "preis_email_enrichi_state.csv"))) {
  getwd()
} else if (file.exists("data/preis_email_enrichi_state.csv")) {
  getwd()
} else {
  getwd()
}
setwd(root)

EMAIL_STATE  <- "data/preis_email_enrichi_state.csv"
SERIES_FP    <- "outputs/analyse/serie_temporelle_nationale.csv"
REGISTRY_FP  <- "data/final/sitrep_registry.csv"
PDF_DIR      <- "data/pdf"
WD_STATE_FP  <- "data/preis_watchdog_state.csv"

# ------------------------------------------------------------
# 1) SitReps deja notifies par email
# ------------------------------------------------------------
emailed <- integer(0)
es <- .read_csv_safe(EMAIL_STATE)
if (!is.null(es) && "sitrep_no" %in% names(es)) {
  emailed <- sort(unique(.as_int(es$sitrep_no)))
  emailed <- emailed[!is.na(emailed)]
}
last_emailed <- if (length(emailed)) max(emailed) else 0L
# Le systeme d'email a demarre a un certain SitRep : on ignore tout ce
# qui precede, pour ne pas signaler d'anciens SitReps pre-automatisation.
floor_sno <- if (length(emailed)) min(emailed) else 0L

# ------------------------------------------------------------
# 2) SitReps reellement disponibles (serie + registre + PDF)
# ------------------------------------------------------------
series_nums <- integer(0)
se <- .read_csv_safe(SERIES_FP)
if (!is.null(se) && "sitrep_no" %in% names(se)) {
  series_nums <- .as_int(se$sitrep_no)
  series_nums <- sort(unique(series_nums[!is.na(series_nums)]))
}
series_max <- if (length(series_nums)) max(series_nums) else 0L

downloaded_nums <- integer(0)
rg <- .read_csv_safe(REGISTRY_FP)
if (!is.null(rg) && "sitrep_no" %in% names(rg)) {
  dl_col <- if ("downloaded" %in% names(rg)) rg$downloaded else NA
  keep <- if (length(dl_col) == nrow(rg)) vapply(dl_col, .is_true, logical(1)) else rep(TRUE, nrow(rg))
  downloaded_nums <- .as_int(rg$sitrep_no[keep])
  downloaded_nums <- sort(unique(downloaded_nums[!is.na(downloaded_nums)]))
}
downloaded_max <- if (length(downloaded_nums)) max(downloaded_nums) else 0L

pdf_nums <- integer(0)
if (dir.exists(PDF_DIR)) {
  pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = FALSE)
  n1 <- .as_int(sub(".*PREIS_DRC_Ebola_SitRep_0*([0-9]{1,3})\\.pdf$", "\\1",
                    pdfs[grepl("PREIS_DRC_Ebola_SitRep_", pdfs)]))
  n2 <- .as_int(sub(".*SitRep_0*([0-9]{1,3})_2026\\.pdf$", "\\1",
                    pdfs[grepl("SitRep_[0-9]", pdfs)]))
  pdf_nums <- sort(unique(c(n1, n2)))
  pdf_nums <- pdf_nums[!is.na(pdf_nums)]
}

# Un SitRep n'est reellement "envoyable" que s'il existe un PDF officiel
# (downloaded=TRUE au registre OU fichier PDF present). On EXCLUT les
# numeros presents uniquement dans la serie (dates interpolees) : sinon
# un SitRep jamais publie par l'INSP -- ex. N63, saute entre N62 et N64 --
# serait signale a tort comme "manquant".
available <- sort(unique(c(downloaded_nums, pdf_nums)))
available <- available[!is.na(available) & available >= floor_sno]
latest_available <- if (length(available)) max(available) else 0L

# ------------------------------------------------------------
# 3) Diagnostic de l'ecart
# ------------------------------------------------------------
missing <- sort(setdiff(available, emailed))          # existent mais jamais envoyes
missing <- missing[missing > last_emailed]
extraction_stuck <- downloaded_max > series_max        # PDF present, serie en retard

force <- .is_true(Sys.getenv("PREIS_WATCHDOG_FORCE", "false"))

cat(sprintf(
  "[watchdog] last_emailed=%d | series_max=%d | downloaded_max=%d | latest_available=%d\n",
  last_emailed, series_max, downloaded_max, latest_available))
cat(sprintf("[watchdog] missing=%s | extraction_stuck=%s\n",
            if (length(missing)) paste(missing, collapse = ",") else "(none)",
            extraction_stuck))

if (length(missing) == 0 && !extraction_stuck && !force) {
  cat("[watchdog] Aucun ecart. Rien a signaler.\n")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------
# 4) Anti-spam : ne pas re-alerter le meme ecart a chaque cron
# ------------------------------------------------------------
signature <- paste0("missing=", paste(missing, collapse = ","),
                    ";series=", series_max, ";dl=", downloaded_max)
prev_sig <- ""
wd <- .read_csv_safe(WD_STATE_FP)
if (!is.null(wd) && "signature" %in% names(wd) && nrow(wd) > 0) {
  prev_sig <- as.character(wd$signature[nrow(wd)])
}
if (identical(signature, prev_sig) && !force) {
  cat("[watchdog] Ecart deja signale (meme signature). Pas de nouvel email.\n")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------
# 5) Construire le message d'alerte
# ------------------------------------------------------------
cause <- if (extraction_stuck) {
  paste0("Cause probable : EXTRACTION BLOQUEE. Le PDF du SitRep ",
         downloaded_max, " est telecharge mais la serie s'arrete a ",
         series_max, " (changement de format PDF, cf. blocage type N65). ",
         "L'email se bloque alors par securite (fail-safe) et ne renvoie ",
         "meme pas le dernier SitRep valide.")
} else {
  paste0("Cause probable : run non execute ou envoi echoue silencieusement ",
         "(ex. panne GitHub Actions, ou erreur SMTP avalee par continue-on-error).")
}

reco <- paste(
  "Actions a faire :",
  "  1. Verifier githubstatus.com (Actions). Si panne, attendre le retablissement.",
  "  2. Onglet Actions -> Run workflow -> force_send = true (relance manuelle).",
  "  3. Si l'ecart persiste apres relance : extraction bloquee -> appliquer le",
  "     repli additif du master (voir PREIS_blocage_N65_diagnostic.md), puis relancer.",
  sep = "\n")

missing_txt <- if (length(missing)) paste(missing, collapse = ", ") else "(serie en retard)"

plain <- paste(
  "ALERTE PREIS - SitRep(s) non envoye(s)",
  "",
  paste0("SitRep(s) manquant(s) : ", missing_txt),
  paste0("Dernier envoye : ", last_emailed,
         " | Serie : ", series_max,
         " | PDF telecharge le plus recent : ", downloaded_max),
  "",
  cause,
  "",
  reco,
  "",
  paste0("Genere le ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
         " par preis_watchdog_gap_alert.R"),
  sep = "\n")

html <- paste0(
  "<div style='font-family:Segoe UI,Arial,sans-serif;color:#252525;max-width:640px;line-height:1.45'>",
  "<div style='background:#8A1C1C;color:#fff;padding:14px 18px;border-radius:8px;font-size:17px;font-weight:700'>",
  "PREIS &mdash; Alerte : SitRep non envoy&eacute;</div>",
  "<p style='margin:14px 0 6px'><b>SitRep(s) manquant(s) :</b> ", missing_txt, "</p>",
  "<table style='border-collapse:collapse;font-size:14px'>",
  "<tr><td style='padding:3px 10px;color:#666'>Dernier envoy&eacute;</td><td style='padding:3px 10px'><b>", last_emailed, "</b></td></tr>",
  "<tr><td style='padding:3px 10px;color:#666'>S&eacute;rie (dashboard)</td><td style='padding:3px 10px'><b>", series_max, "</b></td></tr>",
  "<tr><td style='padding:3px 10px;color:#666'>PDF t&eacute;l&eacute;charg&eacute; le + r&eacute;cent</td><td style='padding:3px 10px'><b>", downloaded_max, "</b></td></tr>",
  "</table>",
  "<p style='background:#FFF4E5;border-left:4px solid #EF7F1A;padding:10px 12px;margin:14px 0'>", cause, "</p>",
  "<pre style='background:#F4F6F7;padding:10px 12px;border-radius:6px;font-size:13px;white-space:pre-wrap'>", reco, "</pre>",
  "<p style='font-size:11px;color:#888'>G&eacute;n&eacute;r&eacute; le ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
  " par preis_watchdog_gap_alert.R</p></div>")

subject <- if (length(missing)) {
  sprintf("[PREIS][ALERTE] SitRep non envoye : %s", missing_txt)
} else {
  sprintf("[PREIS][ALERTE] Serie bloquee a SR%d (PDF SR%d present)",
          series_max, downloaded_max)
}

# ------------------------------------------------------------
# 6) Envoi (meme methode Python SMTP que preis_email_enrichi.R)
# ------------------------------------------------------------
smtp_user <- .env_get(c("SMTP_USER", "SMTP_USERNAME"))
smtp_pass <- .env_get(c("SMTP_PASS", "SMTP_PASSWORD"))
from <- .env_get(c("ALERT_FROM", "EMAIL_FROM", "SMTP_FROM", "MAIL_FROM"), smtp_user)
to   <- .env_get(c("PREIS_WATCHDOG_TO", "ALERT_TO", "EMAIL_TO",
                   "PREIS_ALERT_TO", "PREIS_EMAIL_TO", "SMTP_TO", "MAIL_TO"), from)

if (!nzchar(smtp_user) || !nzchar(smtp_pass) || !nzchar(from) || !nzchar(to)) {
  cat("[watchdog] Identifiants/destinataires SMTP manquants. Pas d'envoi.\n")
  quit(save = "no", status = 3)
}

to_vec <- unique(trimws(unlist(strsplit(to, "[,;]"))))
to_vec <- to_vec[nzchar(to_vec)]

cfg_dir <- file.path(tempdir(), "preis_watchdog")
dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
cfg <- list(from = from, to = as.list(to_vec), subject = subject,
            html = html, plain = plain)
cfg_fp <- file.path(cfg_dir, "cfg.json")

# Petit encodeur JSON sans dependance (jsonlite non requis)
.json_str <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"', '\\\\"', s)
  s <- gsub("\n", "\\\\n", s)
  s <- gsub("\r", "", s)
  s <- gsub("\t", "\\\\t", s)
  paste0('"', s, '"')
}
to_json_arr <- paste0("[", paste(vapply(to_vec, .json_str, character(1)), collapse = ","), "]")
json <- paste0(
  "{",
  '"from":', .json_str(from), ",",
  '"to":', to_json_arr, ",",
  '"subject":', .json_str(subject), ",",
  '"html":', .json_str(html), ",",
  '"plain":', .json_str(plain),
  "}")
writeLines(json, cfg_fp, useBytes = TRUE)

py <- c(
  "import sys, json, os, ssl, smtplib",
  "from email.message import EmailMessage",
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
  "msg.set_content(cfg.get('plain','PREIS watchdog alert'))",
  "msg.add_alternative(cfg['html'], subtype='html')",
  "ctx = ssl.create_default_context()",
  "if use_ssl:",
  "    with smtplib.SMTP_SSL(host, port, timeout=120, context=ctx) as s:",
  "        s.login(user, pw); s.send_message(msg)",
  "else:",
  "    with smtplib.SMTP(host, port, timeout=120) as s:",
  "        s.ehlo(); s.starttls(context=ctx); s.ehlo(); s.login(user, pw); s.send_message(msg)",
  "print('SENT_OK')"
)
py_fp <- file.path(cfg_dir, "send.py")
writeLines(py, py_fp, useBytes = TRUE)

py_bin <- Sys.which("python3"); if (!nzchar(py_bin)) py_bin <- Sys.which("python")
if (!nzchar(py_bin)) { cat("[watchdog] Python introuvable.\n"); quit(save = "no", status = 4) }

out <- tryCatch(
  system2(py_bin, c(shQuote(py_fp), shQuote(cfg_fp)), stdout = TRUE, stderr = TRUE),
  error = function(e) conditionMessage(e))
cat(paste(out, collapse = "\n"), "\n")

if (!any(grepl("SENT_OK", out, fixed = TRUE))) {
  cat("[watchdog] Echec de l'envoi de l'alerte.\n")
  quit(save = "no", status = 5)
}

# ------------------------------------------------------------
# 7) Memoriser la signature (anti-spam)
# ------------------------------------------------------------
new_row <- data.frame(
  signature = signature,
  alerted_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
  missing = if (length(missing)) paste(missing, collapse = "|") else "",
  stringsAsFactors = FALSE)
wd2 <- if (is.null(wd) || !all(names(new_row) %in% names(wd))) new_row else rbind(wd[names(new_row)], new_row)
dir.create(dirname(WD_STATE_FP), recursive = TRUE, showWarnings = FALSE)
tryCatch(utils::write.csv(wd2, WD_STATE_FP, row.names = FALSE),
         error = function(e) cat("[watchdog] Etat non enregistre :", conditionMessage(e), "\n"))

cat(sprintf("[watchdog] ALERTE ENVOYEE -> %s\n", paste(to_vec, collapse = ", ")))
invisible(TRUE)
