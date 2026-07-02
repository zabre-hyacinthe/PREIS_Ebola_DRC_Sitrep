############################################################
# PREIS SMTP scientific fallback patch
# If Gmail blocks full message/PDF, retry with scientific text
############################################################

# === PREIS SMTP scientific fallback START ===

preis_scientific_patch_smtp_python_lines <- function(text) {
  if (!is.character(text)) {
    return(text)
  }

  target <- "server.send_message(msg, from_addr=alert_from, to_addrs=recipients)"

  if (!any(grepl(target, text, fixed = TRUE))) {
    return(text)
  }

  if (any(grepl("PYTHON_SMTP_SCIENTIFIC_FALLBACK_OK", text, fixed = TRUE))) {
    return(text)
  }

  idx <- grep(target, text, fixed = TRUE)

  for (i in rev(idx)) {
    indent <- sub("^(\\s*).*$", "\\1", text[i])

    replacement <- c(
      paste0(indent, "import smtplib as _preis_smtplib"),
      paste0(indent, "import os as _preis_os"),
      paste0(indent, "import re as _preis_re"),
      paste0(indent, "try:"),
      paste0(indent, "    server.send_message(msg, from_addr=alert_from, to_addrs=recipients)"),
      paste0(indent, "except Exception as _preis_e:"),
      paste0(indent, "    _preis_blocked = False"),
      paste0(indent, "    if isinstance(_preis_e, _preis_smtplib.SMTPDataError):"),
      paste0(indent, "        _preis_code = getattr(_preis_e, 'smtp_code', None)"),
      paste0(indent, "        _preis_error = str(getattr(_preis_e, 'smtp_error', b''))"),
      paste0(indent, "        _preis_blocked = (_preis_code == 552) or ('5.7.0' in _preis_error) or ('security issue' in _preis_error.lower())"),
      paste0(indent, "    if not _preis_blocked:"),
      paste0(indent, "        raise"),
      paste0(indent, "    print('PYTHON_SMTP_RETRY_SCIENTIFIC_TEXT: Gmail blocked full message/PDF; retrying scientific text-only alert.', flush=True)"),
      paste0(indent, "    from email.message import EmailMessage as _PreisEmailMessage"),
      paste0(indent, "    _text_parts = []"),
      paste0(indent, "    _attachments = []"),
      paste0(indent, "    try:"),
      paste0(indent, "        for _part in msg.walk():"),
      paste0(indent, "            _fn = _part.get_filename()"),
      paste0(indent, "            if _fn:"),
      paste0(indent, "                _attachments.append(str(_fn))"),
      paste0(indent, "            try:"),
      paste0(indent, "                if _part.get_content_maintype() == 'text':"),
      paste0(indent, "                    _text_parts.append(str(_part.get_content()))"),
      paste0(indent, "            except Exception:"),
      paste0(indent, "                pass"),
      paste0(indent, "    except Exception:"),
      paste0(indent, "        try:"),
      paste0(indent, "            _text_parts.append(str(msg.get_content()))"),
      paste0(indent, "        except Exception:"),
      paste0(indent, "            pass"),
      paste0(indent, "    _subject = str(msg.get('Subject', 'PREIS Ebola DRC SitRep alert'))"),
      paste0(indent, "    _original_text = '\\n'.join([str(x) for x in _text_parts])"),
      paste0(indent, "    _urls = []"),
      paste0(indent, "    for _u in _preis_re.findall(r\"https?://[^\\s<>]+\", _original_text):"),
      paste0(indent, "        _u = _u.strip().rstrip('.,);]')"),
      paste0(indent, "        if _u and _u not in _urls:"),
      paste0(indent, "            _urls.append(_u)"),
      paste0(indent, "    _page_urls = [u for u in _urls if 'insp.cd/sitrep' in u.lower()]"),
      paste0(indent, "    _pdf_urls = [u for u in _urls if '.pdf' in u.lower()]"),
      paste0(indent, "    if not _page_urls:"),
      paste0(indent, "        _page_urls = ['https://insp.cd/category/sitrep/']"),
      paste0(indent, "    _repo = _preis_os.environ.get('GITHUB_REPOSITORY', '')"),
      paste0(indent, "    _runid = _preis_os.environ.get('GITHUB_RUN_ID', '')"),
      paste0(indent, "    _run_url = ''"),
      paste0(indent, "    if _repo and _runid:"),
      paste0(indent, "        _run_url = 'https://github.com/' + _repo + '/actions/runs/' + _runid"),
      paste0(indent, "    _m = _preis_re.search(r'(N[°ºo]?[ ]*0*[0-9]{1,3}|SitRep[ ]*N?[°ºo]?[ ]*0*[0-9]{1,3})', _subject + '\\n' + _original_text, _preis_re.IGNORECASE)"),
      paste0(indent, "    _sitrep_label = _m.group(0).strip() if _m else 'nouveau SitRep détecté'"),
      paste0(indent, "    _fallback = _PreisEmailMessage()"),
      paste0(indent, "    _fallback['From'] = msg.get('From', alert_from)"),
      paste0(indent, "    _fallback['To'] = msg.get('To', ', '.join(recipients) if isinstance(recipients, (list, tuple)) else str(recipients))"),
      paste0(indent, "    if msg.get('Cc'):"),
      paste0(indent, "        _fallback['Cc'] = msg.get('Cc')"),
      paste0(indent, "    _fallback['Subject'] = '[PREIS Ebola RDC] Alerte SitRep - ' + _sitrep_label"),
      paste0(indent, "    _lines = []"),
      paste0(indent, "    _lines.append('PREIS Ebola RDC - Alerte SitRep automatisée')"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('Objet : nouveau rapport de situation Ebola RDC détecté par PREIS.')"),
      paste0(indent, "    _lines.append('SitRep : ' + _sitrep_label)"),
      paste0(indent, "    _lines.append('Source : INSP RDC / page officielle SitRep.')"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('Résumé opérationnel')"),
      paste0(indent, "    _lines.append('- PREIS a détecté un nouveau SitRep publié en ligne.')"),
      paste0(indent, "    _lines.append('- Le workflow cloud a été exécuté avec succès.')"),
      paste0(indent, "    _lines.append('- Les données détaillées et les indicateurs doivent être consultés dans le SitRep et dans les sorties PREIS.')"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('Liens de vérification')"),
      paste0(indent, "    for _u in _page_urls[:3]:"),
      paste0(indent, "        _lines.append('- Page INSP : ' + _u)"),
      paste0(indent, "    for _u in _pdf_urls[:3]:"),
      paste0(indent, "        _lines.append('- PDF SitRep : ' + _u)"),
      paste0(indent, "    if _run_url:"),
      paste0(indent, "        _lines.append('- Run GitHub PREIS : ' + _run_url)"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('Pièce jointe')"),
      paste0(indent, "    if _attachments:"),
      paste0(indent, "        _lines.append('- PREIS a tenté d’envoyer la ou les pièces jointes suivantes : ' + ', '.join(_attachments[:5]))"),
      paste0(indent, "        _lines.append('- Gmail a bloqué le message complet pour raison de sécurité. Cet email de secours est donc envoyé sans pièce jointe.')"),
      paste0(indent, "    else:"),
      paste0(indent, "        _lines.append('- Aucune pièce jointe n’a été transmise dans cet email de secours.')"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('Note méthodologique')"),
      paste0(indent, "    _lines.append('- Cette alerte est générée automatiquement à partir des sources SitRep/PREIS.')"),
      paste0(indent, "    _lines.append('- Les signaux PREIS sont des signaux opérationnels et doivent être interprétés avec les données officielles validées.')"),
      paste0(indent, "    _lines.append('- Cette notification ne remplace pas la validation épidémiologique officielle.')"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('Action attendue')"),
      paste0(indent, "    _lines.append('- Ouvrir le lien INSP/PDF ci-dessus et vérifier les principaux changements épidémiologiques et opérationnels.')"),
      paste0(indent, "    _lines.append('- Mettre à jour les actions de coordination si de nouveaux signaux ou gaps sont confirmés.')"),
      paste0(indent, "    _lines.append('')"),
      paste0(indent, "    _lines.append('PREIS Ebola DRC Automation')"),
      paste0(indent, "    _fallback.set_content('\\n'.join(_lines))"),
      paste0(indent, "    server.send_message(_fallback, from_addr=alert_from, to_addrs=recipients)"),
      paste0(indent, "    print('PYTHON_SMTP_SCIENTIFIC_FALLBACK_OK: scientific text-only alert sent with available links.', flush=True)")
    )

    before <- if (i > 1) text[1:(i - 1)] else character(0)
    after <- if (i < length(text)) text[(i + 1):length(text)] else character(0)
    text <- c(before, replacement, after)
  }

  message("[PREIS SMTP SCIENTIFIC PATCH] Python SMTP sender patched")
  text
}

writeLines <- function(text, con = stdout(), sep = "\n", useBytes = FALSE) {
  if (is.character(text) && any(grepl("server.send_message(msg, from_addr=alert_from, to_addrs=recipients)", text, fixed = TRUE))) {
    text <- preis_scientific_patch_smtp_python_lines(text)
  }

  base::writeLines(text, con = con, sep = sep, useBytes = useBytes)
}

# === PREIS SMTP scientific fallback END ===
