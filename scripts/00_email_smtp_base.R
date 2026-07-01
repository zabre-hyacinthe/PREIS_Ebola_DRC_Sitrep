############################################################
# PREIS Ebola RDC — SMTP email helper
# Fichier: scripts/00_email_smtp_base.R
#
# Version definitive:
#   - aucun curl SMTP
#   - aucun emayili
#   - email via Python standard smtplib
#   - les secrets sont transmis via fichier config temporaire encode hex
############################################################

preis_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    value <- default
  }
  trimws(as.character(value[1]))
}

preis_split_emails <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(character())
  }

  x <- unlist(strsplit(as.character(x[1]), "[,;]"))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
}

preis_validate_emails <- function(x, field_name) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }

  bad <- x[!grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", x)]

  if (length(bad) > 0) {
    stop(
      field_name,
      " contient email(s) invalide(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

preis_find_python <- function() {
  py <- Sys.which("python3")
  if (!nzchar(py)) {
    py <- Sys.which("python")
  }
  if (!nzchar(py)) {
    stop("Python introuvable sur le runner.", call. = FALSE)
  }
  py
}

preis_redact <- function(x, smtp_user, smtp_pass) {
  x <- paste(as.character(x), collapse = "\n")

  if (!is.na(smtp_pass) && nzchar(smtp_pass)) {
    x <- gsub(smtp_pass, "********", x, fixed = TRUE)
  }

  if (!is.na(smtp_user) && nzchar(smtp_user)) {
    x <- gsub(smtp_user, "SMTP_USER", x, fixed = TRUE)
  }

  x
}

preis_hex <- function(x) {
  x <- enc2utf8(as.character(x)[1])
  paste(sprintf("%02x", as.integer(charToRaw(x))), collapse = "")
}

preis_write_hex_config <- function(config_file, values) {
  keys <- names(values)

  lines <- vapply(
    keys,
    function(k) {
      paste0(k, "=", preis_hex(values[[k]]))
    },
    character(1),
    USE.NAMES = FALSE
  )

  writeLines(lines, config_file, useBytes = TRUE)
}

preis_write_python_sender <- function(py_file) {
  py_lines <- c(
    "import os",
    "import re",
    "import ssl",
    "import sys",
    "import smtplib",
    "import traceback",
    "from email.message import EmailMessage",
    "",
    "def load_config(path):",
    "    cfg = {}",
    "    with open(path, 'r', encoding='utf-8') as f:",
    "        for line in f:",
    "            line = line.rstrip('\\r\\n')",
    "            if not line or '=' not in line:",
    "                continue",
    "            k, v = line.split('=', 1)",
    "            cfg[k] = bytes.fromhex(v).decode('utf-8')",
    "    return cfg",
    "",
    "CFG = load_config(sys.argv[1]) if len(sys.argv) > 1 else {}",
    "",
    "def cfgget(name, default=''):",
    "    value = CFG.get(name, default)",
    "    if value is None:",
    "        value = default",
    "    return str(value).strip()",
    "",
    "def split_emails(value):",
    "    value = value or ''",
    "    parts = re.split(r'[,;]', value)",
    "    out = []",
    "    for p in parts:",
    "        p = p.strip()",
    "        if p:",
    "            out.append(p)",
    "    return list(dict.fromkeys(out))",
    "",
    "def redact(text):",
    "    text = str(text)",
    "    user = cfgget('SMTP_USER')",
    "    pwd = cfgget('SMTP_PASS')",
    "    if pwd:",
    "        text = text.replace(pwd, '********')",
    "    if user:",
    "        text = text.replace(user, 'SMTP_USER')",
    "    return text",
    "",
    "def require_email_list(values, name):",
    "    if not values:",
    "        raise ValueError(f'{name} est vide.')",
    "    bad = [x for x in values if not re.match(r'^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$', x)]",
    "    if bad:",
    "        raise ValueError(f'{name} contient email(s) invalide(s): {\", \".join(bad)}')",
    "",
    "def main():",
    "    smtp_host = cfgget('SMTP_HOST')",
    "    smtp_port = int(cfgget('SMTP_PORT', '465'))",
    "    smtp_user = cfgget('SMTP_USER')",
    "    smtp_pass = cfgget('SMTP_PASS')",
    "    alert_from = cfgget('ALERT_FROM')",
    "    alert_to = split_emails(cfgget('ALERT_TO'))",
    "    alert_cc = split_emails(cfgget('ALERT_CC'))",
    "    alert_bcc = split_emails(cfgget('ALERT_BCC'))",
    "    subject = cfgget('PREIS_EMAIL_SUBJECT')",
    "    body_file = cfgget('PREIS_EMAIL_BODY_FILE')",
    "    attachment = cfgget('PREIS_EMAIL_ATTACHMENT')",
    "",
    "    if not smtp_host:",
    "        raise ValueError('SMTP_HOST est vide.')",
    "    if not smtp_user:",
    "        raise ValueError('SMTP_USER est vide.')",
    "    if not smtp_pass:",
    "        raise ValueError('SMTP_PASS est vide.')",
    "    if not alert_from:",
    "        raise ValueError('ALERT_FROM est vide.')",
    "    if not subject:",
    "        raise ValueError('PREIS_EMAIL_SUBJECT est vide.')",
    "    if not body_file or not os.path.exists(body_file):",
    "        raise ValueError('PREIS_EMAIL_BODY_FILE est introuvable.')",
    "    if not attachment or not os.path.exists(attachment):",
    "        raise ValueError('PREIS_EMAIL_ATTACHMENT est introuvable.')",
    "",
    "    require_email_list([alert_from], 'ALERT_FROM')",
    "    require_email_list(alert_to, 'ALERT_TO')",
    "",
    "    with open(body_file, 'r', encoding='utf-8') as f:",
    "        body = f.read()",
    "",
    "    recipients = list(dict.fromkeys(alert_to + alert_cc + alert_bcc))",
    "",
    "    msg = EmailMessage()",
    "    msg['From'] = alert_from",
    "    msg['To'] = ', '.join(alert_to)",
    "    if alert_cc:",
    "        msg['Cc'] = ', '.join(alert_cc)",
    "    msg['Subject'] = subject",
    "    msg.set_content(body, charset='utf-8')",
    "",
    "    with open(attachment, 'rb') as f:",
    "        pdf_data = f.read()",
    "",
    "    msg.add_attachment(",
    "        pdf_data,",
    "        maintype='application',",
    "        subtype='pdf',",
    "        filename=os.path.basename(attachment)",
    "    )",
    "",
    "    print(f'SMTP host: {smtp_host}')",
    "    print(f'SMTP port: {smtp_port}')",
    "    print('SMTP user: SMTP_USER')",
    "    print(f'Recipients: {\", \".join(recipients)}')",
    "    print(f'Attachment: {attachment}')",
    "",
    "    if smtp_port == 465:",
    "        context = ssl.create_default_context()",
    "        with smtplib.SMTP_SSL(smtp_host, smtp_port, timeout=90, context=context) as server:",
    "            server.login(smtp_user, smtp_pass)",
    "            server.send_message(msg, from_addr=alert_from, to_addrs=recipients)",
    "    else:",
    "        context = ssl.create_default_context()",
    "        with smtplib.SMTP(smtp_host, smtp_port, timeout=90) as server:",
    "            server.ehlo()",
    "            server.starttls(context=context)",
    "            server.ehlo()",
    "            server.login(smtp_user, smtp_pass)",
    "            server.send_message(msg, from_addr=alert_from, to_addrs=recipients)",
    "",
    "    print('PYTHON_SMTP_EMAIL_SENT')",
    "    return 0",
    "",
    "if __name__ == '__main__':",
    "    try:",
    "        sys.exit(main())",
    "    except Exception as e:",
    "        print('PYTHON_SMTP_FATAL:', redact(repr(e)), file=sys.stderr)",
    "        print(redact(traceback.format_exc()), file=sys.stderr)",
    "        sys.exit(2)"
  )

  writeLines(py_lines, py_file, useBytes = TRUE)
}

preis_send_email <- function(
    subject,
    body,
    attachment = NULL,
    from = preis_env("ALERT_FROM"),
    to = preis_split_emails(preis_env("ALERT_TO")),
    cc = preis_split_emails(preis_env("ALERT_CC")),
    bcc = preis_split_emails(preis_env("ALERT_BCC")),
    smtp_user = preis_env("SMTP_USER"),
    smtp_pass = preis_env("SMTP_PASS"),
    smtp_host = preis_env("SMTP_HOST"),
    smtp_port = suppressWarnings(as.integer(preis_env("SMTP_PORT", "465")))
) {

  if (!nzchar(from)) stop("ALERT_FROM est vide.", call. = FALSE)
  if (length(to) == 0) stop("ALERT_TO est vide.", call. = FALSE)
  if (!nzchar(smtp_user)) stop("SMTP_USER est vide.", call. = FALSE)
  if (!nzchar(smtp_pass)) stop("SMTP_PASS est vide.", call. = FALSE)
  if (!nzchar(smtp_host)) stop("SMTP_HOST est vide.", call. = FALSE)

  if (is.na(smtp_port)) {
    smtp_port <- 465L
  }

  if (is.null(attachment) || length(attachment) == 0) {
    stop("Pièce jointe PDF manquante.", call. = FALSE)
  }

  attachment <- as.character(attachment[1])

  if (!file.exists(attachment)) {
    stop("Pièce jointe introuvable: ", attachment, call. = FALSE)
  }

  preis_validate_emails(from, "ALERT_FROM")
  preis_validate_emails(to, "ALERT_TO")
  preis_validate_emails(cc, "ALERT_CC")
  preis_validate_emails(bcc, "ALERT_BCC")

  body_file <- tempfile(pattern = "preis_email_body_", fileext = ".txt")
  py_file <- tempfile(pattern = "preis_smtp_sender_", fileext = ".py")
  config_file <- tempfile(pattern = "preis_smtp_config_", fileext = ".txt")

  # PREIS_EMAIL_BODY_IMPROVED_V1
  body <- enc2utf8(body)

  if (grepl("A new DRC Ebola INSP SitRep has been detected by PREIS", body, fixed = TRUE)) {

    body <- gsub(
      "A new DRC Ebola INSP SitRep has been detected by PREIS\\.",
      "PREIS has detected a new Ebola Virus Disease SitRep published by INSP DRC.",
      body
    )

    body <- gsub(
      "Title:\\s*\\nINSP page:",
      "Title: Not available on the INSP page\\nINSP page:",
      body,
      perl = TRUE
    )

    body <- gsub(
      "The PDF is attached as received from INSP\\.",
      "The official PDF is attached exactly as received from INSP.",
      body
    )

    body <- gsub(
      "Analytical outputs will follow once generated and validated\\.",
      "Automated analytical outputs will follow once generated and validated.",
      body
    )

    body <- gsub(
      "Best regards,\\s*\\nPREIS Ebola DRC Automation\\s*\\.?\\s*$",
      "Best regards,\\nPREIS Ebola DRC Automation\\n\\nFor urgent follow-up, please contact Dr Hyacinthe Zabré on WhatsApp: +226 78 08 87 70.",
      body,
      perl = TRUE
    )

    if (!grepl("+226 78 08 87 70", body, fixed = TRUE)) {
      body <- paste0(
        body,
        "\\n\\nFor urgent follow-up, please contact Dr Hyacinthe Zabré on WhatsApp: +226 78 08 87 70."
      )
    }
  }

  writeLines(body, body_file, useBytes = TRUE)
  preis_write_python_sender(py_file)

  preis_write_hex_config(
    config_file,
    list(
      SMTP_HOST = smtp_host,
      SMTP_PORT = as.character(as.integer(smtp_port)),
      SMTP_USER = smtp_user,
      SMTP_PASS = smtp_pass,
      ALERT_FROM = from,
      ALERT_TO = paste(to, collapse = ";"),
      ALERT_CC = paste(cc, collapse = ";"),
      ALERT_BCC = paste(bcc, collapse = ";"),
      PREIS_EMAIL_SUBJECT = subject,
      PREIS_EMAIL_BODY_FILE = body_file,
      PREIS_EMAIL_ATTACHMENT = normalizePath(attachment, mustWork = TRUE)
    )
  )

  py <- preis_find_python()

  output <- tryCatch(
    system2(
      py,
      args = c(py_file, config_file),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) {
      structure(conditionMessage(e), status = 99)
    }
  )

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0
  }

  safe_output <- preis_redact(output, smtp_user, smtp_pass)

  if (nzchar(safe_output)) {
    message(safe_output)
  }

  if (!identical(as.integer(status), 0L)) {
    stop("Erreur envoi SMTP Python. Sortie:\n", safe_output, call. = FALSE)
  }

  invisible(TRUE)
}
