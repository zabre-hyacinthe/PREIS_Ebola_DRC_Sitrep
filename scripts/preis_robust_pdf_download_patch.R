############################################################
# PREIS robust PDF download helper
# Robust against INSP URL encoding issues and temporary PDF failures
############################################################

preis_safe_url_decode <- function(x) {
  tryCatch(utils::URLdecode(x), error = function(e) x)
}

preis_fix_pdf_url_mojibake <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub('NAÂ°', 'N°', x, fixed = TRUE)
  x <- gsub('NÂ°', 'N°', x, fixed = TRUE)
  x <- gsub('Â°', '°', x, fixed = TRUE)
  x <- gsub('NA%C3%82%C2%B0', 'N%C2%B0', x, ignore.case = TRUE)
  x <- gsub('N%C3%82%C2%B0', 'N%C2%B0', x, ignore.case = TRUE)
  x <- gsub('%C3%82%C2%B0', '%C2%B0', x, ignore.case = TRUE)
  x <- gsub('%C3%82%B0', '%C2%B0', x, ignore.case = TRUE)
  x <- gsub('%C2%BA', '%C2%B0', x, ignore.case = TRUE)
  x
}

preis_encode_url_path <- function(u) {
  u <- preis_fix_pdf_url_mojibake(u)
  m <- regexpr('^[A-Za-z][A-Za-z0-9+.-]*://[^/?#]+', u, perl = TRUE)
  if (is.na(m[1]) || m[1] < 0) {
    return(utils::URLencode(preis_safe_url_decode(u), reserved = TRUE))
  }
  prefix <- substr(u, 1, attr(m, 'match.length'))
  rest <- substring(u, attr(m, 'match.length') + 1)
  fragment <- ''
  query <- ''
  if (grepl('#', rest, fixed = TRUE)) {
    sp <- strsplit(rest, '#', fixed = TRUE)[[1]]
    rest <- sp[1]
    fragment <- paste0('#', paste(sp[-1], collapse = '#'))
  }
  if (grepl('?', rest, fixed = TRUE)) {
    sp <- strsplit(rest, '?', fixed = TRUE)[[1]]
    rest <- sp[1]
    query <- paste0('?', paste(sp[-1], collapse = '?'))
  }
  segments <- strsplit(rest, '/', fixed = TRUE)[[1]]
  encoded_segments <- vapply(
    segments,
    function(s) utils::URLencode(preis_safe_url_decode(s), reserved = TRUE),
    character(1)
  )
  paste0(prefix, paste(encoded_segments, collapse = '/'), query, fragment)
}

preis_pdf_url_candidates <- function(url) {
  u0 <- enc2utf8(as.character(url))
  u1 <- preis_fix_pdf_url_mojibake(u0)
  u2 <- preis_safe_url_decode(u0)
  u3 <- preis_fix_pdf_url_mojibake(u2)
  base <- unique(c(u0, u1, u2, u3))
  variants <- unique(c(
    base,
    gsub('NA°', 'N°', base, fixed = TRUE),
    gsub('NAÂ°', 'N°', base, fixed = TRUE),
    gsub('NÂ°', 'N°', base, fixed = TRUE),
    gsub('N°', 'NA°', base, fixed = TRUE),
    gsub('N°', 'Nº', base, fixed = TRUE),
    gsub('°', '%C2%B0', base, fixed = TRUE),
    gsub('º', '%C2%BA', base, fixed = TRUE),
    gsub('%C2%B0', '°', base, fixed = TRUE),
    gsub('%C2%BA', 'º', base, fixed = TRUE)
  ))
  encoded <- unique(vapply(variants, preis_encode_url_path, character(1)))
  unique(c(variants, encoded))
}

preis_is_pdf_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sz <- suppressWarnings(file.info(path)$size)
  if (is.na(sz) || sz < 500) return(FALSE)
  con <- file(path, 'rb')
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = 'raw', n = 4)
  identical(rawToChar(sig), '%PDF')
}

preis_try_download_file <- function(url, destfile, quiet = FALSE, mode = 'wb', method = 'libcurl', ...) {
  candidates <- preis_pdf_url_candidates(url)
  last_error <- NULL
  expects_pdf <- grepl('\\.pdf($|[?#])', url, ignore.case = TRUE) || grepl('\\.pdf$', destfile, ignore.case = TRUE)
  for (candidate in candidates) {
    tmp <- paste0(destfile, '.tmp')
    if (file.exists(tmp)) unlink(tmp)
    message('[PREIS DOWNLOAD] Trying: ', candidate)
    ok <- FALSE
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url = candidate,
        destfile = tmp,
        mode = mode,
        quiet = TRUE,
        method = 'libcurl',
        headers = c(
          'User-Agent' = 'Mozilla/5.0 PREIS-Ebola-DRC-SitRep-Monitor',
          'Accept' = 'application/pdf,application/octet-stream,*/*',
          'Referer' = 'https://insp.cd/'
        )
      )),
      error = function(e) {
        last_error <<- conditionMessage(e)
        1L
      }
    )
    ok_status <- identical(status, 0L) || identical(status, 0)
    ok_file <- file.exists(tmp) && !is.na(file.info(tmp)$size) && file.info(tmp)$size > 0
    ok_pdf <- TRUE
    if (expects_pdf) ok_pdf <- preis_is_pdf_file(tmp)
    ok <- ok_status && ok_file && ok_pdf
    if (!ok && nzchar(Sys.which('curl'))) {
      if (file.exists(tmp)) unlink(tmp)
      cmd <- Sys.which('curl')
      args <- c(
        '-L',
        '--fail',
        '--retry', '3',
        '--connect-timeout', '30',
        '--max-time', '180',
        '-A', 'Mozilla/5.0 PREIS-Ebola-DRC-SitRep-Monitor',
        '-e', 'https://insp.cd/',
        '-o', tmp,
        candidate
      )
      status2 <- tryCatch(system2(cmd, args = args, stdout = TRUE, stderr = TRUE), error = function(e) {
        last_error <<- conditionMessage(e)
        character(0)
      })
      ok_file <- file.exists(tmp) && !is.na(file.info(tmp)$size) && file.info(tmp)$size > 0
      ok_pdf <- TRUE
      if (expects_pdf) ok_pdf <- preis_is_pdf_file(tmp)
      ok <- ok_file && ok_pdf
    }
    if (ok) {
      file.copy(tmp, destfile, overwrite = TRUE)
      unlink(tmp)
      message('[PREIS DOWNLOAD] OK: ', candidate)
      return(0L)
    }
    if (file.exists(tmp)) unlink(tmp)
  }
  warning('All PDF download candidates failed for: ', url, if (!is.null(last_error)) paste0(' | last error: ', last_error) else '')
  1L
}

# === PREIS SMTP Gmail blocked fallback START ===

preis_patch_smtp_python_lines <- function(text) {
  if (!is.character(text)) {
    return(text)
  }

  target <- "server.send_message(msg, from_addr=alert_from, to_addrs=recipients)"

  if (!any(grepl(target, text, fixed = TRUE))) {
    return(text)
  }

  if (any(grepl("PYTHON_SMTP_RETRY_NO_ATTACHMENT", text, fixed = TRUE))) {
    return(text)
  }

  idx <- grep(target, text, fixed = TRUE)

  for (i in rev(idx)) {
    indent <- sub("^(\\s*).*$", "\\1", text[i])

    replacement <- c(
      paste0(indent, "import smtplib as _preis_smtplib"),
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
      paste0(indent, "    print('PYTHON_SMTP_RETRY_NO_ATTACHMENT: Gmail blocked original message; retrying safe text-only alert.', flush=True)"),
      paste0(indent, "    from email.message import EmailMessage as _PreisEmailMessage"),
      paste0(indent, "    _fallback = _PreisEmailMessage()"),
      paste0(indent, "    _fallback['From'] = msg.get('From', alert_from)"),
      paste0(indent, "    _fallback['To'] = msg.get('To', ', '.join(recipients) if isinstance(recipients, (list, tuple)) else str(recipients))"),
      paste0(indent, "    _subject = str(msg.get('Subject', 'PREIS Ebola DRC SitRep alert'))"),
      paste0(indent, "    _subject = ''.join([ch for ch in _subject if ord(ch) < 128]).strip()"),
      paste0(indent, "    if not _subject:"),
      paste0(indent, "        _subject = 'PREIS Ebola DRC SitRep alert'"),
      paste0(indent, "    _fallback['Subject'] = '[PREIS SAFE ALERT] ' + _subject[:120]"),
      paste0(indent, "    _safe_body = 'PREIS Ebola DRC SitRep alert generated.\\n\\nThe original automated message or attachment was blocked by Gmail as a potential security issue.\\nThis fallback email is text-only, without attachment and without links.\\n\\nPlease open PREIS GitHub Actions or the PREIS dashboard to review the full SitRep and outputs.'"),
      paste0(indent, "    _fallback.set_content(_safe_body)"),
      paste0(indent, "    server.send_message(_fallback, from_addr=alert_from, to_addrs=recipients)"),
      paste0(indent, "    print('PYTHON_SMTP_FALLBACK_OK: text-only alert sent without attachments.', flush=True)")
    )

    before <- if (i > 1) text[1:(i - 1)] else character(0)
    after <- if (i < length(text)) text[(i + 1):length(text)] else character(0)
    text <- c(before, replacement, after)
  }

  message("[PREIS SMTP PATCH] Python SMTP sender patched with Gmail blocked fallback")
  text
}

writeLines <- function(text, con = stdout(), sep = "\n", useBytes = FALSE) {
  if (is.character(text) && any(grepl("server.send_message(msg, from_addr=alert_from, to_addrs=recipients)", text, fixed = TRUE))) {
    text <- preis_patch_smtp_python_lines(text)
  }

  base::writeLines(text, con = con, sep = sep, useBytes = useBytes)
}

# === PREIS SMTP Gmail blocked fallback END ===
