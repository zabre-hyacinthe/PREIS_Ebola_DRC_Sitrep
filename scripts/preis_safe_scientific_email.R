############################################################
# PREIS safe scientific SitRep email
# Robust text-only email layer for INSP SitRep alerts
# Supports SMTP STARTTLS 587 and SMTP_SSL 465
############################################################

log_msg <- function(...) {
  ts <- format(Sys.time(), '%Y-%m-%d %H:%M:%S UTC', tz = 'UTC')
  cat('[', ts, '] ', paste0(...), '\n', sep = '')
}

env_get <- function(names, default = '') {
  for (nm in names) {
    val <- Sys.getenv(nm, unset = '')
    if (!is.na(val) && nzchar(trimws(val))) return(trimws(val))
  }
  default
}

truthy <- function(x) {
  tolower(trimws(as.character(x))) %in% c('true', '1', 'yes', 'y', 'oui')
}

safe_url_decode <- function(x) {
  tryCatch(utils::URLdecode(x), error = function(e) x)
}

html_decode_basic <- function(x) {
  x <- gsub('&amp;', '&', x, fixed = TRUE)
  x <- gsub('&quot;', '"', x, fixed = TRUE)
  x <- gsub('&#039;', "'", x, fixed = TRUE)
  x <- gsub('&apos;', "'", x, fixed = TRUE)
  x <- gsub('&lt;', '<', x, fixed = TRUE)
  x <- gsub('&gt;', '>', x, fixed = TRUE)
  x
}

read_url_text <- function(url) {
  tmp <- tempfile(fileext = '.html')
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    utils::download.file(
      url = url,
      destfile = tmp,
      quiet = TRUE,
      method = 'libcurl',
      mode = 'wb',
      headers = c(
        'User-Agent' = 'Mozilla/5.0 PREIS-Ebola-DRC-Monitor',
        'Accept' = 'text/html,application/xhtml+xml,application/xml,*/*'
      )
    )
    TRUE
  }, warning = function(w) {
    log_msg('WARNING: download warning for ', url, ' | ', conditionMessage(w))
    FALSE
  }, error = function(e) {
    log_msg('WARNING: download failed for ', url, ' | ', conditionMessage(e))
    FALSE
  })
  if (!ok || !file.exists(tmp)) return('')
  paste(readLines(tmp, warn = FALSE, encoding = 'UTF-8'), collapse = '\n')
}

extract_links <- function(html) {
  if (!nzchar(html)) return(character(0))
  pattern <- "href=[\\\"']([^\\\"']+)[\\\"']"
  m <- gregexpr(pattern, html, perl = TRUE)
  raw <- regmatches(html, m)[[1]]
  if (length(raw) == 0 || identical(raw, character(0))) return(character(0))
  links <- sub(pattern, '\\1', raw, perl = TRUE)
  links <- html_decode_basic(links)
  unique(links[nzchar(links)])
}

extract_pdf_like_urls <- function(html) {
  if (!nzchar(html)) return(character(0))
  html <- html_decode_basic(html)
  html <- gsub('\\\\/', '/', html)
  patterns <- c(
    'https?://[^"\\\'<>[:space:]]+\\.pdf[^"\\\'<>[:space:]]*',
    '/wp-content/uploads/[^"\\\'<>[:space:]]+\\.pdf[^"\\\'<>[:space:]]*',
    'wp-content/uploads/[^"\\\'<>[:space:]]+\\.pdf[^"\\\'<>[:space:]]*'
  )
  out <- character(0)
  for (pat in patterns) {
    m <- gregexpr(pat, html, perl = TRUE, ignore.case = TRUE)
    r <- regmatches(html, m)[[1]]
    if (length(r) > 0 && !identical(r, character(0))) out <- c(out, r)
  }
  out <- unique(out)
  out[nzchar(out)]
}

abs_url <- function(x, base = 'https://insp.cd') {
  x <- trimws(x)
  x <- sub('#.*$', '', x)
  x <- safe_url_decode(x)
  x <- gsub('NAÂ°', 'N°', x, fixed = TRUE)
  x <- gsub('NÂ°', 'N°', x, fixed = TRUE)
  if (!nzchar(x)) return(NA_character_)
  if (grepl('^https?://', x, ignore.case = TRUE)) return(x)
  if (startsWith(x, '//')) return(paste0('https:', x))
  if (startsWith(x, '/')) return(paste0(base, x))
  paste0(base, '/', x)
}

extract_sitrep_number <- function(x) {
  x <- safe_url_decode(enc2utf8(as.character(x)))
  x <- gsub('NAÂ°', 'N°', x, fixed = TRUE)
  x <- gsub('NÂ°', 'N°', x, fixed = TRUE)
  x <- tolower(x)
  patterns <- c(
    'sitrep[-_ ]*n[^0-9]*0*([0-9]{1,3})',
    'sitrep[^0-9]{0,20}0*([0-9]{1,3})',
    'n[°ºo]?[^0-9]*0*([0-9]{1,3})'
  )
  for (pat in patterns) {
    m <- regexec(pat, x, perl = TRUE)
    r <- regmatches(x, m)[[1]]
    if (length(r) >= 2) return(as.integer(r[2]))
  }
  NA_integer_
}

find_latest_sitrep <- function(max_pages = 5) {
  urls <- c('https://insp.cd/category/sitrep/')
  if (max_pages > 1) {
    urls <- c(urls, paste0('https://insp.cd/category/sitrep/page/', 2:max_pages, '/'))
  }
  candidates <- data.frame(number = integer(), page_url = character(), stringsAsFactors = FALSE)
  for (u in urls) {
    html <- read_url_text(u)
    if (!nzchar(html)) next
    links <- extract_links(html)
    links <- vapply(links, abs_url, character(1))
    links <- unique(links[grepl('/sitrep', links, ignore.case = TRUE)])
    if (length(links) == 0) next
    nums <- vapply(links, extract_sitrep_number, integer(1))
    keep <- !is.na(nums)
    if (any(keep)) {
      candidates <- rbind(candidates, data.frame(number = nums[keep], page_url = links[keep], stringsAsFactors = FALSE))
    }
  }
  if (nrow(candidates) == 0) stop('Aucun SitRep trouve sur INSP.')
  candidates <- candidates[order(candidates$number, decreasing = TRUE), ]
  candidates <- candidates[!duplicated(candidates$number), ]
  candidates[1, ]
}

find_pdf_url <- function(page_url) {
  html <- read_url_text(page_url)
  if (!nzchar(html)) return('')
  links_href <- extract_links(html)
  links_raw <- extract_pdf_like_urls(html)
  links <- unique(c(links_href, links_raw))
  links <- vapply(links, abs_url, character(1))
  links <- unique(links[grepl('\\.pdf($|[?#])|\\.pdf', links, ignore.case = TRUE)])
  links <- links[!is.na(links) & nzchar(links)]
  if (length(links) == 0) return('')
  links[1]
}

write_secret_file <- function(dir, name, value) {
  path <- file.path(dir, name)
  writeLines(enc2utf8(as.character(value)), path, useBytes = TRUE)
  path
}

state_file <- file.path(getwd(), 'data', 'preis_safe_email_notification_state.csv')
force_send <- truthy(env_get(c('PREIS_FORCE_SEND', 'FORCE_SEND', 'INPUT_FORCE_SEND'), 'false'))

log_msg('PREIS safe scientific email layer started')
log_msg('force_send=', force_send)

latest <- find_latest_sitrep(max_pages = 5)
sitrep_number <- latest$number[1]
sitrep_label <- sprintf('N%03d', sitrep_number)
page_url <- latest$page_url[1]
pdf_url <- find_pdf_url(page_url)

log_msg('Latest SitRep detected by safe layer: ', sitrep_label)
log_msg('Page URL: ', page_url)
if (nzchar(pdf_url)) log_msg('PDF URL: ', pdf_url) else log_msg('PDF URL: not found')

state <- data.frame(sitrep_number = integer(), notified_utc = character(), stringsAsFactors = FALSE)
if (file.exists(state_file)) {
  state <- tryCatch(read.csv(state_file, stringsAsFactors = FALSE), error = function(e) state)
}

already <- nrow(state) > 0 && sitrep_number %in% suppressWarnings(as.integer(state$sitrep_number))
if (already && !force_send) {
  log_msg('Safe email skipped: ', sitrep_label, ' already notified and force_send=false')
  quit(save = 'no', status = 0)
}

smtp_host <- env_get(c('SMTP_HOST', 'EMAIL_SMTP_HOST', 'PREIS_SMTP_HOST'), 'smtp.gmail.com')
smtp_port <- env_get(c('SMTP_PORT', 'EMAIL_SMTP_PORT', 'PREIS_SMTP_PORT'), '587')
smtp_user <- env_get(c('SMTP_USERNAME', 'SMTP_USER', 'EMAIL_USER', 'GMAIL_USER', 'ALERT_EMAIL_USER', 'MAIL_USERNAME'))
smtp_pass <- env_get(c('SMTP_PASSWORD', 'SMTP_PASS', 'EMAIL_PASSWORD', 'GMAIL_APP_PASSWORD', 'ALERT_EMAIL_PASSWORD', 'MAIL_PASSWORD'))
email_from <- env_get(c('ALERT_FROM', 'EMAIL_FROM', 'PREIS_ALERT_FROM', 'SMTP_FROM', 'MAIL_FROM'), smtp_user)
email_to <- env_get(c('ALERT_TO', 'EMAIL_TO', 'PREIS_ALERT_TO', 'PREIS_EMAIL_TO', 'ALERT_RECIPIENTS', 'SMTP_TO', 'MAIL_TO'))

if (!nzchar(smtp_user)) stop('SMTP user missing')
if (!nzchar(smtp_pass)) stop('SMTP password missing')
if (!nzchar(email_from)) stop('Email from missing')
if (!nzchar(email_to)) stop('Email recipients missing')

recipients <- unique(trimws(unlist(strsplit(email_to, '[,;]'))))
recipients <- recipients[nzchar(recipients)]
if (length(recipients) == 0) stop('Recipient list is empty')

repo <- env_get(c('GITHUB_REPOSITORY'), 'zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep')
run_id <- env_get(c('GITHUB_RUN_ID'), '')
run_url <- ''

subject <- paste0('[PREIS Ebola DRC] New SitRep detected - PREIS analysis and DSB - ', sitrep_label)

body_detailed <- paste(
  'PREIS Ebola DRC - Automated scientific SitRep alert',
  '',
  paste0('SitRep: ', sitrep_label),
  'Source: INSP DRC official SitRep page.',
  '',
  'PREIS update',
  '- A new Ebola DRC SitRep has been detected online.',
  '- PREIS automated analysis and DSB outputs are being generated/updated for operational review.',
  '- Detailed indicators should be verified against the official SitRep and PREIS outputs.',
  '',
  'Verification links',
  paste0('- Official INSP page: ', page_url),
  if (nzchar(pdf_url)) paste0('- SitRep PDF: ', pdf_url) else '- SitRep PDF: not automatically identified',
  '',
  'Recommended action',
  '- Review the official SitRep and PREIS outputs.',
  '- Update coordination actions if new epidemiological or operational signals are confirmed.',
  '',
  'Automation by PREIS.',
  'For support, contact Dr Hyacinthe ZABRE.',
  'PREIS WhatsApp contact: +226 78 08 87 70.',
  sep = '\n'
)

body_minimal <- paste(
  'PREIS Ebola DRC - Automated SitRep alert',
  '',
  paste0('SitRep: ', sitrep_label),
  'A new Ebola DRC SitRep has been detected by PREIS.',
  'PREIS analysis and DSB outputs are being generated/updated for operational review.',
  '',
  'Please review the official INSP SitRep and PREIS outputs.',
  '',
  'Automation by PREIS.',
  'For support, contact Dr Hyacinthe ZABRE.',
  'PREIS WhatsApp contact: +226 78 08 87 70.',
  sep = '\n'
)

cfg_dir <- tempfile('preis_email_cfg_')
dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
write_secret_file(cfg_dir, 'smtp_host.txt', smtp_host)
write_secret_file(cfg_dir, 'smtp_port.txt', smtp_port)
write_secret_file(cfg_dir, 'smtp_user.txt', smtp_user)
write_secret_file(cfg_dir, 'smtp_pass.txt', smtp_pass)
write_secret_file(cfg_dir, 'email_from.txt', email_from)
write_secret_file(cfg_dir, 'email_to.txt', paste(recipients, collapse = ','))
write_secret_file(cfg_dir, 'subject.txt', subject)
write_secret_file(cfg_dir, 'body_detailed.txt', body_detailed)
write_secret_file(cfg_dir, 'body_minimal.txt', body_minimal)

py_file <- tempfile(fileext = '.py')
py_lines <- c(
  'import sys, smtplib, ssl, socket, time',
  'from pathlib import Path',
  'from email.message import EmailMessage',
  'cfg = Path(sys.argv[1])',
  'def read(name):',
  '    return (cfg / name).read_text(encoding="utf-8").strip()',
  'host = read("smtp_host.txt") or "smtp.gmail.com"',
  'port_raw = read("smtp_port.txt") or "587"',
  'try:',
  '    port = int(port_raw)',
  'except Exception:',
  '    port = 587',
  'user = read("smtp_user.txt")',
  'password = read("smtp_pass.txt")',
  'sender = read("email_from.txt")',
  'recipients = [x.strip() for x in read("email_to.txt").replace(";", ",").split(",") if x.strip()]',
  'subject = read("subject.txt")',
  'body_detailed = (cfg / "body_detailed.txt").read_text(encoding="utf-8")',
  'body_minimal = (cfg / "body_minimal.txt").read_text(encoding="utf-8")',
  'class ContentBlocked(Exception):',
  '    pass',
  'def add_candidate(cands, h, p, mode):',
  '    item = (str(h), int(p), str(mode))',
  '    if item not in cands:',
  '        cands.append(item)',
  'candidates = []',
  'if port == 465:',
  '    add_candidate(candidates, host, 465, "ssl")',
  'elif port == 587:',
  '    add_candidate(candidates, host, 587, "starttls")',
  'else:',
  '    add_candidate(candidates, host, port, "starttls")',
  'add_candidate(candidates, host, 587, "starttls")',
  'add_candidate(candidates, host, 465, "ssl")',
  'add_candidate(candidates, "smtp.gmail.com", 587, "starttls")',
  'add_candidate(candidates, "smtp.gmail.com", 465, "ssl")',
  'print("PREIS_SMTP_CANDIDATES: " + "; ".join([f"{h}:{p}/{m}" for h,p,m in candidates]), flush=True)',
  'def make_msg(body, tag):',
  '    msg = EmailMessage()',
  '    msg["From"] = sender',
  '    msg["To"] = ", ".join(recipients)',
  '    msg["Subject"] = subject if tag == "detailed" else subject + " [minimal]"',
  '    msg.set_content(body)',
  '    return msg',
  'def smtp_send_candidate(h, p, mode, msg):',
  '    ctx = ssl.create_default_context()',
  '    if mode == "ssl":',
  '        with smtplib.SMTP_SSL(h, p, timeout=120, context=ctx) as server:',
  '            server.ehlo()',
  '            server.login(user, password)',
  '            server.send_message(msg, from_addr=sender, to_addrs=recipients)',
  '    else:',
  '        with smtplib.SMTP(h, p, timeout=120) as server:',
  '            server.ehlo()',
  '            server.starttls(context=ctx)',
  '            server.ehlo()',
  '            server.login(user, password)',
  '            server.send_message(msg, from_addr=sender, to_addrs=recipients)',
  'def deliver(body, tag):',
  '    errors = []',
  '    msg = make_msg(body, tag)',
  '    for h, p, mode in candidates:',
  '        try:',
  '            print(f"PREIS_SMTP_TRY {tag}: {h}:{p}/{mode}", flush=True)',
  '            smtp_send_candidate(h, p, mode, msg)',
  '            print(f"PREIS_SAFE_EMAIL_SENT_OK {tag} via {h}:{p}/{mode}", flush=True)',
  '            return True',
  '        except smtplib.SMTPDataError as e:',
  '            code = getattr(e, "smtp_code", None)',
  '            err = str(getattr(e, "smtp_error", b""))',
  '            errors.append(f"{h}:{p}/{mode} SMTPDataError {code} {err[:180]}")',
  '            if code == 552 or "5.7.0" in err or "security issue" in err.lower():',
  '                raise ContentBlocked(f"Gmail blocked {tag}: {code} {err[:200]}")',
  '        except (smtplib.SMTPServerDisconnected, smtplib.SMTPConnectError, smtplib.SMTPHeloError, smtplib.SMTPAuthenticationError, socket.timeout, OSError) as e:',
  '            errors.append(f"{h}:{p}/{mode} {type(e).__name__}: {str(e)[:180]}")',
  '            time.sleep(1)',
  '        except Exception as e:',
  '            errors.append(f"{h}:{p}/{mode} {type(e).__name__}: {str(e)[:180]}")',
  '            time.sleep(1)',
  '    raise RuntimeError("All SMTP candidates failed for " + tag + ": " + " | ".join(errors))',
  'try:',
  '    deliver(body_detailed, "detailed")',
  'except ContentBlocked as e:',
  '    print("PREIS_SAFE_EMAIL_DETAILED_BLOCKED: " + str(e), flush=True)',
  '    deliver(body_minimal, "minimal")',
  'except Exception as e:',
  '    print("PREIS_SAFE_EMAIL_DETAILED_FAILED: " + repr(e), flush=True)',
  '    raise'
)
writeLines(py_lines, py_file, useBytes = TRUE)

py <- Sys.which('python3')
if (!nzchar(py)) py <- Sys.which('python')
if (!nzchar(py)) stop('Python not found on runner')

log_msg('SMTP host configured: ', smtp_host)
log_msg('SMTP port configured: ', smtp_port)
log_msg('Sending safe scientific email to ', paste(recipients, collapse = ', '))

res <- tryCatch(
  system2(py, args = c(py_file, cfg_dir), stdout = TRUE, stderr = TRUE),
  error = function(e) {
    paste0('R_SYSTEM2_ERROR: ', conditionMessage(e))
  }
)

cat(paste(res, collapse = '\n'), '\n')

if (!any(grepl('PREIS_SAFE_EMAIL_SENT_OK', res, fixed = TRUE))) {
  stop('Safe scientific email failed. Output: ', paste(res, collapse = '\n'))
}

new_row <- data.frame(
  sitrep_number = sitrep_number,
  notified_utc = format(Sys.time(), '%Y-%m-%d %H:%M:%S UTC', tz = 'UTC'),
  stringsAsFactors = FALSE
)
state <- state[!(suppressWarnings(as.integer(state$sitrep_number)) == sitrep_number), , drop = FALSE]
state <- rbind(state, new_row)
dir.create(dirname(state_file), recursive = TRUE, showWarnings = FALSE)
write.csv(state, state_file, row.names = FALSE)

log_msg('PREIS safe scientific email sent and state updated for ', sitrep_label)
quit(save = 'no', status = 0)
