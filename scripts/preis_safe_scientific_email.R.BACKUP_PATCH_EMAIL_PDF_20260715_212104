############################################################

############################################################
# PREIS UNIVERSAL INSP PDF RESOLVER
# This block resolves and downloads any PDF linked or embedded
# in an INSP SitRep article page, regardless of filename.
############################################################

preis_null_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x
}

preis_safe_trim <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[nzchar(x)]
}

preis_safe_unique <- function(x) {
  x <- preis_safe_trim(x)
  unique(x[!is.na(x) & nzchar(x)])
}

preis_safe_http_get_text <- function(url, timeout_sec = 90) {
  resp <- tryCatch(
    httr::GET(
      url,
      httr::timeout(timeout_sec),
      httr::add_headers(
        `User-Agent` = 'Mozilla/5.0 PREIS-Ebola-DRC-PDF-Resolver',
        Accept = 'text/html,application/xhtml+xml,application/pdf,*/*',
        `Accept-Language` = 'fr-FR,fr;q=0.9,en;q=0.8'
      )
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_character_)
  if (httr::status_code(resp) < 200 || httr::status_code(resp) >= 400) return(NA_character_)
  httr::content(resp, as = 'text', encoding = 'UTF-8')
}

preis_safe_url_absolute <- function(urls, base_url) {
  urls <- preis_safe_unique(urls)
  if (length(urls) == 0) return(character())
  out <- vapply(urls, function(u) {
    tryCatch(xml2::url_absolute(u, base_url), error = function(e) NA_character_)
  }, character(1))
  preis_safe_unique(out)
}

preis_safe_clean_pdf_url <- function(urls, base_url = 'https://insp.cd') {
  urls <- preis_safe_unique(urls)
  if (length(urls) == 0) return(character())
  urls <- gsub('\\\\/', '/', urls)
  urls <- gsub('&amp;', '&', urls, fixed = TRUE)
  urls <- gsub('\\u0026', '&', urls, fixed = TRUE)
  urls <- gsub('\\u003d', '=', urls, fixed = TRUE)
  urls <- gsub('\\u003a', ':', urls, fixed = TRUE)
  urls <- gsub('\\u002f', '/', urls, fixed = TRUE)
  urls <- gsub('\\u00b0', '°', urls, fixed = TRUE)
  urls <- gsub('%5C/', '/', urls, fixed = TRUE)
  urls <- utils::URLdecode(urls)
  urls <- preis_safe_url_absolute(urls, base_url)
  urls <- urls[grepl('[.]pdf($|[?#])', urls, ignore.case = TRUE)]
  preis_safe_unique(urls)
}

preis_safe_extract_pdf_urls_from_html <- function(html_text, page_url) {
  if (is.na(html_text) || !nzchar(html_text)) return(character())
  urls <- character()
  
  # 1) Raw URLs ending in .pdf
  raw_pdf <- stringr::str_extract_all(
    html_text,
    'https?://[^\"\'<>[:space:]]+[.]pdf(?:[?#][^\"\'<>[:space:]]*)?'
  )[[1]]
  urls <- c(urls, raw_pdf)
  
  # 2) Encoded / escaped URLs that still contain .pdf
  encoded_pdf <- stringr::str_extract_all(
    html_text,
    'https?%3A%2F%2F[^\"\'<>[:space:]]+[.]pdf(?:[?#][^\"\'<>[:space:]]*)?'
  )[[1]]
  urls <- c(urls, encoded_pdf)
  
  # 3) Parse HTML attributes href/src/data/pdfemb-url
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (!is.null(doc)) {
    nodes <- rvest::html_nodes(doc, 'a, iframe, embed, object, source')
    attrs <- c(
      rvest::html_attr(nodes, 'href'),
      rvest::html_attr(nodes, 'src'),
      rvest::html_attr(nodes, 'data'),
      rvest::html_attr(nodes, 'data-pdf'),
      rvest::html_attr(nodes, 'data-url'),
      rvest::html_attr(nodes, 'pdfemb-url')
    )
    urls <- c(urls, attrs)
  }
  
  # 4) PDF Embedder plugin: pdfemb-data base64
  b64_values <- stringr::str_match_all(html_text, 'pdfemb-data=[\"\']?([A-Za-z0-9+/=]+)')[[1]]
  if (!is.null(b64_values) && nrow(b64_values) > 0) {
    for (b64 in b64_values[, 2]) {
      decoded <- tryCatch(rawToChar(base64enc::base64decode(b64)), error = function(e) NA_character_)
      if (!is.na(decoded) && nzchar(decoded)) {
        pdf_inside <- stringr::str_extract_all(
          decoded,
          'https?://[^\"\'<>[:space:]]+[.]pdf(?:[?#][^\"\'<>[:space:]]*)?'
        )[[1]]
        urls <- c(urls, pdf_inside)
      }
    }
  }
  
  preis_safe_clean_pdf_url(urls, base_url = page_url)
}

preis_safe_pdf_signature_ok <- function(file) {
  if (!file.exists(file)) return(FALSE)
  if (file.info(file)$size < 5000) return(FALSE)
  con <- file(file, 'rb')
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = 'raw', n = 4)
  identical(sig, charToRaw('%PDF'))
}

preis_safe_url_variants <- function(url) {
  url <- as.character(url[1])
  out <- c(url)
  encoded <- tryCatch(utils::URLencode(url, reserved = TRUE), error = function(e) url)
  encoded <- gsub('%3A', ':', encoded, fixed = TRUE)
  encoded <- gsub('%2F', '/', encoded, fixed = TRUE)
  encoded <- gsub('%3F', '?', encoded, fixed = TRUE)
  encoded <- gsub('%3D', '=', encoded, fixed = TRUE)
  encoded <- gsub('%26', '&', encoded, fixed = TRUE)
  out <- c(out, encoded)
  out <- c(out, gsub('°', '%C2%B0', url, fixed = TRUE))
  preis_safe_unique(out)
}

preis_safe_download_pdf_from_url <- function(pdf_url, dest_file, referer = 'https://insp.cd/category/sitrep/') {
  urls <- preis_safe_url_variants(pdf_url)
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
  
  for (u in urls) {
    tmp <- tempfile(fileext = '.pdf')
    resp <- tryCatch(
      httr::GET(
        u,
        httr::timeout(180),
        httr::write_disk(tmp, overwrite = TRUE),
        httr::add_headers(
          `User-Agent` = 'Mozilla/5.0 PREIS-Ebola-DRC-PDF-Download',
          Referer = referer,
          Accept = 'application/pdf,*/*'
        )
      ),
      error = function(e) NULL
    )
    
    if (!is.null(resp)) {
      code <- httr::status_code(resp)
      if (code >= 200 && code < 400 && preis_safe_pdf_signature_ok(tmp)) {
        file.copy(tmp, dest_file, overwrite = TRUE)
        unlink(tmp, force = TRUE)
        return(normalizePath(dest_file, mustWork = TRUE))
      }
    }
    unlink(tmp, force = TRUE)
  }
  
  NA_character_
}

preis_safe_generate_pdf_candidates_from_context <- function(sitrep_no, page_url, title = '') {
  n_int <- suppressWarnings(as.integer(sitrep_no))
  if (is.na(n_int)) return(character())
  n1 <- as.character(n_int)
  n2 <- sprintf('%02d', n_int)
  n3 <- sprintf('%03d', n_int)
  
  context <- paste(page_url, title, collapse = ' ')
  m <- stringr::str_match(context, '(\\d{2})[-_/](\\d{2})[-_/](\\d{4})')
  if (all(is.na(m))) return(character())
  dd <- m[2]
  mm <- m[3]
  yyyy <- m[4]
  date_dash <- paste(dd, mm, yyyy, sep = '-')
  date_us <- paste(dd, mm, yyyy, sep = '_')
  date_slash <- paste(dd, mm, yyyy, sep = '/')
  date_compact <- paste0(dd, mm, yyyy)
  
  base <- paste0('https://insp.cd/wp-content/uploads/', yyyy, '/', mm, '/')
  nums <- c(n1, n2, n3)
  dates <- c(date_dash, date_us, date_compact)
  prefixes <- c(
    'SitRep_MVE_RDC_N°',
    'SitRep_MVE_RDC_N',
    'Draft_SitRep_MVE_RDC_N°',
    'Draft_SitRep_MVE_RDC_N',
    'Draft_SitRep_MVB_RDC_N°',
    'Draft_SitRep_MVB_RDC_N',
    'SitRep_MVB_RDC_N°',
    'SitRep_MVB_RDC_N'
  )
  
  names <- character()
  for (p in prefixes) {
    for (n in nums) {
      for (d in dates) {
        names <- c(names, paste0(p, n, '_', d, '.pdf'))
      }
    }
  }
  
  urls <- paste0(base, names)
  urls <- vapply(urls, function(u) {
    e <- utils::URLencode(u, reserved = TRUE)
    e <- gsub('%3A', ':', e, fixed = TRUE)
    e <- gsub('%2F', '/', e, fixed = TRUE)
    e
  }, character(1))
  preis_safe_unique(urls)
}

preis_safe_resolve_pdf_universal <- function(sitrep_no, page_url, current_pdf_url = '', title = '') {
  candidates <- character()
  
  if (!is.na(current_pdf_url) && nzchar(current_pdf_url)) {
    candidates <- c(candidates, current_pdf_url)
  }
  
  html_text <- preis_safe_http_get_text(page_url)
  candidates <- c(candidates, preis_safe_extract_pdf_urls_from_html(html_text, page_url))
  
  # Follow internal article links that may hide the real PDF or attachment page
  if (!is.na(html_text) && nzchar(html_text)) {
    doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
    if (!is.null(doc)) {
      links <- rvest::html_attr(rvest::html_nodes(doc, 'a'), 'href')
      links <- preis_safe_url_absolute(links, page_url)
      links <- links[grepl('wp-content|attachment|download|pdf|media', links, ignore.case = TRUE)]
      links <- links[!grepl('[.]jpg|[.]jpeg|[.]png|[.]gif|[.]webp|[.]css|[.]js', links, ignore.case = TRUE)]
      links <- head(preis_safe_unique(links), 20)
      for (lnk in links) {
        if (grepl('[.]pdf($|[?#])', lnk, ignore.case = TRUE)) {
          candidates <- c(candidates, lnk)
        } else {
          sub_html <- preis_safe_http_get_text(lnk, timeout_sec = 45)
          candidates <- c(candidates, preis_safe_extract_pdf_urls_from_html(sub_html, lnk))
        }
      }
    }
  }
  
  # Last-resort candidates from known INSP upload patterns
  candidates <- c(candidates, preis_safe_generate_pdf_candidates_from_context(sitrep_no, page_url, title))
  candidates <- preis_safe_clean_pdf_url(candidates, page_url)
  
  if (length(candidates) == 0) return('')
  
  # Test candidates by real PDF signature after download
  tmp_dir <- file.path(getwd(), 'data', 'pdf_probe')
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (u in candidates) {
    probe_file <- tempfile(pattern = 'preis_pdf_probe_', fileext = '.pdf', tmpdir = tmp_dir)
    got <- preis_safe_download_pdf_from_url(u, probe_file, referer = page_url)
    if (!is.na(got) && nzchar(got) && file.exists(got) && preis_safe_pdf_signature_ok(got)) {
      unlink(got, force = TRUE)
      return(u)
    }
    unlink(probe_file, force = TRUE)
  }
  
  ''
}

preis_safe_download_resolved_pdf <- function(pdf_url, sitrep_no, page_url) {
  if (is.na(pdf_url) || !nzchar(pdf_url)) return(NA_character_)
  if (!grepl('[.]pdf($|[?#])', pdf_url, ignore.case = TRUE)) return(NA_character_)
  pdf_dir <- file.path(getwd(), 'data', 'pdf')
  dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
  dest_file <- file.path(pdf_dir, paste0('PREIS_DRC_Ebola_SitRep_', sprintf('%03d', as.integer(sitrep_no)), '.pdf'))
  out <- preis_safe_download_pdf_from_url(pdf_url, dest_file, referer = page_url)
  if (!is.na(out) && nzchar(out) && preis_safe_pdf_signature_ok(out)) return(out)
  NA_character_
}

# END PREIS UNIVERSAL INSP PDF RESOLVER

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
# PREIS universal PDF resolver call
if (!exists('pdf_url')) pdf_url <- ''
if (!exists('page_url')) page_url <- ''
if (!exists('sitrep_no')) sitrep_no <- NA_integer_
if (!exists('sitrep_title')) sitrep_title <- ''
pdf_url <- preis_safe_resolve_pdf_universal(
  sitrep_no = sitrep_no,
  page_url = page_url,
  current_pdf_url = pdf_url,
  title = sitrep_title
)
safe_log(paste0('Universal PDF resolved: ', ifelse(nzchar(pdf_url), pdf_url, 'not found')))
# PREIS universal PDF download call
pdf_file <- preis_safe_download_resolved_pdf(pdf_url, sitrep_no, page_url)
pdf_attached <- !is.na(pdf_file) && nzchar(pdf_file) && file.exists(pdf_file)
safe_log(paste0('Universal PDF downloaded: ', ifelse(pdf_attached, pdf_file, 'not downloaded')))
# PREIS universal PDF info normalization
if (exists('pdf_info')) {
  pdf_info$url <- pdf_url
  pdf_info$file <- if (exists('pdf_file')) pdf_file else NA_character_
  pdf_info$attached <- exists('pdf_attached') && isTRUE(pdf_attached)
}
if (nzchar(pdf_url)) log_msg('PDF URL: ', pdf_url) else log_msg('PDF URL: not found')

state <- data.frame(sitrep_number = integer(), notified_utc = character(), stringsAsFactors = FALSE)
if (file.exists(state_file)) {
  state <- tryCatch(read.csv(state_file, stringsAsFactors = FALSE), error = function(e) state)
}

already <- nrow(state) > 0 && sitrep_number %in% suppressWarnings(as.integer(state$sitrep_number))
# PREIS STRICT ANTI REPEAT START
log_msg('Safe email anti-repeat check: sitrep=', sitrep_label, ' already=', already, ' force_send=', force_send)
if (already && !force_send) {
  log_msg('Safe email skipped: ', sitrep_label, ' already notified and force_send=false')
  quit(save = 'no', status = 0)
}
# PREIS STRICT ANTI REPEAT END
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

# PREIS PDF_INFO SAFETY START
# Ensure pdf_info always exists before building the email body.
# Priority: attach the official PDF if possible; otherwise send official links.
if (!exists('pdf_info')) {
  pdf_info <- list(
    attached = FALSE,
    path = '',
    url = '',
    reason = 'PDF information was not created upstream.'
  )

  if (exists('pdf_url') && !is.na(pdf_url) && nzchar(trimws(as.character(pdf_url))) &&
      !grepl('not found', as.character(pdf_url), ignore.case = TRUE)) {
    candidate_pdf_url <- trimws(as.character(pdf_url))
    tmp_pdf <- file.path(tempdir(), sprintf('PREIS_DRC_Ebola_SitRep_%03d.pdf', as.integer(sitrep_number)))

    ok_pdf <- tryCatch({
      utils::download.file(
        url = candidate_pdf_url,
        destfile = tmp_pdf,
        quiet = TRUE,
        method = 'libcurl',
        mode = 'wb',
        headers = c(
          'User-Agent' = 'Mozilla/5.0 PREIS-Ebola-DRC-Monitor',
          'Accept' = 'application/pdf,*/*'
        )
      )
      TRUE
    }, warning = function(w) {
      log_msg('PDF download warning: ', conditionMessage(w))
      FALSE
    }, error = function(e) {
      log_msg('PDF download error: ', conditionMessage(e))
      FALSE
    })

    if (ok_pdf && file.exists(tmp_pdf) && file.info(tmp_pdf)$size > 5000) {
      con <- file(tmp_pdf, 'rb')
      sig <- readBin(con, what = 'raw', n = 4)
      close(con)

      if (identical(sig, charToRaw('%PDF'))) {
        pdf_info <- list(
          attached = TRUE,
          path = tmp_pdf,
          url = candidate_pdf_url,
          reason = ''
        )
        log_msg('PDF attachment prepared from existing pdf_url: ', candidate_pdf_url)
      } else {
        pdf_info <- list(
          attached = FALSE,
          path = '',
          url = candidate_pdf_url,
          reason = 'Downloaded file was not a valid PDF.'
        )
      }
    } else {
      pdf_info <- list(
        attached = FALSE,
        path = '',
        url = candidate_pdf_url,
        reason = 'PDF URL found but file could not be downloaded.'
      )
    }
  }

  if (!isTRUE(pdf_info$attached) && (!nzchar(pdf_info$url)) && exists('page_url')) {
    pdf_info$url <- as.character(page_url)
    pdf_info$reason <- paste(pdf_info$reason, 'Using official INSP page as fallback link.')
  }

  log_msg('PDF attachment status: attached=', pdf_info$attached, ' url=', pdf_info$url)
}
# PREIS PDF_INFO SAFETY END

subject <- paste0('[PREIS Ebola DRC] Official SitRep - ', sitrep_label)

if (isTRUE(pdf_info$attached)) {
  body_detailed <- paste(
    'Dear colleagues,',
    '',
    'Please find attached the latest official Ebola DRC Situation Report detected by PREIS.',
    '',
    paste0('SitRep: ', sitrep_label),
    'Source: INSP DRC official SitRep page.',
    '',
    'Kind regards,',
    'PREIS Ebola DRC Automation',
    '',
    'Automation by PREIS.',
    'For support, contact Dr Hyacinthe ZABRE.',
    'PREIS WhatsApp contact: +226 78 08 87 70.',
    sep = '\n'
  )
} else {
  pdf_line <- if (nzchar(pdf_info$url)) {
    paste0('PDF link: ', pdf_info$url)
  } else {
    'PDF link: not automatically identified'
  }

  body_detailed <- paste(
    'Dear colleagues,',
    '',
    'PREIS detected the latest official Ebola DRC Situation Report.',
    '',
    paste0('SitRep: ', sitrep_label),
    'Source: INSP DRC official SitRep page.',
    paste0('Official page: ', page_url),
    pdf_line,
    '',
    'The PDF attachment could not be added automatically, but the official link is provided above.',
    '',
    'Kind regards,',
    'PREIS Ebola DRC Automation',
    '',
    'Automation by PREIS.',
    'For support, contact Dr Hyacinthe ZABRE.',
    'PREIS WhatsApp contact: +226 78 08 87 70.',
    sep = '\n'
  )
}

body_minimal <- body_detailed

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
