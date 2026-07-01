############################################################
# PREIS robust PDF download helper
# Handles INSP PDF URLs containing N°, NÂ°, NAÂ° and encoded variants
############################################################

preis_safe_urldecode <- function(x) {
  tryCatch(utils::URLdecode(x), error = function(e) x)
}

preis_fix_pdf_url_mojibake <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub('NAÂ°', 'N°', x, fixed = TRUE)
  x <- gsub('NÂ°', 'N°', x, fixed = TRUE)
  x <- gsub('Â°', '°', x, fixed = TRUE)
  x <- gsub('%C3%82%C2%B0', '%C2%B0', x, ignore.case = TRUE)
  x <- gsub('%C3%82%B0', '%C2%B0', x, ignore.case = TRUE)
  x <- gsub('%C2%BA', '%C2%B0', x, ignore.case = TRUE)
  x
}

preis_encode_url_path <- function(u) {
  u <- preis_fix_pdf_url_mojibake(u)
  m <- regexpr('^[A-Za-z][A-Za-z0-9+.-]*://[^/?#]+', u, perl = TRUE)
  if (is.na(m[1]) || m[1] < 0) {
    return(utils::URLencode(preis_safe_urldecode(u), reserved = TRUE))
  }
  prefix <- substr(u, 1, attr(m, 'match.length'))
  rest <- substring(u, attr(m, 'match.length') + 1)
  fragment <- ''
  if (grepl('#', rest, fixed = TRUE)) {
    sp <- strsplit(rest, '#', fixed = TRUE)[[1]]
    rest <- sp[1]
    fragment <- paste0('#', paste(sp[-1], collapse = '#'))
  }
  query <- ''
  if (grepl('?', rest, fixed = TRUE)) {
    sp <- strsplit(rest, '?', fixed = TRUE)[[1]]
    rest <- sp[1]
    query <- paste0('?', paste(sp[-1], collapse = '?'))
  }
  path <- rest
  segments <- strsplit(path, '/', fixed = TRUE)[[1]]
  encoded_segments <- vapply(
    segments,
    function(s) utils::URLencode(preis_safe_urldecode(s), reserved = TRUE),
    character(1)
  )
  paste0(prefix, paste(encoded_segments, collapse = '/'), query, fragment)
}

preis_pdf_url_candidates <- function(url) {
  u0 <- enc2utf8(as.character(url))
  u1 <- preis_fix_pdf_url_mojibake(u0)
  u2 <- preis_safe_urldecode(u0)
  u3 <- preis_fix_pdf_url_mojibake(u2)
  base <- unique(c(u0, u1, u2, u3))
  variants <- unique(c(
    base,
    gsub('NA°', 'N°', base, fixed = TRUE),
    gsub('NAÂ°', 'N°', base, fixed = TRUE),
    gsub('NÂ°', 'N°', base, fixed = TRUE),
    gsub('%C3%82%C2%B0', '%C2%B0', base, ignore.case = TRUE)
  ))
  encoded <- unique(vapply(variants, preis_encode_url_path, character(1)))
  unique(c(variants, encoded))
}

preis_is_pdf_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  if (is.na(file.info(path)$size) || file.info(path)$size < 500) return(FALSE)
  con <- file(path, 'rb')
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = 'raw', n = 4)
  identical(rawToChar(sig), '%PDF')
}

preis_try_download_file <- function(url, destfile, quiet = FALSE, mode = 'wb', method = 'libcurl', headers = NULL, ...) {
  candidates <- preis_pdf_url_candidates(url)
  default_headers <- c(
    'User-Agent' = 'Mozilla/5.0 PREIS-Ebola-DRC-SitRep-Monitor',
    'Accept' = 'application/pdf,application/octet-stream,*/*',
    'Referer' = 'https://insp.cd/'
  )
  all_headers <- c(default_headers, headers)
  last_error <- NULL
  expects_pdf <- grepl('\\.pdf($|[?#])', url, ignore.case = TRUE) || grepl('\\.pdf$', destfile, ignore.case = TRUE)
  for (candidate in candidates) {
    tmp <- paste0(destfile, '.tmp')
    if (file.exists(tmp)) unlink(tmp)
    if (!quiet) message('[PREIS DOWNLOAD] Trying: ', candidate)
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url = candidate,
        destfile = tmp,
        mode = mode,
        quiet = quiet,
        method = 'libcurl',
        headers = all_headers,
        ...
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
    if (ok_status && ok_file && ok_pdf) {
      file.copy(tmp, destfile, overwrite = TRUE)
      unlink(tmp)
      if (!quiet) message('[PREIS DOWNLOAD] OK: ', candidate)
      return(0L)
    }
    if (file.exists(tmp)) unlink(tmp)
  }
  warning('All download candidates failed for: ', url, if (!is.null(last_error)) paste0(' | last error: ', last_error) else '')
  1L
}
