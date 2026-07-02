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

