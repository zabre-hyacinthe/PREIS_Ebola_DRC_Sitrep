############################################################
# PREIS PDF RESOLVER V2.1
# scripts/preis_pdf_resolver_v2.R
#
# Role (unique responsibility of this module):
#   INSP SitRep article page  ->  pdfemb-data (base64 JSON)  ->  real PDF URL
#   ->  validated download (%PDF signature).
#
# This module ONLY resolves and downloads the official PDF.
# It NEVER sends email, NEVER touches the notification state,
# NEVER modifies SMTP / dashboard / GitHub Actions / WhatsApp.
#
# It is designed to be loaded by the main script with:
#   source("scripts/preis_pdf_resolver_v2.R")
#
# Public entry points (call one of these from the main script):
#   preis_v2_resolve_pdf_universal(sitrep_no, page_url, current_pdf_url = "", title = "")
#       -> returns a VALIDATED pdf URL (character), or "" if none.
#   preis_v2_resolve_and_download(sitrep_no, page_url, current_pdf_url = "", title = "",
#                                 dest_dir = file.path(getwd(), "data", "pdf"))
#       -> returns the local file path (validated %PDF), or NA_character_.
#
# ---------------------------------------------------------------------------
# WHY V2.0 RETURNED "NOT FOUND" EVEN THOUGH payload URLs=2 / API URLs=2
# ---------------------------------------------------------------------------
# The candidate URLs were found correctly, then DESTROYED during normalization:
#   1) xml2::url_absolute() was applied to URLs that were ALREADY absolute and
#      that contained a raw multibyte character (the degree sign in "N\u00b060",
#      UTF-8 0xC2 0xB0). libxml2's strict RFC-3986 URI parser rejects a raw
#      non-ASCII byte and returns NA. The good URL became NA.
#   2) utils::URLdecode() is NOT vectorized (it calls charToRaw() which requires
#      length-1). Called on a vector of candidates it either errors or corrupts,
#      and decoding %C2%B0 back to a raw \u00b0 re-triggers problem (1).
#   3) The ".pdf" filter (grepl) then ran AFTER (1)/(2): grepl(".pdf", NA) = NA,
#      so every NA candidate was silently dropped -> 0 testable candidates.
#
# V2.1 FIX:
#   * preis_v2_absolute_url() no longer uses xml2::url_absolute(). Absolute URLs
#     (^https?:// or ^//) are returned UNCHANGED; only truly relative paths are
#     joined to the base with pure base-R string logic. A valid URL can never be
#     turned into NA here.
#   * No blanket URLdecode(). Cleaning is per-element and only un-escapes JSON
#     "\/", fixes known mojibake (N\u00c2\u00b0 -> N\u00b0) and HTML entities. The degree sign
#     is preserved.
#   * Encoding differences are handled at DOWNLOAD time by trying URL variants
#     (raw "\u00b0", "%C2%B0", fully percent-encoded path) instead of mutating the
#     canonical candidate.
#   * Final validation is ALWAYS the binary %PDF signature, never the filename,
#     so future naming changes cannot break it.
############################################################

## --------------------------------------------------------------------------
## 0. Small helpers (self-contained; safe to source repeatedly)
## --------------------------------------------------------------------------

# Logger: reuse the main script's log_msg() if present, otherwise fall back.
# This is why the module never fails with 'could not find function "safe_log"'.
preis_v2_log <- function(...) {
  msg <- paste0(...)
  if (exists("log_msg", mode = "function")) {
    try(log_msg(msg), silent = TRUE)
  } else {
    cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), msg, "\n", sep = "")
  }
  invisible(msg)
}

preis_v2_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || (is.character(x) && !any(nzchar(x)))) y else x
}

preis_v2_compact <- function(x) {
  if (is.null(x)) return(character())
  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

# Best-effort SitRep number from the page URL / title. Used ONLY to name the
# output file when the caller passes NA (e.g. the sitrep_no vs sitrep_number
# wiring in the main script). Never affects which PDF is resolved.
preis_v2_extract_sitrep_no <- function(page_url = "", title = "") {
  x <- tolower(paste(as.character(page_url), as.character(title), collapse = " "))
  for (pat in c("sitrep[^0-9]{0,6}0*([0-9]{1,3})",
                "n[^0-9]{0,4}0*([0-9]{1,3})")) {
    m <- regmatches(x, regexec(pat, x, perl = TRUE))[[1]]
    if (length(m) >= 2) {
      v <- suppressWarnings(as.integer(m[2]))
      if (!is.na(v)) return(v)
    }
  }
  NA_integer_
}

## --------------------------------------------------------------------------
## 1. HTTP GET (text) with a browser-like User-Agent
## --------------------------------------------------------------------------
preis_v2_http_get_text <- function(url, timeout_sec = 90) {
  if (is.null(url) || is.na(url) || !nzchar(url)) return(NA_character_)
  resp <- tryCatch(
    httr::GET(
      url,
      httr::timeout(timeout_sec),
      httr::add_headers(
        `User-Agent`      = "Mozilla/5.0 PREIS-Ebola-DRC-PDF-Resolver",
        Accept            = "text/html,application/xhtml+xml,application/json,application/pdf,*/*",
        `Accept-Language` = "fr-FR,fr;q=0.9,en;q=0.8"
      )
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_character_)
  sc <- httr::status_code(resp)
  if (sc < 200 || sc >= 400) return(NA_character_)
  httr::content(resp, as = "text", encoding = "UTF-8")
}

## --------------------------------------------------------------------------
## 2. Absolute-URL resolution  (FIXED - no xml2::url_absolute())
##    Absolute URLs are returned untouched; only relative paths are joined.
## --------------------------------------------------------------------------
preis_v2_is_absolute <- function(u) {
  !is.na(u) & nzchar(u) & grepl("^https?://", u, ignore.case = TRUE)
}

preis_v2_absolute_url <- function(urls, base_url = "https://insp.cd") {
  urls <- preis_v2_compact(urls)
  if (length(urls) == 0) return(character())
  
  # normalize the base to scheme://host
  base_root <- sub("^(https?://[^/]+).*$", "\\1", base_url)
  if (!grepl("^https?://", base_root, ignore.case = TRUE)) base_root <- "https://insp.cd"
  
  out <- vapply(urls, function(u) {
    if (is.na(u) || !nzchar(u)) return(NA_character_)
    if (grepl("^https?://", u, ignore.case = TRUE)) return(u)          # already absolute -> keep
    if (startsWith(u, "//")) return(paste0("https:", u))               # protocol-relative
    if (startsWith(u, "/"))  return(paste0(base_root, u))              # site-absolute path
    paste0(base_root, "/", u)                                          # relative path
  }, character(1))
  
  preis_v2_compact(out)
}

## --------------------------------------------------------------------------
## 3. Candidate cleaning  (FIXED - per element, NA-safe, no blanket URLdecode)
## --------------------------------------------------------------------------
preis_v2_clean_one <- function(u) {
  if (is.na(u) || !nzchar(u)) return(NA_character_)
  u <- trimws(u)
  u <- sub("#.*$", "", u)                     # drop fragment
  u <- gsub('\\\\/', "/", u)                  # JSON escaped slash  \/  -> /
  u <- gsub("%5C/", "/", u, fixed = TRUE)     # \/ that got %-encoded
  u <- gsub("&amp;", "&", u, fixed = TRUE)    # HTML entity
  u <- gsub("&#038;", "&", u, fixed = TRUE)
  # JSON unicode escapes that may survive a raw base64 decode
  # (patterns/replacements use \uXXXX escapes so this file stays pure ASCII
  #  and is safe to source() under any locale, Windows included)
  u <- gsub("\\u00b0", "\u00b0", u, fixed = TRUE)   # undecoded \u00b0 -> degree sign
  u <- gsub("\\u003a", ":", u, fixed = TRUE)
  u <- gsub("\\u002f", "/", u, fixed = TRUE)
  u <- gsub("\\u0026", "&", u, fixed = TRUE)
  # known mojibake for the degree sign (UTF-8 bytes decoded as Latin-1)
  u <- gsub("NA\u00c2\u00b0", "N\u00b0", u, fixed = TRUE)  # mojibake NAA-deg -> N-deg
  u <- gsub("N\u00c2\u00b0",  "N\u00b0", u, fixed = TRUE)  # mojibake NA-deg  -> N-deg
  if (!nzchar(u)) return(NA_character_)
  u
}

preis_v2_clean_candidate_urls <- function(urls, base_url = "https://insp.cd") {
  urls <- preis_v2_compact(urls)
  if (length(urls) == 0) return(character())
  cleaned <- vapply(urls, preis_v2_clean_one, character(1))
  cleaned <- cleaned[!is.na(cleaned) & nzchar(cleaned)]
  cleaned <- preis_v2_absolute_url(cleaned, base_url)          # keeps absolute URLs intact
  # keep only things that really point at a .pdf (path or query)
  cleaned <- cleaned[grepl("[.]pdf($|[?#])", cleaned, ignore.case = TRUE)]
  preis_v2_compact(cleaned)
}

## --------------------------------------------------------------------------
## 4. pdfemb-data (PDF Embedder plugin) decoder
##    Restores base64 padding, decodes, extracts url / pdfID / title.
## --------------------------------------------------------------------------
preis_v2_b64_fix_padding <- function(b64) {
  b64 <- gsub("[^A-Za-z0-9+/=]", "", b64)
  n <- nchar(b64) %% 4
  if (n > 0) b64 <- paste0(b64, strrep("=", 4 - n))
  b64
}

preis_v2_json_get <- function(json_text, key) {
  # robust single-key string extractor that tolerates \/ escaped values
  m <- regmatches(
    json_text,
    regexpr(paste0('"', key, '"\\s*:\\s*"(?:\\\\.|[^"\\\\])*"'), json_text, perl = TRUE)
  )
  if (length(m) == 0) return(NA_character_)
  val <- sub(paste0('^"', key, '"\\s*:\\s*"'), "", m, perl = TRUE)
  val <- sub('"$', "", val)
  preis_v2_clean_one(val)
}

preis_v2_decode_pdfemb <- function(html_text) {
  res <- list(payload_urls = character(), pdf_ids = character(), titles = character(), n_payloads = 0L)
  if (is.na(html_text) || !nzchar(html_text)) return(res)
  
  b64_matches <- stringr::str_match_all(
    html_text,
    "pdfemb-data=[\"']?([A-Za-z0-9+/=%]+)"
  )[[1]]
  if (is.null(b64_matches) || nrow(b64_matches) == 0) return(res)
  
  res$n_payloads <- nrow(b64_matches)
  for (raw_b64 in b64_matches[, 2]) {
    b64 <- utils::URLdecode(raw_b64)                 # single element -> safe here
    b64 <- preis_v2_b64_fix_padding(b64)
    decoded <- tryCatch(rawToChar(base64enc::base64decode(b64)),
                        error = function(e) NA_character_)
    if (is.na(decoded) || !nzchar(decoded)) next
    
    # Preferred: proper JSON parse when jsonlite is available
    parsed_url <- NA_character_; parsed_id <- NA_character_; parsed_title <- NA_character_
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      obj <- tryCatch(jsonlite::fromJSON(decoded), error = function(e) NULL)
      if (!is.null(obj)) {
        parsed_url   <- preis_v2_coalesce(obj$url,   NA_character_)[1]
        parsed_id    <- as.character(preis_v2_coalesce(obj$pdfID, NA_character_)[1])
        parsed_title <- preis_v2_coalesce(obj$title, NA_character_)[1]
      }
    }
    # Fallback: regex extraction (handles \/ and unicode escapes)
    if (is.na(parsed_url))   parsed_url   <- preis_v2_json_get(decoded, "url")
    if (is.na(parsed_id))    parsed_id    <- {
      me  <- regexec('"pdfID"\\s*:\\s*"?([0-9]+)', decoded, perl = TRUE)
      grp <- regmatches(decoded, me)[[1]]
      if (length(grp) >= 2) grp[2] else NA_character_
    }
    if (is.na(parsed_title)) parsed_title <- preis_v2_json_get(decoded, "title")
    
    if (!is.na(parsed_url) && nzchar(parsed_url)) res$payload_urls <- c(res$payload_urls, preis_v2_clean_one(parsed_url))
    if (!is.na(parsed_id)  && nzchar(parsed_id))  res$pdf_ids      <- c(res$pdf_ids, parsed_id)
    if (!is.na(parsed_title) && nzchar(parsed_title)) res$titles   <- c(res$titles, parsed_title)
  }
  res$payload_urls <- preis_v2_compact(res$payload_urls)
  res$pdf_ids      <- preis_v2_compact(res$pdf_ids)
  res$titles       <- preis_v2_compact(res$titles)
  res
}

## --------------------------------------------------------------------------
## 5. WordPress media API: pdfID -> source_url
## --------------------------------------------------------------------------
preis_v2_media_api_urls <- function(pdf_ids, base_url = "https://insp.cd") {
  ids <- preis_v2_compact(pdf_ids)
  if (length(ids) == 0) return(character())
  base_root <- sub("^(https?://[^/]+).*$", "\\1", base_url)
  out <- character()
  for (id in ids) {
    api <- paste0(base_root, "/wp-json/wp/v2/media/", id)
    txt <- preis_v2_http_get_text(api, timeout_sec = 45)
    if (is.na(txt) || !nzchar(txt)) next
    src <- NA_character_
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      obj <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
      if (!is.null(obj)) {
        src <- preis_v2_coalesce(obj$source_url, NA_character_)[1]
        if (is.na(src) && !is.null(obj$guid)) src <- preis_v2_coalesce(obj$guid$rendered, NA_character_)[1]
      }
    }
    if (is.na(src)) src <- preis_v2_json_get(txt, "source_url")
    if (is.na(src)) src <- preis_v2_json_get(txt, "rendered")
    if (!is.na(src) && nzchar(src)) out <- c(out, preis_v2_clean_one(src))
  }
  preis_v2_compact(out)
}

## --------------------------------------------------------------------------
## 6. Direct .pdf links in the HTML (href / src / raw)
## --------------------------------------------------------------------------
preis_v2_direct_pdf_urls <- function(html_text, page_url) {
  if (is.na(html_text) || !nzchar(html_text)) return(character())
  urls <- character()
  
  raw_pdf <- stringr::str_extract_all(
    html_text,
    "https?://[^\"'<>[:space:]]+[.]pdf(?:[?#][^\"'<>[:space:]]*)?"
  )[[1]]
  urls <- c(urls, raw_pdf)
  
  rel_pdf <- stringr::str_extract_all(
    html_text,
    "/wp-content/uploads/[^\"'<>[:space:]]+[.]pdf(?:[?#][^\"'<>[:space:]]*)?"
  )[[1]]
  urls <- c(urls, rel_pdf)
  
  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)
  if (!is.null(doc)) {
    nodes <- rvest::html_nodes(doc, "a, iframe, embed, object, source")
    attrs <- c(
      rvest::html_attr(nodes, "href"),
      rvest::html_attr(nodes, "src"),
      rvest::html_attr(nodes, "data"),
      rvest::html_attr(nodes, "data-url"),
      rvest::html_attr(nodes, "data-pdf")
    )
    attrs <- attrs[!is.na(attrs) & grepl("[.]pdf", attrs, ignore.case = TRUE)]
    urls <- c(urls, attrs)
  }
  preis_v2_compact(urls)
}

## --------------------------------------------------------------------------
## 7. Last-resort candidates from known INSP upload patterns (kept LOW priority)
## --------------------------------------------------------------------------
preis_v2_context_candidates <- function(sitrep_no, page_url, title = "") {
  n_int <- suppressWarnings(as.integer(sitrep_no))
  if (is.na(n_int)) return(character())
  context <- paste(page_url, title, collapse = " ")
  m <- stringr::str_match(context, "(\\d{2})[-_/](\\d{2})[-_/](\\d{4})")
  if (all(is.na(m))) return(character())
  dd <- m[2]; mm <- m[3]; yyyy <- m[4]
  base <- paste0("https://insp.cd/wp-content/uploads/", yyyy, "/", mm, "/")
  nums  <- c(as.character(n_int), sprintf("%02d", n_int), sprintf("%03d", n_int))
  dates <- c(paste(dd, mm, yyyy, sep = "-"), paste(dd, mm, yyyy, sep = "_"), paste0(dd, mm, yyyy))
  prefixes <- c(
    "SitRep_MVE_RDC_N\u00b0", "Draft_SitRep_MVE_RDC_N\u00b0",
    "SitRep_MVB_RDC_N\u00b0", "Draft_SitRep_MVB_RDC_N\u00b0"
  )
  names <- character()
  for (p in prefixes) for (n in nums) for (d in dates) {
    names <- c(names, paste0(p, n, "_", d, "_Analytique_VF.pdf"),
               paste0(p, n, "_", d, ".pdf"))
  }
  preis_v2_compact(paste0(base, names))
}

## --------------------------------------------------------------------------
## 8. Ranking  (real extracted candidates first; guesses last)
## --------------------------------------------------------------------------
preis_v2_rank_candidates <- function(urls, sitrep_no = NA, is_guess = NULL) {
  urls <- preis_v2_compact(urls)
  if (length(urls) == 0) return(character())
  if (is.null(is_guess)) is_guess <- rep(FALSE, length(urls))
  is_guess <- rep(is_guess, length.out = length(urls))
  n_int <- suppressWarnings(as.integer(sitrep_no))
  num_pat <- if (!is.na(n_int)) {
    paste0("(", n_int, "|", sprintf("%02d", n_int), "|", sprintf("%03d", n_int), ")")
  } else NA_character_
  
  score <- vapply(seq_along(urls), function(i) {
    u <- urls[i]; s <- 0
    if (grepl("wp-content/uploads", u, ignore.case = TRUE)) s <- s + 5
    if (grepl("[.]pdf($|[?#])", u, ignore.case = TRUE))     s <- s + 3
    if (grepl("sitrep",   u, ignore.case = TRUE))           s <- s + 2
    if (grepl("analytique|final|draft|_vf", u, ignore.case = TRUE)) s <- s + 1
    if (grepl("mve|mvb|bdbv|ebola|marburg", u, ignore.case = TRUE)) s <- s + 1
    if (!is.na(num_pat) && grepl(num_pat, u))               s <- s + 2
    if (isTRUE(is_guess[i]))                                s <- s - 6   # guesses last
    s
  }, numeric(1))
  
  urls[order(score, decreasing = TRUE)]
}

## --------------------------------------------------------------------------
## 9. PDF signature validation  (%PDF)  - names never used to validate
## --------------------------------------------------------------------------
preis_v2_pdf_signature_ok <- function(file) {
  if (is.na(file) || !nzchar(file) || !file.exists(file)) return(FALSE)
  if (file.info(file)$size < 1000) return(FALSE)
  con <- file(file, "rb"); on.exit(close(con), add = TRUE)
  head_bytes <- readBin(con, what = "raw", n = 1024)
  if (length(head_bytes) >= 4 && identical(head_bytes[1:4], charToRaw("%PDF"))) return(TRUE)
  # tolerate a small leading offset (BOM / whitespace). Use grepRaw so embedded
  # NUL bytes inside a real PDF do not crash rawToChar().
  length(grepRaw(charToRaw("%PDF-"), head_bytes, fixed = TRUE)) > 0
}

## --------------------------------------------------------------------------
## 10. Download with encoding variants
## --------------------------------------------------------------------------
preis_v2_url_variants <- function(url) {
  url <- as.character(url[1])
  out <- c(url)
  # raw \u00b0 <-> %C2%B0
  out <- c(out, gsub("\u00b0", "%C2%B0", url, fixed = TRUE))
  out <- c(out, gsub("%C2%B0", "\u00b0", url, fixed = TRUE))
  # fully percent-encoded path (but keep the URL delimiters)
  enc <- tryCatch(utils::URLencode(url, reserved = TRUE), error = function(e) url)
  enc <- gsub("%3A", ":", enc, fixed = TRUE)
  enc <- gsub("%2F", "/", enc, fixed = TRUE)
  enc <- gsub("%3F", "?", enc, fixed = TRUE)
  enc <- gsub("%3D", "=", enc, fixed = TRUE)
  enc <- gsub("%26", "&", enc, fixed = TRUE)
  out <- c(out, enc)
  preis_v2_compact(out)
}

preis_v2_download_one <- function(pdf_url, dest_file,
                                  referer = "https://insp.cd/category/sitrep/") {
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
  for (u in preis_v2_url_variants(pdf_url)) {
    resp <- tryCatch(
      httr::GET(
        u,
        httr::timeout(180),
        httr::write_disk(dest_file, overwrite = TRUE),
        httr::add_headers(
          `User-Agent` = "Mozilla/5.0 PREIS-Ebola-DRC-PDF-Download",
          Referer      = referer,
          Accept       = "application/pdf,*/*"
        )
      ),
      error = function(e) NULL
    )
    if (!is.null(resp)) {
      code <- httr::status_code(resp)
      if (code >= 200 && code < 400 && preis_v2_pdf_signature_ok(dest_file)) {
        return(normalizePath(dest_file, mustWork = FALSE))
      }
    }
    if (file.exists(dest_file)) unlink(dest_file, force = TRUE)
  }
  NA_character_
}

## --------------------------------------------------------------------------
## 11. Given ranked candidates, return the FIRST that yields a valid %PDF
##     If dest_file is NULL, download to a probe temp (kept only to validate).
## --------------------------------------------------------------------------
preis_v2_first_valid_pdf <- function(candidates, referer, dest_file = NULL) {
  candidates <- preis_v2_compact(candidates)
  if (length(candidates) == 0) return(list(url = "", file = NA_character_))
  probe_dir <- file.path(getwd(), "data", "pdf_probe")
  for (u in candidates) {
    target <- if (is.null(dest_file)) {
      dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
      tempfile(pattern = "preis_v2_probe_", fileext = ".pdf", tmpdir = probe_dir)
    } else dest_file
    got <- preis_v2_download_one(u, target, referer = referer)
    if (!is.na(got) && preis_v2_pdf_signature_ok(got)) {
      if (is.null(dest_file)) unlink(got, force = TRUE)  # probe only
      return(list(url = u, file = if (is.null(dest_file)) NA_character_ else got))
    }
    if (is.null(dest_file) && file.exists(target)) unlink(target, force = TRUE)
  }
  list(url = "", file = NA_character_)
}

## --------------------------------------------------------------------------
## 12. MAIN: gather -> log -> clean -> rank -> validate
## --------------------------------------------------------------------------
preis_v2_gather_candidates <- function(sitrep_no, page_url, current_pdf_url = "", title = "") {
  html_text <- preis_v2_http_get_text(page_url)
  
  pdfemb  <- preis_v2_decode_pdfemb(html_text)
  api     <- preis_v2_media_api_urls(pdfemb$pdf_ids, base_url = page_url)
  direct  <- preis_v2_direct_pdf_urls(html_text, page_url)
  current <- preis_v2_compact(current_pdf_url)
  guesses <- preis_v2_context_candidates(sitrep_no, page_url, title)
  
  preis_v2_log("PDF resolver V2.1: pdfemb payloads=", pdfemb$n_payloads)
  preis_v2_log("PDF resolver V2.1: payload URLs=", length(pdfemb$payload_urls))
  preis_v2_log("PDF resolver V2.1: direct URLs=", length(direct))
  preis_v2_log("PDF resolver V2.1: API URLs=", length(api))
  
  # order of trust: pdfemb payload, API media, direct link, the value passed in
  real <- preis_v2_compact(c(pdfemb$payload_urls, api, direct, current))
  list(real = real, guesses = guesses, titles = pdfemb$titles)
}

preis_v2_resolve_pdf_universal <- function(sitrep_no, page_url,
                                           current_pdf_url = "", title = "") {
  if (is.null(page_url) || is.na(page_url) || !nzchar(page_url)) {
    preis_v2_log("PDF resolver V2.1: no page_url provided")
    return("")
  }
  g <- preis_v2_gather_candidates(sitrep_no, page_url, current_pdf_url, title)
  
  raw_real <- g$real
  preis_v2_log("PDF resolver V2.1: raw real candidates=", length(raw_real))
  for (i in seq_along(raw_real)) preis_v2_log("PDF resolver V2.1 raw ", i, ": ", raw_real[i])
  
  cleaned_real <- preis_v2_clean_candidate_urls(raw_real, base_url = page_url)
  cleaned_real <- preis_v2_rank_candidates(cleaned_real, sitrep_no, is_guess = FALSE)
  preis_v2_log("PDF resolver V2.1: cleaned real candidates=", length(cleaned_real))
  if (length(cleaned_real) > 0) preis_v2_log("PDF resolver V2.1: first real candidate=", cleaned_real[1])
  
  cleaned_guess <- preis_v2_clean_candidate_urls(g$guesses, base_url = page_url)
  cleaned_guess <- preis_v2_rank_candidates(cleaned_guess, sitrep_no, is_guess = TRUE)
  
  ordered <- preis_v2_compact(c(cleaned_real, cleaned_guess))
  if (length(ordered) == 0) {
    preis_v2_log("PDF resolver V2.1: no candidate URL after cleaning")
    return("")
  }
  
  hit <- preis_v2_first_valid_pdf(ordered, referer = page_url, dest_file = NULL)
  if (nzchar(hit$url)) {
    preis_v2_log("PDF resolver V2.1: valid PDF found")
    preis_v2_log("Resolved URL: ", hit$url)
    return(hit$url)
  }
  preis_v2_log("PDF resolver V2.1: no candidate passed %PDF validation")
  preis_v2_log("Resolved URL: NOT FOUND")
  ""
}

## --------------------------------------------------------------------------
## 13. Resolve + download to data/pdf/PREIS_DRC_Ebola_SitRep_<NNN>.pdf
## --------------------------------------------------------------------------
preis_v2_resolve_and_download <- function(sitrep_no, page_url,
                                          current_pdf_url = "", title = "",
                                          dest_dir = file.path(getwd(), "data", "pdf")) {
  g <- preis_v2_gather_candidates(sitrep_no, page_url, current_pdf_url, title)
  
  raw_real <- g$real
  preis_v2_log("PDF resolver V2.1: raw real candidates=", length(raw_real))
  for (i in seq_along(raw_real)) preis_v2_log("PDF resolver V2.1 raw ", i, ": ", raw_real[i])
  
  cleaned_real  <- preis_v2_rank_candidates(
    preis_v2_clean_candidate_urls(raw_real, base_url = page_url), sitrep_no, is_guess = FALSE)
  cleaned_guess <- preis_v2_rank_candidates(
    preis_v2_clean_candidate_urls(g$guesses, base_url = page_url), sitrep_no, is_guess = TRUE)
  preis_v2_log("PDF resolver V2.1: cleaned real candidates=", length(cleaned_real))
  if (length(cleaned_real) > 0) preis_v2_log("PDF resolver V2.1: first real candidate=", cleaned_real[1])
  
  ordered <- preis_v2_compact(c(cleaned_real, cleaned_guess))
  if (length(ordered) == 0) {
    preis_v2_log("Resolved URL: NOT FOUND")
    return(NA_character_)
  }
  
  n_int <- suppressWarnings(as.integer(sitrep_no))
  if (is.na(n_int)) n_int <- preis_v2_extract_sitrep_no(page_url, title)   # fallback from URL/title
  fname <- if (is.na(n_int)) "PREIS_DRC_Ebola_SitRep_latest.pdf" else sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", n_int)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_file <- file.path(dest_dir, fname)
  
  hit <- preis_v2_first_valid_pdf(ordered, referer = page_url, dest_file = dest_file)
  if (nzchar(hit$url) && !is.na(hit$file) && preis_v2_pdf_signature_ok(hit$file)) {
    preis_v2_log("Resolved URL: ", hit$url)
    preis_v2_log("Downloaded file: ", hit$file)
    preis_v2_log("PDF signature valid: TRUE")
    return(hit$file)
  }
  preis_v2_log("Resolved URL: NOT FOUND")
  NA_character_
}

## --------------------------------------------------------------------------
## 14. Download an ALREADY-resolved URL to the canonical destination.
##     Mirrors the old preis_safe_download_resolved_pdf() 2-step pattern so the
##     main script can do:  url <- resolve_pdf_universal(); file <- download_resolved(url,...)
##     without resolving twice.
## --------------------------------------------------------------------------
preis_v2_download_resolved <- function(pdf_url, sitrep_no, page_url = "",
                                       dest_dir = file.path(getwd(), "data", "pdf")) {
  if (is.null(pdf_url) || is.na(pdf_url) || !nzchar(pdf_url)) return(NA_character_)
  if (!grepl("[.]pdf($|[?#])", pdf_url, ignore.case = TRUE)) return(NA_character_)
  n_int <- suppressWarnings(as.integer(sitrep_no))
  if (is.na(n_int)) n_int <- preis_v2_extract_sitrep_no(page_url, "")   # fallback from URL
  fname <- if (is.na(n_int)) "PREIS_DRC_Ebola_SitRep_latest.pdf" else sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", n_int)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_file <- file.path(dest_dir, fname)
  out <- preis_v2_download_one(pdf_url, dest_file, referer = if (nzchar(page_url)) page_url else "https://insp.cd/category/sitrep/")
  if (!is.na(out) && preis_v2_pdf_signature_ok(out)) {
    preis_v2_log("Downloaded file: ", out)
    preis_v2_log("PDF signature valid: TRUE")
    return(out)
  }
  preis_v2_log("Download failed for resolved URL: ", pdf_url)
  NA_character_
}

# Convenience aliases (in case the main script calls a slightly different name)
preis_v2_resolve_pdf <- preis_v2_resolve_pdf_universal
preis_v2_resolve     <- preis_v2_resolve_pdf_universal

# Signal successful load (mirrors the original "INSTALLED SUCCESSFULLY" message)
preis_v2_log("PREIS PDF RESOLVER V2.1 INSTALLED SUCCESSFULLY")

# END scripts/preis_pdf_resolver_v2.R