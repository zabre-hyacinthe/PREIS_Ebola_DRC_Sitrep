# ============================================================
# PREIS EBOLA DRC — CLOUD SITREP MONITOR
# Version stable base R only
#
# Objectifs:
#   1) Scanner le site INSP pour identifier le dernier SitRep Ebola/MVB
#   2) Extraire le PDF, même s'il est intégré dans une page WordPress
#   3) Télécharger le PDF
#   4) Envoyer le PDF par email via SMTP avec curl système
#   5) Eviter les doublons avec un fichier state RDS
#
# Important:
#   - Aucun install.packages()
#   - Aucun package R externe obligatoire
#   - Fonctionne avec GitHub Actions + secrets SMTP
# ============================================================

options(warn = 1)

# ============================================================
# HELPERS
# ============================================================

log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
}

stop_clean <- function(...) {
  msg <- paste(...)
  log_msg("ERROR:", msg)
  stop(msg, call. = FALSE)
}

first_non_empty <- function(names, default = "") {
  for (nm in names) {
    val <- Sys.getenv(nm, unset = "")
    if (!is.na(val) && nzchar(trimws(val))) {
      return(trimws(val))
    }
  }
  default
}

to_bool <- function(x) {
  tolower(trimws(x)) %in% c("1", "true", "yes", "y", "oui")
}

safe_url_decode <- function(x) {
  if (length(x) == 0) return(character())
  vapply(
    x,
    function(z) {
      tryCatch(utils::URLdecode(z), error = function(e) z)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

html_unescape_basic <- function(x) {
  if (length(x) == 0) return(character())

  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#034;", "\"", x, fixed = TRUE)
  x <- gsub("&#039;", "'", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("\\\\u0026", "&", x, ignore.case = TRUE)
  x <- gsub("\\\\/", "/", x)

  x
}

clean_url_tail <- function(x) {
  if (length(x) == 0) return(character())

  x <- gsub("[\"'<>]+$", "", x)
  x <- gsub("[,;\\)\\]\\}]+$", "", x)
  x <- gsub("\\\\+$", "", x)
  x <- sub("#.*$", "", x)

  x
}

normalize_url <- function(x, base_url = "https://insp.cd") {
  if (length(x) == 0) return(character())

  base_url <- sub("/+$", "", base_url)

  x <- html_unescape_basic(x)
  x <- safe_url_decode(x)
  x <- html_unescape_basic(x)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", "", x)
  x <- clean_url_tail(x)
  x <- x[nzchar(x)]

  if (length(x) == 0) return(character())

  x <- ifelse(grepl("^//", x), paste0("https:", x), x)
  x <- ifelse(grepl("^/", x), paste0(base_url, x), x)
  x <- ifelse(grepl("^wp-content/", x, ignore.case = TRUE), paste0(base_url, "/", x), x)

  is_relative <- !grepl("^https?://", x, ignore.case = TRUE) &
    !grepl("^(mailto:|tel:|javascript:|data:)", x, ignore.case = TRUE) &
    nzchar(x)

  x <- ifelse(is_relative, paste0(base_url, "/", x), x)

  x <- clean_url_tail(x)
  unique(x)
}

read_text_file_safe <- function(path) {
  x <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )

  paste(x, collapse = "\n")
}

write_csv_safe <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE)
}

# ============================================================
# URL AND PDF EXTRACTION
# ============================================================

extract_attr_urls <- function(html) {
  if (!nzchar(html)) return(character())

  patterns <- c(
    "href\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "src\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data-src\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data-url\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data-file\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data-download\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]"
  )

  out <- character()

  for (p in patterns) {
    m <- gregexpr(p, html, perl = TRUE, ignore.case = TRUE)
    hits <- regmatches(html, m)[[1]]

    if (length(hits) > 0 && !identical(hits[1], "-1")) {
      vals <- sub("^[^=]+=\\s*['\\\"]", "", hits, perl = TRUE)
      vals <- sub("['\\\"]$", "", vals, perl = TRUE)
      out <- c(out, vals)
    }
  }

  unique(out)
}

extract_all_urls <- function(text) {
  if (!nzchar(text)) return(character())

  text2 <- html_unescape_basic(text)
  text2 <- safe_url_decode(text2)
  text2 <- html_unescape_basic(text2)

  patterns <- c(
    "https?://[^'\\\"<>[:space:]]+",
    "/(?:wp-content/uploads|sitrep|wp-json|download|downloads)[^'\\\"<>[:space:]]+",
    "wp-content/uploads/[^'\\\"<>[:space:]]+"
  )

  out <- character()

  for (p in patterns) {
    m <- gregexpr(p, text2, perl = TRUE, ignore.case = TRUE)
    hits <- regmatches(text2, m)[[1]]

    if (length(hits) > 0 && !identical(hits[1], "-1")) {
      out <- c(out, hits)
    }
  }

  unique(out)
}

extract_pdf_urls <- function(text) {
  if (!nzchar(text)) return(character())

  text2 <- html_unescape_basic(text)
  text2 <- safe_url_decode(text2)
  text2 <- html_unescape_basic(text2)

  text2 <- gsub("%3A", ":", text2, ignore.case = TRUE)
  text2 <- gsub("%2F", "/", text2, ignore.case = TRUE)
  text2 <- gsub("%3F", "?", text2, ignore.case = TRUE)
  text2 <- gsub("%3D", "=", text2, ignore.case = TRUE)
  text2 <- gsub("%26", "&", text2, ignore.case = TRUE)
  text2 <- gsub("%20", " ", text2, ignore.case = TRUE)
  text2 <- html_unescape_basic(text2)

  patterns <- c(
    "https?://[^'\\\"<>[:space:]]+\\.pdf[^'\\\"<>[:space:]]*",
    "/wp-content/uploads/[^'\\\"<>[:space:]]+\\.pdf[^'\\\"<>[:space:]]*",
    "wp-content/uploads/[^'\\\"<>[:space:]]+\\.pdf[^'\\\"<>[:space:]]*"
  )

  out <- character()

  for (p in patterns) {
    m <- gregexpr(p, text2, perl = TRUE, ignore.case = TRUE)
    hits <- regmatches(text2, m)[[1]]

    if (length(hits) > 0 && !identical(hits[1], "-1")) {
      out <- c(out, hits)
    }
  }

  out <- gsub("\\\\/", "/", out)
  out <- clean_url_tail(out)
  unique(out)
}

extract_sitrep_no <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_integer_)

  y <- tolower(x)
  y <- safe_url_decode(y)
  y <- gsub("_", "-", y)
  y <- gsub("%20", "-", y, ignore.case = TRUE)

  patterns <- c(
    "sitrep[-[:space:]]*n[-[:space:]]*([0-9]{1,3})",
    "sitrep[-[:space:]]*n[^0-9]{0,8}([0-9]{1,3})",
    "sitrep[^0-9]{0,10}([0-9]{1,3})",
    "n[-[:space:]]*([0-9]{1,3})[-[:space:]]*mv[be]",
    "mv[be][^0-9]{0,10}([0-9]{1,3})"
  )

  for (p in patterns) {
    m <- regexec(p, y, perl = TRUE)
    r <- regmatches(y, m)[[1]]

    if (length(r) >= 2) {
      val <- suppressWarnings(as.integer(r[2]))
      if (!is.na(val)) return(val)
    }
  }

  NA_integer_
}

extract_date_from_text <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) return(as.Date(NA))

  y <- safe_url_decode(x)

  m1 <- regexec("([0-9]{2})[-_/]([0-9]{2})[-_/]([0-9]{4})", y, perl = TRUE)
  r1 <- regmatches(y, m1)[[1]]

  if (length(r1) >= 4) {
    d <- suppressWarnings(as.Date(paste0(r1[4], "-", r1[3], "-", r1[2])))
    if (!is.na(d)) return(d)
  }

  m2 <- regexec("([0-9]{4})[-_/]([0-9]{2})[-_/]([0-9]{2})", y, perl = TRUE)
  r2 <- regmatches(y, m2)[[1]]

  if (length(r2) >= 4) {
    d <- suppressWarnings(as.Date(paste0(r2[2], "-", r2[3], "-", r2[4])))
    if (!is.na(d)) return(d)
  }

  as.Date(NA)
}

safe_filename <- function(url, sitrep_no = NA_integer_) {
  clean <- sub("\\?.*$", "", url)
  clean <- safe_url_decode(clean)
  name <- basename(clean)
  name <- html_unescape_basic(name)
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)

  if (!grepl("\\.pdf$", name, ignore.case = TRUE)) {
    if (!is.na(sitrep_no)) {
      name <- paste0("SitRep_N", sitrep_no, "_MVB.pdf")
    } else {
      name <- paste0("SitRep_MVB_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    }
  }

  name
}

# ============================================================
# NETWORK HELPERS
# ============================================================

USER_AGENT <- "PREIS-Ebola-DRC-Monitor/1.0"

fetch_url_text <- function(url) {
  curl_bin <- Sys.which("curl")
  tmp <- tempfile(fileext = ".txt")

  if (nzchar(curl_bin)) {
    args <- c(
      "-L",
      "--fail",
      "--silent",
      "--show-error",
      "--retry", "2",
      "--connect-timeout", "20",
      "--max-time", "25",
      "--compressed",
      "--user-agent", USER_AGENT,
      "-o", tmp,
      url
    )

    res <- suppressWarnings(system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE))
    status <- attr(res, "status")
    if (is.null(status)) status <- 0

    if (status == 0 && file.exists(tmp) && file.info(tmp)$size > 0) {
      return(read_text_file_safe(tmp))
    }

    if (length(res) > 0) {
      log_msg("Warning: curl failed for:", url)
      log_msg(paste(res, collapse = " | "))
    }
  }

  ok <- tryCatch({
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb", method = "libcurl")
    TRUE
  }, error = function(e) FALSE)

  if (ok && file.exists(tmp) && file.info(tmp)$size > 0) {
    return(read_text_file_safe(tmp))
  }

  ""
}

download_binary <- function(url, dest) {
  curl_bin <- Sys.which("curl")

  if (nzchar(curl_bin)) {
    args <- c(
      "-L",
      "--fail",
      "--silent",
      "--show-error",
      "--retry", "2",
      "--connect-timeout", "20",
      "--max-time", "180",
      "--user-agent", USER_AGENT,
      "-o", dest,
      url
    )

    res <- suppressWarnings(system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE))
    status <- attr(res, "status")
    if (is.null(status)) status <- 0

    if (status == 0 && file.exists(dest) && file.info(dest)$size > 1000) {
      return(TRUE)
    }

    if (length(res) > 0) {
      log_msg("Warning: curl download failed for:", url)
      log_msg(paste(res, collapse = " | "))
    }
  }

  ok <- tryCatch({
    utils::download.file(url, dest, quiet = TRUE, mode = "wb", method = "libcurl")
    TRUE
  }, error = function(e) FALSE)

  ok && file.exists(dest) && file.info(dest)$size > 1000
}

# ============================================================
# EMAIL HELPERS
# ============================================================

split_emails <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(character())

  y <- unlist(strsplit(x, "[,;]"))
  y <- trimws(y)
  y <- y[nzchar(y)]

  unique(y)
}

make_base64_lines <- function(file_path) {
  base64_bin <- Sys.which("base64")

  if (!nzchar(base64_bin)) {
    stop_clean("La commande systeme base64 est introuvable.")
  }

  raw <- system2(
    base64_bin,
    args = normalizePath(file_path, winslash = "/", mustWork = TRUE),
    stdout = TRUE
  )

  b64 <- paste(raw, collapse = "")

  starts <- seq(1, nchar(b64), by = 76)
  substring(b64, starts, pmin(starts + 75, nchar(b64)))
}

send_email_with_attachment <- function(to, from, subject, body, attachment_path,
                                       cc = "", bcc = "") {
  smtp_url <- first_non_empty(c("SMTP_URL", "MAIL_SMTP_URL", "PREIS_SMTP_URL"))
  smtp_host <- first_non_empty(c("SMTP_HOST", "MAIL_SMTP_HOST", "PREIS_SMTP_HOST"))
  smtp_port <- first_non_empty(c("SMTP_PORT", "MAIL_SMTP_PORT", "PREIS_SMTP_PORT"), "587")

  if (!nzchar(smtp_url) && nzchar(smtp_host)) {
    if (identical(smtp_port, "465")) {
      smtp_url <- paste0("smtps://", smtp_host, ":", smtp_port)
    } else {
      smtp_url <- paste0("smtp://", smtp_host, ":", smtp_port)
    }
  }

  smtp_user <- first_non_empty(c(
    "SMTP_USERNAME", "SMTP_USER", "MAIL_USERNAME", "MAIL_USER", "PREIS_SMTP_USERNAME"
  ))

  smtp_pass <- first_non_empty(c(
    "SMTP_PASSWORD", "SMTP_PASS", "MAIL_PASSWORD", "MAIL_PASS", "PREIS_SMTP_PASSWORD"
  ))

  if (!nzchar(smtp_url)) stop_clean("SMTP_URL ou SMTP_HOST non configure.")
  if (!nzchar(smtp_user)) stop_clean("SMTP_USERNAME ou SMTP_USER non configure.")
  if (!nzchar(smtp_pass)) stop_clean("SMTP_PASSWORD ou SMTP_PASS non configure.")
  if (!nzchar(to)) stop_clean("EMAIL_TO ou ALERT_TO non configure.")
  if (!nzchar(from)) stop_clean("EMAIL_FROM ou ALERT_FROM non configure.")

  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) stop_clean("La commande systeme curl est introuvable.")

  to_recipients <- split_emails(to)
  cc_recipients <- split_emails(cc)
  bcc_recipients <- split_emails(bcc)

  all_recipients <- unique(c(to_recipients, cc_recipients, bcc_recipients))

  if (length(all_recipients) == 0) {
    stop_clean("Aucun destinataire email valide.")
  }

  boundary <- paste0("----PREIS_EBOLA_", format(Sys.time(), "%Y%m%d%H%M%S"))
  attachment_name <- basename(attachment_path)
  b64_lines <- make_base64_lines(attachment_path)

  mime_file <- tempfile(fileext = ".eml")

  cc_header <- if (length(cc_recipients) > 0) {
    paste0("Cc: ", paste(cc_recipients, collapse = ", "))
  } else {
    character(0)
  }

  msg <- c(
    paste0("From: ", from),
    paste0("To: ", paste(to_recipients, collapse = ", ")),
    cc_header,
    paste0("Subject: ", subject),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
    "",
    paste0("--", boundary),
    "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: 8bit",
    "",
    body,
    "",
    paste0("--", boundary),
    paste0("Content-Type: application/pdf; name=\"", attachment_name, "\""),
    paste0("Content-Disposition: attachment; filename=\"", attachment_name, "\""),
    "Content-Transfer-Encoding: base64",
    "",
    b64_lines,
    "",
    paste0("--", boundary, "--"),
    ""
  )

  writeLines(msg, mime_file, useBytes = TRUE)

  args <- c(
    "--url", smtp_url,
    "--ssl-reqd",
    "--mail-from", paste0("<", from, ">")
  )

  for (r in all_recipients) {
    args <- c(args, "--mail-rcpt", paste0("<", r, ">"))
  }

  args <- c(
    args,
    "--user", paste0(smtp_user, ":", smtp_pass),
    "--upload-file", mime_file,
    "--silent",
    "--show-error",
    "--fail"
  )

  res <- suppressWarnings(system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status")
  if (is.null(status)) status <- 0

  if (status != 0) {
    log_msg("SMTP/curl response:")
    if (length(res) > 0) print(res)
    stop_clean("Echec envoi email SMTP.")
  }

  TRUE
}

# ============================================================
# PDF CANDIDATE MANAGEMENT
# ============================================================

empty_pdf_df <- function() {
  data.frame(
    pdf_url = character(),
    page_url = character(),
    sitrep_no = integer(),
    sitrep_date = as.Date(character()),
    source_rank = integer(),
    stringsAsFactors = FALSE
  )
}

add_pdf_candidates <- function(existing_df, urls, page_url, source_rank, base_url) {
  if (length(urls) == 0) return(existing_df)

  urls <- normalize_url(urls, base_url)
  urls <- urls[nzchar(urls)]
  urls <- unique(urls)
  urls <- urls[grepl("\\.pdf(\\?|$)", urls, ignore.case = TRUE)]

  if (length(urls) == 0) return(existing_df)

  nos <- vapply(urls, extract_sitrep_no, integer(1))

  date_chr <- vapply(
    urls,
    function(z) {
      d <- extract_date_from_text(z)
      if (is.na(d)) NA_character_ else as.character(d)
    },
    character(1)
  )

  new_df <- data.frame(
    pdf_url = urls,
    page_url = page_url,
    sitrep_no = nos,
    sitrep_date = as.Date(date_chr),
    source_rank = source_rank,
    stringsAsFactors = FALSE
  )

  out <- rbind(existing_df, new_df)
  out <- out[!duplicated(out$pdf_url), , drop = FALSE]

  out
}

choose_latest_pdf <- function(pdf_df) {
  if (nrow(pdf_df) == 0) return(NULL)

  no_rank <- ifelse(is.na(pdf_df$sitrep_no), -1, pdf_df$sitrep_no)
  date_rank <- ifelse(is.na(pdf_df$sitrep_date), 0, as.numeric(pdf_df$sitrep_date))
  source_score <- -pdf_df$source_rank

  ord <- order(no_rank, date_rank, source_score, decreasing = TRUE)
  pdf_df[ord[1], , drop = FALSE]
}

# ============================================================
# CONFIGURATION
# ============================================================

BASE_URL <- first_non_empty(c("INSP_BASE_URL"), "https://insp.cd")
BASE_URL <- sub("/+$", "", BASE_URL)

STATE_DIR <- first_non_empty(c("PREIS_STATE_DIR"), "data/state")
INCOMING_DIR <- first_non_empty(c("PREIS_INCOMING_DIR"), "data/incoming/insp_sitreps")
OUT_DIR <- first_non_empty(c("PREIS_MONITOR_OUT_DIR"), "outputs/cloud_monitor")

dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INCOMING_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

STATE_FILE <- file.path(STATE_DIR, "latest_sitrep_monitor_state.rds")
QC_FILE <- file.path(OUT_DIR, "cloud_sitrep_monitor_qc.csv")
DEBUG_LINKS_FILE <- file.path(OUT_DIR, "debug_all_links.csv")
DEBUG_CANDIDATES_FILE <- file.path(OUT_DIR, "debug_candidate_pages.csv")
DEBUG_PDFS_FILE <- file.path(OUT_DIR, "debug_pdf_candidates.csv")

EMAIL_TO <- first_non_empty(c("EMAIL_TO", "ALERT_TO", "MAIL_TO", "PREIS_EMAIL_TO"))
EMAIL_FROM <- first_non_empty(c("EMAIL_FROM", "ALERT_FROM", "MAIL_FROM", "PREIS_EMAIL_FROM"))
EMAIL_CC <- first_non_empty(c("EMAIL_CC", "ALERT_CC", "MAIL_CC", "PREIS_EMAIL_CC"))
EMAIL_BCC <- first_non_empty(c("EMAIL_BCC", "ALERT_BCC", "MAIL_BCC", "PREIS_EMAIL_BCC"))

FORCE_SEND <- to_bool(first_non_empty(c("PREIS_FORCE_SEND", "FORCE_SEND"), "false"))
DRY_RUN <- to_bool(first_non_empty(c("PREIS_DRY_RUN", "DRY_RUN"), "false"))

MAX_CANDIDATES <- suppressWarnings(as.integer(first_non_empty(c("PREIS_MAX_CANDIDATES"), "25")))
if (is.na(MAX_CANDIDATES) || MAX_CANDIDATES < 1) MAX_CANDIDATES <- 100

MANUAL_PAGE_URL <- first_non_empty(c("INSP_LATEST_PAGE_URL", "INSP_PAGE_URL"))
MANUAL_PDF_URL <- first_non_empty(c("INSP_DIRECT_PDF_URL", "SITREP_PDF_URL"))

SEARCH_URLS <- unique(c(
  first_non_empty(c("INSP_SEARCH_URL")),
  paste0(BASE_URL, "/?s=sitrep+mvb"),
  paste0(BASE_URL, "/?s=SitRep+MVB"),
  paste0(BASE_URL, "/?s=Ebola+SitRep"),
  paste0(BASE_URL, "/?s=MVB"),
  paste0(BASE_URL, "/wp-json/wp/v2/search?search=sitrep%20mvb&per_page=50"),
  paste0(BASE_URL, "/wp-json/wp/v2/search?search=Ebola%20SitRep&per_page=50"),
  paste0(BASE_URL, "/wp-json/wp/v2/posts?search=sitrep%20mvb&per_page=50"),
  paste0(BASE_URL, "/wp-json/wp/v2/media?search=sitrep%20mvb&per_page=50")
))

SEARCH_URLS <- SEARCH_URLS[nzchar(SEARCH_URLS)]

log_msg("PREIS Ebola DRC cloud monitor started.")
log_msg("Base R monitor. No external R package installation.")
log_msg("Output directory:", OUT_DIR)

# ============================================================
# COLLECT LINKS AND DIRECT PDF CANDIDATES
# ============================================================

all_links <- character()
pdf_candidates <- empty_pdf_df()

if (nzchar(MANUAL_PDF_URL)) {
  log_msg("Manual PDF URL provided.")

  pdf_candidates <- add_pdf_candidates(
    existing_df = pdf_candidates,
    urls = MANUAL_PDF_URL,
    page_url = "manual_pdf_url",
    source_rank = 0,
    base_url = BASE_URL
  )
}

manual_pages <- character()

if (nzchar(MANUAL_PAGE_URL)) {
  manual_pages <- normalize_url(MANUAL_PAGE_URL, BASE_URL)
}

for (i in seq_along(SEARCH_URLS)) {
  u <- SEARCH_URLS[i]
  log_msg("Scanning:", u)

  html <- fetch_url_text(u)

  if (!nzchar(html)) {
    log_msg("Warning: page inaccessible or empty:", u)
    next
  }

  links <- c(
    extract_attr_urls(html),
    extract_all_urls(html),
    extract_pdf_urls(html)
  )

  links <- normalize_url(links, BASE_URL)
  links <- links[grepl("^https?://", links, ignore.case = TRUE)]
  links <- unique(links)

  all_links <- unique(c(all_links, links))

  direct_pdfs <- links[grepl("\\.pdf(\\?|$)", links, ignore.case = TRUE)]

  if (length(direct_pdfs) > 0) {
    pdf_candidates <- add_pdf_candidates(
      existing_df = pdf_candidates,
      urls = direct_pdfs,
      page_url = u,
      source_rank = i,
      base_url = BASE_URL
    )
  }
}

all_links <- unique(normalize_url(all_links, BASE_URL))

if (length(all_links) > 0) {
  write_csv_safe(
    data.frame(url = all_links, stringsAsFactors = FALSE),
    DEBUG_LINKS_FILE
  )
}

# ============================================================
# SELECT AND SCAN CANDIDATE PAGES
# ============================================================

candidate_links <- all_links[
  !grepl("\\.pdf(\\?|$)", all_links, ignore.case = TRUE) &
    grepl("sitrep|mvb|mve|ebola|situation", all_links, ignore.case = TRUE)
]

candidate_links <- candidate_links[
  !grepl("\\.(jpg|jpeg|png|gif|svg|css|js|ico|woff|woff2|ttf|xml)(\\?|$)",
        candidate_links,
        ignore.case = TRUE)
]

candidate_links <- unique(c(manual_pages, candidate_links))
candidate_links <- candidate_links[nzchar(candidate_links)]

if (length(candidate_links) > 0) {
  candidate_numbers <- vapply(candidate_links, extract_sitrep_no, integer(1))

  candidate_date_chr <- vapply(
    candidate_links,
    function(z) {
      d <- extract_date_from_text(z)
      if (is.na(d)) NA_character_ else as.character(d)
    },
    character(1)
  )

  candidate_dates <- as.Date(candidate_date_chr)

  candidate_order <- data.frame(
    url = candidate_links,
    sitrep_no = candidate_numbers,
    sitrep_date = candidate_dates,
    no_rank = ifelse(is.na(candidate_numbers), -1, candidate_numbers),
    date_rank = ifelse(is.na(candidate_dates), 0, as.numeric(candidate_dates)),
    stringsAsFactors = FALSE
  )

  candidate_order <- candidate_order[
    order(candidate_order$no_rank, candidate_order$date_rank, decreasing = TRUE),
    ,
    drop = FALSE
  ]

  candidate_links <- candidate_order$url

  write_csv_safe(candidate_order, DEBUG_CANDIDATES_FILE)
}

log_msg("Candidate SitRep pages found:", length(candidate_links))
log_msg("Direct PDF candidates found before page scan:", nrow(pdf_candidates))

if (length(candidate_links) > 0) {
  scan_links <- head(candidate_links, MAX_CANDIDATES)

  for (i in seq_along(scan_links)) {
    page_url <- scan_links[i]

    log_msg("Checking candidate page", i, "of", length(scan_links), ":", page_url)

    page_html <- fetch_url_text(page_url)

    if (!nzchar(page_html)) {
      log_msg("Warning: empty candidate page.")
      next
    }

    page_pdfs <- c(
      extract_pdf_urls(page_html),
      extract_attr_urls(page_html),
      extract_all_urls(page_html)
    )

    page_pdfs <- normalize_url(page_pdfs, BASE_URL)
    page_pdfs <- unique(page_pdfs)
    page_pdfs <- page_pdfs[grepl("\\.pdf(\\?|$)", page_pdfs, ignore.case = TRUE)]

    if (length(page_pdfs) == 0) {
      log_msg("No PDF found in candidate page.")
      next
    }

    relevant_pdfs <- page_pdfs[
      grepl("sitrep|mvb|mve|ebola|maladie|virus|situation", page_pdfs, ignore.case = TRUE)
    ]

    if (length(relevant_pdfs) > 0) {
      page_pdfs <- relevant_pdfs
    }

    before_n <- nrow(pdf_candidates)

    pdf_candidates <- add_pdf_candidates(
      existing_df = pdf_candidates,
      urls = page_pdfs,
      page_url = page_url,
      source_rank = 1000 + i,
      base_url = BASE_URL
    )

    added_n <- nrow(pdf_candidates) - before_n
    log_msg("PDF candidates added from page:", added_n)
  }
}

if (nrow(pdf_candidates) > 0) {
  write_csv_safe(pdf_candidates, DEBUG_PDFS_FILE)
}

# ============================================================
# CHOOSE LATEST PDF
# ============================================================

latest_row <- choose_latest_pdf(pdf_candidates)

if (is.null(latest_row) || nrow(latest_row) == 0) {
  qc <- data.frame(
    run_time = as.character(Sys.time()),
    status = "no_pdf_found",
    n_all_links = length(all_links),
    n_candidate_pages = length(candidate_links),
    n_pdf_candidates = nrow(pdf_candidates),
    debug_links_file = DEBUG_LINKS_FILE,
    debug_candidates_file = DEBUG_CANDIDATES_FILE,
    debug_pdfs_file = DEBUG_PDFS_FILE,
    stringsAsFactors = FALSE
  )

  write_csv_safe(qc, QC_FILE)

  stop_clean(
    "Aucun PDF SitRep trouve. Debug files written:",
    DEBUG_LINKS_FILE,
    DEBUG_CANDIDATES_FILE,
    DEBUG_PDFS_FILE
  )
}

latest <- list(
  sitrep_no = latest_row$sitrep_no[1],
  sitrep_date = latest_row$sitrep_date[1],
  page_url = latest_row$page_url[1],
  pdf_url = latest_row$pdf_url[1]
)

if (is.na(latest$pdf_url) || !nzchar(latest$pdf_url)) {
  stop_clean("PDF URL selection failed.")
}

log_msg("Latest SitRep page:", latest$page_url)
log_msg("Latest SitRep PDF :", latest$pdf_url)
log_msg("Latest SitRep no  :", ifelse(is.na(latest$sitrep_no), "NA", latest$sitrep_no))
log_msg("Latest SitRep date:", ifelse(is.na(latest$sitrep_date), "NA", as.character(latest$sitrep_date)))

# ============================================================
# CHECK STATE TO AVOID DUPLICATES
# ============================================================

previous <- list(
  sitrep_no = NA_integer_,
  page_url = NA_character_,
  pdf_url = NA_character_,
  sent_at = NA_character_
)

if (file.exists(STATE_FILE)) {
  previous <- tryCatch(readRDS(STATE_FILE), error = function(e) previous)
}

previous_sitrep_no <- if (!is.null(previous$sitrep_no)) previous$sitrep_no else NA_integer_
previous_pdf_url <- if (!is.null(previous$pdf_url)) previous$pdf_url else NA_character_

same_pdf <- identical(previous_pdf_url, latest$pdf_url)

same_or_older_no <- isTRUE(
  !is.na(previous_sitrep_no) &&
    !is.na(latest$sitrep_no) &&
    latest$sitrep_no <= previous_sitrep_no
)

is_new <- FORCE_SEND || !(same_pdf || same_or_older_no)

if (!is_new) {
  log_msg("No new SitRep detected. Email not sent.")

  qc <- data.frame(
    run_time = as.character(Sys.time()),
    status = "no_new_sitrep",
    latest_sitrep_no = latest$sitrep_no,
    latest_page_url = latest$page_url,
    latest_pdf_url = latest$pdf_url,
    previous_sitrep_no = previous_sitrep_no,
    previous_pdf_url = previous_pdf_url,
    email_sent = FALSE,
    stringsAsFactors = FALSE
  )

  write_csv_safe(qc, QC_FILE)

  log_msg("QC written:", QC_FILE)
  quit(save = "no", status = 0)
}

# ============================================================
# DOWNLOAD PDF
# ============================================================

pdf_name <- safe_filename(latest$pdf_url, latest$sitrep_no)
pdf_path <- file.path(INCOMING_DIR, pdf_name)

ok_download <- download_binary(latest$pdf_url, pdf_path)

if (!ok_download) {
  stop_clean("Echec telechargement PDF:", latest$pdf_url)
}

log_msg("PDF downloaded:", pdf_path)
log_msg("PDF size bytes:", file.info(pdf_path)$size)

# ============================================================
# SEND EMAIL
# ============================================================

sitrep_label <- ifelse(is.na(latest$sitrep_no), "nouveau", paste0("N", latest$sitrep_no))

subject <- paste0("PREIS Ebola DRC - Nouveau SitRep ", sitrep_label)

body <- paste(
  "Bonjour,",
  "",
  paste0("Un nouveau SitRep Ebola/MVB RDC a ete detecte automatiquement par PREIS: ", sitrep_label),
  "",
  paste0("Page INSP: ", latest$page_url),
  paste0("PDF source: ", latest$pdf_url),
  paste0("Fichier joint: ", basename(pdf_path)),
  "",
  "Le PDF original est joint a cet email.",
  "",
  "PREIS Ebola DRC automated monitor",
  sep = "\n"
)

if (DRY_RUN) {
  log_msg("DRY RUN active. Email not sent.")

  qc <- data.frame(
    run_time = as.character(Sys.time()),
    status = "dry_run_pdf_downloaded_no_email",
    latest_sitrep_no = latest$sitrep_no,
    latest_page_url = latest$page_url,
    latest_pdf_url = latest$pdf_url,
    pdf_path = pdf_path,
    pdf_size_bytes = file.info(pdf_path)$size,
    email_to = EMAIL_TO,
    email_sent = FALSE,
    stringsAsFactors = FALSE
  )

  write_csv_safe(qc, QC_FILE)
  quit(save = "no", status = 0)
}

email_sent <- send_email_with_attachment(
  to = EMAIL_TO,
  from = EMAIL_FROM,
  subject = subject,
  body = body,
  attachment_path = pdf_path,
  cc = EMAIL_CC,
  bcc = EMAIL_BCC
)

if (!isTRUE(email_sent)) {
  stop_clean("Email non envoye.")
}

log_msg("Email sent successfully to:", EMAIL_TO)

# ============================================================
# UPDATE STATE ONLY AFTER SUCCESSFUL EMAIL
# ============================================================

new_state <- list(
  sitrep_no = latest$sitrep_no,
  sitrep_date = latest$sitrep_date,
  page_url = latest$page_url,
  pdf_url = latest$pdf_url,
  pdf_path = pdf_path,
  sent_at = as.character(Sys.time())
)

saveRDS(new_state, STATE_FILE)

qc <- data.frame(
  run_time = as.character(Sys.time()),
  status = "new_sitrep_sent",
  latest_sitrep_no = latest$sitrep_no,
  latest_sitrep_date = as.character(latest$sitrep_date),
  latest_page_url = latest$page_url,
  latest_pdf_url = latest$pdf_url,
  pdf_path = pdf_path,
  pdf_size_bytes = file.info(pdf_path)$size,
  email_to = EMAIL_TO,
  email_sent = TRUE,
  stringsAsFactors = FALSE
)

write_csv_safe(qc, QC_FILE)

log_msg("State updated:", STATE_FILE)
log_msg("QC written:", QC_FILE)
log_msg("PREIS Ebola DRC cloud monitor completed successfully.")
