############################################################
# PREIS EBOLA DRC SITREP MONITOR
# Script: scripts/08_cloud_sitrep_monitor.R
############################################################

Sys.setenv(TZ = "UTC")

options(stringsAsFactors = FALSE)
options(timeout = 180)
options(warn = 1)

current_repos <- getOption("repos")

if (
  is.null(current_repos) ||
  length(current_repos) == 0 ||
  identical(unname(current_repos[1]), "@CRAN@")
) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

if (nzchar(Sys.getenv("R_LIBS_USER"))) {
  dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(Sys.getenv("R_LIBS_USER"), .libPaths())))
}

preis_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

preis_log <- function(...) {
  message("[", preis_now(), "] ", paste0(..., collapse = ""))
}

is_true_env <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x %in% c("true", "1", "yes", "y", "oui")
}

first_non_empty <- function(...) {
  values <- c(...)
  values <- values[!is.na(values)]
  values <- values[nzchar(trimws(values))]

  if (length(values) == 0) {
    return("")
  }

  values[[1]]
}

parse_email_vector <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) {
    return(character(0))
  }

  x <- gsub(";", ",", x, fixed = TRUE)
  out <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  out <- trimws(out)
  out <- out[nzchar(out)]
  unique(out)
}

sanitize_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

safe_header <- function(x) {
  x <- as.character(x)
  x <- gsub("[\r\n]+", " ", x)
  trimws(x)
}

split_base64_lines <- function(x, width = 76) {
  x <- as.character(x)

  if (!nzchar(x)) {
    return("")
  }

  starts <- seq(1, nchar(x), by = width)
  ends <- pmin(starts + width - 1, nchar(x))

  paste(substring(x, starts, ends), collapse = "\r\n")
}

install_and_load_packages <- function() {
  required_packages <- c(
    "curl",
    "xml2",
    "openssl"
  )

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    preis_log(
      "Installing missing R packages: ",
      paste(missing_packages, collapse = ", ")
    )

    install.packages(
      missing_packages,
      dependencies = c("Depends", "Imports", "LinkingTo"),
      Ncpus = 2
    )
  }

  failed_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(failed_packages) > 0) {
    stop(
      "Missing R packages after installation: ",
      paste(failed_packages, collapse = ", "),
      "\nRequired Linux packages include libcurl4-openssl-dev, ",
      "libssl-dev, and libxml2-dev."
    )
  }

  suppressPackageStartupMessages({
    library(curl)
    library(xml2)
    library(openssl)
  })

  preis_log("All required R packages are available.")
}

install_and_load_packages()

INSP_BASE <- "https://insp.cd"

USER_AGENT <- paste(
  "PREIS-Ebola-DRC-SitRep-Monitor/1.0",
  "(GitHub Actions; contact: PREIS)"
)

SMTP_USER <- Sys.getenv("SMTP_USER")
SMTP_PASS <- Sys.getenv("SMTP_PASS")
ALERT_FROM <- first_non_empty(Sys.getenv("ALERT_FROM"), SMTP_USER)

SMTP_HOST <- first_non_empty(Sys.getenv("SMTP_HOST"), "smtp.gmail.com")
SMTP_PORT <- suppressWarnings(as.integer(first_non_empty(Sys.getenv("SMTP_PORT"), "465")))

ALERT_TO <- parse_email_vector(
  first_non_empty(Sys.getenv("ALERT_TO"), SMTP_USER)
)

ALERT_CC <- parse_email_vector(Sys.getenv("ALERT_CC"))
ALERT_BCC <- parse_email_vector(Sys.getenv("ALERT_BCC"))

PREIS_DRY_RUN <- is_true_env(Sys.getenv("PREIS_DRY_RUN", "false"))

PREIS_MAX_PAGES <- suppressWarnings(
  as.integer(first_non_empty(Sys.getenv("PREIS_MAX_PAGES"), "50"))
)

if (is.na(PREIS_MAX_PAGES) || PREIS_MAX_PAGES < 5) {
  PREIS_MAX_PAGES <- 50
}

STATE_DIR <- "data/monitor_state"
PDF_DIR <- "data/pdf"
LOG_DIR <- "data/logs"

STATE_FILE <- file.path(STATE_DIR, "preis_sitrep_email_state.csv")

dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PDF_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

preis_log("PREIS Ebola DRC SitRep monitor started.")
preis_log("Dry run mode: ", PREIS_DRY_RUN)
preis_log("SMTP host: ", SMTP_HOST)
preis_log("SMTP port: ", SMTP_PORT)
preis_log("Alert recipients: ", paste(ALERT_TO, collapse = ", "))
preis_log("R library paths: ", paste(.libPaths(), collapse = " | "))

empty_state <- function() {
  data.frame(
    run_id = character(),
    detected_at_utc = character(),
    sent_at_utc = character(),
    sitrep_no = character(),
    sitrep_date = character(),
    page_url = character(),
    pdf_url = character(),
    pdf_sha256 = character(),
    pdf_file = character(),
    email_status = character(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) {
    return(empty_state())
  }

  out <- tryCatch(
    {
      read.csv(
        STATE_FILE,
        stringsAsFactors = FALSE,
        colClasses = "character"
      )
    },
    error = function(e) {
      preis_log("State file exists but could not be read: ", conditionMessage(e))
      empty_state()
    }
  )

  required_cols <- names(empty_state())
  missing_cols <- setdiff(required_cols, names(out))

  if (length(missing_cols) > 0) {
    for (nm in missing_cols) {
      out[[nm]] <- NA_character_
    }
  }

  out <- out[, required_cols, drop = FALSE]
  out
}

write_state <- function(state) {
  required_cols <- names(empty_state())
  state <- state[, required_cols, drop = FALSE]

  write.csv(
    state,
    STATE_FILE,
    row.names = FALSE,
    na = ""
  )

  preis_log("State file updated: ", STATE_FILE)
}

clean_url <- function(x, base_url = INSP_BASE) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }

  x <- as.character(x)
  x <- trimws(x)
  x[!nzchar(x)] <- NA_character_

  idx <- !is.na(x)

  if (any(idx)) {
    y <- x[idx]
    y <- gsub("&amp;", "&", y, fixed = TRUE)
    y <- gsub("\\\\/", "/", y)
    y <- gsub("[\"'<>]+$", "", y)
    y <- gsub("^[\"'<>]+", "", y)
    y <- gsub("#.*$", "", y)

    y <- tryCatch(
      xml2::url_absolute(y, base_url),
      error = function(e) y
    )

    x[idx] <- y
  }

  x
}

is_pdf_url <- function(x) {
  x <- tolower(as.character(x))
  out <- grepl("\\.pdf($|[?&#])", x)
  out[is.na(out)] <- FALSE
  out
}

is_relevant_sitrep_text <- function(x) {
  x <- tolower(paste(x, collapse = " "))

  has_sitrep <- grepl("sitrep|situation|rapport", x)
  has_ebola <- grepl(
    "ebola|mvb|maladie.?a.?virus.?ebola|maladie.?virus.?ebola",
    x
  )

  has_sitrep || has_ebola
}

extract_pdf_from_viewer_url <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }

  x <- as.character(x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x_decoded <- utils::URLdecode(x)

  out <- character(0)

  pattern_query <- "(?:file|url|src)=([^&]+\\.pdf[^&]*)"
  hits_query <- gregexpr(pattern_query, x_decoded, ignore.case = TRUE, perl = TRUE)
  matches_query <- regmatches(x_decoded, hits_query)

  for (one_set in matches_query) {
    if (length(one_set) > 0) {
      extracted <- sub(pattern_query, "\\1", one_set, ignore.case = TRUE, perl = TRUE)
      out <- c(out, extracted)
    }
  }

  pattern_direct <- "https?://[^\"'<>[:space:]]+\\.pdf[^\"'<>[:space:]]*"
  hits_direct <- gregexpr(pattern_direct, x_decoded, ignore.case = TRUE, perl = TRUE)
  matches_direct <- regmatches(x_decoded, hits_direct)

  for (one_set in matches_direct) {
    if (length(one_set) > 0) {
      out <- c(out, one_set)
    }
  }

  out <- clean_url(out)
  out <- out[!is.na(out) & nzchar(out)]
  out <- out[is_pdf_url(out)]
  unique(out)
}

parse_sitrep_no <- function(x) {
  x <- tolower(paste(x, collapse = " "))
  x <- utils::URLdecode(x)
  x <- gsub("[_\\-]+", " ", x)

  patterns <- c(
    "sitrep\\s*n\\s*([0-9]{1,4})",
    "sitrep\\s*no\\s*([0-9]{1,4})",
    "sitrep\\s*numero\\s*([0-9]{1,4})",
    "sitrep\\s*([0-9]{1,4})",
    "\\bn\\s*([0-9]{1,4})\\s*mvb",
    "\\bno\\s*([0-9]{1,4})\\s*mvb"
  )

  for (p in patterns) {
    m <- regexec(p, x, ignore.case = TRUE, perl = TRUE)
    r <- regmatches(x, m)

    if (length(r) > 0 && length(r[[1]]) >= 2) {
      val <- suppressWarnings(as.integer(r[[1]][2]))

      if (!is.na(val)) {
        return(val)
      }
    }
  }

  NA_integer_
}

parse_date_from_text <- function(x) {
  x <- tolower(paste(x, collapse = " "))
  x <- utils::URLdecode(x)
  x <- gsub("[_/\\.]+", "-", x)

  p1 <- "\\b([0-3][0-9])-([0-1][0-9])-(20[0-9]{2})\\b"
  m1 <- regexec(p1, x, perl = TRUE)
  r1 <- regmatches(x, m1)

  if (length(r1) > 0 && length(r1[[1]]) >= 4) {
    dd <- suppressWarnings(as.integer(r1[[1]][2]))
    mm <- suppressWarnings(as.integer(r1[[1]][3]))
    yy <- suppressWarnings(as.integer(r1[[1]][4]))

    out <- suppressWarnings(
      as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
    )

    if (!is.na(out)) {
      return(out)
    }
  }

  p2 <- "\\b(20[0-9]{2})-([0-1][0-9])-([0-3][0-9])\\b"
  m2 <- regexec(p2, x, perl = TRUE)
  r2 <- regmatches(x, m2)

  if (length(r2) > 0 && length(r2[[1]]) >= 4) {
    yy <- suppressWarnings(as.integer(r2[[1]][2]))
    mm <- suppressWarnings(as.integer(r2[[1]][3]))
    dd <- suppressWarnings(as.integer(r2[[1]][4]))

    out <- suppressWarnings(
      as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
    )

    if (!is.na(out)) {
      return(out)
    }
  }

  as.Date(NA)
}

parse_date_chr <- function(x) {
  d <- parse_date_from_text(x)

  if (is.na(d)) {
    return(NA_character_)
  }

  as.character(d)
}

http_get_raw <- function(url, accept = "text/html,application/xhtml+xml,application/pdf,*/*") {
  h <- curl::new_handle()

  curl::handle_setopt(
    h,
    timeout = 60,
    connecttimeout = 30,
    followlocation = TRUE,
    useragent = USER_AGENT
  )

  curl::handle_setheaders(
    h,
    Accept = accept
  )

  resp <- curl::curl_fetch_memory(url, handle = h)
  status <- resp$status_code

  if (status < 200 || status >= 300) {
    stop("HTTP status ", status, " for URL: ", url)
  }

  resp$content
}

read_html_safe <- function(url) {
  preis_log("Reading page: ", url)

  out <- tryCatch(
    {
      raw <- http_get_raw(url)
      html <- rawToChar(raw)
      xml2::read_html(html, encoding = "UTF-8")
    },
    error = function(e) {
      preis_log("Could not read page: ", url, " | ", conditionMessage(e))
      NULL
    }
  )

  out
}

download_file_safe <- function(url, destfile) {
  preis_log("Downloading PDF candidate: ", url)

  raw <- tryCatch(
    {
      http_get_raw(url, accept = "application/pdf,*/*")
    },
    error = function(e) {
      stop("PDF download failed: ", conditionMessage(e))
    }
  )

  if (length(raw) < 10) {
    stop("Downloaded file is too small and is not a valid PDF.")
  }

  writeBin(raw, destfile)

  header <- readBin(destfile, what = "raw", n = 5)
  header_txt <- rawToChar(header)

  if (!identical(header_txt, "%PDF-")) {
    stop(
      "Downloaded file is not a valid PDF. First bytes are: ",
      header_txt
    )
  }

  invisible(destfile)
}

get_seed_urls <- function() {
  unique(c(
    INSP_BASE,
    paste0(INSP_BASE, "/"),
    paste0(INSP_BASE, "/?s=sitrep"),
    paste0(INSP_BASE, "/?s=SitRep"),
    paste0(INSP_BASE, "/?s=MVB"),
    paste0(INSP_BASE, "/?s=Ebola"),
    paste0(INSP_BASE, "/?s=maladie+virus+Ebola"),
    paste0(INSP_BASE, "/?s=situation+epidemiologique")
  ))
}

extract_links_from_html <- function(doc, page_url) {
  if (is.null(doc)) {
    return(data.frame())
  }

  nodes <- xml2::xml_find_all(doc, ".//a[@href]")

  if (length(nodes) == 0) {
    return(data.frame())
  }

  href <- xml2::xml_attr(nodes, "href")
  text <- xml2::xml_text(nodes, trim = TRUE)

  href <- clean_url(href, base_url = page_url)

  keep <- !is.na(href) & nzchar(href)

  if (!any(keep)) {
    return(data.frame())
  }

  data.frame(
    source_page = rep(page_url, sum(keep)),
    link_url = href[keep],
    link_text = text[keep],
    stringsAsFactors = FALSE
  )
}

extract_pdf_urls_from_html <- function(doc, page_url) {
  if (is.null(doc)) {
    return(character(0))
  }

  html_txt <- as.character(doc)
  html_txt <- gsub("&amp;", "&", html_txt, fixed = TRUE)
  html_txt_decoded <- utils::URLdecode(html_txt)

  direct_regex <- "https?://[^\"'<>[:space:]]+\\.pdf[^\"'<>[:space:]]*"

  hits <- gregexpr(
    direct_regex,
    html_txt_decoded,
    ignore.case = TRUE,
    perl = TRUE
  )

  direct_pdf <- unlist(regmatches(html_txt_decoded, hits), use.names = FALSE)

  attr_nodes <- xml2::xml_find_all(
    doc,
    ".//*[@href or @src or @data or @data-src or @data-url or @data-file]"
  )

  attrs <- character(0)

  if (length(attr_nodes) > 0) {
    attrs <- c(
      xml2::xml_attr(attr_nodes, "href"),
      xml2::xml_attr(attr_nodes, "src"),
      xml2::xml_attr(attr_nodes, "data"),
      xml2::xml_attr(attr_nodes, "data-src"),
      xml2::xml_attr(attr_nodes, "data-url"),
      xml2::xml_attr(attr_nodes, "data-file")
    )
  }

  attrs <- attrs[!is.na(attrs)]
  attrs <- clean_url(attrs, base_url = page_url)
  attrs <- attrs[!is.na(attrs) & nzchar(attrs)]

  viewer_pdf <- extract_pdf_from_viewer_url(attrs)

  out <- unique(c(
    clean_url(direct_pdf, base_url = page_url),
    attrs[is_pdf_url(attrs)],
    viewer_pdf
  ))

  out <- out[!is.na(out) & nzchar(out)]
  out <- out[is_pdf_url(out)]
  unique(out)
}

discover_candidate_pages <- function() {
  seed_urls <- get_seed_urls()
  all_links <- data.frame()

  for (u in seed_urls) {
    doc <- read_html_safe(u)
    links <- extract_links_from_html(doc, u)

    if (nrow(links) > 0) {
      all_links <- rbind(all_links, links)
    }
  }

  seed_tbl <- data.frame(
    page_url = seed_urls,
    page_text = seed_urls,
    source_page = seed_urls,
    stringsAsFactors = FALSE
  )

  if (nrow(all_links) == 0) {
    preis_log("No links found on INSP seed pages.")
    return(seed_tbl)
  }

  combined_text <- paste(all_links$link_text, all_links$link_url)
  relevant <- vapply(combined_text, is_relevant_sitrep_text, logical(1))

  all_links <- all_links[relevant, , drop = FALSE]

  if (nrow(all_links) == 0) {
    return(seed_tbl)
  }

  link_tbl <- data.frame(
    page_url = all_links$link_url,
    page_text = all_links$link_text,
    source_page = all_links$source_page,
    stringsAsFactors = FALSE
  )

  out <- rbind(seed_tbl, link_tbl)
  out <- out[grepl("^https?://", out$page_url), , drop = FALSE]
  out <- out[!duplicated(out$page_url), , drop = FALSE]

  out
}

sort_pages <- function(pages) {
  pages$page_sitrep_no <- vapply(
    paste(pages$page_text, pages$page_url),
    parse_sitrep_no,
    integer(1)
  )

  pages$page_sitrep_date_chr <- vapply(
    paste(pages$page_text, pages$page_url),
    parse_date_chr,
    character(1)
  )

  pages$page_sitrep_date <- suppressWarnings(
    as.Date(pages$page_sitrep_date_chr)
  )

  date_key <- as.numeric(pages$page_sitrep_date)
  date_key[is.na(date_key)] <- -Inf

  no_key <- pages$page_sitrep_no
  no_key[is.na(no_key)] <- -Inf

  ord <- order(date_key, no_key, decreasing = TRUE)

  pages[ord, , drop = FALSE]
}

discover_pdf_candidates <- function() {
  pages <- discover_candidate_pages()

  if (nrow(pages) == 0) {
    stop("No candidate SitRep pages were found.")
  }

  pages <- sort_pages(pages)

  if (nrow(pages) > PREIS_MAX_PAGES) {
    pages <- pages[seq_len(PREIS_MAX_PAGES), , drop = FALSE]
  }

  preis_log("Candidate pages to inspect: ", nrow(pages))

  pdfs <- data.frame()

  for (i in seq_len(nrow(pages))) {
    page_url <- pages$page_url[[i]]
    page_text <- pages$page_text[[i]]

    if (is_pdf_url(page_url)) {
      pdf_urls <- page_url
    } else {
      doc <- read_html_safe(page_url)
      pdf_urls <- extract_pdf_urls_from_html(doc, page_url)
    }

    if (length(pdf_urls) > 0) {
      tmp <- data.frame(
        source_order = rep(i, length(pdf_urls)),
        page_url = rep(page_url, length(pdf_urls)),
        page_text = rep(page_text, length(pdf_urls)),
        pdf_url = pdf_urls,
        stringsAsFactors = FALSE
      )

      pdfs <- rbind(pdfs, tmp)
    }
  }

  if (nrow(pdfs) == 0) {
    stop(
      "No PDF SitRep candidate was found. ",
      "Check the INSP website structure or search keywords."
    )
  }

  pdfs$combined_text <- paste(
    pdfs$page_text,
    pdfs$page_url,
    pdfs$pdf_url,
    sep = " "
  )

  pdfs$sitrep_no <- vapply(
    pdfs$combined_text,
    parse_sitrep_no,
    integer(1)
  )

  pdfs$sitrep_date_chr <- vapply(
    pdfs$combined_text,
    parse_date_chr,
    character(1)
  )

  pdfs$sitrep_date <- suppressWarnings(as.Date(pdfs$sitrep_date_chr))

  pdfs$relevant <- vapply(
    pdfs$combined_text,
    is_relevant_sitrep_text,
    logical(1)
  )

  pdfs <- pdfs[pdfs$relevant, , drop = FALSE]

  if (nrow(pdfs) == 0) {
    stop("PDFs were found, but none matched Ebola/MVB/SitRep keywords.")
  }

  pdfs <- pdfs[!duplicated(pdfs$pdf_url), , drop = FALSE]

  date_key <- as.numeric(pdfs$sitrep_date)
  date_key[is.na(date_key)] <- -Inf

  no_key <- pdfs$sitrep_no
  no_key[is.na(no_key)] <- -Inf

  source_key <- -pdfs$source_order

  ord <- order(date_key, no_key, source_key, decreasing = TRUE)
  pdfs <- pdfs[ord, , drop = FALSE]

  rownames(pdfs) <- NULL
  pdfs
}

select_latest_candidate <- function(candidates) {
  candidates[1, , drop = FALSE]
}

prepare_pdf_filename <- function(candidate, pdf_hash = NULL) {
  sitrep_no <- candidate$sitrep_no[[1]]
  sitrep_date <- candidate$sitrep_date[[1]]

  no_part <- if (!is.na(sitrep_no)) {
    paste0("N", sitrep_no)
  } else {
    "N_unknown"
  }

  date_part <- if (!is.na(sitrep_date)) {
    as.character(sitrep_date)
  } else {
    format(Sys.Date(), "%Y-%m-%d")
  }

  hash_part <- if (!is.null(pdf_hash) && nzchar(pdf_hash)) {
    substr(pdf_hash, 1, 10)
  } else {
    format(Sys.time(), "%Y%m%d%H%M%S")
  }

  fname <- paste0(
    "PREIS_Ebola_DRC_SitRep_",
    no_part,
    "_",
    date_part,
    "_",
    hash_part,
    ".pdf"
  )

  file.path(PDF_DIR, sanitize_filename(fname))
}

download_selected_pdf <- function(candidate) {
  tmp_file <- tempfile(fileext = ".pdf")
  download_file_safe(candidate$pdf_url[[1]], tmp_file)

  pdf_raw <- readBin(
    tmp_file,
    what = "raw",
    n = file.info(tmp_file)$size
  )

  pdf_hash <- as.character(openssl::sha256(pdf_raw))
  final_file <- prepare_pdf_filename(candidate, pdf_hash)

  file.copy(tmp_file, final_file, overwrite = TRUE)
  unlink(tmp_file)

  if (!file.exists(final_file)) {
    stop("PDF was downloaded but could not be saved to: ", final_file)
  }

  list(
    pdf_file = final_file,
    pdf_sha256 = pdf_hash
  )
}

validate_email_config <- function() {
  if (PREIS_DRY_RUN) {
    preis_log("Dry run mode is TRUE. SMTP validation skipped.")
    return(invisible(TRUE))
  }

  if (!nzchar(SMTP_USER)) {
    stop("SMTP_USER is missing. Add it as a GitHub secret.")
  }

  if (!nzchar(SMTP_PASS)) {
    stop("SMTP_PASS is missing. Add it as a GitHub secret.")
  }

  if (!nzchar(ALERT_FROM)) {
    stop("ALERT_FROM is missing. Add it as a GitHub secret.")
  }

  if (length(ALERT_TO) == 0) {
    stop("No recipient found. Add ALERT_TO or set SMTP_USER.")
  }

  if (is.na(SMTP_PORT)) {
    stop("SMTP_PORT is invalid.")
  }

  invisible(TRUE)
}

build_email_body <- function(candidate, pdf_file, pdf_sha256) {
  sitrep_no <- candidate$sitrep_no[[1]]
  sitrep_date <- candidate$sitrep_date[[1]]

  sitrep_no_txt <- if (!is.na(sitrep_no)) {
    paste0("SitRep N", sitrep_no)
  } else {
    "SitRep number not automatically identified"
  }

  sitrep_date_txt <- if (!is.na(sitrep_date)) {
    as.character(sitrep_date)
  } else {
    "Date not automatically identified"
  }

  paste(
    "Dear team,",
    "",
    "PREIS Ebola DRC has detected a newly available SitRep PDF from INSP DRC.",
    "",
    paste0("Detected document: ", sitrep_no_txt),
    paste0("SitRep date: ", sitrep_date_txt),
    paste0("Detection time: ", preis_now()),
    "",
    paste0("Source page: ", candidate$page_url[[1]]),
    paste0("PDF URL: ", candidate$pdf_url[[1]]),
    paste0("PDF SHA256: ", pdf_sha256),
    "",
    "The PDF is attached exactly as downloaded from the official source.",
    "",
    "This is an automated PREIS Ebola DRC SitRep monitor notification.",
    sep = "\r\n"
  )
}

build_smtp_server <- function() {
  if (SMTP_PORT == 465) {
    return(paste0("smtps://", SMTP_HOST, ":", SMTP_PORT))
  }

  paste0("smtp://", SMTP_HOST, ":", SMTP_PORT)
}

build_mime_email <- function(candidate, pdf_file, pdf_sha256, subject) {
  boundary <- paste0(
    "----PREIS_EBOLA_DRC_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    substr(pdf_sha256, 1, 10)
  )

  body_txt <- build_email_body(candidate, pdf_file, pdf_sha256)

  pdf_raw <- readBin(
    pdf_file,
    what = "raw",
    n = file.info(pdf_file)$size
  )

  pdf_b64 <- openssl::base64_encode(pdf_raw)
  pdf_b64 <- split_base64_lines(pdf_b64)

  attachment_name <- basename(pdf_file)

  headers <- c(
    paste0("From: ", safe_header(ALERT_FROM)),
    paste0("To: ", safe_header(paste(ALERT_TO, collapse = ", "))),
    if (length(ALERT_CC) > 0) {
      paste0("Cc: ", safe_header(paste(ALERT_CC, collapse = ", ")))
    },
    paste0("Subject: ", safe_header(subject)),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\"")
  )

  message_lines <- c(
    headers,
    "",
    paste0("--", boundary),
    "Content-Type: text/plain; charset=\"UTF-8\"",
    "Content-Transfer-Encoding: 8bit",
    "",
    body_txt,
    "",
    paste0("--", boundary),
    paste0("Content-Type: application/pdf; name=\"", attachment_name, "\""),
    "Content-Transfer-Encoding: base64",
    paste0("Content-Disposition: attachment; filename=\"", attachment_name, "\""),
    "",
    pdf_b64,
    "",
    paste0("--", boundary, "--"),
    ""
  )

  paste(message_lines, collapse = "\r\n")
}

send_sitrep_email <- function(candidate, pdf_file, pdf_sha256) {
  validate_email_config()

  sitrep_no <- candidate$sitrep_no[[1]]
  sitrep_date <- candidate$sitrep_date[[1]]

  subject_no <- if (!is.na(sitrep_no)) {
    paste0("N", sitrep_no)
  } else {
    "new"
  }

  subject_date <- if (!is.na(sitrep_date)) {
    paste0(" - ", as.character(sitrep_date))
  } else {
    ""
  }

  subject <- paste0(
    "PREIS Ebola DRC - New SitRep detected - ",
    subject_no,
    subject_date
  )

  if (PREIS_DRY_RUN) {
    preis_log("Dry run mode: email not sent.")
    preis_log("Email subject would be: ", subject)
    preis_log("PDF attachment would be: ", pdf_file)
    return("dry_run")
  }

  message <- build_mime_email(
    candidate = candidate,
    pdf_file = pdf_file,
    pdf_sha256 = pdf_sha256,
    subject = subject
  )

  smtp_server <- build_smtp_server()
  recipients <- unique(c(ALERT_TO, ALERT_CC, ALERT_BCC))

  preis_log("Sending email through: ", smtp_server)
  preis_log("Recipients: ", paste(recipients, collapse = ", "))

  curl::send_mail(
    mail_from = ALERT_FROM,
    mail_rcpt = recipients,
    message = message,
    smtp_server = smtp_server,
    use_ssl = if (SMTP_PORT == 465) "force" else "try",
    verbose = TRUE,
    username = SMTP_USER,
    password = SMTP_PASS
  )

  preis_log("Email sent successfully to: ", paste(ALERT_TO, collapse = ", "))

  "sent"
}

main <- function() {
  state <- read_state()

  candidates <- discover_pdf_candidates()

  preis_log("PDF candidates found: ", nrow(candidates))

  latest <- select_latest_candidate(candidates)

  preis_log("Selected candidate page: ", latest$page_url[[1]])
  preis_log("Selected candidate PDF: ", latest$pdf_url[[1]])
  preis_log("Selected SitRep number: ", latest$sitrep_no[[1]])
  preis_log("Selected SitRep date: ", latest$sitrep_date[[1]])

  already_sent_by_url <- FALSE

  if (nrow(state) > 0) {
    already_sent_by_url <- any(
      state$pdf_url == latest$pdf_url[[1]] &
        state$email_status %in% c("sent", "dry_run"),
      na.rm = TRUE
    )
  }

  if (already_sent_by_url) {
    preis_log("This PDF URL was already processed. No email sent.")
    return(invisible(TRUE))
  }

  downloaded <- download_selected_pdf(latest)

  pdf_file <- downloaded$pdf_file
  pdf_sha256 <- downloaded$pdf_sha256

  preis_log("Downloaded PDF saved at: ", pdf_file)
  preis_log("PDF SHA256: ", pdf_sha256)

  already_sent_by_hash <- FALSE

  if (nrow(state) > 0) {
    already_sent_by_hash <- any(
      state$pdf_sha256 == pdf_sha256 &
        state$email_status %in% c("sent", "dry_run"),
      na.rm = TRUE
    )
  }

  if (already_sent_by_hash) {
    preis_log("This exact PDF hash was already processed. No email sent.")
    return(invisible(TRUE))
  }

  email_status <- "failed"
  note <- NA_character_

  email_status <- tryCatch(
    {
      send_sitrep_email(latest, pdf_file, pdf_sha256)
    },
    error = function(e) {
      note <<- conditionMessage(e)
      preis_log("Email sending failed: ", note)
      "failed"
    }
  )

  new_row <- data.frame(
    run_id = first_non_empty(Sys.getenv("GITHUB_RUN_ID"), "local"),
    detected_at_utc = preis_now(),
    sent_at_utc = if (email_status %in% c("sent", "dry_run")) {
      preis_now()
    } else {
      NA_character_
    },
    sitrep_no = as.character(latest$sitrep_no[[1]]),
    sitrep_date = as.character(latest$sitrep_date[[1]]),
    page_url = latest$page_url[[1]],
    pdf_url = latest$pdf_url[[1]],
    pdf_sha256 = pdf_sha256,
    pdf_file = pdf_file,
    email_status = email_status,
    note = note,
    stringsAsFactors = FALSE
  )

  state <- rbind(state, new_row)

  state_key <- paste(state$pdf_sha256, state$email_status)
  state <- state[!duplicated(state_key), , drop = FALSE]

  write_state(state)

  if (identical(email_status, "failed")) {
    stop(
      "SitRep detected and PDF downloaded, but email sending failed: ",
      note
    )
  }

  preis_log("Monitor completed successfully. Status: ", email_status)

  invisible(TRUE)
}

tryCatch(
  {
    main()
  },
  error = function(e) {
    preis_log("MONITOR FAILED: ", conditionMessage(e))
    stop(e)
  }
)
