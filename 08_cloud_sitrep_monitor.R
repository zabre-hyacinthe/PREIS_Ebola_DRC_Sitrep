# ============================================================
# PREIS EBOLA DRC — CLOUD SITREP MONITOR FOR GITHUB ACTIONS
# Runs every 5 minutes via .github/workflows/preis_sitrep_monitor.yml
# Purpose:
# 1) Run existing PREIS production pipeline when available
# 2) Find latest valid SitRep PDF
# 3) Read recipients from alert_recipients.csv at repository root
# 4) Send latest PDF by Gmail SMTP/blastula if not already sent
# 5) Persist sent-state in data/monitor_state/preis_sitrep_email_state.csv
# ============================================================

options(warn = 1)

required_packages <- c("blastula", "readr", "rvest", "xml2", "httr2", "stringr", "dplyr", "tibble", "purrr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(blastula)
  library(readr)
  library(rvest)
  library(xml2)
  library(httr2)
  library(stringr)
  library(dplyr)
  library(tibble)
  library(purrr)
})

ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

DIR_PDF <- file.path(ROOT_DIR, "data", "pdf")
DIR_INCOMING <- file.path(ROOT_DIR, "data", "incoming", "insp_sitreps")
DIR_STATE <- file.path(ROOT_DIR, "data", "monitor_state")
DIR_LOG <- file.path(ROOT_DIR, "logs")

dir.create(DIR_PDF, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_INCOMING, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_STATE, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOG, recursive = TRUE, showWarnings = FALSE)

STATE_FILE <- file.path(DIR_STATE, "preis_sitrep_email_state.csv")
RECIPIENTS_FILE <- file.path(ROOT_DIR, "alert_recipients.csv")
LOG_FILE <- file.path(DIR_LOG, paste0("preis_cloud_sitrep_monitor_", format(Sys.Date(), "%Y%m%d"), ".log"))

CATEGORY_URLS <- c(
  "https://insp.cd/category/sitrep/",
  "https://insp.cd/category/sitrep/page/2/",
  "https://insp.cd/category/sitrep/page/3/"
)

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG_FILE, append = TRUE)
}

safe_chr <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return("")
  as.character(x[1])
}

is_truthy <- function(x) {
  tolower(trimws(as.character(x))) %in% c("true", "1", "yes", "y", "oui", "active")
}

is_valid_pdf_file <- function(path) {
  if (length(path) == 0 || is.na(path[1])) return(FALSE)
  if (!file.exists(path)) return(FALSE)
  size <- file.info(path)$size
  if (is.na(size) || size < 10000) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  header <- readBin(con, what = "raw", n = 5)
  identical(header, charToRaw("%PDF-"))
}

extract_sitrep_no <- function(x) {
  x <- safe_chr(x)
  patterns <- c(
    "sitrep\\s*n\\s*[°ºo]*\\s*0*([0-9]{1,3})",
    "sitrep[-_]?n?0*([0-9]{1,3})",
    "n\\s*[°ºo]*\\s*0*([0-9]{1,3})"
  )
  for (p in patterns) {
    m <- regexec(p, x, ignore.case = TRUE, perl = TRUE)
    r <- regmatches(x, m)[[1]]
    if (length(r) >= 2) return(as.integer(r[2]))
  }
  NA_integer_
}

absolute_url_one <- function(x, base = "https://insp.cd") {
  x <- safe_chr(x)
  if (x == "") return(NA_character_)
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
  if (startsWith(x, "//")) return(paste0("https:", x))
  if (startsWith(x, "/")) return(paste0(base, x))
  paste0(base, "/", x)
}

fetch_html_safely <- function(url) {
  tryCatch({
    req <- httr2::request(url)
    req <- httr2::req_user_agent(req, "PREIS Ebola DRC GitHub Actions Monitor")
    req <- httr2::req_timeout(req, 60)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 400) stop("HTTP ", httr2::resp_status(resp))
    httr2::resp_body_string(resp)
  }, error = function(e) {
    log_msg("ERROR fetch:", url, "|", conditionMessage(e))
    NA_character_
  })
}

get_sitrep_posts <- function() {
  posts <- list()

  for (url in CATEGORY_URLS) {
    log_msg("Reading category:", url)
    html_txt <- fetch_html_safely(url)
    if (is.na(html_txt)) next

    page <- tryCatch(xml2::read_html(html_txt), error = function(e) NULL)
    if (is.null(page)) next

    nodes <- rvest::html_elements(page, "a")
    titles <- rvest::html_text2(nodes)
    hrefs <- rvest::html_attr(nodes, "href")
    if (length(titles) == 0 || length(hrefs) == 0) next

    df <- data.frame(title = trimws(titles), href = hrefs, stringsAsFactors = FALSE)
    df$href <- vapply(df$href, absolute_url_one, character(1))

    keep <- !is.na(df$href) & (
      grepl("/sitrep-", df$href, ignore.case = TRUE) |
        grepl("sitrep", df$title, ignore.case = TRUE) |
        grepl("sitrep", df$href, ignore.case = TRUE)
    )
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) next

    df$sitrep_no <- vapply(paste(df$title, df$href), extract_sitrep_no, integer(1))
    df <- df[!is.na(df$sitrep_no), , drop = FALSE]
    if (nrow(df) == 0) next

    posts[[length(posts) + 1]] <- data.frame(
      sitrep_no = as.integer(df$sitrep_no),
      title = df$title,
      post_url = df$href,
      stringsAsFactors = FALSE
    )
  }

  if (length(posts) == 0) {
    return(data.frame(sitrep_no = integer(), title = character(), post_url = character(), stringsAsFactors = FALSE))
  }

  out <- do.call(rbind, posts)
  out <- out[!duplicated(paste(out$sitrep_no, out$post_url)), , drop = FALSE]
  out <- out[order(out$sitrep_no, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

find_latest_local_pdf <- function(sitrep_no) {
  roots <- unique(c(DIR_PDF, DIR_INCOMING, file.path(ROOT_DIR, "data")))
  roots <- roots[dir.exists(roots)]

  all_pdfs <- unique(unlist(lapply(roots, function(root) {
    list.files(root, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  }), use.names = FALSE))

  if (length(all_pdfs) == 0) return(NA_character_)

  b <- tolower(basename(all_pdfs))
  score <- rep(0, length(all_pdfs))

  score[b == paste0("sitrep_", sitrep_no, "_2026.pdf")] <- score[b == paste0("sitrep_", sitrep_no, "_2026.pdf")] + 1000
  score[grepl(paste0("sitrep.*", sitrep_no), b)] <- score[grepl(paste0("sitrep.*", sitrep_no), b)] + 500
  score[grepl(paste0("n", sitrep_no), b)] <- score[grepl(paste0("n", sitrep_no), b)] + 200

  candidates <- all_pdfs[score > 0]
  if (length(candidates) == 0) return(NA_character_)

  candidates <- candidates[order(score[score > 0], decreasing = TRUE)]
  valid <- candidates[vapply(candidates, is_valid_pdf_file, logical(1))]
  if (length(valid) == 0) return(NA_character_)
  valid[1]
}

run_existing_pipeline <- function() {
  pipeline_file <- file.path(ROOT_DIR, "00_RUN_ALL_PRODUCTION.R")
  if (!file.exists(pipeline_file)) {
    log_msg("Production pipeline not found; skipping:", pipeline_file)
    return(FALSE)
  }

  log_msg("Running production pipeline:", pipeline_file)
  tryCatch({
    source(pipeline_file, local = globalenv())
    TRUE
  }, error = function(e) {
    log_msg("WARNING pipeline error:", conditionMessage(e))
    FALSE
  })
}

read_recipients <- function() {
  if (!file.exists(RECIPIENTS_FILE)) {
    default <- data.frame(
      active = TRUE,
      type = "to",
      name = "Dr Zabre",
      email = Sys.getenv("ALERT_TO", "raogoz@africacdc.org"),
      stringsAsFactors = FALSE
    )
    readr::write_csv(default, RECIPIENTS_FILE)
  }

  rec <- as.data.frame(readr::read_csv(RECIPIENTS_FILE, show_col_types = FALSE), stringsAsFactors = FALSE)
  names(rec) <- tolower(names(rec))

  required <- c("active", "type", "email")
  missing <- setdiff(required, names(rec))
  if (length(missing) > 0) stop("Missing columns in alert_recipients.csv: ", paste(missing, collapse = ", "))

  rec <- rec[is_truthy(rec$active), , drop = FALSE]
  rec$email <- trimws(as.character(rec$email))
  rec$type <- tolower(trimws(as.character(rec$type)))
  rec <- rec[nzchar(rec$email), , drop = FALSE]

  list(
    to = unique(rec$email[rec$type == "to"]),
    cc = unique(rec$email[rec$type == "cc"]),
    bcc = unique(rec$email[rec$type == "bcc"])
  )
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) {
    return(data.frame(
      sitrep_no = integer(),
      title = character(),
      post_url = character(),
      pdf_path = character(),
      status = character(),
      sent_at = character(),
      stringsAsFactors = FALSE
    ))
  }

  x <- as.data.frame(readr::read_csv(STATE_FILE, show_col_types = FALSE), stringsAsFactors = FALSE)
  if (!"sitrep_no" %in% names(x)) x$sitrep_no <- integer()
  x$sitrep_no <- as.integer(x$sitrep_no)
  x
}

write_state <- function(x) {
  dir.create(dirname(STATE_FILE), recursive = TRUE, showWarnings = FALSE)
  x$sent_at <- as.character(x$sent_at)
  x <- x[order(x$sitrep_no, decreasing = TRUE), , drop = FALSE]
  x <- x[!duplicated(paste(x$sitrep_no, x$status)), , drop = FALSE]
  readr::write_csv(x, STATE_FILE)
}

send_sitrep_email <- function(latest, pdf_path, recipients) {
  smtp_user <- Sys.getenv("SMTP_USER")
  smtp_pass <- Sys.getenv("SMTP_PASS")
  alert_from <- Sys.getenv("ALERT_FROM", smtp_user)
  smtp_host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  smtp_port <- as.integer(Sys.getenv("SMTP_PORT", "465"))

  if (!nzchar(smtp_user)) stop("SMTP_USER secret is empty.")
  if (!nzchar(smtp_pass)) stop("SMTP_PASS secret is empty.")
  if (!nzchar(alert_from)) stop("ALERT_FROM secret/env is empty.")
  if (length(recipients$to) == 0) stop("No active TO recipient found in alert_recipients.csv.")

  body <- paste(
    "Dear team,",
    "",
    paste0("Please find attached the latest DRC Ebola SitRep detected by PREIS: SitRep ", latest$sitrep_no, "."),
    "",
    paste0("Title: ", safe_chr(latest$title)),
    paste0("INSP page: ", safe_chr(latest$post_url)),
    "PDF source: PREIS cloud automation",
    "",
    "This is an automated PREIS notification. Analytical outputs will follow once generated and validated.",
    "",
    "Best regards,",
    "PREIS Ebola DRC Automation",
    sep = "\n"
  )

  email <- blastula::compose_email(body = blastula::md(body))
  email <- blastula::add_attachment(email = email, file = pdf_path, filename = basename(pdf_path))

  blastula::smtp_send(
    email = email,
    from = alert_from,
    to = recipients$to,
    cc = recipients$cc,
    bcc = recipients$bcc,
    subject = paste0("[PREIS Ebola DRC] SitRep ", latest$sitrep_no, " PDF"),
    credentials = blastula::creds(
      user = smtp_user,
      pass = smtp_pass,
      host = smtp_host,
      port = smtp_port,
      use_ssl = TRUE
    )
  )

  TRUE
}

main <- function() {
  log_msg("============================================================")
  log_msg("PREIS cloud SitRep monitor started")

  posts <- get_sitrep_posts()
  if (nrow(posts) == 0) stop("No SitRep posts detected from INSP.")

  latest <- posts[which.max(posts$sitrep_no), , drop = FALSE]
  latest_no <- as.integer(latest$sitrep_no[1])
  log_msg("Latest online SitRep:", latest_no)

  state <- read_state()
  if (nrow(state) > 0 && latest_no %in% state$sitrep_no[state$status %in% c("sent", "sent_cloud")]) {
    log_msg("No new SitRep to send. Already sent SitRep:", latest_no)
    return(invisible(FALSE))
  }

  run_existing_pipeline()

  pdf_path <- find_latest_local_pdf(latest_no)
  if (is.na(pdf_path) || !is_valid_pdf_file(pdf_path)) {
    stop("Latest SitRep PDF not found/valid after pipeline. SitRep: ", latest_no)
  }

  recipients <- read_recipients()

  log_msg("Sending SitRep", latest_no, "to:", paste(recipients$to, collapse = ", "))
  send_sitrep_email(latest[1, ], pdf_path, recipients)

  sent_row <- data.frame(
    sitrep_no = latest_no,
    title = safe_chr(latest$title),
    post_url = safe_chr(latest$post_url),
    pdf_path = pdf_path,
    status = "sent_cloud",
    sent_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  if (nrow(state) > 0) {
    cols <- union(names(state), names(sent_row))
    for (cc in setdiff(cols, names(state))) state[[cc]] <- NA_character_
    for (cc in setdiff(cols, names(sent_row))) sent_row[[cc]] <- NA_character_
    out <- rbind(state[, cols, drop = FALSE], sent_row[, cols, drop = FALSE])
  } else {
    out <- sent_row
  }

  write_state(out)

  log_msg("EMAIL_SENT_OK SitRep:", latest_no)
  invisible(TRUE)
}

main()
