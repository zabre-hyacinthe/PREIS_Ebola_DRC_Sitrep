# ============================================================
# PREIS EBOLA DRC — PERMANENT SITREP PDF EMAIL MONITOR
# Scheduled-safe: check once by default; no infinite loop unless explicitly called.
# Uses existing SMTP engine from scripts/60_email.R.
# Recipient list: ROOT_DIR/alert_recipients.csv
# ============================================================

ROOT_DIR <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
RECIPIENTS_FILE <- file.path(ROOT_DIR, "alert_recipients.csv")
CHECK_INTERVAL_MINUTES <- 5
SEND_NOW_DEFAULT <- TRUE
RUN_PIPELINE_BEFORE_EMAIL_DEFAULT <- TRUE

suppressPackageStartupMessages({
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  library(readr)
})

DIR_LOG <- file.path(ROOT_DIR, "logs")
DIR_STATE <- file.path(ROOT_DIR, "data", "monitor_state")
dir.create(DIR_LOG, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_STATE, recursive = TRUE, showWarnings = FALSE)
LOG_FILE <- file.path(DIR_LOG, paste0("preis_scheduled_sitrep_monitor_", format(Sys.Date(), "%Y%m%d"), ".log"))
STATE_FILE <- file.path(DIR_STATE, "preis_sitrep_email_state.csv")

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG_FILE, append = TRUE)
}

clean_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x[nzchar(x)]
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

list_pdfs_safely <- function(root) {
  if (!dir.exists(root)) return(character(0))
  tryCatch(list.files(root, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE), error = function(e) character(0))
}

extract_sitrep_no_from_path <- function(path) {
  b <- tolower(basename(path))
  patterns <- c("sitrep[_ -]*([0-9]{1,3})", "sitrep.*n[_ -]*([0-9]{1,3})", "n[_ -]*([0-9]{1,3})")
  for (p in patterns) {
    m <- regexec(p, b, ignore.case = TRUE, perl = TRUE)
    r <- regmatches(b, m)[[1]]
    if (length(r) >= 2) return(as.integer(r[2]))
  }
  NA_integer_
}

find_all_local_sitrep_pdfs <- function() {
  roots <- unique(c(
    file.path(ROOT_DIR, "data", "pdf"),
    file.path(ROOT_DIR, "data", "incoming"),
    ROOT_DIR,
    "D:/PREIS_Ebola_Production",
    "D:/PREIS_Ebola_FV",
    file.path(Sys.getenv("USERPROFILE"), "Downloads"),
    file.path(Sys.getenv("USERPROFILE"), "Documents"),
    file.path(Sys.getenv("USERPROFILE"), "Desktop")
  ))
  roots <- roots[dir.exists(roots)]
  all_pdfs <- unique(unlist(lapply(roots, list_pdfs_safely), use.names = FALSE))
  if (length(all_pdfs) == 0) return(data.frame())
  sitrep_no <- vapply(all_pdfs, extract_sitrep_no_from_path, integer(1))
  out <- data.frame(pdf_path = all_pdfs, sitrep_no = sitrep_no, stringsAsFactors = FALSE)
  out <- out[!is.na(out$sitrep_no), , drop = FALSE]
  if (nrow(out) == 0) return(out)
  out$valid_pdf <- vapply(out$pdf_path, is_valid_pdf_file, logical(1))
  out$size <- suppressWarnings(file.info(out$pdf_path)$size)
  out$mtime <- suppressWarnings(file.info(out$pdf_path)$mtime)
  out <- out[out$valid_pdf, , drop = FALSE]
  out <- out[order(out$sitrep_no, out$mtime, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

find_latest_local_sitrep_pdf <- function() {
  x <- find_all_local_sitrep_pdfs()
  if (nrow(x) == 0) return(NULL)
  x[1, , drop = FALSE]
}

source_email_core <- function() {
  paths <- c(file.path(ROOT_DIR, "scripts", "60_email.R"), file.path(ROOT_DIR, "R", "60_email.R"))
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) stop("60_email.R not found in scripts/ or R/.", call. = FALSE)
  source(paths[1])
  if (!exists("send_email_safely", mode = "function")) stop("send_email_safely() not loaded from 60_email.R", call. = FALSE)
  log_msg("Email core loaded:", paths[1])
  invisible(paths[1])
}

read_recipients <- function() {
  if (!file.exists(RECIPIENTS_FILE)) {
    df <- data.frame(active = TRUE, type = "to", name = "Dr Zabre", email = "raogoz@africacdc.org", stringsAsFactors = FALSE)
    utils::write.csv(df, RECIPIENTS_FILE, row.names = FALSE, fileEncoding = "UTF-8")
  }
  df <- as.data.frame(readr::read_csv(RECIPIENTS_FILE, show_col_types = FALSE), stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  if (!"email" %in% names(df)) stop("alert_recipients.csv must contain an email column.", call. = FALSE)
  if (!"active" %in% names(df)) df$active <- TRUE
  if (!"type" %in% names(df)) df$type <- "to"
  df$email <- clean_chr(df$email)
  df$type <- tolower(trimws(as.character(df$type)))
  active_chr <- tolower(trimws(as.character(df$active)))
  df <- df[active_chr %in% c("true", "1", "yes", "y", "oui", "active"), , drop = FALSE]
  df <- df[nzchar(df$email), , drop = FALSE]
  list(
    to = unique(df$email[df$type %in% c("to", "destinataire", "main")]),
    cc = unique(df$email[df$type == "cc"]),
    bcc = unique(df$email[df$type == "bcc"]),
    table = df
  )
}

already_sent <- function(sitrep_no) {
  if (!file.exists(STATE_FILE)) return(FALSE)
  x <- tryCatch(as.data.frame(readr::read_csv(STATE_FILE, show_col_types = FALSE)), error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0 || !"sitrep_no" %in% names(x)) return(FALSE)
  status <- if ("status" %in% names(x)) as.character(x$status) else rep("", nrow(x))
  any(as.integer(x$sitrep_no) == as.integer(sitrep_no) & grepl("sent", status, ignore.case = TRUE))
}

append_state <- function(sitrep_no, pdf_path, status) {
  row <- data.frame(
    sitrep_no = as.integer(sitrep_no),
    title = paste0("DRC Ebola SitRep ", sitrep_no),
    post_url = "",
    pdf_url = "Local PDF downloaded and verified by PREIS pipeline",
    pdf_path = pdf_path,
    status = status,
    detected_at = as.character(Sys.time()),
    sent_at = if (grepl("sent", status, ignore.case = TRUE)) as.character(Sys.time()) else NA_character_,
    stringsAsFactors = FALSE
  )
  if (file.exists(STATE_FILE)) {
    old <- tryCatch(as.data.frame(readr::read_csv(STATE_FILE, show_col_types = FALSE)), error = function(e) NULL)
    if (!is.null(old)) {
      cols <- union(names(old), names(row))
      for (cc in setdiff(cols, names(old))) old[[cc]] <- NA_character_
      for (cc in setdiff(cols, names(row))) row[[cc]] <- NA_character_
      row <- rbind(old[, cols, drop = FALSE], row[, cols, drop = FALSE])
    }
  }
  readr::write_csv(row, STATE_FILE)
  invisible(row)
}

run_pipeline_if_possible <- function() {
  pipeline <- file.path(ROOT_DIR, "00_RUN_ALL_PRODUCTION.R")
  if (!file.exists(pipeline)) {
    log_msg("Pipeline not found, skip:", pipeline)
    return(FALSE)
  }
  log_msg("Running production pipeline before email check")
  ok <- tryCatch({ source(pipeline); TRUE }, error = function(e) { log_msg("Pipeline error but monitor continues:", conditionMessage(e)); FALSE })
  ok
}

check_email_env_ready <- function() {
  miss <- character(0)
  if (!nzchar(Sys.getenv("SMTP_USER", ""))) miss <- c(miss, "SMTP_USER")
  if (!nzchar(Sys.getenv("SMTP_PASS", ""))) miss <- c(miss, "SMTP_PASS")
  if (length(miss) > 0) stop("Missing SMTP env variable(s): ", paste(miss, collapse = ", "), call. = FALSE)
  TRUE
}

make_sitrep_body <- function(sitrep_no, pdf_path) {
  paste(
    "Dear team,", "",
    paste0("Please find attached the latest DRC Ebola SitRep detected by PREIS: SitRep ", sitrep_no, "."),
    "",
    paste0("Title: DRC Ebola SitRep ", sitrep_no),
    paste0("PDF file: ", basename(pdf_path)),
    "PDF source: Local PDF downloaded and verified by PREIS pipeline",
    "",
    "This is an automated PREIS notification. Analytical outputs will follow once generated and validated.",
    "", "Best regards,", "PREIS Ebola DRC Automation",
    sep = "\n"
  )
}

check_preis_sitrep_once <- function(send_now = SEND_NOW_DEFAULT, run_pipeline = RUN_PIPELINE_BEFORE_EMAIL_DEFAULT) {
  log_msg("============================================================")
  log_msg("PREIS scheduled SitRep check started")
  if (isTRUE(run_pipeline)) run_pipeline_if_possible()
  latest <- find_latest_local_sitrep_pdf()
  if (is.null(latest) || nrow(latest) == 0) stop("No valid SitRep PDF found locally.", call. = FALSE)
  sitrep_no <- as.integer(latest$sitrep_no[1])
  pdf_path <- latest$pdf_path[1]
  log_msg("Latest local SitRep PDF:", sitrep_no, pdf_path)
  if (already_sent(sitrep_no)) {
    log_msg("No new SitRep to send. Already sent SitRep:", sitrep_no)
    return(invisible(list(status = "already_sent", sitrep_no = sitrep_no, pdf_path = pdf_path)))
  }
  recipients <- read_recipients()
  if (length(recipients$to) == 0) stop("No active TO recipients in alert_recipients.csv", call. = FALSE)
  source_email_core()
  check_email_env_ready()
  subject <- paste0("[PREIS Ebola DRC] SitRep ", sitrep_no, " PDF")
  body <- make_sitrep_body(sitrep_no, pdf_path)
  res <- send_email_safely(
    to = recipients$to, cc = recipients$cc, bcc = recipients$bcc,
    subject = subject, body = body, send_now = send_now, attachments = pdf_path
  )
  status <- if (isTRUE(res$success)) res$status else paste0("failed: ", res$error)
  append_state(sitrep_no, pdf_path, status)
  if (!isTRUE(res$success)) stop("Email failed: ", res$error, call. = FALSE)
  log_msg("PREIS scheduled SitRep email completed:", status)
  invisible(res)
}

monitor_preis_sitreps_forever <- function(interval_minutes = CHECK_INTERVAL_MINUTES) {
  repeat {
    tryCatch(check_preis_sitrep_once(send_now = TRUE, run_pipeline = TRUE), error = function(e) log_msg("MONITOR ERROR:", conditionMessage(e)))
    Sys.sleep(interval_minutes * 60)
  }
}

cat("\nPREIS permanent SitRep monitor loaded.\n")
cat("Run once: check_preis_sitrep_once()\n")
cat("Run every 5 min in this R session: monitor_preis_sitreps_forever(5)\n")
