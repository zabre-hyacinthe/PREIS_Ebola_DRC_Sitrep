# ============================================================
# PREIS EBOLA DRC — EMAIL SMTP BASE HELPER
# Purpose: send emails using system curl SMTP, without heavy R email packages
# IMPORTANT: this file must never source itself
# ============================================================

preis_safe_utf8 <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x[is.na(x)] <- ""
  y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = ""))
  y[is.na(y)] <- ""
  Encoding(y) <- "UTF-8"
  y
}

preis_env_first <- function(names, default = "") {
  for (nm in names) {
    val <- Sys.getenv(nm, unset = "")
    if (!is.na(val) && nzchar(val)) return(val)
  }
  default
}

preis_split_recipients <- function(x) {
  x <- preis_safe_utf8(x)
  x <- paste(x, collapse = ",")
  y <- unlist(strsplit(x, "[,;]", fixed = FALSE), use.names = FALSE)
  y <- trimws(y)
  unique(y[nzchar(y)])
}

preis_base64_encode_raw <- function(raw_vec) {
  tbl <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "", fixed = TRUE)[[1]]
  bytes <- as.integer(raw_vec)
  if (length(bytes) == 0) return("")
  pad <- (3 - length(bytes) %% 3) %% 3
  if (pad > 0) bytes <- c(bytes, rep(0L, pad))
  m <- matrix(bytes, ncol = 3, byrow = TRUE)
  b1 <- m[, 1]
  b2 <- m[, 2]
  b3 <- m[, 3]
  i1 <- bitwShiftR(b1, 2)
  i2 <- bitwOr(bitwShiftL(bitwAnd(b1, 3), 4), bitwShiftR(b2, 4))
  i3 <- bitwOr(bitwShiftL(bitwAnd(b2, 15), 2), bitwShiftR(b3, 6))
  i4 <- bitwAnd(b3, 63)
  out <- paste0(tbl[i1 + 1], tbl[i2 + 1], tbl[i3 + 1], tbl[i4 + 1])
  if (pad == 1) out[length(out)] <- paste0(substr(out[length(out)], 1, 3), "=")
  if (pad == 2) out[length(out)] <- paste0(substr(out[length(out)], 1, 2), "==")
  paste(out, collapse = "")
}

preis_base64_file <- function(path) {
  if (!file.exists(path)) stop("Attachment not found: ", path)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw_vec <- readBin(con, what = "raw", n = file.info(path)$size)
  enc <- preis_base64_encode_raw(raw_vec)
  paste(strwrap(enc, width = 76), collapse = "\r\n")
}

preis_mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "pdf")) return("application/pdf")
  if (identical(ext, "csv")) return("text/csv")
  if (identical(ext, "xlsx")) return("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  if (identical(ext, "png")) return("image/png")
  if (identical(ext, "jpg") || identical(ext, "jpeg")) return("image/jpeg")
  "application/octet-stream"
}

preis_header_clean <- function(x) {
  x <- preis_safe_utf8(x)
  x <- gsub("[\r\n]+", " ", x)
  trimws(x)
}

preis_subject_encode <- function(x) {
  x <- preis_header_clean(x)
  raw_vec <- charToRaw(enc2utf8(x))
  paste0("=?UTF-8?B?", preis_base64_encode_raw(raw_vec), "?=")
}

send_email_smtp_base <- function(to = NULL, subject = "PREIS Ebola DRC SitRep", body = "",
                                 attachments = character(0), from = NULL, cc = NULL, bcc = NULL) {
  smtp_host <- preis_env_first(c("SMTP_HOST", "EMAIL_HOST", "MAIL_HOST"), "smtp.gmail.com")
  smtp_port <- preis_env_first(c("SMTP_PORT", "EMAIL_PORT", "MAIL_PORT"), "465")
  smtp_user <- preis_env_first(c("SMTP_USERNAME", "SMTP_USER", "GMAIL_USER", "EMAIL_USER", "MAIL_USERNAME"), "")
  smtp_pass <- preis_env_first(c("SMTP_PASSWORD", "SMTP_PASS", "GMAIL_APP_PASSWORD", "EMAIL_PASSWORD", "MAIL_PASSWORD"), "")

  if (is.null(from) || !nzchar(from)) {
    from <- preis_env_first(c("EMAIL_FROM", "SMTP_FROM", "MAIL_FROM", "GMAIL_USER", "SMTP_USERNAME", "SMTP_USER"), smtp_user)
  }

  if (is.null(to) || length(to) == 0 || !nzchar(paste(to, collapse = ""))) {
    to <- preis_env_first(c("EMAIL_TO", "ALERT_EMAIL_TO", "RECIPIENT_EMAILS", "MAIL_TO"), "")
  }

  recipients <- unique(c(preis_split_recipients(to), preis_split_recipients(cc), preis_split_recipients(bcc)))
  visible_to <- preis_split_recipients(to)
  visible_cc <- preis_split_recipients(cc)

  if (!nzchar(smtp_user)) stop("SMTP username missing. Set SMTP_USERNAME or GMAIL_USER in GitHub Secrets.")
  if (!nzchar(smtp_pass)) stop("SMTP password missing. Set SMTP_PASSWORD or GMAIL_APP_PASSWORD in GitHub Secrets.")
  if (!nzchar(from)) stop("Sender missing. Set EMAIL_FROM or SMTP_FROM in GitHub Secrets.")
  if (length(recipients) == 0) stop("Recipient missing. Set EMAIL_TO or ALERT_EMAIL_TO in GitHub Secrets.")

  attachments <- preis_safe_utf8(attachments)
  attachments <- attachments[nzchar(attachments)]
  attachments <- attachments[file.exists(attachments)]

  boundary <- paste0("PREIS_BOUNDARY_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(100000:999999, 1))

  headers <- c(
    paste0("From: ", preis_header_clean(from)),
    paste0("To: ", paste(visible_to, collapse = ", ")),
    if (length(visible_cc) > 0) paste0("Cc: ", paste(visible_cc, collapse = ", ")) else character(0),
    paste0("Subject: ", preis_subject_encode(subject)),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
    ""
  )

  mime <- c(
    headers,
    paste0("--", boundary),
    "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: 8bit",
    "",
    preis_safe_utf8(body),
    ""
  )

  for (att in attachments) {
    fname <- basename(att)
    mime <- c(
      mime,
      paste0("--", boundary),
      paste0("Content-Type: ", preis_mime_type(att), "; name=\"", fname, "\""),
      "Content-Transfer-Encoding: base64",
      paste0("Content-Disposition: attachment; filename=\"", fname, "\""),
      "",
      preis_base64_file(att),
      ""
    )
  }

  mime <- c(mime, paste0("--", boundary, "--"), "")

  eml <- tempfile(fileext = ".eml")
  writeLines(mime, eml, sep = "\r\n", useBytes = TRUE)

  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) stop("curl command line tool not found.")

  smtp_url <- if (identical(as.character(smtp_port), "465")) {
    paste0("smtps://", smtp_host, ":", smtp_port)
  } else {
    paste0("smtp://", smtp_host, ":", smtp_port)
  }

  args <- c(
    "--silent", "--show-error", "--fail",
    "--url", smtp_url,
    if (!identical(as.character(smtp_port), "465")) "--ssl-reqd" else character(0),
    "--user", paste0(smtp_user, ":", smtp_pass),
    "--mail-from", from
  )

  for (rcpt in recipients) {
    args <- c(args, "--mail-rcpt", rcpt)
  }

  args <- c(args, "--upload-file", eml)
  status <- system2(curl_bin, args = args)

  if (!identical(status, 0L)) {
    stop("Email sending failed via curl SMTP. Check SMTP secrets and recipient variables.")
  }

  message("Email sent successfully to: ", paste(recipients, collapse = ", "))
  invisible(TRUE)
}

# Compatibility functions for old email code

md <- function(x, ...) {
  paste(preis_safe_utf8(x), collapse = "\n")
}

compose_email <- function(body = NULL, ...) {
  extra <- list(...)
  pieces <- c(body, unlist(extra, recursive = TRUE, use.names = FALSE))
  pieces <- preis_safe_utf8(pieces)
  structure(
    list(body = paste(pieces[nzchar(pieces)], collapse = "\n\n"), attachments = character(0)),
    class = "preis_email"
  )
}

add_attachment <- function(email, file = NULL, filename = NULL, ...) {
  if (is.null(email) || !inherits(email, "preis_email")) {
    email <- structure(
      list(body = paste(preis_safe_utf8(email), collapse = "\n"), attachments = character(0)),
      class = "preis_email"
    )
  }
  if (!is.null(file) && length(file) > 0) {
    email$attachments <- unique(c(email$attachments, preis_safe_utf8(file)))
  }
  email
}

creds_file <- function(file = NULL, ...) {
  list(file = file, type = "env")
}

creds_envvar <- function(...) {
  list(type = "env")
}

smtp_send <- function(email, from = NULL, to = NULL, subject = "PREIS Ebola DRC SitRep",
                      credentials = NULL, attachments = character(0), cc = NULL, bcc = NULL, ...) {
  body <- if (inherits(email, "preis_email")) email$body else paste(preis_safe_utf8(email), collapse = "\n")
  atts <- unique(c(
    if (inherits(email, "preis_email")) email$attachments else character(0),
    attachments
  ))
  send_email_smtp_base(to = to, subject = subject, body = body, attachments = atts, from = from, cc = cc, bcc = bcc)
}

message("PREIS email helper loaded: SMTP mode")
