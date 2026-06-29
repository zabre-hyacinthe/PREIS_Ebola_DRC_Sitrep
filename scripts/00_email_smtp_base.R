############################################################
# PREIS Ebola RDC — SMTP email helper
# Fichier: scripts/00_email_smtp_base.R
#
# Version finale GitHub Actions:
#   - aucun emayili
#   - aucun curl::mime
#   - envoi SMTP via curl CLI Linux
#   - pièce jointe PDF encodée MIME/base64
############################################################

preis_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    value <- default
  }
  trimws(as.character(value[1]))
}

preis_split_emails <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(character())
  }

  x <- unlist(strsplit(as.character(x[1]), "[,;]"))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
}

preis_validate_emails <- function(x, field_name) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }

  bad <- x[!grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", x)]

  if (length(bad) > 0) {
    stop(
      field_name,
      " contient email(s) invalide(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

preis_base64_string <- function(x) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Package base64enc manquant.", call. = FALSE)
  }

  out <- base64enc::base64encode(charToRaw(enc2utf8(x)))
  out <- gsub("[\r\n]", "", out)
  out
}

preis_encode_header <- function(x) {
  x <- enc2utf8(as.character(x)[1])
  if (grepl("^[ -~]+$", x)) {
    return(x)
  }
  paste0("=?UTF-8?B?", preis_base64_string(x), "?=")
}

preis_make_date_header <- function() {
  format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")
}

preis_read_attachment_base64 <- function(path) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Package base64enc manquant.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("Pièce jointe introuvable: ", path, call. = FALSE)
  }

  b64 <- base64enc::base64encode(path, linewidth = 76, newline = "\r\n")
  b64 <- gsub("\n", "\r\n", b64, fixed = TRUE)
  b64
}

preis_write_mime_message <- function(
    subject,
    body,
    attachment,
    from,
    to,
    cc = character(),
    bcc = character()
) {

  boundary <- paste0("PREIS_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(100000:999999, 1))
  attachment_name <- basename(attachment)
  attachment_b64 <- preis_read_attachment_base64(attachment)

  to_header <- paste(to, collapse = ", ")
  cc_header <- if (length(cc) > 0) paste(cc, collapse = ", ") else NA_character_

  body_lines <- unlist(strsplit(enc2utf8(body), "\r\n|\n|\r", perl = TRUE))
  if (length(body_lines) == 0) body_lines <- ""

  lines <- c(
    paste0("Date: ", preis_make_date_header()),
    paste0("From: ", from),
    paste0("To: ", to_header),
    if (!is.na(cc_header)) paste0("Cc: ", cc_header) else NULL,
    paste0("Subject: ", preis_encode_header(subject)),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
    "",
    paste0("--", boundary),
    "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: 8bit",
    "",
    body_lines,
    "",
    paste0("--", boundary),
    paste0("Content-Type: application/pdf; name=\"", attachment_name, "\""),
    "Content-Transfer-Encoding: base64",
    paste0("Content-Disposition: attachment; filename=\"", attachment_name, "\""),
    "",
    attachment_b64,
    "",
    paste0("--", boundary, "--"),
    ""
  )

  msg_file <- tempfile(pattern = "preis_email_", fileext = ".eml")
  writeLines(lines, msg_file, sep = "\r\n", useBytes = TRUE)

  msg_file
}

preis_send_email <- function(
    subject,
    body,
    attachment = NULL,
    from = preis_env("ALERT_FROM"),
    to = preis_split_emails(preis_env("ALERT_TO")),
    cc = preis_split_emails(preis_env("ALERT_CC")),
    bcc = preis_split_emails(preis_env("ALERT_BCC")),
    smtp_user = preis_env("SMTP_USER"),
    smtp_pass = preis_env("SMTP_PASS"),
    smtp_host = preis_env("SMTP_HOST"),
    smtp_port = suppressWarnings(as.integer(preis_env("SMTP_PORT", "465")))
) {

  if (!nzchar(from)) {
    stop("ALERT_FROM est vide.", call. = FALSE)
  }

  if (length(to) == 0) {
    stop("ALERT_TO est vide.", call. = FALSE)
  }

  if (!nzchar(smtp_user)) {
    stop("SMTP_USER est vide.", call. = FALSE)
  }

  if (!nzchar(smtp_pass)) {
    stop("SMTP_PASS est vide.", call. = FALSE)
  }

  if (!nzchar(smtp_host)) {
    stop("SMTP_HOST est vide.", call. = FALSE)
  }

  if (is.na(smtp_port)) {
    smtp_port <- 465L
  }

  if (is.null(attachment) || length(attachment) == 0 || !file.exists(as.character(attachment[1]))) {
    stop("Pièce jointe PDF introuvable.", call. = FALSE)
  }

  attachment <- as.character(attachment[1])

  preis_validate_emails(from, "ALERT_FROM")
  preis_validate_emails(to, "ALERT_TO")
  preis_validate_emails(cc, "ALERT_CC")
  preis_validate_emails(bcc, "ALERT_BCC")

  rcpts <- unique(c(to, cc, bcc))

  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) {
    stop("curl CLI introuvable sur le runner.", call. = FALSE)
  }

  msg_file <- preis_write_mime_message(
    subject = subject,
    body = body,
    attachment = attachment,
    from = from,
    to = to,
    cc = cc,
    bcc = bcc
  )

  smtp_scheme <- if (as.integer(smtp_port) == 465L) "smtps" else "smtp"
  smtp_url <- paste0(smtp_scheme, "://", smtp_host, ":", as.integer(smtp_port))

  args <- c(
    "--silent",
    "--show-error",
    "--fail",
    "--ssl-reqd",
    "--connect-timeout", "30",
    "--max-time", "180",
    "--url", smtp_url,
    "--user", paste0(smtp_user, ":", smtp_pass),
    "--mail-from", paste0("<", from, ">")
  )

  for (r in rcpts) {
    args <- c(args, "--mail-rcpt", paste0("<", r, ">"))
  }

  args <- c(args, "--upload-file", msg_file)

  res <- tryCatch(
    system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) {
      attr(e, "preis_system2_error") <- TRUE
      e
    }
  )

  if (inherits(res, "error")) {
    stop("Erreur curl SMTP: ", conditionMessage(res), call. = FALSE)
  }

  status <- attr(res, "status")

  if (!is.null(status) && status != 0) {
    safe_output <- paste(res, collapse = "\n")
    safe_output <- gsub(smtp_pass, "********", safe_output, fixed = TRUE)
    safe_output <- gsub(smtp_user, "SMTP_USER", safe_output, fixed = TRUE)
    stop("Erreur envoi SMTP curl. Sortie:\n", safe_output, call. = FALSE)
  }

  invisible(TRUE)
}