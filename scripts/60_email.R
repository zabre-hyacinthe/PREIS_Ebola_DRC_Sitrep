# =========================================================
# R/60_email.R
# PREIS EBOLA — Email engine using blastula SMTP
# FIXED: supports PDF attachments and existing SMTP env vars
# Required env vars for real send:
#   SMTP_USER, SMTP_PASS
# Optional:
#   SMTP_HOST=smtp.gmail.com, SMTP_PORT=465, ALERT_FROM, PREIS_DRY_RUN=false
# =========================================================

suppressPackageStartupMessages({
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop("Package 'blastula' is required. Run install.packages('blastula').", call. = FALSE)
  }
})

.clean_email_value <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  x
}

get_email_env <- function() {
  list(
    smtp_user  = Sys.getenv("SMTP_USER", ""),
    smtp_pass  = Sys.getenv("SMTP_PASS", ""),
    smtp_host  = Sys.getenv("SMTP_HOST", "smtp.gmail.com"),
    smtp_port  = as.integer(Sys.getenv("SMTP_PORT", "465")),
    alert_from = Sys.getenv("ALERT_FROM", Sys.getenv("SMTP_USER", "")),
    dry_run_env = tolower(Sys.getenv("PREIS_DRY_RUN", "false"))
  )
}

is_dry_run_email <- function(send_now = TRUE) {
  env <- get_email_env()
  if (!isTRUE(send_now)) return(TRUE)
  if (env$dry_run_env %in% c("1", "true", "yes", "y")) return(TRUE)
  FALSE
}

.validate_attachments <- function(attachments = NULL) {
  if (is.null(attachments) || length(attachments) == 0) return(character(0))
  attachments <- as.character(attachments)
  attachments <- attachments[!is.na(attachments)]
  attachments <- trimws(attachments)
  attachments <- attachments[nzchar(attachments)]
  if (length(attachments) == 0) return(character(0))
  missing <- attachments[!file.exists(attachments)]
  if (length(missing) > 0) {
    stop("Attachment file(s) not found: ", paste(missing, collapse = " | "), call. = FALSE)
  }
  normalizePath(attachments, winslash = "/", mustWork = TRUE)
}

send_email_safely <- function(
    to,
    subject,
    body,
    html = NULL,
    send_now = TRUE,
    cc = NULL,
    bcc = NULL,
    attachments = NULL
) {
  env <- get_email_env()

  to <- .clean_email_value(to)
  cc <- .clean_email_value(cc)
  bcc <- .clean_email_value(bcc)

  if (length(to) == 0) {
    stop("send_email_safely(): recipient 'to' is missing.", call. = FALSE)
  }
  if (is.null(subject) || !nzchar(trimws(as.character(subject)[1]))) {
    stop("send_email_safely(): subject is missing.", call. = FALSE)
  }

  msg_body <- if (!is.null(html) && nzchar(trimws(as.character(html)[1]))) html else body
  if (is.null(msg_body) || !nzchar(trimws(as.character(msg_body)[1]))) {
    stop("send_email_safely(): body/html content is empty.", call. = FALSE)
  }

  attachment_paths <- .validate_attachments(attachments)

  if (is_dry_run_email(send_now = send_now)) {
    message("[EMAIL] dry_run -> no email sent to: ", paste(to, collapse = ", "))
    if (length(attachment_paths) > 0) {
      message("[EMAIL] dry_run attachments: ", paste(basename(attachment_paths), collapse = ", "))
    }
    return(list(
      success = TRUE,
      status = "dry_run",
      to = paste(to, collapse = ","),
      subject = as.character(subject)[1],
      attachments = attachment_paths
    ))
  }

  if (!nzchar(trimws(env$smtp_user))) stop("SMTP_USER is missing.", call. = FALSE)
  if (!nzchar(trimws(env$smtp_pass))) stop("SMTP_PASS is missing.", call. = FALSE)
  if (!nzchar(trimws(env$alert_from))) stop("ALERT_FROM is missing.", call. = FALSE)
  if (is.na(env$smtp_port)) stop("SMTP_PORT is invalid.", call. = FALSE)

  Sys.setenv(SMTP_PASS = env$smtp_pass)

  email_obj <- blastula::compose_email(body = blastula::md(msg_body))

  if (length(attachment_paths) > 0) {
    for (att in attachment_paths) {
      email_obj <- blastula::add_attachment(email = email_obj, file = att)
    }
  }

  res <- tryCatch(
    {
      blastula::smtp_send(
        email = email_obj,
        from = env$alert_from,
        to = paste(to, collapse = ","),
        cc = if (length(cc) > 0) paste(cc, collapse = ",") else NULL,
        bcc = if (length(bcc) > 0) paste(bcc, collapse = ",") else NULL,
        subject = as.character(subject)[1],
        credentials = blastula::creds_envvar(
          user = env$smtp_user,
          pass_envvar = "SMTP_PASS",
          host = env$smtp_host,
          port = env$smtp_port,
          use_ssl = TRUE
        )
      )

      message("[EMAIL] sent successfully to: ", paste(to, collapse = ", "))

      list(
        success = TRUE,
        status = "sent",
        to = paste(to, collapse = ","),
        subject = as.character(subject)[1],
        attachments = attachment_paths
      )
    },
    error = function(e) {
      message("[EMAIL] failed for ", paste(to, collapse = ", "), " -> ", conditionMessage(e))
      list(
        success = FALSE,
        status = "failed",
        to = paste(to, collapse = ","),
        subject = as.character(subject)[1],
        attachments = attachment_paths,
        error = conditionMessage(e)
      )
    }
  )

  res
}

preis_send_email <- function(
    to,
    subject,
    body_text,
    attachments = NULL,
    dry_run = TRUE,
    cc = NULL,
    bcc = NULL
) {
  send_email_safely(
    to = to,
    subject = subject,
    body = body_text,
    send_now = !isTRUE(dry_run),
    attachments = attachments,
    cc = cc,
    bcc = bcc
  )
}

stopifnot(exists("send_email_safely", mode = "function"))
stopifnot(exists("preis_send_email", mode = "function"))
