############################################################
# PREIS Ebola RDC — SMTP email helper
# Fichier: scripts/00_email_smtp_base.R
#
# Version definitive:
#   - aucun curl SMTP
#   - aucun emayili
#   - envoi email via Python standard smtplib
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

preis_find_python <- function() {
  py <- Sys.which("python3")
  if (!nzchar(py)) {
    py <- Sys.which("python")
  }
  if (!nzchar(py)) {
    stop("Python introuvable sur le runner.", call. = FALSE)
  }
  py
}

preis_redact <- function(x, smtp_user, smtp_pass) {
  x <- paste(as.character(x), collapse = "\n")

  if (!is.na(smtp_pass) && nzchar(smtp_pass)) {
    x <- gsub(smtp_pass, "********", x, fixed = TRUE)
  }

  if (!is.na(smtp_user) && nzchar(smtp_user)) {
    x <- gsub(smtp_user, "SMTP_USER", x, fixed = TRUE)
  }

  x
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

  if (is.null(attachment) || length(attachment) == 0) {
    stop("Pièce jointe PDF manquante.", call. = FALSE)
  }

  attachment <- as.character(attachment[1])

  if (!file.exists(attachment)) {
    stop("Pièce jointe introuvable: ", attachment, call. = FALSE)
  }

  preis_validate_emails(from, "ALERT_FROM")
  preis_validate_emails(to, "ALERT_TO")
  preis_validate_emails(cc, "ALERT_CC")
  preis_validate_emails(bcc, "ALERT_BCC")

  py_script <- file.path(getwd(), "scripts", "py_send_email.py")

  if (!file.exists(py_script)) {
    stop("Helper Python introuvable: ", py_script, call. = FALSE)
  }

  body_file <- tempfile(pattern = "preis_email_body_", fileext = ".txt")
  writeLines(enc2utf8(body), body_file, useBytes = TRUE)

  py <- preis_find_python()

  env_vars <- c(
    paste0("SMTP_HOST=", smtp_host),
    paste0("SMTP_PORT=", as.integer(smtp_port)),
    paste0("SMTP_USER=", smtp_user),
    paste0("SMTP_PASS=", smtp_pass),
    paste0("ALERT_FROM=", from),
    paste0("ALERT_TO=", paste(to, collapse = ";")),
    paste0("ALERT_CC=", paste(cc, collapse = ";")),
    paste0("ALERT_BCC=", paste(bcc, collapse = ";")),
    paste0("PREIS_EMAIL_SUBJECT=", subject),
    paste0("PREIS_EMAIL_BODY_FILE=", body_file),
    paste0("PREIS_EMAIL_ATTACHMENT=", normalizePath(attachment, mustWork = TRUE))
  )

  output <- tryCatch(
    system2(
      py,
      args = c(py_script),
      stdout = TRUE,
      stderr = TRUE,
      env = env_vars
    ),
    error = function(e) {
      structure(conditionMessage(e), status = 99)
    }
  )

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0
  }

  safe_output <- preis_redact(output, smtp_user, smtp_pass)

  if (nzchar(safe_output)) {
    message(safe_output)
  }

  if (!identical(as.integer(status), 0L)) {
    stop("Erreur envoi SMTP Python. Sortie:\n", safe_output, call. = FALSE)
  }

  invisible(TRUE)
}
