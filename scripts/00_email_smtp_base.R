############################################################
# PREIS Ebola RDC — SMTP email helper
# Fichier: scripts/00_email_smtp_base.R
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
    smtp_host = preis_env("SMTP_HOST", "smtp.gmail.com"),
    smtp_port = suppressWarnings(as.integer(preis_env("SMTP_PORT", "587")))
) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package 'curl' manquant.", call. = FALSE)
  }

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
    smtp_port <- 587L
  }

  preis_validate_emails(from, "ALERT_FROM")
  preis_validate_emails(to, "ALERT_TO")
  preis_validate_emails(cc, "ALERT_CC")
  preis_validate_emails(bcc, "ALERT_BCC")

  recipients_all <- unique(c(to, cc, bcc))

  msg <- curl::mime()

  headers <- list(
    From = from,
    To = paste(to, collapse = ", "),
    Subject = subject
  )

  if (length(cc) > 0) {
    headers$Cc <- paste(cc, collapse = ", ")
  }

  do.call(msg$set_header, headers)

  msg$add_part(enc2utf8(body), type = "text/plain; charset=utf-8")

  if (!is.null(attachment) && length(attachment) > 0) {
    attachment <- as.character(attachment[1])

    if (!file.exists(attachment)) {
      stop("Piece jointe introuvable: ", attachment, call. = FALSE)
    }

    msg$add_part(
      file = attachment,
      name = basename(attachment),
      type = "application/pdf"
    )
  }

  smtp_scheme <- if (identical(as.integer(smtp_port), 465L)) {
    "smtps"
  } else {
    "smtp"
  }

  smtp_server <- paste0(smtp_scheme, "://", smtp_host, ":", smtp_port)

  curl::send_mail(
    mail_from = from,
    mail_rcpt = recipients_all,
    message = msg,
    smtp_server = smtp_server,
    username = smtp_user,
    password = smtp_pass,
    use_ssl = "try"
  )

  invisible(TRUE)
}
