############################################################
# PREIS Ebola RDC — SMTP email helper
############################################################

preis_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (is.na(value)) value <- default
  trimws(value)
}

preis_split_emails <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
    return(character())
  }

  x <- unlist(strsplit(x, "[,;]"))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)
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
  smtp_port = as.integer(preis_env("SMTP_PORT", "587"))
) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package 'curl' manquant.", call. = FALSE)
  }

  if (!nzchar(from)) stop("ALERT_FROM est vide.", call. = FALSE)
  if (length(to) == 0) stop("ALERT_TO est vide.", call. = FALSE)
  if (!nzchar(smtp_user)) stop("SMTP_USER est vide.", call. = FALSE)
  if (!nzchar(smtp_pass)) stop("SMTP_PASS est vide.", call. = FALSE)
  if (!nzchar(smtp_host)) stop("SMTP_HOST est vide.", call. = FALSE)
  if (is.na(smtp_port)) smtp_port <- 587

  recipients_all <- unique(c(to, cc, bcc))

  msg <- curl::mime()
  msg$set_header(
    From = from,
    To = paste(to, collapse = ", "),
    Cc = if (length(cc) > 0) paste(cc, collapse = ", ") else NULL,
    Subject = subject
  )

  msg$add_part(body, type = "text/plain; charset=utf-8")

  if (!is.null(attachment) && length(attachment) > 0 && file.exists(attachment)) {
    msg$add_part(
      file = attachment,
      name = basename(attachment),
      type = "application/pdf"
    )
  }

  smtp_server <- paste0("smtp://", smtp_host, ":", smtp_port)

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
