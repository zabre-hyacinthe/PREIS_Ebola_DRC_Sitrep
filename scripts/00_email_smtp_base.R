############################################################
# PREIS Ebola RDC — SMTP email helper
# Fichier: scripts/00_email_smtp_base.R
#
# Version stable GitHub Actions:
#   - utilise emayili pour email + pièce jointe PDF
#   - évite curl::mime(), non disponible selon version curl
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
    smtp_host = preis_env("SMTP_HOST"),
    smtp_port = suppressWarnings(as.integer(preis_env("SMTP_PORT", "465")))
) {

  if (!requireNamespace("emayili", quietly = TRUE)) {
    stop("Package 'emayili' manquant.", call. = FALSE)
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
    smtp_port <- 465L
  }

  preis_validate_emails(from, "ALERT_FROM")
  preis_validate_emails(to, "ALERT_TO")
  preis_validate_emails(cc, "ALERT_CC")
  preis_validate_emails(bcc, "ALERT_BCC")

  email <- emayili::envelope()
  email <- emayili::from(email, from)

  for (addr in to) {
    email <- emayili::to(email, addr)
  }

  if (length(cc) > 0) {
    for (addr in cc) {
      email <- emayili::cc(email, addr)
    }
  }

  if (length(bcc) > 0) {
    for (addr in bcc) {
      email <- emayili::bcc(email, addr)
    }
  }

  email <- emayili::subject(email, subject)
  email <- emayili::text(email, body)

  if (!is.null(attachment) && length(attachment) > 0) {
    attachment <- as.character(attachment[1])

    if (!file.exists(attachment)) {
      stop("Pièce jointe introuvable: ", attachment, call. = FALSE)
    }

    email <- emayili::attachment(email, attachment)
  }

  smtp <- emayili::server(
    host = smtp_host,
    port = smtp_port,
    username = smtp_user,
    password = smtp_pass,
    reuse = FALSE
  )

  smtp(email, verbose = FALSE)

  invisible(TRUE)
}