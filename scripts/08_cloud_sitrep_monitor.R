# ============================================================
# PREIS EBOLA DRC — CLOUD SITREP MONITOR
# Version definitive SANS blastula / rmarkdown / fs / sass
# Objectif:
#   1) Detecter le dernier SitRep Ebola/MVB sur INSP
#   2) Telecharger le PDF tel quel
#   3) Envoyer le PDF par email via SMTP avec curl systeme
#   4) Eviter les doublons avec un fichier state RDS
#
# IMPORTANT:
#   - Aucun install.packages()
#   - Aucun library(blastula)
#   - Aucun package R lourd
#   - Fonctionne dans GitHub Actions si les secrets SMTP sont configures
# ============================================================

options(warn = 1)

log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
}

stop_clean <- function(...) {
  msg <- paste(...)
  log_msg("ERROR:", msg)
  stop(msg, call. = FALSE)
}

first_non_empty <- function(names, default = "") {
  for (nm in names) {
    val <- Sys.getenv(nm, unset = "")
    if (!is.na(val) && nzchar(trimws(val))) {
      return(trimws(val))
    }
  }
  default
}

html_unescape_basic <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#039;", "'", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x <- gsub("\\\\/", "/", x)
  x
}

normalize_url <- function(x, base_url = "https://insp.cd") {
  x <- html_unescape_basic(x)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", "", x)
  x <- sub("#.*$", "", x)

  x <- ifelse(grepl("^//", x), paste0("https:", x), x)
  x <- ifelse(grepl("^/", x), paste0(base_url, x), x)

  is_relative <- !grepl("^https?://", x, ignore.case = TRUE) &
    !grepl("^(mailto:|tel:|javascript:)", x, ignore.case = TRUE) &
    nzchar(x)

  x <- ifelse(is_relative, paste0(base_url, "/", x), x)
  x
}

extract_attr_urls <- function(html) {
  patterns <- c(
    "href\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "src\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data-src\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]",
    "data\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]"
  )

  out <- character()

  for (p in patterns) {
    m <- gregexpr(p, html, perl = TRUE, ignore.case = TRUE)
    hits <- regmatches(html, m)[[1]]

    if (length(hits) > 0 && hits[1] != "-1") {
      vals <- sub("^[^=]+=\\s*['\\\"]", "", hits, perl = TRUE)
      vals <- sub("['\\\"]$", "", vals, perl = TRUE)
      out <- c(out, vals)
    }
  }

  unique(out)
}

extract_absolute_pdf_urls <- function(html) {
  p <- "https?://[^'\\\"<>[:space:]]+\\.pdf[^'\\\"<>[:space:]]*"
  m <- gregexpr(p, html, perl = TRUE, ignore.case = TRUE)
  hits <- regmatches(html, m)[[1]]
  if (length(hits) == 0 || hits[1] == "-1") return(character())
  unique(hits)
}

extract_sitrep_no <- function(x) {
  y <- tolower(x)

  patterns <- c(
    "sitrep[-_[:space:]]*n[-_[:space:]]*([0-9]{1,3})",
    "sitrep[-_[:space:]]*([0-9]{1,3})",
    "n([0-9]{1,3})[-_[:space:]]*mvb",
    "sitrep-n([0-9]{1,3})"
  )

  for (p in patterns) {
    m <- regexec(p, y, perl = TRUE)
    r <- regmatches(y, m)[[1]]
    if (length(r) >= 2) {
      val <- suppressWarnings(as.integer(r[2]))
      if (!is.na(val)) return(val)
    }
  }

  NA_integer_
}

extract_date_from_text <- function(x) {
  m <- regexec("([0-9]{2})[-_/]([0-9]{2})[-_/]([0-9]{4})", x, perl = TRUE)
  r <- regmatches(x, m)[[1]]
  if (length(r) >= 4) {
    return(paste0(r[4], "-", r[3], "-", r[2]))
  }
  NA_character_
}

safe_filename <- function(url, sitrep_no = NA_integer_) {
  clean <- sub("\\?.*$", "", url)
  name <- basename(clean)
  name <- html_unescape_basic(name)
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)

  if (!grepl("\\.pdf$", name, ignore.case = TRUE)) {
    if (!is.na(sitrep_no)) {
      name <- paste0("SitRep_N", sitrep_no, "_MVB.pdf")
    } else {
      name <- paste0("SitRep_MVB_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    }
  }

  name
}

read_text_file_safe <- function(path) {
  x <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  paste(x, collapse = "\n")
}

fetch_url_text <- function(url) {
  curl_bin <- Sys.which("curl")
  tmp <- tempfile(fileext = ".html")

  if (nzchar(curl_bin)) {
    args <- c(
  "-L",
  "--fail",
  "--silent",
  "--show-error",
  "--retry", "3",
  "--max-time", "90",
  "--user-agent", "PREIS-Ebola-DRC-Monitor/1.0",
  "-o", tmp,
  url
)

    res <- suppressWarnings(system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE))
    status <- attr(res, "status")
    if (is.null(status)) status <- 0

    if (status == 0 && file.exists(tmp) && file.info(tmp)$size > 0) {
      return(read_text_file_safe(tmp))
    }
  }

  ok <- tryCatch({
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb", method = "libcurl")
    TRUE
  }, error = function(e) FALSE)

  if (ok && file.exists(tmp) && file.info(tmp)$size > 0) {
    return(read_text_file_safe(tmp))
  }

  ""
}

download_binary <- function(url, dest) {
  curl_bin <- Sys.which("curl")

  if (nzchar(curl_bin)) {
    args <- c(
  "-L",
  "--fail",
  "--silent",
  "--show-error",
  "--retry", "3",
  "--max-time", "180",
  "--user-agent", "PREIS-Ebola-DRC-Monitor/1.0",
  "-o", dest,
  url
)

    res <- suppressWarnings(system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE))
    status <- attr(res, "status")
    if (is.null(status)) status <- 0

    if (status == 0 && file.exists(dest) && file.info(dest)$size > 1000) {
      return(TRUE)
    }
  }

  ok <- tryCatch({
    utils::download.file(url, dest, quiet = TRUE, mode = "wb", method = "libcurl")
    TRUE
  }, error = function(e) FALSE)

  ok && file.exists(dest) && file.info(dest)$size > 1000
}

make_base64_lines <- function(file_path) {
  base64_bin <- Sys.which("base64")

  if (!nzchar(base64_bin)) {
    stop_clean("La commande systeme 'base64' est introuvable. Sur GitHub Actions Ubuntu elle existe normalement.")
  }

  raw <- system2(base64_bin, args = normalizePath(file_path, winslash = "/", mustWork = TRUE), stdout = TRUE)
  b64 <- paste(raw, collapse = "")

  starts <- seq(1, nchar(b64), by = 76)
  substring(b64, starts, pmin(starts + 75, nchar(b64)))
}

send_email_with_attachment <- function(to, from, subject, body, attachment_path) {
  smtp_url <- first_non_empty(c("SMTP_URL", "MAIL_SMTP_URL", "PREIS_SMTP_URL"))
  smtp_host <- first_non_empty(c("SMTP_HOST", "MAIL_SMTP_HOST", "PREIS_SMTP_HOST"))
  smtp_port <- first_non_empty(c("SMTP_PORT", "MAIL_SMTP_PORT", "PREIS_SMTP_PORT"), "587")

  if (!nzchar(smtp_url) && nzchar(smtp_host)) {
    smtp_url <- paste0("smtp://", smtp_host, ":", smtp_port)
  }

  smtp_user <- first_non_empty(c("SMTP_USERNAME", "SMTP_USER", "MAIL_USERNAME", "MAIL_USER", "PREIS_SMTP_USERNAME"))
  smtp_pass <- first_non_empty(c("SMTP_PASSWORD", "SMTP_PASS", "MAIL_PASSWORD", "MAIL_PASS", "PREIS_SMTP_PASSWORD"))

  if (!nzchar(smtp_url)) stop_clean("SMTP_URL ou SMTP_HOST non configure.")
  if (!nzchar(smtp_user)) stop_clean("SMTP_USERNAME non configure.")
  if (!nzchar(smtp_pass)) stop_clean("SMTP_PASSWORD non configure.")
  if (!nzchar(to)) stop_clean("EMAIL_TO non configure.")
  if (!nzchar(from)) stop_clean("EMAIL_FROM non configure.")

  curl_bin <- Sys.which("curl")
  if (!nzchar(curl_bin)) stop_clean("La commande systeme 'curl' est introuvable.")

  recipients <- unlist(strsplit(to, "[,;]"))
  recipients <- trimws(recipients)
  recipients <- recipients[nzchar(recipients)]

  if (length(recipients) == 0) stop_clean("Aucun destinataire email valide.")

  boundary <- paste0("----PREIS_EBOLA_", format(Sys.time(), "%Y%m%d%H%M%S"))
  attachment_name <- basename(attachment_path)
  b64_lines <- make_base64_lines(attachment_path)

  mime_file <- tempfile(fileext = ".eml")

  msg <- c(
    paste0("From: ", from),
    paste0("To: ", paste(recipients, collapse = ", ")),
    paste0("Subject: ", subject),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
    "",
    paste0("--", boundary),
    "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: 8bit",
    "",
    body,
    "",
    paste0("--", boundary),
    paste0("Content-Type: application/pdf; name=\"", attachment_name, "\""),
    paste0("Content-Disposition: attachment; filename=\"", attachment_name, "\""),
    "Content-Transfer-Encoding: base64",
    "",
    b64_lines,
    "",
    paste0("--", boundary, "--"),
    ""
  )

  writeLines(msg, mime_file, useBytes = TRUE)

  args <- c(
    "--url", smtp_url,
    "--ssl-reqd",
    "--mail-from", paste0("<", from, ">")
  )

  for (r in recipients) {
    args <- c(args, "--mail-rcpt", paste0("<", r, ">"))
  }

  args <- c(
    args,
    "--user", paste0(smtp_user, ":", smtp_pass),
    "--upload-file", mime_file,
    "--silent",
    "--show-error",
    "--fail"
  )

  res <- suppressWarnings(system2(curl_bin, args = args, stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status")
  if (is.null(status)) status <- 0

  if (status != 0) {
    log_msg("SMTP/curl response:")
    print(res)
    stop_clean("Echec envoi email SMTP.")
  }

  TRUE
}

# ============================================================
# CONFIGURATION
# ============================================================

BASE_URL <- first_non_empty(c("INSP_BASE_URL"), "https://insp.cd")

SEARCH_URLS <- unique(c(
  first_non_empty(c("INSP_LATEST_PAGE_URL")),
  first_non_empty(c("INSP_SEARCH_URL")),
  paste0(BASE_URL, "/?s=sitrep+mvb"),
  paste0(BASE_URL, "/?s=SitRep+MVB"),
  paste0(BASE_URL, "/?s=Ebola+SitRep"),
  paste0(BASE_URL, "/?s=MVB")
))

SEARCH_URLS <- SEARCH_URLS[nzchar(SEARCH_URLS)]

STATE_DIR <- first_non_empty(c("PREIS_STATE_DIR"), "data/state")
INCOMING_DIR <- first_non_empty(c("PREIS_INCOMING_DIR"), "data/incoming/insp_sitreps")
OUT_DIR <- first_non_empty(c("PREIS_MONITOR_OUT_DIR"), "outputs/cloud_monitor")

dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INCOMING_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

STATE_FILE <- file.path(STATE_DIR, "latest_sitrep_monitor_state.rds")
QC_FILE <- file.path(OUT_DIR, "cloud_sitrep_monitor_qc.csv")

EMAIL_TO <- first_non_empty(c("EMAIL_TO", "MAIL_TO", "PREIS_EMAIL_TO"))
EMAIL_FROM <- first_non_empty(c("EMAIL_FROM", "MAIL_FROM", "PREIS_EMAIL_FROM"))
FORCE_SEND <- tolower(first_non_empty(c("PREIS_FORCE_SEND", "FORCE_SEND"), "false")) %in% c("1", "true", "yes", "oui")

log_msg("PREIS Ebola DRC cloud monitor started.")
log_msg("No R package installation. No blastula. No rmarkdown. No fs. No sass.")

# ============================================================
# FIND CANDIDATE SITREP PAGES
# ============================================================

all_links <- character()

for (u in SEARCH_URLS) {
  log_msg("Scanning:", u)
  html <- fetch_url_text(u)

  if (!nzchar(html)) {
    log_msg("Warning: page inaccessible or empty:", u)
    next
  }

  links <- c(
    extract_attr_urls(html),
    extract_absolute_pdf_urls(html)
  )

  links <- normalize_url(links, BASE_URL)
  links <- unique(links)
  links <- links[grepl("^https?://", links, ignore.case = TRUE)]

  all_links <- unique(c(all_links, links))
}

if (length(all_links) == 0) {
  stop_clean("Aucun lien trouve sur les pages INSP scannees.")
}

candidate_links <- all_links[
  grepl("sitrep|situation", all_links, ignore.case = TRUE) |
    grepl("\\.pdf", all_links, ignore.case = TRUE)
]

candidate_links <- candidate_links[
  grepl("mvb|ebola|maladie|virus|sitrep|situation", candidate_links, ignore.case = TRUE)
]

candidate_links <- unique(candidate_links)

if (length(candidate_links) == 0) {
  stop_clean("Aucun lien SitRep candidat trouve.")
}

sitrep_numbers <- vapply(candidate_links, extract_sitrep_no, integer(1))
rank_numbers <- ifelse(is.na(sitrep_numbers), -1L, sitrep_numbers)

candidate_links <- candidate_links[order(rank_numbers, decreasing = TRUE)]
candidate_links <- unique(candidate_links)

log_msg("Candidate SitRep links found:", length(candidate_links))

# ============================================================
# FIND LATEST PDF
# ============================================================

latest <- list(
  sitrep_no = NA_integer_,
  page_url = NA_character_,
  pdf_url = NA_character_
)

for (page_url in head(candidate_links, 20)) {
  page_no <- extract_sitrep_no(page_url)

  if (grepl("\\.pdf(\\?|$)", page_url, ignore.case = TRUE)) {
    latest$sitrep_no <- page_no
    latest$page_url <- page_url
    latest$pdf_url <- page_url
    break
  }

  page_html <- fetch_url_text(page_url)
  if (!nzchar(page_html)) next

  pdfs <- c(
    extract_absolute_pdf_urls(page_html),
    extract_attr_urls(page_html)
  )

  pdfs <- normalize_url(pdfs, BASE_URL)
  pdfs <- unique(pdfs)
  pdfs <- pdfs[grepl("\\.pdf(\\?|$)", pdfs, ignore.case = TRUE)]

  if (length(pdfs) == 0) next

  pdfs <- pdfs[
    grepl("sitrep|mvb|ebola|maladie|virus", pdfs, ignore.case = TRUE) |
      grepl("\\.pdf", pdfs, ignore.case = TRUE)
  ]

  if (length(pdfs) == 0) next

  pdf_numbers <- vapply(pdfs, extract_sitrep_no, integer(1))

  if (!is.na(page_no)) {
    matching <- which(is.na(pdf_numbers) | pdf_numbers == page_no)
    if (length(matching) > 0) {
      pdfs <- pdfs[matching]
      pdf_numbers <- pdf_numbers[matching]
    }
  }

  latest$sitrep_no <- ifelse(!is.na(page_no), page_no, pdf_numbers[1])
  latest$page_url <- page_url
  latest$pdf_url <- pdfs[1]
  break
}

if (!nzchar(latest$pdf_url) || is.na(latest$pdf_url)) {
  stop_clean("Aucun PDF SitRep trouve dans les pages candidates.")
}

log_msg("Latest SitRep page:", latest$page_url)
log_msg("Latest SitRep PDF :", latest$pdf_url)
log_msg("Latest SitRep no  :", ifelse(is.na(latest$sitrep_no), "NA", latest$sitrep_no))

# ============================================================
# CHECK STATE TO AVOID DUPLICATES
# ============================================================

previous <- list(
  sitrep_no = NA_integer_,
  page_url = NA_character_,
  pdf_url = NA_character_,
  sent_at = NA_character_
)

if (file.exists(STATE_FILE)) {
  previous <- tryCatch(readRDS(STATE_FILE), error = function(e) previous)
}

same_pdf <- identical(previous$pdf_url, latest$pdf_url)
same_or_older_no <- !is.na(previous$sitrep_no) &&
  !is.na(latest$sitrep_no) &&
  latest$sitrep_no <= previous$sitrep_no

is_new <- FORCE_SEND || !(same_pdf || same_or_older_no)

if (!is_new) {
  log_msg("No new SitRep detected. Email not sent.")

  qc <- data.frame(
    run_time = as.character(Sys.time()),
    status = "no_new_sitrep",
    latest_sitrep_no = latest$sitrep_no,
    latest_page_url = latest$page_url,
    latest_pdf_url = latest$pdf_url,
    previous_sitrep_no = previous$sitrep_no,
    previous_pdf_url = previous$pdf_url,
    email_sent = FALSE,
    stringsAsFactors = FALSE
  )

  write.csv(qc, QC_FILE, row.names = FALSE)
  log_msg("QC written:", QC_FILE)
  quit(save = "no", status = 0)
}

# ============================================================
# DOWNLOAD PDF
# ============================================================

pdf_name <- safe_filename(latest$pdf_url, latest$sitrep_no)
pdf_path <- file.path(INCOMING_DIR, pdf_name)

ok_download <- download_binary(latest$pdf_url, pdf_path)

if (!ok_download) {
  stop_clean("Echec telechargement PDF:", latest$pdf_url)
}

log_msg("PDF downloaded:", pdf_path)
log_msg("PDF size bytes:", file.info(pdf_path)$size)

# ============================================================
# SEND EMAIL
# ============================================================

sitrep_label <- ifelse(is.na(latest$sitrep_no), "nouveau", paste0("N", latest$sitrep_no))

subject <- paste0("PREIS Ebola DRC - Nouveau SitRep ", sitrep_label)

body <- paste(
  "Bonjour,",
  "",
  paste0("Un nouveau SitRep Ebola/MVB RDC a ete detecte automatiquement par PREIS: ", sitrep_label),
  "",
  paste0("Page INSP: ", latest$page_url),
  paste0("PDF source: ", latest$pdf_url),
  paste0("Fichier joint: ", basename(pdf_path)),
  "",
  "Le PDF original est joint a cet email.",
  "",
  "PREIS Ebola DRC automated monitor",
  sep = "\n"
)

email_sent <- send_email_with_attachment(
  to = EMAIL_TO,
  from = EMAIL_FROM,
  subject = subject,
  body = body,
  attachment_path = pdf_path
)

if (!isTRUE(email_sent)) {
  stop_clean("Email non envoye.")
}

log_msg("Email sent successfully to:", EMAIL_TO)

# ============================================================
# UPDATE STATE ONLY AFTER SUCCESSFUL EMAIL
# ============================================================

new_state <- list(
  sitrep_no = latest$sitrep_no,
  page_url = latest$page_url,
  pdf_url = latest$pdf_url,
  pdf_path = pdf_path,
  sent_at = as.character(Sys.time())
)

saveRDS(new_state, STATE_FILE)

qc <- data.frame(
  run_time = as.character(Sys.time()),
  status = "new_sitrep_sent",
  latest_sitrep_no = latest$sitrep_no,
  latest_page_url = latest$page_url,
  latest_pdf_url = latest$pdf_url,
  pdf_path = pdf_path,
  pdf_size_bytes = file.info(pdf_path)$size,
  email_to = EMAIL_TO,
  email_sent = TRUE,
  stringsAsFactors = FALSE
)

write.csv(qc, QC_FILE, row.names = FALSE)

log_msg("State updated:", STATE_FILE)
log_msg("QC written:", QC_FILE)
log_msg("PREIS Ebola DRC cloud monitor completed successfully.")
