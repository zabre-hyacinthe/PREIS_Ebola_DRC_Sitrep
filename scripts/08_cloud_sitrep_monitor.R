############################################################
# PREIS Ebola RDC — Cloud SitRep Monitor
# GitHub Actions + cron
############################################################

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(xml2)
  library(stringr)
  library(dplyr)
  library(readr)
  library(digest)
})

ROOT <- getwd()

SCRIPT_EMAIL <- file.path(ROOT, "scripts", "00_email_smtp_base.R")
if (!file.exists(SCRIPT_EMAIL)) {
  stop("Helper email introuvable: scripts/00_email_smtp_base.R", call. = FALSE)
}
source(SCRIPT_EMAIL, encoding = "UTF-8")

STATE_DIR <- file.path(ROOT, "data", "monitor_state")
PDF_DIR <- file.path(ROOT, "data", "pdf")
LOG_DIR <- file.path(ROOT, "data", "logs")
OUT_DIR <- file.path(ROOT, "outputs", "cloud_monitor")

dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PDF_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

STATE_FILE <- file.path(STATE_DIR, "preis_sitrep_email_state.csv")
RUN_ID <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
LOG_FILE <- file.path(LOG_DIR, paste0("preis_cloud_sitrep_monitor_", format(Sys.Date(), "%Y%m%d"), ".log"))

DRY_RUN <- toupper(Sys.getenv("PREIS_DRY_RUN", "FALSE")) %in% c("TRUE", "1", "YES", "Y")
MAX_PAGES <- suppressWarnings(as.integer(Sys.getenv("PREIS_MAX_PAGES", "5")))
if (is.na(MAX_PAGES) || MAX_PAGES < 1) MAX_PAGES <- 5

log_msg <- function(...) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "] ", paste0(..., collapse = ""))
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
  as.character(x[1])
}

normalize_url <- function(url, base = "https://insp.cd") {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  xml2::url_absolute(url, base)
}

fetch_html <- function(url) {
  req <- request(url) |>
    req_user_agent("Mozilla/5.0 PREIS-Ebola-DRC-Monitor") |>
    req_timeout(60)

  req_perform(req) |>
    resp_body_html()
}

fetch_binary <- function(url, dest) {
  req <- request(url) |>
    req_user_agent("Mozilla/5.0 PREIS-Ebola-DRC-Monitor") |>
    req_timeout(120)

  resp <- req_perform(req)
  writeBin(resp_body_raw(resp), dest)
  invisible(dest)
}

extract_sitrep_no <- function(x) {
  x <- paste(x, collapse = " ")
  x <- str_replace_all(x, "%C2%B0|°", " ")

  patterns <- c(
    "(?i)sitrep[-_[:space:]]*n[°o#]?[-_[:space:]]*0*([0-9]{1,3})",
    "(?i)sitrep[-_[:space:]]*0*([0-9]{1,3})",
    "(?i)n[°o#]?[-_[:space:]]*0*([0-9]{1,3})[-_[:space:]]*mv[be]",
    "(?i)mv[be].*?0*([0-9]{1,3})"
  )

  vals <- integer()

  for (p in patterns) {
    m <- str_match_all(x, regex(p))[[1]]
    if (nrow(m) > 0) {
      nums <- suppressWarnings(as.integer(m[, 2]))
      nums <- nums[!is.na(nums)]
      vals <- c(vals, nums)
    }
  }

  if (length(vals) == 0) return(NA_integer_)
  max(vals, na.rm = TRUE)
}

extract_sitrep_date <- function(x) {
  x <- paste(x, collapse = " ")

  m1 <- str_match(x, "([0-9]{2})[-_/]([0-9]{2})[-_/]([0-9]{4})")
  if (!all(is.na(m1))) {
    return(paste0(m1[4], "-", m1[3], "-", m1[2]))
  }

  m2 <- str_match(x, "([0-9]{4})[-_/]([0-9]{2})[-_/]([0-9]{2})")
  if (!all(is.na(m2))) {
    return(paste0(m2[2], "-", m2[3], "-", m2[4]))
  }

  NA_character_
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) {
    return(tibble(
      run_id = character(),
      detected_at_utc = character(),
      sent_at_utc = character(),
      sitrep_no = integer(),
      sitrep_date = character(),
      page_url = character(),
      pdf_url = character(),
      pdf_sha256 = character(),
      pdf_file = character(),
      email_status = character(),
      note = character()
    ))
  }

  suppressMessages(readr::read_csv(STATE_FILE, show_col_types = FALSE))
}

write_state <- function(state) {
  readr::write_csv(state, STATE_FILE)
}

append_state <- function(row) {
  state <- read_state()
  state <- bind_rows(state, row)
  write_state(state)
}

already_sent <- function(sitrep_no, pdf_sha256 = NA_character_) {
  state <- read_state()

  if (nrow(state) == 0) return(FALSE)

  state <- state |>
    mutate(
      sitrep_no = suppressWarnings(as.integer(sitrep_no)),
      email_status = as.character(email_status)
    )

  hit <- state |>
    filter(
      .data$sitrep_no == !!sitrep_no,
      .data$email_status %in% c("sent", "dry_run")
    )

  nrow(hit) > 0
}

find_pdf_on_page <- function(page_url) {
  html <- fetch_html(page_url)

  hrefs <- html |>
    html_elements("a") |>
    html_attr("href")

  hrefs <- unique(hrefs)
  hrefs <- hrefs[!is.na(hrefs)]
  hrefs <- normalize_url(hrefs, page_url)

  pdfs <- hrefs[str_detect(pdfs <- hrefs, regex("\\.pdf($|\\?)", ignore_case = TRUE))]

  if (length(pdfs) > 0) {
    return(pdfs[1])
  }

  NA_character_
}

find_latest_sitrep <- function() {
  search_urls <- c(
    "https://insp.cd/?s=sitrep+mvb",
    "https://insp.cd/?s=SitRep+MVB",
    "https://insp.cd/?s=sitrep+ebola",
    "https://insp.cd/?s=MVE"
  )

  candidates <- tibble(
    sitrep_no = integer(),
    sitrep_date = character(),
    title = character(),
    page_url = character()
  )

  for (url in search_urls) {
    log_msg("Recherche INSP: ", url)

    html <- tryCatch(fetch_html(url), error = function(e) {
      log_msg("Echec lecture INSP: ", conditionMessage(e))
      NULL
    })

    if (is.null(html)) next

    a <- html_elements(html, "a")

    hrefs <- html_attr(a, "href")
    titles <- html_text2(a)

    df <- tibble(
      title = titles,
      page_url = normalize_url(hrefs, "https://insp.cd")
    ) |>
      filter(!is.na(page_url), nzchar(page_url)) |>
      mutate(
        joined = paste(title, page_url),
        keep = str_detect(joined, regex("sitrep", ignore_case = TRUE)) &
          str_detect(joined, regex("mvb|mve|ebola", ignore_case = TRUE)),
        sitrep_no = vapply(joined, extract_sitrep_no, integer(1)),
        sitrep_date = vapply(joined, extract_sitrep_date, character(1))
      ) |>
      filter(keep, !is.na(sitrep_no)) |>
      select(sitrep_no, sitrep_date, title, page_url)

    candidates <- bind_rows(candidates, df)
  }

  candidates <- candidates |>
    distinct(sitrep_no, page_url, .keep_all = TRUE) |>
    arrange(desc(sitrep_no))

  readr::write_csv(candidates, file.path(OUT_DIR, "latest_sitrep_candidates.csv"))

  if (nrow(candidates) == 0) {
    stop("Aucun SitRep MVB/MVE/Ebola détecté sur INSP.", call. = FALSE)
  }

  candidates[1, ]
}

log_msg("============================================================")
log_msg("PREIS Ebola RDC — démarrage monitor cloud")
log_msg("ROOT: ", ROOT)
log_msg("DRY_RUN: ", DRY_RUN)

latest <- find_latest_sitrep()

latest_no <- latest$sitrep_no
latest_date <- safe_chr(latest$sitrep_date)
latest_title <- safe_chr(latest$title)
latest_page_url <- safe_chr(latest$page_url)

log_msg("Dernier SitRep détecté: ", latest_no)
log_msg("Page INSP: ", latest_page_url)

if (already_sent(latest_no)) {
  log_msg("Notification déjà envoyée pour SitRep ", latest_no, ". Arrêt sans doublon.")
  quit(save = "no", status = 0)
}

pdf_url <- tryCatch(find_pdf_on_page(latest_page_url), error = function(e) {
  log_msg("PDF non détecté sur la page: ", conditionMessage(e))
  NA_character_
})

if (is.na(pdf_url) || !nzchar(pdf_url)) {
  log_msg("Aucun lien PDF direct trouvé. La notification utilisera la page INSP comme source.")
} else {
  log_msg("PDF détecté: ", pdf_url)
}

pdf_file <- NA_character_
pdf_sha256 <- NA_character_

if (!is.na(pdf_url) && nzchar(pdf_url)) {
  pdf_file <- file.path(PDF_DIR, paste0("PREIS_DRC_Ebola_SitRep_", sprintf("%03d", latest_no), ".pdf"))

  tryCatch({
    fetch_binary(pdf_url, pdf_file)

    if (!file.exists(pdf_file) || file.info(pdf_file)$size == 0) {
      stop("PDF téléchargé vide ou absent.")
    }

    pdf_sha256 <- digest::digest(file = pdf_file, algo = "sha256")
    log_msg("PDF sauvegardé: ", pdf_file)
    log_msg("SHA256: ", pdf_sha256)
  }, error = function(e) {
    log_msg("Erreur téléchargement PDF: ", conditionMessage(e))
    pdf_file <<- NA_character_
    pdf_sha256 <<- NA_character_
  })
}

subject <- paste0("[PREIS Ebola DRC] SitRep ", latest_no, " PDF")

body <- paste0(
  "Dear team,\n\n",
  "Please find attached the latest DRC Ebola SitRep detected by PREIS: SitRep ", latest_no, ".\n\n",
  "Title: ", ifelse(is.na(latest_title) || !nzchar(latest_title), "", latest_title), "\n",
  "INSP page: ", latest_page_url, "\n",
  "PDF source: PREIS cloud automation\n\n",
  "This is an automated PREIS notification. Analytical outputs will follow once generated and validated.\n\n",
  "Best regards,\n",
  "PREIS Ebola DRC Automation\n"
)

writeLines(body, file.path(OUT_DIR, paste0("latest_sitrep_", latest_no, "_email_body.txt")))

email_status <- "not_sent"
note <- ""

if (DRY_RUN) {
  log_msg("DRY_RUN=TRUE : email non envoyé.")
  email_status <- "dry_run"
  note <- "Dry run only; email not sent."
} else {
  log_msg("Envoi email SitRep ", latest_no, "...")

  tryCatch({
    preis_send_email(
      subject = subject,
      body = body,
      attachment = if (!is.na(pdf_file) && file.exists(pdf_file)) pdf_file else NULL
    )

    email_status <- "sent"
    note <- "Email sent successfully."
    log_msg("Email envoyé avec succès.")
  }, error = function(e) {
    email_status <<- "failed"
    note <<- conditionMessage(e)
    log_msg("Erreur envoi email: ", conditionMessage(e))
  })
}

state_row <- tibble(
  run_id = RUN_ID,
  detected_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  sent_at_utc = if (email_status %in% c("sent", "dry_run")) format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC") else NA_character_,
  sitrep_no = latest_no,
  sitrep_date = latest_date,
  page_url = latest_page_url,
  pdf_url = pdf_url,
  pdf_sha256 = pdf_sha256,
  pdf_file = pdf_file,
  email_status = email_status,
  note = note
)

append_state(state_row)

log_msg("Etat mis à jour: ", STATE_FILE)
log_msg("Statut email: ", email_status)

if (email_status == "failed") {
  stop("Notification échouée: ", note, call. = FALSE)
}

log_msg("Fin monitor PREIS.")
