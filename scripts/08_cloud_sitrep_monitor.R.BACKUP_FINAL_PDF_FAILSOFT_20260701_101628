############################################################
# PREIS patch: robust INSP PDF download
############################################################
.preis_pdf_patch_file <- file.path(getwd(), 'scripts', 'preis_robust_pdf_download_patch.R')
if (file.exists(.preis_pdf_patch_file)) {
  source(.preis_pdf_patch_file, local = TRUE)
} else {
  stop('Patch fichier téléchargement PDF introuvable : ', .preis_pdf_patch_file)
}
############################################################

############################################################
# PREIS Ebola RDC â€” Cloud SitRep Monitor
# Fichier: scripts/08_cloud_sitrep_monitor.R
#
# Objectif:
#   - Scanner INSP
#   - Identifier le dernier SitRep MVB
#   - TÃ©lÃ©charger le PDF tel quel
#   - Envoyer le PDF par email
#   - Eviter les doublons via state CSV
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(xml2)
  library(stringr)
  library(dplyr)
  library(readr)
  library(tibble)
  library(purrr)
  library(digest)
  library(base64enc)
})

ROOT <- getwd()

SCRIPT_EMAIL <- file.path(ROOT, "scripts", "00_email_smtp_base.R")

if (!file.exists(SCRIPT_EMAIL)) {
  stop("Helper email introuvable: scripts/00_email_smtp_base.R", call. = FALSE)
}

source(SCRIPT_EMAIL, encoding = "UTF-8")

SCRIPT_WHATSAPP <- file.path(ROOT, "scripts", "10_whatsapp_notify.R")

if (file.exists(SCRIPT_WHATSAPP)) {
  tryCatch(
    source(SCRIPT_WHATSAPP, encoding = "UTF-8"),
    error = function(e) message("WhatsApp helper non charge: ", conditionMessage(e))
  )
}

INSP_CATEGORY_PAGE <- "https://insp.cd/category/sitrep/"
MAX_PAGES <- suppressWarnings(as.integer(Sys.getenv("PREIS_MAX_PAGES", "6")))

if (is.na(MAX_PAGES) || MAX_PAGES < 1) {
  MAX_PAGES <- 6L
}

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
LOG_FILE <- file.path(
  LOG_DIR,
  paste0("preis_cloud_sitrep_monitor_", format(Sys.Date(), "%Y%m%d"), ".log")
)

DRY_RUN <- toupper(Sys.getenv("PREIS_DRY_RUN", "FALSE")) %in% c("TRUE", "1", "YES", "Y")

log_msg <- function(...) {
  line <- paste0(
    "[",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    "] ",
    paste0(..., collapse = "")
  )
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  as.character(x[1])
}

get_with_retry <- function(url, timeout_sec = 90, max_try = 4) {
  for (i in seq_len(max_try)) {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::timeout(timeout_sec),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 PREIS-Ebola-DRC-Monitor",
          "Accept" = "text/html,application/xhtml+xml,application/pdf,*/*",
          "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8"
        )
      ),
      error = function(e) {
        log_msg("GET error attempt ", i, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(resp) && httr::status_code(resp) == 200) {
      return(resp)
    }

    if (i < max_try) {
      Sys.sleep(5)
    }
  }

  NULL
}

normalize_url <- function(url, base = "https://insp.cd") {
  if (is.null(url) || length(url) == 0) {
    return(character())
  }

  url <- as.character(url)

  out <- rep(NA_character_, length(url))
  ok <- !is.na(url) & nzchar(url)

  if (any(ok)) {
    out[ok] <- xml2::url_absolute(url[ok], base)
  }

  out
}

extract_sitrep_no <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(NA_integer_)
  }

  x <- paste(as.character(x), collapse = " ")
  x <- stringr::str_to_lower(x)

  m1 <- stringr::str_match(x, "sitrep-n(\\d+)-mvb")[, 2]
  if (!is.na(m1)) {
    return(as.integer(m1))
  }

  m2 <- stringr::str_match(x, "sitrep[^0-9]{0,12}0*(\\d{1,3})")[, 2]
  if (!is.na(m2)) {
    return(as.integer(m2))
  }

  NA_integer_
}

extract_sitrep_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(NA_character_)
  }

  x <- paste(as.character(x), collapse = " ")

  m1 <- stringr::str_match(x, "(\\d{2})-(\\d{2})-(\\d{4})")

  if (!all(is.na(m1))) {
    return(paste0(m1[4], "-", m1[3], "-", m1[2]))
  }

  NA_character_
}

resolve_pdf_url <- function(post_url) {
  resp <- get_with_retry(post_url, timeout_sec = 90, max_try = 3)

  if (is.null(resp)) {
    return(NA_character_)
  }

  html_txt <- httr::content(resp, "text", encoding = "UTF-8")

  direct <- stringr::str_extract(
    html_txt,
    "https://insp\\.cd/wp-content/uploads/[^\"'\\s]+\\.pdf"
  )

  if (!is.na(direct)) {
    return(direct)
  }

  b64 <- stringr::str_match(
    html_txt,
    "pdfemb-data=([A-Za-z0-9+/=]+)"
  )[, 2]

  if (!is.na(b64)) {
    decoded <- tryCatch(
      rawToChar(base64enc::base64decode(b64)),
      error = function(e) NA_character_
    )

    if (!is.na(decoded)) {
      url <- stringr::str_match(
        decoded,
        "\"url\"\\s*:\\s*\"([^\"]+\\.pdf)\""
      )[, 2]

      if (!is.na(url)) {
        url <- stringr::str_replace_all(url, "\\\\/", "/")
        url <- stringr::str_replace_all(url, "\\\\u00b0", "Â°")
        url <- stringr::str_replace_all(url, "\\\\u[0-9a-fA-F]{4}", "")
        return(url)
      }
    }
  }

  NA_character_
}

scrape_latest_sitrep <- function() {
  log_msg("Scanning INSP category page: ", INSP_CATEGORY_PAGE)

  all_posts <- list()

  for (pg in seq_len(MAX_PAGES)) {
    page_url <- if (pg == 1) {
      INSP_CATEGORY_PAGE
    } else {
      paste0(INSP_CATEGORY_PAGE, "page/", pg, "/")
    }

    resp <- get_with_retry(page_url, timeout_sec = 90, max_try = 3)

    if (is.null(resp)) {
      if (pg == 1) {
        log_msg("Cannot reach INSP category page.")
      }
      break
    }

    page_html <- rvest::read_html(
      httr::content(resp, "text", encoding = "UTF-8")
    )

    links <- page_html |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    texts <- page_html |>
      rvest::html_nodes("a") |>
      rvest::html_text(trim = TRUE)

    posts_pg <- tibble::tibble(
      post_url = links,
      post_text = texts
    ) |>
      dplyr::filter(
        !is.na(post_url),
        stringr::str_detect(post_url, "sitrep-n\\d+-mvb")
      ) |>
      dplyr::mutate(
        post_url = normalize_url(post_url, "https://insp.cd"),
        sitrep_no = purrr::map_int(post_url, extract_sitrep_no),
        sitrep_date = purrr::map_chr(post_url, extract_sitrep_date)
      ) |>
      dplyr::filter(!is.na(sitrep_no)) |>
      dplyr::distinct(sitrep_no, .keep_all = TRUE)

    if (nrow(posts_pg) > 0) {
      all_posts[[length(all_posts) + 1]] <- posts_pg
      log_msg("Page ", pg, ": ", nrow(posts_pg), " SitRep candidate(s)")
    }
  }

  posts <- dplyr::bind_rows(all_posts)

  if (nrow(posts) == 0) {
    stop("Aucun SitRep MVB dÃ©tectÃ© sur INSP.", call. = FALSE)
  }

  posts <- posts |>
    dplyr::distinct(sitrep_no, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(sitrep_no))

  readr::write_csv(posts, file.path(OUT_DIR, "latest_sitrep_candidates.csv"))

  latest <- posts[1, , drop = FALSE]

  log_msg("Latest SitRep online: N", latest$sitrep_no)
  log_msg("SitRep page: ", latest$post_url)

  latest$pdf_url <- resolve_pdf_url(latest$post_url)

  if (is.na(latest$pdf_url) || !nzchar(latest$pdf_url)) {
    stop("PDF URL could not be resolved for SitRep N", latest$sitrep_no, call. = FALSE)
  }

  log_msg("PDF URL resolved: ", latest$pdf_url)

  latest
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) {
    return(tibble::tibble(
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

  state <- suppressMessages(
    readr::read_csv(
      STATE_FILE,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  )

  if (!"sitrep_no" %in% names(state)) {
    state$sitrep_no <- NA_character_
  }

  state$sitrep_no <- suppressWarnings(as.integer(state$sitrep_no))
  state
}

write_state <- function(state) {
  readr::write_csv(state, STATE_FILE, na = "")
}

append_state <- function(row) {
  state <- read_state()
  state <- dplyr::bind_rows(state, row)
  write_state(state)
}

already_sent <- function(sitrep_no) {
  state <- read_state()

  if (nrow(state) == 0) {
    return(FALSE)
  }

  hit <- state |>
    dplyr::mutate(
      sitrep_no = suppressWarnings(as.integer(sitrep_no)),
      email_status = as.character(email_status)
    ) |>
    dplyr::filter(
      .data$sitrep_no == !!as.integer(sitrep_no),
      .data$email_status %in% c("sent", "dry_run")
    )

  nrow(hit) > 0
}

download_pdf <- function(pdf_url, sitrep_no) {
  pdf_file <- file.path(
    PDF_DIR,
    paste0("PREIS_DRC_Ebola_SitRep_", sprintf("%03d", as.integer(sitrep_no)), ".pdf")
  )

  if (file.exists(pdf_file) && file.info(pdf_file)$size > 10240) {
    log_msg("PDF already exists locally: ", pdf_file)
    return(pdf_file)
  }

  dl_url <- utils::URLencode(pdf_url, reserved = FALSE)
  dl_url <- gsub("%25C2%25B0", "%C2%B0", dl_url)
  dl_url <- gsub("%25", "%", dl_url)

  log_msg("Downloading PDF SitRep N", sitrep_no)

  for (attempt in 1:3) {
    resp <- tryCatch(
      httr::GET(
        dl_url,
        httr::timeout(180),
        httr::write_disk(pdf_file, overwrite = TRUE),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 PREIS-Ebola-DRC-Monitor",
          "Referer" = INSP_CATEGORY_PAGE,
          "Accept" = "application/pdf,*/*"
        )
      ),
      error = function(e) {
        log_msg("PDF download error attempt ", attempt, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(resp) && httr::status_code(resp) == 200) {
      if (file.exists(pdf_file) && file.info(pdf_file)$size > 10240) {
        log_msg("PDF downloaded: ", pdf_file)
        return(pdf_file)
      }
    }

    if (attempt < 3) {
      Sys.sleep(5)
    }
  }

  stop("PDF download failed for SitRep N", sitrep_no, call. = FALSE)
}


# ============================================================
# PATCH â€” compatibility with old state CSV schema
# This overrides read_state() and already_sent() safely.
# ============================================================

empty_state_template <- function() {
  tibble::tibble(
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
  )
}

normalize_state_schema <- function(state) {
  template <- empty_state_template()

  if (is.null(state) || nrow(state) == 0) {
    return(template)
  }

  if (!"email_status" %in% names(state)) {
    if ("status" %in% names(state)) {
      state$email_status <- as.character(state$status)
    } else if ("email_sent" %in% names(state)) {
      state$email_status <- ifelse(
        as.character(state$email_sent) %in% c("TRUE", "true", "1", "Yes", "yes"),
        "sent",
        "not_sent"
      )
    } else {
      state$email_status <- NA_character_
    }
  }

  for (nm in names(template)) {
    if (!nm %in% names(state)) {
      state[[nm]] <- NA_character_
    }
  }

  state <- state |>
    dplyr::mutate(
      run_id = as.character(run_id),
      detected_at_utc = as.character(detected_at_utc),
      sent_at_utc = as.character(sent_at_utc),
      sitrep_no = suppressWarnings(as.integer(sitrep_no)),
      sitrep_date = as.character(sitrep_date),
      page_url = as.character(page_url),
      pdf_url = as.character(pdf_url),
      pdf_sha256 = as.character(pdf_sha256),
      pdf_file = as.character(pdf_file),
      email_status = as.character(email_status),
      note = as.character(note)
    ) |>
    dplyr::select(dplyr::all_of(names(template)), dplyr::everything())

  state
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) {
    return(empty_state_template())
  }

  state <- suppressMessages(
    readr::read_csv(
      STATE_FILE,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  )

  normalize_state_schema(state)
}

write_state <- function(state) {
  state <- normalize_state_schema(state)
  readr::write_csv(state, STATE_FILE, na = "")
}

already_sent <- function(sitrep_no) {
  state <- read_state()

  if (nrow(state) == 0) {
    return(FALSE)
  }

  state <- normalize_state_schema(state)

  hit <- state |>
    dplyr::filter(
      !is.na(sitrep_no),
      .data$sitrep_no == !!as.integer(sitrep_no),
      .data$email_status %in% c("sent", "dry_run")
    )

  nrow(hit) > 0
}

log_msg("============================================================")
log_msg("PREIS Ebola RDC â€” Cloud SitRep Monitor started")
log_msg("ROOT: ", ROOT)
log_msg("DRY_RUN: ", DRY_RUN)
log_msg("MAX_PAGES: ", MAX_PAGES)

latest <- scrape_latest_sitrep()

latest_no <- as.integer(latest$sitrep_no)
latest_date <- safe_chr(latest$sitrep_date)
latest_title <- safe_chr(latest$post_text)
latest_page_url <- safe_chr(latest$post_url)
latest_pdf_url <- safe_chr(latest$pdf_url)

if (already_sent(latest_no)) {
  log_msg("SitRep N", latest_no, " already sent. No duplicate email.")
  quit(save = "no", status = 0)
}

email_status <- "not_sent"
note <- ""
pdf_file <- NA_character_
pdf_sha256 <- NA_character_

tryCatch(
  {
    pdf_file <- download_pdf(latest_pdf_url, latest_no)
    pdf_sha256 <- digest::digest(file = pdf_file, algo = "sha256")
    log_msg("PDF SHA256: ", pdf_sha256)
  },
  error = function(e) {
    email_status <<- "failed"
    note <<- conditionMessage(e)

    state_row <- tibble::tibble(
      run_id = RUN_ID,
      detected_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      sent_at_utc = NA_character_,
      sitrep_no = latest_no,
      sitrep_date = latest_date,
      page_url = latest_page_url,
      pdf_url = latest_pdf_url,
      pdf_sha256 = NA_character_,
      pdf_file = NA_character_,
      email_status = email_status,
      note = note
    )

    append_state(state_row)
    stop(note, call. = FALSE)
  }
)

subject <- paste0("[PREIS Ebola DRC] Nouveau SitRep INSP â€” N", latest_no)

body <- paste0(
  "Dear colleagues,\n\n",
  "PREIS has identified a newly published INSP Situation Report for the Ebola ",
  "(MVE/MVB) outbreak in the Democratic Republic of the Congo.\n\n",
  "SitRep: N", latest_no, "\n",
  "Title: ", ifelse(is.na(latest_title) || !nzchar(latest_title),
                    "Not available on the INSP page", latest_title), "\n",
  "INSP page: ", latest_page_url, "\n",
  "PDF source: ", latest_pdf_url, "\n\n",
  "The official PDF is attached as received from INSP.\n",
  "Automated analytical outputs will follow once generated and validated.\n\n",
  "For urgent follow-up, please contact Dr Hyacinthe Zabre on WhatsApp: ",
  "+226 78 08 87 70.\n\n",
  "Best regards,\n",
  "PREIS Ebola DRC Automation\n"
)

writeLines(
  body,
  file.path(OUT_DIR, paste0("latest_sitrep_", latest_no, "_email_body.txt")),
  useBytes = TRUE
)

if (DRY_RUN) {
  log_msg("DRY_RUN=TRUE: email not sent.")
  email_status <- "dry_run"
  note <- "Dry run only; email not sent."
} else {
  log_msg("Sending email for SitRep N", latest_no)

  tryCatch(
    {
      preis_send_email(
        subject = subject,
        body = body,
        attachment = pdf_file
      )

      email_status <- "sent"
      note <- "Email sent successfully."
      log_msg("Email sent successfully.")
    },
    error = function(e) {
      email_status <<- "failed"
      note <<- conditionMessage(e)
      log_msg("Email sending failed: ", note)
    }
  )
}

state_row <- tibble::tibble(
  run_id = RUN_ID,
  detected_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  sent_at_utc = if (email_status %in% c("sent", "dry_run")) {
    format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    NA_character_
  },
  sitrep_no = latest_no,
  sitrep_date = latest_date,
  page_url = latest_page_url,
  pdf_url = latest_pdf_url,
  pdf_sha256 = pdf_sha256,
  pdf_file = pdf_file,
  email_status = email_status,
  note = note
)

append_state(state_row)

log_msg("State updated: ", STATE_FILE)
log_msg("Email status: ", email_status)

if (email_status == "sent" && exists("preis_send_whatsapp", mode = "function")) {
  tryCatch(
    {
      wa <- preis_send_whatsapp(
        sitrep_no = latest_no,
        page_url  = latest_page_url,
        pdf_url   = latest_pdf_url
      )
      if (isTRUE(wa$enabled)) {
        log_msg("WhatsApp: ", wa$sent, " envoye(s), ", wa$failed, " echec(s)")
      }
    },
    error = function(e) {
      log_msg("WhatsApp bloc erreur: ", conditionMessage(e))
      strict <- toupper(Sys.getenv("WA_STRICT", "FALSE")) %in% c("TRUE", "1", "YES", "Y")
      if (strict) stop(e)
    }
  )
}

if (email_status == "failed") {
  stop("Notification failed: ", note, call. = FALSE)
}

log_msg("PREIS cloud monitor finished.")
