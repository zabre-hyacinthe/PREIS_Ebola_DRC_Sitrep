############################################################
# PREIS Ebola RDC — 00_RUN_ALL_PRODUCTION.R
# Version adaptée GitHub Actions / cron
#
# Rôle principal dans le système cloud :
# 1) lire https://insp.cd/category/sitrep/
# 2) détecter le dernier SitRep MVB/MVE
# 3) résoudre le vrai PDF, y compris pdfemb-data base64
# 4) télécharger le PDF dans data/pdf/
# 5) écrire le registre et le manifest attendus par le monitor
#
# Ce script ne dépend pas de D:/PREIS...
# Il utilise la racine du dépôt GitHub : getwd()
############################################################

# ============================================================
# 00 — SETUP RACINE COMPATIBLE GITHUB
# ============================================================

ROOT <- Sys.getenv("GITHUB_WORKSPACE", unset = getwd())
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = FALSE)

BASE_DIR           <- ROOT
SCRIPT_DIR         <- file.path(BASE_DIR, "scripts")
DATA_RAW_DIR       <- file.path(BASE_DIR, "data/raw")
DATA_PROCESSED_DIR <- file.path(BASE_DIR, "data/processed")
DATA_FINAL_DIR     <- file.path(BASE_DIR, "data/final")
PDF_DIR            <- file.path(BASE_DIR, "data/pdf")
OUTPUT_DIR         <- file.path(BASE_DIR, "outputs")
CLOUD_OUT_DIR      <- file.path(BASE_DIR, "outputs/cloud_monitor")
DOC_DIR            <- file.path(BASE_DIR, "documentation")
LOG_DIR            <- file.path(BASE_DIR, "data/logs")

for (d in c(
  SCRIPT_DIR, DATA_RAW_DIR, DATA_PROCESSED_DIR,
  DATA_FINAL_DIR, PDF_DIR, OUTPUT_DIR, CLOUD_OUT_DIR,
  DOC_DIR, LOG_DIR
)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

INSP_CATEGORY_PAGE <- "https://insp.cd/category/sitrep/"
INSP_MAX_PAGES <- suppressWarnings(as.integer(Sys.getenv("PREIS_MAX_PAGES", "12")))
if (is.na(INSP_MAX_PAGES) || INSP_MAX_PAGES < 1) INSP_MAX_PAGES <- 12

REGISTRY_FP <- file.path(DATA_FINAL_DIR, "sitrep_registry.csv")
RUN_LOG_FP  <- file.path(LOG_DIR, "master_run_log.csv")
LOG_FILE    <- file.path(LOG_DIR, paste0("preis_run_all_production_", format(Sys.Date(), "%Y%m%d"), ".log"))

# ============================================================
# 01 — PACKAGES
# ============================================================

packages <- c(
  "dplyr", "readr", "stringr", "tibble", "purrr",
  "rvest", "httr", "base64enc", "digest"
)

install_missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(install_missing) > 0) {
  install.packages(install_missing, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(rvest)
  library(httr)
  library(base64enc)
  library(digest)
})

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

log_msg <- function(...) {
  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    paste0(..., collapse = "")
  )
  message(line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

log_msg("============================================================")
log_msg("PREIS 00_RUN_ALL_PRODUCTION.R started")
log_msg("Project root: ", BASE_DIR)
log_msg("INSP max pages: ", INSP_MAX_PAGES)

# ============================================================
# 02 — UTILITAIRES
# ============================================================

safe_num <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_replace_all(x, "%", "")
  x <- stringr::str_replace_all(x, "[^0-9\\.\\-]", "")
  suppressWarnings(as.numeric(x))
}

extract_sitrep_no <- function(url_or_name) {
  if (is.na(url_or_name) || url_or_name == "") return(NA_integer_)

  x <- as.character(url_or_name)
  x <- gsub("%C2%B0", "N", x, ignore.case = TRUE)
  x <- gsub("\\\\u00b0", "N", x, ignore.case = TRUE)

  m_post <- stringr::str_match(
    x,
    stringr::regex("sitrep[-_]?n0*(\\d{1,3})[-_]?mv[be]", ignore_case = TRUE)
  )[, 2]
  if (!is.na(m_post)) return(as.integer(m_post))

  fname <- basename(x)
  fname <- gsub("%[0-9A-Fa-f]{2}", "", fname)

  m <- stringr::str_match(
    fname,
    stringr::regex(
      "(?:SITREP|SitRep|sitrep)[-_ ]?(?:MVE[-_ ]?|MVB[-_ ]?)?(?:NUM[-_ ]?|N[-_ ]?)?0*(\\d{1,3})",
      ignore_case = TRUE
    )
  )[, 2]
  if (!is.na(m)) return(as.integer(m))

  m <- stringr::str_match(
    fname,
    stringr::regex("N[o°º]?0*(\\d{1,3})(?:[-_\\.]|$)", ignore_case = TRUE)
  )[, 2]
  if (!is.na(m)) return(as.integer(m))

  m <- stringr::str_match(
    fname,
    stringr::regex("NUM[-_ ]?0*(\\d{1,3})", ignore_case = TRUE)
  )[, 2]
  if (!is.na(m)) return(as.integer(m))

  NA_integer_
}

is_valid_pdf <- function(path) {
  if (is.na(path) || !file.exists(path)) return(FALSE)
  if (file.info(path)$size < 10240) return(FALSE)

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  sig <- tryCatch(
    rawToChar(readBin(con, "raw", n = 4)),
    error = function(e) ""
  )

  identical(sig, "%PDF")
}

safe_copy <- function(from, to) {
  if (!is.na(from) && file.exists(from)) {
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(from, to, overwrite = TRUE)
  }
}

# ============================================================
# 03 — GET AVEC RETRIES
# ============================================================

get_with_retry <- function(url, max_try = 4, timeout_sec = 90) {
  for (att in seq_len(max_try)) {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::timeout(timeout_sec),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 PREIS-Ebola-DRC-Automation",
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8"
        )
      ),
      error = function(e) {
        log_msg("GET attempt ", att, " failed for ", url, " | ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(resp) && httr::status_code(resp) == 200) return(resp)

    if (!is.null(resp)) {
      log_msg("GET attempt ", att, " HTTP ", httr::status_code(resp), " for ", url)
    }

    if (att < max_try) Sys.sleep(6)
  }

  NULL
}

# ============================================================
# 04 — RÉSOUDRE LE PDF DANS UNE PAGE SITREP
# ============================================================

resolve_pdf_url <- function(post_url) {
  resp <- get_with_retry(post_url, max_try = 3, timeout_sec = 90)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    return(NA_character_)
  }

  html_txt <- httr::content(resp, "text", encoding = "UTF-8")

  direct <- stringr::str_extract(
    html_txt,
    stringr::regex("https://insp\\.cd/wp-content/uploads/[^\"'\\s<>]+\\.pdf", ignore_case = TRUE)
  )

  if (!is.na(direct) && nzchar(direct)) {
    direct <- stringr::str_replace_all(direct, "\\\\/", "/")
    return(direct)
  }

  relative <- stringr::str_extract(
    html_txt,
    stringr::regex("/wp-content/uploads/[^\"'\\s<>]+\\.pdf", ignore_case = TRUE)
  )

  if (!is.na(relative) && nzchar(relative)) {
    return(paste0("https://insp.cd", relative))
  }

  b64 <- stringr::str_match(html_txt, "pdfemb-data=([A-Za-z0-9+/=]+)")[, 2]

  if (!is.na(b64) && nzchar(b64)) {
    decoded <- tryCatch(
      rawToChar(base64enc::base64decode(b64)),
      error = function(e) NA_character_
    )

    if (!is.na(decoded)) {
      url <- stringr::str_match(decoded, '"url"\\s*:\\s*"([^"]+\\.pdf)"')[, 2]

      if (!is.na(url) && nzchar(url)) {
        url <- stringr::str_replace_all(url, "\\\\/", "/")
        url <- stringr::str_replace_all(url, "\\\\u00b0", "°")
        url <- stringr::str_replace_all(url, "\\\\u[0-9a-fA-F]{4}", "")
        return(url)
      }
    }
  }

  NA_character_
}

# ============================================================
# 05 — SCRAPER LA CATÉGORIE SITREP
# ============================================================

scrape_insp_sitrep_list <- function(
  category_url = INSP_CATEGORY_PAGE,
  max_pages = INSP_MAX_PAGES
) {
  log_msg("Scraping INSP category: ", category_url)

  all_posts <- list()
  empty_streak <- 0

  for (pg in seq_len(max_pages)) {
    page_url <- if (pg == 1) {
      category_url
    } else {
      paste0(category_url, "page/", pg, "/")
    }

    log_msg("Reading category: ", page_url)

    resp <- get_with_retry(page_url, max_try = 4, timeout_sec = 90)

    if (is.null(resp)) {
      empty_streak <- empty_streak + 1
      log_msg("Page ", pg, " unreachable")
      if (empty_streak >= 3) break
      next
    }

    page_html <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))

    links <- page_html %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")

    texts <- page_html %>%
      rvest::html_nodes("a") %>%
      rvest::html_text(trim = TRUE)

    post_df <- tibble::tibble(
      post_url = links,
      post_text = texts
    ) %>%
      dplyr::filter(
        !is.na(post_url),
        stringr::str_detect(
          post_url,
          stringr::regex("sitrep[-_]?n\\d+[-_]?(mvb|mve)", ignore_case = TRUE)
        )
      ) %>%
      dplyr::mutate(
        sitrep_no = purrr::map_int(post_url, extract_sitrep_no),
        date_raw = stringr::str_match(
          post_url,
          stringr::regex("(?:mvb|mve)_(\\d{2}-\\d{2}-\\d{4})", ignore_case = TRUE)
        )[, 2],
        epidemic = "MVB_2026_Ituri"
      ) %>%
      dplyr::filter(!is.na(sitrep_no)) %>%
      dplyr::distinct(sitrep_no, .keep_all = TRUE)

    if (nrow(post_df) == 0) {
      empty_streak <- empty_streak + 1
      log_msg("Page ", pg, ": 0 SitRep posts")
      if (empty_streak >= 3) break
      next
    }

    empty_streak <- 0
    all_posts[[length(all_posts) + 1]] <- post_df
    log_msg("Page ", pg, ": ", nrow(post_df), " unique posts")
  }

  posts <- dplyr::bind_rows(all_posts)

  if (nrow(posts) == 0) {
    log_msg("No SitRep posts found.")
    return(tibble::tibble())
  }

  posts <- posts %>%
    dplyr::distinct(sitrep_no, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))

  log_msg("Found ", nrow(posts), " unique SitRep posts")
  log_msg("Latest SitRep online: ", max(posts$sitrep_no, na.rm = TRUE))

  posts$pdf_url <- purrr::map_chr(posts$post_url, function(u) {
    log_msg("Resolving PDF for: ", u)
    resolve_pdf_url(u)
  })

  posts <- posts %>%
    dplyr::filter(!is.na(pdf_url), nzchar(pdf_url)) %>%
    dplyr::mutate(
      link_text = paste0("SitRep N", sitrep_no, " MVB ", date_raw %||% ""),
      year_in_url = 2026L,
      source_page = post_url,
      scraped_at = as.character(Sys.time())
    ) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))

  readr::write_csv(posts, file.path(CLOUD_OUT_DIR, "production_sitrep_candidates.csv"))

  log_msg("Resolved ", nrow(posts), " PDF URLs")

  posts
}

# ============================================================
# 06 — REGISTRE
# ============================================================

load_registry <- function() {
  empty <- tibble::tibble(
    sitrep_no = integer(),
    pdf_url = character(),
    date_raw = character(),
    link_text = character(),
    year_in_url = integer(),
    downloaded = logical(),
    extracted = logical(),
    analysed = logical(),
    local_pdf = character(),
    source_page = character(),
    first_seen = character(),
    last_updated = character(),
    scraped_at = character()
  )

  if (!file.exists(REGISTRY_FP)) return(empty)

  reg <- tryCatch(
    readr::read_csv(REGISTRY_FP, show_col_types = FALSE),
    error = function(e) empty
  )

  for (col in names(empty)) {
    if (!col %in% names(reg)) reg[[col]] <- empty[[col]]
  }

  if ("pdf_url" %in% names(reg) && nrow(reg) > 0) {
    reg$sitrep_no <- purrr::map_int(reg$pdf_url, extract_sitrep_no)
  }

  reg %>%
    dplyr::filter(!is.na(sitrep_no), sitrep_no >= 1)
}

save_registry <- function(registry) {
  readr::write_csv(registry, REGISTRY_FP)
}

update_registry_with_scraped <- function(scraped, registry) {
  if (nrow(scraped) == 0) return(registry)

  scraped2 <- scraped %>%
    dplyr::transmute(
      sitrep_no,
      pdf_url,
      date_raw,
      link_text,
      year_in_url,
      downloaded = FALSE,
      extracted = FALSE,
      analysed = FALSE,
      local_pdf = NA_character_,
      source_page,
      first_seen = as.character(Sys.time()),
      last_updated = as.character(Sys.time()),
      scraped_at = as.character(Sys.time())
    )

  dplyr::bind_rows(registry, scraped2) %>%
    dplyr::arrange(dplyr::desc(sitrep_no)) %>%
    dplyr::distinct(sitrep_no, .keep_all = TRUE)
}

# ============================================================
# 07 — TÉLÉCHARGEMENT PDF
# ============================================================

download_sitrep_pdf <- function(pdf_url, sitrep_no, pdf_dir = PDF_DIR, max_retries = 4) {
  canonical_file <- file.path(pdf_dir, paste0("SitRep_", sitrep_no, "_2026.pdf"))

  if (file.exists(canonical_file) && is_valid_pdf(canonical_file)) {
    log_msg("Already downloaded and valid: ", canonical_file)
    return(canonical_file)
  }

  dl_url <- utils::URLencode(pdf_url, reserved = FALSE)
  dl_url <- gsub("%25C2%25B0", "%C2%B0", dl_url)
  dl_url <- gsub("%25", "%", dl_url)

  log_msg("Downloading SitRep ", sitrep_no, " from: ", dl_url)

  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(
      httr::GET(
        dl_url,
        httr::timeout(240),
        httr::write_disk(canonical_file, overwrite = TRUE),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 PREIS-Ebola-DRC-Automation",
          "Referer" = INSP_CATEGORY_PAGE,
          "Accept" = "application/pdf,*/*"
        )
      ),
      error = function(e) {
        log_msg("Download attempt ", attempt, " error: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(resp)) {
      sc <- httr::status_code(resp)
      ct <- httr::headers(resp)[["content-type"]] %||% NA_character_
      size_kb <- if (file.exists(canonical_file)) round(file.info(canonical_file)$size / 1024, 1) else 0

      log_msg("Download attempt ", attempt, " HTTP ", sc, " | size ", size_kb, " KB | content-type ", ct)

      if (sc == 200 && is_valid_pdf(canonical_file)) {
        log_msg("PDF valid: ", canonical_file)
        return(canonical_file)
      }
    }

    if (attempt < max_retries) Sys.sleep(8)
  }

  log_msg("Trying fallback download.file()")

  ok <- tryCatch({
    old_opt <- options(timeout = 300)
    on.exit(options(old_opt), add = TRUE)

    utils::download.file(
      dl_url,
      canonical_file,
      mode = "wb",
      quiet = TRUE,
      method = "libcurl",
      headers = c(
        "User-Agent" = "Mozilla/5.0 PREIS-Ebola-DRC-Automation",
        "Referer" = INSP_CATEGORY_PAGE
      )
    )

    is_valid_pdf(canonical_file)
  }, error = function(e) {
    log_msg("Fallback error: ", conditionMessage(e))
    FALSE
  })

  if (isTRUE(ok)) {
    log_msg("PDF valid after fallback: ", canonical_file)
    return(canonical_file)
  }

  if (file.exists(canonical_file)) file.remove(canonical_file)

  log_msg("FAILED to download valid PDF for SitRep ", sitrep_no)
  NA_character_
}

create_pdf_aliases <- function(local_pdf, sitrep_no) {
  if (is.na(local_pdf) || !file.exists(local_pdf)) return(invisible(FALSE))

  aliases <- c(
    file.path(PDF_DIR, paste0("SitRep_", sitrep_no, "_2026.pdf")),
    file.path(PDF_DIR, paste0("SitRep_", sprintf("%02d", sitrep_no), "_2026.pdf")),
    file.path(PDF_DIR, paste0("SitRep_", sprintf("%03d", sitrep_no), "_2026.pdf")),
    file.path(PDF_DIR, paste0("PREIS_DRC_Ebola_SitRep_", sitrep_no, ".pdf")),
    file.path(PDF_DIR, paste0("PREIS_DRC_Ebola_SitRep_", sprintf("%03d", sitrep_no), ".pdf")),
    file.path(PDF_DIR, paste0("sitrep_", sitrep_no, ".pdf")),
    file.path(PDF_DIR, paste0("sitrep_", sprintf("%03d", sitrep_no), ".pdf")),
    file.path(PDF_DIR, "latest_sitrep.pdf")
  )

  aliases <- unique(aliases)

  for (a in aliases) {
    if (normalizePath(a, winslash = "/", mustWork = FALSE) != normalizePath(local_pdf, winslash = "/", mustWork = FALSE)) {
      safe_copy(local_pdf, a)
    }
  }

  invisible(TRUE)
}

# ============================================================
# 08 — MAIN
# ============================================================

run_preis_pipeline <- function(force_redownload = FALSE, max_new = 1) {
  log_msg("--- STEP 1: Scraping INSP ---")

  scraped <- scrape_insp_sitrep_list()

  if (nrow(scraped) == 0) {
    stop("Aucun SitRep avec PDF résolu depuis INSP.", call. = FALSE)
  }

  registry <- load_registry()
  registry <- update_registry_with_scraped(scraped, registry)
  save_registry(registry)

  latest <- scraped %>%
    dplyr::arrange(dplyr::desc(sitrep_no)) %>%
    dplyr::slice(1)

  latest_no <- latest$sitrep_no
  latest_pdf_url <- latest$pdf_url
  latest_page <- latest$source_page
  latest_title <- latest$link_text

  log_msg("Latest online SitRep: ", latest_no)
  log_msg("Latest page: ", latest_page)
  log_msg("Latest PDF URL: ", latest_pdf_url)

  local_pdf <- download_sitrep_pdf(latest_pdf_url, latest_no)

  if (is.na(local_pdf) || !is_valid_pdf(local_pdf)) {
    stop("Latest SitRep PDF not downloaded/valid. SitRep: ", latest_no, call. = FALSE)
  }

  create_pdf_aliases(local_pdf, latest_no)

  pdf_sha256 <- digest::digest(file = local_pdf, algo = "sha256")

  registry <- registry %>%
    dplyr::mutate(
      downloaded = dplyr::if_else(sitrep_no == latest_no, TRUE, downloaded),
      extracted = dplyr::if_else(sitrep_no == latest_no, TRUE, extracted),
      analysed = dplyr::if_else(sitrep_no == latest_no, FALSE, analysed),
      local_pdf = dplyr::if_else(sitrep_no == latest_no, local_pdf, local_pdf),
      last_updated = dplyr::if_else(sitrep_no == latest_no, as.character(Sys.time()), last_updated)
    )

  save_registry(registry)

  manifest <- tibble::tibble(
    detected_at = as.character(Sys.time()),
    sitrep_no = latest_no,
    title = latest_title,
    page_url = latest_page,
    pdf_url = latest_pdf_url,
    pdf_file = local_pdf,
    pdf_sha256 = pdf_sha256,
    pdf_valid = is_valid_pdf(local_pdf)
  )

  readr::write_csv(manifest, file.path(CLOUD_OUT_DIR, "latest_sitrep_pdf_manifest.csv"))
  readr::write_csv(manifest, file.path(DATA_FINAL_DIR, "latest_sitrep_pdf_manifest.csv"))

  run_log <- tibble::tibble(
    run_time = as.character(Sys.time()),
    n_scraped = nrow(scraped),
    latest_sitrep = latest_no,
    latest_pdf = local_pdf,
    pdf_valid = is_valid_pdf(local_pdf),
    pdf_sha256 = pdf_sha256
  )

  if (file.exists(RUN_LOG_FP)) {
    old_log <- tryCatch(
      readr::read_csv(RUN_LOG_FP, show_col_types = FALSE),
      error = function(e) tibble::tibble()
    )
    run_log <- dplyr::bind_rows(old_log, run_log)
  }

  readr::write_csv(run_log, RUN_LOG_FP)

  log_msg("PDF saved: ", local_pdf)
  log_msg("PDF SHA256: ", pdf_sha256)
  log_msg("PDF aliases created in: ", PDF_DIR)
  log_msg("Manifest saved: ", file.path(CLOUD_OUT_DIR, "latest_sitrep_pdf_manifest.csv"))
  log_msg("PREIS 00_RUN_ALL_PRODUCTION.R finished successfully")

  invisible(list(
    latest = latest,
    registry = registry,
    manifest = manifest,
    local_pdf = local_pdf
  ))
}

results <- run_preis_pipeline(
  force_redownload = FALSE,
  max_new = 1
)
