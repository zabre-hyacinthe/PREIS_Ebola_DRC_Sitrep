############################################################
# PREIS EBOLA DRC SITREP
# 00_PREIS_MASTER_AUTOMATION.R
# VERSION CORRIGÉE — 2026-06-12
# PIPELINE COMPLET — SURVEILLANCE + EXTRACTION + ANALYSE
#
# Objectif : surveiller https://insp.cd/category/sitrep/ toutes les
# N heures, détecter les nouveaux SitReps 2026, lire le PDF,
# extraire les indicateurs et les zones de santé, produire
# les outputs opérationnels.
#
# Usage :
#   source("00_PREIS_MASTER_AUTOMATION.R")
#   ou planifier via taskscheduleR / cron
############################################################

# ============================================================
# 00 — SETUP
# ============================================================

BASE_DIR           <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
SCRIPT_DIR         <- file.path(BASE_DIR, "scripts")
DATA_RAW_DIR       <- file.path(BASE_DIR, "data/raw")
DATA_PROCESSED_DIR <- file.path(BASE_DIR, "data/processed")
DATA_FINAL_DIR     <- file.path(BASE_DIR, "data/final")
PDF_DIR            <- file.path(BASE_DIR, "data/pdf")
OUTPUT_DIR         <- file.path(BASE_DIR, "outputs")
DOC_DIR            <- file.path(BASE_DIR, "documentation")
LOG_DIR            <- file.path(BASE_DIR, "logs")

for (d in c(SCRIPT_DIR, DATA_RAW_DIR, DATA_PROCESSED_DIR,
            DATA_FINAL_DIR, PDF_DIR, OUTPUT_DIR, DOC_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# SOURCE DE DONNEES — 17eme EPIDEMIE 2026 (Ituri, Bundibugyo)
# ============================================================
# Les SitReps 2026 sont publies comme POSTS WordPress sur la
# page categorie. Chaque post contient un PDF embarque (base64).
# NB: la page /ebola/ contient l ANCIENNE epidemie Bulape 2025
# (SitRep 1-40) — on ne l utilise PAS pour 2026.
INSP_CATEGORY_PAGE <- "https://insp.cd/category/sitrep/"
INSP_MAX_PAGES     <- 6   # nb de pages de pagination a scanner

# Fichier de registre des SitReps connus
REGISTRY_FP  <- file.path(DATA_FINAL_DIR, "sitrep_registry.csv")
RUN_LOG_FP   <- file.path(LOG_DIR, "master_run_log.csv")

# ============================================================
# PACKAGES
# ============================================================

packages <- c(
  "dplyr", "readr", "stringr", "tibble", "tidyr",
  "purrr", "openxlsx", "glue", "lubridate",
  "rvest", "httr", "pdftools", "base64enc"
)

install_missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(install_missing) > 0) {
  install.packages(install_missing)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(openxlsx)
  library(glue)
  library(lubridate)
  library(rvest)
  library(httr)
  library(pdftools)
  library(base64enc)
})

cat("\n============================================================\n")
cat("PREIS EBOLA DRC — MASTER AUTOMATION PIPELINE\n")
cat("Run time:", as.character(Sys.time()), "\n")
cat("============================================================\n\n")

# ============================================================
# FONCTIONS UTILITAIRES
# ============================================================


# Opérateur pour remplacer NULL / NA proprement
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

safe_num <- function(x) {
  # Convertit proprement les nombres extraits des PDF :
  # "1 234", "20,1%", "20.1%" -> numeric.
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- as.character(x[1])
  if (is.na(x) || stringr::str_squish(x) == "") return(NA_real_)

  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_replace_all(x, "%", "")
  x <- stringr::str_extract(x, "-?\\d+(?:\\.\\d+)?")

  suppressWarnings(as.numeric(x))
}

# ------------------------------------------------------------
# EXTRACT SITREP NUMBER FROM A URL OR FILENAME
# Single source of truth — used everywhere for consistency.
# Handles: SitRep32.pdf, SITREP40_MVE16-B.pdf,
#          SITREP-MVE-NUM-23-1.pdf, SitRep_MVE_RDC_N017_...pdf,
#          SitRep_MVE_RDC_N°027_...pdf, Draft_SitRep_..._20260520_...
# ------------------------------------------------------------
extract_sitrep_no <- function(url_or_name) {
  if (is.na(url_or_name) || url_or_name == "") return(NA_integer_)

  # Post-URL format: .../sitrep-n27-mvb_10-06-2026/
  m_post <- stringr::str_match(as.character(url_or_name), "sitrep-n(\\d+)-mvb")[, 2]
  if (!is.na(m_post)) return(as.integer(m_post))

  fname <- basename(as.character(url_or_name))
  # URL-decode %C2%B0 -> ° so N°027 works.
  fname <- tryCatch(utils::URLdecode(fname), error = function(e) fname)
  fname <- gsub("\\u00b0", "°", fname, ignore.case = TRUE)
  fname <- gsub("\\u00ba", "°", fname, ignore.case = TRUE)

  # Pattern A: SITREP / SitRep directly followed by digits
  m <- stringr::str_match(
    fname,
    "(?:SITREP|SitRep|sitrep)[-_ ]?(?:MVE[-_ ]?|MVB[-_ ]?)?(?:NUM[-_ ]?|N[-_ ]?)?0*(\\d{1,3})"
  )[, 2]
  if (!is.na(m)) return(as.integer(m))

  # Pattern B: N°NN / N0NN / NNN anywhere
  m <- stringr::str_match(fname, "N[o\u00b0\u00ba]?0*(\\d{1,3})(?:[-_\\.]|$)")[, 2]
  if (!is.na(m)) return(as.integer(m))

  # Pattern C: NUM-NN
  m <- stringr::str_match(fname, "NUM[-_ ]?0*(\\d{1,3})")[, 2]
  if (!is.na(m)) return(as.integer(m))

  NA_integer_
}


normalize_text <- function(x) {
  # Normalisation robuste pour comparer les libellés issus du PDF :
  # accents, espaces insécables, tirets typographiques, casse.
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_replace_all(x, "[\u2010\u2011\u2012\u2013\u2014\u2015]", "-")
  x <- stringr::str_replace_all(x, "[\u2018\u2019\u0027]", "'")
  x <- stringr::str_squish(x)
  x <- stringr::str_to_lower(x)

  # Translittération sans dépendance supplémentaire.
  y <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  y[is.na(y)] <- x[is.na(y)]

  stringr::str_squish(y)
}

extract_num_before <- function(txt, pattern) {
  if (is.na(txt) || stringr::str_squish(txt) == "") return(NA_real_)
  loc <- stringr::str_locate(txt, stringr::regex(pattern, ignore_case = TRUE))
  if (all(is.na(loc))) return(NA_real_)
  before <- stringr::str_sub(txt, 1, loc[1, 1] - 1)
  nums   <- stringr::str_extract_all(before, "\\d+(?:[\\.,]\\d+)?")[[1]]
  if (length(nums) == 0) return(NA_real_)
  safe_num(tail(nums, 1))
}

extract_num_after <- function(txt, pattern) {
  if (is.na(txt) || stringr::str_squish(txt) == "") return(NA_real_)
  loc <- stringr::str_locate(txt, stringr::regex(pattern, ignore_case = TRUE))
  if (all(is.na(loc))) return(NA_real_)
  after <- stringr::str_sub(txt, loc[1, 2] + 1)
  num   <- stringr::str_extract(after, "\\d+(?:[\\.,]\\d+)?")
  safe_num(num)
}

# ============================================================
# ÉTAPE 1 — SCRAPER LA PAGE INSP ET DÉTECTER LES PDFs
# ============================================================

scrape_insp_sitrep_list <- function(
    category_url = INSP_CATEGORY_PAGE,
    max_pages    = INSP_MAX_PAGES
) {

  cat(">> Scraping category pages (17eme epidemie 2026):", category_url, "\n")

  all_posts <- list()

  # Helper: GET a URL with retries (the INSP server is slow/intermittent)
  get_with_retry <- function(url, max_try = 4) {
    for (att in seq_len(max_try)) {
      resp <- tryCatch(
        httr::GET(
          url, httr::timeout(60),
          httr::add_headers(
            "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
            "Accept"     = "text/html,application/xhtml+xml,*/*",
            "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8"
          )
        ),
        error = function(e) {
          cat("      attempt", att, "error:", conditionMessage(e), "\n")
          NULL
        }
      )
      if (!is.null(resp) && httr::status_code(resp) == 200) return(resp)
      if (att < max_try) {
        cat("      retry", att + 1, "of", max_try, "in 6s...\n")
        Sys.sleep(6)
      }
    }
    NULL
  }

  for (pg in seq_len(max_pages)) {
    page_url <- if (pg == 1) category_url else paste0(category_url, "page/", pg, "/")

    resp <- get_with_retry(page_url)

    if (is.null(resp)) {
      if (pg == 1) {
        cat("   ERROR: cannot reach category page after retries\n")
        cat("   (Le serveur INSP est peut-etre temporairement indisponible.\n")
        cat("    Reessaie dans quelques minutes, ou verifie ta connexion.)\n")
        return(tibble::tibble())
      }
      break  # no more pages
    }

    page_html <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))

    # Post links: href matching /sitrep-nNN-mvb_DD-MM-YYYY/
    links <- page_html %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    texts <- page_html %>% rvest::html_nodes("a") %>% rvest::html_text(trim = TRUE)

    post_df <- tibble::tibble(post_url = links, post_text = texts) %>%
      dplyr::filter(
        !is.na(post_url),
        stringr::str_detect(post_url, "sitrep-n\\d+-mvb")
      ) %>%
      dplyr::distinct(post_url, .keep_all = TRUE)

    if (nrow(post_df) == 0) break

    all_posts[[pg]] <- post_df
    cat("   Page", pg, ":", nrow(post_df), "posts\n")
  }

  posts <- dplyr::bind_rows(all_posts) %>%
    dplyr::distinct(post_url, .keep_all = TRUE)

  if (nrow(posts) == 0) {
    cat("   No 2026 SitRep posts found.\n")
    return(tibble::tibble())
  }

  # Extract sitrep number + date from the post URL
  # e.g. https://insp.cd/sitrep-n27-mvb_10-06-2026/
  posts <- posts %>%
    dplyr::mutate(
      sitrep_no = suppressWarnings(as.integer(
        stringr::str_match(post_url, "sitrep-n(\\d+)-mvb")[, 2]
      )),
      date_raw = stringr::str_match(post_url, "mvb_(\\d{2}-\\d{2}-\\d{4})")[, 2],
      epidemic = "MVB_2026_Ituri"
    ) %>%
    dplyr::filter(!is.na(sitrep_no)) %>%
    # DEDUPLICATE: keep ONE post per sitrep_no (the category page lists
    # each post ~6 times via image/title/read-more links). Keep first.
    dplyr::distinct(sitrep_no, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))

  cat("   Found", nrow(posts), "unique SitRep posts (2026 MVB)\n")
  cat("   SitRep numbers:", paste(sort(posts$sitrep_no), collapse = ", "), "\n")

  # For each UNIQUE post, resolve the embedded PDF URL (one fetch each)
  cat("   Resolving embedded PDF URLs...\n")
  posts$pdf_url <- purrr::map_chr(posts$post_url, resolve_pdf_url)

  posts <- posts %>%
    dplyr::filter(!is.na(pdf_url)) %>%
    dplyr::mutate(
      link_text   = paste0("SitRep N", sitrep_no, " MVB ", date_raw),
      year_in_url = 2026L,
      source_page = post_url,
      scraped_at  = as.character(Sys.time())
    )

  cat("   Resolved", nrow(posts), "PDF URLs\n")
  posts
}

# ------------------------------------------------------------
# Resolve the embedded PDF URL from a SitRep post page
# (decodes the WordPress pdfemb-data base64 blob)
# ------------------------------------------------------------
resolve_pdf_url <- function(post_url) {
  resp <- NULL
  for (att in seq_len(3)) {
    resp <- tryCatch(
      httr::GET(post_url, httr::timeout(60),
                httr::add_headers(
                  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                  "Accept-Language" = "fr-FR,fr;q=0.9")),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr::status_code(resp) == 200) break
    if (att < 3) Sys.sleep(4)
  }
  if (is.null(resp) || httr::status_code(resp) != 200) return(NA_character_)

  html_txt <- httr::content(resp, "text", encoding = "UTF-8")

  # Method 1: direct .pdf link in the page
  direct <- stringr::str_extract(
    html_txt,
    "https://insp\\.cd/wp-content/uploads/[^\"'\\s]+\\.pdf"
  )
  if (!is.na(direct)) return(direct)

  # Method 2: decode pdfemb-data base64 blob
  b64 <- stringr::str_match(html_txt, "pdfemb-data=([A-Za-z0-9+/=]+)")[, 2]
  if (!is.na(b64)) {
    decoded <- tryCatch(
      rawToChar(base64enc::base64decode(b64)),
      error = function(e) NA_character_
    )
    if (!is.na(decoded)) {
      url <- stringr::str_match(decoded, '"url"\\s*:\\s*"([^"]+\\.pdf)"')[, 2]
      if (!is.na(url)) {
        # Unescape JSON: \\/ -> /  and  \\u00b0 -> ° (degree sign, NOT "N")
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
# ÉTAPE 2 — COMPARER AVEC LE REGISTRE EXISTANT
# ============================================================

load_registry <- function() {
  empty <- tibble::tibble(
    sitrep_no    = integer(),
    pdf_url      = character(),
    date_raw     = character(),
    link_text    = character(),
    year_in_url  = integer(),
    downloaded   = logical(),
    extracted    = logical(),
    analysed     = logical(),
    local_pdf    = character(),
    first_seen   = character(),
    last_updated = character()
  )
  if (!file.exists(REGISTRY_FP)) return(empty)
  reg <- readr::read_csv(
    REGISTRY_FP,
    col_types = readr::cols(
      sitrep_no    = readr::col_integer(),
      pdf_url      = readr::col_character(),
      date_raw     = readr::col_character(),
      link_text    = readr::col_character(),
      year_in_url  = readr::col_integer(),
      scraped_at   = readr::col_character(),
      downloaded   = readr::col_logical(),
      extracted    = readr::col_logical(),
      analysed     = readr::col_logical(),
      local_pdf    = readr::col_character(),
      first_seen   = readr::col_character(),
      last_updated = readr::col_character(),
      .default     = readr::col_character()
    ),
    show_col_types = FALSE
  )
  # Ensure all expected columns exist (tolerant of old registry files)
  for (col in names(empty)) {
    if (!col %in% names(reg)) reg[[col]] <- NA
  }
  # Force character on datetime columns in case read_csv parsed them as POSIXct
  reg$first_seen   <- as.character(reg$first_seen)
  reg$last_updated <- as.character(reg$last_updated)
  reg$scraped_at   <- if ("scraped_at" %in% names(reg)) as.character(reg$scraped_at) else NA_character_

  # RE-EXTRACT sitrep_no from pdf_url for ALL rows.
  # Fixes old registry entries that were saved with sitrep_no = NA.
  if ("pdf_url" %in% names(reg) && nrow(reg) > 0) {
    reg$sitrep_no <- purrr::map_int(reg$pdf_url, extract_sitrep_no)
  }
  reg
}

save_registry <- function(registry) {
  readr::write_csv(registry, REGISTRY_FP)
}

detect_new_sitreps <- function(scraped, registry) {

  known_urls <- registry$pdf_url

  # Truly new: in scraped list but not yet in registry at all
  new_sitreps <- scraped %>%
    dplyr::filter(!pdf_url %in% known_urls) %>%
    dplyr::mutate(
      downloaded   = FALSE,
      extracted    = FALSE,
      analysed     = FALSE,
      local_pdf    = NA_character_,
      first_seen   = as.character(Sys.time()),
      last_updated = as.character(Sys.time()),
      scraped_at   = as.character(Sys.time())
    )

  cat("   New SitReps detected:", nrow(new_sitreps), "\n")

  # Pending: in registry AND in current scraped list (= valid 2026 SitReps)
  # but never successfully downloaded. This avoids pulling old 2025 entries.
  scraped_urls <- scraped$pdf_url
  pending <- registry %>%
    dplyr::filter(
      pdf_url %in% scraped_urls,
      is.na(downloaded) | downloaded == FALSE
    ) %>%
    dplyr::mutate(
      first_seen   = as.character(first_seen),
      last_updated = as.character(Sys.time()),
      scraped_at   = as.character(Sys.time())
    )

  if (nrow(pending) > 0) {
    cat("   Pending (not yet downloaded):", nrow(pending), "\n")
  }

  dplyr::bind_rows(new_sitreps, pending) %>%
    dplyr::distinct(pdf_url, .keep_all = TRUE)
}

# ============================================================
# ÉTAPE 3 — TÉLÉCHARGER LES PDFs NOUVEAUX
# ============================================================

download_sitrep_pdf <- function(pdf_url, sitrep_no, pdf_dir = PDF_DIR,
                                 max_retries = 3) {

  # SIMPLE, SAFE local filename (the source filename has special chars
  # like N° that break the filesystem). Use a clean canonical name.
  fname <- paste0("SitRep_", sprintf("%02d", sitrep_no), "_2026.pdf")
  local_path <- file.path(pdf_dir, fname)

  if (file.exists(local_path) && file.info(local_path)$size > 10240) {
    cat("   Already downloaded:", fname, "\n")
    return(local_path)
  }

  # The DOWNLOAD URL must keep the real characters and be properly
  # percent-encoded (° -> %C2%B0). Do NOT alter the path characters.
  # URLencode with reserved=FALSE encodes ° but leaves / : intact.
  dl_url <- utils::URLencode(pdf_url, reserved = FALSE)
  # Guard: if the url already had %C2%B0, URLencode would double-encode
  # the % sign -> fix that back.
  dl_url <- gsub("%25C2%25B0", "%C2%B0", dl_url)
  dl_url <- gsub("%25", "%", dl_url)

  cat("   Downloading SitRep", sitrep_no, "->", fname, "\n")

  for (attempt in seq_len(max_retries)) {

    resp <- tryCatch(
      httr::GET(
        dl_url,
        httr::timeout(180),                    # longer timeout (slow server)
        httr::write_disk(local_path, overwrite = TRUE),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "Referer"    = INSP_CATEGORY_PAGE,
          "Accept"     = "application/pdf,*/*"
        )
      ),
      error = function(e) {
        cat("   Attempt", attempt, "error:", conditionMessage(e), "\n")
        return(NULL)
      }
    )

    if (!is.null(resp)) {
      sc <- httr::status_code(resp)
      ct <- httr::headers(resp)[["content-type"]]
      if (sc == 200) {
        size_kb <- round(file.info(local_path)$size / 1024, 1)
        # Check it's really a PDF, not an HTML error page
        is_pdf <- !is.null(ct) && grepl("pdf", ct, ignore.case = TRUE)
        if (size_kb >= 10 && (is_pdf || size_kb > 50)) {
          cat("   OK:", size_kb, "KB\n")
          return(local_path)
        } else {
          cat("   WARNING: not a valid PDF (size:", size_kb,
              "KB, content-type:", ct %||% "NA", ")\n")
        }
      } else {
        cat("   HTTP", sc, "on attempt", attempt, "\n")
      }
    }

    if (attempt < max_retries) {
      cat("   Retry", attempt + 1, "of", max_retries, "in 8s...\n")
      Sys.sleep(8)
    }
  }

  # FALLBACK: try base R download.file (different backend, sometimes works
  # when httr/curl handshake fails on slow servers)
  cat("   Trying fallback download.file()...\n")
  ok <- tryCatch({
    old_opt <- options(timeout = 300)
    on.exit(options(old_opt), add = TRUE)
    utils::download.file(
      dl_url, local_path, mode = "wb", quiet = TRUE,
      method = "libcurl",
      headers = c(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "Referer"    = INSP_CATEGORY_PAGE
      )
    )
    file.exists(local_path) && file.info(local_path)$size > 10240
  }, error = function(e) {
    cat("   Fallback error:", conditionMessage(e), "\n")
    FALSE
  })

  if (isTRUE(ok)) {
    size_kb <- round(file.info(local_path)$size / 1024, 1)
    cat("   OK (fallback):", size_kb, "KB\n")
    return(local_path)
  }

  cat("   FAILED after", max_retries, "attempts + fallback\n")
  if (file.exists(local_path)) file.remove(local_path)
  NA_character_
}

# ============================================================
# ÉTAPE 4 — EXTRAIRE LE TEXTE DU PDF
# ============================================================

extract_pdf_text <- function(local_pdf, sitrep_no) {

  cat("   Extracting text from PDF", sitrep_no, "\n")

  if (is.na(local_pdf) || !file.exists(local_pdf)) {
    cat("   SKIP: file not found\n")
    return(NULL)
  }

  pages <- tryCatch(
    pdftools::pdf_text(local_pdf),
    error = function(e) {
      cat("   ERROR reading PDF:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(pages) || length(pages) == 0) return(NULL)

  # Build line-level table
  line_table <- purrr::imap_dfr(pages, function(page_text, page_no) {
    lines <- stringr::str_split(page_text, "\n")[[1]]
    tibble::tibble(
      sitrep_no   = sitrep_no,
      page        = as.integer(page_no),
      line_no     = seq_along(lines),
      line_text   = stringr::str_squish(lines)
    )
  }) %>%
    dplyr::filter(
      !is.na(line_text),
      nchar(line_text) > 0
    )

  cat("   Extracted", nrow(line_table), "non-empty lines from",
      length(pages), "pages\n")
  line_table
}

# ============================================================
# ÉTAPE 5A — DICTIONNAIRE DES ZONES DE SANTÉ
# 17ème ÉPIDÉMIE 2026 (Ituri / Nord-Kivu / Sud-Kivu)
# ============================================================
# Source : SitReps INSP N°16/N°17 2026 + bulletins INSP du 08/06/2026.
# Liste officielle des zones de santé touchées (vérité de référence).
# NB: les zones Kasaï 2025 (Bulape, Mweka...) sont VOLONTAIREMENT
# exclues car elles appartiennent à la 16ème épidémie (terminée).

KNOWN_HZ_DICT <- c(
  # ----- ITURI (épicentre, 17 zones de santé touchées) -----
  "Aru", "Aungba", "Bambu", "Bunia", "Damas", "Gety", "Gethy",
  "Kilo", "Komanda", "Lita", "Logo", "Mambasa", "Mangala",
  "Mongbwalu", "Nizi", "Nyankunde", "Rimba", "Rwampara",
  # ----- NORD-KIVU (7 zones de santé) -----
  "Beni", "Butembo", "Goma", "Kalunguta", "Katwa", "Kyondo", "Oicha",
  # ----- SUD-KIVU (1 zone de santé) -----
  "Miti-Murhesa"
)

# Normalise dictionary for matching
KNOWN_HZ_NORM <- normalize_text(stringr::str_to_lower(KNOWN_HZ_DICT))

# ============================================================
# ÉTAPE 5B — EXTRACTION DES ZONES DE SANTÉ D'UN TEXTE
# ============================================================

is_valid_hz <- function(x) {
  x0 <- as.character(x)
  x0 <- stringr::str_squish(x0)
  if (is.na(x0) || x0 == "") return(FALSE)
  # Must start uppercase
  if (!stringr::str_detect(x0, "^[A-Z\u00c0-\u00de]")) return(FALSE)
  # Length 2-30, 1-3 words max
  if (nchar(x0) < 2 || nchar(x0) > 30) return(FALSE)
  n_w <- length(unlist(stringr::str_split(x0, "\\s+")))
  if (n_w > 3) return(FALSE)
  # No digits, no % in name
  if (stringr::str_detect(x0, "[\\d%]")) return(FALSE)
  # Blacklist
  x_l <- normalize_text(stringr::str_to_lower(x0))
  bad <- c(
    "dont", "fig", "total", "tous", "toutes", "au", "aux", "de",
    "du", "et", "la", "le", "les", "ou", "un", "une", "par", "sur",
    "en", "dans", "avec", "pour", "non", "tps", "nd", "nc", "ac",
    "ppl", "appui", "vers", "rapport", "sitrep", "province",
    "tableau", "figure", "zone", "zones", "sante", "aires",
    "indicateurs", "consultations", "transferes", "deploiement",
    "communautaire", "multidisciplinaire", "journee", "ensemble",
    "personnes", "echantillons", "capacite", "gynecolog",
    "nutritionn", "pediatr", "epidemie", "touchee", "demeure"
  )
  if (x_l %in% bad) return(FALSE)
  if (stringr::str_detect(x_l, paste(c(
    "^depuis\\b", "^pour\\b", "^dont\\b", "^afin\\b", "^lors\\b",
    "alerte", "letalite", "deces", "confirme", "suspect",
    "contact", "suivi", "laboratoire", "vaccination", "prise en",
    "surveillance", "lavage", "ménage", "accompagnement",
    "pourcentage", "journee", "\\bct\\b", "\\bcte\\b",
    "\\bhgr\\b", "\\bas\\b"
  ), collapse = "|"))) return(FALSE)
  TRUE
}


extract_hz_from_lines <- function(line_table) {

  if (is.null(line_table) || nrow(line_table) == 0) {
    return(tibble::tibble())
  }

  # Helper pour matcher les zones sans faux positifs.
  hz_match_in_text <- function(txt_low, hz_norm) {
    if (is.na(txt_low) || is.na(hz_norm) || hz_norm == "") return(FALSE)
    if (stringr::str_detect(hz_norm, "[ \\-]")) {
      stringr::str_detect(txt_low, stringr::fixed(hz_norm))
    } else {
      stringr::str_detect(txt_low, stringr::regex(paste0("\\b", hz_norm, "\\b")))
    }
  }

  results <- list()

  for (i in seq_len(nrow(line_table))) {
    txt     <- line_table$line_text[i]
    pg      <- line_table$page[i]
    ln      <- line_table$line_no[i]
    sno     <- line_table$sitrep_no[i]
    txt_low <- normalize_text(txt)

    if (is.na(txt_low) || txt_low == "") next

    matched_idx <- which(vapply(KNOWN_HZ_NORM, function(z) {
      hz_match_in_text(txt_low, z)
    }, logical(1)))

    if (length(matched_idx) == 0) next

    # Contexte : on évite de considérer une simple liste/cartographie comme
    # "zones actives" si la ligne contient beaucoup de zones sans chiffres.
    has_number <- stringr::str_detect(txt, "\\d")
    has_case_context <- stringr::str_detect(
      txt_low,
      paste(c(
        "cas", "deces", "decede", "confirme", "suspect", "contact",
        "alerte", "investigue", "echantillon", "positif", "preleve",
        "zone de sante", "\\bzs\\b", "total", "letalite", "incidence"
      ), collapse = "|")
    )

    too_many_hz_in_line <- length(matched_idx) >= 8

    for (k in matched_idx) {
      hz <- KNOWN_HZ_DICT[k]
      hz_norm <- KNOWN_HZ_NORM[k]

      # Cherche un décompte local : "Bunia (4)" ou "Bunia 4"
      # Utile pour distinguer une vraie ligne/table de distribution.
      hz_pat <- hz_norm
      local_count <- NA_real_

      m1 <- stringr::str_match(
        txt_low,
        stringr::regex(paste0("\\b", hz_pat, "\\b\\s*\\((\\d+)\\)"))
      )
      if (!is.na(m1[1, 2])) local_count <- safe_num(m1[1, 2])

      if (is.na(local_count)) {
        m2 <- stringr::str_match(
          txt_low,
          stringr::regex(paste0("\\b", hz_pat, "\\b\\s+(\\d{1,4})(?:\\s|$)"))
        )
        if (!is.na(m2[1, 2])) local_count <- safe_num(m2[1, 2])
      }

      confidence <- dplyr::case_when(
        !is.na(local_count) ~ "high",
        has_number && has_case_context && !too_many_hz_in_line ~ "medium",
        has_case_context && !too_many_hz_in_line ~ "medium",
        TRUE ~ "low_generic_list"
      )

      # On garde les lignes "low_generic_list" dans le fichier HZ pour audit,
      # mais le rapport opérationnel privilégiera high/medium.
      rule <- dplyr::case_when(
        !is.na(local_count) ~ "known_dictionary_with_local_count",
        confidence == "medium" ~ "known_dictionary_contextual",
        TRUE ~ "known_dictionary_generic_low_confidence"
      )

      results[[length(results) + 1]] <- tibble::tibble(
        sitrep_no     = sno,
        page          = pg,
        line_no       = ln,
        health_zone   = hz,
        hz_count      = local_count,
        rule          = rule,
        confidence    = confidence,
        evidence_line = txt
      )
    }
  }

  if (length(results) == 0) return(tibble::tibble())

  dplyr::bind_rows(results) %>%
    dplyr::mutate(
      confidence_rank = dplyr::case_when(
        confidence == "high" ~ 1L,
        confidence == "medium" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    dplyr::arrange(sitrep_no, health_zone, confidence_rank, page, line_no) %>%
    dplyr::distinct(sitrep_no, health_zone, .keep_all = TRUE) %>%
    dplyr::select(-confidence_rank)
}

# ============================================================
# ÉTAPE 5C — EXTRACTION DES INDICATEURS
# ============================================================

extract_indicators <- function(line_table) {

  if (is.null(line_table) || nrow(line_table) == 0) {
    return(tibble::tibble())
  }

  sno   <- line_table$sitrep_no[1]
  lines <- line_table$line_text
  flat  <- stringr::str_squish(paste(lines, collapse = " "))
  flat_low <- normalize_text(stringr::str_to_lower(flat))

  results <- list()
  seen    <- character(0)
  # add() with PRIORITY: first value added for a code wins.
  add <- function(code, val, rule, domain = "epidemiology", value_source = "observed") {
    if (code %in% seen) return(invisible())
    if (!is.null(val) && length(val) == 1 && !is.na(val) && is.finite(val)) {
      results[[length(results) + 1]] <<- tibble::tibble(
        sitrep_no       = sno,
        indicator_code  = code,
        domain          = domain,
        value           = as.numeric(val),
        extraction_rule = rule,
        value_source    = value_source
      )
      seen <<- c(seen, code)
    }
  }
  g <- function(pattern, src = flat) {
    m <- stringr::str_match(src, stringr::regex(pattern, ignore_case = TRUE))[, 2]
    if (is.na(m)) NA_real_ else safe_num(m)
  }
  g2 <- function(pattern, src = flat) {
    m <- stringr::str_match(src, stringr::regex(pattern, ignore_case = TRUE))
    if (is.na(m[1, 2])) return(c(NA_real_, NA_real_))
    c(safe_num(m[1, 2]), safe_num(m[1, 3]))
  }

  # =========================================================
  # PRIORITY 1 — "Total" row of the spatial distribution table
  #   "Total 321 48 15,0% 23 sur 104 (22,1%) 12"
  # =========================================================
  mt <- stringr::str_match(flat,
    stringr::regex("\\bTotal\\s+(\\d+)\\s+(\\d+)\\s+(\\d+(?:[.,]\\d+)?)\\s*%\\s+(\\d+)\\s+sur",
                   ignore_case = TRUE))
  if (!is.na(mt[1, 2])) {
    add("cumulative_confirmed_cases", safe_num(mt[1, 2]), "table_total_row", "cases")
    add("cumulative_deaths",          safe_num(mt[1, 3]), "table_total_row", "deaths")
    add("case_fatality_ratio",        safe_num(mt[1, 4]), "table_total_row", "deaths")
    add("hz_affected_national",       safe_num(mt[1, 5]), "table_total_row", "geography")
  }

  # Province rows (Ituri / Nord-Kivu / Sud-Kivu)
  mi <- stringr::str_match(flat,
    stringr::regex("\\bIturi\\s+(\\d+)\\s+(\\d+)\\s+(\\d+(?:[.,]\\d+)?)\\s*%", ignore_case = TRUE))
  if (!is.na(mi[1, 2])) {
    add("cases_ituri",  safe_num(mi[1, 2]), "table_ituri", "cases")
    add("deaths_ituri", safe_num(mi[1, 3]), "table_ituri", "deaths")
  }
  mn <- stringr::str_match(flat,
    stringr::regex("Nord[- ]?Kivu\\s+(\\d+)\\s+(\\d+)\\s+(\\d+(?:[.,]\\d+)?)\\s*%", ignore_case = TRUE))
  if (!is.na(mn[1, 2])) {
    add("cases_nordkivu",  safe_num(mn[1, 2]), "table_nk", "cases")
    add("deaths_nordkivu", safe_num(mn[1, 3]), "table_nk", "deaths")
  }
  ms <- stringr::str_match(flat,
    stringr::regex("Sud[- ]?Kivu\\s+(\\d+)\\s+(\\d+)\\s+(\\d+(?:[.,]\\d+)?)\\s*%", ignore_case = TRUE))
  if (!is.na(ms[1, 2])) {
    add("cases_sudkivu",  safe_num(ms[1, 2]), "table_sk", "cases")
    add("deaths_sudkivu", safe_num(ms[1, 3]), "table_sk", "deaths")
  }

  # =========================================================
  # PRIORITY 2 — Structured indicator tables (label value)
  # =========================================================
  add("alerts_reported", g("Alertes\\s+remont\\w+\\s+(\\d+)"), "tbl_alertes", "surveillance")
  ai <- g2("Alertes\\s+investigu\\w+\\s+(\\d+)\\s*\\((\\d+(?:[.,]\\d+)?)")
  add("alerts_investigated",        ai[1], "tbl_invest", "surveillance")
  add("alerts_investigation_rate",  ai[2], "tbl_invest_rate", "surveillance")
  add("alerts_validated", g("Alertes\\s+valid\\w+\\s+(\\d+)"), "tbl_valid", "surveillance")

  # PoE
  add("travellers_total",
      g("Voyageurs\\s+pass\\w+\\s+par\\s+les?\\s+PoE/PoC\\s+([\\d ]{2,8}?)(?:\\s+\\d+[.,]|\\s+Voyageurs|\\s+\\.\\.\\.)"),
      "tbl_poe_total", "poe")
  add("travellers_screened",
      g("Voyageurs\\s+scr?en\\w+\\s+([\\d ]{2,8}?)\\s+\\d+[.,]\\d+\\s*%"),
      "tbl_poe_screened", "poe")

  # Laboratory
  add("samples_collected", g("[E\u00c9]chantillons?\\s+collect\\w+\\s+(\\d+)"), "tbl_lab_coll", "laboratory")
  add("samples_analyzed",  g("[E\u00c9]chantillons?\\s+analys\\w+\\s+(\\d+)"), "tbl_lab_anal", "laboratory")
  lp <- g2("[E\u00c9]chantillons?\\s+positifs?\\s+(\\d+)\\s+Taux\\s+de\\s+positivit\\w+\\s+(\\d+(?:[.,]\\d+)?)")
  add("samples_positive",    lp[1], "tbl_lab_pos", "laboratory")
  add("lab_positivity_rate", lp[2], "tbl_lab_posrate", "laboratory")

  # Isolation / recovered
  add("patients_in_isolation", g("Patients?\\s+en\\s+isolement\\s+(\\d+)"), "tbl_isolation", "care")
  add("recovered_today",       g("Gu[e\u00e9]ris\\s+du\\s+jour\\s+(\\d+)"), "tbl_recovered_today", "care")

  # =========================================================
  # PRIORITY 3 — Narrative fallbacks (older SitReps like N°16)
  #   + early SitReps 18-21 (different layouts)
  # =========================================================
  add("new_confirmed_cases", g("(\\d+)\\s+nouveaux?\\s+cas\\s+confirm"), "narr_nouveaux", "cases")

  # Cumulative cases — multiple narrative variants (try in order):
  add("cumulative_confirmed_cases",
      g("cumul\\s+des\\s+cas\\s+confirm\\w*\\s+s.?[e\u00e9]l[e\u00e8]ve\\s+[a\u00e0]\\s+(\\d+)"),
      "narr_cumul_eleve", "cases")
  add("cumulative_confirmed_cases",
      g("Cumul\\s+cas\\s+confirm\\w*\\s*:\\s*(\\d+)"),
      "narr_cumul_colon", "cases")
  add("cumulative_confirmed_cases",
      g("cumul\\s+de\\s+(\\d+)\\s+cas\\s+confirm"),
      "narr_cumul_de", "cases")
  add("cumulative_confirmed_cases",
      g("total\\s+de\\s+(\\d+)\\s+cas\\s+ont\\s+[e\u00e9]t[e\u00e9]\\s+notifi"),
      "narr_total_notifies", "cases")

  # New confirmed cases — early variant "16 Nouveaux cas confirmés en date"
  add("new_confirmed_cases",
      g("(\\d+)\\s+Nouveaux?\\s+cas\\s+confirm\\w+\\s+en\\s+date"),
      "narr_nouveaux_date", "cases")

  # Horizontal summary box (number BEFORE label):
  #   "51 Cas confirmés 4 Décès confirmés ... 847 Contacts listés"
  add("cumulative_confirmed_cases",
      g("(\\d+)\\s+Cas\\s+confirm[e\u00e9]s\\b"), "box_h_cases", "cases")
  add("cumulative_deaths",
      g("(\\d+)\\s+D[e\u00e9]c[e\u00e8]s\\s+confirm[e\u00e9]s\\b"), "box_h_deaths", "deaths")
  add("contacts_listed",
      g("(\\d+)\\s+Contacts?\\s+list[e\u00e9]s"), "box_h_contacts", "contacts")

  # Lab — early "Echantillons reçus : 46", "Echantillons analysés du jour : 31"
  add("samples_received",
      g("[E\u00c9]chantillons?\\s+re[c\u00e7]us\\s*:?\\s*(\\d+)"), "lab_recus", "laboratory")
  add("samples_analyzed",
      g("[E\u00c9]chantillons?\\s+analys\\w+\\s+du\\s+jour\\s*:?\\s*(\\d+)"),
      "lab_analyses_jour", "laboratory")
  add("cases_ituri",    g("dont\\s+(\\d+)\\s+en\\s+Ituri"), "narr_ituri", "cases")
  add("cases_nordkivu", g("(\\d+)\\s+au\\s+Nord[- ]?Kivu"), "narr_nk", "cases")
  add("cases_sudkivu",  g("(\\d+)\\s+au\\s+Sud[- ]?Kivu"),  "narr_sk", "cases")
  add("hz_affected_national", g("(\\d+)\\s+[a\u00e0]\\s+l.?[e\u00e9]chelle\\s+nationale"), "narr_zs_nat", "geography")
  add("hz_affected_ituri",
      g("zones?\\s+de\\s+sant[e\u00e9]\\s+touch\\w*\\s+demeure\\s+[a\u00e0]\\s+(\\d+)\\s+en\\s+Ituri"),
      "narr_zs_ituri", "geography")

  # Narrative alerts (N°16 style)
  add("alerts_reported", g("(\\d+)\\s+alertes?\\s+ont\\s+[e\u00e9]t[e\u00e9]\\s+remont"), "narr_alertes", "surveillance")
  add("alerts_investigated", g("dont\\s+(\\d+)\\s*\\([\\d,.]+\\s*%?\\)\\s*investigu"), "narr_invest", "surveillance")
  add("samples_collected",
      g("(\\d+)\\s+nouveaux\\s+[e\u00e9]chantillons\\s+ont\\s+[e\u00e9]t[e\u00e9]\\s+collect"),
      "narr_echant", "laboratory")
  add("samples_positive", g("(\\d+)\\s+sont\\s+revenus?\\s+positifs?"), "narr_pos", "laboratory")

  # =========================================================
  # PRIORITY 4 — Summary box value row + adjacency
  # =========================================================
  # Value row appears as either "321 48 * 116" OR "42 282* 220" (order varies);
  # we ONLY use it to fill codes still missing, and we sanity-check
  # cases >= deaths to avoid inversion.
  box_idx <- which(stringr::str_detect(
    lines, "^\\s*\\d{1,4}\\s+\\d{1,4}\\s*\\*?\\s*\\d{0,4}\\s*$"
  ))
  for (bi in box_idx) {
    parts <- stringr::str_split(stringr::str_squish(lines[bi]), "\\s+")[[1]]
    nums  <- suppressWarnings(as.numeric(gsub("\\*", "", parts)))
    nums  <- nums[!is.na(nums)]
    if (length(nums) >= 2) {
      a <- nums[1]; b <- nums[2]
      cc <- max(a, b); dd <- min(a, b)   # cases >= deaths
      add("cumulative_confirmed_cases", cc, "box_value_row", "cases")
      add("cumulative_deaths",          dd, "box_value_row", "deaths")
      if (length(nums) >= 3) add("recovered", nums[length(nums)], "box_value_row", "cases")
    }
  }

  # Standalone numbers near labels (box singletons)
  for (i in seq_along(lines)) {
    if (stringr::str_detect(lines[i], "^\\s*\\d{1,4}\\s*$")) {
      v   <- safe_num(lines[i])
      ctx <- normalize_text(stringr::str_to_lower(
        paste(lines[max(1, i - 3):min(length(lines), i + 3)], collapse = " ")))
      if (stringr::str_detect(ctx, "suspects.{0,20}investigation"))
        add("suspected_cases_investigation", v, "box_adjacent", "cases")
      if (stringr::str_detect(ctx, "suspects en.{0,5}isolement"))
        add("suspected_cases_isolation", v, "box_adjacent", "cases")
      if (stringr::str_detect(ctx, "confirm[e\u00e9]s actifs"))
        add("active_confirmed_cases", v, "box_adjacent", "cases")
    }
  }

  # Contacts follow-up rate: "43% ... Taux de suivi de contacts"
  fr <- g("(\\d+(?:[.,]\\d+)?)\\s*%\\s*Taux\\s+de\\s+suivi\\s+de\\s*contacts")
  if (is.na(fr)) fr <- g("Taux\\s+de\\s+suivi\\s+de\\s*contacts\\s*(\\d+(?:[.,]\\d+)?)\\s*%")
  if (is.na(fr)) fr <- g("taux\\s+global\\s+(\\d+(?:[.,]\\d+)?)\\s*%")
  add("contacts_followup_rate", fr, "taux_suivi", "contacts")

  # =========================================================
  # PRIORITY 5 — Derived CFR (only if not already from table)
  # =========================================================
  vals <- if (length(results) > 0) dplyr::bind_rows(results) else tibble::tibble()
  getv <- function(code) {
    if (nrow(vals) == 0) return(NA_real_)
    v <- vals$value[vals$indicator_code == code]
    if (length(v) == 0) NA_real_ else v[1]
  }
  ## ---- PATCH_00B_APPLIED ---- regles ajoutees (cloud) ----
  ## vaccination, contacts suivis, zones par province
  ## Utilise add() et g() locaux, et 'flat' (texte du SitRep).
  add('doses_vaccine_administered', g('doses?\\s+(?:de\\s+vaccin\\s+)?administr\\w+\\s+(\\d{1,7})'), 'regex_vaccine_doses', 'vaccination')
  add('doses_vaccine_administered', g('(\\d{1,7})\\s+personnes?\\s+(?:ont\\s+[eé]t[eé]\\s+)?vaccin'), 'regex_persons_vaccinated', 'vaccination')
  add('hcw_vaccinated', g('(\\d{1,6})\\s+agents?\\s+de\\s+sant[eé]\\s+(?:ont\\s+[eé]t[eé]\\s+)?vaccin'), 'regex_hcw_vaccinated', 'vaccination')
  add('ring_vaccination_n', g('vaccination\\s+en\\s+anneau\\s*:?\\s*(\\d{1,7})'), 'regex_ring_vaccination', 'vaccination')
  add('contacts_followed_up', g('(\\d{1,7})\\s+contacts?\\s+(?:sous\\s+|en\\s+)?suivi'), 'regex_contacts_followup', 'contacts')
  add('contacts_followed_up', g('contacts?\\s+(?:sous\\s+|en\\s+)?suivi\\s*:?\\s*(\\d{1,7})'), 'regex_contacts_followup2', 'contacts')
  add('deaths_community', g('(\\d{1,5})\\s+d[eé]c[eè]s\\s+(?:en\\s+|dans\\s+la\\s+)?communaut'), 'regex_deaths_community', 'deaths')
  add('hz_affected_ituri',    g('Ituri\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}'), 'regex_hz_ituri', 'geography')
  add('hz_affected_nordkivu', g('Nord[- ]?Kivu\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}'), 'regex_hz_nordkivu', 'geography')
  add('hz_affected_sudkivu',  g('Sud[- ]?Kivu\\s*\\(?\\s*(\\d{1,2})\\s*/\\s*\\d{1,3}'), 'regex_hz_sudkivu', 'geography')
  ## ---- FIN PATCH_00B ----

  if (!("case_fatality_ratio" %in% seen)) {
    cc <- getv("cumulative_confirmed_cases"); cd <- getv("cumulative_deaths")
    if (!is.na(cc) && !is.na(cd) && cc > 0 && cd <= cc) {
      add("case_fatality_ratio", round(100 * cd / cc, 1), "computed_cfr", "deaths", value_source = "derived")
    }
  }

  if (length(results) == 0) return(tibble::tibble())
  dplyr::bind_rows(results) %>%
    dplyr::filter(!is.na(value), is.finite(value)) %>%
    dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE)
}



# ============================================================
# ÉTAPE 5D — STANDARDISATION, INDICATEURS DÉRIVÉS ET QC
# ============================================================

standardize_indicator_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(
      sitrep_no = integer(),
      indicator_code = character(),
      domain = character(),
      value = numeric(),
      extraction_rule = character(),
      value_source = character()
    ))
  }

  if (!"sitrep_no" %in% names(df)) df$sitrep_no <- NA_integer_
  if (!"indicator_code" %in% names(df)) df$indicator_code <- NA_character_
  if (!"domain" %in% names(df)) df$domain <- "unknown"
  if (!"value" %in% names(df)) df$value <- NA_real_
  if (!"extraction_rule" %in% names(df)) df$extraction_rule <- "unknown"
  if (!"value_source" %in% names(df)) {
    df$value_source <- ifelse(
      stringr::str_detect(df$extraction_rule %||% "", "derived|computed"),
      "derived", "observed"
    )
  }

  df %>%
    dplyr::mutate(
      sitrep_no = suppressWarnings(as.integer(sitrep_no)),
      value = suppressWarnings(as.numeric(value)),
      domain = as.character(domain),
      indicator_code = as.character(indicator_code),
      extraction_rule = as.character(extraction_rule),
      value_source = dplyr::case_when(
        is.na(value_source) & stringr::str_detect(extraction_rule, "derived|computed") ~ "derived",
        is.na(value_source) ~ "observed",
        TRUE ~ as.character(value_source)
      )
    )
}

derive_missing_indicators <- function(indicators_long) {
  df <- standardize_indicator_cols(indicators_long)
  if (nrow(df) == 0) return(df)

  # Observé prioritaire sur dérivé.
  df <- df %>%
    dplyr::arrange(
      sitrep_no,
      indicator_code,
      dplyr::case_when(value_source == "observed" ~ 1L, TRUE ~ 2L)
    ) %>%
    dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE)

  get_value <- function(sno, code) {
    v <- df %>%
      dplyr::filter(sitrep_no == .env$sno, indicator_code == .env$code) %>%
      dplyr::slice(1) %>%
      dplyr::pull(value)
    if (length(v) == 0) NA_real_ else v[1]
  }

  has_code <- function(sno, code) {
    any(df$sitrep_no == sno & df$indicator_code == code)
  }

  make_row <- function(sno, code, domain, val, rule) {
    tibble::tibble(
      sitrep_no       = as.integer(sno),
      indicator_code  = code,
      domain          = domain,
      value           = as.numeric(val),
      extraction_rule = rule,
      value_source    = "derived"
    )
  }

  derived <- list()
  snos <- sort(unique(df$sitrep_no[!is.na(df$sitrep_no)]))

  for (s in snos) {
    prev_candidates <- snos[snos < s]
    prev_s <- if (length(prev_candidates) == 0) NA_integer_ else max(prev_candidates)

    # Nouveaux cas confirmés = différence des cumuls si absent.
    if (!has_code(s, "new_confirmed_cases") && !is.na(prev_s)) {
      cur <- get_value(s, "cumulative_confirmed_cases")
      prv <- get_value(prev_s, "cumulative_confirmed_cases")
      d <- cur - prv
      if (!is.na(d) && is.finite(d) && d >= 0) {
        derived[[length(derived) + 1]] <- make_row(
          s, "new_confirmed_cases", "cases", d,
          paste0("derived_from_cumulative_difference_vs_sitrep_", prev_s)
        )
      }
    }

    # Nouveaux décès = différence des cumuls si absent.
    if (!has_code(s, "new_deaths") && !is.na(prev_s)) {
      cur <- get_value(s, "cumulative_deaths")
      prv <- get_value(prev_s, "cumulative_deaths")
      d <- cur - prv
      if (!is.na(d) && is.finite(d) && d >= 0) {
        derived[[length(derived) + 1]] <- make_row(
          s, "new_deaths", "deaths", d,
          paste0("derived_from_cumulative_difference_vs_sitrep_", prev_s)
        )
      }
    }

    # CFR = décès cumulés / cas confirmés cumulés si absent.
    if (!has_code(s, "case_fatality_ratio")) {
      cc <- get_value(s, "cumulative_confirmed_cases")
      cd <- get_value(s, "cumulative_deaths")
      if (!is.na(cc) && !is.na(cd) && cc > 0 && cd <= cc) {
        derived[[length(derived) + 1]] <- make_row(
          s, "case_fatality_ratio", "deaths", round(100 * cd / cc, 1),
          "derived_from_cumulative_deaths_over_cases"
        )
      }
    }
  }

  out <- dplyr::bind_rows(df, dplyr::bind_rows(derived)) %>%
    dplyr::arrange(
      sitrep_no,
      indicator_code,
      dplyr::case_when(value_source == "observed" ~ 1L, TRUE ~ 2L)
    ) %>%
    dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE)

  out
}

qc_preis_outputs <- function(indicators_long, hz_df = tibble::tibble(), registry = NULL) {
  df <- derive_missing_indicators(indicators_long)

  if (nrow(df) == 0) {
    return(list(
      qc_by_sitrep = tibble::tibble(),
      qc_issues = tibble::tibble(
        sitrep_no = NA_integer_,
        severity = "CRITICAL",
        issue = "no_indicators_extracted",
        detail = "Aucun indicateur extrait."
      ),
      blocking = TRUE
    ))
  }

  get_code_table <- function(code) {
    df %>%
      dplyr::filter(indicator_code == .env$code) %>%
      dplyr::select(sitrep_no, value)
  }

  cases_tbl  <- get_code_table("cumulative_confirmed_cases")
  deaths_tbl <- get_code_table("cumulative_deaths")
  cfr_tbl    <- get_code_table("case_fatality_ratio")

  qc_by_sitrep <- df %>%
    dplyr::group_by(sitrep_no) %>%
    dplyr::summarise(
      n_indicators = dplyr::n_distinct(indicator_code),
      has_cumulative_cases = any(indicator_code == "cumulative_confirmed_cases"),
      has_cumulative_deaths = any(indicator_code == "cumulative_deaths"),
      has_new_cases = any(indicator_code == "new_confirmed_cases"),
      has_new_deaths = any(indicator_code == "new_deaths"),
      has_cfr = any(indicator_code == "case_fatality_ratio"),
      has_contact_followup = any(indicator_code == "contacts_followup_rate"),
      has_alert_investigation = any(indicator_code == "alerts_investigation_rate"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      qc_level = dplyr::case_when(
        !has_cumulative_cases | !has_cumulative_deaths ~ "CRITICAL",
        !has_new_cases | !has_new_deaths | !has_cfr ~ "WARNING",
        TRUE ~ "OK"
      )
    )

  issues <- list()

  # Missing key indicators
  missing_key <- qc_by_sitrep %>%
    dplyr::filter(qc_level != "OK") %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("has_"),
      names_to = "check",
      values_to = "present"
    ) %>%
    dplyr::filter(!present) %>%
    dplyr::mutate(
      severity = dplyr::case_when(
        check %in% c("has_cumulative_cases", "has_cumulative_deaths") ~ "CRITICAL",
        TRUE ~ "WARNING"
      ),
      issue = stringr::str_replace(check, "^has_", "missing_"),
      detail = paste0("Indicateur absent ou non extrait : ", issue)
    ) %>%
    dplyr::select(sitrep_no, severity, issue, detail)

  if (nrow(missing_key) > 0) issues[[length(issues) + 1]] <- missing_key

  # Cumulative monotonic checks
  for (code in c("cumulative_confirmed_cases", "cumulative_deaths")) {
    tmp <- df %>%
      dplyr::filter(indicator_code == .env$code) %>%
      dplyr::arrange(sitrep_no) %>%
      dplyr::mutate(prev_value = dplyr::lag(value), diff_value = value - prev_value) %>%
      dplyr::filter(!is.na(diff_value), diff_value < 0)

    if (nrow(tmp) > 0) {
      issues[[length(issues) + 1]] <- tmp %>%
        dplyr::transmute(
          sitrep_no,
          severity = "CRITICAL",
          issue = paste0(code, "_decreased"),
          detail = paste0(code, " baisse de ", prev_value, " à ", value,
                          " — vérifier extraction ou SitRep.")
        )
    }
  }

  # CFR consistency check
  cfr_check <- cases_tbl %>%
    dplyr::rename(cases = value) %>%
    dplyr::left_join(deaths_tbl %>% dplyr::rename(deaths = value), by = "sitrep_no") %>%
    dplyr::left_join(cfr_tbl %>% dplyr::rename(cfr = value), by = "sitrep_no") %>%
    dplyr::mutate(
      cfr_recomputed = dplyr::if_else(!is.na(cases) & cases > 0, round(100 * deaths / cases, 1), NA_real_),
      cfr_gap = abs(cfr - cfr_recomputed)
    ) %>%
    dplyr::filter(!is.na(cfr_gap), cfr_gap > 1.0)

  if (nrow(cfr_check) > 0) {
    issues[[length(issues) + 1]] <- cfr_check %>%
      dplyr::transmute(
        sitrep_no,
        severity = "WARNING",
        issue = "cfr_inconsistent",
        detail = paste0("CFR extrait=", cfr, "% ; CFR recalculé=", cfr_recomputed, "%.")
      )
  }

  # Health zone confidence check
  if (!is.null(hz_df) && nrow(hz_df) > 0 && "confidence" %in% names(hz_df)) {
    hz_qc <- hz_df %>%
      dplyr::group_by(sitrep_no) %>%
      dplyr::summarise(
        n_hz_total = dplyr::n_distinct(health_zone),
        n_hz_high_medium = dplyr::n_distinct(health_zone[confidence %in% c("high", "medium")]),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_hz_total >= 20, n_hz_high_medium == 0)

    if (nrow(hz_qc) > 0) {
      issues[[length(issues) + 1]] <- hz_qc %>%
        dplyr::transmute(
          sitrep_no,
          severity = "WARNING",
          issue = "health_zones_low_confidence_only",
          detail = paste0("Zones détectées surtout via listes génériques (n=", n_hz_total,
                          "). Ne pas interpréter comme zones prioritaires sans validation.")
        )
    }
  }

  qc_issues <- dplyr::bind_rows(issues)
  if (nrow(qc_issues) == 0) {
    qc_issues <- tibble::tibble(
      sitrep_no = integer(),
      severity = character(),
      issue = character(),
      detail = character()
    )
  }

  list(
    qc_by_sitrep = qc_by_sitrep,
    qc_issues = qc_issues,
    blocking = any(qc_issues$severity == "CRITICAL", na.rm = TRUE)
  )
}

# ============================================================
# ÉTAPE 6 — ANALYSE OPÉRATIONNELLE
# ============================================================


analyse_sitrep <- function(indicators_long, hz_mentions, sitrep_no) {

  cat("   Analysing SitRep", sitrep_no, "\n")

  indicators_long <- derive_missing_indicators(indicators_long)

  use_filter <- !is.na(sitrep_no)

  get_row <- function(code) {
    df <- indicators_long
    if (use_filter && "sitrep_no" %in% names(df)) {
      df <- df %>% dplyr::filter(sitrep_no == .env$sitrep_no)
    }
    out <- df %>%
      dplyr::filter(indicator_code == .env$code) %>%
      dplyr::arrange(dplyr::case_when(value_source == "observed" ~ 1L, TRUE ~ 2L)) %>%
      dplyr::slice(1)
    if (nrow(out) == 0) tibble::tibble() else out
  }

  get_val <- function(code) {
    r <- get_row(code)
    if (nrow(r) == 0) NA_real_ else r$value[1]
  }

  get_src <- function(code) {
    r <- get_row(code)
    if (nrow(r) == 0) "missing" else paste0(r$value_source[1], " / ", r$extraction_rule[1])
  }

  cumul_cases   <- get_val("cumulative_confirmed_cases")
  new_cases     <- get_val("new_confirmed_cases")
  cumul_deaths  <- get_val("cumulative_deaths")
  new_deaths    <- get_val("new_deaths")
  cfr           <- get_val("case_fatality_ratio")
  followup_rate <- get_val("contacts_followup_rate")
  alerts_rep    <- get_val("alerts_reported")
  alerts_inv    <- get_val("alerts_investigated")
  inv_rate      <- get_val("alerts_investigation_rate")
  samples       <- get_val("samples_collected")
  positifs      <- get_val("samples_positive")
  positivity    <- get_val("lab_positivity_rate")
  recovered     <- get_val("recovered")
  active_cases  <- get_val("active_confirmed_cases")

  hz_list <- hz_mentions
  if (!is.null(hz_list) && nrow(hz_list) > 0 && use_filter && "sitrep_no" %in% names(hz_list)) {
    hz_list <- hz_list %>% dplyr::filter(sitrep_no == .env$sitrep_no)
  }

  # Pour le rapport, éviter de présenter des listes génériques comme zones prioritaires.
  hz_for_report <- if (!is.null(hz_list) && nrow(hz_list) > 0) {
    if ("confidence" %in% names(hz_list)) {
      hz_list %>%
        dplyr::filter(confidence %in% c("high", "medium")) %>%
        dplyr::distinct(health_zone) %>%
        dplyr::pull(health_zone)
    } else {
      hz_list %>%
        dplyr::distinct(health_zone) %>%
        dplyr::pull(health_zone)
    }
  } else character()

  hz_all_detected <- if (!is.null(hz_list) && nrow(hz_list) > 0) {
    hz_list %>% dplyr::distinct(health_zone) %>% dplyr::pull(health_zone)
  } else character()

  # Signal classification — simple mais opérationnelle.
  signals <- tibble::tibble(
    indicator_code = c(
      "cumulative_confirmed_cases", "new_confirmed_cases",
      "cumulative_deaths", "new_deaths", "case_fatality_ratio",
      "contacts_followup_rate", "alerts_investigation_rate",
      "lab_positivity_rate"
    ),
    value = c(
      cumul_cases, new_cases, cumul_deaths, new_deaths,
      cfr, followup_rate, inv_rate, positivity
    ),
    value_source = c(
      get_src("cumulative_confirmed_cases"),
      get_src("new_confirmed_cases"),
      get_src("cumulative_deaths"),
      get_src("new_deaths"),
      get_src("case_fatality_ratio"),
      get_src("contacts_followup_rate"),
      get_src("alerts_investigation_rate"),
      get_src("lab_positivity_rate")
    )
  ) %>%
    dplyr::mutate(
      signal_level = dplyr::case_when(
        indicator_code == "new_deaths" & !is.na(value) & value >= 5 ~ "RED",
        indicator_code == "new_deaths" & !is.na(value) & value > 0 ~ "ORANGE",
        indicator_code == "case_fatality_ratio" & !is.na(value) & value >= 15 ~ "RED",
        indicator_code == "contacts_followup_rate" & !is.na(value) & value < 80 ~ "RED",
        indicator_code == "contacts_followup_rate" & !is.na(value) & value < 90 ~ "ORANGE",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value < 80 ~ "RED",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value < 90 ~ "ORANGE",
        indicator_code == "lab_positivity_rate" & !is.na(value) & value >= 10 ~ "ORANGE",
        indicator_code == "new_confirmed_cases" & !is.na(value) & value >= 30 ~ "ORANGE",
        indicator_code == "new_confirmed_cases" & !is.na(value) & value > 0 ~ "MONITOR",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value >= 90 ~ "GREEN",
        TRUE ~ "MONITOR"
      ),
      probable_driver = dplyr::case_when(
        indicator_code == "new_deaths" & !is.na(value) & value > 0 ~
          "Présentation tardive, référence tardive, décès communautaires, gaps de prise en charge.",
        indicator_code == "case_fatality_ratio" & !is.na(value) & value >= 15 ~
          "Sous-détection des cas bénins, sévérité clinique, retard de soins ou accès tardif au CTE.",
        indicator_code == "new_confirmed_cases" & !is.na(value) & value > 0 ~
          "Chaînes de transmission actives, isolement incomplet, identification incomplète des contacts ou amélioration de la détection.",
        indicator_code == "contacts_followup_rate" & !is.na(value) & value < 90 ~
          "Capacité insuffisante, mobilité des contacts, réticence communautaire ou difficulté d'accès.",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value < 90 ~
          "Délai d'investigation, contraintes de transport, charge de travail ou remontée tardive des alertes.",
        indicator_code == "lab_positivity_rate" & !is.na(value) & value >= 10 ~
          "Transmission active probable, ciblage des prélèvements ou retard dans la rupture des chaînes.",
        TRUE ~ "Monitoring continu requis."
      )
    ) %>%
    dplyr::filter(!is.na(value))

  list(
    sitrep_no       = sitrep_no,
    cumul_cases     = cumul_cases,
    new_cases       = new_cases,
    cumul_deaths    = cumul_deaths,
    new_deaths      = new_deaths,
    cfr             = cfr,
    followup_rate   = followup_rate,
    inv_rate        = inv_rate,
    alerts_reported = alerts_rep,
    alerts_investigated = alerts_inv,
    samples_collected = samples,
    samples_positive = positifs,
    positivity_rate = positivity,
    recovered       = recovered,
    active_cases    = active_cases,
    hz_list         = hz_for_report,
    hz_all_detected = hz_all_detected,
    signals         = signals,
    sources         = list(
      cumulative_confirmed_cases = get_src("cumulative_confirmed_cases"),
      new_confirmed_cases        = get_src("new_confirmed_cases"),
      cumulative_deaths          = get_src("cumulative_deaths"),
      new_deaths                 = get_src("new_deaths"),
      case_fatality_ratio        = get_src("case_fatality_ratio"),
      contacts_followup_rate     = get_src("contacts_followup_rate"),
      alerts_investigation_rate  = get_src("alerts_investigation_rate")
    )
  )
}

# ============================================================
# ÉTAPE 7 — RAPPORT OPÉRATIONNEL (texte court)
# ============================================================


generate_report <- function(analysis, qc_result = NULL) {

  fmt <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("non disponible")
    format(round(x, 1), big.mark = ",", trim = TRUE, scientific = FALSE)
  }

  pct <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("non disponible")
    paste0(round(x, 1), "%")
  }

  n_red    <- sum(analysis$signals$signal_level == "RED",    na.rm = TRUE)
  n_orange <- sum(analysis$signals$signal_level == "ORANGE", na.rm = TRUE)
  n_green  <- sum(analysis$signals$signal_level == "GREEN",  na.rm = TRUE)

  hz_str <- if (length(analysis$hz_list) > 0) {
    paste(analysis$hz_list, collapse = ", ")
  } else if (length(analysis$hz_all_detected) > 0) {
    paste0(
      "non confirmées par contexte fort — mentions génériques détectées: ",
      paste(analysis$hz_all_detected, collapse = ", ")
    )
  } else {
    "non identifiées"
  }

  sig_df <- analysis$signals %>%
    dplyr::filter(signal_level != "MONITOR")

  sig_txt <- if (nrow(sig_df) == 0) {
    "Aucun signal critique ou important détecté avec les indicateurs disponibles."
  } else {
    paste0(
      "[", sig_df$signal_level, "] ",
      sig_df$indicator_code, " = ", round(sig_df$value, 1),
      " | Source: ", sig_df$value_source,
      "\n   Driver probable: ", sig_df$probable_driver,
      collapse = "\n"
    )
  }

  qc_txt <- ""
  if (!is.null(qc_result) && "qc_issues" %in% names(qc_result)) {
    qci <- qc_result$qc_issues
    qci <- qci %>% dplyr::filter(is.na(sitrep_no) | sitrep_no == analysis$sitrep_no)
    if (nrow(qci) > 0) {
      qc_txt <- paste0(
        "\nCONTRÔLE QUALITÉ\n",
        paste0("[", qci$severity, "] ", qci$issue, " — ", qci$detail, collapse = "\n"),
        "\n"
      )
    } else {
      qc_txt <- "\nCONTRÔLE QUALITÉ\n[OK] Aucun blocage critique détecté pour ce SitRep.\n"
    }
  }

  glue::glue(
    "RAPPORT OPÉRATIONNEL AUTOMATISÉ — PREIS EBOLA RDC\n",
    "SitRep N°{analysis$sitrep_no} | Généré le {Sys.Date()}\n",
    "============================================================\n\n",
    "RÉSUMÉ EXÉCUTIF\n",
    "Signaux critiques (ROUGE): {n_red} | ",
    "Signaux importants (ORANGE): {n_orange} | ",
    "Positifs (VERT): {n_green}\n\n",
    "DONNÉES CLÉS\n",
    "Cas confirmés cumulés      : {fmt(analysis$cumul_cases)}\n",
    "Nouveaux cas               : {fmt(analysis$new_cases)}\n",
    "Décès cumulés              : {fmt(analysis$cumul_deaths)}\n",
    "Nouveaux décès             : {fmt(analysis$new_deaths)}\n",
    "Létalité (CFR)             : {pct(analysis$cfr)}\n",
    "Taux de suivi contacts     : {pct(analysis$followup_rate)}\n",
    "Taux d'investigation alertes: {pct(analysis$inv_rate)}\n",
    "Positivité laboratoire     : {pct(analysis$positivity_rate)}\n\n",
    "SOURCES DES VALEURS CLÉS\n",
    "Cas cumulés                : {analysis$sources$cumulative_confirmed_cases}\n",
    "Nouveaux cas               : {analysis$sources$new_confirmed_cases}\n",
    "Décès cumulés              : {analysis$sources$cumulative_deaths}\n",
    "Nouveaux décès             : {analysis$sources$new_deaths}\n",
    "CFR                         : {analysis$sources$case_fatality_ratio}\n\n",
    "ZONES DE SANTÉ DÉTECTÉES\n",
    "{hz_str}\n",
    "{qc_txt}\n",
    "SIGNAUX OPÉRATIONNELS\n",
    "{sig_txt}\n\n",
    "NOTE : Drivers probables uniquement — pas de causalité établie.\n",
    "Valider avec ligne-liste officielle, registre contacts, laboratoire et données CTE.\n"
  )
}

# ============================================================
# PIPELINE PRINCIPAL
# ============================================================


run_preis_pipeline <- function(
    force_redownload = FALSE,
    max_new          = Inf       # Inf = traiter tous les nouveaux/pending
) {

  cat("\n--- ÉTAPE 1: Scraping INSP ---\n")
  scraped <- scrape_insp_sitrep_list()

  if (nrow(scraped) == 0) {
    cat("Impossible de scraper la page. Pipeline arrêté.\n")
    return(invisible(NULL))
  }

  registry <- load_registry()

  # Purge invalid entries: drop rows with sitrep_no NA.
  n_before <- nrow(registry)
  registry <- registry %>%
    dplyr::filter(!is.na(sitrep_no), sitrep_no >= 1)
  n_purged <- n_before - nrow(registry)
  if (n_purged > 0) {
    cat("   Purged", n_purged, "invalid (NA) entries from registry.\n")
    save_registry(registry)
  }

  if (nrow(registry) == 0) {
    cat("   Registry vide — tous les SitReps seront traités.\n")
  }

  # Update registry with all known SitReps.
  new_rows <- detect_new_sitreps(scraped, registry)

  force_chr_cols <- function(df) {
    chr_cols <- c("first_seen", "last_updated", "scraped_at", "date_raw")
    for (col in chr_cols) {
      if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
    }
    df
  }

  registry <- dplyr::bind_rows(
    force_chr_cols(registry),
    force_chr_cols(new_rows)
  ) %>%
    dplyr::distinct(pdf_url, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))

  save_registry(registry)

  # Select SitReps to process:
  # - force_redownload = TRUE : retraiter les SitReps scrapés.
  # - sinon : seulement ceux non extraits / non analysés.
  to_process <- registry %>%
    dplyr::filter(pdf_url %in% scraped$pdf_url) %>%
    dplyr::filter(
      isTRUE(force_redownload) |
        is.na(extracted) | !extracted |
        is.na(analysed) | !analysed
    ) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))

  if (is.finite(max_new)) {
    to_process <- to_process %>% dplyr::slice_head(n = max_new)
  }

  if (nrow(to_process) == 0) {
    cat("Aucun nouveau SitRep à traiter. Tout est à jour.\n")
    return(invisible(registry))
  }

  cat("\n--- ÉTAPE 2: Téléchargement & Extraction ---\n")

  all_lines      <- list()
  all_indicators <- list()
  all_hz         <- list()
  processed_snos <- integer(0)

  for (i in seq_len(nrow(to_process))) {
    row  <- to_process[i, ]
    sno  <- row$sitrep_no
    purl <- row$pdf_url

    cat("\n>> SitRep", sno, ":", paste0("SitRep_", sprintf("%02d", sno), "_2026.pdf"), "\n")

    # Download
    local_pdf <- download_sitrep_pdf(purl, sno)
    registry$downloaded[registry$pdf_url == purl] <- !is.na(local_pdf)
    registry$local_pdf[registry$pdf_url == purl]  <- ifelse(!is.na(local_pdf), local_pdf, NA_character_)

    if (is.na(local_pdf)) {
      registry$last_updated[registry$pdf_url == purl] <- as.character(Sys.time())
      next
    }

    # Extract text
    lines <- extract_pdf_text(local_pdf, sno)
    if (is.null(lines) || nrow(lines) == 0) {
      registry$last_updated[registry$pdf_url == purl] <- as.character(Sys.time())
      next
    }

    all_lines[[length(all_lines) + 1]] <- lines
    registry$extracted[registry$pdf_url == purl] <- TRUE

    # Extract indicators
    indics <- extract_indicators(lines)
    indics <- standardize_indicator_cols(indics)
    cat("   Indicators extracted:", nrow(indics), "\n")
    if (nrow(indics) > 0) all_indicators[[length(all_indicators) + 1]] <- indics

    # Extract health zones
    hz <- extract_hz_from_lines(lines)
    n_hz <- if ("health_zone" %in% names(hz)) dplyr::n_distinct(hz$health_zone) else 0
    n_hz_hm <- if (nrow(hz) > 0 && "confidence" %in% names(hz)) {
      dplyr::n_distinct(hz$health_zone[hz$confidence %in% c("high", "medium")])
    } else n_hz
    cat("   Health zones found:", n_hz, "| high/medium confidence:", n_hz_hm, "\n")
    if (nrow(hz) > 0) all_hz[[length(all_hz) + 1]] <- hz

    registry$analysed[registry$pdf_url == purl]     <- TRUE
    registry$last_updated[registry$pdf_url == purl] <- as.character(Sys.time())
    processed_snos <- c(processed_snos, sno)
  }

  save_registry(registry)

  cat("\n--- ÉTAPE 3: Consolidation, QC & Export ---\n")

  # Combine run data
  all_lines_df      <- dplyr::bind_rows(all_lines)
  all_indicators_df <- standardize_indicator_cols(dplyr::bind_rows(all_indicators))
  all_hz_df         <- dplyr::bind_rows(all_hz)

  # Save extracted line table for audit/debug
  if (nrow(all_lines_df) > 0) {
    lines_fp <- file.path(DATA_PROCESSED_DIR, paste0("PREIS_lines_extracted_", format(Sys.Date(), "%Y%m%d"), ".csv"))
    readr::write_csv(all_lines_df, lines_fp)
    cat("   Extracted lines saved:", basename(lines_fp), "\n")
  }

  # Merge with existing indicators on disk, then derive missing indicators.
  indic_fp <- file.path(DATA_FINAL_DIR, "PREIS_indicators_long.csv")
  prev_ind <- tibble::tibble()
  if (file.exists(indic_fp)) {
    prev_ind <- tryCatch(
      readr::read_csv(indic_fp, show_col_types = FALSE),
      error = function(e) tibble::tibble()
    )
  }

  full_indicators <- dplyr::bind_rows(
    standardize_indicator_cols(prev_ind),
    all_indicators_df
  ) %>%
    dplyr::arrange(
      sitrep_no,
      indicator_code,
      dplyr::case_when(value_source == "observed" ~ 1L, TRUE ~ 2L)
    ) %>%
    dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE) %>%
    derive_missing_indicators()

  if (nrow(full_indicators) > 0) {
    readr::write_csv(full_indicators, indic_fp)
    cat("   Indicators saved:", nrow(full_indicators), "rows\n")
  }

  # Merge with existing HZ on disk.
  hz_fp <- file.path(DATA_FINAL_DIR, "PREIS_health_zones.csv")
  prev_hz <- tibble::tibble()
  if (file.exists(hz_fp)) {
    prev_hz <- tryCatch(
      readr::read_csv(hz_fp, show_col_types = FALSE),
      error = function(e) tibble::tibble()
    )
  }

  full_hz_df <- dplyr::bind_rows(prev_hz, all_hz_df)
  if (nrow(full_hz_df) > 0 && "health_zone" %in% names(full_hz_df)) {

    if (!"confidence" %in% names(full_hz_df)) full_hz_df$confidence <- "unknown"
    if (!"rule" %in% names(full_hz_df)) full_hz_df$rule <- "unknown"
    if (!"hz_count" %in% names(full_hz_df)) full_hz_df$hz_count <- NA_real_

    full_hz_df <- full_hz_df %>%
      dplyr::arrange(
        sitrep_no,
        health_zone,
        dplyr::case_when(
          confidence == "high" ~ 1L,
          confidence == "medium" ~ 2L,
          TRUE ~ 3L
        )
      ) %>%
      dplyr::distinct(sitrep_no, health_zone, .keep_all = TRUE)

    readr::write_csv(full_hz_df, hz_fp)
    cat("   Health zones saved:", nrow(full_hz_df), "rows\n")
  }

  # QC automatique
  qc_result <- qc_preis_outputs(full_indicators, full_hz_df, registry)

  qc_by_sitrep_fp <- file.path(DATA_FINAL_DIR, "PREIS_QC_by_sitrep.csv")
  qc_issues_fp    <- file.path(DATA_FINAL_DIR, "PREIS_QC_issues.csv")
  readr::write_csv(qc_result$qc_by_sitrep, qc_by_sitrep_fp)
  readr::write_csv(qc_result$qc_issues, qc_issues_fp)

  if (isTRUE(qc_result$blocking)) {
    cat("   QC WARNING: au moins un blocage CRITICAL détecté. Vérifier PREIS_QC_issues.csv\n")
  } else {
    cat("   QC OK: aucun blocage critique détecté.\n")
  }

  # SECOND PASS: analyse/report each successfully processed SitRep using full history.
  all_reports <- list()
  processed_snos <- sort(unique(processed_snos))

  for (s in processed_snos) {
    analysis <- analyse_sitrep(full_indicators, full_hz_df, s)
    report   <- generate_report(analysis, qc_result = qc_result)
    all_reports[[length(all_reports) + 1]] <-
      list(sitrep_no = s, analysis = analysis, report = report)
  }

  # Save reports as TXT + latest copy
  if (length(all_reports) > 0) {
    for (r in all_reports) {
      if (is.null(r)) next
      txt_fp <- file.path(
        OUTPUT_DIR,
        paste0("PREIS_Report_SitRep_", r$sitrep_no, "_", Sys.Date(), ".txt")
      )
      writeLines(r$report, txt_fp)
      cat("   Report saved:", basename(txt_fp), "\n")
      cat("\n", r$report, "\n")
    }

    latest_idx <- which.max(vapply(all_reports, function(x) x$sitrep_no, numeric(1)))
    latest_report <- all_reports[[latest_idx]]
    latest_fp <- file.path(OUTPUT_DIR, paste0("PREIS_Report_LATEST_SitRep_", latest_report$sitrep_no, ".txt"))
    writeLines(latest_report$report, latest_fp)
    cat("   Latest report copy saved:", basename(latest_fp), "\n")
  }

  # Excel summary
  if (nrow(full_indicators) > 0) {
    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(wb, "indicators")
    openxlsx::writeData(wb, "indicators", full_indicators)

    openxlsx::addWorksheet(wb, "health_zones")
    openxlsx::writeData(wb, "health_zones", full_hz_df)

    openxlsx::addWorksheet(wb, "qc_by_sitrep")
    openxlsx::writeData(wb, "qc_by_sitrep", qc_result$qc_by_sitrep)

    openxlsx::addWorksheet(wb, "qc_issues")
    openxlsx::writeData(wb, "qc_issues", qc_result$qc_issues)

    openxlsx::addWorksheet(wb, "registry")
    openxlsx::writeData(wb, "registry", registry)

    xl_fp <- file.path(
      OUTPUT_DIR,
      paste0("PREIS_Output_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    )
    openxlsx::saveWorkbook(wb, xl_fp, overwrite = TRUE)
    cat("   Excel saved:", basename(xl_fp), "\n")
  }

  # Run log
  n_hz_total <- if (nrow(full_hz_df) > 0 && "health_zone" %in% names(full_hz_df)) {
    dplyr::n_distinct(full_hz_df$health_zone)
  } else 0L

  log_entry <- tibble::tibble(
    run_time            = as.character(Sys.time()),
    n_scraped           = nrow(scraped),
    n_new_or_pending    = nrow(new_rows),
    n_requested_process = nrow(to_process),
    n_processed_success = length(processed_snos),
    n_indicators_total  = nrow(full_indicators),
    n_hz_total          = n_hz_total,
    n_qc_critical       = sum(qc_result$qc_issues$severity == "CRITICAL", na.rm = TRUE),
    n_qc_warning        = sum(qc_result$qc_issues$severity == "WARNING", na.rm = TRUE),
    n_reports           = length(purrr::compact(all_reports))
  )

  if (file.exists(RUN_LOG_FP)) {
    existing_log <- readr::read_csv(
      RUN_LOG_FP,
      col_types = readr::cols(run_time = readr::col_character(), .default = readr::col_guess()),
      show_col_types = FALSE
    )
    existing_log$run_time <- as.character(existing_log$run_time)
    log_entry <- dplyr::bind_rows(existing_log, log_entry)
  }
  readr::write_csv(log_entry, RUN_LOG_FP)

  cat("\n============================================================\n")
  cat("PIPELINE TERMINÉ —", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("SitReps scrapés       :", nrow(scraped), "\n")
  cat("Nouveaux/pending      :", nrow(new_rows), "\n")
  cat("Demandés ce run       :", nrow(to_process), "\n")
  cat("Traités avec succès   :", length(processed_snos), "\n")
  cat("Indicateurs totaux    :", nrow(full_indicators), "\n")
  cat("Zones de santé        :", n_hz_total, "\n")
  cat("QC critical           :", sum(qc_result$qc_issues$severity == "CRITICAL", na.rm = TRUE), "\n")
  cat("QC warning            :", sum(qc_result$qc_issues$severity == "WARNING", na.rm = TRUE), "\n")
  cat("============================================================\n\n")

  invisible(list(
    registry      = registry,
    indicators    = full_indicators,
    health_zones  = full_hz_df,
    qc            = qc_result,
    reports       = all_reports
  ))
}

# ============================================================
# EXÉCUTION
# ============================================================

# Lancer le pipeline.
# Pour un premier rattrapage complet : max_new = Inf
# Pour un test rapide : max_new = 5
results <- run_preis_pipeline(
  force_redownload = FALSE,  # TRUE pour re-traiter les déjà téléchargés
  max_new          = Inf     # Inf = traite tous les nouveaux/pending
)
