############################################################
# PREIS EBOLA DRC SITREP
# 00_PREIS_MASTER_AUTOMATION.R
# PIPELINE COMPLET — SURVEILLANCE + EXTRACTION + ANALYSE
#
# Objectif : surveiller https://insp.cd/ebola/ toutes les
# N heures, détecter les nouveaux SitReps, lire le PDF,
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

BASE_DIR           <- Sys.getenv("GITHUB_WORKSPACE",
                                 unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
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
INSP_MAX_PAGES     <- 12  # nb de pages de pagination a scanner (couvre N1 a N28+)

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

safe_num <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_replace_all(x, "%", "")
  x <- stringr::str_replace_all(x, "[^0-9\\.\\-]", "")
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

  # Post-URL format: .../sitrep-n27-mvb_10-06-2026/  ou ...-mve...
  m_post <- stringr::str_match(as.character(url_or_name),
                               "sitrep-n(\\d{1,3})-(?:mvb|mve)")[, 2]
  if (!is.na(m_post)) return(as.integer(m_post))

  fname <- basename(as.character(url_or_name))
  # Decoder les caracteres URL-encodes (%C2%B0 -> °, %20 -> espace, etc.)
  fname <- gsub("%C2%B0", "N", fname, ignore.case = TRUE)
  fname <- tryCatch(utils::URLdecode(fname), error = function(e) fname)

  # Strategie GENERALISEE : du plus fiable au plus general. Tolerante a
  # tout intercalaire (MVE, RDC, Draft-Final, ...) entre les mots-cles.
  patterns <- c(
    # N° / No / N suivi (eventuellement separe) du numero : N°30, N027, No-31, N 16
    "N[\u00b0\u00bao]?\\s*[-_]?\\s*0*(\\d{1,3})(?![0-9])",
    # NUM-NN
    "NUM[-_ ]?0*(\\d{1,3})(?![0-9])",
    # SITREP / SR directement suivi de chiffres : SR32, SitRep_30, SITREP-23
    "(?:SITREP|SR)[-_ ]?0*(\\d{1,3})(?![0-9])"
  )
  for (pat in patterns) {
    m <- stringr::str_match(fname, stringr::regex(pat, ignore_case = TRUE))[, 2]
    if (!is.na(m)) return(as.integer(m))
  }
  NA_integer_
}

normalize_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\u2018\u2019\u0027]", "'") %>%
    stringr::str_replace_all("[\u00e9\u00e8\u00ea\u00eb]", "e") %>%
    stringr::str_replace_all("[\u00e0\u00e2\u00e4]", "a") %>%
    stringr::str_replace_all("[\u00ee\u00ef]", "i") %>%
    stringr::str_replace_all("[\u00f4\u00f6]", "o") %>%
    stringr::str_replace_all("[\u00f9\u00fb\u00fc]", "u") %>%
    stringr::str_replace_all("\u00e7", "c") %>%
    stringr::str_squish()
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
            "User-Agent" = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                                  "AppleWebKit/537.36 (KHTML, like Gecko) ",
                                  "Chrome/126.0.0.0 Safari/537.36"),
            "Accept"     = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8",
            "Referer"    = "https://insp.cd/"
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

  empty_streak <- 0   # consecutive pages with no SitRep posts
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
      # A failed intermediate page: count as empty but keep going
      empty_streak <- empty_streak + 1
      cat("   Page", pg, ": unreachable (skipped)\n")
      if (empty_streak >= 3) break   # 3 dead pages in a row = really the end
      next
    }

    page_html <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))

    # Post links: href matching /sitrep-nNN-mvb...  OR  /sitrep-...-mve...
    links <- page_html %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    texts <- page_html %>% rvest::html_nodes("a") %>% rvest::html_text(trim = TRUE)

    post_df <- tibble::tibble(post_url = links, post_text = texts) %>%
      dplyr::filter(
        !is.na(post_url),
        # Tolerant : tout lien de post contenant 'sitrep' (n'importe quel
        # format de slug), pas seulement sitrep-nNN-mvb. Le numero exact
        # est extrait ensuite par extract_sitrep_no().
        stringr::str_detect(stringr::str_to_lower(post_url), "sitrep")
      ) %>%
      dplyr::distinct(post_url, .keep_all = TRUE)

    if (nrow(post_df) == 0) {
      empty_streak <- empty_streak + 1
      cat("   Page", pg, ": 0 posts\n")
      if (empty_streak >= 3) break   # likely past the last page
      next
    }

    empty_streak <- 0
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
      sitrep_no = vapply(post_url, extract_sitrep_no, integer(1)),
      date_raw  = stringr::str_match(post_url, "(\\d{2}-\\d{2}-\\d{4})")[, 2],
      epidemic  = "MVB_2026_Ituri"
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

  # Garde : si le scraping n'a rien retourné (ex. INSP temporairement
  # bloqué/indisponible), on s'arrête proprement sans planter. Le
  # téléchargement direct de 08 prendra le relais si nécessaire.
  if (is.null(scraped) || nrow(scraped) == 0 ||
      !("pdf_url" %in% names(scraped))) {
    cat("   Aucun SitRep scrapé (source indisponible) — étape ignorée proprement.\n")
    return(tibble::tibble())
  }

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

  # Robustesse : forcer les colonnes date/heure en character des DEUX
  # cotes avant bind_rows (evite 'Can't combine <datetime> and <character>'
  # quand le registry relu du CSV a first_seen en POSIXct).
  .force_chr <- function(df) {
    for (col in c("first_seen","last_updated","scraped_at","date_raw")) {
      if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
    }
    df
  }
  dplyr::bind_rows(.force_chr(new_sitreps), .force_chr(pending)) %>%
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

  results <- list()

  for (i in seq_len(nrow(line_table))) {
    txt     <- line_table$line_text[i]
    pg      <- line_table$page[i]
    ln      <- line_table$line_no[i]
    sno     <- line_table$sitrep_no[i]
    txt_low <- normalize_text(stringr::str_to_lower(txt))

    found_hz   <- character()
    found_rule <- character()

    # ------ RULE 1: Known dictionary match (whole-word, SAFE) ------
    # Token-based matching avoids regex metacharacter crashes
    # (e.g. "Miti-Murhesa" contains a hyphen that broke str_replace before).
    txt_tokens <- unlist(stringr::str_split(txt_low, "[^a-z0-9\\-]+"))
    txt_tokens <- txt_tokens[txt_tokens != ""]
    for (k in seq_along(KNOWN_HZ_DICT)) {
      hz      <- KNOWN_HZ_DICT[k]
      hz_norm <- KNOWN_HZ_NORM[k]
      is_match <- if (stringr::str_detect(hz_norm, "[ \\-]")) {
        stringr::str_detect(txt_low, stringr::fixed(hz_norm))
      } else {
        hz_norm %in% txt_tokens
      }
      if (isTRUE(is_match)) {
        found_hz   <- c(found_hz, hz)
        found_rule <- c(found_rule, "known_dictionary_match")
      }
    }

    # ------ RULE 2: "X (n)" count pattern, but ONLY if X is in dictionary
    # Captures "Rwampara (5), Bunia (4)" — validated against KNOWN_HZ_DICT
    # to prevent false positives like "Deux (2)" or "Huit (8)".
    m_all <- stringr::str_match_all(
      txt, "([A-Z][A-Za-z\u00c0-\u00ff\\-]{2,20})\\s*\\(\\d+"
    )[[1]][, 2]
    m_all <- m_all[!is.na(m_all)]
    for (cand in m_all) {
      cand_norm <- normalize_text(stringr::str_to_lower(stringr::str_squish(cand)))
      if (cand_norm %in% KNOWN_HZ_NORM) {
        # map back to canonical dictionary spelling
        canon <- KNOWN_HZ_DICT[match(cand_norm, KNOWN_HZ_NORM)]
        found_hz   <- c(found_hz, canon)
        found_rule <- c(found_rule, "hz_count_validated")
      }
    }

    if (length(found_hz) > 0) {
      results[[length(results) + 1]] <- tibble::tibble(
        sitrep_no    = sno,
        page         = pg,
        line_no      = ln,
        health_zone  = unique(found_hz),
        rule         = found_rule[!duplicated(found_hz)],
        evidence_line = txt
      )
    }
  }

  if (length(results) == 0) return(tibble::tibble())

  dplyr::bind_rows(results) %>%
    dplyr::distinct(sitrep_no, health_zone, .keep_all = TRUE)
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
  add <- function(code, val, rule, domain = "epidemiology") {
    if (code %in% seen) return(invisible())
    if (!is.null(val) && length(val) == 1 && !is.na(val) && is.finite(val)) {
      results[[length(results) + 1]] <<- tibble::tibble(
        sitrep_no       = sno,
        indicator_code  = code,
        domain          = domain,
        value           = as.numeric(val),
        extraction_rule = rule
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

  # Cumulative deaths — narrative variants (SitReps without a Total row)
  add("cumulative_deaths",
      g("[Cc]umul\\s+d[e\u00e9]c[e\u00e8]s\\s+parmi\\s+les\\s+confirm\\w*\\s+(\\d+)"),
      "narr_cumul_deces", "deaths")
  add("cumulative_deaths",
      g("dont\\s+(\\d+)\\s+d[e\u00e9]c[e\u00e8]s"), "narr_dont_deces", "deaths")
  add("cumulative_deaths",
      g("(\\d+)\\s+d[e\u00e9]c[e\u00e8]s\\s+parmi\\s+les\\s+(?:cas\\s+)?confirm"),
      "narr_deces_parmi", "deaths")
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
  # Box value row like "42 282* 220" or "48 321* 116" (deaths cases * gueris).
  # Scan individual lines first, then the flattened text as a fallback
  # (pdftools sometimes splits the row across lines).
  box_candidates <- lines[stringr::str_detect(
    lines, "^\\s*\\d{1,4}\\s+\\d{1,4}\\*?\\s*\\d{0,4}\\s*$"
  )]
  if (length(box_candidates) == 0) {
    # fallback: find "NN NNN* NNN" anywhere in flat text
    m <- stringr::str_match(flat, "(\\d{1,4})\\s+(\\d{2,4})\\*\\s*(\\d{1,4})")
    if (!is.na(m[1, 1])) box_candidates <- m[1, 1]
  }
  for (bx in box_candidates) {
    parts <- stringr::str_split(stringr::str_squish(gsub("\\*", " ", bx)), "\\s+")[[1]]
    nums  <- suppressWarnings(as.numeric(parts))
    nums  <- nums[!is.na(nums)]
    if (length(nums) >= 2) {
      a <- nums[1]; b <- nums[2]
      cc <- max(a, b); dd <- min(a, b)   # cases >= deaths (anti-inversion)
      # Sanity: a real cumulative case count is plausibly >= 10 by SitRep 5+
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
      add("case_fatality_ratio", round(100 * cd / cc, 1), "computed_cfr", "deaths")
    }
  }

  if (length(results) == 0) return(tibble::tibble())
  out <- dplyr::bind_rows(results) %>%
    dplyr::filter(!is.na(value), is.finite(value)) %>%
    dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE)

  # ---- SANITY CHECKS (drop implausible values) ----
  # A cumulative confirmed-case count below 1 is meaningless; and if the
  # box-horizontal pattern grabbed a tiny number (e.g. "2") while a province
  # breakdown or narrative implies far more, drop the tiny one.
  cc_idx <- which(out$indicator_code == "cumulative_confirmed_cases")
  if (length(cc_idx) == 1) {
    cc_val <- out$value[cc_idx]
    ituri  <- out$value[out$indicator_code == "cases_ituri"]
    # if Ituri alone exceeds the national cumulative, the cumulative is wrong
    if (length(ituri) == 1 && !is.na(ituri) && cc_val < ituri) {
      out <- out[-cc_idx, ]
    }
  }
  # CFR must be 0-100
  out <- out[!(out$indicator_code == "case_fatality_ratio" &
               (out$value < 0 | out$value > 100)), ]
  out
}



# ============================================================
# ÉTAPE 6 — ANALYSE OPÉRATIONNELLE
# ============================================================

analyse_sitrep <- function(indicators_long, hz_mentions, sitrep_no) {

  cat("   Analysing SitRep", sitrep_no, "\n")

  # Guard: if sitrep_no is NA, fall back to all indicators passed in
  # (they all belong to this sitrep anyway since extract is per-file)
  use_filter <- !is.na(sitrep_no)

  get_val <- function(code) {
    df <- indicators_long
    if (use_filter && "sitrep_no" %in% names(df)) {
      df <- df %>% dplyr::filter(sitrep_no == .env$sitrep_no)
    }
    v <- df %>%
      dplyr::filter(indicator_code == code) %>%
      dplyr::slice(1) %>%
      dplyr::pull(value)
    if (length(v) == 0) NA_real_ else v
  }

  cumul_cases   <- get_val("cumulative_confirmed_cases")
  new_cases     <- get_val("new_confirmed_cases")
  cumul_deaths  <- get_val("cumulative_deaths")
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

  # NEW DEATHS: computed as difference vs previous SitRep cumulative deaths.
  # This is more reliable than parsing "X décès aujourd'hui" (often partial).
  new_deaths <- NA_real_
  if (!is.na(sitrep_no) && "sitrep_no" %in% names(indicators_long)) {
    prev_deaths <- indicators_long %>%
      dplyr::filter(
        sitrep_no < .env$sitrep_no,
        indicator_code == "cumulative_deaths"
      ) %>%
      dplyr::arrange(dplyr::desc(sitrep_no)) %>%
      dplyr::slice(1) %>%
      dplyr::pull(value)
    if (length(prev_deaths) == 1 && !is.na(cumul_deaths) && !is.na(prev_deaths)) {
      d <- cumul_deaths - prev_deaths
      if (is.finite(d) && d >= 0) new_deaths <- d
    }
  }
  # Fallback: explicit daily-deaths indicator if present
  if (is.na(new_deaths)) new_deaths <- get_val("deaths_today")

  hz_list <- hz_mentions
  if (use_filter && nrow(hz_list) > 0 && "sitrep_no" %in% names(hz_list)) {
    hz_list <- hz_list %>% dplyr::filter(sitrep_no == .env$sitrep_no)
  }
  hz_list <- if (nrow(hz_list) > 0) {
    hz_list %>% dplyr::distinct(health_zone) %>% dplyr::pull(health_zone)
  } else character()

  # Signal classification
  signals <- tibble::tibble(
    indicator_code = c(
      "cumulative_confirmed_cases", "new_confirmed_cases",
      "cumulative_deaths", "new_deaths", "case_fatality_ratio",
      "contacts_followup_rate", "alerts_investigation_rate"
    ),
    value = c(
      cumul_cases, new_cases, cumul_deaths, new_deaths,
      cfr, followup_rate, inv_rate
    )
  ) %>%
    dplyr::mutate(
      signal_level = dplyr::case_when(
        indicator_code == "new_deaths"          & !is.na(value) & value > 0   ~ "RED",
        indicator_code == "case_fatality_ratio" & !is.na(value) & value >= 15 ~ "RED",
        indicator_code == "contacts_followup_rate" & !is.na(value) & value < 80 ~ "RED",
        indicator_code == "new_confirmed_cases" & !is.na(value) & value > 0   ~ "ORANGE",
        indicator_code == "contacts_followup_rate" & !is.na(value) & value < 90 ~ "ORANGE",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value < 90 ~ "ORANGE",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value >= 90 ~ "GREEN",
        TRUE ~ "MONITOR"
      ),
      probable_driver = dplyr::case_when(
        indicator_code == "new_deaths" ~
          "Présentation tardive, référence tardive, décès communautaires, gaps de prise en charge.",
        indicator_code == "case_fatality_ratio" & !is.na(value) & value >= 15 ~
          "Sous-détection des cas bénins, sévérité clinique, retard de soins.",
        indicator_code == "new_confirmed_cases" ~
          "Chaînes de transmission actives, isolement incomplet, détection améliorée.",
        indicator_code == "contacts_followup_rate" & !is.na(value) & value < 90 ~
          "Capacité insuffisante, mobilité des contacts, réticence communautaire.",
        indicator_code == "alerts_investigation_rate" & !is.na(value) & value < 90 ~
          "Délai d'investigation, contraintes transport, charge de travail.",
        TRUE ~ "Monitoring continu requis."
      )
    ) %>%
    dplyr::filter(!is.na(value))

  list(
    sitrep_no    = sitrep_no,
    cumul_cases  = cumul_cases,
    new_cases    = new_cases,
    cumul_deaths = cumul_deaths,
    new_deaths   = new_deaths,
    cfr          = cfr,
    followup_rate = followup_rate,
    inv_rate     = inv_rate,
    hz_list      = hz_list,
    signals      = signals
  )
}

# ============================================================
# ÉTAPE 7 — RAPPORT OPÉRATIONNEL (texte court)
# ============================================================

generate_report <- function(analysis) {

  fmt <- function(x) if (is.na(x)) "non disponible" else
    format(round(x, 1), big.mark = ",", trim = TRUE)

  pct <- function(x) if (is.na(x)) "non disponible" else
    paste0(round(x, 1), "%")

  n_red    <- sum(analysis$signals$signal_level == "RED",    na.rm = TRUE)
  n_orange <- sum(analysis$signals$signal_level == "ORANGE", na.rm = TRUE)
  n_green  <- sum(analysis$signals$signal_level == "GREEN",  na.rm = TRUE)
  hz_str   <- if (length(analysis$hz_list) > 0)
    paste(analysis$hz_list, collapse = ", ") else "non identifiées"

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
    "Taux d'investigation alertes: {pct(analysis$inv_rate)}\n\n",
    "ZONES DE SANTÉ DÉTECTÉES\n",
    "{hz_str}\n\n",
    "SIGNAUX OPÉRATIONNELS\n",
    paste0(
      ifelse(analysis$signals$signal_level != "MONITOR",
             paste0("[", analysis$signals$signal_level, "] ",
                    analysis$signals$indicator_code, " = ",
                    round(analysis$signals$value, 1), "\n   Driver probable: ",
                    analysis$signals$probable_driver),
             NA),
      collapse = "\n"
    ) %>%
      stringr::str_replace_all("NA\n?", "") %>%
      stringr::str_squish(),
    "\n\n",
    "NOTE : Drivers probables uniquement — pas de causalité établie.\n",
    "Valider avec ligne-liste officielle, registre contacts, laboratoire.\n"
  )
}

# ============================================================
# PIPELINE PRINCIPAL
# ============================================================

run_preis_pipeline <- function(
    force_redownload = FALSE,
    max_new          = 5       # max SitReps à traiter par run
) {

  cat("\n--- ÉTAPE 1: Scraping INSP ---\n")
  scraped  <- scrape_insp_sitrep_list()

  if (nrow(scraped) == 0) {
    cat("Impossible de scraper la page. Pipeline arrêté.\n")
    return(invisible(NULL))
  }

  registry <- load_registry()

  # Purge invalid entries: drop rows with sitrep_no NA.
  # 2026 (17eme epidemie) numbering restarts at 1, so keep all >= 1.
  n_before <- nrow(registry)
  registry <- registry %>%
    dplyr::filter(!is.na(sitrep_no), sitrep_no >= 1)
  n_purged <- n_before - nrow(registry)
  if (n_purged > 0) {
    cat("   Purged", n_purged, "invalid (NA) entries from registry.\n")
    save_registry(registry)
  }

  # Safety: if registry has 0 rows but scraped has data → treat all as new
  if (nrow(registry) == 0) {
    cat("   Registry vide — tous les SitReps seront traités.\n")
  }

  # Update registry with all known SitReps
  new_rows <- detect_new_sitreps(scraped, registry)

  # Force character on all datetime columns before bind_rows
  # (prevents POSIXct vs character conflict)
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

  # ----------------------------------------------------------
  # REGISTER LOCAL PDFs (e.g. SitReps 1-14 fetched from GitHub
  # INRB repo, which the INSP site does not expose as posts).
  # Any data/pdf/SitRep_NN_2026.pdf not already in the registry
  # is added so it gets extracted too.
  # ----------------------------------------------------------
  local_pdfs <- list.files(PDF_DIR, pattern = "^SitRep_\\d+_2026\\.pdf$",
                           full.names = TRUE)
  if (length(local_pdfs) > 0) {
    local_df <- tibble::tibble(
      local_pdf = local_pdfs,
      sitrep_no = as.integer(stringr::str_match(
        basename(local_pdfs), "SitRep_(\\d+)_2026")[, 2])
    ) %>%
      dplyr::filter(!is.na(sitrep_no))

    # Keep only those NOT already in registry (by sitrep_no)
    known_nos <- registry$sitrep_no[!is.na(registry$sitrep_no)]
    orphans <- local_df %>% dplyr::filter(!sitrep_no %in% known_nos)

    if (nrow(orphans) > 0) {
      orphan_rows <- tibble::tibble(
        sitrep_no    = orphans$sitrep_no,
        pdf_url      = paste0("local://", basename(orphans$local_pdf)),
        date_raw     = NA_character_,
        link_text    = "GitHub INRB",
        year_in_url  = 2026L,
        downloaded   = TRUE,
        extracted    = FALSE,
        analysed     = FALSE,
        local_pdf    = orphans$local_pdf,
        first_seen   = as.character(Sys.time()),
        last_updated = as.character(Sys.time()),
        scraped_at   = as.character(Sys.time()),
        epidemic     = "MVB_2026_Ituri"
      )
      registry <- dplyr::bind_rows(force_chr_cols(registry),
                                   force_chr_cols(orphan_rows)) %>%
        dplyr::distinct(sitrep_no, .keep_all = TRUE) %>%
        dplyr::arrange(dplyr::desc(sitrep_no))
      save_registry(registry)
      cat("   Registered", nrow(orphans), "local PDF(s) from data/pdf/:",
          paste(sort(orphans$sitrep_no), collapse = ", "), "\n")
    }
  }

  # Select SitReps to process:
  # - not yet extracted, OR force_redownload is TRUE
  # - always process from highest sitrep_no down (most recent first)
  to_process <- registry %>%
    dplyr::filter(
      is.na(extracted) | !extracted | force_redownload
    ) %>%
    dplyr::arrange(dplyr::desc(sitrep_no)) %>%
    dplyr::slice_head(n = max_new)

  if (nrow(to_process) == 0) {
    cat("Aucun nouveau SitRep à traiter. Tout est à jour.\n")
    return(invisible(registry))
  }

  cat("\n--- ÉTAPE 2: Téléchargement & Extraction ---\n")

  all_lines      <- list()
  all_indicators <- list()
  all_hz         <- list()
  all_reports    <- list()

  for (i in seq_len(nrow(to_process))) {
    row    <- to_process[i, ]
    sno    <- row$sitrep_no
    purl   <- row$pdf_url

    cat("\n>> SitRep", sno, ":", paste0("SitRep_", sprintf("%02d", sno), "_2026.pdf"), "\n")

    # Local PDF (from GitHub INRB) vs remote (scraped from INSP site)
    if (grepl("^local://", purl)) {
      local_pdf <- row$local_pdf
      if (is.na(local_pdf) || !file.exists(local_pdf)) {
        # reconstruct expected path
        local_pdf <- file.path(PDF_DIR, sprintf("SitRep_%02d_2026.pdf", sno))
      }
      if (!file.exists(local_pdf)) {
        cat("   Local PDF introuvable:", local_pdf, "\n")
        next
      }
      cat("   Source: fichier local (GitHub INRB)\n")
    } else {
      local_pdf <- download_sitrep_pdf(purl, sno)
    }
    registry$downloaded[registry$pdf_url == purl] <- !is.na(local_pdf)
    registry$local_pdf[registry$pdf_url == purl]  <- local_pdf %||% NA_character_

    if (is.na(local_pdf)) next

    # Extract text
    lines <- extract_pdf_text(local_pdf, sno)
    if (is.null(lines) || nrow(lines) == 0) next

    all_lines[[i]] <- lines
    registry$extracted[registry$pdf_url == purl] <- TRUE

    # Extract indicators
    indics <- extract_indicators(lines)
    cat("   Indicators extracted:", nrow(indics), "\n")
    if (nrow(indics) > 0) all_indicators[[i]] <- indics

    # Extract health zones
    hz <- extract_hz_from_lines(lines)
    n_hz <- if ("health_zone" %in% names(hz)) dplyr::n_distinct(hz$health_zone) else 0
    cat("   Health zones found:", n_hz, "\n")
    if (nrow(hz) > 0) all_hz[[i]] <- hz

    registry$analysed[registry$pdf_url == purl]     <- TRUE
    registry$last_updated[registry$pdf_url == purl] <- as.character(Sys.time())
  }

  save_registry(registry)

  cat("\n--- ÉTAPE 3: Consolidation & Export ---\n")

  # Combine all data
  all_lines_df      <- dplyr::bind_rows(all_lines)
  all_indicators_df <- dplyr::bind_rows(all_indicators)
  all_hz_df         <- dplyr::bind_rows(all_hz)

  # Merge with EXISTING indicators on disk so new_deaths difference
  # can use the full history (previous runs included).
  full_indicators <- all_indicators_df
  indic_existing_fp <- file.path(DATA_FINAL_DIR, "PREIS_indicators_long.csv")
  if (file.exists(indic_existing_fp)) {
    prev_ind <- tryCatch(
      readr::read_csv(indic_existing_fp, show_col_types = FALSE),
      error = function(e) tibble::tibble()
    )
    if (nrow(prev_ind) > 0) {
      full_indicators <- dplyr::bind_rows(prev_ind, all_indicators_df) %>%
        dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE)
    }
  }

  # ----------------------------------------------------------
  # INRB REFERENCE: fill gaps for scanned/unreadable SitReps and
  # provide cross-validation. Maps INRB national codes to ours.
  # ----------------------------------------------------------
  inrb_fp <- file.path(DATA_FINAL_DIR, "INRB_reference_national.csv")
  if (file.exists(inrb_fp)) {
    inrb <- tryCatch(readr::read_csv(inrb_fp, show_col_types = FALSE),
                     error = function(e) tibble::tibble())
    if (nrow(inrb) > 0) {
      code_map <- c(
        national_cumulative_confirmed_cases  = "cumulative_confirmed_cases",
        national_cumulative_confirmed_deaths = "cumulative_deaths",
        national_cumulative_suspected_cases  = "suspected_cases_investigation"
      )
      inrb_mapped <- inrb %>%
        dplyr::filter(indicator_code %in% names(code_map)) %>%
        dplyr::mutate(
          indicator_code = unname(code_map[indicator_code]),
          domain = "reference",
          extraction_rule = "INRB_reference"
        ) %>%
        dplyr::select(sitrep_no, indicator_code, value, domain, extraction_rule)

      # ----------------------------------------------------------
      # INRB = PRIORITY SOURCE for national cumulative indicators.
      # The INRB figures are manually transcribed and validated by
      # the institution; PDF auto-extraction is error-prone for
      # these totals (e.g. SitRep 13 -> 906, SitRep 19 -> 2026).
      # So for cumulative_confirmed_cases / cumulative_deaths /
      # suspected_cases_investigation, we OVERRIDE the PDF value
      # with the INRB value for EVERY SitRep INRB covers, and mark
      # it supervisor_validated. PDF extraction is kept only for
      # what INRB lacks (health zones, alerts, lab, PoE, etc.).
      # ----------------------------------------------------------
      override_codes <- c("cumulative_confirmed_cases",
                          "cumulative_deaths",
                          "suspected_cases_investigation")

      # Snapshot of PDF-extracted values BEFORE override, so the
      # validation table documents where auto-extraction diverged.
      pdf_snapshot <- full_indicators %>%
        dplyr::filter(indicator_code %in% c("cumulative_confirmed_cases",
                                            "cumulative_deaths"),
                      extraction_rule != "INRB_reference") %>%
        dplyr::select(sitrep_no, indicator_code, pdf_value = value)

      # Drop PDF-extracted rows for these codes where INRB has a value
      inrb_pairs <- inrb_mapped %>%
        dplyr::select(sitrep_no, indicator_code) %>%
        dplyr::distinct()

      n_before <- nrow(full_indicators)
      full_indicators <- full_indicators %>%
        dplyr::anti_join(
          inrb_pairs %>% dplyr::filter(indicator_code %in% override_codes),
          by = c("sitrep_no", "indicator_code")
        )
      n_dropped <- n_before - nrow(full_indicators)

      # Add INRB values (these become the authoritative source)
      inrb_authoritative <- inrb_mapped %>%
        dplyr::filter(indicator_code %in% override_codes)
      full_indicators <- dplyr::bind_rows(full_indicators, inrb_authoritative)

      # RECOMPUTE CFR from the INRB-corrected cas/décès, so the létalité
      # is always consistent with the authoritative totals. Any PDF-extracted
      # case_fatality_ratio for a SitRep covered by INRB is replaced.
      cfr_recompute <- inrb_authoritative %>%
        dplyr::filter(indicator_code %in% c("cumulative_confirmed_cases",
                                            "cumulative_deaths")) %>%
        tidyr::pivot_wider(id_cols = sitrep_no,
                           names_from = indicator_code, values_from = value) %>%
        dplyr::filter(!is.na(cumulative_confirmed_cases),
                      cumulative_confirmed_cases > 0,
                      !is.na(cumulative_deaths),
                      cumulative_deaths <= cumulative_confirmed_cases) %>%
        dplyr::transmute(
          sitrep_no,
          indicator_code  = "case_fatality_ratio",
          value           = round(100 * cumulative_deaths /
                                   cumulative_confirmed_cases, 1),
          domain          = "deaths",
          extraction_rule = "computed_from_INRB"
        )
      if (nrow(cfr_recompute) > 0) {
        full_indicators <- full_indicators %>%
          dplyr::anti_join(
            cfr_recompute %>% dplyr::select(sitrep_no, indicator_code),
            by = c("sitrep_no", "indicator_code")
          ) %>%
          dplyr::bind_rows(cfr_recompute)
      }

      cat("   INRB priority applied:", nrow(inrb_authoritative),
          "national values (cas/décès/suspects) sur",
          length(unique(inrb_authoritative$sitrep_no)), "SitReps;",
          n_dropped, "valeurs PDF remplacées;",
          nrow(cfr_recompute), "CFR recalculés\n")

      # Cross-validation: PDF auto-extraction vs INRB reference.
      # Documents where extraction was right/wrong (for the method note).
      validation <- pdf_snapshot %>%
        dplyr::left_join(
          inrb_mapped %>% dplyr::select(sitrep_no, indicator_code,
                                        inrb_value = value),
          by = c("sitrep_no", "indicator_code")
        ) %>%
        dplyr::mutate(
          diff = pdf_value - inrb_value,
          match = dplyr::case_when(
            is.na(inrb_value) ~ "no_ref",
            abs(diff) <= 2    ~ "OK",
            TRUE              ~ "ECART_corrige_par_INRB"
          )
        ) %>%
        dplyr::arrange(sitrep_no, indicator_code)
      readr::write_csv(validation,
                       file.path(DATA_FINAL_DIR, "PREIS_validation_vs_INRB.csv"))
      n_ecart <- sum(validation$match == "ECART_corrige_par_INRB", na.rm = TRUE)
      cat("   Validation vs INRB:",
          sum(validation$match == "OK", na.rm = TRUE), "OK,",
          n_ecart, "écarts\n")
    }
  }

  # SECOND PASS: analyse each processed SitRep using the FULL history
  # (enables new_deaths = cumul_deaths[t] - cumul_deaths[t-1]).
  processed_snos <- sort(unique(full_indicators$sitrep_no))
  for (s in processed_snos) {
    hz_s <- if (nrow(all_hz_df) > 0) {
      all_hz_df %>% dplyr::filter(sitrep_no == s)
    } else tibble::tibble()
    analysis <- analyse_sitrep(full_indicators, hz_s, s)
    report   <- generate_report(analysis)
    all_reports[[length(all_reports) + 1]] <-
      list(sitrep_no = s, analysis = analysis, report = report)
  }

  # Save CSVs — on sauvegarde full_indicators (valeurs CORRIGÉES INRB),
  # pas all_indicators_df (extraction PDF brute). On REMPLACE proprement :
  # une seule valeur par (sitrep_no, indicator_code), priorité aux valeurs
  # déjà corrigées de ce run.
  if (exists("full_indicators") && nrow(full_indicators) > 0) {
    indic_fp <- file.path(DATA_FINAL_DIR, "PREIS_indicators_long.csv")
    to_save <- full_indicators
    if (file.exists(indic_fp)) {
      existing <- readr::read_csv(indic_fp, show_col_types = FALSE)
      # SitReps retraités dans ce run -> on écrase leurs anciennes lignes
      snos_now <- unique(to_save$sitrep_no)
      existing_keep <- existing %>% dplyr::filter(!sitrep_no %in% snos_now)
      to_save <- dplyr::bind_rows(existing_keep, to_save)
    }
    to_save <- to_save %>%
      dplyr::distinct(sitrep_no, indicator_code, .keep_all = TRUE) %>%
      dplyr::arrange(sitrep_no, indicator_code)
    readr::write_csv(to_save, indic_fp)
    cat("   Indicators saved:", nrow(to_save), "rows (valeurs corrigées INRB)\n")
  }

  if (nrow(all_hz_df) > 0) {
    hz_fp <- file.path(DATA_FINAL_DIR, "PREIS_health_zones.csv")
    if (file.exists(hz_fp)) {
      existing <- readr::read_csv(hz_fp, show_col_types = FALSE)
      all_hz_df <- dplyr::bind_rows(existing, all_hz_df) %>%
        dplyr::distinct(sitrep_no, health_zone, .keep_all = TRUE)
    }
    readr::write_csv(all_hz_df, hz_fp)
    cat("   Health zones saved:", nrow(all_hz_df), "rows\n")
  }

  # Save reports as TXT
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

  # Excel summary
  if (nrow(all_indicators_df) > 0) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "indicators")
    openxlsx::writeData(wb, "indicators", all_indicators_df)
    openxlsx::addWorksheet(wb, "health_zones")
    openxlsx::writeData(wb, "health_zones", all_hz_df)
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
  log_entry <- tibble::tibble(
    run_time          = as.character(Sys.time()),
    n_scraped         = nrow(scraped),
    n_new_detected    = nrow(new_rows),
    n_processed       = nrow(to_process),
    n_indicators_total = nrow(all_indicators_df),
    n_hz_total        = nrow(all_hz_df),
    n_reports         = length(purrr::compact(all_reports))
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
  cat("SitReps scrapés     :", nrow(scraped), "\n")
  cat("Nouveaux détectés   :", nrow(new_rows), "\n")
  cat("Traités ce run      :", nrow(to_process), "\n")
  cat("Indicateurs totaux  :", nrow(all_indicators_df), "\n")
  cat("Zones de santé      :", dplyr::n_distinct(all_hz_df$health_zone), "\n")
  cat("============================================================\n\n")

  invisible(list(
    registry      = registry,
    indicators    = all_indicators_df,
    health_zones  = all_hz_df,
    reports       = all_reports,
    n_new         = nrow(new_rows),
    new_sitrep_nos= if (nrow(new_rows) > 0) sort(unique(new_rows$sitrep_no)) else integer(0),
    n_processed   = nrow(to_process)
  ))
}

# ============================================================
# EXÉCUTION
# ============================================================

# Opérateur pour remplacer NULL (base R ne l'a pas)
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# Lancer le pipeline
results <- run_preis_pipeline(
  force_redownload = FALSE,  # TRUE pour re-traiter les déjà téléchargés
  max_new          = 5       # Limite par run (augmenter pour rattrapage)
)
