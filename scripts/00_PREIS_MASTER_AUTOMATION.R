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

# Source de données principale
INSP_EBOLA_PAGE <- "https://insp.cd/ebola/"

# Fichier de registre des SitReps connus
REGISTRY_FP  <- file.path(DATA_FINAL_DIR, "sitrep_registry.csv")
RUN_LOG_FP   <- file.path(LOG_DIR, "master_run_log.csv")

# ============================================================
# PACKAGES
# ============================================================

packages <- c(
  "dplyr", "readr", "stringr", "tibble", "tidyr",
  "purrr", "openxlsx", "glue", "lubridate",
  "rvest", "httr", "pdftools"
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

scrape_insp_sitrep_list <- function(page_url = INSP_EBOLA_PAGE) {

  cat(">> Scraping:", page_url, "\n")

  resp <- tryCatch(
    httr::GET(
      page_url,
      httr::timeout(30),
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (PREIS-Bot/1.0)"
      )
    ),
    error = function(e) {
      cat("   ERROR: could not reach", page_url, "-", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    cat("   WARNING: HTTP", if (!is.null(resp)) httr::status_code(resp) else "NA", "\n")
    return(tibble::tibble())
  }

  page_html <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))

  # Extract all links pointing to PDF files
  all_links <- page_html %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # Also extract link text (labels like "SITREP N°27")
  all_texts <- page_html %>%
    rvest::html_nodes("a") %>%
    rvest::html_text(trim = TRUE)

  pdf_links <- tibble::tibble(
    href      = all_links,
    link_text = all_texts
  ) %>%
    dplyr::filter(
      !is.na(href),
      stringr::str_detect(href, "\\.pdf$|\\.PDF$")
    ) %>%
    dplyr::mutate(
      pdf_url     = dplyr::if_else(
        stringr::str_starts(href, "http"),
        href,
        paste0("https://insp.cd", href)
      ),
      # Extract sitrep number from URL or label
      sitrep_no_raw = stringr::str_extract(
        link_text,
        "(?:N[o\u00b0]?|n[o\u00b0]?|NUM[\\. ]?|num[\\. ]?)\\s*(\\d+)"
      ),
      sitrep_no = suppressWarnings(as.integer(
        stringr::str_extract(sitrep_no_raw, "\\d+")
      )),
      # Extract sitrep number from filename as fallback
      sitrep_no = dplyr::if_else(
        is.na(sitrep_no),
        suppressWarnings(as.integer(
          stringr::str_extract(href, "(?:NUM[-_]?|N[o\u00b0]?[-_]?)0*(\\d+)")
        )),
        sitrep_no
      ),
      # Extract date from URL/filename  (DD_MM_YYYY or YYYYMMDD)
      date_raw = stringr::str_extract(
        href,
        "(?:\\d{2}[-_]\\d{2}[-_]\\d{4}|\\d{4}\\d{2}\\d{2})"
      ),
      source_page = page_url,
      scraped_at  = as.character(Sys.time())
    ) %>%
    dplyr::filter(!is.na(pdf_url)) %>%
    dplyr::distinct(pdf_url, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))

  cat("   Found", nrow(pdf_links), "PDF links\n")
  pdf_links
}

# ============================================================
# ÉTAPE 2 — COMPARER AVEC LE REGISTRE EXISTANT
# ============================================================

load_registry <- function() {
  if (file.exists(REGISTRY_FP)) {
    readr::read_csv(REGISTRY_FP, show_col_types = FALSE)
  } else {
    tibble::tibble(
      sitrep_no    = integer(),
      pdf_url      = character(),
      date_raw     = character(),
      link_text    = character(),
      downloaded   = logical(),
      extracted    = logical(),
      analysed     = logical(),
      local_pdf    = character(),
      first_seen   = character(),
      last_updated = character()
    )
  }
}

save_registry <- function(registry) {
  readr::write_csv(registry, REGISTRY_FP)
}

detect_new_sitreps <- function(scraped, registry) {

  known_urls <- registry$pdf_url

  new_sitreps <- scraped %>%
    dplyr::filter(!pdf_url %in% known_urls) %>%
    dplyr::mutate(
      downloaded   = FALSE,
      extracted    = FALSE,
      analysed     = FALSE,
      local_pdf    = NA_character_,
      first_seen   = as.character(Sys.time()),
      last_updated = as.character(Sys.time())
    )

  cat("   New SitReps detected:", nrow(new_sitreps), "\n")
  new_sitreps
}

# ============================================================
# ÉTAPE 3 — TÉLÉCHARGER LES PDFs NOUVEAUX
# ============================================================

download_sitrep_pdf <- function(pdf_url, sitrep_no, pdf_dir = PDF_DIR) {

  # Build safe local filename
  fname <- stringr::str_extract(pdf_url, "[^/]+\\.pdf$")
  if (is.na(fname)) fname <- paste0("SitRep_", sitrep_no, ".pdf")
  local_path <- file.path(pdf_dir, fname)

  if (file.exists(local_path)) {
    cat("   Already downloaded:", fname, "\n")
    return(local_path)
  }

  cat("   Downloading SitRep", sitrep_no, "->", fname, "\n")

  resp <- tryCatch(
    httr::GET(
      pdf_url,
      httr::timeout(60),
      httr::write_disk(local_path, overwrite = TRUE),
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (PREIS-Bot/1.0)",
        "Referer"    = INSP_EBOLA_PAGE
      )
    ),
    error = function(e) {
      cat("   ERROR downloading:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    cat("   FAILED: HTTP", if (!is.null(resp)) httr::status_code(resp) else "NA", "\n")
    if (file.exists(local_path)) file.remove(local_path)
    return(NA_character_)
  }

  # Verify it's a real PDF
  size_kb <- round(file.info(local_path)$size / 1024, 1)
  if (size_kb < 10) {
    cat("   WARNING: file too small (", size_kb, "KB) — may not be a valid PDF\n")
    return(NA_character_)
  }

  cat("   OK:", size_kb, "KB\n")
  local_path
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
# ÉTAPE 5A — DICTIONNAIRE DES ZONES DE SANTÉ (INSP MVE 2026)
# ============================================================
# Source : pages INSP lues + SitReps N°16, N°17
# Ce dictionnaire est la VÉRITÉ DE RÉFÉRENCE pour valider
# les candidats extraits du texte.
# Maintenu à jour manuellement + auto-enrichi.

KNOWN_HZ_DICT <- c(
  # Ituri (17ème épidémie 2026)
  "Aru", "Aungba", "Bambu", "Bunia", "Damas", "Gety", "Gethy",
  "Kilo", "Komanda", "Lita", "Logo", "Mangala", "Mongbwalu",
  "Nizi", "Nyankunde", "Rwampara",
  # Nord-Kivu (17ème épidémie 2026)
  "Beni", "Butembo", "Goma", "Kalunguta", "Katwa", "Kyondo", "Oicha",
  # Sud-Kivu (17ème épidémie 2026)
  "Miti-Murhesa",
  # Kasaï (épidémies 2025)
  "Bulape", "Mweka", "Bambalayi", "Bambalaie", "Dikolo",
  "Ingongo", "Mpianga", "Kananga", "Tshikapa",
  "Dekese", "Dibaya", "Ilebo", "Katende", "Kazumba",
  "Luebo", "Lungudi", "Demba", "Dimbelenge",
  "Kamonia", "Kole", "Luiza", "Muya", "Ngandajika",
  "Sankuru", "Tshilenge", "Tshimbulu"
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

    # ------ RULE 1: Known dictionary match (word boundary) ------
    for (k in seq_along(KNOWN_HZ_DICT)) {
      hz      <- KNOWN_HZ_DICT[k]
      hz_norm <- KNOWN_HZ_NORM[k]
      pattern <- paste0("\\b", stringr::str_replace_all(
        hz_norm, "([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1"
      ), "\\b")
      if (stringr::str_detect(txt_low, stringr::regex(pattern, ignore_case = TRUE))) {
        found_hz   <- c(found_hz, hz)
        found_rule <- c(found_rule, "known_dictionary_match")
      }
    }

    # ------ RULE 2: "ZS de X" / "zone de santé de X" ------
    m <- stringr::str_match(
      txt,
      stringr::regex(
        "(?:ZS|zones?\\s+de\\s+sant[e\u00e9])\\s+(?:de|d'|du|des|la)?\\s*([A-Z][A-Za-z\u00c0-\u00ff\\- ]{1,28})",
        ignore_case = TRUE
      )
    )[, 2]
    if (!is.na(m)) {
      m <- stringr::str_replace(m, "\\s*\\(.*$", "")
      m <- stringr::str_squish(m)
      if (is_valid_hz(m)) {
        found_hz   <- c(found_hz, m)
        found_rule <- c(found_rule, "zs_de_phrase")
      }
    }

    # ------ RULE 3: "répartis dans les ZS de A (n), B (n)" ------
    if (stringr::str_detect(txt_low, "zones?\\s+de\\s+sant[e\u00e9]|\\bzs\\b")) {
      m_all <- stringr::str_match_all(
        txt,
        "([A-Z][A-Za-z\u00c0-\u00ff\\-]{2,20})\\s*\\(\\d+"
      )[[1]][, 2]
      m_all <- m_all[!is.na(m_all)]
      m_all <- m_all[vapply(m_all, is_valid_hz, logical(1))]
      if (length(m_all) > 0) {
        found_hz   <- c(found_hz, m_all)
        found_rule <- c(found_rule, rep("hz_with_count_pattern", length(m_all)))
      }
    }

    # ------ RULE 4: Comma list after "ZS touchées (n) PROV: A, B, C" ------
    if (stringr::str_detect(
      txt_low,
      "zones?\\s+de\\s+sant[e\u00e9]\\s+touch"
    )) {
      # Remove province labels
      cleaned <- stringr::str_replace_all(
        txt,
        stringr::regex("[A-Z][A-Z\\-]+\\s*:", ignore_case = FALSE),
        ","
      )
      parts <- unlist(stringr::str_split(cleaned, "[,;\\|\\n]|\\bet\\b"))
      parts <- stringr::str_replace_all(parts, "\\s*\\(\\d+\\).*$", "")
      parts <- stringr::str_replace_all(parts, "\\s*\\d+.*$", "")
      parts <- stringr::str_squish(parts)
      parts <- parts[vapply(parts, is_valid_hz, logical(1))]
      if (length(parts) > 0) {
        found_hz   <- c(found_hz, parts)
        found_rule <- c(found_rule, rep("hz_list_header", length(parts)))
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

  results <- list()

  for (i in seq_len(nrow(line_table))) {
    txt  <- line_table$line_text[i]
    low  <- normalize_text(stringr::str_to_lower(txt))
    sno  <- line_table$sitrep_no[i]
    pg   <- line_table$page[i]
    ln   <- line_table$line_no[i]

    add <- function(code, val, rule) {
      if (!is.na(val) && is.finite(val)) {
        results[[length(results) + 1]] <<- tibble::tibble(
          sitrep_no       = sno,
          page            = pg,
          line_no         = ln,
          indicator_code  = code,
          value           = val,
          extraction_rule = rule,
          evidence_line   = txt
        )
      }
    }

    # ---- CAS CONFIRMÉS CUMULÉS ----
    val <- extract_num_before(
      txt, "cas\\s+confirm[e\u00e9]|confirmed\\s+cases?"
    )
    add("cumulative_confirmed_cases", val, "num_before_cas_confirmes")

    # "cumul... s'élève à N cas"
    if (stringr::str_detect(low, "cumul.*s.*el[e\u00e8]ve|cumul.*de.*cas")) {
      val <- extract_num_after(
        txt, "(?:s\\'[e\u00e9]l[e\u00e8]ve\\s+[a\u00e0]|cumul\\s+de)\\s*"
      )
      add("cumulative_confirmed_cases", val, "cumul_eleve_a")
    }

    # ---- NOUVEAUX CAS ----
    if (stringr::str_detect(
      low, "aucun nouveau cas|no new confirmed"
    )) add("new_confirmed_cases", 0, "explicit_zero_new_cases")

    val <- extract_num_before(
      txt, "nouveaux?\\s+cas\\s+confirm|new\\s+confirmed\\s+cases?"
    )
    add("new_confirmed_cases", val, "num_before_nouveaux_cas")

    # ---- DÉCÈS CUMULÉS ----
    val <- extract_num_after(txt, "dont\\s+")
    if (!is.na(val) && stringr::str_detect(low, "dec[e\u00e8]s|death|mortal")) {
      add("cumulative_deaths", val, "num_after_dont_deces")
    }

    val <- extract_num_before(
      txt, "d[e\u00e9]c[e\u00e8]s\\s+cum|cumul.*d[e\u00e9]c[e\u00e8]s"
    )
    add("cumulative_deaths", val, "num_before_cumul_deces")

    # ---- NOUVEAUX DÉCÈS ----
    if (stringr::str_detect(low, "aucun nouveau d[e\u00e9]c")) {
      add("new_deaths", 0, "explicit_zero_new_deaths")
    }
    val <- extract_num_before(
      txt,
      "nouveaux?\\s+d[e\u00e9]c[e\u00e8]s|d[e\u00e9]c[e\u00e8]s\\s+(?:ont|enregistr|rapport)"
    )
    add("new_deaths", val, "num_before_nouveaux_deces")

    # ---- CFR / LÉTALITÉ ----
    if (stringr::str_detect(low, "l[e\u00e9]talit[e\u00e9]|cfr|case\\s+fatality")) {
      val <- extract_num_after(
        txt,
        "l[e\u00e9]talit[e\u00e9]\\s*(?::|de|=)|cfr\\s*(?::|=)"
      )
      if (!is.na(val) && val <= 100) add("case_fatality_ratio", val, "num_after_letalite")
      if (is.na(val)) {
        pct <- safe_num(stringr::str_extract(txt, "\\d+(?:[\\.,]\\d+)?\\s*%"))
        if (!is.na(pct) && pct <= 100) add("case_fatality_ratio", pct, "pct_in_letalite_line")
      }
    }

    # ---- CAS SUSPECTS ----
    val <- extract_num_before(
      txt, "cas\\s+suspects?(?:\\s+en\\s+(?:cours|isolement))?|suspected\\s+cases?"
    )
    add("suspected_cases", val, "num_before_cas_suspects")

    # ---- CONTACTS LISTÉS / SUIVIS ----
    val <- extract_num_before(
      txt,
      "contacts?\\s+(?:[a\u00e0]\\s+suivre|list[e\u00e9]s?|identifi[e\u00e9]s?)"
    )
    add("contacts_listed", val, "num_before_contacts_listes")

    val <- extract_num_before(
      txt,
      "contacts?\\s+(?:ont\\s+[e\u00e9]t[e\u00e9]\\s+vus?|suivis?|vus?)"
    )
    add("contacts_followed", val, "num_before_contacts_suivis")

    # ---- TAUX SUIVI CONTACTS ----
    if (stringr::str_detect(low, "suivi|follow") &&
        stringr::str_detect(txt, "%")) {
      val <- extract_num_after(
        txt,
        "(?:taux|proportion)\\s+de\\s+suivi\\s+(?:de\\s+)?"
      )
      if (is.na(val)) {
        val <- safe_num(stringr::str_extract(txt, "\\d+(?:[\\.,]\\d+)?\\s*%"))
      }
      if (!is.na(val) && val <= 100) {
        add("contacts_followup_rate", val, "pct_suivi_contacts")
      }
    }

    # ---- ALERTES REÇUES ----
    val <- extract_num_before(
      txt,
      "alertes?\\s+(?:ont\\s+[e\u00e9]t[e\u00e9]\\s+)?(?:remontees?|remount[e\u00e9]es?|re[c\u00e7]ues?|report)"
    )
    add("alerts_reported", val, "num_before_alertes_remontees")

    # ---- ALERTES INVESTIGUÉES ----
    val <- extract_num_before(
      txt, "investig[u\u00fc][e\u00e9]es?|investigated"
    )
    add("alerts_investigated", val, "num_before_investiguees")

    if (stringr::str_detect(low, "investig") && stringr::str_detect(txt, "%")) {
      pct <- safe_num(stringr::str_extract(txt, "\\d+(?:[\\.,]\\d+)?\\s*%"))
      if (!is.na(pct) && pct <= 100) {
        add("alerts_investigation_rate", pct, "pct_investigation")
      }
    }

    # ---- ÉCHANTILLONS ----
    val <- extract_num_before(
      txt, "[e\u00e9]chantillons?\\s+(?:collect[e\u00e9]s?|analys[e\u00e9]s?|re[c\u00e7]us?)"
    )
    add("samples_collected", val, "num_before_echantillons")

    val <- extract_num_before(
      txt, "(?:sont\\s+)?(?:revenus?\\s+)?positifs?|positifs?\\s+(?:au|ebola|mve)"
    )
    add("samples_positive", val, "num_before_positifs")

    if (stringr::str_detect(low, "positivit") && stringr::str_detect(txt, "%")) {
      pct <- safe_num(stringr::str_extract(txt, "\\d+(?:[\\.,]\\d+)?\\s*%"))
      if (!is.na(pct) && pct <= 100) add("lab_positivity_rate", pct, "pct_positivite")
    }

    # ---- GUÉRIS ----
    val <- extract_num_before(
      txt, "gu[e\u00e9]ris?|recovered"
    )
    add("recovered", val, "num_before_gueris")

    # ---- PATIENTS EN ISOLEMENT ----
    val <- extract_num_before(
      txt, "(?:patients?|malades?)\\s+en\\s+isolement|en\\s+isolement"
    )
    add("patients_in_isolation", val, "num_before_isolement")

    # ---- VOYAGEURS SCREENÉS (PoE) ----
    if (stringr::str_detect(low, "scren[e\u00e9]|screen")) {
      val <- extract_num_before(txt, "scren[e\u00e9]|screen")
      add("travellers_screened", val, "num_before_screened")
    }

    # ---- ZONES DE SANTÉ TOUCHÉES (count) ----
    val <- extract_num_before(
      txt, "zones?\\s+de\\s+sant[e\u00e9]\\s+touch"
    )
    add("health_zones_affected_count", val, "num_before_zs_touchees")
  }

  if (length(results) == 0) return(tibble::tibble())

  dplyr::bind_rows(results) %>%
    dplyr::filter(!is.na(value), is.finite(value)) %>%
    dplyr::distinct(sitrep_no, indicator_code, value, .keep_all = TRUE)
}

# ============================================================
# ÉTAPE 6 — ANALYSE OPÉRATIONNELLE
# ============================================================

analyse_sitrep <- function(indicators_long, hz_mentions, sitrep_no) {

  cat("   Analysing SitRep", sitrep_no, "\n")

  get_val <- function(code) {
    v <- indicators_long %>%
      dplyr::filter(
        .data$sitrep_no == .env$sitrep_no,
        indicator_code == code
      ) %>%
      dplyr::slice(1) %>%
      dplyr::pull(value)
    if (length(v) == 0) NA_real_ else v
  }

  cumul_cases   <- get_val("cumulative_confirmed_cases")
  new_cases     <- get_val("new_confirmed_cases")
  cumul_deaths  <- get_val("cumulative_deaths")
  new_deaths    <- get_val("new_deaths")
  cfr           <- get_val("case_fatality_ratio")
  contacts_f    <- get_val("contacts_followed")
  followup_rate <- get_val("contacts_followup_rate")
  alerts_rep    <- get_val("alerts_reported")
  alerts_inv    <- get_val("alerts_investigated")
  inv_rate      <- get_val("alerts_investigation_rate")
  samples       <- get_val("samples_collected")
  positifs      <- get_val("samples_positive")
  positivity    <- get_val("lab_positivity_rate")

  hz_list <- hz_mentions %>%
    dplyr::filter(.data$sitrep_no == .env$sitrep_no) %>%
    dplyr::distinct(health_zone) %>%
    dplyr::pull(health_zone)

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
  registry <- load_registry()

  if (nrow(scraped) == 0) {
    cat("Impossible de scraper la page. Pipeline arrêté.\n")
    return(invisible(NULL))
  }

  # Update registry with all known SitReps
  new_rows <- detect_new_sitreps(scraped, registry)
  registry <- dplyr::bind_rows(registry, new_rows) %>%
    dplyr::distinct(pdf_url, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(sitrep_no))
  save_registry(registry)

  # Select SitReps to process
  to_process <- registry %>%
    dplyr::filter(!extracted | force_redownload) %>%
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

    cat("\n>> SitRep", sno, ":", basename(purl), "\n")

    # Download
    local_pdf <- download_sitrep_pdf(purl, sno)
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
    cat("   Health zones found:", dplyr::n_distinct(hz$health_zone), "\n")
    if (nrow(hz) > 0) all_hz[[i]] <- hz

    # Analyse
    if (nrow(indics) > 0) {
      analysis <- analyse_sitrep(indics, hz, sno)
      report   <- generate_report(analysis)
      all_reports[[i]] <- list(sitrep_no = sno, analysis = analysis, report = report)
      cat("   Report generated\n")
    }

    registry$analysed[registry$pdf_url == purl]     <- TRUE
    registry$last_updated[registry$pdf_url == purl] <- as.character(Sys.time())
  }

  save_registry(registry)

  cat("\n--- ÉTAPE 3: Consolidation & Export ---\n")

  # Combine all data
  all_lines_df      <- dplyr::bind_rows(all_lines)
  all_indicators_df <- dplyr::bind_rows(all_indicators)
  all_hz_df         <- dplyr::bind_rows(all_hz)

  # Save CSVs
  if (nrow(all_indicators_df) > 0) {
    indic_fp <- file.path(DATA_FINAL_DIR, "PREIS_indicators_long.csv")
    if (file.exists(indic_fp)) {
      existing <- readr::read_csv(indic_fp, show_col_types = FALSE)
      all_indicators_df <- dplyr::bind_rows(existing, all_indicators_df) %>%
        dplyr::distinct(sitrep_no, indicator_code, value, .keep_all = TRUE)
    }
    readr::write_csv(all_indicators_df, indic_fp)
    cat("   Indicators saved:", nrow(all_indicators_df), "rows\n")
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
    existing_log <- readr::read_csv(RUN_LOG_FP, show_col_types = FALSE)
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
    reports       = all_reports
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
