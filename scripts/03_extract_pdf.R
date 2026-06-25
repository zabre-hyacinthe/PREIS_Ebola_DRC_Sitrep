############################################################
# 03_extract_pdf.R — PREIS EBOLA DRC
############################################################

download_sitrep_pdf <- function(pdf_url, sitrep_no, pdf_dir = PDF_DIR, force_redownload = FALSE, max_retries = 3) {
  fname <- paste0("SitRep_", sprintf("%02d", sitrep_no), "_2026.pdf")
  local_path <- file.path(pdf_dir, fname)

  if (!force_redownload && file.exists(local_path) && file.info(local_path)$size > 10240) {
    cat("   Already downloaded:", fname, "\n")
    return(local_path)
  }

  dl_url <- utils::URLencode(pdf_url, reserved = FALSE)
  dl_url <- gsub("%25C2%25B0", "%C2%B0", dl_url)
  dl_url <- gsub("%25", "%", dl_url)

  cat("   Downloading SitRep", sitrep_no, "->", fname, "\n")

  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(
      httr::GET(
        dl_url,
        httr::timeout(180),
        httr::write_disk(local_path, overwrite = TRUE),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "Referer" = INSP_CATEGORY_PAGE,
          "Accept" = "application/pdf,*/*"
        )
      ),
      error = function(e) {
        cat("   Attempt", attempt, "error:", conditionMessage(e), "\n")
        NULL
      }
    )

    if (!is.null(resp) && httr::status_code(resp) == 200 && file.exists(local_path)) {
      size_kb <- round(file.info(local_path)$size / 1024, 1)
      ct <- httr::headers(resp)[["content-type"]]
      is_pdf <- !is.null(ct) && grepl("pdf", ct, ignore.case = TRUE)
      if (size_kb >= 10 && (is_pdf || size_kb > 50)) {
        cat("   OK:", size_kb, "KB\n")
        return(local_path)
      }
    }

    if (attempt < max_retries) {
      cat("   Retry", attempt + 1, "of", max_retries, "in 8s...\n")
      Sys.sleep(8)
    }
  }

  cat("   Trying fallback download.file()...\n")
  ok <- tryCatch({
    old_opt <- options(timeout = 300)
    on.exit(options(old_opt), add = TRUE)
    utils::download.file(dl_url, local_path, mode = "wb", quiet = TRUE, method = "libcurl",
                         headers = c("User-Agent" = "Mozilla/5.0", "Referer" = INSP_CATEGORY_PAGE))
    file.exists(local_path) && file.info(local_path)$size > 10240
  }, error = function(e) FALSE)

  if (isTRUE(ok)) {
    cat("   OK fallback:", round(file.info(local_path)$size / 1024, 1), "KB\n")
    return(local_path)
  }

  if (file.exists(local_path)) file.remove(local_path)
  NA_character_
}

extract_pdf_text_lines <- function(local_pdf, sitrep_no) {
  cat("   Extracting text from PDF", sitrep_no, "\n")
  if (is.na(local_pdf) || !file.exists(local_pdf)) return(tibble::tibble())

  pages <- tryCatch(pdftools::pdf_text(local_pdf), error = function(e) {
    cat("   ERROR reading PDF:", conditionMessage(e), "\n")
    NULL
  })
  if (is.null(pages) || length(pages) == 0) return(tibble::tibble())

  line_table <- purrr::imap_dfr(pages, function(page_text, page_no) {
    lines <- stringr::str_split(page_text, "\n")[[1]]
    tibble::tibble(sitrep_no = sitrep_no, page = as.integer(page_no),
                   line_no = seq_along(lines), line_text = stringr::str_squish(lines))
  }) %>%
    dplyr::filter(!is.na(line_text), nchar(line_text) > 0)

  cat("   Extracted", nrow(line_table), "non-empty lines from", length(pages), "pages\n")
  line_table
}

has_tabulizer <- function() {
  requireNamespace("tabulizer", quietly = TRUE)
}

extract_pdf_table_rows <- function(local_pdf, sitrep_no, enable_tabulizer = TRUE) {
  if (!enable_tabulizer || !has_tabulizer()) {
    return(tibble::tibble())
  }

  cat("   Extracting tables with tabulizer for SitRep", sitrep_no, "\n")
  tabs <- tryCatch(
    tabulizer::extract_tables(local_pdf, guess = TRUE, output = "matrix"),
    error = function(e) {
      cat("   tabulizer skipped/error:", conditionMessage(e), "\n")
      list()
    }
  )

  if (length(tabs) == 0) return(tibble::tibble())

  out <- purrr::imap_dfr(tabs, function(tb, table_id) {
    if (is.null(tb) || length(tb) == 0) return(tibble::tibble())
    df <- as.data.frame(tb, stringsAsFactors = FALSE)
    names(df) <- paste0("col_", seq_len(ncol(df)))
    df$row_id <- seq_len(nrow(df))
    df$row_text <- apply(df[, grep("^col_", names(df)), drop = FALSE], 1, function(z) stringr::str_squish(paste(z, collapse = " ")))
    tibble::as_tibble(df) %>%
      dplyr::mutate(sitrep_no = sitrep_no, table_id = as.integer(table_id), .before = 1)
  }) %>%
    dplyr::filter(!is.na(row_text), nchar(row_text) > 0)

  if (nrow(out) > 0) {
    raw_fp <- file.path(TABLE_DIR, paste0("SitRep_", sprintf("%02d", sitrep_no), "_tables_raw.csv"))
    readr::write_csv(out, raw_fp)
    cat("   Table rows extracted:", nrow(out), "\n")
  }

  out
}

extract_pdf_bundle <- function(local_pdf, sitrep_no, enable_tabulizer = TRUE) {
  lines <- extract_pdf_text_lines(local_pdf, sitrep_no)
  tables <- extract_pdf_table_rows(local_pdf, sitrep_no, enable_tabulizer = enable_tabulizer)
  list(lines = lines, tables = tables)
}
