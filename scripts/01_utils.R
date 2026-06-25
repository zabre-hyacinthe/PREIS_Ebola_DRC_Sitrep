############################################################
# 01_utils.R — PREIS EBOLA DRC
############################################################

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

safe_num <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_replace_all(x, "(?<=\\d)[ ]+(?=\\d{3}(\\D|$))", "")
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_replace_all(x, "%", "")
  x <- stringr::str_replace_all(x, "[^0-9.-]", "")
  suppressWarnings(as.numeric(x))
}

normalize_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\u2018\u2019\u0027]", "'") %>%
    stringr::str_replace_all("[\u00e9\u00e8\u00ea\u00eb]", "e") %>%
    stringr::str_replace_all("[\u00c9\u00c8\u00ca\u00cb]", "E") %>%
    stringr::str_replace_all("[\u00e0\u00e2\u00e4]", "a") %>%
    stringr::str_replace_all("[\u00c0\u00c2\u00c4]", "A") %>%
    stringr::str_replace_all("[\u00ee\u00ef]", "i") %>%
    stringr::str_replace_all("[\u00ce\u00cf]", "I") %>%
    stringr::str_replace_all("[\u00f4\u00f6]", "o") %>%
    stringr::str_replace_all("[\u00d4\u00d6]", "O") %>%
    stringr::str_replace_all("[\u00f9\u00fb\u00fc]", "u") %>%
    stringr::str_replace_all("[\u00d9\u00db\u00dc]", "U") %>%
    stringr::str_replace_all("\u00e7", "c") %>%
    stringr::str_replace_all("\u00c7", "C") %>%
    stringr::str_squish()
}

extract_sitrep_no <- function(url_or_name) {
  if (is.na(url_or_name) || url_or_name == "") return(NA_integer_)
  src <- as.character(url_or_name)
  m_post <- stringr::str_match(src, "sitrep-n(\\d+)-mvb")[, 2]
  if (!is.na(m_post)) return(as.integer(m_post))
  fname <- basename(src)
  fname <- gsub("%C2%B0", "N", fname, ignore.case = TRUE)
  fname <- gsub("%[0-9A-Fa-f]{2}", "", fname)
  m <- stringr::str_match(fname, "(?:SITREP|SitRep|sitrep)[-_ ]?(?:MVE[-_ ]?|MVB[-_ ]?)?(?:NUM[-_ ]?|N[-_ ]?)?0*(\\d{1,3})")[, 2]
  if (!is.na(m)) return(as.integer(m))
  m <- stringr::str_match(fname, "N[o\u00b0\u00ba]?0*(\\d{1,3})(?:[-_\\.]|$)")[, 2]
  if (!is.na(m)) return(as.integer(m))
  m <- stringr::str_match(fname, "NUM[-_ ]?0*(\\d{1,3})")[, 2]
  if (!is.na(m)) return(as.integer(m))
  NA_integer_
}

get_with_retry <- function(url, timeout_sec = 90, max_try = 4, accept = "text/html,application/xhtml+xml,*/*") {
  for (att in seq_len(max_try)) {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::timeout(timeout_sec),
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "Accept" = accept,
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
      cat("      retry", att + 1, "of", max_try, "in 5s...\n")
      Sys.sleep(5)
    }
  }
  NULL
}

read_csv_if_exists <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) tibble::tibble())
}

safe_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path)
  invisible(path)
}

append_distinct_csv <- function(new_df, path, keys) {
  old <- read_csv_if_exists(path)
  out <- dplyr::bind_rows(old, new_df)
  if (nrow(out) > 0 && all(keys %in% names(out))) {
    out <- dplyr::distinct(out, dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
  }
  safe_write_csv(out, path)
  out
}
