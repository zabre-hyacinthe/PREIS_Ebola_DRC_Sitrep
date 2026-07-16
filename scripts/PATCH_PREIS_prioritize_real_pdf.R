############################################################
# PREIS EBOLA RDC
# PATCH CIBLE : PRIORISER LE VRAI PDF EXTRAIT DE PDFEMB-DATA
#
# Cause corrigée :
# - les candidats générés artificiellement passaient avant
#   le vrai PDF extrait de la page ;
# - head(..., 30) supprimait le vrai PDF avant le test.
#
# Ce patch :
# - sauvegarde le script actuel ;
# - remplace uniquement la fonction de résolution ;
# - teste d'abord les candidats extraits du HTML / pdfemb-data ;
# - n'utilise les noms générés qu'en dernier recours ;
# - n'envoie aucun email.
############################################################

project_dir <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"

target_file <- file.path(
  project_dir,
  "scripts",
  "preis_safe_scientific_email.R"
)

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S",
  tz = "UTC"
)

backup_file <- paste0(
  target_file,
  ".BACKUP_PRIORITY_REAL_PDF_",
  timestamp
)

if (!file.exists(target_file)) {
  stop("Script cible introuvable : ", target_file)
}

if (!file.copy(target_file, backup_file, overwrite = FALSE)) {
  stop("Impossible de créer la sauvegarde : ", backup_file)
}

lines <- readLines(
  target_file,
  warn = FALSE,
  encoding = "UTF-8"
)

start <- grep(
  "^preis_safe_resolve_pdf_universal\\s*<-\\s*function",
  lines,
  perl = TRUE
)

end <- grep(
  "^preis_safe_download_resolved_pdf\\s*<-\\s*function",
  lines,
  perl = TRUE
)

if (length(start) != 1L) {
  stop(
    "Fonction preis_safe_resolve_pdf_universal introuvable ou ambiguë."
  )
}

end <- end[end > start]

if (length(end) == 0L) {
  stop(
    "Fonction preis_safe_download_resolved_pdf introuvable après le résolveur."
  )
}

end <- end[1L]

replacement <- c(
"preis_safe_resolve_pdf_universal <- function(",
"  sitrep_no,",
"  page_url,",
"  current_pdf_url = '',",
"  title = ''",
") {",
"  resolver_log <- function(...) {",
"    if (exists('log_msg', mode = 'function')) {",
"      log_msg(...)",
"    } else {",
"      cat(paste0(...), '\\n')",
"    }",
"  }",
"",
"  direct_candidates <- character()",
"  html_candidates <- character()",
"  api_candidates <- character()",
"  fallback_candidates <- character()",
"",
"  if (!is.null(current_pdf_url) &&",
"      length(current_pdf_url) > 0 &&",
"      !is.na(current_pdf_url[1]) &&",
"      nzchar(trimws(as.character(current_pdf_url[1])))) {",
"    direct_candidates <- c(",
"      direct_candidates,",
"      as.character(current_pdf_url[1])",
"    )",
"  }",
"",
"  html_text <- preis_safe_http_get_text(",
"    page_url,",
"    timeout_sec = 35",
"  )",
"",
"  if (!is.na(html_text) && nzchar(html_text)) {",
"    html_candidates <- preis_safe_extract_pdf_urls_from_html(",
"      html_text,",
"      page_url",
"    )",
"",
"    resolver_log(",
"      'PDF resolver: HTML/embed candidates=',",
"      length(preis_safe_unique(html_candidates))",
"    )",
"",
"    # Extract WordPress post IDs only to obtain additional real URLs.",
"    post_ids <- character()",
"",
"    post_api_match <- stringr::str_match_all(",
"      html_text,",
"      'wp-json/wp/v2/posts/([0-9]+)'",
"    )[[1]]",
"",
"    if (!is.null(post_api_match) &&",
"        nrow(post_api_match) > 0) {",
"      post_ids <- c(post_ids, post_api_match[, 2])",
"    }",
"",
"    body_match <- stringr::str_match_all(",
"      html_text,",
"      'postid-([0-9]+)'",
"    )[[1]]",
"",
"    if (!is.null(body_match) && nrow(body_match) > 0) {",
"      post_ids <- c(post_ids, body_match[, 2])",
"    }",
"",
"    post_ids <- unique(post_ids[nzchar(post_ids)])",
"",
"    for (post_id in head(post_ids, 3)) {",
"      post_api <- paste0(",
"        'https://insp.cd/wp-json/wp/v2/posts/',",
"        post_id",
"      )",
"",
"      post_text <- preis_safe_http_get_text(",
"        post_api,",
"        timeout_sec = 25",
"      )",
"",
"      if (is.na(post_text) || !nzchar(post_text)) next",
"",
"      post_payload <- tryCatch(",
"        jsonlite::fromJSON(post_text),",
"        error = function(e) NULL",
"      )",
"",
"      if (is.null(post_payload)) next",
"",
"      rendered_fields <- character()",
"",
"      if (!is.null(post_payload$content$rendered)) {",
"        rendered_fields <- c(",
"          rendered_fields,",
"          post_payload$content$rendered",
"        )",
"      }",
"",
"      if (!is.null(post_payload$excerpt$rendered)) {",
"        rendered_fields <- c(",
"          rendered_fields,",
"          post_payload$excerpt$rendered",
"        )",
"      }",
"",
"      for (rendered in rendered_fields) {",
"        api_candidates <- c(",
"          api_candidates,",
"          preis_safe_extract_pdf_urls_from_html(",
"            rendered,",
"            page_url",
"          )",
"        )",
"      }",
"    }",
"  }",
"",
"  # Generated filenames are now strictly last-resort candidates.",
"  fallback_candidates <- preis_safe_generate_pdf_candidates_from_context(",
"    sitrep_no,",
"    page_url,",
"    title",
"  )",
"",
"  clean_candidates <- function(x) {",
"    x <- preis_safe_unique(x)",
"",
"    x <- x[",
"      grepl('^https?://', x, ignore.case = TRUE)",
"    ]",
"",
"    bad_extensions <- paste0(",
"      '[.](jpg|jpeg|png|gif|webp|svg|css|js|woff|woff2|ttf|ico)',",
"      '($|[?#])'",
"    )",
"",
"    x <- x[",
"      !grepl(bad_extensions, x, ignore.case = TRUE)",
"    ]",
"",
"    x",
"  }",
"",
"  direct_candidates <- clean_candidates(direct_candidates)",
"  html_candidates <- clean_candidates(html_candidates)",
"  api_candidates <- clean_candidates(api_candidates)",
"  fallback_candidates <- clean_candidates(fallback_candidates)",
"",
"  # Prioritize exact PDFs extracted from the page/embed payload.",
"  rank_real_candidates <- function(x) {",
"    if (length(x) == 0) return(x)",
"",
"    exact_pdf <- grepl(",
"      '[.]pdf($|[?#])',",
"      x,",
"      ignore.case = TRUE",
"    )",
"",
"    sitrep_match <- grepl(",
"      paste0('(?:^|[^0-9])0*', as.integer(sitrep_no), '(?:[^0-9]|$)'),",
"      utils::URLdecode(x),",
"      ignore.case = TRUE,",
"      perl = TRUE",
"    )",
"",
"    analytical_match <- grepl(",
"      'analytique|final|vf|sitrep|mve|mvb',",
"      utils::URLdecode(x),",
"      ignore.case = TRUE",
"    )",
"",
"    score <- as.integer(exact_pdf) * 100L +",
"      as.integer(sitrep_match) * 50L +",
"      as.integer(analytical_match) * 10L",
"",
"    x[order(score, decreasing = TRUE)]",
"  }",
"",
"  direct_candidates <- rank_real_candidates(direct_candidates)",
"  html_candidates <- rank_real_candidates(html_candidates)",
"  api_candidates <- rank_real_candidates(api_candidates)",
"",
"  # Keep real candidates first. Generated names are tested only if needed.",
"  real_candidates <- preis_safe_unique(c(",
"    direct_candidates,",
"    html_candidates,",
"    api_candidates",
"  ))",
"",
"  fallback_candidates <- fallback_candidates[",
"    !fallback_candidates %in% real_candidates",
"  ]",
"",
"  real_candidates <- head(real_candidates, 40)",
"  fallback_candidates <- head(fallback_candidates, 12)",
"",
"  resolver_log(",
"    'PDF resolver: real candidates to test=',",
"    length(real_candidates)",
"  )",
"",
"  resolver_log(",
"    'PDF resolver: fallback candidates available=',",
"    length(fallback_candidates)",
"  )",
"",
"  test_candidates <- function(candidates, label) {",
"    if (length(candidates) == 0) return('')",
"",
"    tmp_dir <- file.path(getwd(), 'data', 'pdf_probe')",
"    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)",
"",
"    for (i in seq_along(candidates)) {",
"      candidate <- candidates[i]",
"",
"      resolver_log(",
"        'PDF resolver [',",
"        label,",
"        ']: testing ',",
"        i,",
"        '/',",
"        length(candidates),",
"        ' ',",
"        substr(utils::URLdecode(candidate), 1, 180)",
"      )",
"",
"      probe_file <- tempfile(",
"        pattern = 'preis_pdf_probe_',",
"        fileext = '.bin',",
"        tmpdir = tmp_dir",
"      )",
"",
"      got <- preis_safe_download_pdf_from_url(",
"        candidate,",
"        probe_file,",
"        referer = page_url,",
"        timeout_sec = 30",
"      )",
"",
"      valid <- !is.na(got) &&",
"        nzchar(got) &&",
"        file.exists(got) &&",
"        preis_safe_pdf_signature_ok(got)",
"",
"      unlink(probe_file, force = TRUE)",
"",
"      if (valid) {",
"        resolver_log(",
"          'PDF resolver: valid PDF found from ',",
"          label",
"        )",
"        return(candidate)",
"      }",
"    }",
"",
"    ''",
"  }",
"",
"  resolved <- test_candidates(",
"    real_candidates,",
"    'page/embed'",
"  )",
"",
"  if (nzchar(resolved)) return(resolved)",
"",
"  resolved <- test_candidates(",
"    fallback_candidates,",
"    'generated fallback'",
"  )",
"",
"  resolved",
"}",
""
)

new_lines <- c(
  lines[seq_len(start - 1L)],
  replacement,
  lines[end:length(lines)]
)

temporary_file <- paste0(
  target_file,
  ".TEMP_PRIORITY_REAL_PDF_",
  timestamp
)

writeLines(
  new_lines,
  temporary_file,
  useBytes = TRUE
)

parse_ok <- tryCatch(
  {
    parse(file = temporary_file)
    TRUE
  },
  error = function(e) {
    message(
      "Erreur de syntaxe du script corrigé : ",
      conditionMessage(e)
    )
    FALSE
  }
)

if (!parse_ok) {
  unlink(temporary_file, force = TRUE)

  stop(
    "Patch non installé. Le script original reste intact."
  )
}

new_text <- paste(new_lines, collapse = "\n")

required_markers <- c(
  "PDF resolver: real candidates to test=",
  "PDF resolver [",
  "page/embed",
  "generated fallback",
  "real_candidates <- head(real_candidates, 40)"
)

missing_markers <- required_markers[
  !vapply(
    required_markers,
    function(marker) {
      grepl(marker, new_text, fixed = TRUE)
    },
    logical(1)
  )
]

if (length(missing_markers) > 0) {
  unlink(temporary_file, force = TRUE)

  stop(
    "Validation du patch échouée. Marqueurs absents : ",
    paste(missing_markers, collapse = ", ")
  )
}

installed <- file.copy(
  temporary_file,
  target_file,
  overwrite = TRUE
)

unlink(temporary_file, force = TRUE)

if (!isTRUE(installed)) {
  stop("Impossible d'installer le patch.")
}

final_ok <- tryCatch(
  {
    parse(file = target_file)
    TRUE
  },
  error = function(e) FALSE
)

if (!final_ok) {
  file.copy(
    backup_file,
    target_file,
    overwrite = TRUE
  )

  stop(
    "Validation finale échouée. ",
    "La sauvegarde a été restaurée."
  )
}

cat("\n============================================================\n")
cat("PATCH DE PRIORITE DU VRAI PDF INSTALLE\n")
cat("============================================================\n")
cat("Script corrigé :", target_file, "\n")
cat("Sauvegarde      :", backup_file, "\n")
cat("Syntaxe R       : OK\n")
cat("Email envoyé    : NON\n")
cat("\nLe vrai PDF extrait de pdfemb-data sera testé avant\n")
cat("les noms de fichiers générés artificiellement.\n")
cat("============================================================\n")
