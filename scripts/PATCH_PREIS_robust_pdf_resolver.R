############################################################
# PREIS EBOLA RDC
# PATCH ROBUSTE DU RESOLVEUR PDF INSP
#
# Objectif :
# - retrouver les PDF même si leur nom change ;
# - décoder pdfemb-data du plugin PDF Embedder ;
# - interroger l'API WordPress du post et du média ;
# - accepter les URL qui ne finissent pas par .pdf ;
# - valider le fichier uniquement par sa signature %PDF ;
# - limiter le nombre de requêtes et les délais ;
# - sauvegarder et restaurer automatiquement en cas d'échec.
#
# Ce patch n'envoie aucun email.
############################################################

project_dir <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"

target_file <- file.path(
  project_dir,
  "scripts",
  "preis_safe_scientific_email.R"
)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")

backup_file <- paste0(
  target_file,
  ".BACKUP_ROBUST_PDF_RESOLVER_",
  timestamp
)

if (!dir.exists(project_dir)) {
  stop("Dossier PREIS introuvable : ", project_dir)
}

if (!file.exists(target_file)) {
  stop("Script cible introuvable : ", target_file)
}

required_packages <- c(
  "httr",
  "xml2",
  "rvest",
  "stringr",
  "base64enc",
  "jsonlite"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Packages R manquants : ",
    paste(missing_packages, collapse = ", "),
    "\nInstalle-les avant de relancer le patch."
  )
}

if (!file.copy(target_file, backup_file, overwrite = FALSE)) {
  stop("Impossible de créer la sauvegarde : ", backup_file)
}

lines <- readLines(target_file, warn = FALSE, encoding = "UTF-8")

replace_between_functions <- function(
  x,
  function_name,
  next_function_name,
  replacement
) {
  start_pattern <- paste0(
    "^",
    function_name,
    "\\s*<-\\s*function"
  )

  end_pattern <- paste0(
    "^",
    next_function_name,
    "\\s*<-\\s*function"
  )

  start <- grep(start_pattern, x, perl = TRUE)
  end <- grep(end_pattern, x, perl = TRUE)

  if (length(start) != 1L) {
    stop(
      "Fonction cible introuvable ou ambiguë : ",
      function_name,
      " (occurrences : ",
      length(start),
      ")"
    )
  }

  end <- end[end > start]

  if (length(end) == 0L) {
    stop(
      "Fonction suivante introuvable après ",
      function_name,
      " : ",
      next_function_name
    )
  }

  end <- end[1L]

  c(
    x[seq_len(start - 1L)],
    replacement,
    x[end:length(x)]
  )
}

download_function <- c(
"preis_safe_download_pdf_from_url <- function(",
"  pdf_url,",
"  dest_file,",
"  referer = 'https://insp.cd/category/sitrep/',",
"  timeout_sec = 45",
") {",
"  if (is.null(pdf_url) || length(pdf_url) == 0 ||",
"      is.na(pdf_url[1]) || !nzchar(trimws(as.character(pdf_url[1])))) {",
"    return(NA_character_)",
"  }",
"",
"  urls <- preis_safe_url_variants(as.character(pdf_url[1]))",
"  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)",
"",
"  for (u in urls) {",
"    tmp <- tempfile(fileext = '.bin')",
"",
"    resp <- tryCatch(",
"      httr::GET(",
"        u,",
"        httr::timeout(timeout_sec),",
"        httr::write_disk(tmp, overwrite = TRUE),",
"        httr::add_headers(",
"          `User-Agent` = 'Mozilla/5.0 PREIS-Ebola-DRC-PDF-Download',",
"          Referer = referer,",
"          Accept = 'application/pdf,application/octet-stream,*/*'",
"        )",
"      ),",
"      error = function(e) NULL",
"    )",
"",
"    if (!is.null(resp)) {",
"      code <- httr::status_code(resp)",
"",
"      if (code >= 200 && code < 400 &&",
"          preis_safe_pdf_signature_ok(tmp)) {",
"        copied <- file.copy(tmp, dest_file, overwrite = TRUE)",
"        unlink(tmp, force = TRUE)",
"",
"        if (isTRUE(copied) && preis_safe_pdf_signature_ok(dest_file)) {",
"          return(normalizePath(dest_file, mustWork = TRUE))",
"        }",
"      }",
"    }",
"",
"    unlink(tmp, force = TRUE)",
"  }",
"",
"  NA_character_",
"}",
""
)

extract_function <- c(
"preis_safe_extract_pdf_urls_from_html <- function(html_text, page_url) {",
"  if (length(html_text) == 0 || is.na(html_text[1]) ||",
"      !nzchar(as.character(html_text[1]))) {",
"    return(character())",
"  }",
"",
"  html_text <- as.character(html_text[1])",
"  urls <- character()",
"",
"  raw_patterns <- c(",
"    'https?://[^\"\\'<>[:space:]]+[.]pdf(?:[?#][^\"\\'<>[:space:]]*)?',",
"    'https?:\\\\/\\\\/[^\"\\'<>[:space:]]+[.]pdf(?:[?#][^\"\\'<>[:space:]]*)?',",
"    '/wp-content/uploads/[^\"\\'<>[:space:]]+[.]pdf(?:[?#][^\"\\'<>[:space:]]*)?'",
"  )",
"",
"  for (pattern in raw_patterns) {",
"    found <- stringr::str_extract_all(html_text, pattern)[[1]]",
"    if (length(found) > 0) urls <- c(urls, found)",
"  }",
"",
"  doc <- tryCatch(rvest::read_html(html_text), error = function(e) NULL)",
"",
"  if (!is.null(doc)) {",
"    nodes <- rvest::html_elements(",
"      doc,",
"      'a, iframe, embed, object, source, link, meta, div'",
"    )",
"",
"    attributes <- c(",
"      'href', 'src', 'data', 'content', 'data-src',",
"      'data-pdf', 'data-url', 'data-file',",
"      'data-download', 'data-pdf-url',",
"      'pdfemb-url', 'pdfemb-data'",
"    )",
"",
"    for (attribute in attributes) {",
"      values <- rvest::html_attr(nodes, attribute)",
"      values <- values[!is.na(values) & nzchar(trimws(values))]",
"      urls <- c(urls, values)",
"    }",
"  }",
"",
"  b64_matches <- stringr::str_match_all(",
"    html_text,",
"    'pdfemb-data=[\"\\']?([A-Za-z0-9+/_=-]+)'",
"  )[[1]]",
"",
"  if (!is.null(b64_matches) && nrow(b64_matches) > 0) {",
"    for (b64_value in unique(b64_matches[, 2])) {",
"      b64_value <- trimws(b64_value)",
"      padding_needed <- (4L - nchar(b64_value) %% 4L) %% 4L",
"",
"      if (padding_needed > 0L) {",
"        b64_value <- paste0(",
"          b64_value,",
"          paste(rep('=', padding_needed), collapse = '')",
"        )",
"      }",
"",
"      decoded <- tryCatch(",
"        rawToChar(base64enc::base64decode(b64_value)),",
"        error = function(e) NA_character_",
"      )",
"",
"      if (is.na(decoded) || !nzchar(decoded)) next",
"",
"      payload <- tryCatch(",
"        jsonlite::fromJSON(decoded, simplifyVector = TRUE),",
"        error = function(e) NULL",
"      )",
"",
"      if (!is.null(payload)) {",
"        if (!is.null(payload$url) && length(payload$url) > 0) {",
"          urls <- c(urls, as.character(payload$url[1]))",
"        }",
"",
"        if (!is.null(payload$pdfID) && length(payload$pdfID) > 0) {",
"          media_id <- suppressWarnings(as.integer(payload$pdfID[1]))",
"",
"          if (!is.na(media_id)) {",
"            media_api <- paste0(",
"              'https://insp.cd/wp-json/wp/v2/media/',",
"              media_id",
"            )",
"",
"            media_text <- preis_safe_http_get_text(",
"              media_api,",
"              timeout_sec = 25",
"            )",
"",
"            if (!is.na(media_text) && nzchar(media_text)) {",
"              media_payload <- tryCatch(",
"                jsonlite::fromJSON(media_text),",
"                error = function(e) NULL",
"              )",
"",
"              if (!is.null(media_payload)) {",
"                if (!is.null(media_payload$source_url)) {",
"                  urls <- c(urls, media_payload$source_url)",
"                }",
"",
"                if (!is.null(media_payload$guid$rendered)) {",
"                  urls <- c(urls, media_payload$guid$rendered)",
"                }",
"              }",
"            }",
"          }",
"        }",
"      }",
"",
"      fallback_urls <- stringr::str_extract_all(",
"        decoded,",
"        'https?:\\\\\\\\?/\\\\\\\\?/[^\"\\'<>[:space:]]+[.]pdf(?:[?#][^\"\\'<>[:space:]]*)?'",
"      )[[1]]",
"",
"      if (length(fallback_urls) > 0) {",
"        fallback_urls <- gsub('\\\\\\\\/', '/', fallback_urls)",
"        urls <- c(urls, fallback_urls)",
"      }",
"    }",
"  }",
"",
"  urls <- gsub('\\\\\\\\/', '/', urls)",
"  urls <- gsub('&amp;', '&', urls, fixed = TRUE)",
"  urls <- utils::URLdecode(urls)",
"",
"  absolute <- vapply(",
"    urls,",
"    function(u) {",
"      tryCatch(",
"        xml2::url_absolute(u, page_url),",
"        error = function(e) NA_character_",
"      )",
"    },",
"    character(1)",
"  )",
"",
"  preis_safe_unique(absolute)",
"}",
""
)

resolver_function <- c(
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
"  candidates <- character()",
"",
"  if (!is.null(current_pdf_url) && length(current_pdf_url) > 0 &&",
"      !is.na(current_pdf_url[1]) &&",
"      nzchar(trimws(as.character(current_pdf_url[1])))) {",
"    candidates <- c(candidates, as.character(current_pdf_url[1]))",
"  }",
"",
"  html_text <- preis_safe_http_get_text(page_url, timeout_sec = 35)",
"",
"  if (!is.na(html_text) && nzchar(html_text)) {",
"    html_candidates <- preis_safe_extract_pdf_urls_from_html(",
"      html_text,",
"      page_url",
"    )",
"",
"    candidates <- c(candidates, html_candidates)",
"    resolver_log(",
"      'PDF resolver: HTML/embed candidates=',",
"      length(preis_safe_unique(html_candidates))",
"    )",
"",
"    post_ids <- character()",
"",
"    post_api_match <- stringr::str_match_all(",
"      html_text,",
"      'wp-json/wp/v2/posts/([0-9]+)'",
"    )[[1]]",
"",
"    if (!is.null(post_api_match) && nrow(post_api_match) > 0) {",
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
"        candidates <- c(",
"          candidates,",
"          preis_safe_extract_pdf_urls_from_html(",
"            rendered,",
"            page_url",
"          )",
"        )",
"      }",
"    }",
"  }",
"",
"  fallback_candidates <- preis_safe_generate_pdf_candidates_from_context(",
"    sitrep_no,",
"    page_url,",
"    title",
"  )",
"",
"  candidates <- c(candidates, head(fallback_candidates, 40))",
"  candidates <- preis_safe_unique(candidates)",
"",
"  bad_extensions <- paste0(",
"    '[.](jpg|jpeg|png|gif|webp|svg|css|js|woff|woff2|ttf|ico)',",
"    '($|[?#])'",
"  )",
"",
"  candidates <- candidates[",
"    grepl('^https?://', candidates, ignore.case = TRUE) &",
"      !grepl(bad_extensions, candidates, ignore.case = TRUE)",
"  ]",
"",
"  direct_pdf <- grepl('[.]pdf($|[?#])', candidates, ignore.case = TRUE)",
"  media_like <- grepl(",
"    'pdf|download|attachment|media|document|wp-content|wp-json',",
"    candidates,",
"    ignore.case = TRUE",
"  )",
"",
"  order_index <- order(",
"    as.integer(direct_pdf) * 2L + as.integer(media_like),",
"    decreasing = TRUE",
"  )",
"",
"  candidates <- candidates[order_index]",
"  candidates <- head(candidates, 30)",
"",
"  resolver_log(",
"    'PDF resolver: candidates to test=',",
"    length(candidates)",
"  )",
"",
"  if (length(candidates) == 0) return('')",
"",
"  tmp_dir <- file.path(getwd(), 'data', 'pdf_probe')",
"  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)",
"",
"  for (i in seq_along(candidates)) {",
"    candidate <- candidates[i]",
"",
"    resolver_log(",
"      'PDF resolver: testing ',",
"      i,",
"      '/',",
"      length(candidates),",
"      ' ',",
"      substr(candidate, 1, 140)",
"    )",
"",
"    probe_file <- tempfile(",
"      pattern = 'preis_pdf_probe_',",
"      fileext = '.bin',",
"      tmpdir = tmp_dir",
"    )",
"",
"    got <- preis_safe_download_pdf_from_url(",
"      candidate,",
"      probe_file,",
"      referer = page_url,",
"      timeout_sec = 30",
"    )",
"",
"    valid <- !is.na(got) && nzchar(got) &&",
"      file.exists(got) && preis_safe_pdf_signature_ok(got)",
"",
"    unlink(probe_file, force = TRUE)",
"",
"    if (valid) {",
"      resolver_log('PDF resolver: valid PDF found')",
"      return(candidate)",
"    }",
"  }",
"",
"  ''",
"}",
""
)

download_resolved_function <- c(
"preis_safe_download_resolved_pdf <- function(",
"  pdf_url,",
"  sitrep_no,",
"  page_url",
") {",
"  if (is.null(pdf_url) || length(pdf_url) == 0 ||",
"      is.na(pdf_url[1]) || !nzchar(trimws(as.character(pdf_url[1])))) {",
"    return(NA_character_)",
"  }",
"",
"  n_int <- suppressWarnings(as.integer(sitrep_no))",
"  if (is.na(n_int)) return(NA_character_)",
"",
"  pdf_dir <- file.path(getwd(), 'data', 'pdf')",
"  dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)",
"",
"  dest_file <- file.path(",
"    pdf_dir,",
"    paste0(",
"      'PREIS_DRC_Ebola_SitRep_',",
"      sprintf('%03d', n_int),",
"      '.pdf'",
"    )",
"  )",
"",
"  out <- preis_safe_download_pdf_from_url(",
"    as.character(pdf_url[1]),",
"    dest_file,",
"    referer = page_url,",
"    timeout_sec = 90",
"  )",
"",
"  if (!is.na(out) && nzchar(out) &&",
"      preis_safe_pdf_signature_ok(out)) {",
"    return(out)",
"  }",
"",
"  NA_character_",
"}",
""
)

lines <- replace_between_functions(
  lines,
  "preis_safe_download_pdf_from_url",
  "preis_safe_generate_pdf_candidates_from_context",
  download_function
)

lines <- replace_between_functions(
  lines,
  "preis_safe_extract_pdf_urls_from_html",
  "preis_safe_pdf_signature_ok",
  extract_function
)

lines <- replace_between_functions(
  lines,
  "preis_safe_resolve_pdf_universal",
  "preis_safe_download_resolved_pdf",
  resolver_function
)

lines <- replace_between_functions(
  lines,
  "preis_safe_download_resolved_pdf",
  "log_msg",
  download_resolved_function
)

temporary_file <- paste0(
  target_file,
  ".PATCH_TEMP_",
  timestamp
)

writeLines(lines, temporary_file, useBytes = TRUE)

parse_ok <- tryCatch(
  {
    parse(file = temporary_file)
    TRUE
  },
  error = function(e) {
    message("Erreur de syntaxe du script corrigé : ", conditionMessage(e))
    FALSE
  }
)

if (!parse_ok) {
  unlink(temporary_file, force = TRUE)
  stop(
    "Le patch n'a pas été installé. ",
    "Le fichier original est intact. Sauvegarde : ",
    backup_file
  )
}

patched_text <- paste(lines, collapse = "\n")

required_markers <- c(
  "pdfemb-data",
  "jsonlite::fromJSON",
  "wp-json/wp/v2/media/",
  "PDF resolver: candidates to test=",
  "timeout_sec = 30",
  "PREIS_DRC_Ebola_SitRep_"
)

missing_markers <- required_markers[
  !vapply(
    required_markers,
    function(marker) grepl(marker, patched_text, fixed = TRUE),
    logical(1)
  )
]

if (length(missing_markers) > 0) {
  unlink(temporary_file, force = TRUE)
  stop(
    "Validation fonctionnelle du patch échouée. Marqueurs absents : ",
    paste(missing_markers, collapse = ", ")
  )
}

if (!file.copy(temporary_file, target_file, overwrite = TRUE)) {
  unlink(temporary_file, force = TRUE)
  stop("Impossible d'installer le script corrigé.")
}

unlink(temporary_file, force = TRUE)

final_ok <- tryCatch(
  {
    parse(file = target_file)
    TRUE
  },
  error = function(e) FALSE
)

if (!final_ok) {
  file.copy(backup_file, target_file, overwrite = TRUE)
  stop(
    "La validation finale a échoué. ",
    "L'ancien script a été restauré."
  )
}

cat("\n============================================================\n")
cat("PATCH ROBUSTE INSTALLE AVEC SUCCES\n")
cat("============================================================\n")
cat("Script corrigé :", target_file, "\n")
cat("Sauvegarde      :", backup_file, "\n")
cat("Syntaxe R       : OK\n")
cat("Email envoyé    : NON\n")
cat("\nTest protégé recommandé :\n")
cat("set PREIS_FORCE_SEND=false\n")
cat("Rscript --vanilla scripts\\preis_safe_scientific_email.R\n")
cat("============================================================\n")
