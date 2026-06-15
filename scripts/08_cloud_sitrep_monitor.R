# ============================================================
# PREIS PROJECT ROOT FIX — DO NOT REMOVE
# Ensures script works in RStudio and GitHub Actions
# ============================================================
get_preis_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- NA_character_

  hit <- grep(file_arg, args, fixed = TRUE)
  if (length(hit) > 0) {
    script_path <- sub(file_arg, "", args[hit[1]], fixed = TRUE)
  }

  if (is.na(script_path) || !nzchar(script_path)) {
    script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NA_character_)
  }

  if (!is.na(script_path) && nzchar(script_path)) {
    script_path <- normalizePath(script_path, winslash = "/", mustWork = FALSE)
    root <- dirname(dirname(script_path))
    if (file.exists(file.path(root, "00_RUN_ALL_PRODUCTION.R")) || dir.exists(file.path(root, "scripts"))) {
      return(normalizePath(root, winslash = "/", mustWork = FALSE))
    }
  }

  known_roots <- c(
    "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26",
    "D:/PREIS_Ebola_Production",
    "D:/PREIS_Ebola_DRC_Sitrep"
  )

  known_roots <- normalizePath(known_roots, winslash = "/", mustWork = FALSE)
  ok <- known_roots[file.exists(file.path(known_roots, "00_RUN_ALL_PRODUCTION.R"))]
  if (length(ok) > 0) return(ok[1])

  getwd()
}

PREIS_PROJECT_ROOT <- get_preis_project_root()
setwd(PREIS_PROJECT_ROOT)
message("PREIS project root: ", PREIS_PROJECT_ROOT)
# ============================================================


# ============================================================
# UTF-8 SAFE HELPERS — DO NOT REMOVE
# Prevents Rscript crash on invalid UTF-8 strings from INSP pages
# ============================================================
safe_utf8 <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x[is.na(x)] <- ""
  y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = ""))
  y[is.na(y)] <- ""
  Encoding(y) <- "UTF-8"
  y
}

html_unescape_basic <- function(x) {
  x <- safe_utf8(x)
  x <- gsub("&amp;", "&", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("&#034;", "\"", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("&#039;", "'", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE, useBytes = TRUE)
  safe_utf8(x)
}

# ============================================================
# PREIS EBOLA DRC — CLOUD SITREP MONITOR FOR GITHUB ACTIONS
# Runs every 5 minutes via .github/workflows/preis_sitrep_monitor.yml
# Purpose:
# 1) Run existing PREIS production pipeline when available
# 2) Find latest valid SitRep PDF
# 3) Read recipients from alert_recipients.csv at repository root
# 4) Send latest PDF by Gmail SMTP/blastula if not already sent
# 5) Persist sent-state in data/monitor_state/preis_sitrep_email_state.csv
# ============================================================

options(warn = 1)

required_packages <- c("blastula", "readr", "rvest", "xml2", "httr2", "stringr", "dplyr", "tibble", "purrr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(blastula)
  library(readr)
  library(rvest)
  library(xml2)
  library(httr2)
  library(stringr)
  library(dplyr)
  library(tibble)
  library(purrr)
})

ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Module d'identité du SitRep : SEULE source de vérité pour reconnaître
# un SitRep (web ou fichier). Garantit qu'on ne ramasse jamais un visa,
# une facture, etc. Voir scripts/10_sitrep_identity.R.
.identity_fp <- file.path(ROOT_DIR, "scripts", "10_sitrep_identity.R")
if (file.exists(.identity_fp)) {
  source(.identity_fp)
} else {
  stop("Module d'identité introuvable : ", .identity_fp,
       "\nIl est requis pour identifier les SitReps de façon fiable.")
}

DIR_PDF <- file.path(ROOT_DIR, "data", "pdf")
DIR_INCOMING <- file.path(ROOT_DIR, "data", "incoming", "insp_sitreps")
DIR_STATE <- file.path(ROOT_DIR, "data", "monitor_state")
DIR_LOG <- file.path(ROOT_DIR, "logs")

dir.create(DIR_PDF, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_INCOMING, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_STATE, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_LOG, recursive = TRUE, showWarnings = FALSE)

STATE_FILE <- file.path(DIR_STATE, "preis_sitrep_email_state.csv")
# Liste des destinataires : source unique = data/alert_recipients.csv
# (même fichier que celui lu par 04_send_sitrep_alerts_conditional.R).
# Repli sur la racine du repo pour compatibilité avec d'anciennes installs.
RECIPIENTS_FILE <- local({
  primary <- file.path(ROOT_DIR, "data", "alert_recipients.csv")
  legacy  <- file.path(ROOT_DIR, "alert_recipients.csv")
  if (file.exists(primary)) primary
  else if (file.exists(legacy)) legacy
  else primary   # défaut : on créera le fichier dans data/
})
LOG_FILE <- file.path(DIR_LOG, paste0("preis_cloud_sitrep_monitor_", format(Sys.Date(), "%Y%m%d"), ".log"))

CATEGORY_URLS <- c(
  "https://insp.cd/category/sitrep/",
  "https://insp.cd/category/sitrep/page/2/",
  "https://insp.cd/category/sitrep/page/3/"
)

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG_FILE, append = TRUE)
}

safe_chr <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return("")
  as.character(x[1])
}

is_truthy <- function(x) {
  tolower(trimws(as.character(x))) %in% c("true", "1", "yes", "y", "oui", "active")
}

is_valid_pdf_file <- function(path) {
  if (length(path) == 0 || is.na(path[1])) return(FALSE)
  if (!file.exists(path)) return(FALSE)
  size <- file.info(path)$size
  if (is.na(size) || size < 10000) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  header <- readBin(con, what = "raw", n = 5)
  identical(header, charToRaw("%PDF-"))
}

extract_sitrep_no <- function(x) {
  # Délègue au module d'identité (strict : exige sitrep + mvb/mve/ebola,
  # numéro borné 1-60). Ne se trompe plus jamais sur un titre parasite.
  sitrep_no_from_web(safe_chr(x))
}

absolute_url_one <- function(x, base = "https://insp.cd") {
  x <- safe_chr(x)
  if (x == "") return(NA_character_)
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
  if (startsWith(x, "//")) return(paste0("https:", x))
  if (startsWith(x, "/")) return(paste0(base, x))
  paste0(base, "/", x)
}

fetch_html_safely <- function(url, max_try = 4) {
  ua <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) ",
               "Chrome/126.0.0.0 Safari/537.36")
  for (att in seq_len(max_try)) {
    out <- tryCatch({
      req <- httr2::request(url)
      req <- httr2::req_user_agent(req, ua)
      req <- httr2::req_headers(req,
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8",
        "Referer" = "https://insp.cd/")
      req <- httr2::req_timeout(req, 60)
      resp <- httr2::req_perform(req)
      if (httr2::resp_status(resp) >= 400) stop("HTTP ", httr2::resp_status(resp))
      httr2::resp_body_string(resp)
    }, error = function(e) {
      log_msg("fetch attempt", att, "error:", url, "|", conditionMessage(e))
      NA_character_
    })
    if (!is.na(out) && nzchar(out)) return(out)
    if (att < max_try) Sys.sleep(6)
  }
  NA_character_
}

get_sitrep_posts <- function() {
  posts <- list()

  for (url in CATEGORY_URLS) {
    log_msg("Reading category:", url)
    html_txt <- fetch_html_safely(url)
    if (is.na(html_txt)) next

    page <- tryCatch(xml2::read_html(html_txt), error = function(e) NULL)
    if (is.null(page)) next

    nodes <- rvest::html_elements(page, "a")
    titles <- rvest::html_text2(nodes)
    hrefs <- rvest::html_attr(nodes, "href")
    if (length(titles) == 0 || length(hrefs) == 0) next

    df <- data.frame(title = trimws(titles), href = hrefs, stringsAsFactors = FALSE)
    df$href <- vapply(df$href, absolute_url_one, character(1))

    keep <- !is.na(df$href) & (
      grepl("/sitrep-", df$href, ignore.case = TRUE) |
        grepl("sitrep", df$title, ignore.case = TRUE) |
        grepl("sitrep", df$href, ignore.case = TRUE)
    )
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) next

    df$sitrep_no <- vapply(paste(df$title, df$href), extract_sitrep_no, integer(1))
    df <- df[!is.na(df$sitrep_no), , drop = FALSE]
    if (nrow(df) == 0) next

    posts[[length(posts) + 1]] <- data.frame(
      sitrep_no = as.integer(df$sitrep_no),
      title = df$title,
      post_url = df$href,
      stringsAsFactors = FALSE
    )
  }

  out <- if (length(posts) == 0) {
    data.frame(sitrep_no = integer(), title = character(),
               post_url = character(), stringsAsFactors = FALSE)
  } else {
    o <- do.call(rbind, posts)
    o <- o[!duplicated(paste(o$sitrep_no, o$post_url)), , drop = FALSE]
    o[order(o$sitrep_no, decreasing = TRUE), , drop = FALSE]
  }

  # ----------------------------------------------------------
  # FALLBACK par URL directe : l'INSP oublie parfois de ranger
  # un SitRep dans la catégorie (ex. SitRep 30 publié mais absent
  # de /category/sitrep/). On teste alors directement les URLs des
  # numéros suivants à partir du plus élevé connu.
  # ----------------------------------------------------------
  highest_known <- if (nrow(out) > 0) max(out$sitrep_no, na.rm = TRUE) else 0
  # On lit aussi le dernier numéro déjà traité dans l'état, pour
  # repartir du bon endroit même si la catégorie est très en retard.
  state_max <- tryCatch({
    st <- read_state()
    if (nrow(st) > 0) max(as.integer(st$sitrep_no), na.rm = TRUE) else 0
  }, error = function(e) 0)

  # PLANCHER de sécurité : l'épidémie 2026 est déjà bien avancée. On ne
  # sonde JAMAIS en partant de zéro (sinon on teste les SitReps 1, 2, 3
  # qui n'existent plus en ligne). Ce plancher est ajusté si besoin ;
  # il garantit que le fallback cherche les numéros plausibles actuels.
  KNOWN_FLOOR <- as.integer(Sys.getenv("PREIS_SITREP_FLOOR", "29"))
  base_no <- max(highest_known, state_max, KNOWN_FLOOR, na.rm = TRUE)

  # Si la catégorie a fonctionné ET donné un maximum >= plancher, on ne
  # sonde que si rien de neuf n'a été vu (évite des requêtes inutiles).
  extra <- probe_direct_sitreps(base_no, lookahead = 3)
  if (nrow(extra) > 0) {
    log_msg("Fallback URL directe : trouve", nrow(extra),
            "SitRep(s) absent(s) de la categorie :",
            paste(extra$sitrep_no, collapse = ", "))
    out <- rbind(out, extra)
    out <- out[!duplicated(out$sitrep_no), , drop = FALSE]
    out <- out[order(out$sitrep_no, decreasing = TRUE), , drop = FALSE]
  }

  # Garde de plausibilité : on ne retient que des numéros de SitRep dans
  # une fourchette réaliste (évite tout parasite type "SitRep 3" ou "752").
  # Bornes ajustables via variables d'environnement.
  SNO_MIN <- as.integer(Sys.getenv("PREIS_SITREP_MIN", "20"))
  SNO_MAX <- as.integer(Sys.getenv("PREIS_SITREP_MAX", "80"))
  if (nrow(out) > 0) {
    out <- out[!is.na(out$sitrep_no) &
               out$sitrep_no >= SNO_MIN & out$sitrep_no <= SNO_MAX, , drop = FALSE]
  }

  rownames(out) <- NULL
  out
}

# Teste directement les URLs des SitReps base_no+1 .. base_no+lookahead.
# Validation STRICTE : une page ne compte comme SitRep que si elle contient
# la signature d'un PDF embarqué (pdfemb-data) OU un lien .pdf, ET que le
# numéro attendu apparaît dans son URL/titre. Évite les faux positifs sur
# les pages 404/menu qui contiennent le mot « sitrep ».
probe_direct_sitreps <- function(base_no, lookahead = 3) {
  found <- list()

  page_is_real_sitrep <- function(html_txt, n) {
    if (is.na(html_txt) || !nzchar(html_txt)) return(FALSE)
    # 1) doit contenir un PDF embarqué ou un lien PDF
    has_pdf <- grepl("pdfemb-data=", html_txt, ignore.case = TRUE) ||
               grepl("https?://[^\"']+?\\.pdf", html_txt, ignore.case = TRUE)
    if (!has_pdf) return(FALSE)
    # 2) le numéro attendu doit apparaître dans un contexte SitRep
    nn  <- sprintf("%d", n); nnn <- sprintf("%03d", n)
    pat <- sprintf("sitrep[^0-9]{0,4}n?%s\\b|sitrep[^0-9]{0,4}n?%s\\b|N\u00b0\\s*%s\\b|N\u00b0\\s*%s\\b",
                   nn, nnn, nn, nnn)
    grepl(pat, html_txt, ignore.case = TRUE)
  }

  for (n in (base_no + 1):(base_no + lookahead)) {
    nn  <- sprintf("%d", n)
    nnn <- sprintf("%03d", n)
    hit_url <- NA_character_; hit_title <- NA_character_

    # 1) formes sans date (slug -mve-bundibugyo)
    direct_try <- c(
      sprintf("https://insp.cd/sitrep-n%s-mve-bundibugyo/", nnn),
      sprintf("https://insp.cd/sitrep-n%s-mve-bundibugyo/", nn)
    )
    for (u in direct_try) {
      h <- fetch_html_safely(u, max_try = 1)
      if (page_is_real_sitrep(h, n)) {
        hit_url <- u; hit_title <- paste0("SitRep N\u00b0", n); break
      }
    }

    # 2) formes avec date : estimation autour de la date attendue (+/-4 j)
    if (is.na(hit_url)) {
      anchor_no <- 28L
      anchor_date <- as.Date("2026-06-11")
      est_date <- anchor_date + (n - anchor_no)
      try_dates <- est_date + (-4:4)
      for (di in seq_along(try_dates)) {
        d <- as.Date(try_dates[di], origin = "1970-01-01")
        jj <- format(d, "%d"); mm <- format(d, "%m"); yy <- format(d, "%Y")
        for (typ in c("mvb", "mve")) {
          u <- sprintf("https://insp.cd/sitrep-n%s-%s_%s-%s-%s/",
                       nn, typ, jj, mm, yy)
          h <- fetch_html_safely(u, max_try = 1)
          if (page_is_real_sitrep(h, n)) {
            hit_url <- u; hit_title <- paste0("SitRep N\u00b0", n); break
          }
        }
        if (!is.na(hit_url)) break
      }
    }

    if (!is.na(hit_url)) {
      found[[length(found) + 1]] <- data.frame(
        sitrep_no = as.integer(n), title = hit_title,
        post_url = hit_url, stringsAsFactors = FALSE)
    } else {
      # N+1 introuvable -> inutile de tester N+2, N+3 (numéros consécutifs).
      break
    }
  }
  if (length(found) == 0)
    return(data.frame(sitrep_no = integer(), title = character(),
                      post_url = character(), stringsAsFactors = FALSE))
  do.call(rbind, found)
}

download_sitrep_pdf_direct <- function(sitrep_no, post_url) {
  # Télécharge le PDF d'un SitRep directement depuis sa page INSP, sans
  # dépendre du pipeline maître. Gère l'URL encodée en base64 (pdfemb-data)
  # et les liens .pdf directs. Robuste au blocage anti-bot (UA navigateur).
  if (is.na(post_url) || !nzchar(post_url)) {
    log_msg("download direct: post_url manquant.")
    return(NA_character_)
  }
  html_txt <- fetch_html_safely(post_url, max_try = 3)
  if (is.na(html_txt)) {
    log_msg("download direct: page inaccessible:", post_url)
    return(NA_character_)
  }

  pdf_url <- NA_character_
  # 1) URL encodée base64 dans pdfemb-data="..."
  m <- regmatches(html_txt, regexpr("pdfemb-data=[\"'][^\"']+[\"']", html_txt))
  if (length(m) == 1) {
    b64 <- sub("pdfemb-data=[\"']([^\"']+)[\"']", "\\1", m)
    decoded <- tryCatch(
      rawToChar(base64enc::base64decode(b64)),
      error = function(e) NA_character_)
    if (!is.na(decoded)) {
      um <- regmatches(decoded, regexpr("https?:[^\"]+?\\.pdf", decoded))
      if (length(um) == 1) pdf_url <- gsub("\\\\/", "/", um)
    }
  }
  # 2) Lien .pdf direct dans la page
  if (is.na(pdf_url)) {
    um <- regmatches(html_txt, regexpr("https?://[^\"']+?\\.pdf", html_txt))
    if (length(um) == 1) pdf_url <- um
  }
  if (is.na(pdf_url)) {
    log_msg("download direct: aucune URL PDF trouvée dans la page.")
    return(NA_character_)
  }

  dir.create(DIR_PDF, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(DIR_PDF, sitrep_canonical_filename(sitrep_no))
  ua <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) ",
               "Chrome/126.0.0.0 Safari/537.36")
  ok <- tryCatch({
    req <- httr2::request(pdf_url)
    req <- httr2::req_user_agent(req, ua)
    req <- httr2::req_headers(req, "Referer" = post_url)
    req <- httr2::req_timeout(req, 120)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 400) stop("HTTP ", httr2::resp_status(resp))
    writeBin(httr2::resp_body_raw(resp), dest)
    TRUE
  }, error = function(e) {
    log_msg("download direct: échec téléchargement:", conditionMessage(e)); FALSE
  })

  if (ok && is_valid_pdf_file(dest)) {
    log_msg("download direct: PDF récupéré ->", basename(dest))
    return(dest)
  }
  NA_character_
}

find_latest_local_pdf <- function(sitrep_no) {
  # On cherche le PDF du SitRep demandé UNIQUEMENT dans les dossiers projet.
  # 1) D'abord le nom canonique exact SitRep_NN_2026.pdf.
  # 2) Sinon, tout PDF dont le module d'identité confirme le bon numéro.
  roots <- unique(c(DIR_PDF, DIR_INCOMING))
  roots <- roots[dir.exists(roots)]
  if (length(roots) == 0) return(NA_character_)

  # 1) Nom canonique
  canon <- sitrep_canonical_filename(sitrep_no)   # "SitRep_NN_2026.pdf"
  for (r in roots) {
    fp <- file.path(r, canon)
    if (file.exists(fp) && is_valid_pdf_file(fp)) return(fp)
  }

  # 2) Recherche par identité stricte (non récursif, nom doit contenir 'sitrep')
  all_pdfs <- unique(unlist(lapply(roots, function(root) {
    list.files(root, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  }), use.names = FALSE))
  if (length(all_pdfs) == 0) return(NA_character_)

  matched <- all_pdfs[vapply(all_pdfs, function(p) {
    identical(sitrep_no_from_filename(p), as.integer(sitrep_no))
  }, logical(1))]
  matched <- matched[vapply(matched, is_valid_pdf_file, logical(1))]
  if (length(matched) == 0) return(NA_character_)

  # Le plus récent si plusieurs
  matched[order(file.info(matched)$mtime, decreasing = TRUE)][1]
}

run_existing_pipeline <- function() {
  # Le pipeline de production peut s'appeler différemment selon les installs.
  # On cherche, dans l'ordre, plusieurs noms connus, dans scripts/ puis à la racine.
  candidate_names <- c(
    "00_PREIS_MASTER_AUTOMATION.R",   # nom actuel (pipeline maître)
    "00_RUN_ALL_PRODUCTION.R"         # ancien nom (compat)
  )
  search_dirs <- c(file.path(ROOT_DIR, "scripts"), ROOT_DIR)
  pipeline_file <- NA_character_
  for (d in search_dirs) {
    for (nm in candidate_names) {
      fp <- file.path(d, nm)
      if (file.exists(fp)) { pipeline_file <- fp; break }
    }
    if (!is.na(pipeline_file)) break
  }

  if (is.na(pipeline_file)) {
    log_msg("Production pipeline not found in scripts/ or root; tried:",
            paste(candidate_names, collapse = ", "))
    return(FALSE)
  }

  log_msg("Running production pipeline:", pipeline_file)
  tryCatch({
    source(pipeline_file, local = globalenv())
    TRUE
  }, error = function(e) {
    log_msg("WARNING pipeline error:", conditionMessage(e))
    FALSE
  })
}

read_recipients <- function() {
  if (!file.exists(RECIPIENTS_FILE)) {
    dir.create(dirname(RECIPIENTS_FILE), recursive = TRUE, showWarnings = FALSE)
    default <- data.frame(
      active = TRUE,
      type = "to",
      name = "Dr Zabre",
      email = Sys.getenv("ALERT_TO", "raogoz@africacdc.org"),
      stringsAsFactors = FALSE
    )
    readr::write_csv(default, RECIPIENTS_FILE)
  }

  rec <- as.data.frame(readr::read_csv(RECIPIENTS_FILE, show_col_types = FALSE), stringsAsFactors = FALSE)
  names(rec) <- tolower(names(rec))

  required <- c("active", "type", "email")
  missing <- setdiff(required, names(rec))
  if (length(missing) > 0) stop("Missing columns in alert_recipients.csv: ", paste(missing, collapse = ", "))

  rec <- rec[is_truthy(rec$active), , drop = FALSE]
  rec$email <- trimws(as.character(rec$email))
  rec$type <- tolower(trimws(as.character(rec$type)))
  rec <- rec[nzchar(rec$email), , drop = FALSE]

  list(
    to = unique(rec$email[rec$type == "to"]),
    cc = unique(rec$email[rec$type == "cc"]),
    bcc = unique(rec$email[rec$type == "bcc"])
  )
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) {
    return(data.frame(
      sitrep_no = integer(),
      title = character(),
      post_url = character(),
      pdf_path = character(),
      status = character(),
      sent_at = character(),
      stringsAsFactors = FALSE
    ))
  }

  x <- as.data.frame(readr::read_csv(STATE_FILE, show_col_types = FALSE), stringsAsFactors = FALSE)
  if (!"sitrep_no" %in% names(x)) x$sitrep_no <- integer()
  x$sitrep_no <- as.integer(x$sitrep_no)
  x
}

write_state <- function(x) {
  dir.create(dirname(STATE_FILE), recursive = TRUE, showWarnings = FALSE)
  x$sent_at <- as.character(x$sent_at)
  x <- x[order(x$sitrep_no, decreasing = TRUE), , drop = FALSE]
  x <- x[!duplicated(paste(x$sitrep_no, x$status)), , drop = FALSE]
  readr::write_csv(x, STATE_FILE)
}

send_email_python <- function(smtp_host, smtp_port, smtp_user, smtp_pass,
                              from_addr, to, cc = character(), bcc = character(),
                              subject, body, attachment = NULL) {
  # Génère un script Python autonome qui envoie l'email via smtplib.
  # Beaucoup plus stable que blastula en CI (pas de segfault SSL).
  # Le mot de passe est passé par variable d'environnement (jamais en clair).
  to_csv  <- paste(to,  collapse = ",")
  cc_csv  <- paste(cc,  collapse = ",")
  bcc_csv <- paste(bcc, collapse = ",")
  att <- if (!is.null(attachment) && file.exists(attachment)) attachment else ""

  py <- '
import os, sys, smtplib, ssl
from email.message import EmailMessage

host = os.environ["PY_SMTP_HOST"]; port = int(os.environ["PY_SMTP_PORT"])
user = os.environ["PY_SMTP_USER"]; pwd = os.environ["PY_SMTP_PASS"]
frm  = os.environ["PY_FROM"]
to   = [x for x in os.environ.get("PY_TO","").split(",") if x]
cc   = [x for x in os.environ.get("PY_CC","").split(",") if x]
bcc  = [x for x in os.environ.get("PY_BCC","").split(",") if x]
subject = os.environ.get("PY_SUBJECT","")
body = os.environ.get("PY_BODY","")
att = os.environ.get("PY_ATT","")

msg = EmailMessage()
msg["From"] = frm; msg["To"] = ", ".join(to)
if cc: msg["Cc"] = ", ".join(cc)
msg["Subject"] = subject
msg.set_content(body)

if att and os.path.exists(att):
    with open(att, "rb") as f:
        data = f.read()
    msg.add_attachment(data, maintype="application", subtype="pdf",
                       filename=os.path.basename(att))

recipients = to + cc + bcc
try:
    if port == 465:
        ctx = ssl.create_default_context()
        with smtplib.SMTP_SSL(host, port, context=ctx, timeout=60) as s:
            s.login(user, pwd); s.send_message(msg, to_addrs=recipients)
    else:
        with smtplib.SMTP(host, port, timeout=60) as s:
            s.ehlo(); s.starttls(context=ssl.create_default_context()); s.ehlo()
            s.login(user, pwd); s.send_message(msg, to_addrs=recipients)
    print("PYEMAIL_OK")
except Exception as e:
    print("PYEMAIL_ERROR:", e, file=sys.stderr); sys.exit(1)
'
  tmp_py <- tempfile(fileext = ".py")
  writeLines(py, tmp_py)

  res <- withr_env(
    c(PY_SMTP_HOST = smtp_host, PY_SMTP_PORT = as.character(smtp_port),
      PY_SMTP_USER = smtp_user, PY_SMTP_PASS = smtp_pass, PY_FROM = from_addr,
      PY_TO = to_csv, PY_CC = cc_csv, PY_BCC = bcc_csv,
      PY_SUBJECT = subject, PY_BODY = body, PY_ATT = att),
    {
      py_bin <- Sys.which("python3"); if (!nzchar(py_bin)) py_bin <- Sys.which("python")
      if (!nzchar(py_bin)) { log_msg("Python introuvable pour l'envoi email."); return(FALSE) }
      out <- suppressWarnings(system2(py_bin, shQuote(tmp_py),
                                      stdout = TRUE, stderr = TRUE))
      ok <- any(grepl("PYEMAIL_OK", out))
      if (!ok) log_msg("Echec envoi Python:", paste(out, collapse = " | "))
      else log_msg("Email envoye via Python smtplib (port", smtp_port, ").")
      ok
    }
  )
  unlink(tmp_py)
  isTRUE(res)
}

# Petit helper : exécute une expression avec des variables d'environnement
# temporaires (évite d'ajouter une dépendance à withr).
withr_env <- function(vars, expr) {
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(vars))
  on.exit({
    set_back <- old[!is.na(old)]
    if (length(set_back)) do.call(Sys.setenv, as.list(set_back))
    unset <- names(old)[is.na(old)]
    if (length(unset)) Sys.unsetenv(unset)
  })
  force(expr)
}

send_sitrep_email <- function(latest, pdf_path, recipients) {
  smtp_user <- Sys.getenv("SMTP_USER")
  smtp_pass <- Sys.getenv("SMTP_PASS")
  alert_from <- Sys.getenv("ALERT_FROM", smtp_user)
  smtp_host <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
  smtp_port <- as.integer(Sys.getenv("SMTP_PORT", "587"))

  if (!nzchar(smtp_user)) stop("SMTP_USER secret is empty.")
  if (!nzchar(smtp_pass)) stop("SMTP_PASS secret is empty.")
  if (!nzchar(alert_from)) stop("ALERT_FROM secret/env is empty.")
  if (length(recipients$to) == 0) stop("No active TO recipient found in alert_recipients.csv.")

  body <- paste(
    "Dear team,",
    "",
    paste0("Please find attached the latest DRC Ebola SitRep detected by PREIS: SitRep ", latest$sitrep_no, "."),
    "",
    paste0("Title: ", safe_chr(latest$title)),
    paste0("INSP page: ", safe_chr(latest$post_url)),
    "PDF source: PREIS cloud automation",
    "",
    "This is an automated PREIS notification. Analytical outputs will follow once generated and validated.",
    "",
    "Best regards,",
    "PREIS Ebola DRC Automation",
    sep = "\n"
  )

  subject <- paste0("[PREIS Ebola DRC] SitRep ", latest$sitrep_no, " PDF")

  # ----------------------------------------------------------
  # Envoi via Python (smtplib) — plus robuste que blastula en
  # cloud (blastula peut provoquer un segfault sur la connexion
  # SSL/SMTP). Python est préinstallé sur le runner GitHub.
  # ----------------------------------------------------------
  ok <- send_email_python(
    smtp_host = smtp_host, smtp_port = smtp_port,
    smtp_user = smtp_user, smtp_pass = smtp_pass,
    from_addr = alert_from,
    to = recipients$to, cc = recipients$cc, bcc = recipients$bcc,
    subject = subject, body = body, attachment = pdf_path
  )
  if (!ok) stop("Python SMTP send failed for SitRep PDF.")

  TRUE
}

# ============================================================
# POST-ANALYSE : déclenché APRÈS l'envoi du PDF brut.
# Enchaîne analyse consolidée -> synthèse -> alerte propre.
# TOLÉRANT AUX PANNES : chaque étape est encapsulée ; un échec
# est journalisé mais n'interrompt pas la sauvegarde de l'état
# (sinon le PDF serait renvoyé en boucle au run suivant).
# ============================================================
run_post_analysis <- function(latest, pdf_path, recipients) {
  scripts_dir <- file.path(ROOT_DIR, "scripts")
  safe_source <- function(label, file) {
    fp <- file.path(scripts_dir, file)
    if (!file.exists(fp)) { log_msg("POST-ANALYSE skip (absent):", file); return(FALSE) }
    ok <- tryCatch({ source(fp, local = new.env()); TRUE },
                   error = function(e) { log_msg("POST-ANALYSE ECHEC", label, ":", conditionMessage(e)); FALSE })
    if (ok) log_msg("POST-ANALYSE OK:", label)
    ok
  }

  # 0) Indicateurs journaliers (national + province + zone) depuis INRB
  #    -> data/final/PREIS_daily_indicators.csv (lu par le dashboard)
  safe_source("indicateurs_journaliers", "11_daily_indicators.R")

  # 0ter) Détection de signaux d'alerte précoce (seuils explicites)
  #       -> data/final/PREIS_signals.csv + texte pour l'email d'alerte
  safe_source("detection_signaux", "13_signal_detection.R")

  # 0quater) Validation rétrospective : rejoue la détection sur toute la
  #          série (produit les CSV + figure lus par l'onglet dashboard).
  safe_source("validation_retro", "14_retrospective_validation.R")

  # 0bis) Couche choroplèthe zones de santé : régénérée seulement si absente
  #       (le shapefile est volumineux et ne change pas entre SitReps)
  geo_fp <- file.path(ROOT_DIR, "dashboard_ebola", "data", "curated",
                      "rdc_zones_sante_est.geojson")
  geo_fp2 <- file.path(ROOT_DIR, "data", "curated", "rdc_zones_sante_est.geojson")
  if (!file.exists(geo_fp) && !file.exists(geo_fp2)) {
    safe_source("zones_sante_geo", "12_build_health_zones_geo.R")
  } else {
    log_msg("POST-ANALYSE skip (geojson zones deja present)")
  }

  # 1) Analyse consolidée (tableaux + graphiques + carte)
  safe_source("analyse_consolidee", "03_analyse_consolidee.R")

  # 1bis) Graphiques publication aux couleurs Africa CDC (5 figures)
  safe_source("graphiques_africa_cdc", "charts_7_13_june_SR29.R")

  # 1ter) Restaging des données vers le dashboard (si le script est présent)
  safe_source("prepare_dashboard", file.path("..", "dashboard_ebola",
                                             "prepare_dashboard_data.R"))

  # 2) Synthèse narrative (3 niveaux) -> écrit synthese_narrative.txt
  safe_source("synthese_narrative", "05_synthese_narrative.R")

  # 3) Alerte propre : résumé + signaux + recommandations + lien dashboard
  #    + graphiques en pièces jointes. Réutilise le système conditionnel
  #    (dédoublonnage indépendant via son propre sent_log).
  alert_ok <- safe_source("alerte_propre", "04_send_sitrep_alerts_conditional.R")

  # Repli : si le script conditionnel n'existe pas, on envoie au moins
  # une alerte synthétique simple avec les graphiques disponibles.
  if (!alert_ok) {
    tryCatch({
      out_dir <- file.path(ROOT_DIR, "outputs", "analyse")
      synth_fp <- file.path(out_dir, "synthese_narrative.txt")
      body_txt <- if (file.exists(synth_fp)) paste(readLines(synth_fp, warn = FALSE), collapse = "\n")
                  else paste0("Analyse du SitRep ", latest$sitrep_no, " disponible.")
      imgs <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
      email <- blastula::compose_email(body = blastula::md(body_txt))
      for (im in imgs) email <- blastula::add_attachment(email, file = im, filename = basename(im))
      blastula::smtp_send(
        email = email,
        from = Sys.getenv("ALERT_FROM", Sys.getenv("SMTP_USER")),
        to = recipients$to, cc = recipients$cc, bcc = recipients$bcc,
        subject = paste0("[PREIS Ebola DRC] SitRep ", latest$sitrep_no, " — analyse"),
        credentials = blastula::creds(
          user = Sys.getenv("SMTP_USER"), pass = Sys.getenv("SMTP_PASS"),
          host = Sys.getenv("SMTP_HOST", "smtp.gmail.com"),
          port = as.integer(Sys.getenv("SMTP_PORT", "465")), use_ssl = TRUE))
      log_msg("POST-ANALYSE OK: alerte_propre (repli)")
    }, error = function(e) log_msg("POST-ANALYSE ECHEC alerte repli :", conditionMessage(e)))
  }

  invisible(TRUE)
}

main <- function() {
  log_msg("============================================================")
  log_msg("PREIS cloud SitRep monitor started")

  posts <- get_sitrep_posts()
  if (nrow(posts) == 0) {
    log_msg("Aucun SitRep detecte (source INSP indisponible ou rien de nouveau). Sortie propre.")
    return(invisible(FALSE))
  }

  latest <- posts[which.max(posts$sitrep_no), , drop = FALSE]
  latest_no <- as.integer(latest$sitrep_no[1])
  log_msg("Latest online SitRep:", latest_no)

  # -----------------------------------------------------------------
  # MODE TEST (PREIS_TEST_ALERT=true) : relance UNIQUEMENT l'analyse et
  # l'email d'alerte scientifique pour le dernier SitRep, SANS renvoyer
  # le PDF brut et SANS modifier l'état. Sert à vérifier que l'alerte
  # avec les signaux fonctionne. Ne s'active que si explicitement demandé.
  # -----------------------------------------------------------------
  if (tolower(Sys.getenv("PREIS_TEST_ALERT", "false")) %in% c("true","1","yes")) {
    log_msg(">>> MODE TEST ALERTE : analyse + email d'alerte du SitRep", latest_no,
            "(sans renvoi du PDF, sans modif d'etat).")
    run_existing_pipeline()
    pdf_path <- find_latest_local_pdf(latest_no)
    if (is.na(pdf_path) || !is_valid_pdf_file(pdf_path)) {
      pdf_path <- download_sitrep_pdf_direct(latest_no, latest$post_url[1])
    }
    recipients <- read_recipients()
    tryCatch(
      run_post_analysis(latest[1, ], pdf_path, recipients),
      error = function(e) log_msg("MODE TEST : erreur post-analyse :", conditionMessage(e))
    )
    log_msg(">>> MODE TEST terminé. Vérifie l'email d'alerte scientifique.")
    return(invisible(TRUE))
  }

  state <- read_state()
  if (nrow(state) > 0 && latest_no %in% state$sitrep_no[state$status %in% c("sent", "sent_cloud")]) {
    log_msg("No new SitRep to send. Already sent SitRep:", latest_no)
    return(invisible(FALSE))
  }

  run_existing_pipeline()

  pdf_path <- find_latest_local_pdf(latest_no)

  # FALLBACK robuste : si le pipeline n'a pas produit le PDF (ex. scraping
  # INSP bloqué), on le télécharge directement depuis l'URL déjà trouvée
  # par la détection (page catégorie ou probe_direct_sitreps).
  if (is.na(pdf_path) || !is_valid_pdf_file(pdf_path)) {
    log_msg("PDF absent après pipeline — tentative de téléchargement direct.")
    pdf_path <- download_sitrep_pdf_direct(latest_no, latest$post_url[1])
  }

  if (is.na(pdf_path) || !is_valid_pdf_file(pdf_path)) {
    log_msg("PDF du SitRep", latest_no, "non recuperable pour le moment.",
            "Nouvel essai au prochain cycle. Sortie propre.")
    return(invisible(FALSE))
  }

  recipients <- read_recipients()

  log_msg("Sending SitRep", latest_no, "to:", paste(recipients$to, collapse = ", "))
  send_sitrep_email(latest[1, ], pdf_path, recipients)
  log_msg("EMAIL_PDF_BRUT_OK SitRep:", latest_no)

  # --- CHAÎNE POST-ANALYSE : analyse -> synthèse -> alerte propre ---
  # Tolérante aux pannes : ne bloque jamais la sauvegarde de l'état.
  tryCatch(
    run_post_analysis(latest[1, ], pdf_path, recipients),
    error = function(e) log_msg("POST-ANALYSE erreur globale :", conditionMessage(e))
  )

  sent_row <- data.frame(
    sitrep_no = latest_no,
    title = safe_chr(latest$title),
    post_url = safe_chr(latest$post_url),
    pdf_path = pdf_path,
    status = "sent_cloud",
    sent_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  if (nrow(state) > 0) {
    cols <- union(names(state), names(sent_row))
    for (cc in setdiff(cols, names(state))) state[[cc]] <- NA_character_
    for (cc in setdiff(cols, names(sent_row))) sent_row[[cc]] <- NA_character_
    out <- rbind(state[, cols, drop = FALSE], sent_row[, cols, drop = FALSE])
  } else {
    out <- sent_row
  }

  write_state(out)

  log_msg("EMAIL_SENT_OK SitRep:", latest_no)
  invisible(TRUE)
}

# ============================================================
# Point d'entrée sécurisé : le monitoring ne doit JAMAIS planter
# pour une cause externe (INSP indisponible, page vide, réseau).
# Il se termine proprement (exit 0). Une vraie erreur de config
# (secrets manquants) reste signalée distinctement dans les logs,
# mais sans bloquer le workflow en rouge de façon répétée.
# ============================================================
tryCatch(
  main(),
  error = function(e) {
    msg <- conditionMessage(e)
    log_msg("FIN AVEC AVERTISSEMENT (non bloquant) :", msg)
    # Sortie 0 : on ne casse pas le workflow pour une cause transitoire.
    # Les vraies erreurs de configuration sont visibles dans les logs
    # ci-dessus (ex. 'SMTP_USER secret is empty').
    quit(save = "no", status = 0)
  }
)
