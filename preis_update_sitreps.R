############################################################
# preis_update_sitreps.R  —  Acquisition ROBUSTE + mise a jour complete
#
# Corrige definitivement l'acquisition des SitReps recents :
#   - le nom du PDF varie (N_61...Analytique, N°60..._VF, ...) -> deviner le
#     nom echoue. Ici on LIT le vrai PDF embarque dans chaque page-article
#     (attribut pdfemb-data, base64 -> JSON -> url) OU un lien .pdf direct.
#   - decouverte souple des articles depuis la page categorie INSP
#     (URL du type https://insp.cd/sitrep-n0XX-.../).
#   - "graine" fiable pour le 061 (URL exacte connue) en filet de securite.
#   - telecharge dans data/pdf/PREIS_DRC_Ebola_SitRep_NNN.pdf, valide %PDF.
#   - enchaine ensuite re-extraction du cumul national + consolidation.
#
# Reutilisable tel quel dans la CI (remplace le telechargement fragile).
#
# Usage (console R, racine du projet) :
#   source("preis_update_sitreps.R")
# Puis (si la serie est bonne) : git add/commit/push.
############################################################

suppressPackageStartupMessages({ library(httr); library(stringr) })

.preis_b64dec <- function(s) {
  s <- gsub("[^A-Za-z0-9+/=]", "", s)
  if (!nzchar(s)) return("")
  if (requireNamespace("base64enc", quietly = TRUE))
    return(tryCatch(rawToChar(base64enc::base64decode(s)), error = function(e) ""))
  if (requireNamespace("jsonlite", quietly = TRUE))
    return(tryCatch(rawToChar(jsonlite::base64_dec(s)), error = function(e) ""))
  ""
}

.preis_get_html <- function(url) {
  r <- tryCatch(httr::GET(url, httr::timeout(90),
                          httr::user_agent("Mozilla/5.0 PREIS")),
                error = function(e) NULL)
  if (is.null(r) || httr::status_code(r) >= 400) return(NA_character_)
  tryCatch(httr::content(r, as = "text", encoding = "UTF-8"), error = function(e) NA_character_)
}

# Extrait l'URL du PDF depuis le HTML d'une page-article
.preis_pdf_url <- function(html) {
  if (is.na(html) || !nzchar(html)) return(NA_character_)
  # a) lien .pdf direct dans /wp-content/uploads
  u <- str_extract(html, "https?://insp\\.cd/wp-content/uploads/[^\"'<> ]+?\\.pdf")
  if (!is.na(u)) return(u)
  # b) tout lien .pdf en clair
  u <- str_extract(html, "https?://[^\"'<> ]+?\\.pdf")
  if (!is.na(u)) return(u)
  # c) attribut pdfemb-data (base64 -> JSON -> url)
  d <- str_match(html, "pdfemb-data=[\"']([^\"']+)[\"']")[1, 2]
  if (!is.na(d)) {
    dec <- .preis_b64dec(utils::URLdecode(d))
    u <- str_extract(dec, "https?:\\\\?/\\\\?/[^\"'<> ]+?\\.pdf")
    if (!is.na(u)) return(gsub("\\\\/", "/", u))
  }
  NA_character_
}

.preis_dl <- function(url, no, dest_dir = "data/pdf") {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", no))
  ok <- tryCatch({
    httr::GET(url, httr::write_disk(dest, overwrite = TRUE), httr::timeout(180),
              httr::user_agent("Mozilla/5.0 PREIS")); TRUE
  }, error = function(e) FALSE)
  if (!ok) return(FALSE)
  sig <- tryCatch({ con <- file(dest, "rb"); on.exit(close(con))
  rawToChar(readBin(con, "raw", 4)) }, error = function(e) "")
  identical(sig, "%PDF")
}

.preis_is_pdf <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sig <- tryCatch({ con <- file(path, "rb"); on.exit(close(con))
  rawToChar(readBin(con, "raw", 4)) }, error = function(e) "")
  identical(sig, "%PDF")
}

# Decouverte des articles SitRep sur la page categorie
.preis_discover <- function(max_pages = 6) {
  base <- "https://insp.cd/category/sitrep/"; map <- list()
  empty <- 0L
  for (pg in seq_len(max_pages)) {
    url  <- if (pg == 1) base else paste0(base, "page/", pg, "/")
    html <- .preis_get_html(url)
    if (is.na(html)) { empty <- empty + 1L; if (empty >= 2) break; next }
    arts <- unique(unlist(str_extract_all(html, "https?://insp\\.cd/sitrep[^\"'<> ]*")))
    if (length(arts) == 0) { empty <- empty + 1L; if (empty >= 2) break; next }
    empty <- 0L
    for (a in arts) {
      m <- str_match(tolower(a), "sitrep[-_ ]*n[^0-9]*0*([0-9]{1,3})")[1, 2]
      if (is.na(m)) next
      no <- as.integer(m); if (no < 1 || no > 300) next
      if (is.null(map[[as.character(no)]])) map[[as.character(no)]] <- a
    }
  }
  map
}

preis_update_sitreps <- function(from = 41, seeds = list(), max_pages = 6) {
  map <- .preis_discover(max_pages)
  det <- sort(as.integer(names(map)))
  cat("SitRep articles detectes sur INSP:",
      if (length(det)) paste(det, collapse = ", ") else "(aucun)", "\n")
  nos <- sort(unique(c(det, as.integer(names(seeds)))))
  nos <- nos[nos >= from]
  if (length(nos) == 0) { cat("Rien a acquerir.\n"); return(invisible(NULL)) }
  for (no in nos) {
    dest <- file.path("data/pdf", sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", no))
    if (.preis_is_pdf(dest)) { cat(sprintf("  %03d : deja present\n", no)); next }
    purl <- seeds[[as.character(no)]]
    if (is.null(purl)) {
      art <- map[[as.character(no)]]
      if (!is.null(art)) purl <- .preis_pdf_url(.preis_get_html(art))
    }
    if (is.null(purl) || is.na(purl)) { cat(sprintf("  %03d : PDF introuvable\n", no)); next }
    okd <- .preis_dl(purl, no)
    cat(sprintf("  %03d : %s\n         %s\n", no, if (okd) "OK" else "ECHEC", purl))
  }
  invisible(NULL)
}

# --- Graine fiable : URL exacte du PDF SR61 (lue sur la page INSP) ---------
.PREIS_SEEDS <- list(
  "61" = "https://insp.cd/wp-content/uploads/2026/07/Draft_SitRep_MVE_RDC_N_61_14-07-2026_Analytique.pdf"
)

cat("========== PREIS — ACQUISITION ROBUSTE ==========\n")
preis_update_sitreps(from = 41, seeds = .PREIS_SEEDS)

cat("\n========== RE-EXTRACTION + CONSOLIDATION ==========\n")
if (file.exists("preis_fix_national_cumul.R")) source("preis_fix_national_cumul.R")
if (file.exists(file.path("scripts", "03_analyse_consolidee.R")))
  source(file.path("scripts", "03_analyse_consolidee.R"))
cat("\n>> Termine. Verifie la serie ci-dessus (max attendu = 61).\n")