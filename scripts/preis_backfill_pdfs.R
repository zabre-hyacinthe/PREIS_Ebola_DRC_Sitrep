############################################################
# scripts/preis_backfill_pdfs.R
#
# Backfill robuste et IDEMPOTENT des PDF officiels SitRep (INSP) dans data/pdf/.
# Reutilise le resolveur V2.1 (scripts/preis_pdf_resolver_v2.R).
#
# Ce script:
#   - enumere TOUS les SitRep listes sur https://insp.cd/category/sitrep/
#   - pour chaque numero, telecharge le PDF sous le nom canonique
#         data/pdf/PREIS_DRC_Ebola_SitRep_<NNN>.pdf
#     SEULEMENT s'il n'est pas deja present et valide (%PDF).
#   - n'envoie AUCUN email, ne touche AUCUN fichier d'etat, ne consolide RIEN.
#
# Utilisation (console R):
#   source("scripts/preis_backfill_pdfs.R")
#   res <- preis_backfill_run()              # tous les SitRep listes
#   res <- preis_backfill_run(from = 41)      # a partir du 41
#   res <- preis_backfill_run(from = 29, to = 60)
#
# En production (GitHub Actions):
#   Rscript scripts/preis_backfill_pdfs.R     # execute un backfill complet
############################################################

# --- Charger le resolveur V2.1 (obligatoire) ------------------------------
.preis_bf_root <- getwd()
.preis_bf_resolver <- file.path(.preis_bf_root, "scripts", "preis_pdf_resolver_v2.R")
if (!file.exists(.preis_bf_resolver)) {
  stop("Resolveur introuvable: ", .preis_bf_resolver,
       " (lancer depuis la racine du projet).")
}
source(.preis_bf_resolver)

suppressWarnings(suppressMessages({
  ok_pkgs <- all(vapply(c("httr", "stringr"), requireNamespace, logical(1), quietly = TRUE))
}))
if (!ok_pkgs) stop("Packages requis manquants: httr, stringr")

# --- 1) Enumerer les pages SitRep de la categorie INSP --------------------
preis_backfill_enumerate <- function(max_pages = 15) {
  base <- "https://insp.cd/category/sitrep/"
  found_no  <- integer()
  found_url <- character()
  empty <- 0L
  for (pg in seq_len(max_pages)) {
    url  <- if (pg == 1) base else paste0(base, "page/", pg, "/")
    html <- preis_v2_http_get_text(url, timeout_sec = 90)
    if (is.na(html) || !nzchar(html)) {
      empty <- empty + 1L
      if (empty >= 3) break
      next
    }
    links <- unique(unlist(stringr::str_extract_all(
      html, "https?://insp\\.cd/sitrep[-_]?n[0-9]+[a-z0-9_%-]*"
    )))
    links <- links[!is.na(links) & nzchar(links)]
    if (length(links) == 0) {
      empty <- empty + 1L
      if (empty >= 3) break
      next
    }
    empty <- 0L
    for (lk in links) {
      no <- preis_v2_extract_sitrep_no(lk, "")
      if (!is.na(no) && !(no %in% found_no)) {
        found_no  <- c(found_no, no)
        found_url <- c(found_url, lk)
      }
    }
  }
  if (length(found_no) == 0) {
    return(data.frame(sitrep_no = integer(), page_url = character(),
                      stringsAsFactors = FALSE))
  }
  ord <- order(found_no)
  data.frame(sitrep_no = found_no[ord], page_url = found_url[ord],
             stringsAsFactors = FALSE)
}

# --- 2) Backfill : telecharger ce qui manque ------------------------------
preis_backfill_run <- function(from = NA, to = NA,
                               dest_dir = file.path(getwd(), "data", "pdf"),
                               max_pages = 15,
                               write_manifest = TRUE) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  enum <- preis_backfill_enumerate(max_pages)
  if (nrow(enum) == 0) {
    cat("Aucun SitRep trouve sur INSP (categorie inaccessible ?).\n")
    return(invisible(data.frame()))
  }
  if (!is.na(from)) enum <- enum[enum$sitrep_no >= from, , drop = FALSE]
  if (!is.na(to))   enum <- enum[enum$sitrep_no <= to,   , drop = FALSE]
  if (nrow(enum) == 0) { cat("Rien a faire dans l'intervalle demande.\n"); return(invisible(data.frame())) }
  
  cat(sprintf("SitRep listes sur INSP: %d..%d  (%d elements)\n",
              min(enum$sitrep_no), max(enum$sitrep_no), nrow(enum)))
  
  status <- character(nrow(enum))
  files  <- rep(NA_character_, nrow(enum))
  for (i in seq_len(nrow(enum))) {
    no <- enum$sitrep_no[i]; pg <- enum$page_url[i]
    dest <- file.path(dest_dir, sprintf("PREIS_DRC_Ebola_SitRep_%03d.pdf", no))
    if (file.exists(dest) && preis_v2_pdf_signature_ok(dest)) {
      status[i] <- "present"; files[i] <- dest
      cat(sprintf("  SitRep %03d : deja present (skip)\n", no))
      next
    }
    cat(sprintf("  SitRep %03d : resolution + telechargement...\n", no))
    f <- tryCatch(
      preis_v2_resolve_and_download(sitrep_no = no, page_url = pg,
                                    current_pdf_url = "", title = "",
                                    dest_dir = dest_dir),
      error = function(e) { cat("     erreur: ", conditionMessage(e), "\n"); NA_character_ }
    )
    if (!is.na(f) && preis_v2_pdf_signature_ok(f)) {
      status[i] <- "downloaded"; files[i] <- f
      cat(sprintf("  SitRep %03d : OK -> %s\n", no, basename(f)))
    } else {
      status[i] <- "FAILED"
      cat(sprintf("  SitRep %03d : ECHEC (PDF non resolu/valide)\n", no))
    }
  }
  
  res <- data.frame(sitrep_no = enum$sitrep_no, page_url = enum$page_url,
                    status = status, file = files, stringsAsFactors = FALSE)
  
  n_present <- sum(status == "present")
  n_dl      <- sum(status == "downloaded")
  n_fail    <- sum(status == "FAILED")
  cat(sprintf("\nRESUME backfill: %d present(s), %d telecharge(s), %d echec(s).\n",
              n_present, n_dl, n_fail))
  if (n_fail > 0) {
    cat("SitRep en echec: ", paste(res$sitrep_no[status == "FAILED"], collapse = ", "), "\n")
  }
  
  if (isTRUE(write_manifest)) {
    mf_dir <- file.path(getwd(), "data", "final")
    dir.create(mf_dir, recursive = TRUE, showWarnings = FALSE)
    mf <- file.path(mf_dir, "backfill_pdf_manifest.csv")
    tryCatch(utils::write.csv(res, mf, row.names = FALSE),
             error = function(e) NULL)
    cat("Manifest: ", mf, "\n")
  }
  invisible(res)
}

# --- Execution directe en mode script (Rscript) ---------------------------
if (!interactive()) {
  invisible(preis_backfill_run())
}

# END scripts/preis_backfill_pdfs.R