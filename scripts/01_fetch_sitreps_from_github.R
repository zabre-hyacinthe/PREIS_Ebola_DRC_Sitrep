############################################################
# PREIS EBOLA DRC
# 01_fetch_sitreps_from_github.R
#
# Récupère TOUS les SitReps 2026 (N°1 à N°28) depuis le
# dépôt officiel INRB-UMIE/BDBV2026-Data (GitHub).
#
# Ce dépôt est maintenu par l'INRB (Institut National de
# Recherche Biomédicale) — c'est la source de référence
# pour les SitReps que le site INSP n'expose pas (1-14).
#
# Les PDFs sont stockés en Git LFS. Ce script utilise
# l'API LFS batch pour obtenir les URLs et télécharger.
#
# Sortie : data/pdf/SitRep_NN_2026.pdf
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(stringr)
})

# ---- Configuration ----
BASE_DIR <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
PDF_DIR  <- file.path(BASE_DIR, "data/pdf")
dir.create(PDF_DIR, recursive = TRUE, showWarnings = FALSE)

# Dépôt INRB (renommé en BDBV2026-Data)
REPO_RAW_API <- "https://api.github.com/repos/INRB-UMIE/BDBV2026-Data/contents/data/insp_sitrep/raw"
LFS_BATCH    <- "https://github.com/INRB-UMIE/BDBV2026-Data.git/info/lfs/objects/batch"

cat("\n============================================================\n")
cat("RÉCUPÉRATION DES SITREPS 1-28 DEPUIS LE DÉPÔT INRB (GitHub)\n")
cat("============================================================\n\n")

# ---- 1. Lister les fichiers PDF du dépôt ----
cat(">> Listing des PDFs du dépôt INRB...\n")

resp <- GET(REPO_RAW_API,
            add_headers("User-Agent" = "PREIS-Bot",
                        "Accept" = "application/vnd.github+json"),
            timeout(60))

if (status_code(resp) != 200) {
  stop("Impossible de lister le dépôt GitHub (HTTP ", status_code(resp),
       "). Réessaie dans quelques minutes (rate limit API).")
}

files <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)

pdf_files <- Filter(function(f) grepl("\\.pdf$", f$name, ignore.case = TRUE), files)
cat("   Trouvé", length(pdf_files), "fichiers PDF\n\n")

# ---- 2. Helper: extraire le numéro de SitRep du nom de fichier ----
extract_no <- function(fname) {
  # SitRep_MVE_001-2026.pdf, SitRep_MVE_28_2026.pdf, SitRep_MVE_012-2026_v2.pdf
  m <- str_match(fname, "SitRep_MVE_0*(\\d{1,3})[-_]")[, 2]
  if (is.na(m)) return(NA_integer_)
  as.integer(m)
}

# ---- 3. Pour chaque PDF : lire le pointeur LFS, obtenir l'URL, télécharger ----
download_lfs_pdf <- function(file_obj) {
  fname <- file_obj$name
  sno   <- extract_no(fname)
  if (is.na(sno)) {
    cat("   SKIP (pas de numéro):", fname, "\n")
    return(invisible(NULL))
  }

  # Ignore the v2 duplicate of 012 (keep main)
  if (grepl("_v2", fname)) {
    cat("   SKIP (doublon v2):", fname, "\n")
    return(invisible(NULL))
  }

  local_path <- file.path(PDF_DIR, sprintf("SitRep_%02d_2026.pdf", sno))

  if (file.exists(local_path) && file.info(local_path)$size > 10240) {
    cat("   SitRep", sno, ": déjà présent\n")
    return(invisible(local_path))
  }

  # 3a. Lire le pointeur LFS (le download_url du contenu = le pointeur texte)
  ptr <- tryCatch(
    content(GET(file_obj$download_url,
                add_headers("User-Agent" = "PREIS-Bot"), timeout(60)),
            "text", encoding = "UTF-8"),
    error = function(e) NA_character_
  )
  if (is.na(ptr) || !grepl("git-lfs", ptr)) {
    # Pas un pointeur LFS — c'est le PDF direct
    ok <- tryCatch({
      download.file(file_obj$download_url, local_path, mode = "wb", quiet = TRUE)
      file.exists(local_path) && file.info(local_path)$size > 10240
    }, error = function(e) FALSE)
    cat("   SitRep", sno, if (isTRUE(ok)) ": OK (direct)\n" else ": ÉCHEC\n")
    return(invisible(local_path))
  }

  oid  <- str_match(ptr, "oid sha256:([a-f0-9]+)")[, 2]
  size <- as.numeric(str_match(ptr, "size (\\d+)")[, 2])
  if (is.na(oid) || is.na(size)) {
    cat("   SitRep", sno, ": pointeur LFS illisible\n")
    return(invisible(NULL))
  }

  # 3b. Demander l'URL de téléchargement via l'API LFS batch
  body <- toJSON(list(
    operation = "download",
    transfer  = c("basic"),
    objects   = list(list(oid = oid, size = size))
  ), auto_unbox = TRUE)

  batch_resp <- tryCatch(
    POST(LFS_BATCH,
         add_headers("Accept" = "application/vnd.git-lfs+json",
                     "Content-Type" = "application/vnd.git-lfs+json",
                     "User-Agent" = "git-lfs/3.0"),
         body = body, timeout(60)),
    error = function(e) NULL
  )
  if (is.null(batch_resp) || status_code(batch_resp) != 200) {
    cat("   SitRep", sno, ": échec API LFS batch\n")
    return(invisible(NULL))
  }

  batch <- fromJSON(content(batch_resp, "text", encoding = "UTF-8"),
                    simplifyDataFrame = FALSE)
  dl_url <- tryCatch(batch$objects[[1]]$actions$download$href,
                     error = function(e) NA_character_)
  if (is.na(dl_url)) {
    cat("   SitRep", sno, ": pas d'URL de téléchargement\n")
    return(invisible(NULL))
  }

  # 3c. Télécharger le vrai PDF
  ok <- tryCatch({
    options(timeout = 300)
    download.file(dl_url, local_path, mode = "wb", quiet = TRUE)
    file.exists(local_path) && file.info(local_path)$size > 10240
  }, error = function(e) { cat("    erreur:", conditionMessage(e), "\n"); FALSE })

  if (isTRUE(ok)) {
    kb <- round(file.info(local_path)$size / 1024, 1)
    cat("   SitRep", sno, ": OK (", kb, "KB)\n")
  } else {
    cat("   SitRep", sno, ": ÉCHEC téléchargement\n")
    if (file.exists(local_path)) file.remove(local_path)
  }
  invisible(local_path)
}

# ---- 4. Boucle sur tous les PDFs ----
cat(">> Téléchargement des PDFs...\n")
for (f in pdf_files) {
  download_lfs_pdf(f)
}

# ---- 5. Bilan ----
cat("\n============================================================\n")
local_pdfs <- list.files(PDF_DIR, pattern = "^SitRep_\\d+_2026\\.pdf$", full.names = TRUE)
nums <- sort(as.integer(str_match(basename(local_pdfs), "SitRep_(\\d+)_2026")[, 2]))
cat("PDFs présents dans data/pdf/ :", length(local_pdfs), "\n")
cat("Numéros :", paste(nums, collapse = ", "), "\n")
missing <- setdiff(1:28, nums)
if (length(missing) > 0) {
  cat("Manquants :", paste(missing, collapse = ", "), "\n")
  cat("(Le N°3 n'existe pas dans le dépôt INRB — trou officiel.)\n")
}
cat("============================================================\n")
cat("\nÉTAPE SUIVANTE : relance le pipeline principal.\n")
cat("Il lira tous ces PDFs et les ajoutera au registre.\n")
