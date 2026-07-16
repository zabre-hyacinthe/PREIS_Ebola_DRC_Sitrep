############################################################
# PREIS EBOLA RDC
# INVENTAIRE ET NORMALISATION NON DESTRUCTIVE DES PDF SITREP
#
# Les originaux ne sont jamais supprimés ni renommés.
# Une copie canonique est créée seulement si le PDF est valide.
############################################################

project_dir <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
pdf_dir <- file.path(project_dir, "data", "pdf")
canonical_dir <- file.path(pdf_dir, "canonical")

dir.create(canonical_dir, recursive = TRUE, showWarnings = FALSE)

pdf_signature_ok <- function(path) {
  if (!file.exists(path)) return(FALSE)

  info <- file.info(path)
  if (is.na(info$size) || info$size < 5000) return(FALSE)

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  identical(
    readBin(con, what = "raw", n = 4),
    charToRaw("%PDF")
  )
}

extract_sitrep_number <- function(filename) {
  x <- basename(filename)
  x <- enc2utf8(x)

  x <- gsub("NAÂ°|NÂ°|Nº|N°|N°", "N", x, ignore.case = TRUE)
  x <- gsub("%C2%B0", "", x, fixed = TRUE)

  patterns <- c(
    "(?i)sitrep[^0-9]{0,35}n?[^0-9]*0*([0-9]{1,3})",
    "(?i)bd(?:b|v)[^0-9]{0,35}sitrep[^0-9]*0*([0-9]{1,3})",
    "(?i)draft[^0-9]{0,35}sitrep[^0-9]*0*([0-9]{1,3})",
    "(?i)(?:^|[_ -])n[^0-9]*0*([0-9]{1,3})(?:[_ .-]|$)"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, x, perl = TRUE)
    values <- regmatches(x, match)[[1]]

    if (length(values) >= 2) {
      number <- suppressWarnings(as.integer(values[2]))

      if (!is.na(number) && number >= 1 && number <= 999) {
        return(number)
      }
    }
  }

  NA_integer_
}

files <- list.files(
  pdf_dir,
  pattern = "\\.pdf$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

inventory <- data.frame(
  original_file = files,
  original_name = basename(files),
  sitrep_no = vapply(files, extract_sitrep_number, integer(1)),
  valid_pdf = vapply(files, pdf_signature_ok, logical(1)),
  size_bytes = file.info(files)$size,
  canonical_file = NA_character_,
  copied = FALSE,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(inventory))) {
  if (is.na(inventory$sitrep_no[i]) || !inventory$valid_pdf[i]) {
    next
  }

  canonical_name <- paste0(
    "PREIS_DRC_Ebola_SitRep_",
    sprintf("%03d", inventory$sitrep_no[i]),
    ".pdf"
  )

  destination <- file.path(canonical_dir, canonical_name)
  inventory$canonical_file[i] <- destination

  if (!file.exists(destination)) {
    inventory$copied[i] <- file.copy(
      inventory$original_file[i],
      destination,
      overwrite = FALSE
    )
  } else {
    current_size <- file.info(destination)$size
    incoming_size <- inventory$size_bytes[i]

    if (!is.na(incoming_size) && !is.na(current_size) &&
        incoming_size > current_size) {
      inventory$copied[i] <- file.copy(
        inventory$original_file[i],
        destination,
        overwrite = TRUE
      )
    }
  }
}

output_file <- file.path(
  pdf_dir,
  paste0(
    "PREIS_pdf_inventory_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".csv"
  )
)

write.csv(
  inventory,
  output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nInventaire terminé.\n")
cat("PDF analysés :", nrow(inventory), "\n")
cat("Numéros détectés :", sum(!is.na(inventory$sitrep_no)), "\n")
cat("PDF valides :", sum(inventory$valid_pdf), "\n")
cat("Copies canoniques créées/mises à jour :", sum(inventory$copied), "\n")
cat("Inventaire :", output_file, "\n")
cat("Dossier canonique :", canonical_dir, "\n")
