############################################################
# PREIS EBOLA RDC
# INSTALL PDF RESOLVER V2 SAFELY
#
# This installer:
# - validates the V2 module syntax;
# - backs up the main email script;
# - inserts one source() call after log_msg;
# - does not send email;
# - restores the original script on validation failure.
############################################################

project_dir <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"

main_file <- file.path(
  project_dir,
  "scripts",
  "preis_safe_scientific_email.R"
)

module_file <- file.path(
  project_dir,
  "scripts",
  "preis_pdf_resolver_v2.R"
)

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S",
  tz = "UTC"
)

backup_file <- paste0(
  main_file,
  ".BACKUP_BEFORE_PDF_RESOLVER_V2_",
  timestamp
)

if (!file.exists(main_file)) {
  stop("Main script not found: ", main_file)
}

if (!file.exists(module_file)) {
  stop("V2 module not found: ", module_file)
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
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

module_parse_ok <- tryCatch(
  {
    parse(file = module_file)
    TRUE
  },
  error = function(e) {
    message(
      "V2 module syntax error: ",
      conditionMessage(e)
    )

    FALSE
  }
)

if (!module_parse_ok) {
  stop("Installation cancelled. Main script unchanged.")
}

if (!file.copy(
  main_file,
  backup_file,
  overwrite = FALSE
)) {
  stop("Could not create backup: ", backup_file)
}

lines <- readLines(
  main_file,
  warn = FALSE,
  encoding = "UTF-8"
)

start_marker <- "# PREIS PDF RESOLVER V2 SOURCE START"
end_marker <- "# PREIS PDF RESOLVER V2 SOURCE END"

source_block <- c(
  start_marker,
  "source(",
  "  file.path(",
  "    getwd(),",
  "    'scripts',",
  "    'preis_pdf_resolver_v2.R'",
  "  ),",
  "  local = globalenv()",
  ")",
  end_marker
)

start_existing <- grep(
  start_marker,
  lines,
  fixed = TRUE
)

end_existing <- grep(
  end_marker,
  lines,
  fixed = TRUE
)

if (length(start_existing) == 1L &&
    length(end_existing) == 1L &&
    end_existing > start_existing) {
  lines <- c(
    lines[seq_len(start_existing - 1L)],
    source_block,
    lines[(end_existing + 1L):length(lines)]
  )
} else if (length(start_existing) == 0L &&
           length(end_existing) == 0L) {
  insertion_line <- grep(
    "^env_get\\s*<-\\s*function",
    lines,
    perl = TRUE
  )

  if (length(insertion_line) != 1L) {
    stop(
      "Could not identify safe insertion point before env_get(). ",
      "Main script restored path: ",
      backup_file
    )
  }

  insertion_line <- insertion_line[1L]

  lines <- c(
    lines[seq_len(insertion_line - 1L)],
    "",
    source_block,
    "",
    lines[insertion_line:length(lines)]
  )
} else {
  stop(
    "Incomplete existing V2 source markers. ",
    "No modification applied."
  )
}

temporary_file <- paste0(
  main_file,
  ".TEMP_INSTALL_V2_",
  timestamp
)

writeLines(
  lines,
  temporary_file,
  useBytes = TRUE
)

main_parse_ok <- tryCatch(
  {
    parse(file = temporary_file)
    TRUE
  },
  error = function(e) {
    message(
      "Patched main script syntax error: ",
      conditionMessage(e)
    )

    FALSE
  }
)

if (!main_parse_ok) {
  unlink(temporary_file, force = TRUE)

  stop(
    "Installation cancelled. Original main script remains. ",
    "Backup: ",
    backup_file
  )
}

if (!file.copy(
  temporary_file,
  main_file,
  overwrite = TRUE
)) {
  unlink(temporary_file, force = TRUE)
  stop("Could not install source block.")
}

unlink(temporary_file, force = TRUE)

final_parse_ok <- tryCatch(
  {
    parse(file = main_file)
    TRUE
  },
  error = function(e) FALSE
)

if (!final_parse_ok) {
  file.copy(
    backup_file,
    main_file,
    overwrite = TRUE
  )

  stop(
    "Final validation failed. Original main script restored."
  )
}

cat("\n============================================================\n")
cat("PREIS PDF RESOLVER V2 INSTALLED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Main script :", main_file, "\n")
cat("V2 module   :", module_file, "\n")
cat("Backup      :", backup_file, "\n")
cat("R syntax    : OK\n")
cat("Email sent  : NO\n")
cat("============================================================\n")
