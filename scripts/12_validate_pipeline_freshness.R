############################################################
# PREIS EBOLA DRC
# 12_validate_pipeline_freshness.R
#
# Validation de bout en bout avant synchronisation du dashboard
# et avant envoi de l'e-mail enrichi.
#
# Le script ne modifie aucune donnée analytique. Il vérifie :
#   1) le dernier PDF officiel valide et son numéro interne ;
#   2) la présence du même SitRep dans PREIS_indicators_long.csv ;
#   3) la présence du même SitRep dans la série nationale ;
#   4) la disponibilité et la plausibilité des cumuls cas/décès.
############################################################

suppressPackageStartupMessages({
  library(readr)
  library(stringr)
  library(pdftools)
})

BASE_DIR <- Sys.getenv(
  "GITHUB_WORKSPACE",
  unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
)

PDF_DIR <- file.path(BASE_DIR, "data", "pdf")
IND_FP <- file.path(BASE_DIR, "data", "final", "PREIS_indicators_long.csv")
SERIE_FP <- file.path(
  BASE_DIR,
  "outputs",
  "analyse",
  "serie_temporelle_nationale.csv"
)
AUDIT_DIR <- file.path(BASE_DIR, "outputs", "audit")
AUDIT_FP <- file.path(AUDIT_DIR, "preis_pipeline_freshness.csv")

dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

max_integer_or_na <- function(x) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_integer_)
  }
  max(x, na.rm = TRUE)
}

pdf_signature_ok <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 5000) {
    return(FALSE)
  }

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  sig <- readBin(con, what = "raw", n = 4)
  identical(sig, charToRaw("%PDF"))
}

sitrep_no_from_filename <- function(path) {
  name <- basename(path)

  m1 <- stringr::str_match(
    name,
    "^PREIS_DRC_Ebola_SitRep_(\\d{3})\\.pdf$"
  )
  if (!is.na(m1[1, 2])) {
    return(as.integer(m1[1, 2]))
  }

  m2 <- stringr::str_match(
    name,
    "^SitRep_(\\d+)_2026\\.pdf$"
  )
  if (!is.na(m2[1, 2])) {
    return(as.integer(m2[1, 2]))
  }

  NA_integer_
}

sitrep_no_from_pdf <- function(path) {
  if (!pdf_signature_ok(path)) {
    return(NA_integer_)
  }

  pages <- tryCatch(
    pdftools::pdf_text(path),
    error = function(e) character()
  )

  if (length(pages) == 0) {
    return(NA_integer_)
  }

  header <- paste(utils::head(pages, 2), collapse = " ")

  match <- stringr::str_match(
    header,
    stringr::regex(
      "SitRep\\s*N\\s*[°ºo]?\\s*0*([0-9]{1,3})",
      ignore_case = TRUE
    )
  )

  if (is.na(match[1, 2])) {
    return(NA_integer_)
  }

  as.integer(match[1, 2])
}

pdf_files <- if (dir.exists(PDF_DIR)) {
  list.files(
    PDF_DIR,
    pattern = "^(PREIS_DRC_Ebola_SitRep_\\d{3}|SitRep_\\d+_2026)\\.pdf$",
    full.names = TRUE
  )
} else {
  character()
}

pdf_audit <- if (length(pdf_files) > 0) {
  data.frame(
    pdf_file = basename(pdf_files),
    filename_sitrep = vapply(
      pdf_files,
      sitrep_no_from_filename,
      integer(1)
    ),
    internal_sitrep = vapply(
      pdf_files,
      sitrep_no_from_pdf,
      integer(1)
    ),
    signature_ok = vapply(
      pdf_files,
      pdf_signature_ok,
      logical(1)
    ),
    stringsAsFactors = FALSE
  )
} else {
  data.frame(
    pdf_file = character(),
    filename_sitrep = integer(),
    internal_sitrep = integer(),
    signature_ok = logical(),
    stringsAsFactors = FALSE
  )
}

pdf_audit$number_match <- with(
  pdf_audit,
  signature_ok &
    !is.na(filename_sitrep) &
    !is.na(internal_sitrep) &
    filename_sitrep == internal_sitrep
)

valid_pdf_nos <- pdf_audit$internal_sitrep[pdf_audit$number_match]
latest_pdf_sitrep <- max_integer_or_na(valid_pdf_nos)

errors <- character()

if (is.na(latest_pdf_sitrep)) {
  errors <- c(
    errors,
    "Aucun PDF officiel valide avec concordance entre le nom du fichier et le numéro interne."
  )
}

if (!file.exists(IND_FP)) {
  errors <- c(errors, paste0("Base d'indicateurs absente : ", IND_FP))
  indicators <- NULL
  max_indicators <- NA_integer_
} else {
  indicators <- tryCatch(
    readr::read_csv(IND_FP, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (is.null(indicators) || !"sitrep_no" %in% names(indicators)) {
    errors <- c(errors, "PREIS_indicators_long.csv est illisible ou sans sitrep_no.")
    max_indicators <- NA_integer_
  } else {
    indicators$sitrep_no <- suppressWarnings(
      as.integer(indicators$sitrep_no)
    )
    max_indicators <- max_integer_or_na(indicators$sitrep_no)
  }
}

if (!file.exists(SERIE_FP)) {
  errors <- c(errors, paste0("Série nationale absente : ", SERIE_FP))
  serie <- NULL
  max_serie <- NA_integer_
} else {
  serie <- tryCatch(
    readr::read_csv(SERIE_FP, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (is.null(serie) || !"sitrep_no" %in% names(serie)) {
    errors <- c(errors, "serie_temporelle_nationale.csv est illisible ou sans sitrep_no.")
    max_serie <- NA_integer_
  } else {
    serie$sitrep_no <- suppressWarnings(as.integer(serie$sitrep_no))
    max_serie <- max_integer_or_na(serie$sitrep_no)
  }
}

if (!is.na(latest_pdf_sitrep) &&
    !is.na(max_indicators) &&
    max_indicators != latest_pdf_sitrep) {
  errors <- c(
    errors,
    paste0(
      "Décalage PDF/indicateurs : PDF N",
      latest_pdf_sitrep,
      " mais PREIS_indicators_long.csv N",
      max_indicators,
      "."
    )
  )
}

if (!is.na(latest_pdf_sitrep) &&
    !is.na(max_serie) &&
    max_serie != latest_pdf_sitrep) {
  errors <- c(
    errors,
    paste0(
      "Décalage PDF/série : PDF N",
      latest_pdf_sitrep,
      " mais serie_temporelle_nationale.csv N",
      max_serie,
      "."
    )
  )
}

required_ok <- FALSE
cases_value <- NA_real_
deaths_value <- NA_real_

if (!is.null(indicators) &&
    !is.na(latest_pdf_sitrep) &&
    all(c("indicator_code", "value") %in% names(indicators))) {

  latest_indicators <- indicators[
    indicators$sitrep_no == latest_pdf_sitrep,
    ,
    drop = FALSE
  ]

  required_codes <- c(
    "cumulative_confirmed_cases",
    "cumulative_deaths"
  )

  missing_codes <- setdiff(
    required_codes,
    unique(as.character(latest_indicators$indicator_code))
  )

  if (length(missing_codes) > 0) {
    errors <- c(
      errors,
      paste0(
        "Indicateurs obligatoires absents pour N",
        latest_pdf_sitrep,
        " : ",
        paste(missing_codes, collapse = ", "),
        "."
      )
    )
  } else {
    get_value <- function(code) {
      values <- suppressWarnings(
        as.numeric(
          latest_indicators$value[
            latest_indicators$indicator_code == code
          ]
        )
      )
      values <- values[is.finite(values)]
      if (length(values) == 0) {
        return(NA_real_)
      }
      values[1]
    }

    cases_value <- get_value("cumulative_confirmed_cases")
    deaths_value <- get_value("cumulative_deaths")

    required_ok <- is.finite(cases_value) &&
      cases_value > 0 &&
      is.finite(deaths_value) &&
      deaths_value >= 0 &&
      deaths_value <= cases_value

    if (!required_ok) {
      errors <- c(
        errors,
        paste0(
          "Cumuls non plausibles pour N",
          latest_pdf_sitrep,
          " : cas=",
          cases_value,
          ", décès=",
          deaths_value,
          "."
        )
      )
    }
  }
}

status <- if (length(errors) == 0) "PASS" else "FAIL"

summary_audit <- data.frame(
  checked_utc = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S UTC",
    tz = "UTC"
  ),
  latest_valid_pdf_sitrep = latest_pdf_sitrep,
  max_indicators_sitrep = max_indicators,
  max_series_sitrep = max_serie,
  cumulative_cases = cases_value,
  cumulative_deaths = deaths_value,
  required_indicators_ok = required_ok,
  status = status,
  details = if (length(errors) == 0) {
    "PDF, indicateurs et série nationale concordent."
  } else {
    paste(errors, collapse = " | ")
  },
  stringsAsFactors = FALSE
)

readr::write_csv(summary_audit, AUDIT_FP)

cat("\n============================================================\n")
cat("PREIS — VALIDATION FRAÎCHEUR PIPELINE\n")
cat("============================================================\n")
print(summary_audit)
cat("Audit :", AUDIT_FP, "\n")

if (length(errors) > 0) {
  stop(
    paste(errors, collapse = "\n"),
    call. = FALSE
  )
}

cat(
  "VALIDATION OK — SitRep N",
  sprintf("%03d", latest_pdf_sitrep),
  " prêt pour le dashboard et l'e-mail enrichi.\n",
  sep = ""
)
