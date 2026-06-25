############################################################
# PREIS EBOLA DRC — INSTALL DEPENDENCIES
############################################################

cran_packages <- c(
  "dplyr", "readr", "stringr", "tibble", "tidyr", "purrr",
  "openxlsx", "glue", "lubridate", "rvest", "httr",
  "pdftools", "base64enc", "xml2", "jsonlite", "fs",
  "shiny", "shinydashboard", "DT", "plotly", "leaflet",
  "blastula", "emayili", "taskscheduleR"
)

missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

message("\nPackages principaux installés/vérifiés.")
message("\nPour extraction avancée des tableaux PDF, installer Java 64-bit puis essayer :")
message("install.packages('remotes')")
message("remotes::install_github('ropensci/tabulizer')")
message("\nSi tabulizer échoue, le pipeline fonctionne quand même avec pdftools + QC, mais l'extraction de tableaux sera moins forte.")
