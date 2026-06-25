############################################################
# PREIS EBOLA DRC
# PowerPoint-ready CFR scatter plot by health zone
# Improved readability: larger bubbles, labels, titles, spacing
# Output:
#   scripts/14_cfr_scatter_health_zone_english_PPT.R
#   outputs/figures/
#   outputs/data/
############################################################

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

# ============================================================
# 1. Project folder
# ============================================================

PROJECT_DIR_MANUAL <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"

is_valid_project_dir <- function(x) {
  if (is.na(x) || !nzchar(x)) return(FALSE)
  
  x <- normalizePath(x, winslash = "/", mustWork = FALSE)
  
  dir.exists(x) &&
    (
      file.exists(file.path(x, "app.R")) ||
        file.exists(file.path(x, "dashboard_ebola", "app.R")) ||
        file.exists(file.path(x, "data", "final", "PREIS_daily_indicators.csv")) ||
        file.exists(file.path(x, "outputs", "analyse", "serie_temporelle_nationale.csv")) ||
        dir.exists(file.path(x, "scripts"))
    )
}

safe_list_dirs <- function(x) {
  tryCatch(
    list.dirs(x, recursive = FALSE, full.names = TRUE),
    error = function(e) character()
  )
}

common_roots <- unique(c(
  Sys.getenv("PREIS_EBOLA_PROJECT_DIR"),
  PROJECT_DIR_MANUAL,
  getwd(),
  dirname(getwd()),
  safe_list_dirs(getwd()),
  safe_list_dirs("D:/"),
  safe_list_dirs("C:/Users/AfricaCDC/OneDrive/Documents")
))

common_roots <- common_roots[!is.na(common_roots) & nzchar(common_roots)]

project_like <- common_roots[
  grepl("PREIS.*Ebola|Ebola.*Sitrep|Ebola.*DRC", basename(common_roots), ignore.case = TRUE)
]

candidate_roots <- unique(c(
  Sys.getenv("PREIS_EBOLA_PROJECT_DIR"),
  PROJECT_DIR_MANUAL,
  getwd(),
  dirname(getwd()),
  project_like
))

valid_roots <- candidate_roots[vapply(candidate_roots, is_valid_project_dir, logical(1))]

if (length(valid_roots) == 0) {
  stop(
    "PREIS Ebola project folder not found.\n",
    "Open the correct PREIS Ebola project in RStudio or correct PROJECT_DIR_MANUAL.\n",
    "Current working directory: ",
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  )
}

ROOT_DIR <- normalizePath(valid_roots[1], winslash = "/", mustWork = TRUE)
setwd(ROOT_DIR)

SCRIPTS_DIR <- file.path(ROOT_DIR, "scripts")
OUTPUT_DIR <- file.path(ROOT_DIR, "outputs")
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
DATA_OUT_DIR <- file.path(OUTPUT_DIR, "data")

dir.create(SCRIPTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_OUT_DIR, recursive = TRUE, showWarnings = FALSE)

SCRIPT_TARGET <- file.path(SCRIPTS_DIR, "14_cfr_scatter_health_zone_english_PPT.R")

cat("\n============================================================\n")
cat("PREIS Ebola DRC — PowerPoint-ready CFR scatter plot\n")
cat("Project folder :", ROOT_DIR, "\n")
cat("Scripts folder :", SCRIPTS_DIR, "\n")
cat("Outputs folder :", OUTPUT_DIR, "\n")
cat("============================================================\n\n")

# ============================================================
# 2. Save script into scripts/
# ============================================================

save_current_script <- function(target_file) {
  saved <- FALSE
  source_file <- NA_character_
  
  for (i in rev(seq_along(sys.frames()))) {
    tmp <- tryCatch(sys.frames()[[i]]$ofile, error = function(e) NULL)
    
    if (!is.null(tmp) && length(tmp) == 1 && nzchar(tmp)) {
      source_file <- normalizePath(tmp, winslash = "/", mustWork = FALSE)
      break
    }
  }
  
  if (!is.na(source_file) && file.exists(source_file)) {
    file.copy(source_file, target_file, overwrite = TRUE)
    saved <- TRUE
  }
  
  if (!saved && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
    
    if (!is.null(ctx) && length(ctx$contents) > 0) {
      marker_found <- any(grepl("PowerPoint-ready CFR scatter plot", ctx$contents, fixed = TRUE))
      if (marker_found) {
        writeLines(ctx$contents, target_file, useBytes = TRUE)
        saved <- TRUE
      }
    }
  }
  
  saved
}

script_saved <- save_current_script(SCRIPT_TARGET)

if (script_saved) {
  cat("Script saved to:", SCRIPT_TARGET, "\n\n")
} else {
  cat("Note: script auto-save was not possible because it was probably pasted directly into the console.\n")
  cat("Expected script path:", SCRIPT_TARGET, "\n\n")
}

# ============================================================
# 3. Packages
# ============================================================

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "plotly",
  "htmlwidgets",
  "htmltools",
  "tibble"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(plotly)
  library(htmlwidgets)
  library(htmltools)
  library(tibble)
})

# ============================================================
# 4. Utility functions
# ============================================================

clean_names_base <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

rename_if_present <- function(d, from, to) {
  for (f in from) {
    if (f %in% names(d) && !(to %in% names(d))) {
      names(d)[match(f, names(d))] <- to
    }
  }
  d
}

parse_date_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  
  if (is.numeric(x)) {
    return(suppressWarnings(as.Date(x, origin = "1899-12-30")))
  }
  
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  
  parsed <- suppressWarnings(as.Date(x))
  
  formats <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%m/%d/%Y",
    "%d-%m-%Y",
    "%Y/%m/%d",
    "%d.%m.%Y"
  )
  
  for (fmt in formats) {
    idx <- is.na(parsed) & !is.na(x)
    if (any(idx)) {
      parsed[idx] <- suppressWarnings(as.Date(x[idx], format = fmt))
    }
  }
  
  parsed
}

to_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("%", "", x)
  x <- gsub("\\s+", "", x)
  
  x <- ifelse(
    grepl(",", x) & !grepl("\\.", x),
    gsub(",", ".", x),
    gsub(",", "", x)
  )
  
  suppressWarnings(as.numeric(x))
}

fix_cfr_scale <- function(x) {
  if (all(is.na(x))) return(x)
  
  max_abs <- suppressWarnings(max(abs(x), na.rm = TRUE))
  
  if (is.finite(max_abs) && max_abs <= 1.5) {
    return(x * 100)
  }
  
  x
}

local_matches <- function(filename) {
  all_files <- tryCatch(
    list.files(ROOT_DIR, recursive = TRUE, full.names = TRUE),
    error = function(e) character()
  )
  
  all_files[basename(all_files) == filename]
}

read_csv_safely <- function(path) {
  tryCatch(
    {
      d <- readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        guess_max = 100000
      )
      
      attr(d, "source_path") <- path
      d
    },
    error = function(e) {
      message("Cannot read: ", path)
      message("Reason     : ", conditionMessage(e))
      tibble()
    }
  )
}

standardize_daily <- function(d) {
  if (nrow(d) == 0) return(d)
  
  names(d) <- clean_names_base(names(d))
  
  d <- rename_if_present(
    d,
    c("sitrep", "sitrep_number", "sitrep_no", "sitrep_num", "no_sitrep"),
    "sitrep_no"
  )
  
  d <- rename_if_present(
    d,
    c("sitrep_date", "report_date", "date_report", "day", "jour"),
    "date"
  )
  
  d <- rename_if_present(
    d,
    c("health_zone", "zone_sante", "zone_de_sante", "zs", "nom", "name", "area", "location"),
    "zone"
  )
  
  d <- rename_if_present(
    d,
    c("province_name", "prov"),
    "province"
  )
  
  d <- rename_if_present(
    d,
    c(
      "cumulative_confirmed_cases",
      "cum_confirmed_cases",
      "confirmed_cases_cumulative",
      "total_confirmed_cases",
      "total_cases",
      "cases",
      "cas",
      "cas_confirmes_cumules"
    ),
    "cum_cases"
  )
  
  d <- rename_if_present(
    d,
    c(
      "cumulative_deaths",
      "cum_confirmed_deaths",
      "deaths_cumulative",
      "total_deaths",
      "deaths",
      "deces",
      "deces_cumules"
    ),
    "cum_deaths"
  )
  
  d <- rename_if_present(
    d,
    c(
      "case_fatality_ratio",
      "case_fatality_rate",
      "cfr_percent",
      "letalite",
      "letalite_cfr"
    ),
    "cfr"
  )
  
  if (!"date" %in% names(d)) d$date <- as.Date(NA)
  if (!"level" %in% names(d)) d$level <- NA_character_
  if (!"province" %in% names(d)) d$province <- NA_character_
  if (!"zone" %in% names(d)) d$zone <- NA_character_
  if (!"cum_cases" %in% names(d)) d$cum_cases <- NA_real_
  if (!"cum_deaths" %in% names(d)) d$cum_deaths <- NA_real_
  if (!"cfr" %in% names(d)) d$cfr <- NA_real_
  if (!"sitrep_no" %in% names(d)) d$sitrep_no <- NA_real_
  
  d %>%
    mutate(
      date = parse_date_any(date),
      sitrep_no = to_number(sitrep_no),
      level = str_squish(as.character(level)),
      province = str_squish(as.character(province)),
      zone = str_squish(as.character(zone)),
      cum_cases = to_number(cum_cases),
      cum_deaths = to_number(cum_deaths),
      cfr = fix_cfr_scale(to_number(cfr)),
      cfr = ifelse(
        is.na(cfr) & !is.na(cum_cases) & cum_cases > 0 & !is.na(cum_deaths),
        100 * cum_deaths / cum_cases,
        cfr
      )
    )
}

standardize_serie <- function(d) {
  if (nrow(d) == 0) return(d)
  
  names(d) <- clean_names_base(names(d))
  
  d <- rename_if_present(
    d,
    c("sitrep", "sitrep_number", "sitrep_no", "sitrep_num", "no_sitrep"),
    "sitrep_no"
  )
  
  d <- rename_if_present(
    d,
    c("sitrep_date", "report_date", "date_report", "day", "jour"),
    "date"
  )
  
  if (!"date" %in% names(d)) d$date <- as.Date(NA)
  if (!"sitrep_no" %in% names(d)) d$sitrep_no <- NA_real_
  
  d %>%
    mutate(
      date = parse_date_any(date),
      sitrep_no = to_number(sitrep_no)
    )
}

choose_best_dataset <- function(candidates, standardizer, label) {
  candidates <- unique(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  
  data_list <- list()
  
  meta <- tibble(
    index = integer(),
    source = character(),
    rows = integer(),
    max_sitrep = numeric(),
    max_date = as.Date(character()),
    score = numeric()
  )
  
  for (path in candidates) {
    d_raw <- read_csv_safely(path)
    if (nrow(d_raw) == 0) next
    
    d <- standardizer(d_raw)
    if (nrow(d) == 0) next
    
    max_sitrep <- if ("sitrep_no" %in% names(d) && any(!is.na(d$sitrep_no))) {
      max(d$sitrep_no, na.rm = TRUE)
    } else {
      NA_real_
    }
    
    max_date_num <- if ("date" %in% names(d) && any(!is.na(d$date))) {
      max(as.numeric(d$date), na.rm = TRUE)
    } else {
      NA_real_
    }
    
    max_date <- if (!is.na(max_date_num)) {
      as.Date(max_date_num, origin = "1970-01-01")
    } else {
      as.Date(NA)
    }
    
    score <- if (!is.na(max_sitrep)) {
      max_sitrep * 100000 + ifelse(is.na(max_date_num), 0, max_date_num)
    } else if (!is.na(max_date_num)) {
      max_date_num
    } else {
      nrow(d)
    }
    
    data_list[[length(data_list) + 1]] <- d
    
    meta <- bind_rows(
      meta,
      tibble(
        index = length(data_list),
        source = path,
        rows = nrow(d),
        max_sitrep = max_sitrep,
        max_date = max_date,
        score = score
      )
    )
  }
  
  if (length(data_list) == 0) {
    stop(
      "No usable dataset found for: ", label, "\n",
      "Check local files, outputs, or GitHub raw path."
    )
  }
  
  chosen <- which.max(meta$score)
  chosen_data <- data_list[[chosen]]
  attr(chosen_data, "source_path") <- meta$source[chosen]
  
  cat("\nSelected dataset:", label, "\n")
  cat("Source          :", meta$source[chosen], "\n")
  cat("Rows            :", meta$rows[chosen], "\n")
  cat("Max SitRep      :", meta$max_sitrep[chosen], "\n")
  cat("Max date        :", as.character(meta$max_date[chosen]), "\n\n")
  
  chosen_data
}

normalize_province_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[-_]+", " ", x)
  x <- str_squish(x)
  x
}

make_case_ticks <- function(xmin, xmax) {
  ticks <- c(
    1:10,
    seq(20, 90, by = 10),
    seq(100, 900, by = 100),
    seq(1000, 9000, by = 1000)
  )
  
  ticks <- ticks[ticks >= max(1, floor(xmin * 0.7)) & ticks <= ceiling(xmax * 1.4)]
  
  if (length(ticks) == 0) {
    ticks <- pretty(c(xmin, xmax))
    ticks <- ticks[ticks > 0]
  }
  
  ticks
}

# ============================================================
# 5. Load latest available datasets
# ============================================================

GH_RAW <- Sys.getenv(
  "PREIS_GH_RAW_BASE",
  "https://raw.githubusercontent.com/zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep/refs/heads/main"
)

daily_candidates <- unique(c(
  file.path(ROOT_DIR, "data", "PREIS_daily_indicators.csv"),
  file.path(ROOT_DIR, "data", "final", "PREIS_daily_indicators.csv"),
  file.path(ROOT_DIR, "dashboard_ebola", "data", "PREIS_daily_indicators.csv"),
  file.path(ROOT_DIR, "outputs", "data", "PREIS_daily_indicators.csv"),
  local_matches("PREIS_daily_indicators.csv"),
  paste0(GH_RAW, "/data/final/PREIS_daily_indicators.csv")
))

serie_candidates <- unique(c(
  file.path(ROOT_DIR, "outputs", "analyse", "serie_temporelle_nationale.csv"),
  file.path(ROOT_DIR, "data", "serie_temporelle_nationale.csv"),
  file.path(ROOT_DIR, "dashboard_ebola", "outputs", "analyse", "serie_temporelle_nationale.csv"),
  local_matches("serie_temporelle_nationale.csv"),
  paste0(GH_RAW, "/outputs/analyse/serie_temporelle_nationale.csv")
))

daily_all <- choose_best_dataset(
  candidates = daily_candidates,
  standardizer = standardize_daily,
  label = "PREIS_daily_indicators.csv"
)

serie_all <- choose_best_dataset(
  candidates = serie_candidates,
  standardizer = standardize_serie,
  label = "serie_temporelle_nationale.csv"
)

# ============================================================
# 6. Detect latest SitRep and cutoff date
# ============================================================

latest_sitrep <- if (nrow(serie_all) > 0 && any(!is.na(serie_all$sitrep_no))) {
  max(serie_all$sitrep_no, na.rm = TRUE)
} else {
  NA_real_
}

latest_sitrep_date <- if (nrow(serie_all) > 0 && any(!is.na(serie_all$date))) {
  max(serie_all$date, na.rm = TRUE)
} else {
  as.Date(NA)
}

latest_daily_date <- if (nrow(daily_all) > 0 && any(!is.na(daily_all$date))) {
  max(daily_all$date, na.rm = TRUE)
} else {
  as.Date(NA)
}

selected_cutoff_date <- latest_sitrep_date

if (is.na(selected_cutoff_date)) {
  selected_cutoff_date <- latest_daily_date
}

if (is.na(selected_cutoff_date)) {
  stop("No valid date found in PREIS datasets.")
}

sitrep_suffix <- if (!is.na(latest_sitrep) && is.finite(latest_sitrep)) {
  paste0("SR", latest_sitrep)
} else {
  format(selected_cutoff_date, "%Y%m%d")
}

cat("Latest SitRep detected :", latest_sitrep, "\n")
cat("Latest SitRep date     :", as.character(latest_sitrep_date), "\n")
cat("Latest daily data date :", as.character(latest_daily_date), "\n")
cat("Cutoff date used       :", as.character(selected_cutoff_date), "\n\n")

# ============================================================
# 7. Prepare health-zone data
# ============================================================

zone_levels <- c(
  "zone",
  "health zone",
  "zone de sante",
  "zone de santé",
  "zs"
)

zlast <- daily_all %>%
  filter(!is.na(date), date <= selected_cutoff_date) %>%
  filter(tolower(level) %in% zone_levels) %>%
  mutate(
    zone = ifelse(is.na(zone) | zone == "", "Unspecified health zone", zone),
    province = ifelse(is.na(province) | province == "", "Unspecified province", province)
  ) %>%
  group_by(zone, province) %>%
  arrange(date, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    cum_cases = to_number(cum_cases),
    cum_deaths = to_number(cum_deaths),
    cfr = fix_cfr_scale(to_number(cfr)),
    cfr = ifelse(
      is.na(cfr) & !is.na(cum_cases) & cum_cases > 0 & !is.na(cum_deaths),
      100 * cum_deaths / cum_cases,
      cfr
    )
  ) %>%
  filter(!is.na(cum_cases), cum_cases > 0) %>%
  filter(!is.na(cfr)) %>%
  mutate(
    log_cases = log10(cum_cases)
  )

if (nrow(zlast) == 0) {
  available_levels <- paste(sort(unique(daily_all$level)), collapse = " | ")
  
  stop(
    "No usable health-zone CFR data found.\n",
    "Available levels: ", available_levels
  )
}

# ============================================================
# 8. National CFR reference
# ============================================================

national_levels <- c(
  "national",
  "country",
  "rdc",
  "drc",
  "democratic republic of the congo"
)

natl <- daily_all %>%
  filter(!is.na(date), date <= selected_cutoff_date) %>%
  filter(tolower(level) %in% national_levels) %>%
  arrange(date) %>%
  slice_tail(n = 1)

cfr_nat <- NA_real_
total_cases <- NA_real_
total_deaths <- NA_real_

if (nrow(natl) > 0) {
  cfr_nat <- natl$cfr[1]
  total_cases <- natl$cum_cases[1]
  total_deaths <- natl$cum_deaths[1]
  
  if ((is.na(cfr_nat) || !is.finite(cfr_nat)) &&
      !is.na(total_cases) && total_cases > 0 &&
      !is.na(total_deaths)) {
    cfr_nat <- 100 * total_deaths / total_cases
  }
}

if (is.na(cfr_nat) || !is.finite(cfr_nat)) {
  zone_case_sum <- sum(zlast$cum_cases, na.rm = TRUE)
  zone_death_sum <- sum(zlast$cum_deaths, na.rm = TRUE)
  
  if (zone_case_sum > 0) {
    cfr_nat <- 100 * zone_death_sum / zone_case_sum
  }
}

if (is.na(total_cases) || !is.finite(total_cases)) {
  total_cases <- sum(zlast$cum_cases, na.rm = TRUE)
}

if (is.na(total_deaths) || !is.finite(total_deaths)) {
  total_deaths <- sum(zlast$cum_deaths, na.rm = TRUE)
}

# ============================================================
# 9. Colors, bubbles and labels
# ============================================================

AU_RED <- "#E31C23"
AU_GREEN <- "#00843E"
AU_GOLD <- "#F0B323"

province_palette <- c(
  "ituri" = AU_RED,
  "nord kivu" = AU_GREEN,
  "north kivu" = AU_GREEN,
  "sud kivu" = AU_GOLD,
  "south kivu" = AU_GOLD
)

province_keys <- normalize_province_key(zlast$province)

zlast$color <- province_palette[province_keys]
zlast$color[is.na(zlast$color)] <- AU_RED

# All CTE / health-zone points must be labelled.
# The original script labelled only a short priority list; this version
# keeps the same data, colors and bubbles, but uses every zone name as label.

zlast <- zlast %>%
  arrange(desc(cum_cases), desc(cfr), zone) %>%
  mutate(
    label_graph = zone,
    bubble_size = pmin(12 + sqrt(cum_cases) * 3.6, 64),
    hover_text = paste0(
      "<b>", zone, "</b>",
      "<br>Province: ", province,
      "<br>Cumulative confirmed cases: ", format(cum_cases, big.mark = " "),
      "<br>Cumulative deaths: ", ifelse(
        is.na(cum_deaths),
        "NA",
        format(cum_deaths, big.mark = " ")
      ),
      "<br>Provisional CFR: ", round(cfr, 1), "%",
      "<br>Latest zone data date: ", as.character(date),
      "<extra></extra>"
    )
  )

# Tighter label placement for all CTE / health-zone names.
# Objective: keep every point labelled, keep names close to the bubble,
# and reduce overlaps without changing the plotted data.

label_position_table <- tibble::tribble(
  ~zone,          ~xshift_manual, ~yshift_manual, ~xanchor_manual, ~yanchor_manual,
  "Beni",                     0,             24, "center",       "bottom",
  "Katwa",                    0,             24, "center",       "bottom",
  "Butembo",                  0,             24, "center",       "bottom",
  "Mongbwalu",                0,             30, "center",       "bottom",
  "Rwampara",               -26,             20, "right",        "bottom",
  "Bunia",                   28,             20, "left",         "bottom",
  "Nyankunde",                0,             26, "center",       "bottom",
  "Nizi",                     0,             18, "center",       "bottom",
  "Lita",                     0,            -18, "center",       "top",
  "Komanda",                 18,             17, "left",         "bottom",
  "Bambu",                  -20,             14, "right",        "bottom",
  "Kilo",                    15,            -12, "left",         "top",
  "Aungba",                  10,            -16, "left",         "top",
  "Miti-Murhesa",            15,             13, "left",         "bottom",
  "Aru",                      0,             18, "center",       "bottom",
  "Mangala",                -18,              0, "right",        "middle",
  "Oicha",                    0,            -16, "center",       "top",
  "Kalunguta",              -10,            -16, "right",        "top",
  "Mambasa",                 17,             -5, "left",         "middle",
  "Kyondo",                 -22,             10, "right",        "bottom",
  "Goma",                     0,             22, "center",       "bottom",
  "Gety",                     0,            -22, "center",       "top",
  "Rimba",                   18,             15, "left",         "bottom",
  "Logo",                   -18,            -18, "right",        "top",
  "Damas",                    0,            -22, "center",       "top"
)

# Generic fallback for future SitReps or new health zones.
# Offsets are intentionally short so labels stay close to their point.
auto_label_positions <- tibble::tribble(
  ~label_slot, ~xshift_auto, ~yshift_auto, ~xanchor_auto, ~yanchor_auto,
            1,            0,           18, "center",     "bottom",
            2,           20,           12, "left",       "bottom",
            3,          -20,           12, "right",      "bottom",
            4,           24,            0, "left",       "middle",
            5,          -24,            0, "right",      "middle",
            6,           18,          -12, "left",       "top",
            7,          -18,          -12, "right",      "top",
            8,            0,          -18, "center",     "top"
)

label_rows <- zlast %>%
  mutate(
    label_rank = row_number(),
    label_slot = ((label_rank - 1) %% nrow(auto_label_positions)) + 1
  ) %>%
  left_join(auto_label_positions, by = "label_slot") %>%
  left_join(label_position_table, by = "zone") %>%
  mutate(
    # Keep labels close, but for large bubbles move them just outside
    # the bubble edge so the name remains readable.
    min_vertical_offset = pmin(30, pmax(14, bubble_size / 2 + 4)),
    xshift = dplyr::coalesce(xshift_manual, xshift_auto),
    yshift = dplyr::coalesce(yshift_manual, yshift_auto),
    yshift = dplyr::case_when(
      is.na(yshift) ~ 18,
      abs(xshift) <= 6 & yshift > 0 ~ pmax(yshift, min_vertical_offset),
      abs(xshift) <= 6 & yshift < 0 ~ -pmax(abs(yshift), min_vertical_offset),
      TRUE ~ yshift
    ),
    xanchor = dplyr::coalesce(xanchor_manual, xanchor_auto),
    yanchor = dplyr::coalesce(yanchor_manual, yanchor_auto),
    label_text = zone
  )

# Strict quality check: every plotted point must have a non-empty label.
missing_labels <- label_rows %>%
  filter(is.na(label_text) | !nzchar(label_text))

if (nrow(missing_labels) > 0) {
  stop("Some plotted health-zone points do not have labels. Please check the zone variable.")
}

# ============================================================
# 10. Build PowerPoint-ready chart
# ============================================================

x_min_case <- min(zlast$cum_cases, na.rm = TRUE)
x_max_case <- max(zlast$cum_cases, na.rm = TRUE)

x_left <- min(zlast$log_cases, na.rm = TRUE) - 0.18
x_right <- max(zlast$log_cases, na.rm = TRUE) + 0.38

case_ticks <- make_case_ticks(x_min_case, x_max_case)
case_ticks <- case_ticks[case_ticks > 0]

y_max <- max(zlast$cfr, cfr_nat, na.rm = TRUE)
y_upper <- max(90, ceiling(y_max / 10) * 10 + 8)

shapes_cfr <- list()
annotations_cfr <- list()

if (!is.na(cfr_nat) && is.finite(cfr_nat)) {
  shapes_cfr <- list(
    list(
      type = "line",
      x0 = x_left,
      x1 = x_right,
      y0 = cfr_nat,
      y1 = cfr_nat,
      xref = "x",
      yref = "y",
      line = list(
        dash = "dash",
        color = "grey",
        width = 1.5
      )
    )
  )
  
  annotations_cfr <- list(
    list(
      x = x_right,
      y = cfr_nat + 2.2,
      xref = "x",
      yref = "y",
      text = paste0("National CFR: ", round(cfr_nat, 1), "%"),
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "bottom",
      font = list(
        size = 16,
        color = "grey",
        family = "Arial"
      )
    )
  )
}

plot_subtitle <- paste0(
  "DRC Ebola — ",
  ifelse(!is.na(latest_sitrep), paste0("SitRep ", latest_sitrep), "latest available SitRep"),
  " — data up to ",
  format(selected_cutoff_date, "%d %b %Y")
)

p <- plot_ly(
  data = zlast,
  x = ~log_cases,
  y = ~cfr,
  type = "scatter",
  mode = "markers",
  cliponaxis = FALSE,
  marker = list(
    size = ~bubble_size,
    sizemode = "diameter",
    color = ~color,
    opacity = 0.88,
    line = list(
      color = "white",
      width = 1.5
    )
  ),
  hovertext = ~hover_text,
  hoverinfo = "text"
) %>%
  layout(
    xaxis = list(
      title = list(
        text = "Cumulative confirmed cases (log scale)",
        font = list(size = 20)
      ),
      range = c(x_left, x_right),
      tickvals = log10(case_ticks),
      ticktext = as.character(case_ticks),
      tickfont = list(size = 15),
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.08)",
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(
        text = "Provisional case fatality ratio (%)",
        font = list(size = 20)
      ),
      range = c(0, y_upper),
      tickfont = list(size = 15),
      ticksuffix = "%",
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.08)",
      zeroline = TRUE,
      zerolinecolor = "rgba(0,0,0,0.45)"
    ),
    shapes = shapes_cfr,
    annotations = annotations_cfr,
    showlegend = FALSE,
    margin = list(t = 38, r = 150, b = 105, l = 135),
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    font = list(
      family = "Arial, Helvetica, sans-serif",
      color = "#2C3E50"
    ),
    hoverlabel = list(
      align = "left",
      bgcolor = "white",
      bordercolor = "#CCCCCC",
      font = list(size = 14, color = "#2C3E50")
    ),
    height = 760
  ) %>%
  config(
    displayModeBar = TRUE,
    responsive = TRUE
  )

if (nrow(label_rows) > 0) {
  for (i in seq_len(nrow(label_rows))) {
    p <- p %>%
      add_annotations(
        x = label_rows$log_cases[i],
        y = label_rows$cfr[i],
        text = label_rows$zone[i],
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        xshift = label_rows$xshift[i],
        yshift = label_rows$yshift[i],
        xanchor = label_rows$xanchor[i],
        yanchor = label_rows$yanchor[i],
        font = list(
          size = 15,
          color = "#1F3347",
          family = "Arial"
        ),
        bgcolor = "rgba(255,255,255,0.62)",
        bordercolor = "rgba(255,255,255,0.00)",
        borderpad = 0
      )
  }
}

# ============================================================
# 11. Add dashboard-style header
# ============================================================

header_html <- htmltools::tags$div(
  style = paste0(
    "background:#C0392B;",
    "color:white;",
    "font-weight:700;",
    "font-size:24px;",
    "padding:12px 14px;",
    "border-radius:3px 3px 0 0;",
    "font-family:Arial, Helvetica, sans-serif;"
  ),
  "Provisional case fatality ratio vs cumulative confirmed cases by health zone"
)

subtitle_html <- htmltools::tags$div(
  style = paste0(
    "background:#FFFFFF;",
    "color:#555555;",
    "font-size:18px;",
    "padding:10px 14px 4px 14px;",
    "font-family:Arial, Helvetica, sans-serif;"
  ),
  plot_subtitle
)

p_final <- htmlwidgets::prependContent(
  p,
  header_html,
  subtitle_html
)

# ============================================================
# 12. Save outputs
# ============================================================

html_file <- file.path(
  FIGURES_DIR,
  paste0("cfr_scatter_health_zone_english_PPT_", sitrep_suffix, ".html")
)

csv_file <- file.path(
  DATA_OUT_DIR,
  paste0("cfr_scatter_health_zone_english_PPT_data_", sitrep_suffix, ".csv")
)

rds_file <- file.path(
  DATA_OUT_DIR,
  paste0("cfr_scatter_health_zone_english_PPT_plotly_", sitrep_suffix, ".rds")
)

png_file <- file.path(
  FIGURES_DIR,
  paste0("cfr_scatter_health_zone_english_PPT_", sitrep_suffix, ".png")
)

zlast_export <- zlast %>%
  arrange(desc(cum_cases)) %>%
  select(
    province,
    health_zone = zone,
    latest_zone_data_date = date,
    cumulative_confirmed_cases = cum_cases,
    cumulative_deaths = cum_deaths,
    provisional_cfr_percent = cfr,
    log10_cumulative_cases = log_cases,
    bubble_size,
    label = label_graph,
    color
  )

readr::write_csv(zlast_export, csv_file)
saveRDS(p, rds_file)

save_ok <- FALSE

tryCatch(
  {
    htmlwidgets::saveWidget(
      p_final,
      file = html_file,
      selfcontained = TRUE,
      title = "Provisional CFR vs cumulative confirmed cases by health zone"
    )
    save_ok <- TRUE
  },
  error = function(e) {
    message("Self-contained HTML save failed. Retrying with selfcontained = FALSE.")
    message("Reason: ", conditionMessage(e))
  }
)

if (!save_ok) {
  htmlwidgets::saveWidget(
    p_final,
    file = html_file,
    selfcontained = FALSE,
    title = "Provisional CFR vs cumulative confirmed cases by health zone"
  )
}

# PNG export for PowerPoint
# This section saves a high-resolution PNG version of the chart.
png_status <- "PNG not created"

if (!requireNamespace("webshot2", quietly = TRUE)) {
  tryCatch(
    install.packages("webshot2", dependencies = TRUE),
    error = function(e) NULL
  )
}

html_for_png <- paste0("file:///", normalizePath(html_file, winslash = "/", mustWork = FALSE))

if (requireNamespace("webshot2", quietly = TRUE)) {
  tryCatch(
    {
      webshot2::webshot(
        url = html_for_png,
        file = png_file,
        vwidth = 2400,
        vheight = 1350,
        zoom = 2,
        delay = 1
      )
      png_status <- "PNG created successfully"
    },
    error = function(e) {
      png_status <- paste("PNG export failed:", conditionMessage(e))
    }
  )
}

# ============================================================
# 13. Final quality control
# ============================================================

qc_status <- "OK"

if (!is.na(latest_sitrep_date) && !is.na(latest_daily_date) && latest_daily_date < latest_sitrep_date) {
  qc_status <- "OK with warning: zone-level daily data are older than the latest SitRep date"
}

cat("\n============================================================\n")
cat("POWERPOINT-READY CFR SCATTER PLOT CREATED SUCCESSFULLY\n")
cat("QC status                  :", qc_status, "\n")
cat("Latest SitRep detected     :", latest_sitrep, "\n")
cat("Latest SitRep date         :", as.character(latest_sitrep_date), "\n")
cat("Latest zone data date      :", as.character(latest_daily_date), "\n")
cat("Cutoff date used           :", as.character(selected_cutoff_date), "\n")
cat("Health zones plotted       :", nrow(zlast), "\n")
cat("Health zones labelled      :", nrow(label_rows), "\n")
cat("National CFR               :", ifelse(is.na(cfr_nat), "NA", paste0(round(cfr_nat, 1), "%")), "\n")
cat("Total confirmed cases      :", format(total_cases, big.mark = " "), "\n")
cat("Total deaths               :", format(total_deaths, big.mark = " "), "\n")
cat("Script target              :", SCRIPT_TARGET, "\n")
cat("HTML output                :", html_file, "\n")
cat("PNG output                 :", png_file, "\n")
cat("PNG status                 :", png_status, "\n")
cat("CSV output                 :", csv_file, "\n")
cat("Plotly RDS                 :", rds_file, "\n")
cat("============================================================\n\n")

print(p_final)

if (interactive()) {
  browseURL(html_file)
}



