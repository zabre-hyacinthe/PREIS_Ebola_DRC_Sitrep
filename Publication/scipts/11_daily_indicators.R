## ============================================================
## PREIS EBOLA DRC
## 11_daily_indicators.R
##
## Builds the DAILY indicator series since outbreak onset
## (14 May 2026), at NATIONAL and PROVINCE level, from INRB
## dated data. Saves a tidy CSV used by the dashboard.
##
## Indicators per day:
##   - cumulative confirmed cases / deaths
##   - new confirmed cases / deaths (daily incidence)
##   - provisional CFR (%)
##   - 7-day moving average of new cases
##
## HONEST NOTES:
##   - Some days have no SitRep (gaps) -> series is irregular.
##   - 30 May shows negative new cases (263->238): this is a REAL
##     INRB downward revision (harmonisation), not a bug. Flagged.
##   - Province-level daily series rely on per-zone INRB data summed
##     by province; national totals remain the reference.
##   - CFR is provisional (active outbreak).
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(zoo)
})

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
OUT_DIR  <- file.path(BASE_DIR, "data", "final")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RAW_BASE <- paste0("https://raw.githubusercontent.com/INRB-UMIE/",
                   "BDBV2026-Data/main/data/insp_sitrep/processed/")

# Province assignment for zones (to aggregate province-level daily)
prov_map <- c(
  Bunia="Ituri",Rwampara="Ituri",Mongbwalu="Ituri",Nyankunde="Ituri",Nizi="Ituri",
  Bambu="Ituri",Lita="Ituri",Kilo="Ituri",Aru="Ituri",Damas="Ituri",Rimba="Ituri",
  Komanda="Ituri",Mambasa="Ituri",Mangala="Ituri",Aungba="Ituri",Logo="Ituri",
  Tchomia="Ituri",Gety="Ituri",Kambala="Ituri",`Nia-nia`="Ituri",Fataki="Ituri",Jiba="Ituri",
  Katwa="Nord-Kivu",Beni="Nord-Kivu",Butembo="Nord-Kivu",Oicha="Nord-Kivu",
  Kyondo="Nord-Kivu",Kalunguta="Nord-Kivu",Masereka="Nord-Kivu",Vuhovi="Nord-Kivu",
  Manguredjipa="Nord-Kivu",Karisimbi="Nord-Kivu",Goma="Nord-Kivu",Mabalako="Nord-Kivu",
  `Miti-Murhesa`="Sud-Kivu"
)
canon <- function(x) dplyr::recode(str_squish(x),
  "Mongbalu"="Mongbwalu","Nyakunde"="Nyankunde","Gethy"="Gety", .default=x)

local_or_url <- function(name) {
  lp <- file.path(BASE_DIR, "data", "final", name)
  if (file.exists(lp)) lp else paste0(RAW_BASE, name)
}

# ---- National dated series ----
read_nat <- function(name, col) {
  d <- readr::read_csv(local_or_url(name), show_col_types = FALSE)
  names(d)[names(d) == col] <- "value"
  d %>% transmute(date = as.Date(date), value = suppressWarnings(as.numeric(value))) %>%
    filter(!is.na(value))
}
nat_cas <- read_nat("insp_sitrep__national_cumulative_confirmed_cases__daily.csv",
                    "national_cumulative_confirmed_cases")
nat_dec <- read_nat("insp_sitrep__national_cumulative_confirmed_deaths__daily.csv",
                    "national_cumulative_confirmed_deaths")

national <- full_join(
  nat_cas %>% rename(cum_cases = value),
  nat_dec %>% rename(cum_deaths = value), by = "date") %>%
  arrange(date) %>%
  # add SitRep 29 (13 June) if not present
  bind_rows(if (!as.Date("2026-06-13") %in% .$date)
    tibble(date = as.Date("2026-06-13"), cum_cases = 781, cum_deaths = 179) else NULL) %>%
  arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
  mutate(level = "National", province = "National")

# ---- Province dated series (sum of zones by province) ----
read_zone <- function(name, col) {
  d <- readr::read_csv(local_or_url(name), show_col_types = FALSE)
  names(d)[names(d) == col] <- "value"
  d %>% filter(!nom %in% c("DRC","NA",NA)) %>%
    transmute(zone = canon(nom), date = as.Date(date),
              value = suppressWarnings(as.numeric(value))) %>%
    filter(!is.na(value)) %>%
    group_by(zone, date) %>% summarise(value = max(value), .groups="drop") %>%
    mutate(province = unname(prov_map[zone])) %>%
    filter(!is.na(province))
}
z_cas <- read_zone("insp_sitrep__cumulative_confirmed_cases__daily.csv",
                   "cumulative_confirmed_cases")
z_dec <- read_zone("insp_sitrep__cumulative_confirmed_deaths__daily.csv",
                   "cumulative_confirmed_deaths")

prov_cas <- z_cas %>% group_by(province, date) %>%
  summarise(cum_cases = sum(value), .groups="drop")
prov_dec <- z_dec %>% group_by(province, date) %>%
  summarise(cum_deaths = sum(value), .groups="drop")
province <- full_join(prov_cas, prov_dec, by = c("province","date")) %>%
  arrange(province, date) %>% mutate(level = "Province", zone = NA_character_)

# ---- Zone-level series (only zones with enough data points) ----
# Audit montre que les petites zones n'ont que 2-4 jours : une courbe
# n'aurait pas de sens. Seuil de qualite : >= MIN_DAYS jours de donnees.
MIN_DAYS <- 10
zone <- full_join(
  z_cas %>% select(zone, province, date, cum_cases = value),
  z_dec %>% select(zone, province, date, cum_deaths = value),
  by = c("zone","province","date")) %>%
  arrange(zone, date) %>%
  group_by(zone) %>% filter(dplyr::n() >= MIN_DAYS) %>% ungroup() %>%
  mutate(level = "Zone")
zones_kept <- sort(unique(zone$zone))

# ---- Combine + compute daily indicators ----
add_indics <- function(df) {
  df %>% arrange(date) %>%
    mutate(
      new_cases  = cum_cases  - dplyr::lag(cum_cases),
      new_deaths = cum_deaths - dplyr::lag(cum_deaths),
      cfr        = ifelse(cum_cases > 0, round(100 * cum_deaths / cum_cases, 1), NA),
      revision   = !is.na(new_cases) & new_cases < 0   # flag INRB downward revisions
    ) %>%
    mutate(ma7_new_cases = round(zoo::rollmean(pmax(new_cases, 0), 7,
                                               fill = NA, align = "right"), 1))
}

daily <- bind_rows(
  national %>% mutate(zone = NA_character_) %>%
    select(level, province, zone, date, cum_cases, cum_deaths),
  province %>% select(level, province, zone, date, cum_cases, cum_deaths),
  zone     %>% select(level, province, zone, date, cum_cases, cum_deaths)
) %>%
  group_by(level, province, zone) %>% group_modify(~ add_indics(.x)) %>% ungroup() %>%
  arrange(level, province, zone, date)

out_fp <- file.path(OUT_DIR, "PREIS_daily_indicators.csv")
readr::write_excel_csv(daily, out_fp)

cat("Daily indicators written:", out_fp, "\n")
cat("  Rows:", nrow(daily), "| Levels:", paste(unique(daily$level), collapse=", "), "\n")
cat("  Date range:", as.character(min(daily$date)), "->", as.character(max(daily$date)), "\n")
cat("  Zones with enough data (>=", MIN_DAYS, "days):", length(zones_kept),
    "->", paste(zones_kept, collapse=", "), "\n")
nrev <- sum(daily$revision, na.rm = TRUE)
if (nrev > 0) cat("  NOTE:", nrev, "day(s) with INRB downward revision (negative new cases) - flagged in 'revision' column.\n")
