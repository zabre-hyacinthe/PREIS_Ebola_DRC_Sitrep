# =============================================================================
# PREIS — COUCHE D'ADAPTATION MODULE EBOLA DRC
# Fichier : 00_preis_adapter_ebola.R
# Auteur  : Dr R. Hyacinthe ZABRE — Africa CDC
# Version : 1.0 — 16 juin 2026
#
# RÔLE DE CE SCRIPT
# -----------------
# Transformer les fichiers de sortie du module Ebola DRC (format spécifique)
# vers le FORMAT SOCLE COMMUN PREIS, utilisable par tous les modules et
# lisible directement par le dashboard unifié multi-modules.
#
# Ce script est LA COUCHE D'ADAPTATION du module Ebola.
# Il ne modifie AUCUN fichier source Ebola.
# Il produit 4 fichiers standardisés dans data/final/preis_common/ :
#   - preis_series.csv    (série temporelle nationale, format long)
#   - preis_zones.csv     (détail géographique par zone de santé)
#   - preis_signals.csv   (signaux d'alerte détectés)
#   - preis_meta.json     (fraîcheur, statut système, métadonnées)
#
# PRINCIPE : seule cette couche d'adaptation change si le format Ebola évolue.
#            Le dashboard et les autres modules ne lisent que le format commun.
# =============================================================================


# --------------------------------------------------------------------------- #
# 0. CONFIGURATION ET CHEMINS
# --------------------------------------------------------------------------- #

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(lubridate)
})

# Détection automatique du répertoire racine (local ou cloud GitHub Actions)
BASE_DIR <- Sys.getenv("PREIS_ROOT", getwd())

# Chemins sources (fichiers produits par le pipeline Ebola)
PATH_INDICATORS_LONG <- file.path(BASE_DIR, "data/final/PREIS_indicators_long.csv")
PATH_INDICATORS_VALID <- file.path(BASE_DIR, "data/final/PREIS_indicators_validated.csv")
PATH_SITREP_REGISTRY  <- file.path(BASE_DIR, "data/final/sitrep_registry.csv")
PATH_ZONES            <- file.path(BASE_DIR, "dashboard_ebola/data/tableau_zones_sante.csv")
PATH_SIGNALS          <- file.path(BASE_DIR, "data/final/PREIS_signals.csv")
PATH_HEALTH_ZONES     <- file.path(BASE_DIR, "data/final/PREIS_health_zones.csv")
PATH_DAILY            <- file.path(BASE_DIR, "data/final/PREIS_daily_indicators.csv")

# Dossier de sortie format commun
DIR_COMMON <- file.path(BASE_DIR, "data/final/preis_common")
if (!dir.exists(DIR_COMMON)) dir.create(DIR_COMMON, recursive = TRUE)

# Constantes du module
MODULE_ID   <- "ebola_drc"
SOURCE_NAME <- "INSP/INRB"
GEO_COUNTRY <- "DRC"
GEO_CODE    <- "COD"   # ISO3166-1 alpha-3

cat("=== PREIS Adapter — Module:", MODULE_ID, "===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"), "\n\n")


# --------------------------------------------------------------------------- #
# 1. FONCTIONS UTILITAIRES
# --------------------------------------------------------------------------- #

# Vérifie qu'un fichier source existe avant de le lire
read_safe <- function(path, ...) {
  if (!file.exists(path)) {
    warning("Fichier source introuvable, ignoré : ", path)
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE, ...)
}

# Déduit le type de valeur depuis le domaine Ebola
# (cumulative, incident, proportion, score, binary)
map_value_type <- function(domain, indicator_code) {
  dplyr::case_when(
    grepl("cfr|fatality|lethality|proportion|rate|pct|pct", 
          indicator_code, ignore.case = TRUE)          ~ "proportion",
    grepl("new_|nouveaux_|incident", 
          indicator_code, ignore.case = TRUE)          ~ "incident",
    grepl("signal|alert|score", 
          indicator_code, ignore.case = TRUE)          ~ "score",
    domain %in% c("reference", "deaths", "cases", 
                  "surveillance")                      ~ "cumulative",
    TRUE                                               ~ "cumulative"
  )
}

# Normalise le niveau de signal vers les valeurs du socle commun
# Entrée : severity Ebola (high, moderate, low, …)
# Sortie : none | info | moderate | high | critical
map_signal_level <- function(severity) {
  dplyr::case_when(
    is.na(severity)                                    ~ "none",
    tolower(severity) %in% c("critical", "critique")  ~ "critical",
    tolower(severity) %in% c("high", "élevé", "eleve","elevé") ~ "high",
    tolower(severity) %in% c("moderate", "modéré", "modere") ~ "moderate",
    tolower(severity) %in% c("low", "faible", "info") ~ "info",
    TRUE                                               ~ "info"
  )
}

cat("Fonctions utilitaires chargées.\n")


# --------------------------------------------------------------------------- #
# 2. CHARGEMENT DES SOURCES
# --------------------------------------------------------------------------- #

cat("\n-- Chargement des fichiers sources --\n")

indicators_long  <- read_safe(PATH_INDICATORS_LONG)
indicators_valid <- read_safe(PATH_INDICATORS_VALID)
sitrep_registry  <- read_safe(PATH_SITREP_REGISTRY)
zones_raw        <- read_safe(PATH_ZONES)
signals_raw      <- read_safe(PATH_SIGNALS)
daily_raw        <- read_safe(PATH_DAILY)

# Table de correspondance sitrep_no → date + URL (depuis le registre)
if (!is.null(sitrep_registry)) {
  sitrep_lookup <- sitrep_registry %>%
    mutate(
      sitrep_no   = as.integer(sitrep_no),
      report_date = as.Date(date_raw, tryFormats = c("%d-%m-%Y", "%Y-%m-%d")),
      report_id   = paste0("sitrep_", sitrep_no),
      source_url  = coalesce(pdf_url, post_url, NA_character_)
    ) %>%
    select(sitrep_no, report_id, report_date, source_url) %>%
    distinct()
  
  # Dernier sitrep connu
  last_sitrep    <- sitrep_lookup %>% filter(!is.na(report_date)) %>%
    arrange(desc(report_date)) %>% slice(1)
  last_report_id <- last_sitrep$report_id
  last_date      <- last_sitrep$report_date
  last_url       <- last_sitrep$source_url
  
  cat("Dernier SitRep détecté :", last_report_id, 
      "(", format(last_date), ")\n")
} else {
  sitrep_lookup  <- tibble(sitrep_no = integer(), report_id = character(),
                           report_date = as.Date(character()), 
                           source_url = character())
  last_report_id <- NA_character_
  last_date      <- NA
  last_url       <- NA_character_
  warning("Registre SitRep introuvable — métadonnées partielles.")
}


# --------------------------------------------------------------------------- #
# 3. PRODUCTION DE preis_series.csv
#    Format long unifié : une ligne par (module, report_id, geo, indicator)
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_series.csv --\n")

# Priorité : indicateurs validés > indicateurs longs bruts
indicators_source <- if (!is.null(indicators_valid)) {
  cat("  Source : PREIS_indicators_validated.csv\n")
  indicators_valid
} else if (!is.null(indicators_long)) {
  cat("  Source : PREIS_indicators_long.csv (non validé)\n")
  indicators_long
} else {
  cat("  AVERTISSEMENT : aucune source d'indicateurs disponible.\n")
  NULL
}

if (!is.null(indicators_source)) {
  
  preis_series <- indicators_source %>%
    # Jointure avec le registre pour obtenir date et URL
    mutate(sitrep_no = as.integer(sitrep_no)) %>%
    left_join(sitrep_lookup, by = "sitrep_no") %>%
    # Ajout des colonnes du socle commun
    mutate(
      module       = MODULE_ID,
      source       = SOURCE_NAME,
      geo_level    = "national",
      geo_name     = GEO_COUNTRY,
      geo_code     = GEO_CODE,
      indicator    = indicator_code,
      value_type   = map_value_type(
        if ("domain" %in% names(.)) domain else NA_character_,
        indicator_code),
      signal_level = "none",
      provisional  = TRUE,
      extracted_at = Sys.time()
    ) %>%
    # Sélection et ordre des colonnes du socle commun
    select(
      module, source, report_id, report_date,
      geo_level, geo_name, geo_code,
      indicator, value, value_type,
      signal_level, provisional,
      source_url,
      extracted_at
    ) %>%
    # Suppression des lignes sans valeur numérique
    filter(!is.na(value)) %>%
    # Déduplication sur la clé primaire
    distinct(module, report_id, geo_name, indicator, .keep_all = TRUE)
  
  # Complément depuis PREIS_daily_indicators.csv (niveau national avec dates)
  if (!is.null(daily_raw)) {
    daily_long <- daily_raw %>%
      filter(level == "National") %>%
      mutate(
        report_date = as.Date(date),
        report_id   = paste0("daily_", format(report_date, "%Y%m%d")),
        source_url  = NA_character_,
        extracted_at = Sys.time()
      ) %>%
      # Pivot vers le format long
      tidyr::pivot_longer(
        cols      = c(cum_cases, cum_deaths, new_cases, new_deaths, cfr,
                      ma7_new_cases),
        names_to  = "indicator",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) %>%
      mutate(
        module       = MODULE_ID,
        source       = SOURCE_NAME,
        geo_level    = "national",
        geo_name     = GEO_COUNTRY,
        geo_code     = GEO_CODE,
        value_type   = map_value_type(NA_character_, indicator),
        signal_level = "none",
        provisional  = TRUE
      ) %>%
      select(module, source, report_id, report_date,
             geo_level, geo_name, geo_code,
             indicator, value, value_type,
             signal_level, provisional,
             source_url, extracted_at)
    
    # Fusion : les indicateurs validés ont priorité sur daily
    preis_series <- bind_rows(preis_series, daily_long) %>%
      distinct(module, report_id, geo_name, indicator, .keep_all = TRUE)
    
    cat("  Complément daily_indicators intégré.\n")
  }
  
  out_series <- file.path(DIR_COMMON, "preis_series.csv")
  write_csv(preis_series, out_series)
  cat("  preis_series.csv écrit :", nrow(preis_series), "lignes →", out_series, "\n")
  
} else {
  preis_series <- NULL
  cat("  preis_series.csv NON produit (données sources manquantes).\n")
}


# --------------------------------------------------------------------------- #
# 4. PRODUCTION DE preis_zones.csv
#    Détail géographique par zone de santé
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_zones.csv --\n")

if (!is.null(zones_raw)) {
  
  # tableau_zones_sante.csv : colonnes nom | cas
  preis_zones <- zones_raw %>%
    rename(geo_name = nom,
           value    = cas) %>%
    mutate(
      module       = MODULE_ID,
      source       = SOURCE_NAME,
      report_id    = last_report_id,
      report_date  = last_date,
      geo_level    = "zone",
      geo_code     = NA_character_,    # À enrichir ultérieurement via geoboundaries
      indicator    = "cases_cumulative",
      value        = as.numeric(value),
      value_type   = "cumulative",
      signal_level = "none",
      provisional  = TRUE,
      source_url   = last_url,
      extracted_at = Sys.time()
    ) %>%
    select(module, source, report_id, report_date,
           geo_level, geo_name, geo_code,
           indicator, value, value_type,
           signal_level, provisional,
           source_url, extracted_at) %>%
    filter(!is.na(value), value > 0) %>%
    arrange(desc(value))
  
  # Enrichissement signal_level depuis preis_signals si disponible
  if (!is.null(signals_raw)) {
    zones_with_signals <- signals_raw %>%
      filter(!is.na(zone)) %>%
      group_by(zone) %>%
      summarise(
        max_severity = dplyr::first(severity[order(
          match(severity, c("critical","high","moderate","low","info")))]),
        .groups = "drop"
      ) %>%
      rename(geo_name = zone) %>%
      mutate(signal_level_from_signals = map_signal_level(max_severity)) %>%
      select(geo_name, signal_level_from_signals)
    
    preis_zones <- preis_zones %>%
      left_join(zones_with_signals, by = "geo_name") %>%
      mutate(
        signal_level = coalesce(signal_level_from_signals, signal_level)
      ) %>%
      select(-signal_level_from_signals)
    
    cat("  Niveaux de signal intégrés depuis PREIS_signals.csv.\n")
  }
  
  out_zones <- file.path(DIR_COMMON, "preis_zones.csv")
  write_csv(preis_zones, out_zones)
  cat("  preis_zones.csv écrit :", nrow(preis_zones), "zones →", out_zones, "\n")
  
} else {
  preis_zones <- NULL
  cat("  preis_zones.csv NON produit (tableau_zones_sante.csv introuvable).\n")
}


# --------------------------------------------------------------------------- #
# 5. PRODUCTION DE preis_signals.csv
#    Signaux d'alerte — format commun multi-modules
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_signals.csv --\n")

if (!is.null(signals_raw)) {
  
  preis_signals <- signals_raw %>%
    mutate(
      module       = MODULE_ID,
      source       = SOURCE_NAME,
      # report_id : depuis detected_on ou date
      report_date_sig = as.Date(coalesce(
        suppressWarnings(as.character(detected_on)),
        suppressWarnings(as.character(date))
      )),
      report_id    = paste0("sitrep_", 
                            # Tentative de retrouver le sitrep_no depuis la date
                            ifelse(!is.null(sitrep_lookup) && nrow(sitrep_lookup) > 0,
                                   {
                                     idx <- match(report_date_sig, sitrep_lookup$report_date)
                                     ifelse(is.na(idx), last_report_id,
                                            sitrep_lookup$report_id[idx])
                                   },
                                   last_report_id
                            )),
      geo_level    = tolower(coalesce(level, "zone")),
      geo_name     = coalesce(zone, province, GEO_COUNTRY),
      geo_code     = NA_character_,
      indicator    = type,            # type de signal = l'indicateur concerné
      value        = NA_real_,        # les signaux n'ont pas de valeur unique
      value_type   = "score",
      signal_level = map_signal_level(severity),
      provisional  = TRUE,
      source_url   = last_url,
      extracted_at = Sys.time(),
      # Colonnes spécifiques au module (conservées en supplément)
      signal_detail     = detail,
      signal_hypotheses = hypotheses
    ) %>%
    select(
      module, source, report_id, report_date_sig,
      geo_level, geo_name, geo_code,
      indicator, value, value_type,
      signal_level, provisional,
      source_url, extracted_at,
      signal_detail, signal_hypotheses
    ) %>%
    rename(report_date = report_date_sig) %>%
    filter(!is.na(geo_name)) %>%
    distinct()
  
  out_signals <- file.path(DIR_COMMON, "preis_signals.csv")
  write_csv(preis_signals, out_signals)
  cat("  preis_signals.csv écrit :", nrow(preis_signals), "signaux →", 
      out_signals, "\n")
  
} else {
  preis_signals <- NULL
  cat("  preis_signals.csv NON produit (PREIS_signals.csv introuvable).\n")
}


# --------------------------------------------------------------------------- #
# 6. PRODUCTION DE preis_meta.json
#    Fraîcheur, statut système, métadonnées — lu par le dashboard
# --------------------------------------------------------------------------- #

cat("\n-- Construction de preis_meta.json --\n")

# Calcul des statistiques de synthèse
n_signals  <- if (!is.null(preis_signals)) nrow(preis_signals) else 0
n_zones    <- if (!is.null(preis_zones))   nrow(preis_zones)   else 0
n_series   <- if (!is.null(preis_series))  nrow(preis_series)  else 0

# Nombre de SitReps analysés
n_sitreps  <- if (!is.null(sitrep_registry)) {
  nrow(filter(sitrep_registry, analysed == TRUE))
} else 0

# Indicateurs clés du dernier SitRep (depuis serie nationale)
kpi <- list()
if (!is.null(preis_series) && !is.na(last_date)) {
  last_series <- preis_series %>%
    filter(report_id == last_report_id, geo_name == GEO_COUNTRY)
  
  get_val <- function(ind) {
    v <- last_series %>% filter(grepl(ind, indicator, ignore.case = TRUE)) %>%
      pull(value)
    if (length(v) > 0) v[1] else NA
  }
  
  kpi <- list(
    cases_cumulative   = get_val("cumulative_confirmed|cas_cumules|cum_cases"),
    deaths_cumulative  = get_val("cumulative_death|deces_cumules|cum_deaths"),
    cfr_provisional    = get_val("cfr|fatality_ratio"),
    new_cases_last     = get_val("new_confirmed|nouveaux_cas|new_cases")
  )
}

# Signaux actifs par niveau
signals_summary <- list(critical = 0L, high = 0L, 
                        moderate = 0L, info = 0L)
if (!is.null(preis_signals) && nrow(preis_signals) > 0) {
  sig_counts <- preis_signals %>%
    count(signal_level) %>%
    tibble::deframe()
  for (lv in names(signals_summary)) {
    if (lv %in% names(sig_counts))
      signals_summary[[lv]] <- as.integer(sig_counts[lv])
  }
}

# Statut système
system_status <- dplyr::case_when(
  signals_summary$critical > 0 ~ "critical",
  signals_summary$high     > 0 ~ "alert",
  signals_summary$moderate > 0 ~ "warning",
  n_series > 0                 ~ "ok",
  TRUE                         ~ "no_data"
)

meta <- list(
  module           = MODULE_ID,
  module_label     = "Ebola DRC (Bundibugyo)",
  source           = SOURCE_NAME,
  last_report_id   = last_report_id,
  last_report_date = as.character(last_date),
  last_extracted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  system_status    = system_status,
  n_sitreps        = n_sitreps,
  n_series_rows    = n_series,
  n_zones          = n_zones,
  n_signals        = n_signals,
  signals_by_level = signals_summary,
  kpi              = kpi,
  source_url       = as.character(last_url),
  geographic_scope = list(
    country    = GEO_COUNTRY,
    geo_code   = GEO_CODE,
    provinces  = c("Ituri", "Nord-Kivu", "Sud-Kivu"),
    geo_level_detail = "zone_sante"
  ),
  methodology_notes = list(
    cfr_status    = "provisional — données non finalisées",
    signals_note  = "Signaux = hypothèses épidémiologiques, pas un diagnostic",
    data_source   = "Totaux nationaux INRB (validés); détail zones extrait PDF SitRep",
    cosignature   = "Co-signature INSP/INRB requise avant publication officielle"
  ),
  schema_version = "1.0",
  adapter_version = "00_preis_adapter_ebola.R v1.0"
)

out_meta <- file.path(DIR_COMMON, "preis_meta.json")
write_json(meta, out_meta, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("  preis_meta.json écrit →", out_meta, "\n")
cat("  Statut système :", system_status, "\n")


# --------------------------------------------------------------------------- #
# 7. VÉRIFICATION FINALE ET RAPPORT
# --------------------------------------------------------------------------- #

cat("\n", strrep("=", 60), "\n")
cat("RÉSUMÉ — PREIS Adapter Module:", MODULE_ID, "\n")
cat(strrep("=", 60), "\n")

files_produced <- c(
  "preis_series.csv"  = file.path(DIR_COMMON, "preis_series.csv"),
  "preis_zones.csv"   = file.path(DIR_COMMON, "preis_zones.csv"),
  "preis_signals.csv" = file.path(DIR_COMMON, "preis_signals.csv"),
  "preis_meta.json"   = file.path(DIR_COMMON, "preis_meta.json")
)

for (fname in names(files_produced)) {
  fpath  <- files_produced[fname]
  exists <- file.exists(fpath)
  size   <- if (exists) paste0(round(file.size(fpath) / 1024, 1), " KB") else "—"
  status <- if (exists) "OK  ✓" else "MANQUANT ✗"
  cat(sprintf("  %-22s %s  [%s]\n", fname, status, size))
}

cat("\nDernier rapport source   :", last_report_id, 
    "(", as.character(last_date), ")\n")
cat("Indicateurs produits     :", n_series, "lignes\n")
cat("Zones de santé           :", n_zones, "\n")
cat("Signaux d'alerte         :", n_signals, 
    sprintf("(critique:%d | élevé:%d | modéré:%d | info:%d)\n",
            signals_summary$critical, signals_summary$high,
            signals_summary$moderate, signals_summary$info))
cat("Statut système           :", toupper(system_status), "\n")
cat(strrep("=", 60), "\n")

cat("\nAdapter terminé :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Retourner invisiblement les 4 objets pour usage dans un pipeline parent
invisible(list(
  series  = preis_series,
  zones   = preis_zones,
  signals = preis_signals,
  meta    = meta
))

# FIN : 00_preis_adapter_ebola.R