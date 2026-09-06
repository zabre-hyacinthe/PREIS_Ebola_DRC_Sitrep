# ============================================================
# 16_generate_situation_room_data.R
#
# PREIS Ebola DRC — Generateur de donnees pour la Situation Room EOC
# (situation_room/SituationRoom_AfricaCDC.html)
#
# Auteur  : Dr R. Hyacinthe ZABRE — PREIS / Africa CDC
# Version : 1.0 — 30 juillet 2026
#
# ROLE
#   Lit les sorties DEJA produites par le pipeline existant (02, 00, 11,
#   13, 03...) et assemble un unique fichier JSON
#   situation_room/data/situation_room_data.json que la Situation Room
#   HTML va "fetch()" au chargement -- exactement le meme principe
#   "auto-frais" que dashboard_ebola/app.R (qui lit ses CSV depuis
#   GitHub raw a chaque ouverture, sans redeploiement).
#
#   Ce script NE CASSE RIEN d'existant : il est additif, en lecture
#   seule sur les fichiers du pipeline, et n'ecrit que dans
#   situation_room/data/.
#
# GARDE-FOUS METHODOLOGIQUES (coherents avec dashboard_ebola/app.R) :
#   - Pas de Rt / R0 / SEIR : ces modeles demandent des dates de debut
#     des symptomes (donnees individuelles), indisponibles avec des
#     SitReps agreges. cf. app.R lignes ~1419 et ~1694.
#     -> on calcule a la place un TEMPS DE DOUBLEMENT (methode deja
#     utilisee par app.R, ligne ~1325), sur les cumuls de cas.
#   - Toute valeur "surveillance" (alertes, contacts, labo, vaccin) est
#     du LOCF (Last Observation Carried Forward) car ces indicateurs ne
#     sont pas rapportes a chaque SitRep -- le SitRep source de chaque
#     valeur est trace explicitement (champ *_sitrep) pour eviter de
#     presenter une vieille valeur comme si elle etait fraiche.
#   - Occupation des lits / evenements terrain / One Health : AUCUNE
#     source automatisee n'existe a ce jour dans le pipeline CI (cf.
#     audit du 30/07/2026). Le script NE FABRIQUE PAS ces valeurs : il
#     preserve un bloc "manual" existant si present (a mettre a jour a
#     la main jusqu'a ce que PREIS 2.0 les automatise), et l'etiquette
#     clairement comme tel.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(jsonlite); library(lubridate)
  library(stringr); library(tidyr); library(purrr); library(tibble); library(glue)
})

# ------------------------------------------------------------
# 0. CONFIGURATION (memes conventions que les autres scripts/)
# ------------------------------------------------------------
BASE_DIR   <- Sys.getenv("PREIS_ROOT", unset = getwd())
DATA_FINAL <- file.path(BASE_DIR, "data", "final")
OUT_DIR    <- file.path(BASE_DIR, "outputs", "analyse")
SR_DIR     <- file.path(BASE_DIR, "situation_room", "data")
dir.create(SR_DIR, recursive = TRUE, showWarnings = FALSE)
SR_JSON    <- file.path(SR_DIR, "situation_room_data.json")

GEN_TIME <- format(with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) { cat("  [!] absent:", path, "\n"); return(tibble()) }
  tryCatch(readr::read_csv(path, show_col_types = FALSE, ...),
           error = function(e) { cat("  [!] erreur lecture", path, ":", conditionMessage(e), "\n"); tibble() })
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
fmt <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) "\u2014" else format(round(as.numeric(x)), big.mark = ",")

cat("=== 16_generate_situation_room_data.R ===\n")

# ------------------------------------------------------------
# 1. SERIE NATIONALE (source : outputs/analyse/serie_temporelle_nationale.csv)
# ------------------------------------------------------------
serie <- safe_read_csv(file.path(OUT_DIR, "serie_temporelle_nationale.csv")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(sitrep_no)

if (nrow(serie) == 0) stop("serie_temporelle_nationale.csv introuvable/vide -- pipeline 03 doit tourner avant ce script.")

last_row <- serie %>% filter(!is.na(cas_cumules)) %>% slice_tail(n = 1)

# Temps de doublement : croissance exponentielle sur les 14 derniers jours de
# cumul (meme logique que app.R "Delai de doublement (cas)"). NA si epidemie
# stable/en baisse (pas de doublement a calculer).
compute_doubling <- function(s, window_days = 14) {
  s2 <- s %>% filter(!is.na(cas_cumules)) %>% arrange(date)
  if (nrow(s2) < 2) return(NA_real_)
  last_date <- max(s2$date); last_val <- s2$cas_cumules[s2$date == last_date][1]
  ref <- s2 %>% filter(date <= last_date - window_days) %>% slice_tail(n = 1)
  if (nrow(ref) == 0) ref <- s2 %>% slice(1)
  n_days <- as.numeric(last_date - ref$date)
  if (n_days <= 0 || is.na(ref$cas_cumules) || ref$cas_cumules <= 0) return(NA_real_)
  growth <- last_val / ref$cas_cumules
  if (!is.finite(growth) || growth <= 1) return(NA_real_)
  round(n_days * log(2) / log(growth), 1)
}
doubling <- compute_doubling(serie)

# Variation d'incidence 7 jours vs 7 jours precedents -- meme methode que
# nat_growth_txt dans 06_generate_africa_cdc_sitrep_final.R ("7-day incidence
# changed by -35% vs. the prior 7 days"), pour que la Situation Room et le
# supplement SitRep officiel racontent le meme chiffre.
compute_growth7 <- function(s) {
  s2 <- s %>% filter(!is.na(nouveaux_cas_calc)) %>% arrange(date)
  n <- nrow(s2)
  if (n < 14) return(NA_real_)
  inc7 <- sum(pmax(tail(s2$nouveaux_cas_calc, 7), 0), na.rm = TRUE)
  inc7prev <- sum(pmax(s2$nouveaux_cas_calc[(n - 13):(n - 7)], 0), na.rm = TRUE)
  if (inc7prev <= 0) return(NA_real_)
  round(100 * (inc7 - inc7prev) / inc7prev, 0)
}
growth7 <- compute_growth7(serie)

series_out <- serie %>%
  filter(!is.na(date)) %>%
  transmute(d = as.character(date),
            nc  = nouveaux_cas_calc,
            ma7 = round(moy_mobile_cas, 2),
            cum = cas_cumules)

# ------------------------------------------------------------
# 2. INDICATEURS LONG FORMAT (source : data/final/PREIS_indicators_long.csv)
#    -> surveillance (alertes, contacts, labo, vaccin...), LOCF + tracabilite
# ------------------------------------------------------------
ind_long <- safe_read_csv(file.path(DATA_FINAL, "PREIS_indicators_long.csv"))

latest_indicator <- function(code) {
  if (nrow(ind_long) == 0) return(list(value = NA, sitrep = NA))
  rows <- ind_long %>% filter(indicator_code == code, !is.na(value)) %>%
    arrange(sitrep_no) %>% slice_tail(n = 1)
  if (nrow(rows) == 0) return(list(value = NA, sitrep = NA))
  list(value = suppressWarnings(as.numeric(rows$value[1])), sitrep = rows$sitrep_no[1])
}

latest_sitrep_no <- max(serie$sitrep_no, na.rm = TRUE)
staleness <- function(sitrep) if (is.na(sitrep)) NA_integer_ else as.integer(latest_sitrep_no - sitrep)

surv_codes <- list(
  alerts_reported       = "alerts_reported",
  alerts_investigated   = "alerts_investigated",
  investigation_rate    = "alerts_investigation_rate",
  contacts_followed     = "contacts_followed_up",
  suspects              = "suspected_cases_investigation",
  samples_analyzed      = "samples_analyzed",
  lab_positivity        = "lab_positivity_rate",
  vaccine               = "doses_vaccine_administered",
  community_deaths      = "deaths_community",
  recovered             = "recovered",
  isolation             = "patients_in_isolation"
)
surv_vals <- purrr::map(surv_codes, latest_indicator)
surv <- purrr::map(surv_vals, "value")
surv_sitrep <- purrr::map(surv_vals, "sitrep")
surv_staleness <- purrr::map(surv_sitrep, staleness)

# ------------------------------------------------------------
# 3. PROVINCES & ZONES (source : data/final/PREIS_daily_indicators.csv)
# ------------------------------------------------------------
daily <- safe_read_csv(file.path(DATA_FINAL, "PREIS_daily_indicators.csv")) %>%
  mutate(date = as.Date(date))

prov_latest_date <- if (nrow(daily) > 0) max(daily$date[daily$level == "Province"], na.rm = TRUE) else NA
zone_latest_date <- if (nrow(daily) > 0) max(daily$date[daily$level == "Zone"], na.rm = TRUE) else NA

# Reference zone_coords + harmonisation orthographique -- copiees telles
# quelles depuis dashboard_ebola/app.R pour rester coherent avec le
# dashboard officiel (meme reference geographique partout dans PREIS).
zone_coords <- tibble::tribble(
  ~health_zone,   ~province,     ~lat,    ~lon,
  "Mabalako",     "Nord-Kivu",    0.420,  29.420,
  "Bunia",        "Ituri",        1.565,  30.244,
  "Rwampara",     "Ituri",        1.530,  30.180,
  "Mongbwalu",    "Ituri",        1.960,  30.040,
  "Nyankunde",    "Ituri",        1.420,  30.150,
  "Nizi",         "Ituri",        1.700,  30.060,
  "Bambu",        "Ituri",        1.870,  30.080,
  "Lita",         "Ituri",        1.690,  30.300,
  "Kilo",         "Ituri",        1.830,  30.130,
  "Aru",          "Ituri",        2.880,  30.910,
  "Damas",        "Ituri",        1.600,  30.300,
  "Rimba",        "Ituri",        2.000,  30.500,
  "Komanda",      "Ituri",        1.360,  29.770,
  "Mambasa",      "Ituri",        1.360,  29.050,
  "Mangala",      "Ituri",        1.600,  30.400,
  "Aungba",       "Ituri",        2.300,  30.900,
  "Logo",         "Ituri",        2.700,  30.700,
  "Tchomia",      "Ituri",        1.480,  30.530,
  "Gety",         "Ituri",        1.350,  30.190,
  "Kambala",      "Ituri",        1.700,  30.200,
  "Fataki",       "Ituri",        2.100,  30.700,
  "Katwa",        "Nord-Kivu",   -0.470,  29.250,
  "Beni",         "Nord-Kivu",    0.491,  29.473,
  "Butembo",      "Nord-Kivu",    0.131,  29.290,
  "Oicha",        "Nord-Kivu",    0.700,  29.520,
  "Kyondo",       "Nord-Kivu",    0.150,  29.400,
  "Kalunguta",    "Nord-Kivu",    0.300,  29.350,
  "Masereka",     "Nord-Kivu",    0.200,  29.300,
  "Vuhovi",       "Nord-Kivu",    0.450,  29.300,
  "Manguredjipa", "Nord-Kivu",    0.700,  29.000,
  "Goma",         "Nord-Kivu",   -1.679,  29.235,
  "Karisimbi",    "Nord-Kivu",   -1.700,  29.230,
  "Miti-Murhesa", "Sud-Kivu",    -2.350,  28.770,
  "Jiba",         "Ituri",        2.400,  30.900
)
canon_zone <- function(x) {
  x <- str_squish(as.character(x))
  dplyr::recode(x,
    "Mongbalu"="Mongbwalu","Nyakunde"="Nyankunde","Gethy"="Gety",
    # Variantes orthographiques reellement observees dans tableau_zones_sante.csv
    # (audit du 30/07/2026) : sans cette harmonisation, ces zones se comptent en
    # double ET certaines (Nia-Nia = 127 cas cumules a elles deux) disparaissent
    # entierement du "Top health zones" et de la carte -- gap documente dans
    # claude_PREIS_DHIS2_kit_execution.md ("~21 zones manquantes de la carte").
    "Nia Nia"="Nia-Nia","Makiso Kisangani"="Makiso-Kisangani",
    "Lubunga (Tshopo)"="Lubunga","Miti Murhesa"="Miti-Murhesa",
    .default = x)
}

provinces_out <- tibble()
zones_out <- tibble()
zones_missing_coords <- character(0)

if (nrow(daily) > 0 && !is.na(prov_latest_date)) {
  hz_per_prov <- daily %>% filter(level == "Zone", date == zone_latest_date, cum_cases > 0) %>%
    count(province, name = "hz")
  provinces_out <- daily %>% filter(level == "Province", date == prov_latest_date) %>%
    transmute(name = province, cum = cum_cases, deaths = cum_deaths, cfr = cfr) %>%
    left_join(hz_per_prov, by = c("name" = "province")) %>%
    mutate(hz = coalesce(hz, 0L)) %>%
    arrange(desc(cum))
}

# ---- Zones : fusion de 2 sources plutot que la seule PREIS_daily_indicators.csv ----
# outputs/analyse/tableau_zones_sante.csv est la liste CUMULEE la plus complete
# (toutes les zones ayant jamais eu des cas) ; PREIS_daily_indicators.csv (niveau
# Zone) est plus granulaire (cas/deces/nouveaux par date) mais NE COUVRE PAS
# toutes les zones -- 22 zones a cas reels en sont absentes au 30/07/2026
# (ex. Nia-Nia 127 cas cumules, Musienene 31, Mandima 16...). On fusionne les
# deux : le cumul vient de tableau_zones_sante (autorite), enrichi par
# deces/nouveaux-cas quand la zone existe aussi dans daily_indicators.
tabzones <- safe_read_csv(file.path(OUT_DIR, "tableau_zones_sante.csv"))

if (nrow(daily) > 0 && !is.na(zone_latest_date)) {
  zone_detail <- daily %>% filter(level == "Zone", date == zone_latest_date) %>%
    mutate(health_zone = canon_zone(zone)) %>%
    group_by(health_zone) %>%
    summarise(cum_di = sum(cum_cases, na.rm = TRUE), deaths = sum(cum_deaths, na.rm = TRUE),
              new = sum(new_cases, na.rm = TRUE), cfr_di = mean(cfr, na.rm = TRUE), .groups = "drop")
} else zone_detail <- tibble(health_zone = character(), cum_di = numeric(), deaths = numeric(), new = numeric(), cfr_di = numeric())

if (nrow(tabzones) > 0) {
  name_col <- intersect(c("nom","zone","health_zone"), names(tabzones))[1]
  case_col <- intersect(c("cas","cases","total_cases"), names(tabzones))[1]
  zone_base <- tabzones %>%
    transmute(health_zone = canon_zone(.data[[name_col]]),
              cum_tz = suppressWarnings(as.numeric(.data[[case_col]]))) %>%
    filter(!is.na(health_zone), health_zone != "NA") %>%
    group_by(health_zone) %>% summarise(cum_tz = sum(cum_tz, na.rm = TRUE), .groups = "drop")
} else zone_base <- zone_detail %>% transmute(health_zone, cum_tz = cum_di)

z <- zone_base %>%
  full_join(zone_detail, by = "health_zone") %>%
  # cumul = le plus grand des deux (le detail journalier peut etre legerement
  # en retard sur le cumul officiel si une zone n'a pas de ligne a la derniere date)
  mutate(cum = pmax(coalesce(cum_tz, 0), coalesce(cum_di, 0)),
         new = coalesce(new, 0),
         cfr = ifelse(!is.na(cfr_di), round(cfr_di, 1),
                       ifelse(cum > 0 & !is.na(deaths), round(100 * deaths / cum, 1), NA))) %>%
  filter(cum > 0) %>%
  left_join(zone_coords, by = "health_zone")
zones_missing_coords <- z$health_zone[is.na(z$lat)]
zones_out <- z %>% transmute(name = health_zone, cum, new, lat, lon) %>% arrange(desc(cum))

# ---- Dynamique 21 jours par zone (pour la carte Situation Room) ----
# Cas/deces des 21 DERNIERS JOURS (pas juste le cumul total) -- au niveau
# le plus fin disponible dans les donnees agregees (zone de sante). Un
# niveau plus fin (aire de sante/village) exigerait la line list
# individuelle, indisponible ici -- on ne l'invente pas.
zone_dyn21 <- tibble()
if (nrow(daily) > 0 && !is.na(zone_latest_date)) {
  zd <- daily %>% filter(level == "Zone", !is.na(cum_cases)) %>%
    mutate(health_zone = canon_zone(zone)) %>%
    group_by(health_zone, date) %>%
    summarise(cum_cases = max(cum_cases, na.rm = TRUE),
              cum_deaths = suppressWarnings(max(cum_deaths, na.rm = TRUE)), .groups = "drop") %>%
    filter(is.finite(cum_cases)) %>%
    mutate(cum_deaths = ifelse(is.finite(cum_deaths), cum_deaths, NA_real_))
  at_date21 <- function(hz, target, col) {
    sub <- zd %>% filter(health_zone == hz, date <= target)
    if (nrow(sub) == 0) return(NA_real_)
    sub %>% arrange(desc(date)) %>% slice(1) %>% pull(.data[[col]])
  }
  hzs21 <- unique(zd$health_zone)
  zone_dyn21 <- purrr::map_dfr(hzs21, function(hz) {
    c_now <- at_date21(hz, zone_latest_date, "cum_cases")
    c_21  <- at_date21(hz, zone_latest_date - 21, "cum_cases")
    d_now <- at_date21(hz, zone_latest_date, "cum_deaths")
    d_21  <- at_date21(hz, zone_latest_date - 21, "cum_deaths")
    tibble(name = hz,
           cases_last21d  = ifelse(is.na(c_now) || is.na(c_21), NA_real_, pmax(0, c_now - c_21)),
           deaths_last21d = ifelse(is.na(d_now) || is.na(d_21), NA_real_, pmax(0, d_now - d_21)))
  })
}
zones_out <- zones_out %>% left_join(zone_dyn21, by = "name") %>%
  mutate(cases_last21d = coalesce(cases_last21d, 0), deaths_last21d = coalesce(deaths_last21d, 0))

# ---- CFR vs cases (scatter) : meme paire de variables que
# cfr_scatter_plot dans 06_generate_africa_cdc_sitrep_final.R (le supplement
# SitRep officiel envoye par email) -- reutilisee ici telle quelle pour que
# la Situation Room et le document officiel racontent exactement la meme
# histoire.
cfr_scatter_out <- z %>% filter(!is.na(cfr)) %>%
  transmute(name = health_zone, cum, cfr) %>% arrange(desc(cum))

# ---- Statut de transmission par zone : jours depuis le dernier nouveau cas
# (logique identique et copiee telle quelle de 06_generate_africa_cdc_sitrep_final.R,
# "Transmission status by zone" -- meme calcul, meme lecture) -- utile pour
# reperer a la fois les zones en silence prolonge (verification active
# recommandee) et les candidates a la levee de vigilance.
transmission_status_out <- tibble()
if (nrow(daily) > 0) {
  zone_hist <- daily %>% filter(level == "Zone") %>% arrange(zone, province, date) %>%
    group_by(zone, province) %>%
    mutate(new_case_day = coalesce(cum_cases - lag(cum_cases), 0) > 0) %>%
    ungroup()
  transmission_status_out <- zone_hist %>% filter(new_case_day) %>%
    group_by(zone, province) %>%
    summarise(last_case_date = max(date), .groups = "drop") %>%
    mutate(days_since = as.integer(as.Date(zone_latest_date) - as.Date(last_case_date))) %>%
    arrange(desc(days_since)) %>%
    transmute(name = zone, province, last_case_date = as.character(last_case_date), days_since) %>%
    slice_head(n = 12)
}

# ---- Watch operationnel : REUTILISE tel quel le module partage
# scripts/operational_watch.R (les 4 memes regles RED/ORANGE que le
# supplement SitRep officiel envoye par email et que 04_send_sitrep_alerts_conditional.R)
# plutot que de reimplementer une logique de detection differente ici.
OW_HELPER_FP <- file.path(BASE_DIR, "scripts", "operational_watch.R")
watch_zones_out <- tibble()
if (file.exists(OW_HELPER_FP) && nrow(daily) > 0 && !is.na(zone_latest_date)) {
  source(OW_HELPER_FP)
  ow <- tryCatch(compute_operational_watch(daily, zone_latest_date, min_cases = 10),
                 error = function(e) { cat("  [!] operational_watch:", conditionMessage(e), "\n"); data.frame() })
  if (nrow(ow) > 0) {
    watch_zones_out <- ow %>% transmute(
      zone, province, cum_cases, cfr = round(cfr, 1), new_24h = new_24h, new_7d = new_7d,
      severity, reason, action) %>% as_tibble()
  }
} else {
  cat("  [!] operational_watch.R introuvable a", OW_HELPER_FP, "-- watch operationnel omis (pas de donnees fabriquees).\n")
}

# ---- Gaps derives du watch operationnel officiel (voir plus haut,
# watch_zones_out) : on ne reimplemente pas une 2e logique de detection ici --
# une seule source de verite pour "quelle zone regarder", coherente avec le
# supplement SitRep officiel envoye par email. Seules les zones RED (les plus
# urgentes) remontent dans les gaps ; la table complete (RED+ORANGE) est dans
# watch_zones_out / le panneau dedie.
trend_gaps <- if (nrow(watch_zones_out) > 0) {
  watch_zones_out %>% filter(severity == "RED") %>% slice_head(n = 3) %>% transmute(
    k = "Operational watch", v = zone,
    rule = glue("{zone} ({province}): {reason}. {action}") %>% as.character(),
    level = "bad", detected_on = as.character(Sys.Date())
  )
} else tibble()

# ------------------------------------------------------------
# 4. SIGNAUX -> "gaps" (source : data/final/PREIS_signals.csv)
#    Alimente reellement par 13_signal_detection.R quand ce script
#    tourne ; NON encore appele par le workflow CI actuel (a wirer
#    separement -- voir la note livree avec ce script). En attendant,
#    on affiche les signaux existants avec leur date de detection,
#    pour que l'EOC voie leur fraicheur reelle plutot qu'un statut
#    fige.
# ------------------------------------------------------------
registry <- safe_read_csv(file.path(DATA_FINAL, "sitrep_registry.csv"))
inrb_ref <- safe_read_csv(file.path(DATA_FINAL, "INRB_reference_national.csv"))
signals <- safe_read_csv(file.path(DATA_FINAL, "PREIS_signals.csv"))
gaps_out <- tibble()
if (nrow(signals) > 0) {
  gaps_out <- signals %>%
    arrange(desc(detected_on)) %>%
    transmute(
      k = type,
      v = coalesce(zone, level),
      rule = detail,
      level = case_when(severity %in% c("high") ~ "bad",
                         severity %in% c("moderate") ~ "warn",
                         TRUE ~ "warn"),
      detected_on = as.character(detected_on)
    )
}
# Toujours ajouter les 2 constats structurels du pipeline lui-meme
# (Rt non calculable, occupation des lits non automatisee) -- coherent
# avec le principe PREIS "jamais surestimer / toujours dire les limites".
# Toujours ajouter les constats structurels du pipeline lui-meme -- coherent
# avec le principe PREIS "jamais surestimer / toujours dire les limites".
# Les 2 premiers sont calcules en direct (pas de date figee en dur) :
days_since_sitrep <- as.integer(Sys.Date() - last_row$date[1])
inrb_max_sitrep <- if (nrow(inrb_ref) > 0) max(inrb_ref$sitrep_no, na.rm = TRUE) else NA_integer_
inrb_max_date   <- if (nrow(inrb_ref) > 0) max(inrb_ref$sitrep_date, na.rm = TRUE) else NA
inrb_lag <- if (!is.na(inrb_max_sitrep)) latest_sitrep_no - inrb_max_sitrep else NA_integer_
prov_sum <- if (nrow(provinces_out) > 0) sum(provinces_out$cum, na.rm = TRUE) else NA_real_
national_cases <- last_row$cas_cumules[1]
prov_gap_pct <- if (!is.na(prov_sum) && !is.na(national_cases) && national_cases > 0)
  round(100 * (national_cases - prov_sum) / national_cases, 1) else NA_real_

structural_gaps <- tibble::tribble(
  ~k, ~v, ~rule, ~level, ~detected_on,
  "Rt / R0", "non calcule",
    "Necessite des dates de debut des symptomes (donnees individuelles) -- indisponibles avec des SitReps agreges. Temps de doublement affiche a la place.",
    "info", as.character(Sys.Date()),
  "Occupation des lits", "non automatise",
    "Aucune source CI ne produit ce chiffre en continu actuellement (script 15_bed_occupancy_analysis_v2.R non branche au workflow). Mise a jour manuelle requise.",
    "warn", as.character(Sys.Date()),
  "Fraicheur pipeline", as.character(glue("{days_since_sitrep} j depuis SitRep {latest_sitrep_no}")),
    as.character(if (days_since_sitrep > 3)
      glue("Aucun nouveau SitRep detecte depuis {days_since_sitrep} jours -- verifier le cron GitHub Actions (habituellement ~quotidien).")
    else
      "Rythme normal."),
    if (days_since_sitrep > 5) "bad" else if (days_since_sitrep > 3) "warn" else "info",
    as.character(Sys.Date()),
  "Reference INRB", as.character(if (!is.na(inrb_lag)) glue("{inrb_lag} SitReps de retard") else "indisponible"),
    as.character(if (!is.na(inrb_lag) && inrb_lag > 0)
      glue("La reference nationale INRB utilisee pour la contre-validation independante des cumuls n'a plus ete mise a jour depuis le SitRep {inrb_max_sitrep} ({inrb_max_date}) -- {inrb_lag} SitReps plus tard, les cumuls officiels ne sont plus contre-verifies (validation = 'no_ref').")
    else "A jour."),
    if (!is.na(inrb_lag) && inrb_lag > 10) "bad" else if (!is.na(inrb_lag) && inrb_lag > 0) "warn" else "info",
    as.character(Sys.Date()),
  "Repartition par province", as.character(if (!is.na(prov_gap_pct)) glue("ecart {prov_gap_pct}%") else "indisponible"),
    as.character(if (!is.na(prov_gap_pct) && abs(prov_gap_pct) > 1)
      glue("La somme des cumuls par province ({fmt(prov_sum)}) ne correspond pas exactement au total national ({fmt(national_cases)}) -- ecart de {prov_gap_pct}%. La ventilation par province extraite des SitReps est probablement incomplete pour certaines lignes ; ne pas sur-interpreter de petits ecarts entre provinces.")
    else "Coherent avec le total national."),
    if (!is.na(prov_gap_pct) && abs(prov_gap_pct) > 5) "warn" else "info",
    as.character(Sys.Date())
)
gaps_out <- bind_rows(gaps_out, structural_gaps, trend_gaps,
  tibble::tibble(k = "CFR reading", v = "small-denominator caveat",
    rule = "Zones with very few cumulative cases can show 100% CFR from a single death -- this is not comparable to a high CFR over a large case count. Always check the case count (shown alongside CFR) before treating a zone as a mortality-review priority.",
    level = "info", detected_on = as.character(Sys.Date())))

# ------------------------------------------------------------
# 5. FRAICHEUR DES SOURCES (sources chips)
# ------------------------------------------------------------
sitrep_last_date <- if (nrow(registry) > 0) {
  registry %>% filter(extracted == TRUE) %>% arrange(desc(sitrep_no)) %>% slice(1) %>% pull(sitrep_no)
} else NA
sources_out <- tibble::tribble(
  ~name, ~cadence, ~last, ~detail,
  "SitRep (INSP)", "Per report", as.character(last_row$date[1]), paste0("N\u00B0", latest_sitrep_no),
  "INRB reference", "Daily", if (nrow(inrb_ref) > 0) as.character(max(inrb_ref$sitrep_date, na.rm = TRUE)) else NA_character_, "Reference nationale",
  "Daily indicators", "Daily", if (!is.na(zone_latest_date)) as.character(zone_latest_date) else NA_character_, "Province/zone"
)

# ------------------------------------------------------------
# 6. ASSEMBLAGE KPI + HIGHLIGHTS
# ------------------------------------------------------------
kpi <- list(
  cases = last_row$cas_cumules[1], deaths = last_row$deces_cumules[1],
  cfr = last_row$cfr[1], new = last_row$nouveaux_cas_calc[1] %||% last_row$nouveaux_cas[1],
  ma7 = round(last_row$moy_mobile_cas[1], 1), doubling = doubling, growth7 = growth7,
  recovered = surv$recovered, isolation = surv$isolation,
  hz = latest_indicator("hz_affected_national")$value,
  provinces = if (nrow(provinces_out) > 0) nrow(provinces_out) else NA,
  beds_occ = NA, beds_deficit = NA   # non automatise -- cf. gaps structurels
)

highlights <- c(
  glue("{fmt(kpi$new)} new confirmed cases in the latest report; 7-day average {fmt(kpi$ma7)}/day{if (!is.na(growth7)) glue(' ({ifelse(growth7>=0,\"+\",\"\")}{growth7}% vs. the prior 7 days)') else ''}.") %>% as.character(),
  if (!is.na(doubling)) glue("Cumulative cases doubling approximately every {doubling} days (trailing 14-day growth).") else "Case growth flat/declining over the trailing 14 days -- no doubling time to report.",
  glue("Case-fatality ratio {kpi$cfr}% (provisional); {fmt(surv$recovered %||% NA)} recovered, {fmt(surv$isolation %||% NA)} in isolation."),
  glue("{fmt(kpi$hz)} health zones affected across {fmt(kpi$provinces)} provinces."),
  if (nrow(watch_zones_out) > 0) glue("Operational watch: {sum(watch_zones_out$severity=='RED')} zone(s) RED, {sum(watch_zones_out$severity=='ORANGE')} ORANGE for priority verification.") else NA
)
highlights <- highlights[!is.na(highlights)]

# ------------------------------------------------------------
# 7. BLOC MANUEL (bed occupancy / operational events / One Health)
#    On PRESERVE ce qui existe deja dans le JSON precedent (mis a jour
#    a la main) plutot que de l'ecraser ou de le fabriquer.
# ------------------------------------------------------------
manual_block <- list(
  beds = NULL, bed_sites = list(), events = list(), onehealth = NULL,
  correlation = list(), new_areas = list(),
  # Champs suivants alimentes par les decks EOC hebdomadaires quand ils sont
  # partages (non fiable comme flux recurrent -- cf. echange du 30/07/2026 --
  # donc jamais calcules ici, seulement accueillis et preserves s'ils existent
  # deja). Chacun porte sa PROPRE date, distincte de `asof`/`sitrep_date` qui
  # restent la verite officielle SitRep.
  detection_delay = NULL,        # {date, national_median_days, who_target_days, by_zone:[{zone,median_days,n}]}
  contacts_by_province = NULL,   # {date, national_rate, delta_24h, threshold, rows:[{province,under,seen,rate}]}
  poe = NULL,                    # {date, travelers_screened_24h, screening_coverage_pct, sensitization_coverage_pct, alerts_24h, sites, bodies_intercepted, bodies_swabbed_pct}
  ipc_hcw = NULL,                # {date, ppl_infected, ppl_deaths, cfr_pct, triage_units_functional_pct, premises_decontam_48h_pct}
  eds = NULL,                    # {date, alerts, safe_burials, swabs, report_back_rate_pct, security_incidents:[...]}
  special_populations = NULL,    # {date, pregnant_women_cumulative, children_under15_pct, mhpss_coverage_pct}
  line_list_rt = NULL,           # {date, value, method, caveat} -- estimation Rt sur line-list (EpiEstim), distincte
                                  # du temps de doublement (calcule, lui, automatiquement plus haut sur donnees agregees)
  last_manual_update = NA
)
if (file.exists(SR_JSON)) {
  prev <- tryCatch(jsonlite::fromJSON(SR_JSON, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(prev) && !is.null(prev$manual)) {
    # Fusion champ par champ : un champ absent de l'ancien JSON garde son
    # defaut ci-dessus plutot que de disparaitre (cas d'un JSON genere avant
    # l'ajout de ce champ). MAIS on ne preserve que des valeurs REELLEMENT
    # non vides -- un {} JSON (liste vide en R apres fromJSON) ne doit
    # jamais ecraser le defaut NULL propre, sinon un ancien {} issu d'un
    # bug de serialisation anterieur se perpetue indefiniment d'une
    # execution a l'autre (c'est exactement ce qui s'est produit avant
    # ce correctif : line_list_rt restait {} meme apres avoir corrige
    # write_json, parce que ce {} etait recopie depuis le JSON precedent
    # a chaque run).
    is_meaningful <- function(v) {
      if (is.null(v)) return(FALSE)
      if (is.list(v) && length(v) == 0) return(FALSE)
      TRUE
    }
    for (k in names(prev$manual)) {
      v <- prev$manual[[k]]
      if (is_meaningful(v)) manual_block[[k]] <- v
    }
  }
}

# ------------------------------------------------------------
# 8. ECRITURE JSON
# ------------------------------------------------------------

SR_DATA <- list(
  generated_at = GEN_TIME,
  asof = as.character(last_row$date[1]),
  sitrep = as.character(latest_sitrep_no),
  sitrep_date = as.character(last_row$date[1]),
  kpi = kpi,
  surv = surv,
  surv_meta = list(sitrep = surv_sitrep, staleness_sitreps = surv_staleness),
  series = series_out,
  provinces = provinces_out,
  zones = zones_out,
  cfr_scatter = cfr_scatter_out,
  transmission_status = transmission_status_out,
  watch_zones = watch_zones_out,
  gaps = gaps_out,
  sources = sources_out,
  highlights = highlights,
  manual = manual_block,
  diagnostics = list(
    build_version = "v5.0-auto",
    build_date = as.character(Sys.Date()),
    generator = "scripts/16_generate_situation_room_data.R",
    latest_sitrep_no = latest_sitrep_no,
    zones_missing_coords = zones_missing_coords
  )
)

jsonlite::write_json(SR_DATA, SR_JSON, auto_unbox = TRUE, na = "null", null = "null", pretty = TRUE, dataframe = "rows")
cat("OK ->", SR_JSON, "\n")
cat("SitRep", latest_sitrep_no, "-", as.character(last_row$date[1]),
    "| cas:", kpi$cases, "| deces:", kpi$deaths, "| CFR:", kpi$cfr, "\n")
