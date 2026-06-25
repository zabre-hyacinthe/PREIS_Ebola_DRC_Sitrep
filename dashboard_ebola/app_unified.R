# =============================================================================
# PREIS — DASHBOARD UNIFIÉ MULTI-MODULES
# Fichier : app_unified.R
# Auteur  : Dr R. Hyacinthe ZABRE — Africa CDC
# Version : 1.0 — 16 juin 2026
#
# ARCHITECTURE
# ------------
# Ce dashboard est le point d'entrée unique de la plateforme PREIS.
# Il lit :
#   - Module Ebola  : ses fichiers natifs (aucun changement au pipeline existant)
#   - Module Polio  : data/final/preis_common/ (produit par 00_preis_adapter_polio.R)
#   - Autres modules futurs : même convention preis_common/
#
# Un sélecteur de module dans la sidebar bascule entre les modules.
# Chaque module a son propre onglet latéral et ses propres vues.
# La page d'accueil (Overview) est commune : vue continentale consolidée.
#
# DÉPLOIEMENT
# -----------
# Ce fichier remplace app.R dans dashboard_ebola/ (ou dans un nouveau dossier).
# Les fichiers geojson, données Ebola et scripts existants ne bougent pas.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(shinyWidgets)
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(DT); library(plotly); library(leaflet); library(sf)
  library(ggplot2); library(htmltools); library(scales); library(jsonlite)
})

options(shiny.maxRequestSize = 100 * 1024^2)
if (requireNamespace("sf", quietly = TRUE)) sf::sf_use_s2(FALSE)

# Opérateur null-coalesce (défini tôt car utilisé partout)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# --------------------------------------------------------------------------- #
# 0. CHEMINS ET CONFIGURATION
# --------------------------------------------------------------------------- #

ROOT_DIR    <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DATA_DIR    <- file.path(ROOT_DIR, "data")
CURATED_DIR <- file.path(DATA_DIR, "curated")
ANALYSE_DIR <- file.path(ROOT_DIR, "outputs", "analyse")

GH_RAW <- Sys.getenv(
  "PREIS_GH_RAW_BASE",
  "https://raw.githubusercontent.com/zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep/refs/heads/main")

.LOCAL_FIRST <- tolower(Sys.getenv("PREIS_LOCAL_FIRST", "0")) %in% c("1","true","yes")

find_first <- function(paths) {
  for (p in paths) {
    if (grepl("^https?://", p)) return(p)
    if (file.exists(p)) return(p)
  }
  NA_character_
}

prefere_github <- function(local_paths, gh_url) {
  if (.LOCAL_FIRST) return(find_first(c(local_paths, gh_url)))
  find_first(c(gh_url, local_paths))
}

# Dossier socle commun (produit par les adapters)
COMMON_DIR <- file.path(DATA_DIR, "final", "preis_common")

# --------------------------------------------------------------------------- #
# 1. MODULES ENREGISTRÉS
# --------------------------------------------------------------------------- #
# Pour ajouter un module : copier le bloc ci-dessous et adapter.
# L'adapter correspondant doit produire les 4 fichiers dans COMMON_DIR.

MODULES <- list(
  ebola = list(
    id       = "ebola",
    label    = "Ebola DRC",
    icon     = "biohazard",
    color    = "red",
    pathogen = "MVE / Bundibugyo",
    scope    = "RDC — Ituri, Nord-Kivu, Sud-Kivu",
    source   = "INSP/INRB",
    freq     = "Quotidien (SitRep)"
  ),
  polio = list(
    id       = "polio",
    label    = "Polio Afrique",
    icon     = "circle-dot",
    color    = "blue",
    pathogen = "Poliovirus (cVDPV, WPV)",
    scope    = "Afrique — 55 États membres",
    source   = "GPEI / Polio This Week",
    freq     = "Hebdomadaire"
  )
  # Ajout futur :
  # mpox = list(id="mpox", label="Mpox", icon="virus", color="purple", ...)
)

MODULE_IDS    <- names(MODULES)
MODULE_LABELS <- setNames(sapply(MODULES, `[[`, "label"), MODULE_IDS)

# --------------------------------------------------------------------------- #
# 2. INTERNATIONALISATION (6 langues Africa CDC)
# --------------------------------------------------------------------------- #

I18N <- list(
  fr = c(
    app_title    = "PREIS — Intelligence Épidémiologique",
    module_sel   = "Module de surveillance",
    overview_all = "Vue continentale",
    module_detail= "Détail du module",
    freshness    = "Fraîcheur des données",
    signals_all  = "Alertes consolidées",
    about        = "À propos",
    language     = "Langue",
    last_update  = "Dernière mise à jour",
    status_ok    = "Système opérationnel",
    status_alert = "Alertes actives",
    status_warn  = "Avertissements",
    status_crit  = "CRITIQUE",
    no_data      = "Données indisponibles",
    n_signals    = "Signaux détectés",
    n_countries  = "Pays/zones concernés",
    module_label = "Module actif"
  ),
  en = c(
    app_title    = "PREIS — Epidemic Intelligence",
    module_sel   = "Surveillance module",
    overview_all = "Continental overview",
    module_detail= "Module detail",
    freshness    = "Data freshness",
    signals_all  = "Consolidated alerts",
    about        = "About",
    language     = "Language",
    last_update  = "Last updated",
    status_ok    = "System operational",
    status_alert = "Active alerts",
    status_warn  = "Warnings",
    status_crit  = "CRITICAL",
    no_data      = "Data unavailable",
    n_signals    = "Signals detected",
    n_countries  = "Countries/zones affected",
    module_label = "Active module"
  ),
  pt = c(
    app_title    = "PREIS — Inteligência Epidemiológica",
    module_sel   = "Módulo de vigilância",
    overview_all = "Visão continental",
    module_detail= "Detalhe do módulo",
    freshness    = "Atualidade dos dados",
    signals_all  = "Alertas consolidados",
    about        = "Sobre",
    language     = "Idioma",
    last_update  = "Última atualização",
    status_ok    = "Sistema operacional",
    status_alert = "Alertas ativos",
    status_warn  = "Avisos",
    status_crit  = "CRÍTICO",
    no_data      = "Dados indisponíveis",
    n_signals    = "Sinais detectados",
    n_countries  = "Países/zonas afetados",
    module_label = "Módulo ativo"
  ),
  es = c(
    app_title    = "PREIS — Inteligencia Epidemiológica",
    module_sel   = "Módulo de vigilancia",
    overview_all = "Vista continental",
    module_detail= "Detalle del módulo",
    freshness    = "Actualidad de los datos",
    signals_all  = "Alertas consolidadas",
    about        = "Acerca de",
    language     = "Idioma",
    last_update  = "Última actualización",
    status_ok    = "Sistema operativo",
    status_alert = "Alertas activas",
    status_warn  = "Advertencias",
    status_crit  = "CRÍTICO",
    no_data      = "Datos no disponibles",
    n_signals    = "Señales detectadas",
    n_countries  = "Países/zonas afectados",
    module_label = "Módulo activo"
  ),
  sw = c(
    app_title    = "PREIS — Akili ya Magonjwa",
    module_sel   = "Moduli ya ufuatiliaji",
    overview_all = "Muhtasari wa bara",
    module_detail= "Maelezo ya moduli",
    freshness    = "Usafi wa data",
    signals_all  = "Tahadhari zilizounganishwa",
    about        = "Kuhusu",
    language     = "Lugha",
    last_update  = "Sasisho la mwisho",
    status_ok    = "Mfumo unafanya kazi",
    status_alert = "Tahadhari zinazoendelea",
    status_warn  = "Maonyo",
    status_crit  = "MUHIMU SANA",
    no_data      = "Data haipatikani",
    n_signals    = "Ishara zilizotambuliwa",
    n_countries  = "Nchi/maeneo yaliyoathirika",
    module_label = "Moduli inayofanya kazi"
  ),
  ar = c(
    app_title    = "PREIS — الذكاء الوبائي",
    module_sel   = "وحدة المراقبة",
    overview_all = "نظرة عامة قارية",
    module_detail= "تفاصيل الوحدة",
    freshness    = "حداثة البيانات",
    signals_all  = "التنبيهات الموحدة",
    about        = "حول",
    language     = "اللغة",
    last_update  = "آخر تحديث",
    status_ok    = "النظام يعمل",
    status_alert = "تنبيهات نشطة",
    status_warn  = "تحذيرات",
    status_crit  = "حرج",
    no_data      = "البيانات غير متاحة",
    n_signals    = "الإشارات المكتشفة",
    n_countries  = "الدول/المناطق المتضررة",
    module_label = "الوحدة النشطة"
  )
)

LANG_CHOICES <- c("Français"="fr","English"="en","Português"="pt",
                  "Español"="es","Kiswahili"="sw","العربية"="ar")

# --------------------------------------------------------------------------- #
# 3. CHARGEMENT DES DONNÉES — SOCLE COMMUN
# --------------------------------------------------------------------------- #

load_common <- function(module_id) {
  base <- file.path(COMMON_DIR)
  
  # Cherche d'abord le fichier préfixé "{module}_", sinon le fichier nu
  read_if <- function(fname) {
    fp_pref <- file.path(base, paste0(module_id, "_", fname))
    fp_nu   <- file.path(base, fname)
    fp <- if (file.exists(fp_pref)) fp_pref
    else if (file.exists(fp_nu)) fp_nu
    else return(NULL)
    tryCatch(read_csv(fp, show_col_types = FALSE), error = function(e) NULL)
  }
  
  # Métadonnées : préfixées d'abord, sinon nues
  meta_pref <- file.path(base, paste0(module_id, "_preis_meta.json"))
  meta_nu   <- file.path(base, "preis_meta.json")
  meta_fp <- if (file.exists(meta_pref)) meta_pref
  else if (file.exists(meta_nu)) meta_nu else NA
  meta <- if (!is.na(meta_fp))
    tryCatch(fromJSON(meta_fp, simplifyVector = TRUE), error = function(e) NULL)
  else NULL
  
  # Filtre de sécurité : ne garde que les lignes du bon module.
  # Tolérant aux variantes d'ID (ex: "ebola" vs "ebola_drc") : on accepte
  # toute valeur de module qui commence par l'ID du dashboard.
  filter_mod <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if ("module" %in% names(df)) {
      df <- df %>% filter(grepl(paste0("^", module_id), module))
    }
    df
  }
  
  list(
    series  = filter_mod(read_if("preis_series.csv")),
    zones   = filter_mod(read_if("preis_zones.csv")),
    signals = filter_mod(read_if("preis_signals.csv")),
    meta    = meta
  )
}

# Chargement au démarrage
common_data <- lapply(MODULE_IDS, function(m) load_common(m))
names(common_data) <- MODULE_IDS

# --------------------------------------------------------------------------- #
# 4. CHARGEMENT DONNÉES EBOLA (fichiers natifs — aucun changement)
# --------------------------------------------------------------------------- #

SERIE_FP <- prefere_github(
  c(file.path(ANALYSE_DIR, "serie_temporelle_nationale.csv"),
    file.path(DATA_DIR, "serie_temporelle_nationale.csv")),
  paste0(GH_RAW, "/outputs/analyse/serie_temporelle_nationale.csv"))

ZONES_FP <- prefere_github(
  c(file.path(ANALYSE_DIR, "tableau_zones_sante.csv"),
    file.path(DATA_DIR, "tableau_zones_sante.csv")),
  paste0(GH_RAW, "/outputs/analyse/tableau_zones_sante.csv"))

AFRICA_FP <- find_first(c(
  file.path(CURATED_DIR, "africa_countries_rcc.geojson"),
  file.path(DATA_DIR, "africa_countries_rcc.geojson"),
  paste0(GH_RAW, "/dashboard_ebola/data/curated/africa_countries_rcc.geojson")))

ZONES_GEO_FP <- find_first(c(
  file.path(CURATED_DIR, "rdc_zones_sante_est.geojson"),
  file.path(DATA_DIR, "rdc_zones_sante_est.geojson"),
  paste0(GH_RAW, "/dashboard_ebola/data/curated/rdc_zones_sante_est.geojson")))

DAILY_FP <- prefere_github(
  c(file.path(DATA_DIR, "PREIS_daily_indicators.csv"),
    file.path(ROOT_DIR, "data", "final", "PREIS_daily_indicators.csv")),
  paste0(GH_RAW, "/data/final/PREIS_daily_indicators.csv"))

SIGNALS_FP <- prefere_github(
  c(file.path(DATA_DIR, "final", "PREIS_validation_signals.csv"),
    file.path(DATA_DIR, "PREIS_validation_signals.csv")),
  paste0(GH_RAW, "/data/final/PREIS_validation_signals.csv"))

load_safe <- function(fp, type = "csv") {
  if (is.na(fp)) return(NULL)
  tryCatch({
    if (type == "csv") read_csv(fp, show_col_types = FALSE)
    else if (type == "sf") sf::read_sf(fp, quiet = TRUE)
  }, error = function(e) NULL)
}

serie_ebola  <- load_safe(SERIE_FP) %||% tibble()
zones_ebola  <- load_safe(ZONES_FP) %||% tibble()
daily_ebola  <- load_safe(DAILY_FP) %||% tibble()
signals_val  <- load_safe(SIGNALS_FP) %||% tibble()
africa_sf    <- load_safe(AFRICA_FP, "sf")
zones_geo    <- load_safe(ZONES_GEO_FP, "sf")

# Coordonnées zones de santé
zone_coords <- tibble::tribble(
  ~health_zone,~province,~lat,~lon,
  "Bunia","Ituri",1.565,30.244,"Rwampara","Ituri",1.530,30.180,
  "Mongbwalu","Ituri",1.960,30.040,"Nyankunde","Ituri",1.420,30.150,
  "Nizi","Ituri",1.700,30.060,"Bambu","Ituri",1.870,30.080,
  "Lita","Ituri",1.690,30.300,"Kilo","Ituri",1.830,30.130,
  "Aru","Ituri",2.880,30.910,"Damas","Ituri",1.600,30.300,
  "Katwa","Nord-Kivu",-0.470,29.250,"Beni","Nord-Kivu",0.491,29.473,
  "Butembo","Nord-Kivu",0.131,29.290,"Oicha","Nord-Kivu",0.700,29.520,
  "Goma","Nord-Kivu",-1.679,29.235,"Miti-Murhesa","Sud-Kivu",-2.350,28.770
)

canon_zone <- function(x) {
  x <- str_squish(as.character(x))
  dplyr::recode(x, "Mongbalu"="Mongbwalu","Nyakunde"="Nyankunde",
                "Gethy"="Gety", .default = x)
}

load_zones_ebola <- function() {
  z <- zones_ebola
  if (is.null(z) || nrow(z) == 0) return(tibble())
  names(z)[names(z) %in% c("nom","zone","health_zone")] <- "health_zone"
  names(z)[names(z) %in% c("cas","cases","total_cases")] <- "cases"
  z %>%
    mutate(health_zone = canon_zone(health_zone),
           cases = suppressWarnings(as.numeric(cases))) %>%
    group_by(health_zone) %>%
    summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
    left_join(zone_coords, by = "health_zone") %>%
    filter(!is.na(lat), !is.na(lon))
}

zones_ebola_proc <- load_zones_ebola()

# --------------------------------------------------------------------------- #
# 5. FONCTIONS UTILITAIRES
# --------------------------------------------------------------------------- #

# Couleur statut système
status_color <- function(status) {
  switch(status %||% "no_data",
         "critical" = "#E31C23", "alert"   = "#C0392B",
         "warning"  = "#F0B323", "ok"      = "#00843E",
         "#888888")
}

status_icon <- function(status) {
  switch(status %||% "no_data",
         "critical" = "circle-exclamation", "alert" = "triangle-exclamation",
         "warning"  = "bell",               "ok"    = "circle-check",
         "circle-question")
}

# Badge statut HTML
status_badge <- function(status, label) {
  col <- status_color(status)
  tags$span(
    style = paste0("background:", col, "; color:white; padding:3px 10px;",
                   " border-radius:12px; font-size:12px; font-weight:600;"),
    label)
}

# Indicateur de fraîcheur
freshness_box <- function(module_id, meta) {
  if (is.null(meta)) return(tags$p("—", style="color:#888;"))
  last_date <- meta$last_report_date %||% "?"
  last_ext  <- meta$last_extracted_at %||% "?"
  status    <- meta$system_status %||% "no_data"
  tags$div(
    style = paste0("border-left: 4px solid ", status_color(status),
                   "; padding: 8px 12px; margin-bottom:8px;",
                   " background:#f9f9f9; border-radius:0 8px 8px 0;"),
    tags$strong(MODULES[[module_id]]$label), tags$br(),
    tags$span(style="font-size:12px; color:#555;",
              "Dernier rapport : ", tags$b(last_date)), tags$br(),
    tags$span(style="font-size:12px; color:#888;",
              "Extrait le : ", last_ext), tags$br(),
    status_badge(status, toupper(status))
  )
}

# --------------------------------------------------------------------------- #
# 6. UI
# --------------------------------------------------------------------------- #

ui <- dashboardPage(
  skin = "black",
  
  # HEADER
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://africacdc.org/wp-content/uploads/2021/08/Africa-CDC-Logo.png",
               height = "32px", style = "margin-right:8px; vertical-align:middle;"),
      "PREIS"),
    titleWidth = 280
  ),
  
  # SIDEBAR
  dashboardSidebar(
    width = 280,
    tags$div(
      style = "padding:12px 16px 4px;",
      tags$p(style="color:#aaa; font-size:11px; margin:0; text-transform:uppercase;",
             "Plateforme PREIS"),
      selectInput("ui_lang", NULL,
                  choices = LANG_CHOICES, selected = "fr",
                  width = "100%")
    ),
    tags$hr(style="border-color:#444; margin:4px 0;"),
    
    # Sélecteur de module (le cœur du dashboard unifié)
    tags$div(
      style = "padding:8px 16px;",
      tags$label(style="color:#ccc; font-size:12px;", "Module de surveillance"),
      selectInput("active_module", NULL,
                  choices  = setNames(MODULE_IDS, MODULE_LABELS),
                  selected = "ebola",
                  width    = "100%")
    ),
    
    sidebarMenuOutput("dynamic_menu"),
    
    tags$hr(style="border-color:#444; margin:4px 0;"),
    
    # Indicateurs de statut de tous les modules
    tags$div(
      style = "padding:8px 16px;",
      tags$p(style="color:#aaa; font-size:11px; margin-bottom:6px;
                    text-transform:uppercase;", "Statut des modules"),
      uiOutput("all_modules_status")
    )
  ),
  
  # BODY
  dashboardBody(
    tags$head(tags$style(HTML("
      .main-header .logo {background:#1a1a2e!important; font-weight:700;}
      .main-header .navbar {background:#1a1a2e!important;}
      .main-sidebar {background-color:#16213e!important;}
      .sidebar-menu>li>a {color:#c8d6e5!important;}
      .sidebar-menu>li.active>a {background:#0f3460!important; border-left:4px solid #e94560!important;}
      .content-wrapper,.right-side {background-color:#f0f2f5!important;}
      .small-box {border-radius:12px!important; box-shadow:0 2px 8px rgba(0,0,0,0.10)!important;}
      .box {border-radius:12px; box-shadow:0 2px 8px rgba(0,0,0,0.08)!important;}
      .module-card {border-radius:12px; padding:16px; margin-bottom:12px;
                    background:white; box-shadow:0 2px 8px rgba(0,0,0,0.08);}
      .signal-critical {background:#fdecea; border-left:4px solid #E31C23;}
      .signal-high     {background:#fff3e0; border-left:4px solid #C0392B;}
      .signal-moderate {background:#fffde7; border-left:4px solid #F0B323;}
      .signal-info     {background:#e8f5e9; border-left:4px solid #00843E;}
      .freshness-ok   {color:#00843E;}
      .freshness-warn {color:#F0B323;}
      .freshness-crit {color:#E31C23;}
    "))),
    
    tabItems(
      
      # =========================================================
      # VUE CONTINENTALE (commune à tous les modules)
      # =========================================================
      tabItem(
        tabName = "overview_all",
        
        # Bandeau statut consolidé
        fluidRow(
          column(12,
                 tags$div(
                   style = "background:white; border-radius:12px; padding:16px;
                       margin-bottom:16px; box-shadow:0 2px 8px rgba(0,0,0,0.08);",
                   tags$h4(style="margin:0 0 12px;", "Tableau de bord PREIS —
                      Situation épidémiologique en Afrique"),
                   uiOutput("consolidated_status")
                 )
          )
        ),
        
        fluidRow(
          # Carte continentale
          box(width = 8, title = "Carte continentale — tous modules actifs",
              status = "primary", solidHeader = TRUE,
              leafletOutput("map_continental", height = 500)),
          
          # Panneau droite : alertes consolidées
          box(width = 4, title = "Alertes consolidées", status = "danger",
              solidHeader = TRUE,
              uiOutput("consolidated_alerts"),
              tags$hr(),
              uiOutput("freshness_all"))
        ),
        
        fluidRow(
          # KPIs par module
          uiOutput("module_kpi_cards")
        )
      ),
      
      # =========================================================
      # MODULE EBOLA — vues spécifiques
      # =========================================================
      tabItem(
        tabName = "ebola_overview",
        fluidRow(
          valueBoxOutput("eb_cas", 3), valueBoxOutput("eb_deces", 3),
          valueBoxOutput("eb_cfr", 3), valueBoxOutput("eb_zones", 3)
        ),
        fluidRow(
          box(width = 8, title = "Courbe épidémique Ebola DRC", status = "danger",
              solidHeader = TRUE, plotlyOutput("eb_epi_curve", height = 380)),
          box(width = 4, title = "Top zones — cas cumulés", status = "danger",
              solidHeader = TRUE, plotlyOutput("eb_top_zones", height = 380))
        ),
        fluidRow(
          box(width = 12, title = "Signaux d'alerte précoce (validation rétrospective)",
              status = "warning", solidHeader = TRUE,
              DTOutput("eb_signals_table"))
        )
      ),
      
      tabItem(
        tabName = "ebola_map",
        fluidRow(
          box(width = 12, title = "Carte interactive — zones de santé RDC",
              status = "danger", solidHeader = TRUE,
              fluidRow(
                column(3, checkboxInput("eb_show_choro", "Choroplèthe zones", TRUE)),
                column(3, checkboxInput("eb_show_bubbles", "Bulles proportionnelles", TRUE)),
                column(3, checkboxInput("eb_zoom_rdc", "Zoom Est RDC", TRUE)),
                column(3, selectInput("eb_map_indic", "Indicateur",
                                      choices = c("Cas cumulés"="cases","CFR (%)"="cfr"),
                                      selected = "cases"))
              ),
              leafletOutput("eb_map", height = 620))
        )
      ),
      
      tabItem(
        tabName = "ebola_daily",
        fluidRow(
          box(width = 12, status = "danger", solidHeader = TRUE,
              title = "Suivi journalier Ebola DRC",
              fluidRow(
                column(4, selectInput("eb_daily_metric", "Indicateur",
                                      choices = c("Cas cumulés"="cum_cases","Décès cumulés"="cum_deaths",
                                                  "Nouveaux cas/j"="new_cases","CFR (%)"="cfr",
                                                  "Moyenne mobile 7j"="ma7_new_cases"),
                                      selected = "cum_cases")),
                column(4, selectInput("eb_daily_level", "Niveau",
                                      choices = c("National","Province"), selected = "National"))
              ),
              plotlyOutput("eb_daily_plot", height = 400))
        ),
        fluidRow(
          box(width = 12, title = "Données journalières", status = "info",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              DTOutput("eb_daily_table"),
              downloadButton("dl_eb_daily", "Télécharger (CSV)"))
        )
      ),
      
      # =========================================================
      # MODULE POLIO — vues spécifiques
      # =========================================================
      tabItem(
        tabName = "polio_overview",
        fluidRow(
          valueBoxOutput("po_events", 3), valueBoxOutput("po_cases", 3),
          valueBoxOutput("po_countries", 3), valueBoxOutput("po_signals", 3)
        ),
        fluidRow(
          box(width = 8, title = "Carte Polio Afrique — événements par pays",
              status = "primary", solidHeader = TRUE,
              leafletOutput("po_map", height = 420)),
          box(width = 4, title = "Pays avec événements",
              status = "primary", solidHeader = TRUE,
              DTOutput("po_countries_table"))
        ),
        fluidRow(
          box(width = 12, title = "Détail des événements (dernière issue GPEI)",
              status = "info", solidHeader = TRUE,
              DTOutput("po_events_table"),
              tags$p(style="color:#888;font-size:12px;margin-top:8px;",
                     "Source: GPEI / Polio This Week. cVDPV = poliovirus circulant dérivé du vaccin.",
                     "WPV = poliovirus sauvage. CFR non applicable (surveillance, pas de létalité).",
                     "Données validées GPEI — provisional = FALSE."))
        )
      ),
      
      tabItem(
        tabName = "polio_signals",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Signaux Polio — classification épidémiologique",
              tags$div(style="font-size:13px;color:#555;margin-bottom:12px;",
                       tags$strong("Règle de classification :"),
                       tags$ul(style="margin:4px 0;",
                               tags$li(tags$span(style="color:#E31C23;font-weight:600;","CRITIQUE"),
                                       " — WPV (poliovirus sauvage) : menace pour l'éradication mondiale"),
                               tags$li(tags$span(style="color:#C0392B;font-weight:600;","ÉLEVÉ"),
                                       " — cVDPV + cas humain confirmé : transmission active"),
                               tags$li(tags$span(style="color:#F0B323;font-weight:600;","MODÉRÉ"),
                                       " — cVDPV environnement seul : circulation silencieuse"),
                               tags$li(tags$span(style="color:#00843E;font-weight:600;","INFO"),
                                       " — autre événement à surveiller")
                       )
              ),
              DTOutput("po_signals_table"))
        )
      ),
      
      # =========================================================
      # FRAÎCHEUR DES DONNÉES
      # =========================================================
      tabItem(
        tabName = "freshness",
        fluidRow(
          box(width = 12, title = "Statut et fraîcheur — tous les modules PREIS",
              status = "info", solidHeader = TRUE,
              uiOutput("freshness_detail"))
        )
      ),
      
      # =========================================================
      # À PROPOS
      # =========================================================
      tabItem(
        tabName = "about",
        box(width = 12, title = "À propos de PREIS", status = "primary",
            solidHeader = TRUE,
            div(style="font-size:15px;line-height:1.7;",
                tags$h4("PREIS — Pan-African Real-time Epidemiological Intelligence System"),
                tags$p("Développé par ", tags$strong("Dr R. Hyacinthe ZABRE"),
                       ", Épidémiologiste-Biostatisticien, Africa CDC — Programme 5."),
                tags$p("PREIS est une plateforme d'intelligence épidémiologique qui agrège",
                       " automatiquement des données de surveillance de multiples sources",
                       " (SitReps, sites officiels, publications) et les transforme en",
                       " alertes et tableaux de bord opérationnels."),
                tags$h5("Architecture"),
                tags$ul(
                  tags$li(tags$strong("Module Ebola DRC :"),
                          " surveillance autonome en production (GitHub Actions, shinyapps.io).",
                          " SitReps INSP/INRB → extraction → analyse → dashboard → alertes."),
                  tags$li(tags$strong("Module Polio :"),
                          " GPEI Polio This Week → adapter → socle commun → dashboard."),
                  tags$li(tags$strong("Socle commun :"),
                          " format de données unifié (preis_series, preis_zones,",
                          " preis_signals, preis_meta) permettant l'agrégation multi-modules.")
                ),
                tags$h5("Garde-fous méthodologiques"),
                tags$ul(
                  tags$li("CFR toujours provisoire pendant une épidémie active."),
                  tags$li("Signaux = faits + hypothèses à investiguer, jamais un diagnostic."),
                  tags$li("Totaux nationaux = sources officielles validées (INRB, GPEI)."),
                  tags$li("Co-signature INSP/INRB/GPEI requise avant publication officielle.")
                ),
                tags$h5("Référence"),
                tags$p("Preprint PREIS Core publié (OSF/medRxiv) — disponible avec DOI."),
                tags$p(tags$a(href="mailto:raogoz@africacdc.org",
                              "raogoz@africacdc.org")),
                tags$hr(),
                tags$p(style="color:#888;font-size:12px;",
                       "Dashboard PREIS v2.0 (multi-modules) — Africa CDC Programme 5,",
                       " Situation Room d'Intelligence Épidémiologique.")
            ))
      )
    )
  )
)

# --------------------------------------------------------------------------- #
# 7. SERVER
# --------------------------------------------------------------------------- #

server <- function(input, output, session) {
  
  # Langue et traduction
  cur_lang <- reactive({
    l <- input$ui_lang; if (is.null(l)||!(l %in% names(I18N))) "fr" else l })
  tr <- function(key) {
    l <- cur_lang(); v <- I18N[[l]][[key]]
    if (is.null(v)||is.na(v)) I18N[["fr"]][[key]] %||% key else v }
  
  # Module actif
  active_mod <- reactive({ input$active_module %||% "ebola" })
  
  # Menu latéral dynamique selon le module actif
  output$dynamic_menu <- renderMenu({
    mod <- active_mod()
    common_items <- list(
      menuItem(tr("overview_all"), tabName="overview_all",
               icon=icon("globe-africa")),
      menuItem(tr("freshness"),    tabName="freshness",
               icon=icon("clock")),
      menuItem(tr("about"),        tabName="about",
               icon=icon("circle-info"))
    )
    ebola_items <- list(
      menuItem("Ebola DRC", icon=icon("biohazard"),
               menuSubItem("Vue d'ensemble", tabName="ebola_overview"),
               menuSubItem("Carte interactive", tabName="ebola_map"),
               menuSubItem("Suivi journalier", tabName="ebola_daily")
      )
    )
    polio_items <- list(
      menuItem("Polio Afrique", icon=icon("circle-dot"),
               menuSubItem("Vue d'ensemble", tabName="polio_overview"),
               menuSubItem("Signaux d'alerte", tabName="polio_signals")
      )
    )
    module_items <- if (mod == "ebola") ebola_items else polio_items
    do.call(sidebarMenu,
            c(list(id="tabs", selected=if(mod=="ebola") "ebola_overview"
                   else "polio_overview"),
              common_items, module_items))
  })
  
  # ---- STATUT DE TOUS LES MODULES ----
  output$all_modules_status <- renderUI({
    tagList(lapply(MODULE_IDS, function(mid) {
      meta   <- common_data[[mid]]$meta
      status <- if (!is.null(meta)) meta$system_status %||% "no_data" else "no_data"
      n_sig  <- if (!is.null(meta)) meta$n_signals %||% 0 else 0
      tags$div(style="margin-bottom:6px;",
               tags$span(style=paste0("color:", status_color(status), ";"),
                         icon(status_icon(status))),
               tags$span(style="color:#ccc; font-size:12px; margin-left:6px;",
                         MODULES[[mid]]$label),
               tags$span(style="color:#888; font-size:11px; float:right;",
                         if(n_sig>0) paste0(n_sig, " sig.") else "")
      )
    }))
  })
  
  # =========================================================
  # VUE CONTINENTALE
  # =========================================================
  
  output$consolidated_status <- renderUI({
    statuses <- sapply(MODULE_IDS, function(m) {
      meta <- common_data[[m]]$meta
      if (is.null(meta)) "no_data" else meta$system_status %||% "no_data"
    })
    overall <- if ("critical" %in% statuses) "critical"
    else if ("alert" %in% statuses) "alert"
    else if ("warning" %in% statuses) "warning"
    else if ("ok" %in% statuses) "ok" else "no_data"
    
    total_signals <- sum(sapply(MODULE_IDS, function(m) {
      meta <- common_data[[m]]$meta
      if (is.null(meta)) 0 else as.integer(meta$n_signals %||% 0)
    }))
    
    fluidRow(
      column(3, tags$div(style="text-align:center;",
                         tags$div(style=paste0("font-size:28px;color:", status_color(overall), ";"),
                                  icon(status_icon(overall))),
                         tags$div(style="font-size:13px;font-weight:600;",
                                  switch(overall, critical="CRITIQUE",alert="ALERTE",
                                         warning="AVERTISSEMENT",ok="OPÉRATIONNEL","SANS DONNÉES"))
      )),
      column(3, valueBox(total_signals, "Signaux actifs (tous modules)",
                         icon=icon("triangle-exclamation"),
                         color=if(total_signals>0) "red" else "green",
                         width=NULL)),
      column(3, valueBox(length(MODULE_IDS), "Modules surveillés",
                         icon=icon("layer-group"), color="blue", width=NULL)),
      column(3, valueBox(format(Sys.Date(), "%d %b %Y"), "Date du tableau de bord",
                         icon=icon("calendar"), color="navy", width=NULL))
    )
  })
  
  # Carte continentale
  output$map_continental <- renderLeaflet({
    m <- leaflet(options=leafletOptions(minZoom=2, maxZoom=8)) %>%
      addProviderTiles("CartoDB.Positron")
    
    # Couche Afrique
    if (!is.null(africa_sf)) {
      m <- m %>% addPolygons(
        data=africa_sf, fill=FALSE,
        color="#888888", weight=0.5, smoothFactor=0.3)
    }
    
    # Événements Ebola (bulles rouges — zones de santé)
    if (nrow(zones_ebola_proc) > 0) {
      zb <- zones_ebola_proc %>% filter(cases > 0)
      if (nrow(zb) > 0) {
        m <- m %>% addCircleMarkers(
          data=zb, lng=~lon, lat=~lat,
          radius=~pmax(5, sqrt(cases)*1.5),
          stroke=TRUE, weight=1.5, color="#7B241C",
          fillColor="#E31C23", fillOpacity=0.75,
          popup=~paste0("<b>", health_zone, "</b> (Ebola DRC)<br>Cas: ", cases),
          label=~paste0(health_zone, ": ", cases, " cas"),
          group="Ebola DRC")
      }
    }
    
    # Événements Polio (bulles bleues — pays)
    po_zones <- common_data[["polio"]]$zones
    if (!is.null(po_zones) && nrow(po_zones) > 0) {
      pz <- po_zones %>%
        filter(!is.na(geo_lat), !is.na(geo_lon), value > 0)
      if (nrow(pz) > 0) {
        sig_col <- function(sl) dplyr::case_when(
          sl == "critical" ~ "#E31C23",
          sl == "high"     ~ "#C0392B",
          sl == "moderate" ~ "#F0B323",
          TRUE             ~ "#3498DB")
        pz$col <- sig_col(pz$signal_level)
        m <- m %>% addCircleMarkers(
          data=pz, lng=~geo_lon, lat=~geo_lat,
          radius=~pmax(6, sqrt(value)*2),
          stroke=TRUE, weight=1.5, color="#1A5276",
          fillColor=~col, fillOpacity=0.75,
          popup=~paste0("<b>", geo_name, "</b> (Polio)<br>",
                        "Événements: ", value, "<br>",
                        "Niveau: ", signal_level,
                        if(!is.na(polio_virus)) paste0("<br>Virus: ", polio_virus) else ""),
          label=~paste0(geo_name, ": ", value, " événement(s)"),
          group="Polio Afrique")
      }
    }
    
    m %>%
      addLayersControl(
        overlayGroups = c("Ebola DRC", "Polio Afrique"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      fitBounds(lng1=-20, lat1=-36, lng2=52, lat2=38) %>%
      addLegend(position="bottomleft",
                colors=c("#E31C23","#3498DB"),
                labels=c("Ebola DRC (zones)","Polio Afrique (pays)"),
                title="Modules PREIS", opacity=0.8)
  })
  
  # Alertes consolidées
  output$consolidated_alerts <- renderUI({
    all_signals <- lapply(MODULE_IDS, function(mid) {
      s <- common_data[[mid]]$signals
      if (is.null(s) || nrow(s) == 0) return(NULL)
      s %>% mutate(module_label = MODULES[[mid]]$label)
    })
    all_sig <- bind_rows(Filter(Negate(is.null), all_signals))
    if (nrow(all_sig) == 0) {
      return(tags$p(style="color:#888;", "Aucun signal actif."))
    }
    all_sig <- all_sig %>%
      arrange(match(signal_level, c("critical","high","moderate","info")))
    tagList(lapply(seq_len(min(nrow(all_sig), 8)), function(i) {
      r   <- all_sig[i, ]
      cls <- paste0("signal-", r$signal_level)
      tags$div(
        class = cls,
        style = "padding:8px 10px; margin-bottom:6px; border-radius:4px;",
        tags$span(style="font-weight:600; font-size:13px;",
                  r$geo_name, " — ", r$module_label),
        tags$br(),
        tags$span(style="font-size:12px; color:#555;",
                  r$indicator, " | ", toupper(r$signal_level %||% ""))
      )
    }))
  })
  
  # Fraîcheur (sidebar overview)
  output$freshness_all <- renderUI({
    tagList(
      tags$p(style="font-size:11px;font-weight:600;color:#888;
                    text-transform:uppercase;margin-bottom:8px;", "Fraîcheur"),
      lapply(MODULE_IDS, function(mid) {
        freshness_box(mid, common_data[[mid]]$meta)
      })
    )
  })
  
  # KPI cards par module
  output$module_kpi_cards <- renderUI({
    lapply(MODULE_IDS, function(mid) {
      meta   <- common_data[[mid]]$meta
      mod    <- MODULES[[mid]]
      status <- if (!is.null(meta)) meta$system_status %||% "no_data" else "no_data"
      n_sig  <- if (!is.null(meta)) as.integer(meta$n_signals %||% 0) else 0
      last_d <- if (!is.null(meta)) meta$last_report_date %||% "—" else "—"
      column(6, tags$div(
        class = "module-card",
        style = paste0("border-top:4px solid ", status_color(status), ";"),
        fluidRow(
          column(8,
                 tags$h4(style="margin:0 0 4px;", icon(mod$icon), " ", mod$label),
                 tags$p(style="font-size:12px;color:#666;margin:0;", mod$pathogen),
                 tags$p(style="font-size:12px;color:#666;margin:0;", mod$scope)
          ),
          column(4, style="text-align:right;",
                 status_badge(status, toupper(status)), tags$br(), tags$br(),
                 tags$span(style="font-size:22px;font-weight:700;color:#333;", n_sig),
                 tags$br(),
                 tags$span(style="font-size:11px;color:#888;", "signaux")
          )
        ),
        tags$hr(style="margin:8px 0;"),
        fluidRow(
          column(6,
                 tags$span(style="font-size:11px;color:#888;", "Dernier rapport"),
                 tags$br(),
                 tags$span(style="font-size:13px;font-weight:600;", last_d)
          ),
          column(6,
                 tags$span(style="font-size:11px;color:#888;", "Source"),
                 tags$br(),
                 tags$span(style="font-size:12px;", mod$source)
          )
        ),
        tags$div(style="margin-top:10px;",
                 actionButton(paste0("goto_", mid), paste0("Voir le module"),
                              class="btn btn-sm btn-default",
                              icon=icon("arrow-right"),
                              onclick=paste0("Shiny.setInputValue('active_module','", mid, "')"))
        )
      ))
    })
  })
  
  # =========================================================
  # MODULE EBOLA
  # =========================================================
  
  # Données réactives Ebola
  eb_serie <- reactive({
    if (is.null(serie_ebola) || nrow(serie_ebola) == 0) return(tibble())
    serie_ebola %>% arrange(sitrep_no)
  })
  eb_last <- reactive({
    s <- eb_serie(); if (nrow(s) == 0) return(NULL); s %>% tail(1)
  })
  
  output$eb_cas <- renderValueBox({
    lr <- eb_last()
    valueBox(if(is.null(lr)) "—" else format(lr$cas_cumules, big.mark=" "),
             "Cas confirmés cumulés", icon=icon("virus"), color="red")
  })
  output$eb_deces <- renderValueBox({
    lr <- eb_last()
    valueBox(if(is.null(lr)) "—" else format(lr$deces_cumules, big.mark=" "),
             "Décès confirmés", icon=icon("skull"), color="black")
  })
  output$eb_cfr <- renderValueBox({
    lr <- eb_last()
    valueBox(if(is.null(lr)||is.na(lr$cfr)) "—" else paste0(lr$cfr, "%"),
             "CFR provisoire", icon=icon("percent"), color="orange")
  })
  output$eb_zones <- renderValueBox({
    valueBox(nrow(zones_ebola_proc), "Zones de santé touchées",
             icon=icon("location-dot"), color="maroon")
  })
  
  output$eb_epi_curve <- renderPlotly({
    s <- eb_serie()
    if (nrow(s) < 2) return(plotly_empty())
    s <- s %>% mutate(date = as.Date(date),
                      nv = pmax(cas_cumules - lag(cas_cumules, default=0), 0))
    plot_ly(s) %>%
      add_bars(x=~date, y=~nv, name="Nouveaux cas",
               marker=list(color="#E59866")) %>%
      add_lines(x=~date, y=~cas_cumules, name="Cas cumulés", yaxis="y2",
                line=list(color="#7B241C", width=2)) %>%
      layout(yaxis=list(title="Nouveaux cas"),
             yaxis2=list(title="Cumul", overlaying="y", side="right"),
             xaxis=list(title=""), legend=list(orientation="h", y=-0.2),
             margin=list(r=50))
  })
  
  output$eb_top_zones <- renderPlotly({
    z <- zones_ebola_proc %>% arrange(desc(cases)) %>% head(10)
    if (nrow(z) == 0) return(plotly_empty())
    z$health_zone <- factor(z$health_zone, levels=rev(z$health_zone))
    p <- ggplot(z, aes(x=health_zone, y=cases, fill=cases,
                       text=paste0(health_zone,": ",cases," cas"))) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low="#F1948A", high="#7B241C", guide="none") +
      labs(x=NULL, y="Cas cumulés") + theme_minimal(base_size=11)
    ggplotly(p, tooltip="text") %>% layout(showlegend=FALSE)
  })
  
  output$eb_signals_table <- renderDT({
    if (is.null(signals_val) || nrow(signals_val) == 0)
      return(datatable(data.frame(Message="Validation non générée"), rownames=FALSE))
    show <- signals_val %>%
      transmute(`Première détection`=if("first_date" %in% names(.))
        format(as.Date(first_date),"%d/%m/%Y") else NA,
        Zone=zone, `Type de signal`=type,
        Détail=if("detail" %in% names(.)) detail else NA)
    datatable(show, rownames=FALSE, options=list(pageLength=8, dom="tip"))
  })
  
  output$eb_map <- renderLeaflet({
    z  <- zones_ebola_proc %>% filter(cases > 0)
    m  <- leaflet(options=leafletOptions(minZoom=4,maxZoom=10)) %>%
      addProviderTiles("CartoDB.Positron")
    if (!is.null(africa_sf))
      m <- m %>% addPolygons(data=africa_sf, fill=FALSE,
                             color="#aaaaaa", weight=0.5)
    if (!is.null(zones_geo) && isTRUE(input$eb_show_choro)) {
      zg <- zones_geo; zg$val <- z$cases[match(zg$zone, z$health_zone)]
      zg$val[is.na(zg$val)] <- 0
      pal <- colorNumeric(c("#EAF3EC","#F0B323","#E31C23"), domain=zg$val)
      m <- m %>% addPolygons(data=zg[zg$val>0,],
                             fillColor=~pal(val), fillOpacity=0.8,
                             color="#7B241C", weight=1,
                             popup=~paste0("<b>",zone,"</b><br>Cas: ",val))
    }
    if (isTRUE(input$eb_show_bubbles) && nrow(z) > 0) {
      pal2 <- colorNumeric(c("#F1948A","#7B241C"), domain=z$cases)
      m <- m %>% addCircleMarkers(data=z, lng=~lon, lat=~lat,
                                  radius=~pmax(5,sqrt(cases)*1.8),
                                  fillColor=~pal2(cases), fillOpacity=0.8,
                                  color="#7B241C", weight=1.2,
                                  popup=~paste0("<b>",health_zone,"</b><br>Cas: ",cases),
                                  label=~paste0(health_zone,": ",cases))
    }
    if (isTRUE(input$eb_zoom_rdc))
      m <- m %>% fitBounds(27.5,-3.0,31.5,3.2)
    else
      m <- m %>% fitBounds(-20,-36,52,38)
    m
  })
  
  output$eb_daily_plot <- renderPlotly({
    df <- daily_ebola
    if (is.null(df) || nrow(df) == 0) return(plotly_empty())
    lvl <- input$eb_daily_level %||% "National"
    df  <- df %>% filter(level == lvl) %>% arrange(date) %>%
      mutate(date = as.Date(date))
    m   <- input$eb_daily_metric %||% "cum_cases"
    lbl <- c(cum_cases="Cas cumulés", cum_deaths="Décès cumulés",
             new_cases="Nouveaux cas/j", cfr="CFR (%)",
             ma7_new_cases="Moyenne mobile 7j")[m]
    df$y <- df[[m]]
    plot_ly(df, x=~date, y=~y, type="scatter", mode="lines+markers",
            line=list(color="#C0392B", width=2),
            marker=list(color="#7B241C", size=5),
            hovertemplate=paste0("%{x}<br>",lbl,": %{y}<extra></extra>")) %>%
      layout(xaxis=list(title=""), yaxis=list(title=lbl, rangemode="tozero"))
  })
  
  output$eb_daily_table <- renderDT({
    df <- daily_ebola
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    df <- df %>% filter(level == (input$eb_daily_level %||% "National")) %>%
      arrange(desc(date)) %>%
      select(Date=date, Cas=cum_cases, Décès=cum_deaths,
             `Nvx cas`=new_cases, CFR=cfr, `MA7j`=ma7_new_cases)
    datatable(df, rownames=FALSE, options=list(pageLength=10, dom="tip"))
  })
  
  output$dl_eb_daily <- downloadHandler(
    filename=function() paste0("ebola_daily_", Sys.Date(), ".csv"),
    content=function(f) {
      df <- daily_ebola
      if (!is.null(df)) write_csv(df, f)
    })
  
  # =========================================================
  # MODULE POLIO
  # =========================================================
  
  po_data <- reactive({ common_data[["polio"]] })
  
  output$po_events <- renderValueBox({
    d <- po_data()$zones
    v <- if(!is.null(d) && nrow(d)>0) sum(d$value, na.rm=TRUE) else "—"
    valueBox(v, "Événements Polio (total)", icon=icon("circle-dot"), color="blue")
  })
  output$po_cases <- renderValueBox({
    d <- po_data()$zones
    v <- if(!is.null(d) && nrow(d)>0 && "polio_cases" %in% names(d))
      sum(d$polio_cases, na.rm=TRUE) else "—"
    valueBox(v, "Cas humains confirmés", icon=icon("person"), color="maroon")
  })
  output$po_countries <- renderValueBox({
    d <- po_data()$zones
    v <- if(!is.null(d)) nrow(d) else "—"
    valueBox(v, "Pays avec événements", icon=icon("globe-africa"), color="navy")
  })
  output$po_signals <- renderValueBox({
    d <- po_data()$signals
    v <- if(!is.null(d)) nrow(d) else "—"
    col <- if(!is.null(d) && nrow(d)>0) "orange" else "green"
    valueBox(v, "Signaux actifs", icon=icon("triangle-exclamation"), color=col)
  })
  
  output$po_map <- renderLeaflet({
    pz <- po_data()$zones
    m  <- leaflet(options=leafletOptions(minZoom=2,maxZoom=7)) %>%
      addProviderTiles("CartoDB.Positron")
    if (!is.null(africa_sf))
      m <- m %>% addPolygons(data=africa_sf, fill=FALSE,
                             color="#aaaaaa", weight=0.4)
    if (!is.null(pz) && nrow(pz)>0) {
      pz <- pz %>% filter(!is.na(geo_lat),!is.na(geo_lon), value>0)
      if (nrow(pz)>0) {
        sig_col <- function(sl) dplyr::case_when(
          sl=="critical" ~ "#E31C23", sl=="high" ~ "#C0392B",
          sl=="moderate" ~ "#F0B323", TRUE ~ "#3498DB")
        pz$col <- sig_col(pz$signal_level)
        m <- m %>% addCircleMarkers(
          data=pz, lng=~geo_lon, lat=~geo_lat,
          radius=~pmax(6,sqrt(value)*2.5),
          fillColor=~col, fillOpacity=0.8,
          color="#1A5276", weight=1.5,
          popup=~paste0("<b>",geo_name,"</b><br>",
                        "Événements: ",value,"<br>",
                        "Cas humains: ",polio_cases,"<br>",
                        "Env. samples: ",polio_env,"<br>",
                        "Virus: ",polio_virus,"<br>",
                        "Niveau: ",toupper(signal_level)),
          label=~paste0(geo_name,": ",value," événement(s) | ",
                        toupper(signal_level)))
      }
    }
    m %>% fitBounds(-20,-36,52,38) %>%
      addLegend(position="bottomleft",
                colors=c("#E31C23","#C0392B","#F0B323","#3498DB"),
                labels=c("Critique (WPV)","Élevé (cVDPV+cas)",
                         "Modéré (env. seul)","Info"),
                title="Niveau de signal", opacity=0.8)
  })
  
  output$po_countries_table <- renderDT({
    d <- po_data()$zones
    if (is.null(d) || nrow(d)==0)
      return(datatable(data.frame(Message="Données indisponibles"), rownames=FALSE))
    show <- d %>%
      arrange(match(signal_level,c("critical","high","moderate","info")),
              desc(value)) %>%
      transmute(Pays=geo_name, RCC=rcc,
                Événements=value, Cas=polio_cases,
                Niveau=toupper(signal_level),
                Virus=polio_virus)
    datatable(show, rownames=FALSE, options=list(pageLength=10, dom="tip"))
  })
  
  output$po_events_table <- renderDT({
    d <- po_data()$series
    if (is.null(d) || nrow(d)==0)
      return(datatable(data.frame(Message="Données indisponibles"), rownames=FALSE))
    show <- d %>%
      arrange(desc(report_date), geo_name) %>%
      transmute(Date=as.character(report_date), Pays=geo_name,
                Code=geo_code, Indicateur=indicator,
                Valeur=value, Niveau=toupper(signal_level))
    datatable(show, rownames=FALSE, options=list(pageLength=12, dom="ftip"))
  })
  
  output$po_signals_table <- renderDT({
    d <- po_data()$signals
    if (is.null(d) || nrow(d)==0)
      return(datatable(data.frame(Message="Aucun signal actif"), rownames=FALSE))
    show <- d %>%
      arrange(match(signal_level,c("critical","high","moderate","info")),
              geo_name) %>%
      transmute(Pays=geo_name, Code=geo_code,
                Pathogène=indicator,
                Niveau=toupper(signal_level),
                Détail=signal_detail,
                `À investiguer`=signal_hypotheses)
    dt <- datatable(show, rownames=FALSE, escape=FALSE,
                    options=list(pageLength=10, dom="ftip"))
    # Colorisation des lignes par niveau
    dt %>%
      formatStyle("Niveau",
                  target="row",
                  backgroundColor=styleEqual(
                    c("CRITICAL","HIGH","MODERATE","INFO"),
                    c("#fdecea","#fff3e0","#fffde7","#e8f5e9")))
  })
  
  # =========================================================
  # FRAÎCHEUR DÉTAILLÉE
  # =========================================================
  
  output$freshness_detail <- renderUI({
    tagList(lapply(MODULE_IDS, function(mid) {
      meta <- common_data[[mid]]$meta
      mod  <- MODULES[[mid]]
      if (is.null(meta)) {
        return(tags$div(class="module-card",
                        tags$h4(icon(mod$icon), " ", mod$label),
                        tags$p(style="color:#E31C23;", "Données non disponibles — adapter non exécuté.")))
      }
      status   <- meta$system_status %||% "no_data"
      n_sig    <- as.integer(meta$n_signals %||% 0)
      n_series <- as.integer(meta$n_series_rows %||% 0)
      sig_by   <- meta$signals_by_level %||% list()
      
      tags$div(class="module-card",
               style=paste0("border-top:4px solid ", status_color(status), ";"),
               fluidRow(
                 column(8,
                        tags$h4(style="margin:0 0 8px;", icon(mod$icon), " ", mod$label),
                        tags$p(style="color:#666;margin:0;", mod$pathogen),
                        tags$p(style="color:#666;margin:0;font-size:12px;", mod$scope)
                 ),
                 column(4, style="text-align:right;",
                        status_badge(status, toupper(status)))
               ),
               tags$hr(style="margin:10px 0;"),
               fluidRow(
                 column(3,
                        tags$div(style="font-size:11px;color:#888;", "Dernier rapport"),
                        tags$div(style="font-size:14px;font-weight:600;",
                                 meta$last_report_date %||% "—")),
                 column(3,
                        tags$div(style="font-size:11px;color:#888;", "Extrait le"),
                        tags$div(style="font-size:13px;",
                                 substr(meta$last_extracted_at %||% "—", 1, 16))),
                 column(3,
                        tags$div(style="font-size:11px;color:#888;", "Lignes série"),
                        tags$div(style="font-size:14px;font-weight:600;", n_series)),
                 column(3,
                        tags$div(style="font-size:11px;color:#888;", "Source"),
                        tags$div(style="font-size:13px;", mod$source))
               ),
               if (n_sig > 0) {
                 tags$div(style="margin-top:10px;background:#f8f8f8;
                          border-radius:8px;padding:8px 12px;",
                          tags$span(style="font-size:12px;font-weight:600;", "Signaux par niveau : "),
                          tags$span(style="color:#E31C23;",
                                    paste0("Critique: ", sig_by$critical %||% 0)),
                          tags$span(style="margin:0 6px;color:#ccc;", "|"),
                          tags$span(style="color:#C0392B;",
                                    paste0("Élevé: ", sig_by$high %||% 0)),
                          tags$span(style="margin:0 6px;color:#ccc;", "|"),
                          tags$span(style="color:#F0B323;",
                                    paste0("Modéré: ", sig_by$moderate %||% 0)),
                          tags$span(style="margin:0 6px;color:#ccc;", "|"),
                          tags$span(style="color:#00843E;",
                                    paste0("Info: ", sig_by$info %||% 0))
                 )
               }
      )
    }))
  })
  
  # Navigation via boutons KPI cards
  observeEvent(input$active_module, {
    mod <- input$active_module
    tab <- if (mod == "ebola") "ebola_overview" else "polio_overview"
    updateTabItems(session, "tabs", tab)
  })
}

shinyApp(ui, server)

# FIN : app_unified.R