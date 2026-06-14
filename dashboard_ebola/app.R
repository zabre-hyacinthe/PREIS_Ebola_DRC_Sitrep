## ============================================================
## PREIS EBOLA RDC — DASHBOARD (app.R)
## Adapté de PREIS_Polio_FV.
## Carte Afrique (RCC) + zoom RDC + cas localisés par ZONE DE SANTÉ.
## Thème intensité épidémique (dégradé rouge).
## Données : analyse consolidée INRB (à jour, SitRep 1-28).
## ============================================================

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard)
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(DT); library(plotly); library(leaflet); library(sf)
  library(ggplot2); library(htmltools); library(scales)
})

options(shiny.maxRequestSize = 100 * 1024^2)
if (requireNamespace("sf", quietly = TRUE)) sf::sf_use_s2(FALSE)

# ------------------------------------------------------------
# INTERNATIONALISATION — 6 langues officielles Africa CDC :
# FR français, EN anglais, PT portugais, ES espagnol,
# SW kiswahili, AR arabe.
# ------------------------------------------------------------
I18N <- list(
  fr = c(
    app_title="PREIS Ebola RDC", map="Carte", overview="Vue d'ensemble",
    kpi="Indicateurs (KPI)", daily="Suivi journalier", cfr_tab="Analyse CFR",
    faq="Questions fréquentes", synthesis="Synthèse narrative", zones="Zones", about="À propos",
    province="Province", indicator="Indicateur", language="Langue",
    map_cases="Cas confirmés cumulés", map_deaths="Décès confirmés cumulés",
    map_cfr="Létalité CFR (%) — provisoire", map_newcases="Nouveaux cas (7 j)",
    show_africa="Contour Afrique/RDC", show_choro="Zones (choroplèthe)",
    show_bubbles="Bulles proportionnelles", zoom_rdc="Zoom sur l'Est RDC",
    map_full_title="Carte interactive — zones de santé",
    faq_question="Question", faq_answer="Réponse", faq_illustration="Illustration"),
  en = c(
    app_title="PREIS Ebola DRC", map="Map", overview="Overview",
    kpi="Indicators (KPI)", daily="Daily tracking", cfr_tab="CFR analysis",
    faq="Frequently asked questions", synthesis="Narrative summary", zones="Zones", about="About",
    province="Province", indicator="Indicator", language="Language",
    map_cases="Cumulative confirmed cases", map_deaths="Cumulative confirmed deaths",
    map_cfr="Case-fatality ratio (%) — provisional", map_newcases="New cases (7 d)",
    show_africa="Africa/DRC outline", show_choro="Health zones (choropleth)",
    show_bubbles="Proportional bubbles", zoom_rdc="Zoom on eastern DRC",
    map_full_title="Interactive map — health zones",
    faq_question="Question", faq_answer="Answer", faq_illustration="Illustration"),
  pt = c(
    app_title="PREIS Ebola RDC", map="Mapa", overview="Visão geral",
    kpi="Indicadores (KPI)", daily="Acompanhamento diário", cfr_tab="Análise CFR",
    faq="Perguntas frequentes", synthesis="Resumo narrativo", zones="Zonas", about="Sobre",
    province="Província", indicator="Indicador", language="Idioma",
    map_cases="Casos confirmados acumulados", map_deaths="Óbitos confirmados acumulados",
    map_cfr="Taxa de letalidade (%) — provisória", map_newcases="Novos casos (7 d)",
    show_africa="Contorno África/RDC", show_choro="Zonas de saúde (coroplético)",
    show_bubbles="Bolhas proporcionais", zoom_rdc="Zoom no leste da RDC",
    map_full_title="Mapa interativo — zonas de saúde",
    faq_question="Pergunta", faq_answer="Resposta", faq_illustration="Ilustração"),
  es = c(
    app_title="PREIS Ébola RDC", map="Mapa", overview="Resumen general",
    kpi="Indicadores (KPI)", daily="Seguimiento diario", cfr_tab="Análisis CFR",
    faq="Preguntas frecuentes", synthesis="Resumen narrativo", zones="Zonas", about="Acerca de",
    province="Provincia", indicator="Indicador", language="Idioma",
    map_cases="Casos confirmados acumulados", map_deaths="Muertes confirmadas acumuladas",
    map_cfr="Letalidad CFR (%) — provisional", map_newcases="Casos nuevos (7 d)",
    show_africa="Contorno África/RDC", show_choro="Zonas de salud (coroplético)",
    show_bubbles="Burbujas proporcionales", zoom_rdc="Zoom en el este de la RDC",
    map_full_title="Mapa interactivo — zonas de salud",
    faq_question="Pregunta", faq_answer="Respuesta", faq_illustration="Ilustración"),
  sw = c(
    app_title="PREIS Ebola DRC", map="Ramani", overview="Muhtasari",
    kpi="Viashiria (KPI)", daily="Ufuatiliaji wa kila siku", cfr_tab="Uchambuzi wa CFR",
    faq="Maswali yanayoulizwa mara kwa mara", synthesis="Muhtasari wa maelezo",
    zones="Maeneo", about="Kuhusu",
    province="Jimbo", indicator="Kiashiria", language="Lugha",
    map_cases="Visa vilivyothibitishwa jumla", map_deaths="Vifo vilivyothibitishwa jumla",
    map_cfr="Kiwango cha vifo CFR (%) — cha muda", map_newcases="Visa vipya (siku 7)",
    show_africa="Mpaka Afrika/DRC", show_choro="Maeneo ya afya (choropleth)",
    show_bubbles="Viputo sawia", zoom_rdc="Kuza mashariki mwa DRC",
    map_full_title="Ramani shirikishi — maeneo ya afya",
    faq_question="Swali", faq_answer="Jibu", faq_illustration="Mchoro"),
  ar = c(
    app_title="PREIS إيبولا الكونغو", map="خريطة", overview="نظرة عامة",
    kpi="المؤشرات", daily="المتابعة اليومية", cfr_tab="تحليل معدل الوفيات",
    faq="الأسئلة الشائعة", synthesis="ملخص سردي", zones="المناطق", about="حول",
    province="المقاطعة", indicator="المؤشر", language="اللغة",
    map_cases="إجمالي الحالات المؤكدة", map_deaths="إجمالي الوفيات المؤكدة",
    map_cfr="معدل الإماتة (%) — مؤقت", map_newcases="حالات جديدة (7 أيام)",
    show_africa="حدود أفريقيا/الكونغو", show_choro="المناطق الصحية (خرائط)",
    show_bubbles="فقاعات نسبية", zoom_rdc="تكبير شرق الكونغو",
    map_full_title="خريطة تفاعلية — المناطق الصحية",
    faq_question="سؤال", faq_answer="إجابة", faq_illustration="رسم توضيحي")
)
LANG_CHOICES <- c("Français"="fr", "English"="en", "Português"="pt",
                  "Español"="es", "Kiswahili"="sw", "العربية"="ar")

# Module de synthèse narrative (3 niveaux). Optionnel : si absent,
# l'onglet Synthèse affichera un message au lieu de planter.
.narr_fp <- file.path(normalizePath(getwd(), winslash="/", mustWork=FALSE),
                      "05_synthese_narrative.R")
HAS_NARR <- file.exists(.narr_fp)
if (HAS_NARR) source(.narr_fp)

# ------------------------------------------------------------
# CHEMINS (relatifs — compatibles shinyapps.io)
# ------------------------------------------------------------
ROOT_DIR    <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
DATA_DIR    <- file.path(ROOT_DIR, "data")
CURATED_DIR <- file.path(DATA_DIR, "curated")
ANALYSE_DIR <- file.path(ROOT_DIR, "outputs", "analyse")

# Sources possibles pour la série & les zones (selon déploiement)
find_first <- function(paths) { p <- paths[file.exists(paths)]; if (length(p)) p[1] else NA_character_ }

SERIE_FP <- find_first(c(
  file.path(ANALYSE_DIR, "serie_temporelle_nationale.csv"),
  file.path(DATA_DIR, "serie_temporelle_nationale.csv")
))
ZONES_FP <- find_first(c(
  file.path(ANALYSE_DIR, "tableau_zones_sante.csv"),
  file.path(DATA_DIR, "tableau_zones_sante.csv")
))
AFRICA_FP <- find_first(c(
  file.path(CURATED_DIR, "africa_countries_rcc.geojson"),
  file.path(DATA_DIR, "africa_countries_rcc.geojson")
))
ZONES_GEO_FP <- find_first(c(
  file.path(CURATED_DIR, "rdc_zones_sante_est.geojson"),
  file.path(DATA_DIR, "rdc_zones_sante_est.geojson")
))
LONG_FP <- find_first(c(
  file.path(DATA_DIR, "PREIS_indicators_long.csv"),
  file.path(ROOT_DIR, "data", "final", "PREIS_indicators_long.csv")
))
DAILY_FP <- find_first(c(
  file.path(DATA_DIR, "PREIS_daily_indicators.csv"),
  file.path(ROOT_DIR, "data", "final", "PREIS_daily_indicators.csv")
))

# Dictionnaire : code technique -> libellé lisible (indicateurs suivis dans le temps)
INDIC_LABELS <- c(
  cumulative_confirmed_cases   = "Cas confirmés cumulés",
  cumulative_deaths            = "Décès cumulés",
  case_fatality_ratio          = "Létalité CFR (%) — provisoire",
  new_confirmed_cases          = "Nouveaux cas confirmés",
  suspected_cases_investigation= "Cas suspects en investigation",
  recovered                    = "Guéris (cumulés)",
  contacts_listed              = "Contacts listés",
  contacts_followup_rate       = "Taux de suivi des contacts (%)",
  cumulative_contacts_traced   = "Contacts tracés (cumulés)",
  cumulative_contacts_isolated = "Contacts isolés (cumulés)",
  patients_in_isolation        = "Patients en isolement",
  hospitalised                 = "Hospitalisés",
  samples_analyzed             = "Échantillons analysés",
  samples_positive             = "Échantillons positifs",
  lab_positivity_rate          = "Taux de positivité labo (%)",
  alerts_investigation_rate    = "Taux d'investigation des alertes (%)",
  travellers_screened          = "Voyageurs dépistés (PoE)"
)

# ------------------------------------------------------------
# COORDONNÉES DES ZONES DE SANTÉ (chef-lieu approx.)
# ------------------------------------------------------------
zone_coords <- tibble::tribble(
  ~health_zone,   ~province,     ~lat,    ~lon,
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

# Harmonise les doublons orthographiques venant des données INRB
canon_zone <- function(x) {
  x <- str_squish(as.character(x))
  dplyr::recode(x,
    "Mongbalu"="Mongbwalu","Nyakunde"="Nyankunde","Gethy"="Gety",
    .default = x)
}

# ------------------------------------------------------------
# CHARGEMENT DES DONNÉES
# ------------------------------------------------------------
load_serie <- function() {
  if (is.na(SERIE_FP)) return(tibble())
  tryCatch(read_csv(SERIE_FP, show_col_types = FALSE), error=function(e) tibble())
}
load_zones <- function() {
  if (is.na(ZONES_FP)) return(tibble())
  z <- tryCatch(read_csv(ZONES_FP, show_col_types = FALSE), error=function(e) tibble())
  if (nrow(z) == 0) return(z)
  # tableau_zones_sante.csv a colonnes : nom, cas
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
load_africa <- function() {
  if (is.na(AFRICA_FP)) return(NULL)
  tryCatch(sf::read_sf(AFRICA_FP, quiet = TRUE), error=function(e) NULL)
}
load_zones_geo <- function() {
  if (is.na(ZONES_GEO_FP)) return(NULL)
  tryCatch(sf::read_sf(ZONES_GEO_FP, quiet = TRUE), error=function(e) NULL)
}

serie_all  <- load_serie()
zones_all  <- load_zones()
africa_sf  <- load_africa()
zones_geo  <- load_zones_geo()

# Base longue (tous indicateurs) + jointure dates depuis la série
load_long <- function() {
  if (is.na(LONG_FP)) return(tibble())
  d <- tryCatch(read_csv(LONG_FP, show_col_types = FALSE), error=function(e) tibble())
  if (nrow(d) == 0) return(d)
  d %>% filter(indicator_code %in% names(INDIC_LABELS))
}
long_all <- load_long()
# Série journalière (national + province) calculée par 11_daily_indicators.R
load_daily <- function() {
  if (is.na(DAILY_FP)) return(tibble())
  d <- tryCatch(read_csv(DAILY_FP, show_col_types = FALSE), error = function(e) tibble())
  if (nrow(d) == 0) return(d)
  d %>% mutate(date = as.Date(date))
}
daily_all <- load_daily()
# Mapping sitrep -> date (depuis la série consolidée)
sno_date <- if (nrow(serie_all))
  serie_all %>% select(sitrep_no, date) %>% distinct() else tibble()
# Indicateurs réellement présents dans la base, pour le menu
indic_present <- if (nrow(long_all))
  intersect(names(INDIC_LABELS), unique(long_all$indicator_code)) else character()
indic_choices <- setNames(indic_present, INDIC_LABELS[indic_present])

# Dernier SitRep dispo
last_sno <- if (nrow(serie_all)) max(serie_all$sitrep_no, na.rm=TRUE) else NA
sno_choices <- if (nrow(serie_all)) sort(unique(serie_all$sitrep_no)) else integer()

# Palette intensité (dégradé rouge épidémie)
EBOLA_RED   <- "#C0392B"
EBOLA_DARK  <- "#7B241C"
EBOLA_LIGHT <- "#F1948A"

header_title <- "PREIS Ebola RDC — Surveillance MVE"

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = header_title, titleWidth = 320),
  dashboardSidebar(
    width = 320,
    sidebarMenuOutput("dynamic_menu"),
    br(),
    sliderInput("sitrep", "SitRep (jusqu'à)",
                min = if(length(sno_choices)) min(sno_choices) else 1,
                max = if(length(sno_choices)) max(sno_choices) else 28,
                value = if(!is.na(last_sno)) last_sno else 28, step = 1, sep = ""),
    selectInput("province", "Province",
                choices = c("Toutes", sort(unique(zone_coords$province))),
                selected = "Toutes"),
    sliderInput("min_cases", "Seuil cas minimum (carte)",
                min = 0, max = 50, value = 0, step = 1),
    selectInput("ui_lang", "Langue / Language",
                choices = LANG_CHOICES, selected = "en"),
    selectInput("map_indic", "Indicateur carte / Map indicator",
                choices = c("Cas cumulés / Cases"        = "cases",
                            "Décès cumulés / Deaths"      = "deaths",
                            "Létalité CFR (%)"            = "cfr",
                            "Nouveaux cas 7j / New cases" = "new_cases"),
                selected = "cases"),
    checkboxInput("show_africa", "Contour Afrique/RDC", value = TRUE),
    checkboxInput("show_choro", "Zones (choroplèthe)", value = TRUE),
    checkboxInput("show_bubbles", "Bulles proportionnelles", value = TRUE),
    checkboxInput("zoom_rdc", "Zoom Est RDC", value = TRUE)
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-red .main-header .logo {background:linear-gradient(90deg,#7B241C 0%,#C0392B 100%)!important;font-weight:700;}
      .skin-red .main-header .navbar {background:linear-gradient(90deg,#7B241C 0%,#C0392B 100%)!important;}
      .skin-red .main-sidebar {background-color:#2C1512!important;}
      .content-wrapper,.right-side {background-color:#f5f3f2!important;}
      .small-box {border-radius:12px!important;box-shadow:0 2px 8px rgba(0,0,0,0.10)!important;min-height:120px!important;}
      .small-box h3 {font-size:32px!important;font-weight:700!important;}
      .box {border-radius:12px;box-shadow:0 2px 8px rgba(0,0,0,0.08)!important;}
      .box.box-solid.box-primary>.box-header {background:#C0392B!important;}
      .box.box-primary {border-top-color:#C0392B!important;}
      .note-block {font-size:14px;line-height:1.55;color:#2d3436;}
      .ebola-blink {animation: ebolaBlink 1.1s infinite alternate;}
      @keyframes ebolaBlink {from{opacity:0.85;} to{opacity:1;}}
    "))),
    tabItems(
      # ---- Vue d'ensemble ----
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_cas", 3),
          valueBoxOutput("vb_deces", 3),
          valueBoxOutput("vb_cfr", 3),
          valueBoxOutput("vb_zones", 3)
        ),
        fluidRow(
          box(width = 8, title = "Carte — cas par zone de santé", status="primary",
              solidHeader = TRUE, leafletOutput("map_overview", height = 520)),
          box(width = 4, title = "Top zones touchées", status="primary",
              solidHeader = TRUE, plotlyOutput("plot_topzones", height = 520))
        ),
        fluidRow(
          box(width = 7, title = "Courbe épidémique (incidence + cumul)", status="primary",
              solidHeader = TRUE, plotlyOutput("plot_epi", height = 300)),
          box(width = 5, title = "Interprétation opérationnelle", status="primary",
              solidHeader = TRUE,
              uiOutput("sitrep_button"),
              div(class="note-block", uiOutput("interpretation")))
        )
      ),
      # ---- Indicateurs (KPI) ----
      tabItem(
        tabName = "kpi",
        fluidRow(
          valueBoxOutput("kpi_growth", 3),
          valueBoxOutput("kpi_inc7", 3),
          valueBoxOutput("kpi_trend", 3),
          valueBoxOutput("kpi_double", 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_active_zones", 3),
          valueBoxOutput("kpi_concentration", 3),
          valueBoxOutput("kpi_cfr", 3),
          valueBoxOutput("kpi_completeness", 3)
        ),
        fluidRow(
          box(width = 12, title = "Évolution d'un indicateur dans le temps (SitRep 1 → dernier)",
              status = "primary", solidHeader = TRUE,
              fluidRow(
                column(5, selectInput("kpi_indic", "Indicateur à tracer",
                                      choices = indic_choices,
                                      selected = if(length(indic_choices)) indic_choices[1] else NULL)),
                column(3, checkboxInput("kpi_show_points", "Afficher les points", TRUE))
              ),
              plotlyOutput("plot_kpi_evol", height = 340))
        ),
        fluidRow(
          box(width = 12, title = "Lecture opérationnelle des indicateurs (→ action)",
              status = "primary", solidHeader = TRUE,
              div(class = "note-block", uiOutput("kpi_actions")))
        ),
        fluidRow(
          box(width = 12, title = "Variables à collecter pour débloquer les KPI avancés",
              status = "warning", solidHeader = TRUE,
              div(class = "note-block",
                p(strong("Pour mesurer si la transmission est sous contrôle (Rt, traçage) :")),
                tags$ul(
                  tags$li("Date de début des symptômes par cas (→ courbe épidémique réelle, Rt)"),
                  tags$li("% de cas issus de contacts déjà listés (→ chaînes connues vs invisibles)"),
                  tags$li("Nombre de contacts identifiés et % suivis (→ couverture du traçage)")
                ),
                p(strong("Pour mesurer si la prise en charge sauve des vies :")),
                tags$ul(
                  tags$li("% de décès communautaires (hors centre de traitement)"),
                  tags$li("Délai début symptômes → isolement")
                ),
                p(strong("Pour les projections crédibles :")),
                tags$ul(
                  tags$li("Dates de symptômes + dénominateur population par zone + intervalle sériel Bundibugyo"),
                  tags$li(em("Sans dates de symptômes, toute projection serait de la fausse précision — non fournie."))
                ),
                p("Ces variables figurent dans la ", strong("ligne-liste (line list)"),
                  " détaillée de l'INRB que les SitReps résument.")
              ))
        )
      ),
      # ---- Suivi journalier (national + province) ----
      tabItem(
        tabName = "daily",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Suivi journalier depuis le début de l'épidémie",
              fluidRow(
                column(3, selectInput("daily_level", "Niveau",
                       choices = c("National", "Province", "Zone de santé" = "Zone"),
                       selected = "National")),
                column(3, conditionalPanel(
                  condition = "input.daily_level == 'Province'",
                  selectInput("daily_prov", "Province",
                       choices = c("Ituri", "Nord-Kivu", "Sud-Kivu"), selected = "Ituri"))),
                column(3, conditionalPanel(
                  condition = "input.daily_level == 'Zone'",
                  selectInput("daily_zone", "Zone de santé",
                       choices = NULL))),
                column(3, selectInput("daily_metric", "Indicateur",
                       choices = c("Cumul cas"            = "cum_cases",
                                   "Cumul décès"          = "cum_deaths",
                                   "Nouveaux cas / jour"  = "new_cases",
                                   "Nouveaux décès / jour"= "new_deaths",
                                   "Létalité CFR (%)"     = "cfr",
                                   "Moyenne mobile 7j (cas)" = "ma7_new_cases"),
                       selected = "cum_cases")))
          )),
        fluidRow(
          valueBoxOutput("d_cum_cases", 3),
          valueBoxOutput("d_cum_deaths", 3),
          valueBoxOutput("d_cfr", 3),
          valueBoxOutput("d_ma7", 3)
        ),
        fluidRow(
          box(width = 12, title = "Courbe épidémique (incidence + cumul)",
              status = "primary", solidHeader = TRUE,
              radioButtons("epi_mode", NULL, inline = TRUE,
                choices = c("Incidence + cumul" = "inc_cum",
                            "Incidence + moyenne mobile 7j" = "inc_ma7"),
                selected = "inc_cum"),
              plotlyOutput("epi_curve", height = 400),
              uiOutput("epi_note"))
        ),
        fluidRow(
          box(width = 12, title = "Évolution d'un indicateur", status = "primary",
              solidHeader = TRUE, plotlyOutput("daily_plot", height = 340))
        ),
        fluidRow(
          box(width = 12, title = "Tableau journalier", status = "info",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              DTOutput("daily_table"),
              tags$br(),
              downloadButton("dl_daily", "Télécharger la série journalière (CSV)"),
              tags$p(style = "color:#888; font-size:12px; margin-top:8px;",
                "Note : certains jours sans SitRep créent des écarts. ",
                "Une valeur négative de nouveaux cas (ex. 30 mai) correspond à ",
                "une révision à la baisse de l'INRB (harmonisation), pas à une erreur. ",
                "CFR provisoire (épidémie active). Seules les zones avec assez de ",
                "données apparaissent dans le filtre zone."))
        )
      ),
      # ---- Analyse CFR (graphique apprécié par Africa CDC) ----
      tabItem(
        tabName = "cfr",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Létalité provisoire vs nombre de cas par zone de santé",
              plotlyOutput("cfr_scatter", height = 460),
              tags$p(style = "color:#888; font-size:12px; margin-top:8px;",
                "Chaque point = une zone de santé. Axe X = cas cumulés (échelle log), ",
                "axe Y = létalité provisoire (%), taille = nombre de cas. ",
                "Les zones à gauche (peu de cas) ont une létalité statistiquement ",
                "instable : à ne pas surinterpréter. Ligne pointillée = létalité ",
                "nationale. Couleurs Africa CDC (vert = faible, rouge = élevé). ",
                "CFR provisoire (épidémie active)."))
        )
      ),
      # ---- Questions fréquentes (Q&R analytiques) ----
      tabItem(
        tabName = "faq",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Frequently asked questions — analytical answers",
              tags$p(style = "color:#555; margin-bottom:12px;",
                "Select a question. The answer combines a validated interpretation ",
                "with figures computed live on the most recent data (current SitRep). ",
                "The case-fatality ratio is always provisional."),
              selectInput("faq_q", "Question",
                choices = list(
                  "Population / general public" = c(
                    "Which health zones are most affected right now?"              = "q_top_zones",
                    "Is the outbreak growing or slowing down?"                     = "q_national_trend",
                    "What is being done to protect the population (border screening)?" = "q_poe"),
                  "Health zones / response" = c(
                    "Where is mortality concentrated (high-lethality foci)?"       = "q_mortality",
                    "How many suspected cases are under investigation?"            = "q_suspects",
                    "Is contact tracing under control?"                            = "q_contacts",
                    "What is the hospital burden (ETC)?"                           = "q_hosp"),
                  "DRC / RCC / Africa CDC / partners" = c(
                    "What is the situation by province?"                            = "q_province",
                    "What is the regional (cross-border) spread risk?"             = "q_crossborder",
                    "What are the response's priority needs?"                      = "q_needs"),
                  "Scientists / epidemiologists" = c(
                    "Can we analyse the sex ratio / sexual transmission?"          = "q_sexratio",
                    "Can we make projections (R0, SEIR model)?"                    = "q_projection",
                    "Why do some zones show a 100% case-fatality ratio?"           = "q_cfr_unstable",
                    "What are the limitations of the available data?"              = "q_datalimits")),
                width = "100%"))
        ),
        fluidRow(
          box(width = 12, status = "info", solidHeader = TRUE,
              title = "Answer", htmlOutput("faq_answer"))
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Illustration", uiOutput("faq_plot_ui"))
        )
      ),
      # ---- Synthèse narrative (3 niveaux) ----
      tabItem(
        tabName = "synthese",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = "Synthèse narrative automatique — fondée sur les chiffres validés",
              div(class = "note-block",
                  uiOutput("synth_national"),
                  tags$hr(),
                  uiOutput("synth_zones"),
                  tags$hr(),
                  uiOutput("synth_strategique")),
              tags$hr(),
              downloadButton("dl_synthese", "Télécharger la synthèse (texte)"))
        )
      ),
      # ---- Carte plein écran ----
      tabItem(
        tabName = "map",
        fluidRow(box(width = 12, title="Carte interactive — Afrique / RDC / zones de santé",
                     status="primary", solidHeader = TRUE,
                     leafletOutput("map_full", height = 760)))
      ),
      # ---- Zones ----
      tabItem(
        tabName = "zones",
        fluidRow(box(width = 12, title="Zones de santé touchées", status="primary",
                     solidHeader = TRUE, DTOutput("tbl_zones"))),
        fluidRow(box(width = 12, title="Télécharger", status="primary",
                     solidHeader = TRUE,
                     downloadButton("dl_zones","Données zones (CSV)"),
                     downloadButton("dl_serie","Série temporelle (CSV)")))
      ),
      # ---- À propos ----
      tabItem(
        tabName = "about",
        box(width = 12, title="À propos de ce tableau de bord", status="primary",
            solidHeader = TRUE,
            div(class="note-block",
              p(strong("PREIS Ebola RDC"), " — suivi de la 17e épidémie de MVE (souche Bundibugyo) en Ituri / Nord-Kivu / Sud-Kivu."),
              p("Les cumuls nationaux (cas, décès, CFR) proviennent des données INRB validées."),
              p("La localisation par zone de santé utilise les cas confirmés cumulés rapportés par l'INRB, positionnés au chef-lieu de chaque zone (coordonnées approximatives)."),
              p(strong("CFR provisoire :"), " pendant une épidémie active, certains cas récents peuvent encore évoluer ; ne pas interpréter comme létalité finale."),
              p("Carte de fond : Africa CDC RCC (54 pays). Mise à jour automatique via le pipeline PREIS.")
            ))
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  # Langue courante (réactive) + helper de traduction
  cur_lang <- reactive({
    l <- input$ui_lang
    if (is.null(l) || !(l %in% names(I18N))) "en" else l
  })
  i18n <- function(key) {
    l <- isolate(cur_lang())
    val <- I18N[[l]][[key]]
    if (is.null(val) || is.na(val)) I18N[["en"]][[key]] else val
  }
  # Version réactive (pour éléments qui doivent se retraduire en direct)
  tr <- function(key) {
    l <- cur_lang()
    val <- I18N[[l]][[key]]
    if (is.null(val) || is.na(val)) I18N[["en"]][[key]] else val
  }

  # Menu latéral dynamique (se retraduit au changement de langue)
  output$dynamic_menu <- renderMenu({
    sidebarMenu(
      id = "tabs", selected = "overview",
      menuItem(tr("overview"),  tabName = "overview", icon = icon("dashboard")),
      menuItem(tr("kpi"),       tabName = "kpi",      icon = icon("gauge-high")),
      menuItem(tr("daily"),     tabName = "daily",    icon = icon("chart-line")),
      menuItem(tr("cfr_tab"),   tabName = "cfr",      icon = icon("circle-dot")),
      menuItem(tr("faq"),       tabName = "faq",      icon = icon("circle-question")),
      menuItem(tr("synthesis"), tabName = "synthese", icon = icon("file-lines")),
      menuItem(tr("map"),       tabName = "map",      icon = icon("globe-africa")),
      menuItem(tr("zones"),     tabName = "zones",    icon = icon("table")),
      menuItem(tr("about"),     tabName = "about",    icon = icon("circle-info"))
    )
  })

  # Série filtrée jusqu'au SitRep choisi
  serie_f <- reactive({
    if (nrow(serie_all) == 0) return(serie_all)
    serie_all %>% filter(sitrep_no <= input$sitrep) %>% arrange(sitrep_no)
  })

  # Dernier point de la série filtrée
  last_row <- reactive({
    s <- serie_f(); if (nrow(s)==0) return(NULL); s %>% slice_tail(n=1)
  })

  # Zones filtrées (province + seuil)
  zones_f <- reactive({
    z <- zones_all
    if (nrow(z) == 0) return(z)
    if (!is.null(input$province) && input$province != "Toutes")
      z <- z %>% filter(province == input$province)
    z %>% filter(cases >= input$min_cases, cases > 0)
  })

  # ---- ValueBoxes ----
  output$vb_cas <- renderValueBox({
    lr <- last_row()
    valueBox(if(is.null(lr)) "—" else format(lr$cas_cumules, big.mark=" "),
             "Cas confirmés cumulés", icon=icon("virus"), color="red")
  })
  output$vb_deces <- renderValueBox({
    lr <- last_row()
    valueBox(if(is.null(lr)) "—" else format(lr$deces_cumules, big.mark=" "),
             "Décès cumulés", icon=icon("heart-crack"), color="black")
  })
  output$vb_cfr <- renderValueBox({
    lr <- last_row()
    valueBox(if(is.null(lr)||is.na(lr$cfr)) "—" else paste0(lr$cfr,"%"),
             "Létalité (CFR provisoire)", icon=icon("percent"), color="orange")
  })
  output$vb_zones <- renderValueBox({
    valueBox(nrow(zones_f()), "Zones de santé touchées",
             icon=icon("location-dot"), color="maroon")
  })

  # ---- Construction carte (fonction partagée) ----
  # Palette Africa CDC : vert (faible) -> or -> rouge (élevé)
  AU_RAMP <- c("#EAF3EC", "#F0B323", "#E31C23")

  # Valeur de l'indicateur cartographique choisi, par zone
  map_values <- reactive({
    zf <- zones_f()
    ind <- if (is.null(input$map_indic)) "cases" else input$map_indic
    # Récupère décès / CFR / nouveaux cas depuis les données zone si dispo
    zd <- if (exists("daily_all") && nrow(daily_all))
      daily_all %>% filter(level == "Zone") %>%
        group_by(zone) %>% arrange(date) %>% slice_tail(n = 1) %>% ungroup() else NULL
    zf$deaths   <- if (!is.null(zd)) zd$cum_deaths[match(zf$health_zone, zd$zone)] else NA
    zf$cfr      <- if (!is.null(zd)) zd$cfr[match(zf$health_zone, zd$zone)] else NA
    zf$new_cases<- if (!is.null(zd)) zd$new_cases[match(zf$health_zone, zd$zone)] else NA
    zf$val <- switch(ind,
      cases     = zf$cases,
      deaths    = zf$deaths,
      cfr       = zf$cfr,
      new_cases = zf$new_cases,
      zf$cases)
    zf$val[is.na(zf$val)] <- 0
    list(zf = zf, ind = ind)
  })

  indic_title <- function(ind) switch(ind,
    cases     = i18n("map_cases"),
    deaths    = i18n("map_deaths"),
    cfr       = i18n("map_cfr"),
    new_cases = i18n("map_newcases"),
    i18n("map_cases"))

  build_map <- function() {
    mv <- map_values(); zf <- mv$zf; ind <- mv$ind
    m <- leaflet(options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
      addProviderTiles("CartoDB.Positron")

    # 1) Couche Afrique : CONTOUR léger seulement (ne masque plus la choroplèthe)
    if (isTRUE(input$show_africa) && !is.null(africa_sf)) {
      af <- africa_sf
      is_drc <- toupper(af$iso3) == "COD"
      m <- m %>% addPolygons(
        data = af, fill = FALSE,
        color = ifelse(is_drc, "#00843E", "#BBBBBB"),
        weight = ifelse(is_drc, 1.8, 0.4), smoothFactor = 0.3,
        group = "Afrique/RCC")
    }

    # 2) Choroplèthe : zones de santé réelles colorées par l'indicateur
    if (isTRUE(input$show_choro) && !is.null(zones_geo)) {
      zg <- zones_geo
      zg$val <- zf$val[match(zg$zone, zf$health_zone)]
      zg$val[is.na(zg$val)] <- 0
      touched <- zg[zg$val > 0, ]
      m <- m %>% addPolygons(
        data = zg, fillColor = "#f4f4f4", fillOpacity = 0.45,
        color = "#cccccc", weight = 0.4, smoothFactor = 0.2,
        group = "Zones (choroplèthe)")
      if (nrow(touched) > 0) {
        palc <- colorNumeric(AU_RAMP, domain = touched$val)
        unit <- if (ind == "cfr") "%" else ""
        m <- m %>% addPolygons(
          data = touched, fillColor = ~palc(val), fillOpacity = 0.82,
          color = "#7B241C", weight = 1, smoothFactor = 0.2,
          popup = ~paste0("<b>", htmlEscape(zone), "</b><br/>",
                          i18n("province"), " : ", htmlEscape(province), "<br/><b>",
                          indic_title(ind), " : ", val, unit, "</b>"),
          label = ~paste0(zone, " (", val, unit, ")"),
          group = "Zones (choroplèthe)") %>%
          addLegend(position = "bottomleft", pal = palc, values = touched$val,
                    title = indic_title(ind), opacity = 0.9)
      }
    }

    # 3) Bulles proportionnelles par-dessus (intensité sans biais de surface)
    if (isTRUE(input$show_bubbles) && nrow(zf) > 0) {
      zb <- zf[zf$val > 0, ]
      if (nrow(zb) > 0) {
        palb <- colorNumeric(AU_RAMP, domain = zb$val)
        zb$radius <- pmax(5, sqrt(pmax(zb$val, 1)) * 2.4)
        unit <- if (ind == "cfr") "%" else ""
        m <- m %>% addCircleMarkers(
          data = zb, lng = ~lon, lat = ~lat, radius = ~radius,
          stroke = TRUE, weight = 1.3, color = "#7B241C",
          fillColor = ~palb(val), fillOpacity = 0.85,
          popup = ~paste0("<b>", htmlEscape(health_zone), "</b><br/>",
                          i18n("province"), " : ", htmlEscape(province), "<br/><b>",
                          indic_title(ind), " : ", val, unit, "</b>"),
          label = ~paste0(health_zone, " (", val, unit, ")"),
          group = "Bulles")
      }
    }

    # Cadrage
    if (isTRUE(input$zoom_rdc)) {
      m <- m %>% fitBounds(lng1 = 27.5, lat1 = -3.0, lng2 = 31.5, lat2 = 3.2)
    } else {
      m <- m %>% fitBounds(lng1 = -20, lat1 = -36, lng2 = 52, lat2 = 38)
    }
    m
  }

  output$map_overview <- renderLeaflet({ input$ui_lang; input$map_indic; build_map() })
  output$map_full     <- renderLeaflet({ input$ui_lang; input$map_indic; build_map() })

  # ---- Top zones (barres) ----
  output$plot_topzones <- renderPlotly({
    zf <- zones_f() %>% arrange(desc(cases)) %>% slice_head(n = 12)
    if (nrow(zf) == 0) return(plotly_empty())
    zf$health_zone <- factor(zf$health_zone, levels = rev(zf$health_zone))
    p <- ggplot(zf, aes(x = health_zone, y = cases, fill = cases,
                        text = paste0(health_zone, " : ", cases, " cas"))) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low = EBOLA_LIGHT, high = EBOLA_DARK, guide = "none") +
      labs(x = NULL, y = "Cas cumulés") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  # ---- Courbe épidémique ----
  output$plot_epi <- renderPlotly({
    s <- serie_f()
    if (nrow(s) == 0) return(plotly_empty())
    s <- s %>% mutate(
      date = as.Date(date),
      nv_cas_brut = if ("nouveaux_cas" %in% names(.)) nouveaux_cas
                    else cas_cumules - dplyr::lag(cas_cumules),
      # Incidence négative = révision/harmonisation des cumuls (pas un vrai
      # signal épidémio). On l'affiche à 0 pour ne pas induire en erreur.
      nv_cas = pmax(nv_cas_brut, 0),
      revision = !is.na(nv_cas_brut) & nv_cas_brut < 0
    )
    rev_txt <- if (any(s$revision, na.rm=TRUE))
      paste0("Note : ", sum(s$revision, na.rm=TRUE),
             " révision(s) à la baisse (harmonisation INRB) affichée(s) à 0.") else ""
    plot_ly(s) %>%
      add_bars(x = ~date, y = ~nv_cas, name = "Nouveaux cas",
               marker = list(color = "#E59866")) %>%
      add_lines(x = ~date, y = ~cas_cumules, name = "Cas cumulés",
                yaxis = "y2", line = list(color = EBOLA_DARK, width = 2)) %>%
      layout(
        yaxis  = list(title = "Nouveaux cas (incidence)", rangemode = "tozero"),
        yaxis2 = list(title = "Cumul", overlaying = "y", side = "right"),
        xaxis  = list(title = ""),
        legend = list(orientation = "h", y = -0.2),
        margin = list(r = 50),
        annotations = if (nzchar(rev_txt)) list(list(
          x = 0, y = 1.08, xref = "paper", yref = "paper",
          text = rev_txt, showarrow = FALSE,
          font = list(size = 10, color = "grey"))) else list()
      )
  })

  # ---- Interprétation ----
  output$interpretation <- renderUI({
    lr <- last_row()
    if (is.null(lr)) return(p("Aucune donnée."))
    s <- serie_f()
    nd <- if ("nouveaux_deces" %in% names(s)) tail(s$nouveaux_deces,1) else NA
    msgs <- list()
    if (!is.na(lr$cfr) && lr$cfr >= 15)
      msgs <- c(msgs, paste0("CFR élevé (", lr$cfr, "%) — vérifier délais de prise en charge et décès communautaires."))
    if (!is.na(nd) && nd > 0)
      msgs <- c(msgs, paste0(nd, " nouveau(x) décès depuis le SitRep précédent — investiguer chaque décès."))
    msgs <- c(msgs, "Épicentre en Ituri (Bunia, Rwampara, Mongbwalu).")
    tagList(
      p(strong(paste0("SitRep N°", lr$sitrep_no, " — ", lr$date))),
      tags$ul(lapply(msgs, tags$li)),
      tags$hr(),
      p(em("CFR provisoire ; cumuls nationaux = INRB validé ; localisation par zone = à valider."))
    )
  })

  # ---- Bouton "Voir le SitRep" (PDF local si dispo, sinon lien INSP) ----
  output$sitrep_button <- renderUI({
    lr <- last_row(); if (is.null(lr)) return(NULL)
    sno <- lr$sitrep_no
    # URL INSP construite à partir du n° et de la date
    d <- as.Date(lr$date)
    insp_url <- sprintf("https://insp.cd/sitrep-n%d-mvb_%02d-%02d-%d/",
                        sno, as.integer(format(d,"%d")),
                        as.integer(format(d,"%m")), as.integer(format(d,"%Y")))
    tags$p(
      tags$a(href = insp_url, target = "_blank",
             class = "btn btn-danger btn-sm",
             icon("file-pdf"), paste0(" Voir le SitRep N°", sno, " (INSP)"))
    )
  })

  # ============================================================
  # KPI — calculs (chacun lié à une action)
  # ============================================================
  kpi <- reactive({
    s <- serie_f()
    if (nrow(s) < 2) return(NULL)
    s <- s %>% arrange(sitrep_no) %>%
      mutate(nv = pmax(cas_cumules - dplyr::lag(cas_cumules), 0))

    n <- nrow(s)
    last <- s[n, ]; prev <- s[n-1, ]

    # Croissance : variation des nouveaux cas entre 2 derniers SitReps
    nv_last <- last$nv; nv_prev <- if (n>=2) s$nv[n-1] else NA
    growth <- if (!is.na(nv_prev) && nv_prev > 0)
      round(100*(nv_last - nv_prev)/nv_prev, 0) else NA

    # Incidence récente : somme des nouveaux cas sur les 7 derniers SitReps
    k <- min(7, n)
    inc7 <- sum(tail(s$nv, k), na.rm = TRUE)
    inc7_prev <- if (n >= 2*k) sum(s$nv[(n-2*k+1):(n-k)], na.rm = TRUE) else NA

    # Tendance sur 3 derniers points (pente du lissage)
    last3 <- tail(s$nv, 3)
    trend <- if (length(last3) == 3) {
      if (last3[3] > last3[1]) "Hausse" else if (last3[3] < last3[1]) "Baisse" else "Stable"
    } else "n/d"

    # Délai de doublement (si croissance) sur les cumuls
    c_last <- last$cas_cumules; c_prev <- prev$cas_cumules
    dbl <- if (!is.na(c_prev) && c_prev > 0 && c_last > c_prev) {
      r <- log(c_last/c_prev)  # par intervalle SitRep (~1j ici)
      round(log(2)/r, 1)
    } else NA

    # CFR
    cfr <- last$cfr

    list(growth=growth, inc7=inc7, inc7_prev=inc7_prev, trend=trend,
         dbl=dbl, cfr=cfr, nv_last=nv_last)
  })

  # KPI géographiques
  kpi_geo <- reactive({
    z <- zones_f()
    if (nrow(z) == 0) return(list(active=0, total=nrow(zones_all), conc=NA))
    total_cases <- sum(z$cases, na.rm = TRUE)
    top3 <- sum(head(sort(z$cases, decreasing=TRUE), 3), na.rm = TRUE)
    conc <- if (total_cases > 0) round(100*top3/total_cases, 0) else NA
    list(active = nrow(z), total = nrow(zones_all), conc = conc)
  })

  # Complétude : % SitReps avec détail par zone (proxy de fiabilité)
  kpi_quality <- reactive({
    s <- serie_f(); if (nrow(s)==0) return(NA)
    # proxy : SitReps avec CFR non manquant / total
    round(100 * sum(!is.na(s$cfr)) / nrow(s), 0)
  })

  vb <- function(val, sub, icon_name, color) {
    valueBox(val, sub, icon = icon(icon_name), color = color)
  }

  output$kpi_growth <- renderValueBox({
    k <- kpi(); g <- if(is.null(k)||is.na(k$growth)) "—" else paste0(k$growth, "%")
    col <- if(!is.null(k) && !is.na(k$growth) && k$growth > 0) "red" else "green"
    vb(g, "Croissance nouveaux cas (vs préc.)", "arrow-trend-up", col)
  })
  output$kpi_inc7 <- renderValueBox({
    k <- kpi(); v <- if(is.null(k)) "—" else k$inc7
    vb(v, "Cas sur 7 derniers SitReps", "calendar-week", "orange")
  })
  output$kpi_trend <- renderValueBox({
    k <- kpi(); t <- if(is.null(k)) "—" else k$trend
    col <- switch(if(is.null(k)) "n/d" else k$trend,
                  "Hausse"="red","Baisse"="green","Stable"="yellow","navy")
    vb(t, "Tendance (3 derniers SitReps)", "chart-line", col)
  })
  output$kpi_double <- renderValueBox({
    k <- kpi(); d <- if(is.null(k)||is.na(k$dbl)) "—" else paste0(k$dbl, " j")
    vb(d, "Délai de doublement (cas)", "clock", "maroon")
  })
  output$kpi_active_zones <- renderValueBox({
    g <- kpi_geo(); vb(paste0(g$active, "/", g$total),
                       "Zones touchées / total", "location-dot", "red")
  })
  output$kpi_concentration <- renderValueBox({
    g <- kpi_geo(); v <- if(is.na(g$conc)) "—" else paste0(g$conc, "%")
    vb(v, "Concentration top-3 zones", "bullseye", "orange")
  })
  output$kpi_cfr <- renderValueBox({
    k <- kpi(); v <- if(is.null(k)||is.na(k$cfr)) "—" else paste0(k$cfr, "%")
    vb(v, "CFR provisoire", "percent", "black")
  })
  output$kpi_completeness <- renderValueBox({
    q <- kpi_quality(); v <- if(is.na(q)) "—" else paste0(q, "%")
    vb(v, "Complétude données (CFR dispo)", "clipboard-check", "navy")
  })

  # Lecture opérationnelle des KPI -> actions
  output$kpi_actions <- renderUI({
    k <- kpi(); g <- kpi_geo()
    if (is.null(k)) return(p("Données insuffisantes."))
    acts <- list()
    if (!is.na(k$growth) && k$growth > 0)
      acts <- c(acts, paste0("Cas en hausse (+", k$growth, "%) → renforcer immédiatement",
                             " la recherche active de cas et l'isolement dans les zones actives."))
    if (!is.na(k$growth) && k$growth <= 0)
      acts <- c(acts, "Cas stables ou en baisse → maintenir la pression, ne pas relâcher le traçage.")
    if (!is.na(k$dbl) && k$dbl < 7)
      acts <- c(acts, paste0("Doublement rapide (", k$dbl, " j) → escalade : équipes supplémentaires,",
                             " capacité de lits, vaccination en anneau si disponible."))
    if (!is.na(g$conc) && g$conc >= 70)
      acts <- c(acts, paste0("Forte concentration (", g$conc, "% dans 3 zones) → concentrer les",
                             " ressources sur l'épicentre (Bunia, Rwampara, Mongbwalu)."))
    if (!is.na(k$cfr) && k$cfr >= 15)
      acts <- c(acts, paste0("CFR élevé (", k$cfr, "%) → réduire le délai détection→soins,",
                             " sécuriser les enterrements, alerte communautaire précoce."))
    acts <- c(acts, "Investiguer chaque décès communautaire : indicateur de transmission cachée.")
    tagList(
      tags$ul(lapply(acts, tags$li)),
      tags$hr(),
      p(em("Indicateurs descriptifs et de tendance. Rt et projections non calculés faute de",
           " dates de début de symptômes (voir encadré). Drivers probables — pas de causalité établie."))
    )
  })

  # Évolution d'un indicateur choisi, du SitRep 1 au SitRep filtré
  output$plot_kpi_evol <- renderPlotly({
    if (nrow(long_all) == 0 || is.null(input$kpi_indic))
      return(plotly_empty() %>% layout(title = "Base d'indicateurs non disponible"))
    code <- input$kpi_indic
    df <- long_all %>%
      filter(indicator_code == code, sitrep_no <= input$sitrep) %>%
      left_join(sno_date, by = "sitrep_no") %>%
      arrange(sitrep_no) %>%
      mutate(date = as.Date(date))
    if (nrow(df) == 0)
      return(plotly_empty() %>% layout(title = "Aucune donnée pour cet indicateur"))
    lbl <- INDIC_LABELS[[code]]
    p <- plot_ly(df, x = ~date, y = ~value, type = "scatter",
                 mode = if (isTRUE(input$kpi_show_points)) "lines+markers" else "lines",
                 line = list(color = EBOLA_RED, width = 2),
                 marker = list(color = EBOLA_DARK, size = 6),
                 hovertemplate = paste0("SitRep %{customdata}<br>", lbl, " : %{y}<extra></extra>"),
                 customdata = ~sitrep_no) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = lbl),
             title = list(text = lbl, font = list(size = 13)))
    p
  })

  # ============================================================
  # Synthèse narrative (3 niveaux) — réactive au curseur SitRep
  # ============================================================
  output$synth_national <- renderUI({
    if (!HAS_NARR) return(tagList(h4("Synthèse nationale"),
        p(em("Module de synthèse non disponible (05_synthese_narrative.R absent)."))))
    txt <- synthese_nationale(serie_f())
    tagList(h4("Niveau national"),
            HTML(gsub("\n", "<br/>", txt)))
  })
  output$synth_zones <- renderUI({
    if (!HAS_NARR) return(NULL)
    txt <- synthese_zones(zones_f())
    tagList(h4("Niveau zone de santé"),
            HTML(gsub("\n", "<br/>", txt)))
  })
  output$synth_strategique <- renderUI({
    if (!HAS_NARR) return(NULL)
    txt <- synthese_strategique(serie_f(), zones_f())
    tagList(h4("Niveau Africa CDC / partenaires"),
            HTML(gsub("\n", "<br/>", txt)))
  })
  output$dl_synthese <- downloadHandler(
    filename = function() paste0("synthese_ebola_sitrep", input$sitrep, ".txt"),
    content = function(f) {
      if (!HAS_NARR) { writeLines("Module de synthèse non disponible.", f); return() }
      writeLines(synthese_complete(serie_f(), zones_f(), html = FALSE), f)
    }
  )

  # ============================================================
  # Suivi journalier (national + province)
  # ============================================================
  daily_f <- reactive({
    if (nrow(daily_all) == 0) return(daily_all)
    if (input$daily_level == "National") {
      daily_all %>% filter(level == "National")
    } else if (input$daily_level == "Province") {
      pr <- if (is.null(input$daily_prov)) "Ituri" else input$daily_prov
      daily_all %>% filter(level == "Province", province == pr)
    } else {
      zn <- input$daily_zone
      if (is.null(zn) || zn == "") return(daily_all[0, ])
      daily_all %>% filter(level == "Zone", zone == zn)
    }
  })

  # Peupler la liste des zones disponibles (celles retenues par le module)
  observe({
    zones_av <- if (nrow(daily_all))
      sort(unique(daily_all$zone[daily_all$level == "Zone"])) else character(0)
    updateSelectInput(session, "daily_zone", choices = zones_av,
                      selected = if (length(zones_av)) zones_av[1] else NULL)
  })

  d_last <- reactive({
    df <- daily_f()
    if (nrow(df) == 0) return(NULL)
    df %>% arrange(date) %>% tail(1)
  })

  output$d_cum_cases <- renderValueBox({
    l <- d_last(); v <- if (is.null(l)) "—" else format(l$cum_cases, big.mark=" ")
    valueBox(v, "Cas confirmés (cumul)", icon = icon("virus"), color = "red")
  })
  output$d_cum_deaths <- renderValueBox({
    l <- d_last(); v <- if (is.null(l)) "—" else format(l$cum_deaths, big.mark=" ")
    valueBox(v, "Décès confirmés (cumul)", icon = icon("skull"), color = "black")
  })
  output$d_cfr <- renderValueBox({
    l <- d_last(); v <- if (is.null(l) || is.na(l$cfr)) "—" else paste0(l$cfr, "%")
    valueBox(v, "Létalité CFR (provisoire)", icon = icon("percent"), color = "orange")
  })
  output$d_ma7 <- renderValueBox({
    l <- d_last(); v <- if (is.null(l) || is.na(l$ma7_new_cases)) "—" else l$ma7_new_cases
    valueBox(v, "Moy. mobile 7j (cas/j)", icon = icon("wave-square"), color = "blue")
  })

  output$daily_plot <- renderPlotly({
    df <- daily_f(); m <- input$daily_metric
    if (nrow(df) == 0) return(plotly_empty())
    lbl <- c(cum_cases="Cas confirmés cumulés", cum_deaths="Décès confirmés cumulés",
             new_cases="Nouveaux cas / jour", new_deaths="Nouveaux décès / jour",
             cfr="Létalité CFR (%) — provisoire", ma7_new_cases="Moyenne mobile 7j (cas)")[m]
    df$y <- df[[m]]
    p <- plot_ly(df, x = ~date, y = ~y, type = "scatter",
                 mode = if (m %in% c("new_cases","new_deaths")) "lines+markers" else "lines",
                 line = list(color = "#C44E1E", width = 2),
                 marker = list(color = "#C44E1E", size = 5),
                 hovertemplate = paste0("%{x}<br>", lbl, ": %{y}<extra></extra>")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = lbl, rangemode = "tozero"),
             margin = list(t = 10))
    p
  })

  # ---- Courbe épidémique : incidence (barres) + cumul OU moyenne mobile ----
  output$epi_curve <- renderPlotly({
    df <- daily_f() %>% arrange(date)
    if (nrow(df) < 2) return(plotly_empty())
    AU_GREEN <- "#00843E"; AU_RED <- "#E31C23"
    # Incidence en barres (mettre les valeurs négatives = révisions à 0 pour l'affichage)
    inc <- pmax(df$new_cases, 0)
    p <- plot_ly(df, x = ~date)
    p <- p %>% add_bars(y = inc, name = "Nouveaux cas/jour",
                        marker = list(color = AU_GREEN),
                        hovertemplate = "%{x}<br>Nouveaux cas: %{y}<extra></extra>")
    if (input$epi_mode == "inc_cum") {
      p <- p %>% add_lines(y = ~cum_cases, name = "Cumul cas", yaxis = "y2",
                           line = list(color = AU_RED, width = 2),
                           hovertemplate = "%{x}<br>Cumul: %{y}<extra></extra>")
      y2 <- list(title = "Cumul cas", overlaying = "y", side = "right",
                 rangemode = "tozero", showgrid = FALSE)
    } else {
      p <- p %>% add_lines(y = ~ma7_new_cases, name = "Moyenne mobile 7j", yaxis = "y2",
                           line = list(color = AU_RED, width = 2),
                           hovertemplate = "%{x}<br>Moy 7j: %{y}<extra></extra>")
      y2 <- list(title = "Moyenne mobile 7j", overlaying = "y", side = "right",
                 rangemode = "tozero", showgrid = FALSE)
    }
    p %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Nouveaux cas / jour", rangemode = "tozero"),
      yaxis2 = y2,
      legend = list(orientation = "h", x = 0, y = 1.12),
      margin = list(t = 30, r = 60))
  })

  output$epi_note <- renderUI({
    df <- daily_f()
    if (nrow(df) < 2)
      return(tags$p(style = "color:#c0392b;",
        "Données insuffisantes pour une courbe épidémique à ce niveau."))
    nrev <- sum(df$revision, na.rm = TRUE)
    msg <- "Barres = incidence quotidienne (nouveaux cas confirmés). Ligne = cumul (ou moyenne mobile 7j)."
    if (nrev > 0) msg <- paste0(msg,
      " Note : ", nrev, " jour(s) de révision INRA à la baisse mis à 0 dans les barres (cumul non affecté).")
    tags$p(style = "color:#888; font-size:12px; margin-top:6px;", msg)
  })

  # ---- Analyse CFR : nuage létalité vs cas par zone (couleurs Africa CDC) ----
  output$cfr_scatter <- renderPlotly({
    if (nrow(daily_all) == 0) return(plotly_empty())
    # Dernier point par zone
    zlast <- daily_all %>% filter(level == "Zone") %>%
      group_by(zone, province) %>% arrange(date) %>% slice_tail(n = 1) %>% ungroup() %>%
      filter(cum_cases > 0)
    if (nrow(zlast) == 0) return(plotly_empty())
    natl <- daily_all %>% filter(level == "National") %>% arrange(date) %>% tail(1)
    cfr_nat <- if (nrow(natl)) natl$cfr else NA
    pal <- c("Ituri" = "#E31C23", "Nord-Kivu" = "#00843E", "Sud-Kivu" = "#F0B323")
    zlast$col <- pal[zlast$province]
    p <- plot_ly(zlast, x = ~cum_cases, y = ~cfr,
                 type = "scatter", mode = "markers+text",
                 text = ~ifelse(cum_cases >= 20 | (cfr >= 60 & cum_cases >= 10), zone, ""),
                 textposition = "top center", textfont = list(size = 11),
                 cliponaxis = FALSE,
                 marker = list(size = ~pmin(8 + cum_cases/3, 45),
                               color = ~col, opacity = 0.82,
                               line = list(color = "white", width = 1.2)),
                 hovertext = ~paste0(zone, "<br>Cas: ", cum_cases, "<br>CFR: ", cfr, "%"),
                 hoverinfo = "text") %>%
      layout(
        xaxis = list(title = "Cas confirmés cumulés (échelle log)", type = "log"),
        yaxis = list(title = "Létalité provisoire (%)", rangemode = "tozero"),
        shapes = if (!is.na(cfr_nat)) list(list(type = "line",
          x0 = min(zlast$cum_cases), x1 = max(zlast$cum_cases),
          y0 = cfr_nat, y1 = cfr_nat, xref = "x", yref = "y",
          line = list(dash = "dash", color = "grey", width = 1))) else list(),
        annotations = if (!is.na(cfr_nat)) list(list(
          x = log10(max(zlast$cum_cases)), y = cfr_nat + 4,
          text = paste0("Létalité nationale ", cfr_nat, "%"),
          showarrow = FALSE, font = list(size = 10, color = "grey"))) else list(),
        margin = list(t = 20))
    p
  })

  # ============================================================
  # Questions fréquentes — réponses (texte validé + chiffres en direct)
  # ============================================================
  # Chiffres clés recalculés depuis les données actuelles
  faq_facts <- reactive({
    natl <- if (nrow(daily_all))
      daily_all %>% filter(level == "National") %>% arrange(date) %>% tail(1) else NULL
    zlast <- if (nrow(daily_all))
      daily_all %>% filter(level == "Zone") %>%
        group_by(zone, province) %>% arrange(date) %>% slice_tail(n = 1) %>% ungroup() else NULL
    top_cases <- if (!is.null(zlast)) zlast %>% arrange(desc(cum_cases)) %>% head(3) else NULL
    top_cfr   <- if (!is.null(zlast)) zlast %>% filter(cum_cases >= 10) %>%
      arrange(desc(cfr)) %>% head(3) else NULL

    # Indicateurs thématiques (lus à la volée si présents)
    read_ind <- function(name, col) {
      fp <- find_first(c(file.path(DATA_DIR, "final", name),
                         file.path(ROOT_DIR, "data", "final", name)))
      if (is.na(fp)) return(NULL)
      d <- tryCatch(read_csv(fp, show_col_types = FALSE), error = function(e) NULL)
      if (is.null(d) || !col %in% names(d)) return(NULL)
      d %>% mutate(v = suppressWarnings(as.numeric(.data[[col]]))) %>% filter(!is.na(v))
    }
    list(natl = natl, zlast = zlast, top_cases = top_cases, top_cfr = top_cfr)
  })

  output$faq_answer <- renderUI({
    f <- faq_facts(); q <- input$faq_q
    natl <- f$natl
    fmt <- function(x) if (is.null(x) || length(x)==0 || is.na(x)) "—" else format(x, big.mark=" ")
    date_str <- if (!is.null(natl)) format(natl$date, "%d %b %Y") else "—"
    style_p <- "font-size:15px; line-height:1.6;"

    txt <- switch(q,
      q_top_zones = {
        tz <- f$top_cases
        zlist <- if (!is.null(tz)) paste(sprintf("%s (%d cases)", tz$zone, tz$cum_cases),
                                         collapse = ", ") else "—"
        paste0("<p style='", style_p, "'>As of ", date_str, ", the most affected health zones ",
          "(cumulative confirmed cases) are: <b>", zlist, "</b>. ",
          "The epicentre is in Ituri. See the chart below for the full ranking.</p>")
      },
      q_national_trend = paste0("<p style='", style_p, "'>As of ", date_str,
        ", the national cumulative total is <b>", fmt(natl$cum_cases), " confirmed cases</b> and <b>",
        fmt(natl$cum_deaths), " deaths</b> (provisional CFR <b>", fmt(natl$cfr), "%</b>). ",
        "The 7-day moving average of new cases is <b>", fmt(natl$ma7_new_cases),
        "/day</b>. The epidemic curve below shows the trajectory since 14 May.</p>"),
      q_mortality = {
        tc <- f$top_cfr
        clist <- if (!is.null(tc)) paste(sprintf("%s (%.1f%% on %d cases)",
          tc$zone, tc$cfr, tc$cum_cases), collapse = ", ") else "—"
        paste0("<p style='", style_p, "'>The high-lethality foci (among zones with ",
          "at least 10 cases, for statistical reliability) are: <b>", clist, "</b>. ",
          "A high CFR may reflect late detection or late care. ",
          "See the CFR-versus-cases scatter below.</p>")
      },
      q_province = paste0("<p style='", style_p, "'>The situation by province is detailed ",
        "in the chart below. Ituri concentrates the large majority of cases, ",
        "followed by North Kivu (emerging front), with South Kivu remaining marginal. ",
        "Data as of ", date_str, ".</p>"),
      q_sexratio = paste0("<p style='", style_p, "'><b>No, not with the current data.</b> ",
        "INSP/INRB SitReps provide <b>aggregated</b> data (cases and deaths by zone and date), ",
        "without a 'sex' variable at the individual case level. Computing a sex ratio ",
        "— and its evolution over time — requires an <b>individual line list</b> ",
        "(one case = one row, with sex, age, date, zone). ",
        "Note: a sex-ratio imbalance would be a <i>signal to investigate</i>, not proof of ",
        "sexual transmission (occupational mining exposure, caregiving, or funeral rites are ",
        "alternative explanations). The analysis will be possible once the line list is obtained.</p>"),
      q_projection = paste0("<p style='", style_p, "'><b>Not reliably at this stage.</b> ",
        "Projection models (R0, Rt, SEIR) require individual data with <b>symptom-onset dates</b>, ",
        "absent from the aggregated SitReps. Producing a projection from cumulative data alone ",
        "would give a falsely precise result. ",
        "Tracking the 7-day moving average (Daily tracking tab) remains the most robust ",
        "trend indicator available.</p>"),
      q_cfr_unstable = paste0("<p style='", style_p, "'>A CFR of 100% appears in zones with ",
        "<b>very few cases</b> (sometimes 1 or 2). On such small numbers, the ratio is ",
        "<b>statistically unstable</b>: a single death is enough to show 100%. ",
        "These values should not be over-interpreted. The scatter in the CFR analysis tab ",
        "illustrates this: the reliable zones are those with enough cases.</p>"),
      q_poe = paste0("<p style='", style_p, "'>Screening at points of entry (PoE) ",
        "is part of population-protection measures: temperature checks and hand-washing at ",
        "borders and transit routes. Screening data exist for several zones but remain partial ",
        "(recorded on some days only). The chart shows screened volumes where available. ",
        "The best individual protection remains early alert when symptoms appear and ",
        "adherence to hygiene measures.</p>"),
      q_suspects = paste0("<p style='", style_p, "'>Beyond confirmed cases, <b>suspected cases</b> ",
        "are continuously investigated. Tracking suspects reflects the active detection effort. ",
        "The chart shows the evolution of cumulative suspected cases nationally. ",
        "Note: not every suspect is a case — many are ruled out after investigation/laboratory. ",
        "Per-zone detail and laboratory confirmation delays are not available in the current ",
        "aggregated data.</p>"),
      q_contacts = paste0("<p style='", style_p, "'><b>Contact tracing</b> is a pillar of the ",
        "response: each case generates a list of contacts to monitor for 21 days. ",
        "Data on contacts traced and isolated are available for several zones (over a limited ",
        "period). The chart shows contacts traced by zone. ",
        "High, complete follow-up is a good sign; zones without contact data may signal ",
        "surveillance to strengthen — interpret with caution given the partial coverage.</p>"),
      q_hosp = paste0("<p style='", style_p, "'>The <b>hospital burden</b> (patients in Ebola ",
        "Treatment Centres, ETC) indicates pressure on the care system. Hospitalisation data ",
        "exist for a few zones and a few days. The chart shows hospitalised numbers where ",
        "recorded. To be read alongside ETC capacity (the SitRep flags potential saturation and ",
        "the opening of new ETCs in Bunia, Rwampara, Beni).</p>"),
      q_crossborder = paste0("<p style='", style_p, "'>The cross-border risk is real: the affected ",
        "North Kivu zones (Beni, Butembo, Katwa) and Ituri are close to the borders of ",
        "<b>Uganda</b> and <b>Rwanda</b>. The map (Map tab) shows the geographic proximity. ",
        "For the RCC (Eastern/Central) and Africa CDC, this justifies strengthening surveillance ",
        "at points of entry and regional coordination. Important: this dashboard covers DRC data; ",
        "it contains no data on possible cases in neighbouring countries.</p>"),
      q_needs = paste0("<p style='", style_p, "'>Priority needs are described in the SitRep: ",
        "standardised ETC capacity (saturation risk), financial resources (a significant gap is ",
        "reported by the response), and strengthening of the pillars (surveillance, case management, ",
        "IPC, community engagement). For partners and Africa CDC, support for ETC construction and ",
        "response financing is central. Precise amounts are in the official SitRep and change with ",
        "each bulletin.</p>"),
      q_datalimits = paste0("<p style='", style_p, "'>Data transparency: (1) the data are ",
        "<b>aggregated</b> (cumulative by zone and date), without an individual line list — so no sex, ",
        "age, or symptom-onset date; (2) the <b>CFR is provisional</b> (recent cases still evolving); ",
        "(3) INRB makes <b>downward revisions</b> on some days (reclassification); ",
        "(4) some indicators (contacts, hospitalisations, points of entry) are only partially ",
        "recorded; (5) no detailed <b>laboratory</b> data (confirmation delays, tests performed) ",
        "is available. These limits condition what the dashboard can and cannot assert.</p>"),
      paste0("<p style='", style_p, "'>Select a question.</p>")
    )
    HTML(txt)
  })

  # Graphique contextuel selon la question
  output$faq_plot_ui <- renderUI({
    q <- input$faq_q
    no_plot <- c("q_sexratio", "q_projection", "q_needs", "q_datalimits", "q_crossborder")
    if (q %in% no_plot) {
      msg <- if (q == "q_crossborder")
        "See the Map tab for the geographic proximity of borders."
      else if (q %in% c("q_needs"))
        "Needs and amounts are in the official SitRep (text above)."
      else
        "No chart: the requested analysis is not feasible with the available data."
      tags$p(style = "color:#888; font-style:italic;", msg)
    } else {
      plotlyOutput("faq_plot", height = 360)
    }
  })

  output$faq_plot <- renderPlotly({
    f <- faq_facts(); q <- input$faq_q
    AU_GREEN <- "#00843E"; AU_RED <- "#E31C23"; AU_GOLD <- "#F0B323"
    pal <- c("Ituri" = AU_RED, "Nord-Kivu" = AU_GREEN, "Sud-Kivu" = AU_GOLD)
    if (q == "q_national_trend") {
      df <- daily_all %>% filter(level == "National") %>% arrange(date)
      plot_ly(df, x = ~date) %>%
        add_bars(y = ~pmax(new_cases,0), name = "Nouveaux cas/j", marker = list(color = AU_GREEN)) %>%
        add_lines(y = ~cum_cases, name = "Cumul", yaxis = "y2", line = list(color = AU_RED, width = 2)) %>%
        layout(yaxis = list(title = "Nouveaux cas/j"),
               yaxis2 = list(title = "Cumul", overlaying = "y", side = "right", showgrid = FALSE),
               legend = list(orientation = "h"), margin = list(t = 20))
    } else if (q == "q_mortality" || q == "q_cfr_unstable") {
      zl <- f$zlast %>% filter(cum_cases > 0)
      zl$col <- pal[zl$province]
      plot_ly(zl, x = ~cum_cases, y = ~cfr, type = "scatter", mode = "markers+text",
              text = ~ifelse(cum_cases >= 20 | (cfr >= 60 & cum_cases >= 10), zone, ""),
              textposition = "top center",
              marker = list(size = ~pmin(8 + cum_cases/3, 40), color = ~col, opacity = 0.82,
                            line = list(color = "white", width = 1)),
              hovertext = ~paste0(zone, ": ", cfr, "% (", cum_cases, " cas)"),
              hoverinfo = "text") %>%
        layout(xaxis = list(title = "Cas cumulés (log)", type = "log"),
               yaxis = list(title = "Létalité provisoire (%)"), margin = list(t = 20))
    } else if (q == "q_province") {
      pv <- daily_all %>% filter(level == "Province") %>%
        group_by(province) %>% arrange(date) %>% slice_tail(n = 1) %>% ungroup()
      plot_ly(pv, x = ~province, y = ~cum_cases, type = "bar",
              marker = list(color = unname(pal[pv$province])),
              name = "Cas") %>%
        layout(yaxis = list(title = "Cas confirmés cumulés"), xaxis = list(title = ""),
               margin = list(t = 20))
    } else if (q == "q_suspects") {
      fp <- find_first(c(file.path(DATA_DIR, "final", "PREIS_daily_indicators.csv")))
      # suspects = série nationale séparée : lecture directe du fichier INRB stagé si présent
      sp <- find_first(c(file.path(DATA_DIR, "insp_sitrep__national_cumulative_suspected_cases__daily.csv"),
                         file.path(ROOT_DIR, "data", "raw", "insp_sitrep__national_cumulative_suspected_cases__daily.csv")))
      if (is.na(sp)) return(plotly_empty() %>% layout(title = "Suspected-case data not staged"))
      d <- tryCatch(read_csv(sp, show_col_types = FALSE), error = function(e) NULL)
      if (is.null(d)) return(plotly_empty())
      names(d)[grepl("suspected", names(d))][1] -> col
      d$v <- suppressWarnings(as.numeric(d[[col]])); d <- d[!is.na(d$v), ]
      d$date <- as.Date(d$date)
      plot_ly(d, x = ~date, y = ~v, type = "scatter", mode = "lines+markers",
              line = list(color = AU_GOLD, width = 2), name = "Cas suspects") %>%
        layout(yaxis = list(title = "Cas suspects cumulés"), xaxis = list(title = ""),
               margin = list(t = 20))
    } else if (q %in% c("q_contacts", "q_hosp", "q_poe")) {
      info <- list(
        q_contacts = c("insp_sitrep__cumulative_contacts_traced__daily.csv",
                       "cumulative_contacts_traced", "Contacts tracés (cumul)"),
        q_hosp     = c("insp_sitrep__hospitalised__daily.csv",
                       "hospitalised", "Patients hospitalisés"),
        q_poe      = c("insp_sitrep__total_poe_screened__daily.csv",
                       "total_poe_screened", "Personnes dépistées (PoE)"))[[q]]
      fp <- find_first(c(file.path(DATA_DIR, info[1]),
                         file.path(ROOT_DIR, "data", "raw", info[1])))
      if (is.na(fp)) return(plotly_empty() %>%
        layout(title = list(text = "Data not staged in the dashboard", font = list(size = 13))))
      d <- tryCatch(read_csv(fp, show_col_types = FALSE), error = function(e) NULL)
      if (is.null(d) || !info[2] %in% names(d)) return(plotly_empty())
      d$v <- suppressWarnings(as.numeric(d[[info[2]]])); d <- d[!is.na(d$v), ]
      # dernier état par zone
      agg <- d %>% group_by(nom) %>% summarise(v = max(v), .groups = "drop") %>%
        filter(v > 0) %>% arrange(desc(v)) %>% head(12)
      plot_ly(agg, x = ~v, y = ~reorder(nom, v), type = "bar", orientation = "h",
              marker = list(color = AU_GREEN),
              hovertext = ~paste0(nom, ": ", v), hoverinfo = "text") %>%
        layout(xaxis = list(title = info[3]), yaxis = list(title = ""),
               margin = list(t = 20, l = 100))
    } else { # q_top_zones
      tz <- f$zlast %>% arrange(desc(cum_cases)) %>% head(10)
      tz$col <- pal[tz$province]
      plot_ly(tz, x = ~cum_cases, y = ~reorder(zone, cum_cases), type = "bar",
              orientation = "h", marker = list(color = ~col),
              hovertext = ~paste0(zone, ": ", cum_cases, " cas"), hoverinfo = "text") %>%
        layout(xaxis = list(title = "Cas confirmés cumulés"), yaxis = list(title = ""),
               margin = list(t = 20, l = 90))
    }
  })

  output$daily_table <- renderDT({
    df <- daily_f() %>% arrange(desc(date)) %>%
      select(Date = date, `Cas cumul` = cum_cases, `Décès cumul` = cum_deaths,
             `Nouv. cas` = new_cases, `Nouv. décès` = new_deaths,
             `CFR %` = cfr, `Moy7j cas` = ma7_new_cases)
    datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "tip"))
  })

  output$dl_daily <- downloadHandler(
    filename = function() paste0("suivi_journalier_",
                                 tolower(input$daily_level), ".csv"),
    content = function(f) readr::write_excel_csv(daily_f(), f)
  )

  # ---- Tableau zones ----
  output$tbl_zones <- renderDT({
    zf <- zones_f() %>% arrange(desc(cases)) %>%
      select(`Zone de santé`=health_zone, Province=province, `Cas cumulés`=cases)
    datatable(zf, options = list(pageLength = 15), rownames = FALSE)
  })

  # ---- Téléchargements ----
  output$dl_zones <- downloadHandler(
    filename = function() paste0("zones_ebola_sitrep", input$sitrep, ".csv"),
    content = function(f) write_csv(zones_f(), f)
  )
  output$dl_serie <- downloadHandler(
    filename = function() "serie_temporelle_ebola.csv",
    content = function(f) write_csv(serie_f(), f)
  )
}

shinyApp(ui, server)
