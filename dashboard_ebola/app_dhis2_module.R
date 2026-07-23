## ============================================================
## PREIS Ebola — Module DHIS2 pour app.R (v3 — propre)
## Définit : D2_OK, ui_dhis2_tab, server_dhis2()
## ============================================================

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard)
  library(dplyr); library(readr); library(plotly); library(DT)
  library(lubridate); library(stringr)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

## ── Chemin source_line_list (robuste) ────────────────────────
.LL <- (function() {
  p1 <- normalizePath("../source_line_list", winslash = "/", mustWork = FALSE)
  if (dir.exists(p1)) return(p1)
  p2 <- normalizePath("source_line_list", winslash = "/", mustWork = FALSE)
  if (dir.exists(p2)) return(p2)
  p3 <- file.path(normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                  "source_line_list")
  if (dir.exists(p3)) return(p3)
  p1
})()

## ── Chargement silencieux ────────────────────────────────────
.load <- function(fn) {
  fp <- file.path(.LL, fn)
  if (!file.exists(fp)) return(tibble())
  tryCatch(read_csv(fp, show_col_types = FALSE, guess_max = 2000),
           error = function(e) tibble())
}

# ===== PREIS DHIS2 online patch: router .load via le resolveur =====
if (exists('preis_resolve_data') && !isTRUE(.PREIS_LOAD_WRAPPED)) {
  .load_orig <- .load
  .load <- function(fn) {
    p <- tryCatch(preis_resolve_data(fn, 'aggregate'), error=function(e) NA_character_)
    if (!is.na(p) && file.exists(p)) return(.load_orig(fn))
    if (!is.na(p) && grepl('^https?://', p)) {
      return(tryCatch(readr::read_csv(p, show_col_types = FALSE),
                      error = function(e) .load_orig(fn)))
    }
    .load_orig(fn)
  }
  .PREIS_LOAD_WRAPPED <- TRUE
}
# ==================================================================

sr_kpi      <- .load("dhis2_mve_situation_room.csv")
sr_zones    <- .load("dhis2_mve_situation_room_zones.csv")
sr_priority <- .load("dhis2_mve_situation_room_priority_zones.csv")
sr_epi      <- .load("dhis2_mve_situation_room_epi_curve.csv")
if ("semaine" %in% names(sr_epi))
  sr_epi$semaine <- suppressWarnings(as.Date(sr_epi$semaine))

D2_OK <- nrow(sr_kpi) > 0
message("[DHIS2 MODULE] LL=", .LL)
message("[DHIS2 MODULE] D2_OK=", D2_OK,
        " | KPIs=", nrow(sr_kpi),
        " | Zones=", nrow(sr_zones),
        " | Epi=", nrow(sr_epi))

## ── Helpers KPI ──────────────────────────────────────────────
.v <- function(kpi_id, col = "interpretation") {
  if (!D2_OK || !col %in% names(sr_kpi)) return(NA_character_)
  x <- sr_kpi %>% filter(kpi == kpi_id) %>% pull(col)
  if (length(x) == 0) NA_character_ else as.character(x[1])
}
.n <- function(kpi_id, col) {
  x <- suppressWarnings(as.numeric(.v(kpi_id, col)))
  if (length(x) == 0 || is.na(x)) NA_real_ else x
}

C_RED <- "#C0392B"; C_ORA <- "#E67E22"; C_GRN <- "#27AE60"
C_BLU <- "#2980B9"; C_GREY <- "#95A5A6"
.gc <- function(p, ok=80, warn=60) {
  if (is.na(p)) C_GREY else if (p>=ok) C_GRN else if (p>=warn) C_ORA else C_RED
}
.bg <- function(p, ok=80, warn=60) {
  if (is.na(p)) "#F5F5F5" else if (p>=ok) "#EAFAF1"
  else if (p>=warn) "#FEF9E7" else "#FDECEA"
}

## ── Valeurs extraites ────────────────────────────────────────
total_alertes <- .n("KPI_1_VOLUME","total")              %||% 0
alertes_7j    <- .n("KPI_1_VOLUME","last_7j")            %||% NA
pct_complet   <- .n("KPI_2_COMPLETION","pct_completes")  %||% NA
n_actifs      <- .n("KPI_5_PGSTATUS","n_actifs")         %||% NA
pct_prelev    <- .n("KPI_7_PRELEVEMENT","pct_preleves")  %||% NA
n_prelev      <- .n("KPI_7_PRELEVEMENT","n_preleves")    %||% 0
pct_pos       <- .n("KPI_7b_POSITIVITE","pct_positifs")  %||% NA
n_pos         <- .n("KPI_7b_POSITIVITE","n_positifs")    %||% 0
n_neg         <- .n("KPI_7b_POSITIVITE","n_negatifs")    %||% 0
n_inv         <- .n("KPI_7b_POSITIVITE","n_invalides")   %||% 0
pct_class     <- .n("KPI_7c_CLASSIFICATION_RATE","pct_classes") %||% NA
n_class       <- .n("KPI_7c_CLASSIFICATION_RATE","n_classes")   %||% 0
cfr           <- .n("KPI_9_CFR","cfr_pct")               %||% NA
n_confirmes   <- .n("KPI_9_CFR","n_confirmes")           %||% 0
n_zones_haute <- .n("KPI_10_PRIORITY","n_zones_haute")   %||% NA

## ── Card gap ─────────────────────────────────────────────────
.gap_card <- function(titre, pct_val, n_gap, label_gap) {
  col <- .gc(pct_val); bg <- .bg(pct_val)
  tags$div(
    style = paste0("border-left:4px solid ",col,";padding:12px 15px;background:",
                   bg,";border-radius:4px;margin:4px;"),
    tags$h4(style="margin:0 0 2px;font-size:13px;", titre),
    tags$h2(style=paste0("margin:0;color:",col,";"),
            if (!is.na(pct_val)) paste0(pct_val,"%") else "N/A"),
    tags$p(style="color:#666;font-size:11px;margin:2px 0 0;",
           if (!is.na(n_gap)) paste0(format(round(n_gap),big.mark=" "),
                                     " cas sans ", label_gap) else "")
  )
}

## ══════════════════════════════════════════════════════════════
## UI
## ══════════════════════════════════════════════════════════════
ui_dhis2_tab <- tabItem(
  tabName = "dhis2",

  fluidRow(box(width=12, background = if (D2_OK) "green" else "red",
    tags$b(if (D2_OK)
      HTML(paste0("&#10003; DHIS2 Line List MVE — ",
                  format(total_alertes,big.mark=" "),
                  " notifications | ", nrow(sr_zones), " zones"))
    else
      "Donnees DHIS2 absentes — lancer 08_situation_room_dhis2.R"))),

  fluidRow(
    valueBox(format(total_alertes,big.mark=" "), "Notifications totales",
             icon("bell"), color="red", width=3),
    valueBox(if(!is.na(alertes_7j)) alertes_7j else "—",
             "Alertes 7 derniers jours", icon("calendar-week"),
             color="orange", width=3),
    valueBox(if(!is.na(pct_complet)) paste0(pct_complet,"%") else "—",
             "Completion fiches DHIS2", icon("circle-check"),
             color=if(!is.na(pct_complet)&&pct_complet>=90)"green"else"yellow", width=3),
    valueBox(if(!is.na(n_actifs)) n_actifs else "—",
             "Cas en suivi actif", icon("user-clock"), color="yellow", width=3)
  ),

  fluidRow(
    valueBox(if(!is.na(pct_prelev)) paste0(pct_prelev,"%") else "—",
             "Taux prelevement", icon("syringe"),
             color=if(!is.na(pct_prelev)&&pct_prelev>=80)"green"
                   else if(!is.na(pct_prelev)&&pct_prelev>=60)"yellow"else"red", width=3),
    valueBox(if(!is.na(pct_pos)) paste0(pct_pos,"% POS") else "—",
             "Positivite labo", icon("flask"), color="purple", width=3),
    valueBox(if(!is.na(pct_class)) paste0(pct_class,"%") else "—",
             "Cas classifies", icon("tag"),
             color=if(!is.na(pct_class)&&pct_class>=70)"green"else"red", width=3),
    valueBox(if(!is.na(cfr)) paste0(cfr,"%") else "—",
             "CFR (cas documentes)", icon("percent"), color="black", width=3)
  ),

  fluidRow(
    box(width=8, title="Courbe epidemique — Notifications hebdomadaires",
        status="primary", solidHeader=TRUE,
        plotlyOutput("d2_epi", height=280)),
    box(width=4, title="Resultats laboratoire",
        status="primary", solidHeader=TRUE,
        plotlyOutput("d2_labo", height=280))
  ),

  fluidRow(
    box(width=12, title="Analyse des Gaps — Chaine de riposte MVE",
        status="danger", solidHeader=TRUE,
        tags$p(style="color:#555;font-size:12px;margin-bottom:8px;",
          "Chaque barre montre combien de cas franchissent chaque etape. ",
          "Barres courtes = gaps = priorites d'action."),
        plotlyOutput("d2_funnel", height=300),
        tags$hr(style="margin:10px 0;"),
        fluidRow(
          column(4, uiOutput("gap_prelev")),
          column(4, uiOutput("gap_class")),
          column(4, uiOutput("gap_statut"))
        ))
  ),

  fluidRow(
    box(width=6, title="Top 15 zones — Volume alertes",
        status="warning", solidHeader=TRUE,
        plotlyOutput("d2_zones", height=360)),
    box(width=6, title=paste0("Zones priorite HAUTE (", n_zones_haute %||% "?", ")"),
        status="danger", solidHeader=TRUE,
        DTOutput("d2_prio"), tags$br(),
        downloadButton("d2_dl","Exporter CSV"))
  )
)

## ══════════════════════════════════════════════════════════════
## SERVER
## ══════════════════════════════════════════════════════════════
server_dhis2 <- function(input, output, session) {

  output$d2_epi <- renderPlotly({
    d <- sr_epi
    if (nrow(d)==0) return(plotly_empty() %>% layout(title="Donnees absentes"))
    plot_ly(d, x=~semaine, y=~n_alertes, type="bar",
            marker=list(color=C_RED, opacity=.85)) %>%
      layout(xaxis=list(title=""), yaxis=list(title="Alertes"),
             margin=list(t=10,b=40))
  })

  output$d2_labo <- renderPlotly({
    n_nd <- max(0, total_alertes - n_pos - n_neg - n_inv)
    plot_ly(labels=c("Positif","Negatif","Invalide","Non teste"),
            values=c(n_pos,n_neg,n_inv,n_nd), type="pie",
            marker=list(colors=c(C_RED,C_BLU,C_GREY,"#DDDDDD")),
            textinfo="label+percent") %>%
      layout(showlegend=FALSE, margin=list(t=10))
  })

  output$d2_funnel <- renderPlotly({
    n_test <- n_pos + n_neg + n_inv
    steps <- c("Notifications","Preleves","Testes (resultat)",
               "Positifs","Classifies","Statut documente")
    vals  <- c(total_alertes, n_prelev, n_test, n_pos, n_class, n_confirmes)
    pcts  <- round(vals / max(total_alertes,1) * 100, 1)
    cols  <- sapply(pcts, .gc)
    plot_ly(x=vals, y=steps, type="bar", orientation="h",
            marker=list(color=cols),
            text=paste0("<b>",format(vals,big.mark=" "),"</b> (",pcts,"%)"),
            textposition="outside", hoverinfo="text") %>%
      layout(
        yaxis=list(title="", categoryorder="array", categoryarray=rev(steps)),
        xaxis=list(title="Cas", range=c(0,total_alertes*1.18)),
        margin=list(l=170,t=10,b=40),
        shapes=list(list(type="line", x0=total_alertes*.8, x1=total_alertes*.8,
                         y0=-.5, y1=length(steps)-.5,
                         line=list(color=C_GRN,dash="dot",width=2))),
        annotations=list(list(x=total_alertes*.8, y=length(steps)-.3,
                              text="Seuil 80%", showarrow=FALSE,
                              font=list(color=C_GRN,size=10), xanchor="left")))
  })

  output$gap_prelev <- renderUI(
    .gap_card("Taux prelevement", pct_prelev, total_alertes-n_prelev, "prelevement"))
  output$gap_class <- renderUI(
    .gap_card("Taux classification", pct_class, total_alertes-n_class, "classification"))
  output$gap_statut <- renderUI({
    p_st <- round(n_confirmes / max(total_alertes,1) * 100, 1)
    .gap_card("Statut final documente", p_st, total_alertes-n_confirmes, "statut")
  })

  output$d2_zones <- renderPlotly({
    d <- sr_zones
    if (nrow(d)==0) return(plotly_empty())
    col_z <- intersect(c("zone_sante","aire_sante","orgUnitName"), names(d))[1]
    col_n <- intersect(c("n_alertes","n"), names(d))[1]
    if (is.na(col_z)||is.na(col_n)) return(plotly_empty())
    top <- d %>% arrange(desc(.data[[col_n]])) %>% head(15) %>%
      mutate(lbl = str_trunc(.data[[col_z]], 28))
    plot_ly(top, x=top[[col_n]], y=reorder(top$lbl, top[[col_n]]),
            type="bar", orientation="h", marker=list(color=C_RED)) %>%
      layout(xaxis=list(title="Alertes"), yaxis=list(title=""),
             margin=list(l=200,t=10))
  })

  output$d2_prio <- renderDT({
    d <- sr_priority
    if (nrow(d)==0)
      return(datatable(data.frame(Message="Absentes"), rownames=FALSE))
    cols <- intersect(c("province","zone_sante","aire_sante","n_alertes",
                        "alertes_7j","score_criticite","priorite"), names(d))
    dd <- d
    if ("priorite" %in% names(dd)) dd <- dd %>% filter(priorite=="HAUTE")
    dd %>% select(all_of(cols)) %>%
      { if ("score_criticite" %in% names(.)) arrange(., desc(score_criticite)) else . } %>%
      head(20) %>%
      datatable(rownames=FALSE, options=list(pageLength=8,dom="tip",scrollX=TRUE),
                class="table-condensed")
  })

  output$d2_dl <- downloadHandler(
    filename=function() paste0("preis_zones_prioritaires_",Sys.Date(),".csv"),
    content =function(f) write_excel_csv(sr_priority, f))
}

message("[DHIS2 MODULE] Charge — ui_dhis2_tab et server_dhis2() disponibles")
