## ============================================================
## PREIS Ebola DRC — Narrative synthesis generator
## 05_synthese_narrative.R
##
## Produces a FACTUAL written synthesis at 3 reading levels:
##   1. NATIONAL      — outbreak overview
##   2. HEALTH ZONE   — detail by zone (epicentre, expansion)
##   3. STRATEGIC     — regional Africa CDC / partners angle
##
## GOLDEN RULE: every sentence follows from a verifiable number.
## No causality asserted; "probable drivers" only.
## CFR always labelled "provisional".
##
## Reusable: dashboard, email, report. Pure functions
## (take data.frames, return text).
##
## CHANGE (this version): bilingual EN/FR. Default language is
## now "en" (Africa CDC default), matching the decision recorded
## in claude/PREIS_dashboard_inventaire.md ("anglais par défaut +
## sélecteur conservé"). All existing calls (synthese_nationale(x),
## synthese_zones(x), synthese_strategique(x,y)) keep working
## unchanged — they now simply return English text by default.
## Pass lang = "fr" explicitly to get the original French text.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(stringr)
})

# ------------------------------------------------------------
# Helpers (language-agnostic)
# ------------------------------------------------------------
.fmt_n  <- function(x, lang = "en") {
  if (is.na(x)) return(if (lang == "fr") "n/d" else "n/a")
  format(round(x), big.mark = if (lang == "fr") " " else ",")
}
.fmt_pc <- function(x, lang = "en") {
  if (is.na(x)) return(if (lang == "fr") "n/d" else "n/a")
  paste0(round(x, 1), "%")
}
.safe_tail <- function(x, default = NA) if (length(x)) x[length(x)] else default

# National aggregates from the series (up to SitRep sno). Returns
# LANGUAGE-NEUTRAL codes for trend ("rising"/"falling"/"stable"/
# "undetermined") — the display layer below localises them.
.compute_nat <- function(serie, sno = NULL) {
  s <- serie %>% dplyr::arrange(sitrep_no)
  if (!is.null(sno)) s <- s %>% dplyr::filter(sitrep_no <= sno)
  if (nrow(s) == 0) return(NULL)
  s <- s %>% dplyr::mutate(
    nv_cas = pmax(cas_cumules - dplyr::lag(cas_cumules), 0),
    nv_dec = pmax(deces_cumules - dplyr::lag(deces_cumules), 0)
  )
  n <- nrow(s); last <- s[n, ]
  k <- min(7, n)
  inc7      <- sum(utils::tail(s$nv_cas, k), na.rm = TRUE)
  inc7_prev <- if (n >= 2*k) sum(s$nv_cas[(n-2*k+1):(n-k)], na.rm = TRUE) else NA
  last3 <- utils::tail(s$nv_cas, 3)
  trend <- if (length(last3) == 3) {
    if (last3[3] > last3[1]) "rising"
    else if (last3[3] < last3[1]) "falling" else "stable"
  } else "undetermined"
  growth <- if (!is.na(inc7_prev) && inc7_prev > 0)
    round(100*(inc7 - inc7_prev)/inc7_prev, 0) else NA
  list(
    sitrep_no = last$sitrep_no, date = last$date,
    cas = last$cas_cumules, deces = last$deces_cumules,
    cfr = last$cfr, nv_cas_last = .safe_tail(s$nv_cas),
    nv_dec_last = .safe_tail(s$nv_dec),
    inc7 = inc7, growth = growth, trend = trend,
    debut = s$date[1]
  )
}

.trend_label <- function(code, lang) {
  map <- list(
    en = c(rising = "rising", falling = "falling", stable = "stable", undetermined = "undetermined"),
    fr = c(rising = "\u00e0 la hausse", falling = "\u00e0 la baisse", stable = "stable", undetermined = "ind\u00e9termin\u00e9e")
  )
  map[[lang]][[code]]
}
.niveau_label <- function(code, lang) {
  map <- list(
    en = c(high = "high", sustained = "sustained", declining = "declining", to_monitor = "to monitor"),
    fr = c(high = "\u00e9lev\u00e9", sustained = "soutenu", declining = "en d\u00e9croissance", to_monitor = "\u00e0 surveiller")
  )
  map[[lang]][[code]]
}

# ------------------------------------------------------------
# 1. NATIONAL SUMMARY
# ------------------------------------------------------------
synthese_nationale <- function(serie, sno = NULL, lang = "en") {
  a <- .compute_nat(serie, sno)
  if (is.null(a)) return(if (lang == "fr") "Donn\u00e9es nationales indisponibles." else "National data unavailable.")

  trend_lbl <- .trend_label(a$trend, lang)

  if (lang == "fr") {
    p1 <- sprintf(
      paste0("Au SitRep N\u00b0%s (%s), la 17e \u00e9pid\u00e9mie de maladie \u00e0 virus Ebola ",
             "(souche Bundibugyo) en R\u00e9publique d\u00e9mocratique du Congo totalise ",
             "%s cas confirm\u00e9s cumul\u00e9s et %s d\u00e9c\u00e8s, soit une l\u00e9talit\u00e9 provisoire ",
             "de %s. L'\u00e9pid\u00e9mie est suivie depuis le %s."),
      a$sitrep_no, a$date, .fmt_n(a$cas, lang), .fmt_n(a$deces, lang),
      .fmt_pc(a$cfr, lang), a$debut)

    growth_txt <- if (is.na(a$growth)) "la variation r\u00e9cente ne peut \u00eatre estim\u00e9e"
      else if (a$growth > 0) sprintf("une progression de %s%% des nouveaux cas sur la derni\u00e8re p\u00e9riode", a$growth)
      else if (a$growth < 0) sprintf("un recul de %s%% des nouveaux cas sur la derni\u00e8re p\u00e9riode", abs(a$growth))
      else "une incidence stable sur la derni\u00e8re p\u00e9riode"

    p2 <- sprintf(
      paste0("La tendance des nouveaux cas est %s, avec %s cas confirm\u00e9s sur les ",
             "7 derniers rapports et %s. Le dernier rapport fait \u00e9tat de %s nouveau(x) ",
             "cas et %s nouveau(x) d\u00e9c\u00e8s."),
      trend_lbl, .fmt_n(a$inc7, lang), growth_txt,
      .fmt_n(a$nv_cas_last, lang), .fmt_n(a$nv_dec_last, lang))

    caveat <- paste0("La l\u00e9talit\u00e9 est provisoire (certains cas r\u00e9cents peuvent encore ",
                     "\u00e9voluer) ; les cumuls nationaux proviennent des donn\u00e9es INRB valid\u00e9es.")
  } else {
    p1 <- sprintf(
      paste0("As of SitRep No. %s (%s), the 17th outbreak of Ebola virus disease ",
             "(Bundibugyo strain) in the Democratic Republic of the Congo totals ",
             "%s cumulative confirmed cases and %s deaths, a provisional case-fatality ",
             "ratio of %s. The outbreak has been under surveillance since %s."),
      a$sitrep_no, a$date, .fmt_n(a$cas, lang), .fmt_n(a$deces, lang),
      .fmt_pc(a$cfr, lang), a$debut)

    growth_txt <- if (is.na(a$growth)) "the recent trend cannot be estimated"
      else if (a$growth > 0) sprintf("a %s%% increase in new cases over the last period", a$growth)
      else if (a$growth < 0) sprintf("a %s%% decrease in new cases over the last period", abs(a$growth))
      else "stable incidence over the last period"

    p2 <- sprintf(
      paste0("The trend in new cases is %s, with %s confirmed cases over the last ",
             "7 reports and %s. The latest report recorded %s new case(s) and ",
             "%s new death(s)."),
      trend_lbl, .fmt_n(a$inc7, lang), growth_txt,
      .fmt_n(a$nv_cas_last, lang), .fmt_n(a$nv_dec_last, lang))

    caveat <- paste0("The case-fatality ratio is provisional (some recent cases may ",
                     "still evolve); national cumulative totals come from validated INRB data.")
  }
  paste(p1, p2, caveat, sep = "\n\n")
}

# ------------------------------------------------------------
# 2. HEALTH ZONE SUMMARY
# ------------------------------------------------------------
synthese_zones <- function(zones, top = 5, lang = "en") {
  if (is.null(zones) || nrow(zones) == 0)
    return(if (lang == "fr") "Donn\u00e9es par zone indisponibles." else "Health-zone data unavailable.")
  z <- zones %>% dplyr::filter(cases > 0) %>% dplyr::arrange(dplyr::desc(cases))
  total <- sum(z$cases, na.rm = TRUE)
  ztop <- utils::head(z, top)
  top3 <- sum(utils::head(z$cases, 3), na.rm = TRUE)
  conc <- if (total > 0) round(100*top3/total, 0) else NA

  if (lang == "fr") {
    lignes <- apply(ztop, 1, function(r) sprintf("%s (%s) : %s cas (%s%% du total)",
      r[["health_zone"]], r[["province"]], .fmt_n(as.numeric(r[["cases"]]), lang),
      round(100*as.numeric(r[["cases"]])/total)))
    p1 <- sprintf(paste0("Au total, %s zones de sant\u00e9 rapportent des cas confirm\u00e9s, r\u00e9parties ",
             "principalement en Ituri et au Nord-Kivu. La transmission est fortement ",
             "concentr\u00e9e : les trois zones les plus touch\u00e9es regroupent %s%% des cas."),
      nrow(z), .fmt_n(conc, lang))
    p2 <- paste0("Zones les plus touch\u00e9es :\n  - ", paste(lignes, collapse = "\n  - "))
    caveat <- paste0("La localisation par zone s'appuie sur les cas confirm\u00e9s cumul\u00e9s ",
                     "rapport\u00e9s par l'INRB ; elle reste \u00e0 valider avec la ligne-liste d\u00e9taill\u00e9e.")
  } else {
    lignes <- apply(ztop, 1, function(r) sprintf("%s (%s): %s cases (%s%% of total)",
      r[["health_zone"]], r[["province"]], .fmt_n(as.numeric(r[["cases"]]), lang),
      round(100*as.numeric(r[["cases"]])/total)))
    p1 <- sprintf(paste0("In total, %s health zones report confirmed cases, concentrated ",
             "mainly in Ituri and North Kivu. Transmission is heavily concentrated: ",
             "the three most affected zones account for %s%% of all cases."),
      nrow(z), .fmt_n(conc, lang))
    p2 <- paste0("Most affected zones:\n  - ", paste(lignes, collapse = "\n  - "))
    caveat <- paste0("Zone-level location is based on cumulative confirmed cases ",
                     "reported by INRB; it remains to be validated against the detailed line list.")
  }
  paste(p1, p2, caveat, sep = "\n\n")
}

# ------------------------------------------------------------
# 3. STRATEGIC SUMMARY (Africa CDC / partners)
# ------------------------------------------------------------
synthese_strategique <- function(serie, zones, sno = NULL, lang = "en") {
  a <- .compute_nat(serie, sno)
  if (is.null(a)) return(if (lang == "fr") "Donn\u00e9es strat\u00e9giques indisponibles." else "Strategic data unavailable.")
  z <- if (!is.null(zones)) zones %>% dplyr::filter(cases > 0) else NULL
  total <- if (!is.null(z)) sum(z$cases, na.rm = TRUE) else NA
  top3  <- if (!is.null(z)) sum(utils::head(sort(z$cases, decreasing = TRUE), 3), na.rm = TRUE) else NA
  conc  <- if (!is.na(total) && total > 0) round(100*top3/total, 0) else NA

  niveau_code <- dplyr::case_when(
    !is.na(a$growth) && a$growth > 20 ~ "high",
    a$trend == "rising"               ~ "sustained",
    a$trend == "falling"              ~ "declining",
    TRUE                               ~ "to_monitor"
  )
  niveau <- .niveau_label(niveau_code, lang)
  trend_lbl <- .trend_label(a$trend, lang)

  if (lang == "fr") {
    p1 <- sprintf(paste0("Enjeu r\u00e9gional : l'\u00e9pid\u00e9mie touche l'Est de la RDC (Ituri, Nord-Kivu, ",
             "Sud-Kivu), zone fronti\u00e8re (Ouganda, Rwanda, Burundi, Soudan du Sud). ",
             "Avec %s cas et une l\u00e9talit\u00e9 provisoire de %s, le niveau de pr\u00e9occupation ",
             "op\u00e9rationnelle est %s."), .fmt_n(a$cas, lang), .fmt_pc(a$cfr, lang), niveau)
    p2 <- sprintf(paste0("La concentration de %s%% des cas dans trois zones plaide pour un ciblage ",
             "des ressources sur l'\u00e9picentre. La dynamique \u00e9tant %s, les priorit\u00e9s ",
             "partenaires sont : (i) renforcement de la recherche active et du tra\u00e7age ",
             "des contacts, (ii) r\u00e9duction du d\u00e9lai d\u00e9tection\u2013isolement, (iii) ",
             "s\u00e9curisation des enterrements et engagement communautaire, (iv) ",
             "surveillance transfronti\u00e8re aux points d'entr\u00e9e."), .fmt_n(conc, lang), trend_lbl)
    p3 <- paste0("Recommandation de vigilance : maintenir la coordination Africa CDC / ",
                 "Minist\u00e8re de la Sant\u00e9 / partenaires, et consolider la compl\u00e9tude des ",
                 "donn\u00e9es (ligne-liste, dates de sympt\u00f4mes) pour permettre une estimation ",
                 "du nombre de reproduction (Rt) et des projections fiables.")
    caveat <- paste0("Analyse fond\u00e9e sur les donn\u00e9es agr\u00e9g\u00e9es des SitReps ; drivers ",
                     "probables uniquement, sans causalit\u00e9 \u00e9tablie. CFR provisoire.")
  } else {
    p1 <- sprintf(paste0("Regional stakes: the outbreak affects eastern DRC (Ituri, North Kivu, ",
             "South Kivu), a border area (Uganda, Rwanda, Burundi, South Sudan). With ",
             "%s cases and a provisional case-fatality ratio of %s, the operational ",
             "concern level is %s."), .fmt_n(a$cas, lang), .fmt_pc(a$cfr, lang), niveau)
    p2 <- sprintf(paste0("The concentration of %s%% of cases in three zones supports targeting ",
             "resources on the epicentre. With a %s dynamic, partner priorities are: ",
             "(i) strengthening active case-finding and contact tracing, (ii) reducing ",
             "the detection-to-isolation delay, (iii) securing safe burials and community ",
             "engagement, (iv) cross-border surveillance at points of entry."),
             .fmt_n(conc, lang), trend_lbl)
    p3 <- paste0("Vigilance recommendation: maintain Africa CDC / Ministry of Health / ",
                 "partner coordination, and strengthen data completeness (line list, ",
                 "symptom-onset dates) to enable a reliable reproduction-number (Rt) ",
                 "estimate and projections.")
    caveat <- paste0("Analysis based on aggregated SitRep data; probable drivers only, ",
                     "no established causality. Case-fatality ratio is provisional.")
  }
  paste(p1, p2, p3, caveat, sep = "\n\n")
}

# ------------------------------------------------------------
# Wrapper: all three levels at once (text or HTML)
# ------------------------------------------------------------
synthese_complete <- function(serie, zones, sno = NULL, html = FALSE, lang = "en") {
  nat   <- synthese_nationale(serie, sno, lang = lang)
  zon   <- synthese_zones(zones, lang = lang)
  strat <- synthese_strategique(serie, zones, sno, lang = lang)

  hdr <- if (lang == "fr")
    c("SYNTH\u00c8SE NATIONALE", "SYNTH\u00c8SE PAR ZONE DE SANT\u00c9", "SYNTH\u00c8SE STRAT\u00c9GIQUE (Africa CDC / partenaires)")
  else
    c("NATIONAL SUMMARY", "HEALTH ZONE SUMMARY", "STRATEGIC SUMMARY (Africa CDC / partners)")

  if (!html) {
    paste0("=== ", hdr[1], " ===\n\n", nat, "\n\n",
           "=== ", hdr[2], " ===\n\n", zon, "\n\n",
           "=== ", hdr[3], " ===\n\n", strat, "\n")
  } else {
    nl2br <- function(x) gsub("\n", "<br/>", x)
    paste0("<h4>", hdr[1], "</h4><p>", nl2br(nat), "</p>",
           "<h4>", hdr[2], "</h4><p>", nl2br(zon), "</p>",
           "<h4>", hdr[3], "</h4><p>", nl2br(strat), "</p>")
  }
}
