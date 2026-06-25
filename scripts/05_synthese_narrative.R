## ============================================================
## PREIS Ebola RDC — Générateur de synthèse narrative
## 05_synthese_narrative.R
##
## Produit une synthèse écrite FACTUELLE à 3 niveaux de lecture :
##   1. NATIONAL        — vue d'ensemble de l'épidémie
##   2. ZONE DE SANTÉ   — détail par zone (épicentre, expansion)
##   3. STRATÉGIQUE     — enjeu régional Africa CDC / partenaires
##
## RÈGLE D'OR : chaque phrase découle d'un chiffre vérifiable.
## Aucune causalité affirmée ; "drivers probables" uniquement.
## CFR toujours étiqueté "provisoire".
##
## Réutilisable : dashboard, email, rapport. Fonctions pures
## (prennent des data.frames, renvoient du texte).
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(stringr)
})

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
.fmt_n  <- function(x) if (is.na(x)) "n/d" else format(round(x), big.mark = " ")
.fmt_pc <- function(x) if (is.na(x)) "n/d" else paste0(round(x, 1), "%")
.safe_tail <- function(x, default = NA) if (length(x)) x[length(x)] else default

# Calcule les agrégats nationaux utiles à partir de la série (jusqu'au SitRep sno)
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
    if (last3[3] > last3[1]) "à la hausse"
    else if (last3[3] < last3[1]) "à la baisse" else "stable"
  } else "indéterminée"
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

# ------------------------------------------------------------
# 1. SYNTHÈSE NATIONALE
# ------------------------------------------------------------
synthese_nationale <- function(serie, sno = NULL) {
  a <- .compute_nat(serie, sno)
  if (is.null(a)) return("Données nationales indisponibles.")

  p1 <- sprintf(
    paste0("Au SitRep N°%s (%s), la 17e épidémie de maladie à virus Ebola ",
           "(souche Bundibugyo) en République démocratique du Congo totalise ",
           "%s cas confirmés cumulés et %s décès, soit une létalité provisoire ",
           "de %s. L'épidémie est suivie depuis le %s."),
    a$sitrep_no, a$date, .fmt_n(a$cas), .fmt_n(a$deces),
    .fmt_pc(a$cfr), a$debut)

  growth_txt <- if (is.na(a$growth)) "la variation récente ne peut être estimée"
    else if (a$growth > 0) sprintf("une progression de %s%% des nouveaux cas sur la dernière période", a$growth)
    else if (a$growth < 0) sprintf("un recul de %s%% des nouveaux cas sur la dernière période", abs(a$growth))
    else "une incidence stable sur la dernière période"

  p2 <- sprintf(
    paste0("La tendance des nouveaux cas est %s, avec %s cas confirmés sur les ",
           "7 derniers rapports et %s. Le dernier rapport fait état de %s nouveau(x) ",
           "cas et %s nouveau(x) décès."),
    a$trend, .fmt_n(a$inc7), growth_txt,
    .fmt_n(a$nv_cas_last), .fmt_n(a$nv_dec_last))

  caveat <- paste0("La létalité est provisoire (certains cas récents peuvent encore ",
                   "évoluer) ; les cumuls nationaux proviennent des données INRB validées.")
  paste(p1, p2, caveat, sep = "\n\n")
}

# ------------------------------------------------------------
# 2. SYNTHÈSE PAR ZONE DE SANTÉ
# ------------------------------------------------------------
synthese_zones <- function(zones, top = 5) {
  if (is.null(zones) || nrow(zones) == 0) return("Données par zone indisponibles.")
  z <- zones %>% dplyr::filter(cases > 0) %>% dplyr::arrange(dplyr::desc(cases))
  total <- sum(z$cases, na.rm = TRUE)
  ztop <- utils::head(z, top)
  top3 <- sum(utils::head(z$cases, 3), na.rm = TRUE)
  conc <- if (total > 0) round(100*top3/total, 0) else NA

  lignes <- apply(ztop, 1, function(r) {
    sprintf("%s (%s) : %s cas (%s%% du total)",
            r[["health_zone"]], r[["province"]], .fmt_n(as.numeric(r[["cases"]])),
            round(100*as.numeric(r[["cases"]])/total))
  })

  p1 <- sprintf(
    paste0("Au total, %s zones de santé rapportent des cas confirmés, réparties ",
           "principalement en Ituri et au Nord-Kivu. La transmission est fortement ",
           "concentrée : les trois zones les plus touchées regroupent %s%% des cas."),
    nrow(z), .fmt_n(conc))

  p2 <- paste0("Zones les plus touchées :\n  - ", paste(lignes, collapse = "\n  - "))

  caveat <- paste0("La localisation par zone s'appuie sur les cas confirmés cumulés ",
                   "rapportés par l'INRB ; elle reste à valider avec la ligne-liste détaillée.")
  paste(p1, p2, caveat, sep = "\n\n")
}

# ------------------------------------------------------------
# 3. SYNTHÈSE STRATÉGIQUE (Africa CDC / partenaires)
# ------------------------------------------------------------
synthese_strategique <- function(serie, zones, sno = NULL) {
  a <- .compute_nat(serie, sno)
  if (is.null(a)) return("Données stratégiques indisponibles.")
  z <- if (!is.null(zones)) zones %>% dplyr::filter(cases > 0) else NULL
  total <- if (!is.null(z)) sum(z$cases, na.rm = TRUE) else NA
  top3  <- if (!is.null(z)) sum(utils::head(sort(z$cases, decreasing = TRUE), 3), na.rm = TRUE) else NA
  conc  <- if (!is.na(total) && total > 0) round(100*top3/total, 0) else NA

  # Niveau de préoccupation dérivé des chiffres (factuel)
  niveau <- dplyr::case_when(
    !is.na(a$growth) && a$growth > 20 ~ "élevé",
    a$trend == "à la hausse"          ~ "soutenu",
    a$trend == "à la baisse"          ~ "en décroissance",
    TRUE                              ~ "à surveiller"
  )

  p1 <- sprintf(
    paste0("Enjeu régional : l'épidémie touche l'Est de la RDC (Ituri, Nord-Kivu, ",
           "Sud-Kivu), zone frontalière (Ouganda, Rwanda, Burundi, Soudan du Sud). ",
           "Avec %s cas et une létalité provisoire de %s, le niveau de préoccupation ",
           "opérationnelle est %s."),
    .fmt_n(a$cas), .fmt_pc(a$cfr), niveau)

  p2 <- sprintf(
    paste0("La concentration de %s%% des cas dans trois zones plaide pour un ciblage ",
           "des ressources sur l'épicentre. La dynamique étant %s, les priorités ",
           "partenaires sont : (i) renforcement de la recherche active et du traçage ",
           "des contacts, (ii) réduction du délai détection–isolement, (iii) ",
           "sécurisation des enterrements et engagement communautaire, (iv) ",
           "surveillance transfrontalière aux points d'entrée."),
    .fmt_n(conc), a$trend)

  p3 <- paste0("Recommandation de vigilance : maintenir la coordination Africa CDC / ",
               "Ministère de la Santé / partenaires, et consolider la complétude des ",
               "données (ligne-liste, dates de symptômes) pour permettre une estimation ",
               "du nombre de reproduction (Rt) et des projections fiables.")

  caveat <- paste0("Analyse fondée sur les données agrégées des SitReps ; drivers ",
                   "probables uniquement, sans causalité établie. CFR provisoire.")
  paste(p1, p2, p3, caveat, sep = "\n\n")
}

# ------------------------------------------------------------
# Wrapper : les trois niveaux d'un coup (texte ou HTML)
# ------------------------------------------------------------
synthese_complete <- function(serie, zones, sno = NULL, html = FALSE) {
  nat  <- synthese_nationale(serie, sno)
  zon  <- synthese_zones(zones)
  strat<- synthese_strategique(serie, zones, sno)
  if (!html) {
    paste0(
      "=== SYNTHÈSE NATIONALE ===\n\n", nat, "\n\n",
      "=== SYNTHÈSE PAR ZONE DE SANTÉ ===\n\n", zon, "\n\n",
      "=== SYNTHÈSE STRATÉGIQUE (Africa CDC / partenaires) ===\n\n", strat, "\n")
  } else {
    nl2br <- function(x) gsub("\n", "<br/>", x)
    paste0(
      "<h4>Synthèse nationale</h4><p>", nl2br(nat), "</p>",
      "<h4>Synthèse par zone de santé</h4><p>", nl2br(zon), "</p>",
      "<h4>Synthèse stratégique (Africa CDC / partenaires)</h4><p>", nl2br(strat), "</p>")
  }
}
