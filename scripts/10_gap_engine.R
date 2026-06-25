## ============================================================
##  PREIS EBOLA — MOTEUR DE GAPS (v1)
##  Script : 10_gap_engine.R
##  Auteur : Dr R. Hyacinthe ZABRE — Africa CDC / PREIS
##  Date   : 2026-06
## ------------------------------------------------------------
##  Ce script :
##    1. Lit PREIS_indicators_long.csv (base de données PREIS)
##    2. Lit PREIS_gap_rules_v1.csv (table de règles)
##    3. Évalue chaque règle sur le dernier SitRep disponible
##    4. Produit un rapport de gaps : CONSTAT → HYPOTHÈSES → RECOMMANDATIONS
##    5. Exporte outputs/analyse/PREIS_gaps_sitrep_XX.csv
##       et outputs/analyse/PREIS_gaps_sitrep_XX.html (rapport lisible)
## ------------------------------------------------------------
##  GARDE-FOUS :
##    - CFR toujours labellisé "PROVISOIRE"
##    - Gaps = HYPOTHÈSES à investiguer, JAMAIS un diagnostic
##    - Formulation : "drivers probables — pas de causalité établie"
##  CONTRAINTES TECHNIQUES :
##    - Aucun chemin Windows en dur
##    - Jamais quit() — utiliser stop()
##    - Tout output doit être commité/poussé (runner éphémère)
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(glue)
  library(knitr)        # pour le rapport HTML simple
  library(kableExtra)   # tableaux HTML
})

## ----------------------------------------------------------
## 0. CHEMINS (compatibles Linux cloud + Windows local)
## ----------------------------------------------------------
BASE_DIR <- Sys.getenv("PREIS_ROOT", unset = getwd())

path_indicators <- file.path(BASE_DIR, "data", "final", "PREIS_indicators_long.csv")
path_rules      <- file.path(BASE_DIR, "data", "rules", "PREIS_gap_rules_v1.csv")
path_out_dir    <- file.path(BASE_DIR, "outputs", "analyse")

if (!dir.exists(path_out_dir)) dir.create(path_out_dir, recursive = TRUE)

cat("[PREIS GAP ENGINE] Démarrage...\n")
cat(glue("  BASE_DIR     : {BASE_DIR}\n"))
cat(glue("  Indicateurs  : {path_indicators}\n"))
cat(glue("  Règles       : {path_rules}\n"))

## ----------------------------------------------------------
## 1. CHARGEMENT DES DONNÉES
## ----------------------------------------------------------
if (!file.exists(path_indicators)) {
  stop(glue("[ERREUR] Fichier indicateurs introuvable : {path_indicators}"))
}
if (!file.exists(path_rules)) {
  stop(glue("[ERREUR] Table de règles introuvable : {path_rules}"))
}

indicators_long <- read_csv(path_indicators, show_col_types = FALSE)
rules           <- read_csv(path_rules, show_col_types = FALSE)

cat(glue("  {nrow(indicators_long)} lignes d'indicateurs chargées.\n"))
cat(glue("  {nrow(rules)} règles chargées.\n"))

## ----------------------------------------------------------
## 2. SÉLECTION DU DERNIER SITREP (niveau National uniquement)
## ----------------------------------------------------------
# On travaille sur le niveau National pour la détection de gaps
# (les niveaux Province/Zone = contexte, pas règles nationales)

sitrep_max <- max(indicators_long$sitrep_no, na.rm = TRUE)
cat(glue("  SitRep analysé : {sitrep_max}\n"))

# SitRep précédent pour les comparaisons inter-SitRep
sitrep_prev <- sitrep_max - 1

national_current <- indicators_long %>%
  filter(sitrep_no == sitrep_max, level == "National") %>%
  select(indicator_code, value_current = value)

national_prev <- indicators_long %>%
  filter(sitrep_no == sitrep_prev, level == "National") %>%
  select(indicator_code, value_prev = value)

# Table large : un indicateur par colonne (pour les règles qui croisent indicateurs)
wide_current <- national_current %>%
  pivot_wider(names_from = indicator_code, values_from = value_current)

## ----------------------------------------------------------
## 3. FONCTION D'ÉVALUATION D'UNE RÈGLE
## ----------------------------------------------------------
# Retourne TRUE si le gap est détecté, FALSE sinon, NA si données absentes

evaluate_gap <- function(rule, wide_current, national_current, national_prev) {

  ind_code  <- rule$indicateur_code
  condition <- rule$condition_gap

  # Récupérer valeur courante
  val_current <- national_current %>%
    filter(indicator_code == ind_code) %>%
    pull(value_current)
  val_current <- if (length(val_current) == 0) NA_real_ else val_current[1]

  # Valeur précédente (pour règles de variation)
  val_prev <- national_prev %>%
    filter(indicator_code == ind_code) %>%
    pull(value_prev)
  val_prev <- if (length(val_prev) == 0) NA_real_ else val_prev[1]

  if (is.na(val_current)) return(list(gap = NA, val_current = NA, val_prev = val_prev))

  # --- Évaluation selon condition_gap ---
  gap_detected <- tryCatch({

    # Cas 1 : seuil numérique simple (ex: "< 90%")
    if (str_detect(condition, "^<\\s*\\d")) {
      threshold <- as.numeric(str_extract(condition, "\\d+\\.?\\d*"))
      val_current < threshold

    } else if (str_detect(condition, "^>\\s*\\d")) {
      threshold <- as.numeric(str_extract(condition, "\\d+\\.?\\d*"))
      val_current > threshold

    # Cas 2 : plage (ex: "30-50%" — gap si hors plage)
    } else if (str_detect(condition, "\\d+-\\d+%")) {
      bounds <- as.numeric(str_extract_all(condition, "\\d+")[[1]])
      val_current >= bounds[1] & val_current <= bounds[2]  # pas un gap si dans plage

    # Cas 3 : variation inter-SitRep (ex: "Baisse ≥ 20% vs SitRep précédent")
    } else if (str_detect(condition, "Baisse ≥ (\\d+)%")) {
      pct <- as.numeric(str_match(condition, "Baisse ≥ (\\d+)%")[,2])
      if (!is.na(val_prev) & val_prev > 0) {
        variation <- (val_current - val_prev) / val_prev * 100
        variation <= -pct
      } else NA

    } else if (str_detect(condition, "hausse ≥ (\\d+)%")) {
      pct <- as.numeric(str_match(condition, "hausse ≥ (\\d+)%")[,2])
      if (!is.na(val_prev) & val_prev > 0) {
        variation <- (val_current - val_prev) / val_prev * 100
        variation >= pct
      } else NA

    # Cas 4 : règle croisée — contacts vs new_confirmed_cases
    } else if (str_detect(condition, "contacts_listed < \\(new_confirmed_cases")) {
      new_cases <- wide_current$new_confirmed_cases[1]
      contacts  <- wide_current$contacts_listed[1]
      if (!is.na(new_cases) & !is.na(contacts) & new_cases > 0) {
        contacts < (new_cases * 10 * 0.8)
      } else NA

    # Cas 5 : règle croisée — patients_in_isolation vs new_confirmed_cases
    } else if (str_detect(condition, "patients_in_isolation < \\(new_confirmed_cases")) {
      new_cases   <- wide_current$new_confirmed_cases[1]
      in_isolation <- wide_current$patients_in_isolation[1]
      if (!is.na(new_cases) & !is.na(in_isolation) & new_cases > 0) {
        in_isolation < (new_cases * 0.9)
      } else NA

    # Cas 6 : stagnation sur 2 SitReps — simplified (na if prev unavailable)
    } else if (str_detect(condition, "Stagnation")) {
      if (!is.na(val_prev)) {
        val_current == val_prev
      } else NA

    } else {
      NA  # condition non reconnue — à étendre
    }

  }, error = function(e) {
    cat(glue("  [WARN] Évaluation échouée pour {ind_code} : {e$message}\n"))
    NA
  })

  list(gap = gap_detected, val_current = val_current, val_prev = val_prev)
}

## ----------------------------------------------------------
## 4. APPLICATION DU MOTEUR SUR TOUTES LES RÈGLES
## ----------------------------------------------------------

results <- list()

for (i in seq_len(nrow(rules))) {
  rule <- rules[i, ]
  eval <- evaluate_gap(rule, wide_current, national_current, national_prev)

  # Calcul variation %
  variation_pct <- NA_real_
  if (!is.na(eval$val_current) & !is.na(eval$val_prev) & eval$val_prev != 0) {
    variation_pct <- round((eval$val_current - eval$val_prev) / eval$val_prev * 100, 1)
  }

  # Label CFR toujours PROVISOIRE
  label_ind <- rule$indicateur_label
  if (rule$indicateur_code == "case_fatality_ratio") {
    label_ind <- paste0(label_ind, " [PROVISOIRE]")
  }

  # Statut
  statut <- case_when(
    is.na(eval$gap)       ~ "DONNÉES ABSENTES",
    eval$gap == TRUE      ~ paste0("⚠ GAP — ", rule$severite_gap),
    eval$gap == FALSE     ~ "✓ OK",
    TRUE                  ~ "?"
  )

  results[[i]] <- tibble(
    sitrep_no           = sitrep_max,
    domaine             = rule$domaine,
    indicateur_code     = rule$indicateur_code,
    indicateur_label    = label_ind,
    valeur_actuelle     = eval$val_current,
    valeur_precedente   = eval$val_prev,
    variation_pct       = variation_pct,
    cible               = paste0(rule$cible_valeur, " ", rule$cible_unite),
    cible_source        = rule$cible_source,
    condition_gap       = rule$condition_gap,
    statut              = statut,
    severite            = ifelse(isTRUE(eval$gap), rule$severite_gap, NA_character_),
    hypotheses          = ifelse(isTRUE(eval$gap), rule$hypotheses_possibles, NA_character_),
    recommandations     = ifelse(isTRUE(eval$gap), rule$recommandations, NA_character_),
    acteur_a_alerter    = ifelse(isTRUE(eval$gap), rule$acteur_a_alerter, NA_character_),
    note_methodologique = rule$note_methodologique
  )
}

gaps_df <- bind_rows(results)

## ----------------------------------------------------------
## 5. RÉSUMÉ CONSOLE
## ----------------------------------------------------------

n_gaps_critique <- sum(str_detect(gaps_df$statut, "CRITIQUE"), na.rm = TRUE)
n_gaps_attention <- sum(str_detect(gaps_df$statut, "ATTENTION"), na.rm = TRUE)
n_ok             <- sum(str_detect(gaps_df$statut, "✓ OK"), na.rm = TRUE)
n_absent         <- sum(str_detect(gaps_df$statut, "DONNÉES ABSENTES"), na.rm = TRUE)

cat("\n=== RÉSUMÉ GAPS — SITREP", sitrep_max, "===\n")
cat(glue("  CRITIQUE     : {n_gaps_critique}\n"))
cat(glue("  ATTENTION    : {n_gaps_attention}\n"))
cat(glue("  OK           : {n_ok}\n"))
cat(glue("  Données abs. : {n_absent}\n"))

if (n_gaps_critique > 0) {
  cat("\n[GAPS CRITIQUES]\n")
  gaps_critique <- gaps_df %>% filter(str_detect(statut, "CRITIQUE"))
  for (j in seq_len(nrow(gaps_critique))) {
    g <- gaps_critique[j, ]
    cat(glue("\n  🔴 {g$indicateur_label}\n"))
    cat(glue("     Valeur    : {g$valeur_actuelle} (cible : {g$cible})\n"))
    cat(glue("     CONSTAT   : {g$statut}\n"))
    cat(glue("     HYPOTHÈSES (drivers probables — pas de causalité établie) :\n"))
    hyp_list <- str_split(g$hypotheses, " \\| ")[[1]]
    for (h in hyp_list) cat(glue("       • {str_trim(h)}\n"))
    cat(glue("     RECOMMANDATIONS :\n"))
    rec_list <- str_split(g$recommandations, " \\| ")[[1]]
    for (r in rec_list) cat(glue("       → {str_trim(r)}\n"))
    cat(glue("     ALERTER   : {g$acteur_a_alerter}\n"))
  }
}

## ----------------------------------------------------------
## 6. EXPORT CSV
## ----------------------------------------------------------

out_csv <- file.path(path_out_dir, glue("PREIS_gaps_sitrep_{sitrep_max}.csv"))
write_csv(gaps_df, out_csv)
cat(glue("\n[OUTPUT] CSV exporté : {out_csv}\n"))

## ----------------------------------------------------------
## 7. RAPPORT HTML SIMPLE
## ----------------------------------------------------------

out_html <- file.path(path_out_dir, glue("PREIS_gaps_sitrep_{sitrep_max}.html"))

# Tableau de synthèse pour le HTML
table_html <- gaps_df %>%
  select(domaine, indicateur_label, valeur_actuelle, cible, statut,
         hypotheses, recommandations, acteur_a_alerter) %>%
  mutate(
    statut = cell_spec(
      statut,
      color = case_when(
        str_detect(statut, "CRITIQUE")        ~ "white",
        str_detect(statut, "ATTENTION")       ~ "white",
        str_detect(statut, "✓ OK")            ~ "white",
        str_detect(statut, "DONNÉES ABSENTES")~ "white",
        TRUE ~ "black"
      ),
      background = case_when(
        str_detect(statut, "CRITIQUE")        ~ "#c0392b",
        str_detect(statut, "ATTENTION")       ~ "#e67e22",
        str_detect(statut, "✓ OK")            ~ "#27ae60",
        str_detect(statut, "DONNÉES ABSENTES")~ "#7f8c8d",
        TRUE ~ "white"
      ),
      escape = FALSE
    )
  ) %>%
  kable(
    format    = "html",
    escape    = FALSE,
    col.names = c("Domaine", "Indicateur", "Valeur actuelle", "Cible / Seuil",
                  "Statut", "Hypothèses (à investiguer)", "Recommandations", "Acteur à alerter"),
    caption   = glue(
      "PREIS Ebola RDC — Rapport de gaps — SitRep {sitrep_max} | ",
      "CFR TOUJOURS PROVISOIRE | Gaps = hypothèses à investiguer, pas un diagnostic | ",
      "Totaux INRB validés ; détail zones à valider terrain"
    )
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = TRUE,
    font_size         = 13
  ) %>%
  column_spec(6, width = "25%") %>%
  column_spec(7, width = "20%")

# Entête HTML avec garde-fous visibles
html_header <- glue('
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>PREIS Ebola RDC — Gaps SitRep {sitrep_max}</title>
  <style>
    body {{ font-family: "Segoe UI", Arial, sans-serif; margin: 20px; background: #f8f9fa; color: #2c3e50; }}
    .header {{ background: #2c3e50; color: white; padding: 16px 24px; border-radius: 8px; margin-bottom: 16px; }}
    .header h2 {{ margin: 0 0 4px 0; font-size: 1.3em; }}
    .header p {{ margin: 0; font-size: 0.9em; opacity: 0.85; }}
    .garde-fous {{ background: #fef9e7; border-left: 4px solid #f39c12; padding: 12px 16px;
                   margin-bottom: 20px; border-radius: 0 6px 6px 0; font-size: 0.88em; }}
    .garde-fous strong {{ color: #e67e22; }}
    .legend {{ display: flex; gap: 16px; margin-bottom: 16px; flex-wrap: wrap; }}
    .leg {{ padding: 4px 12px; border-radius: 4px; font-size: 0.85em; color: white; font-weight: bold; }}
    .footer {{ margin-top: 24px; font-size: 0.8em; color: #7f8c8d; border-top: 1px solid #ddd; padding-top: 12px; }}
  </style>
</head>
<body>
<div class="header">
  <h2>🔬 PREIS Ebola RDC — Rapport de Gaps automatique</h2>
  <p>SitRep {sitrep_max} | Généré automatiquement par PREIS | Africa CDC — Dr R.H. Zabre</p>
</div>
<div class="garde-fous">
  <strong>⚠ GARDE-FOUS MÉTHODOLOGIQUES (non négociables)</strong><br>
  • <strong>CFR : TOUJOURS PROVISOIRE</strong> — à ne pas publier sans validation INSP/INRB.<br>
  • Les <strong>gaps détectés sont des signaux</strong>, pas des diagnostics. Les hypothèses proposées sont des <em>drivers probables à investiguer</em> — aucune causalité établie.<br>
  • Totaux nationaux = données INRB validées. Détail zones de santé (extrait PDF) = à valider terrain avant toute communication officielle.
</div>
<div class="legend">
  <span class="leg" style="background:#c0392b;">🔴 CRITIQUE</span>
  <span class="leg" style="background:#e67e22;">🟠 ATTENTION</span>
  <span class="leg" style="background:#27ae60;">🟢 OK</span>
  <span class="leg" style="background:#7f8c8d;">⚫ Données absentes</span>
</div>
')

html_footer <- glue('
<div class="footer">
  PREIS (Platform for Real-time Epidemiological Intelligence and Surveillance) — Africa CDC | 
  SitRep {sitrep_max} | {format(Sys.time(), "%Y-%m-%d %H:%M UTC")}<br>
  <em>Ce rapport est un outil d\'aide à la décision. Toute communication officielle requiert 
  validation par INSP/INRB et co-signature.</em>
</div>
</body>
</html>
')

writeLines(
  c(html_header, as.character(table_html), html_footer),
  con = out_html
)
cat(glue("[OUTPUT] Rapport HTML exporté : {out_html}\n"))

## ----------------------------------------------------------
## 8. RETOUR POUR USAGE PROGRAMMATIQUE (dashboard Shiny)
## ----------------------------------------------------------
# Le dashboard peut sourcer ce script et lire gaps_df directement,
# ou lire le CSV exporté.
# Exemple d'usage dans app.R :
#   source("scripts/10_gap_engine.R")
#   # => gaps_df disponible dans l'environnement

invisible(gaps_df)

cat("\n[PREIS GAP ENGINE] Terminé sans erreur.\n")
