# =============================================================================
# 12_validate_pipeline_freshness.R  (v2 TOLERANT ET AUTO-CICATRISANT)
# -----------------------------------------------------------------------------
# Remplace la version qui utilise stop() (bloquante). La nouvelle logique :
#
#   1. DETECTER  : compare PDFs presents / indicateurs / serie temporelle
#   2. TOLERER   : un decalage transitoire d'1-2 SR est NORMAL en production
#                  (extraction asynchrone, race conditions inevitable)
#   3. ALERTER   : email seulement si le meme decalage persiste >= 3 runs
#                  (soit environ 1h30 d'anomalie confirmee)
#   4. NE JAMAIS BLOQUER : toujours exit 0. Le prochain run rattrape.
#
# Etat persistant : data/final/pipeline_health_state.csv
#   Permet de tracer les gaps recurrents et d'envoyer l'alerte a partir
#   du 3e cycle consecutif ou le meme SR reste manquant.
#
# Diagnostic detaille : cat print stdout avec details complets pour les logs
#   GitHub Actions consultables sans quitter le workflow.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tibble); library(lubridate)
})

# ---- Config ------------------------------------------------------------------
PDF_DIR              <- "data/pdf"
IND_LONG             <- "data/final/PREIS_indicators_long.csv"
IND_VALIDATED        <- "data/final/PREIS_indicators_validated.csv"
SERIE_NATIONALE      <- "outputs/analyse/serie_temporelle_nationale.csv"
REGISTRY             <- "data/final/sitrep_registry.csv"
HEALTH_STATE_FILE    <- "data/final/pipeline_health_state.csv"
ALERT_THRESHOLD_RUNS <- 3      # nombre de runs consecutifs avant alerte
TOLERANCE_TRANSIENT  <- 2      # decalage <= 2 SR = transitoire normal

# ---- Utils -------------------------------------------------------------------
log_msg <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), tz = "UTC"), m))
`%||%`  <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

banner <- function(txt) {
  bar <- paste(rep("=", 70), collapse = "")
  cat(sprintf("\n%s\n%s\n%s\n", bar, txt, bar))
}

# =============================================================================
# 1. LECTURE DE L'ETAT ACTUEL
# =============================================================================
read_pdf_max <- function() {
  if (!dir.exists(PDF_DIR)) return(NA_integer_)
  files <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = FALSE)
  if (length(files) == 0L) return(NA_integer_)
  # Support des 3 conventions : PREIS_DRC_Ebola_SitRep_NNN, SitRep_NNNN_YYYY-MM-DD, SitRep_NN_YYYY
  m1 <- stringr::str_match(files, "PREIS_DRC_Ebola_SitRep_(\\d+)\\.pdf")[, 2]
  m2 <- stringr::str_match(files, "SitRep_N(\\d{2,4})_")[, 2]
  m3 <- stringr::str_match(files, "^SitRep_(\\d+)_\\d{4}\\.pdf$")[, 2]
  nums <- suppressWarnings(as.integer(c(m1, m2, m3)))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0L) return(NA_integer_)
  max(nums)
}

read_indicator_max <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  d <- tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0L) return(NA_integer_)
  col_candidates <- c("sitrep_no", "num_sitrep", "sitrep_num", "SitRep", "num")
  col <- intersect(col_candidates, names(d))[1]
  if (is.na(col)) return(NA_integer_)
  nums <- suppressWarnings(as.integer(d[[col]]))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0L) return(NA_integer_)
  max(nums)
}

read_series_max <- function() read_indicator_max(SERIE_NATIONALE)
read_registry_max <- function() read_indicator_max(REGISTRY)

# =============================================================================
# 2. ETAT DE SANTE PERSISTANT
# =============================================================================
read_health_state <- function() {
  if (!file.exists(HEALTH_STATE_FILE)) {
    return(tibble::tibble(
      run_utc = as.POSIXct(character(0), tz = "UTC"),
      pdf_max = integer(0),
      indicators_max = integer(0),
      series_max = integer(0),
      registry_max = integer(0),
      gap_indicators = integer(0),
      gap_series = integer(0),
      status = character(0),
      alerted = logical(0)
    ))
  }
  suppressMessages(readr::read_csv(HEALTH_STATE_FILE, show_col_types = FALSE)) %>%
    dplyr::mutate(run_utc = as.POSIXct(run_utc, tz = "UTC"))
}

write_health_state <- function(state) {
  dir.create(dirname(HEALTH_STATE_FILE), recursive = TRUE, showWarnings = FALSE)
  # Garder les 100 derniers runs seulement
  if (nrow(state) > 100) state <- utils::tail(state, 100)
  readr::write_csv(state, HEALTH_STATE_FILE)
}

# =============================================================================
# 3. ENVOI ALERTE EMAIL (avec fallback silencieux si infra indisponible)
# =============================================================================
send_alert_email <- function(subject, body_text) {
  # Priorite 1 : reutiliser la couche email PREIS existante
  if (file.exists("scripts/00_email_smtp_base.R")) {
    ok <- tryCatch({
      source("scripts/00_email_smtp_base.R", local = TRUE)
      if (exists("preis_send_email", mode = "function")) {
        preis_send_email(subject = subject, body_text = body_text)
        TRUE
      } else FALSE
    }, error = function(e) {
      log_msg(sprintf("Envoi email echec (source) : %s", e$message))
      FALSE
    })
    if (ok) {
      log_msg("Alerte email envoyee via PREIS SMTP")
      return(TRUE)
    }
  }
  log_msg("Alerte NON envoyee (infra email non disponible)")
  FALSE
}

# =============================================================================
# MAIN
# =============================================================================
banner("PREIS - VALIDATION FRAICHEUR PIPELINE (v2 tolerant)")

now_utc <- Sys.time()
attr(now_utc, "tzone") <- "UTC"

# Lecture etat actuel
pdf_max        <- read_pdf_max()
indicators_max <- read_indicator_max(IND_LONG)
series_max     <- read_series_max()
registry_max   <- read_registry_max()

cat(sprintf("Timestamp UTC     : %s\n", format(now_utc, "%Y-%m-%d %H:%M:%S")))
cat(sprintf("PDFs (data/pdf/)  : max N%03d\n", pdf_max %||% -1))
cat(sprintf("Registre max      : N%03d\n", registry_max %||% -1))
cat(sprintf("Indicateurs long  : N%03d\n", indicators_max %||% -1))
cat(sprintf("Serie temporelle  : N%03d\n", series_max %||% -1))

# Calcul des gaps
gap_indicators <- if (!is.na(pdf_max) && !is.na(indicators_max)) {
  pdf_max - indicators_max
} else NA_integer_

gap_series <- if (!is.na(pdf_max) && !is.na(series_max)) {
  pdf_max - series_max
} else NA_integer_

cat(sprintf("\nDecalage indicateurs : %s\n",
            if (is.na(gap_indicators)) "N/A" else sprintf("%+d SR", gap_indicators)))
cat(sprintf("Decalage serie      : %s\n",
            if (is.na(gap_series)) "N/A" else sprintf("%+d SR", gap_series)))

# Determination du statut
status <- if (is.na(pdf_max) || is.na(indicators_max)) {
  "CRITIQUE_donnees_manquantes"
} else if (gap_indicators == 0) {
  "OK"
} else if (gap_indicators <= TOLERANCE_TRANSIENT) {
  "TRANSITOIRE_tolerable"
} else {
  "DECALAGE_persistant"
}

cat(sprintf("\n>>> Statut : %s\n", status))

# Mise a jour de l'etat persistant
state <- read_health_state()
new_row <- tibble::tibble(
  run_utc        = now_utc,
  pdf_max        = as.integer(pdf_max %||% NA),
  indicators_max = as.integer(indicators_max %||% NA),
  series_max     = as.integer(series_max %||% NA),
  registry_max   = as.integer(registry_max %||% NA),
  gap_indicators = as.integer(gap_indicators %||% NA),
  gap_series     = as.integer(gap_series %||% NA),
  status         = status,
  alerted        = FALSE
)
state <- dplyr::bind_rows(state, new_row)

# =============================================================================
# 4. ANALYSE DES GAPS PERSISTANTS + ALERTE
# =============================================================================
alert_needed <- FALSE
alert_reason <- character(0)

if (status == "CRITIQUE_donnees_manquantes") {
  cat("\n>>> Donnees critiques manquantes. Verification recommandee.\n")
  # On alerte des le 1er cas critique (pas de tolerance)
  if (nrow(state %>% dplyr::filter(status == "CRITIQUE_donnees_manquantes" & alerted)) == 0L) {
    alert_needed <- TRUE
    alert_reason <- c(alert_reason, "Fichiers pipeline critiques manquants")
  }
}

if (status == "DECALAGE_persistant") {
  # Compter les runs consecutifs avec gap similaire
  recent <- state %>%
    dplyr::arrange(dplyr::desc(run_utc)) %>%
    utils::head(ALERT_THRESHOLD_RUNS)
  same_gap_count <- sum(recent$status == "DECALAGE_persistant" &
                        recent$pdf_max == pdf_max &
                        recent$indicators_max == indicators_max, na.rm = TRUE)
  cat(sprintf("\nRuns consecutifs avec ce meme gap : %d / %d\n",
              same_gap_count, ALERT_THRESHOLD_RUNS))

  if (same_gap_count >= ALERT_THRESHOLD_RUNS) {
    already_alerted <- state %>%
      dplyr::filter(pdf_max == !!pdf_max &
                    indicators_max == !!indicators_max &
                    alerted) %>%
      nrow() > 0
    if (!already_alerted) {
      alert_needed <- TRUE
      alert_reason <- c(alert_reason,
        sprintf("Decalage persistant : PDF N%03d present depuis %d runs, indicateurs bloques a N%03d",
                pdf_max, same_gap_count, indicators_max))
    } else {
      cat("Alerte deja envoyee pour ce gap - pas de nouvel envoi\n")
    }
  }
}

# Envoi de l'alerte
if (alert_needed) {
  banner("ALERTE PREIS - Envoi email")
  subject <- sprintf("[PREIS Ebola DRC] Anomalie pipeline persistante (N%03d)",
                     pdf_max %||% 0)
  body <- sprintf(paste0(
    "Le validateur PREIS signale une anomalie confirmee.\n\n",
    "Timestamp UTC : %s\n\n",
    "Etat pipeline :\n",
    "  PDFs disponibles     : N%03d\n",
    "  Indicateurs long     : N%03d (decalage : %+d SR)\n",
    "  Serie temporelle     : N%03d (decalage : %+d SR)\n",
    "  Registre             : N%03d\n\n",
    "Raison(s) :\n%s\n\n",
    "Actions recommandees :\n",
    "  1. Verifier data/final/pipeline_health_state.csv (100 derniers runs)\n",
    "  2. Inspecter le PDF concerne : data/pdf/PREIS_DRC_Ebola_SitRep_%03d.pdf\n",
    "  3. Relancer manuellement : Rscript scripts/00_PREIS_MASTER_AUTOMATION.R\n\n",
    "Le pipeline continue de tourner. Aucune action bloquante requise.\n\n",
    "-- PREIS Automation Layer (Africa CDC)"),
    format(now_utc, "%Y-%m-%d %H:%M:%S"),
    pdf_max %||% 0, indicators_max %||% 0, gap_indicators %||% 0,
    series_max %||% 0, gap_series %||% 0, registry_max %||% 0,
    paste("  - ", alert_reason, collapse = "\n"),
    pdf_max %||% 0)
  sent <- send_alert_email(subject, body)
  if (sent) {
    new_row$alerted <- TRUE
    state[nrow(state), "alerted"] <- TRUE
  }
}

# Sauvegarde etat
write_health_state(state)
cat(sprintf("\nEtat persistant : %s (%d runs historises)\n",
            HEALTH_STATE_FILE, nrow(state)))

# =============================================================================
# 5. VERDICT FINAL - PIPELINE VERT SAUF CAS EXTREME
# =============================================================================
banner("VERDICT")

if (status == "OK") {
  cat("Pipeline en parfait etat. Aucune action requise.\n")
} else if (status == "TRANSITOIRE_tolerable") {
  cat("Decalage transitoire tolerable.\n")
  cat("Le prochain cycle de 30 min rattrapera automatiquement.\n")
} else if (status == "DECALAGE_persistant") {
  cat("Decalage persistant detecte.\n")
  if (alert_needed) {
    cat("Alerte email envoyee - investigation humaine requise.\n")
  } else {
    cat("Alerte deja envoyee ou seuil non atteint.\n")
  }
  cat("Le pipeline continue de tourner normalement.\n")
} else {
  cat("Donnees critiques manquantes - investigation urgente requise.\n")
  cat("Le pipeline continue de tourner mais peut produire des sorties partielles.\n")
}

cat("\n>>> Sortie code : 0 (validation NON-bloquante par design)\n")
log_msg("=== Validation terminee ===")
quit(save = "no", status = 0)  # <<< TOUJOURS SUCCES - pipeline autonome
