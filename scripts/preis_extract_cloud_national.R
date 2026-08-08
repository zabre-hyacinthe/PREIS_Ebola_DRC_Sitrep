#!/usr/bin/env Rscript
############################################################
# preis_extract_cloud_national.R  (v2 - SANS pdftools/poppler)
#
# CHAINON MANQUANT DU CLOUD, version robuste sans dependance systeme.
#
# Probleme de la v1 : elle lisait le PDF via pdftools, qui exige la lib
# systeme poppler (libpoppler-cpp-dev) absente du runner -> abandon
# systematique ("pdftools indisponible").
#
# v2 : on ne lit PLUS le PDF. On prend les cumuls nationaux dans la
# SOURCE INRB VALIDEE, deja telechargee et ecrite par 11_daily_indicators.R
# dans le meme run (data/final/PREIS_daily_indicators.csv, lignes level=National).
# -> aucune analyse PDF, aucune lib systeme, base R uniquement.
# -> conforme a votre regle : "totaux nationaux = source INRB validee".
#
# On mappe le dernier SitRep telecharge a sa DATE (regle d'ancrage :
# SitRep >= 14 = 1 par jour depuis le 28/05/2026), on lit le cumul
# national INRB de cette date, et on ajoute 3 lignes a
# PREIS_indicators_long.csv pour que 03_analyse fasse avancer la serie.
#
# SECURITE (fail-safe) : n'ecrit QUE si coherent (deces<=cas, CFR plausible),
# monotone vs le max existant, et si la source INRB n'est pas trop en retard
# sur la date cible. Sinon : ABSTENTION (aucune ecriture) -> le garde-fou
# alerte, vous extrayez en local. JAMAIS de chiffre douteux.
#
# A lancer APRES 11_daily_indicators.R et AVANT 03_analyse_consolidee.R.
# Idempotent : ne re-ecrit pas un SitRep deja present.
############################################################

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
IND_FP   <- file.path(BASE_DIR, "data", "final", "PREIS_indicators_long.csv")
DAILY_FP <- file.path(BASE_DIR, "data", "final", "PREIS_daily_indicators.csv")
PDF_DIR  <- file.path(BASE_DIR, "data", "pdf")
STATE_FP <- file.path(BASE_DIR, "data", "monitor_state", "preis_sitrep_email_state.csv")

reject <- function(why) {
  cat("[extract-cloud] ABSTENTION (aucune ecriture) : ", why, "\n", sep = "")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------
# 1) Numero du dernier SitRep : dernier PDF telecharge, sinon monitor state
# ------------------------------------------------------------
target_no <- NA_integer_
if (dir.exists(PDF_DIR)) {
  pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$")
  n1 <- suppressWarnings(as.integer(sub(".*PREIS_DRC_Ebola_SitRep_0*([0-9]{1,3})\\.pdf$", "\\1", pdfs)))
  n2 <- suppressWarnings(as.integer(sub(".*SitRep_0*([0-9]{1,3})_2026\\.pdf$", "\\1", pdfs)))
  nn <- c(n1, n2); nn <- nn[!is.na(nn)]
  if (length(nn)) target_no <- max(nn)
}
if (is.na(target_no) && file.exists(STATE_FP)) {
  st <- tryCatch(utils::read.csv(STATE_FP, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(st) && "sitrep_no" %in% names(st)) {
    v <- suppressWarnings(as.integer(st$sitrep_no)); v <- v[!is.na(v)]
    if (length(v)) target_no <- max(v)
  }
}
if (is.na(target_no)) reject("impossible de determiner le dernier SitRep")
cat(sprintf("[extract-cloud] Cible : SitRep %d\n", target_no))

# ------------------------------------------------------------
# 2) Idempotence + max existant
# ------------------------------------------------------------
cols <- c("sitrep_no","indicator_code","domain","value",
          "extraction_rule","value_source","source_type","priority")
ind <- if (file.exists(IND_FP)) {
  tryCatch(utils::read.csv(IND_FP, stringsAsFactors = FALSE, colClasses = "character"),
           error = function(e) NULL)
} else NULL
if (is.null(ind)) reject("PREIS_indicators_long.csv illisible")
for (nm in cols) if (!nm %in% names(ind)) ind[[nm]] <- NA_character_

.cc <- ind[ind$indicator_code == "cumulative_confirmed_cases", , drop = FALSE]
.cc$sno <- suppressWarnings(as.integer(.cc$sitrep_no))
.cc$val <- suppressWarnings(as.numeric(.cc$value))
if (target_no %in% .cc$sno[!is.na(.cc$sno)]) {
  cat(sprintf("[extract-cloud] SitRep %d deja present - rien a faire.\n", target_no)); quit(save = "no", status = 0)
}
prev_max <- suppressWarnings(max(.cc$val[!is.na(.cc$val) & .cc$sno < target_no], na.rm = TRUE))
if (!is.finite(prev_max)) prev_max <- 0

# ------------------------------------------------------------
# 3) Date cible (regle d'ancrage) + cumuls nationaux INRB valides
# ------------------------------------------------------------
target_date <- if (target_no >= 14L) as.Date("2026-05-28") + (target_no - 14L) else NA
if (is.na(target_date)) reject("SitRep anterieur a l'ancrage (deja historise)")

if (!file.exists(DAILY_FP)) reject("PREIS_daily_indicators.csv absent (11_daily a echoue ?)")
d <- tryCatch(utils::read.csv(DAILY_FP, stringsAsFactors = FALSE), error = function(e) NULL)
if (is.null(d) || !all(c("level","date","cum_cases","cum_deaths") %in% names(d))) reject("daily indicators invalide")

nat <- d[d$level == "National", , drop = FALSE]
nat$date <- as.Date(nat$date)
nat <- nat[!is.na(nat$date) & !is.na(suppressWarnings(as.numeric(nat$cum_cases))), , drop = FALSE]
if (nrow(nat) == 0) reject("aucune ligne National dans les indicateurs journaliers")
nat <- nat[order(nat$date), , drop = FALSE]

# Ligne exacte a la date cible, sinon la plus recente <= cible (si pas trop vieille)
row <- nat[nat$date == target_date, , drop = FALSE]
if (nrow(row) == 0) {
  cand <- nat[nat$date <= target_date, , drop = FALSE]
  if (nrow(cand) == 0) reject(sprintf("aucune donnee INRB a/avant %s", target_date))
  row <- cand[nrow(cand), , drop = FALSE]
  lag_days <- as.numeric(target_date - row$date)
  if (lag_days > 3) reject(sprintf("source INRB en retard de %d j (dernier=%s, cible=%s)",
                                    as.integer(lag_days), row$date, target_date))
  cat(sprintf("[extract-cloud] Date exacte absente ; utilise le point INRB du %s (%d j avant).\n",
              row$date, as.integer(lag_days)))
}

cas <- as.integer(round(as.numeric(row$cum_cases[1])))
dec <- as.integer(round(as.numeric(row$cum_deaths[1])))
cfr <- if ("cfr" %in% names(row) && !is.na(suppressWarnings(as.numeric(row$cfr[1])))) {
  round(as.numeric(row$cfr[1]), 1)
} else if (!is.na(cas) && cas > 0) round(100 * dec / cas, 1) else NA_real_

cat(sprintf("[extract-cloud] INRB %s : cas=%s deces=%s cfr=%s\n",
            as.character(target_date), cas, dec, ifelse(is.na(cfr), "NA", cfr)))

# ------------------------------------------------------------
# 4) Filtre de confiance (fail-safe)
# ------------------------------------------------------------
if (is.na(cas) || is.na(dec)) reject("cas ou deces manquant")
if (cas < 100 || dec < 1)     reject("valeurs trop faibles / implausibles")
if (dec > cas)                reject("deces > cas (incoherent)")
implied <- 100 * dec / cas
if (implied < 5 || implied > 90) reject(sprintf("CFR hors plage (%.1f%%)", implied))
if (cas + 1e-9 < prev_max)    reject(sprintf("cumul cas (%d) < max existant (%.0f) : non monotone", cas, prev_max))
if (prev_max > 0 && cas > prev_max * 1.6) reject(sprintf("saut de cumul suspect : %d vs max %.0f", cas, prev_max))
if (is.na(cfr)) cfr <- round(implied, 1)

# ------------------------------------------------------------
# 5) Ecriture : 3 lignes ajoutees a PREIS_indicators_long.csv
# ------------------------------------------------------------
new_rows <- data.frame(
  sitrep_no      = as.character(rep(target_no, 3)),
  indicator_code = c("cumulative_confirmed_cases","cumulative_deaths","case_fatality_ratio"),
  domain         = c("cases","deaths","deaths"),
  value          = c(as.character(cas), as.character(dec), as.character(cfr)),
  extraction_rule = rep("cloud_inrb_daily", 3),
  value_source    = rep("observed", 3),
  source_type     = rep("inrb_validated", 3),
  priority        = rep("1", 3),
  stringsAsFactors = FALSE
)
ind2 <- rbind(ind[, cols, drop = FALSE], new_rows[, cols, drop = FALSE])
tryCatch({
  utils::write.csv(ind2, IND_FP, row.names = FALSE, na = "NA")
  cat(sprintf("[extract-cloud] ECRIT : SitRep %d -> cas=%d deces=%d cfr=%.1f (source INRB validee)\n",
              target_no, cas, dec, cfr))
}, error = function(e) {
  cat("[extract-cloud] Echec ecriture : ", conditionMessage(e), " (aucune modification)\n", sep = "")
  quit(save = "no", status = 0)
})
