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
  stop(structure(class = c("preis_abstention", "condition"),
                 list(message = why, call = NULL)))
}

# ------------------------------------------------------------
# 1) Numero du dernier SitRep : dernier PDF telecharge, sinon monitor state
# ------------------------------------------------------------
target_no <- NA_integer_
all_nn <- integer(0)
if (dir.exists(PDF_DIR)) {
  pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$")
  n1 <- suppressWarnings(as.integer(sub(".*PREIS_DRC_Ebola_SitRep_0*([0-9]{1,3})\\.pdf$", "\\1", pdfs)))
  n2 <- suppressWarnings(as.integer(sub(".*SitRep_0*([0-9]{1,3})_2026\\.pdf$", "\\1", pdfs)))
  nn <- c(n1, n2); nn <- nn[!is.na(nn)]
  all_nn <- sort(unique(nn))
  if (length(nn)) target_no <- max(nn)
}
if (is.na(target_no) && file.exists(STATE_FP)) {
  st <- tryCatch(utils::read.csv(STATE_FP, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(st) && "sitrep_no" %in% names(st)) {
    v <- suppressWarnings(as.integer(st$sitrep_no)); v <- v[!is.na(v)]
    if (length(v)) target_no <- max(v)
  }
}
if (is.na(target_no)) { cat("[extract-cloud] ABSTENTION (aucune ecriture) : impossible de determiner le dernier SitRep\n"); quit(save = "no", status = 0) }
cat(sprintf("[extract-cloud] Cible : SitRep %d\n", target_no))

# ------------------------------------------------------------
# 1b) CORRECTIF 14/10/2026 : traiter TOUS les SitReps manquants (ordre croissant)
# Avant : seule la cible = PDF au numero le plus eleve etait essayee. Si la source INRB
# avait un jour de retard sur ce numero (ex. cible 151 = 12/10, INRB s'arrete au 11/10),
# le script s'abstenait et le SitRep 150 (INRB disponible au 11/10) n'etait JAMAIS essaye.
# Maintenant : chaque numero > dernier numero present est essaye separement, avec les
# MEMES garde-fous (date exacte, monotonie, coherence). Un refus n'empeche pas le suivant ;
# un numero refuse sera retente au cycle suivant (idempotent).
# ------------------------------------------------------------
.cur <- if (file.exists(IND_FP)) tryCatch(utils::read.csv(IND_FP, stringsAsFactors = FALSE, colClasses = "character"), error = function(e) NULL) else NULL
.cur_max <- 0L
if (!is.null(.cur) && all(c("sitrep_no", "indicator_code") %in% names(.cur))) {
  .v <- suppressWarnings(as.integer(.cur$sitrep_no[.cur$indicator_code == "cumulative_confirmed_cases"]))
  .v <- .v[!is.na(.v)]
  if (length(.v)) .cur_max <- max(.v)
}
targets <- all_nn[all_nn > .cur_max & all_nn >= 14L]
if (!length(targets)) targets <- target_no
cat(sprintf("[extract-cloud] SitReps a traiter (ordre croissant) : %s\n", paste(targets, collapse = ", ")))

# ------------------------------------------------------------
# 1c) REPLI PDF STRICT (14/10/2026) : quand la source INRB a du retard sur la date du SitRep.
# Le bandeau de la page 1 du PDF officiel (cumul cas / deces / letalite) n'est accepte QUE si
# TOUTES ces conditions sont vraies, sinon abstention (aucun chiffre douteux) :
#   - l'en-tete du PDF porte bien le numero cible (SitRep N°<n>) ;
#   - le SitRep precedent (n-1) est deja present dans la base (serie continue) ;
#   - cumul cas = cumul cas du SitRep n-1 + « nouveaux cas confirmes » du texte (egalite exacte) ;
#   - cumul deces >= celui du SitRep n-1 et <= cas ; letalite du bandeau = deces/cas (+-0,15) ;
#   - plage de plausibilite habituelle (CFR 5-90 %).
# Desactivable : PREIS_PDF_FALLBACK=0.
# ------------------------------------------------------------
pdf_banner_crosscheck <- function(target_no, ind) {
  if (identical(Sys.getenv("PREIS_PDF_FALLBACK", "1"), "0")) return(NULL)
  fn <- list.files(PDF_DIR, full.names = TRUE)
  fn <- fn[grepl(sprintf("(PREIS_DRC_Ebola_SitRep_0*%d\\.pdf$|SitRep_0*%d_2026\\.pdf$)", target_no, target_no), fn)]
  if (!length(fn)) return(NULL)
  txt <- tryCatch({
    if (requireNamespace("pdftools", quietly = TRUE)) pdftools::pdf_text(fn[1])[1]
    else paste(system2("pdftotext", c("-layout", "-f", "1", "-l", "1", shQuote(fn[1]), "-"), stdout = TRUE), collapse = "\n")
  }, error = function(e) NA_character_)
  if (is.na(txt) || !nzchar(txt)) return(NULL)
  hdr <- regmatches(txt, regexpr("N\u00b0\\s*[0-9]{1,3}\\s*/\\s*MVE", txt))
  if (!length(hdr) || as.integer(gsub("[^0-9]", "", sub("/.*", "", hdr))) != target_no) return(NULL)
  lines <- strsplit(txt, "\n")[[1]]
  pat <- "^\\s*([0-9][0-9 ]*?)\\s{2,}([0-9][0-9 ]*?)\\s{2,}([0-9]+[,.][0-9])\\s*%"
  hit <- regmatches(lines, regexec(pat, lines)); hit <- hit[lengths(hit) == 4]
  if (!length(hit)) return(NULL)
  num <- function(x) as.numeric(gsub("[^0-9]", "", x))
  cas <- num(hit[[1]][2]); dec <- num(hit[[1]][3]); cfr <- as.numeric(sub(",", ".", hit[[1]][4]))
  flat <- gsub("\\s+", " ", txt)
  m <- regmatches(flat, regexec("\\(([0-9]{1,3})\\) nouveaux cas|([0-9]{1,3}) nouveaux cas confirm", flat))[[1]]
  newc <- suppressWarnings(as.numeric(c(m[2], m[3])[nzchar(c(m[2], m[3]))][1]))
  if (is.na(cas) || is.na(dec) || is.na(cfr) || is.na(newc)) return(NULL)
  sn <- suppressWarnings(as.integer(ind$sitrep_no)); vv <- suppressWarnings(as.numeric(ind$value))
  pc <- vv[!is.na(sn) & sn == target_no - 1L & ind$indicator_code == "cumulative_confirmed_cases"]
  pd <- vv[!is.na(sn) & sn == target_no - 1L & ind$indicator_code == "cumulative_deaths"]
  if (!length(pc) || !length(pd)) return(NULL)
  if (cas != pc[1] + newc) { cat(sprintf("[extract-cloud] repli PDF refuse : bandeau %d != precedent %d + nouveaux %d\n", cas, pc[1], newc)); return(NULL) }
  if (dec < pd[1] || dec > cas) return(NULL)
  if (abs(round(100 * dec / cas, 1) - cfr) > 0.15) return(NULL)
  list(cas = as.integer(cas), dec = as.integer(dec), cfr = cfr)
}

process_target <- function(target_no) {
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
    cat(sprintf("[extract-cloud] SitRep %d deja present - rien a faire.\n", target_no)); return(invisible("present"))
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
  pdf_vals <- NULL
  row <- nat[nat$date == target_date, , drop = FALSE]
  if (nrow(row) == 0) {
    cand <- nat[nat$date <= target_date, , drop = FALSE]
    if (nrow(cand) == 0) reject(sprintf("aucune donnee INRB a/avant %s", target_date))
    row <- cand[nrow(cand), , drop = FALSE]
    lag_days <- as.numeric(target_date - row$date)
    # Tolerance par defaut 0 jour : jamais de total d'un autre jour sous le numero d'un SitRep
    # (cas du SitRep 145 : total du 05/10 affiche comme celui du 06/10). Variable pour ajuster.
    max_lag <- suppressWarnings(as.numeric(Sys.getenv("PREIS_INRB_MAX_LAG_DAYS", "0")))
    if (is.na(max_lag)) max_lag <- 0
    if (lag_days > max_lag) {
      pdf_vals <- pdf_banner_crosscheck(target_no, ind)
      if (is.null(pdf_vals)) reject(sprintf("source INRB en retard de %d j (dernier=%s, cible=%s) et repli PDF non valide",
                                            as.integer(lag_days), row$date, target_date))
      cat(sprintf("[extract-cloud] INRB en retard de %d j : repli PDF strict accepte (egalite exacte avec n-1 + nouveaux cas).\n", as.integer(lag_days)))
    }
    if (is.null(pdf_vals)) cat(sprintf("[extract-cloud] Date exacte absente ; utilise le point INRB du %s (%d j avant).\n",
                row$date, as.integer(lag_days)))
  }

  cas <- as.integer(round(as.numeric(row$cum_cases[1])))
  dec <- as.integer(round(as.numeric(row$cum_deaths[1])))
  cfr <- if ("cfr" %in% names(row) && !is.na(suppressWarnings(as.numeric(row$cfr[1])))) {
    round(as.numeric(row$cfr[1]), 1)
  } else if (!is.na(cas) && cas > 0) round(100 * dec / cas, 1) else NA_real_

  src_rule <- "cloud_inrb_daily"; src_type <- "inrb_validated"
  if (!is.null(pdf_vals)) { cas <- pdf_vals$cas; dec <- pdf_vals$dec; cfr <- pdf_vals$cfr
                            src_rule <- "pdf_banner_xcheck"; src_type <- "sitrep_pdf_xcheck" }
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
    extraction_rule = rep(src_rule, 3),
    value_source    = rep("observed", 3),
    source_type     = rep(src_type, 3),
    priority        = rep("1", 3),
    stringsAsFactors = FALSE
  )
  ind2 <- rbind(ind[, cols, drop = FALSE], new_rows[, cols, drop = FALSE])
  tryCatch({
    utils::write.csv(ind2, IND_FP, row.names = FALSE, na = "NA")
    cat(sprintf("[extract-cloud] ECRIT : SitRep %d -> cas=%d deces=%d cfr=%.1f (source : %s)\n",
                target_no, cas, dec, cfr, src_type))
  }, error = function(e) {
    cat("[extract-cloud] Echec ecriture : ", conditionMessage(e), " (aucune modification)\n", sep = "")
    stop(structure(class = c("preis_abstention", "condition"), list(message = "echec ecriture", call = NULL)))
  })
}

for (.t in targets) {
  cat(sprintf("[extract-cloud] --- SitRep %d ---\n", .t))
  tryCatch(process_target(.t), preis_abstention = function(e) invisible(NULL))
}
