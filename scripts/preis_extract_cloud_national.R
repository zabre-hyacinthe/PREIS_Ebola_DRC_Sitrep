#!/usr/bin/env Rscript
############################################################
# preis_extract_cloud_national.R
#
# CHAINON MANQUANT DU CLOUD.
# Le monitor (08) telecharge le PDF ; 03_analyse re-pivote
# PREIS_indicators_long.csv ; mais RIEN dans le workflow n'extrait
# les cumuls nationaux du nouveau PDF vers ce fichier (seul le master
# local 00_PREIS_MASTER_AUTOMATION.R le faisait). Resultat : la serie
# cloud reste bloquee au dernier SitRep extrait EN LOCAL, et l'email
# enrichi se bloque (fail-safe) -> run vert, aucun email.
#
# Ce script lit le DERNIER PDF telecharge, extrait cumul cas / deces /
# letalite de facon ROBUSTE (tolere le separateur de milliers exotique
# U+202F / U+2009 / U+00A0 qui cassait N65..N82), et ajoute les 3
# lignes a PREIS_indicators_long.csv pour que 03_analyse fasse avancer
# la serie.
#
# SECURITE (fail-safe, meme philosophie que preis_fix_national_cumul.R) :
#   - n'ecrit QUE si cas ET deces sont trouves, coherents avec le CFR,
#     monotones vs le max existant, et dans une plage plausible ;
#   - sinon n'ecrit RIEN : statu quo, le garde-fou alerte, vous extrayez
#     en local. JAMAIS de chiffre douteux pousse vers les emails.
#
# A lancer dans le workflow APRES 08_cloud_sitrep_monitor.R et AVANT
# 03_analyse_consolidee.R. Idempotent : ne re-ecrit pas un SitRep deja
# present.
############################################################

suppressPackageStartupMessages({
  library(stringr)
})
if (!requireNamespace("pdftools", quietly = TRUE)) {
  cat("[extract-cloud] pdftools indisponible - abandon (aucune ecriture).\n")
  quit(save = "no", status = 0)
}

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
IND_FP  <- file.path(BASE_DIR, "data", "final", "PREIS_indicators_long.csv")
PDF_DIR <- file.path(BASE_DIR, "data", "pdf")

if (!dir.exists(PDF_DIR)) { cat("[extract-cloud] Pas de dossier data/pdf - abandon.\n"); quit(save="no", status=0) }

# ------------------------------------------------------------
# 0) Outils de normalisation / parsing robustes
# ------------------------------------------------------------
# Colle les separateurs de milliers "exotiques" (espace fine / insecable /
# etroite / figure space / word joiner) ENTRE deux chiffres. On ne touche
# PAS a l'espace ASCII normal (separateur de colonnes du tableau).
.norm_num_spaces <- function(x) {
  thin <- "\u00A0\u202F\u2009\u2007\u2060\uFEFF\u2005\u2006"
  pat  <- paste0("([0-9])[", thin, "]+([0-9])")
  repeat {
    x2 <- gsub(pat, "\\1\\2", x, perl = TRUE)
    if (identical(x2, x)) break
    x <- x2
  }
  # espaces divers -> espace simple, puis compaction
  x <- gsub("[\u00A0\u202F\u2009\u2007\u2005\u2006\u2003\u2002]", " ", x, perl = TRUE)
  x <- gsub("[ \t]+", " ", x)
  x
}
.to_int <- function(s) suppressWarnings(as.integer(gsub("[^0-9]", "", s)))
.to_pct <- function(s) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", s))))

# ------------------------------------------------------------
# 1) Trouver le dernier PDF telecharge + son numero
# ------------------------------------------------------------
pdfs <- list.files(PDF_DIR, pattern = "\\.pdf$", full.names = TRUE)
if (length(pdfs) == 0) { cat("[extract-cloud] Aucun PDF - abandon.\n"); quit(save="no", status=0) }
num_of <- function(f) {
  b <- basename(f)
  n1 <- suppressWarnings(as.integer(sub(".*PREIS_DRC_Ebola_SitRep_0*([0-9]{1,3})\\.pdf$", "\\1", b)))
  n2 <- suppressWarnings(as.integer(sub(".*SitRep_0*([0-9]{1,3})_2026\\.pdf$", "\\1", b)))
  ifelse(!is.na(n1), n1, n2)
}
nums <- vapply(pdfs, num_of, integer(1))
ok <- !is.na(nums)
if (!any(ok)) { cat("[extract-cloud] Aucun PDF nomme reconnaissable - abandon.\n"); quit(save="no", status=0) }
target_no  <- max(nums[ok])
target_pdf <- pdfs[ok][which.max(nums[ok])]
cat(sprintf("[extract-cloud] Cible : SitRep %d (%s)\n", target_no, basename(target_pdf)))

# ------------------------------------------------------------
# 2) Charger l'existant + idempotence + max courant
# ------------------------------------------------------------
cols <- c("sitrep_no","indicator_code","domain","value",
          "extraction_rule","value_source","source_type","priority")
ind <- if (file.exists(IND_FP)) {
  tryCatch(utils::read.csv(IND_FP, stringsAsFactors = FALSE, colClasses = "character"),
           error = function(e) NULL)
} else NULL
if (is.null(ind)) { cat("[extract-cloud] PREIS_indicators_long.csv illisible - abandon (aucune ecriture).\n"); quit(save="no", status=0) }
for (nm in cols) if (!nm %in% names(ind)) ind[[nm]] <- NA_character_

.cc <- ind[ind$indicator_code == "cumulative_confirmed_cases", , drop = FALSE]
.cc$sno <- suppressWarnings(as.integer(.cc$sitrep_no))
.cc$val <- suppressWarnings(as.numeric(.cc$value))
already <- target_no %in% .cc$sno[!is.na(.cc$sno)]
if (already) {
  cat(sprintf("[extract-cloud] SitRep %d deja present dans les indicateurs - rien a faire.\n", target_no))
  quit(save = "no", status = 0)
}
prev_max_cases <- suppressWarnings(max(.cc$val[!is.na(.cc$val) & .cc$sno < target_no], na.rm = TRUE))
if (!is.finite(prev_max_cases)) prev_max_cases <- 0

# ------------------------------------------------------------
# 3) Lire le PDF + extraire les cumuls nationaux (robuste)
# ------------------------------------------------------------
pages <- tryCatch(pdftools::pdf_text(target_pdf), error = function(e) character())
if (length(pages) == 0) { cat("[extract-cloud] PDF illisible - abandon.\n"); quit(save="no", status=0) }
# Valider que le PDF porte bien ce numero (evite un mauvais fichier)
head_txt <- .norm_num_spaces(paste(pages[seq_len(min(2, length(pages)))], collapse = " "))
if (!str_detect(head_txt, regex(sprintf("SitRep\\s*N\\s*[\u00b0\u00bao]?\\s*0*%d\\b", target_no), ignore_case = TRUE))) {
  cat(sprintf("[extract-cloud] Le PDF ne confirme pas 'SitRep N%d' en en-tete - abandon par prudence.\n", target_no))
  quit(save = "no", status = 0)
}
T <- .norm_num_spaces(paste(pages, collapse = " \n "))

cas <- NA_integer_; dec <- NA_integer_; cfr <- NA_real_; rule <- NA_character_

## Strategie 1 : ligne "Total <cas> <deces> <cfr>%"
m <- str_match(T, regex("Total\\s+([0-9]{3,6})\\s+([0-9]{2,5})\\s+([0-9]{1,3}(?:[.,][0-9]+)?)\\s*%",
                        ignore_case = TRUE))
if (!is.na(m[1,2])) { cas <- .to_int(m[1,2]); dec <- .to_int(m[1,3]); cfr <- .to_pct(m[1,4]); rule <- "cloud_total_row" }

## Strategie 2 : narratif "cumul national ... X cas ... et Y deces"
if (is.na(cas) || is.na(dec)) {
  m2 <- str_match(T, regex("cumul\\s+national[^0-9]{0,25}([0-9]{3,6})\\s*cas[^0-9]{0,25}([0-9]{2,5})\\s*d[e\u00e9]c",
                           ignore_case = TRUE))
  if (!is.na(m2[1,2])) { cas <- .to_int(m2[1,2]); dec <- .to_int(m2[1,3]); rule <- "cloud_narr_cumul_national" }
}

## Strategie 3 : formes espacees "X cas confirmes" + "Y deces"
if (is.na(cas)) {
  mc <- str_match(T, regex("([0-9]{3,6})\\s*cas\\s+confirm", ignore_case = TRUE))
  if (!is.na(mc[1,2])) { cas <- .to_int(mc[1,2]); if (is.na(rule)) rule <- "cloud_spaced_box" }
}
if (is.na(dec)) {
  md <- str_match(T, regex("([0-9]{2,5})\\s*d[e\u00e9]c[e\u00e8]s\\s+confirm", ignore_case = TRUE))
  if (!is.na(md[1,2])) { dec <- .to_int(md[1,2]); if (is.na(rule)) rule <- "cloud_spaced_box" }
}

## CFR : explicite si trouve, sinon calcule
if (is.na(cfr)) {
  mcfr <- str_match(T, regex("l[e\u00e9]talit[e\u00e9][^0-9]{0,15}([0-9]{1,3}(?:[.,][0-9]+)?)\\s*%", ignore_case = TRUE))
  if (!is.na(mcfr[1,2])) cfr <- .to_pct(mcfr[1,2])
}
if (is.na(cfr)) {
  mcfr2 <- str_match(T, regex("\\(\\s*([0-9]{1,3}[.,][0-9]+)\\s*%\\s*\\)"))  # ex "(39,7%)"
  if (!is.na(mcfr2[1,2])) cfr <- .to_pct(mcfr2[1,2])
}
implied_cfr <- if (!is.na(cas) && !is.na(dec) && cas > 0) 100 * dec / cas else NA_real_
if (is.na(cfr)) cfr <- if (!is.na(implied_cfr)) round(implied_cfr, 1) else NA_real_

cat(sprintf("[extract-cloud] Brut : cas=%s deces=%s cfr=%s (regle=%s)\n",
            cas, dec, ifelse(is.na(cfr),"NA",cfr), ifelse(is.na(rule),"NA",rule)))

# ------------------------------------------------------------
# 4) FILTRE DE CONFIANCE (fail-safe) : n'ecrire que si tout est coherent
# ------------------------------------------------------------
reject <- function(why) { cat("[extract-cloud] ABSTENTION (aucune ecriture) : ", why, "\n", sep=""); quit(save="no", status=0) }

if (is.na(cas) || is.na(dec))                 reject("cas ou deces introuvable")
if (cas < 100 || dec < 1)                     reject("valeurs trop faibles / implausibles")
if (dec > cas)                                reject("deces > cas (incoherent)")
if (!is.na(implied_cfr) && (implied_cfr < 5 || implied_cfr > 90)) reject(sprintf("CFR implicite hors plage (%.1f%%)", implied_cfr))
if (!is.na(cfr) && !is.na(implied_cfr) && abs(cfr - implied_cfr) > 3) reject(sprintf("CFR affiche (%.1f) incoherent avec deces/cas (%.1f)", cfr, implied_cfr))
if (cas + 1e-9 < prev_max_cases)              reject(sprintf("cumul cas (%d) < max existant (%.0f) : non monotone", cas, prev_max_cases))
if (prev_max_cases > 0 && cas > prev_max_cases * 1.6) reject(sprintf("saut de cumul suspect : %d vs max %.0f", cas, prev_max_cases))

# ------------------------------------------------------------
# 5) Ecriture : 3 lignes ajoutees a PREIS_indicators_long.csv
# ------------------------------------------------------------
new_rows <- data.frame(
  sitrep_no      = as.character(rep(target_no, 3)),
  indicator_code = c("cumulative_confirmed_cases","cumulative_deaths","case_fatality_ratio"),
  domain         = c("cases","deaths","deaths"),
  value          = c(as.character(cas), as.character(dec),
                     as.character(ifelse(is.na(cfr), round(implied_cfr,1), cfr))),
  extraction_rule = rep(ifelse(is.na(rule), "cloud_extract", rule), 3),
  value_source    = rep("observed", 3),
  source_type     = rep("cloud_pdf", 3),
  priority        = rep("1", 3),
  stringsAsFactors = FALSE
)
ind2 <- rbind(ind[, cols, drop = FALSE], new_rows[, cols, drop = FALSE])
tryCatch({
  utils::write.csv(ind2, IND_FP, row.names = FALSE, na = "NA")
  cat(sprintf("[extract-cloud] ECRIT : SitRep %d -> cas=%d deces=%d cfr=%s (%s)\n",
              target_no, cas, dec, ifelse(is.na(cfr), round(implied_cfr,1), cfr), rule))
}, error = function(e) {
  cat("[extract-cloud] Echec ecriture : ", conditionMessage(e), " (aucune modification)\n", sep="")
  quit(save = "no", status = 0)
})
