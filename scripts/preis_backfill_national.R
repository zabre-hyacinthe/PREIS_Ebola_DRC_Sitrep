#############################################################
# preis_backfill_national.R
# Complete les CUMULS NATIONAUX (cas / deces / letalite) MANQUANTS dans
# data/final/PREIS_indicators_long.csv en les lisant dans les PDF de data/pdf
# via l'extraction robuste (repli du correctif). Ne touche jamais une valeur
# deja presente ; garde de plausibilite ; sauvegarde .BAK avant d'ecrire.
#   Rscript scripts/preis_backfill_national.R
#   Rscript scripts/03_analyse_consolidee.R
#   Rscript scripts/12_validate_pipeline_freshness.R
#############################################################
suppressPackageStartupMessages(library(pdftools))

# ---- fonction de repli (copie EXACTE du correctif) ----
national_cumul_fallback <- function(lines) {
  norm <- function(s) {
    s <- enc2utf8(as.character(s))
    # neutralise les separateurs de milliers "espaces exotiques" (NBSP,
    # narrow-NBSP U+202F, thin/figure/hair spaces...) qui cassent la regex
    s <- gsub("[\u00A0\u2007\u2008\u2009\u200A\u202F\u205F]", " ", s, perl = TRUE)
    s <- gsub("\\*", "", s)
    s <- gsub("[[:space:]]+", " ", s)
    trimws(s)
  }
  ci  <- function(x) suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))
  cp  <- function(x) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", x))))
  NUM <- "([0-9]{1,3}(?: [0-9]{3})+|[0-9]{1,6})"
  t   <- norm(paste(lines, collapse = " "))
  cases <- NA_real_; deaths <- NA_real_; cfr <- NA_real_; rule <- NA_character_

  grp <- function(pat, s = t) {
    r <- regmatches(s, regexec(pat, s, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(r) == 0) NULL else r
  }

  # A) Ligne "Total <cas> <deces> <cfr>% <n> sur"
  r <- grp(paste0("\\bTotal\\s+", NUM, "\\s+", NUM,
                  "\\s+([0-9]+(?:[.,][0-9]+)?)\\s*%\\s+[0-9]+\\s+sur"))
  if (!is.null(r)) { cases <- ci(r[2]); deaths <- ci(r[3]); cfr <- cp(r[4]); rule <- "total_row" }

  # B) Encadre etiquete
  if (is.na(cases)) {
    r <- grp(paste0("CAS\\s+CONFIRM\\w*[^/]{0,40}/\\s*", NUM))
    if (!is.null(r)) { cases <- ci(r[2]); if (is.na(rule)) rule <- "box" }
  }
  if (is.na(deaths)) {
    r <- grp(paste0("D[EÉ]C[EÈ]S\\s+CONFIRM\\w*[^/]{0,40}/\\s*", NUM))
    if (!is.null(r)) { deaths <- ci(r[2]); if (is.na(rule)) rule <- "box" }
  }

  # C) Ratio (deces / cas) recoupe a la letalite globale
  if (is.na(cases) || is.na(deaths)) {
    cfr_ref <- NA_real_
    rc <- grp("l[eé]talit[eé]\\s+globale[^%(]*?([0-9]+[.,][0-9]+)\\s*%")
    if (!is.null(rc)) cfr_ref <- cp(rc[2])
    par <- paste0("\\(\\s*", NUM, "\\s*/\\s*", NUM, "\\s*\\)")
    hits <- regmatches(t, gregexpr(par, t, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(hits) > 0) {
      aa <- c(); bb <- c()
      for (h in hits) {
        rr <- regmatches(h, regexec(par, h, perl = TRUE))[[1]]
        aa <- c(aa, ci(rr[2])); bb <- c(bb, ci(rr[3]))
      }
      keep <- !is.na(aa) & !is.na(bb) & bb > aa & bb > 100 & (aa / bb) >= 0.02 & (aa / bb) <= 0.95
      aa <- aa[keep]; bb <- bb[keep]
      if (length(bb) > 0) {
        if (!is.na(cfr_ref)) {
          k <- which.min(abs(100 * aa / bb - cfr_ref))
          if (length(k) == 1 && abs(100 * aa[k] / bb[k] - cfr_ref) <= 3) {
            deaths <- aa[k]; cases <- bb[k]; rule <- "ratio_cfr" }
        }
        if (is.na(cases)) { k <- which.max(bb); deaths <- aa[k]; cases <- bb[k]; if (is.na(rule)) rule <- "ratio" }
      }
    }
  }
  if (is.na(cfr) && !is.na(cases) && !is.na(deaths) && cases > 0) cfr <- round(100 * deaths / cases, 1)
  list(cases = cases, deaths = deaths, cfr = cfr, rule = rule)
}

internal_no <- function(f) {
  txt <- tryCatch(pdftools::pdf_text(f), error = function(e) character())
  if (length(txt) == 0) return(NA_integer_)
  hdr <- paste(utils::head(txt, 2), collapse = " ")
  m <- regmatches(hdr, regexec("SitRep\\s*N\\s*[°ºo]?\\s*0*([0-9]{1,3})", hdr, perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(m) >= 2) as.integer(m[2]) else NA_integer_
}

IND <- file.path("data", "final", "PREIS_indicators_long.csv")
if (!file.exists(IND)) stop("Introuvable: ", IND, " (lancez depuis la racine du projet)")
ind <- as.data.frame(read.csv(IND, stringsAsFactors = FALSE, check.names = FALSE))
ind$sitrep_no <- suppressWarnings(as.integer(ind$sitrep_no))

bak <- paste0(IND, ".BAK_", format(Sys.time(), "%Y%m%d_%H%M%S"))
write.csv(ind, bak, row.names = FALSE, na = "")
cat("Sauvegarde:", bak, "\n")

has_code <- function(df, sn, code) any(df$sitrep_no == sn & df$indicator_code == code, na.rm = TRUE)
add_row <- function(df, sn, code, val, rule) {
  if (is.na(val)) return(df)
  r <- df[1, ]; r[, ] <- NA
  r$sitrep_no <- as.integer(sn); r$indicator_code <- code
  if ("value" %in% names(df))           r$value <- as.numeric(val)
  if ("domain" %in% names(df))          r$domain <- "epidemiology"
  if ("extraction_rule" %in% names(df)) r$extraction_rule <- rule
  if ("value_source" %in% names(df))    r$value_source <- "observed"
  if ("source_type" %in% names(df))     r$source_type <- "pdf_backfill"
  if ("priority" %in% names(df))        r$priority <- 1L
  rbind(df, r)
}

pdfs <- list.files("data/pdf", pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
added <- integer(0)
for (f in pdfs) {
  sn <- internal_no(f)
  if (is.na(sn) || sn < 1 || sn > 300) next
  need_cases  <- !has_code(ind, sn, "cumulative_confirmed_cases")
  need_deaths <- !has_code(ind, sn, "cumulative_deaths")
  if (!need_cases && !need_deaths) next
  txt <- tryCatch(pdftools::pdf_text(f), error = function(e) character())
  if (length(txt) == 0) next
  fb <- national_cumul_fallback(unlist(strsplit(txt, "\n")))
  ok <- !is.na(fb$cases) && !is.na(fb$deaths) && fb$cases > 0 && fb$deaths >= 0 &&
        fb$deaths <= fb$cases && (fb$deaths / fb$cases) >= 0.02 && (fb$deaths / fb$cases) <= 0.95
  if (!ok) { cat(sprintf("SitRep %d : cumul non extrait (ignore)\n", sn)); next }
  if (need_cases)  ind <- add_row(ind, sn, "cumulative_confirmed_cases", fb$cases,  paste0("backfill_", fb$rule))
  if (need_deaths) ind <- add_row(ind, sn, "cumulative_deaths",          fb$deaths, paste0("backfill_", fb$rule))
  if (!has_code(ind, sn, "case_fatality_ratio") && !is.na(fb$cfr))
    ind <- add_row(ind, sn, "case_fatality_ratio", fb$cfr, paste0("backfill_", fb$rule))
  cat(sprintf("SitRep %d complete : cas=%s deces=%s cfr=%s (%s)\n", sn, fb$cases, fb$deaths, fb$cfr, fb$rule))
  added <- c(added, sn)
}

if (length(added) == 0) {
  cat("\nRien a completer (cumuls nationaux deja tous presents).\n")
} else {
  ind <- ind[order(ind$sitrep_no, ind$indicator_code), ]
  write.csv(ind, IND, row.names = FALSE, na = "")
  cat(sprintf("\nSitReps completes : %s\n", paste(sort(unique(added)), collapse = ", ")))
  cat(sprintf("Fichier reecrit : %s (%d lignes)\n", IND, nrow(ind)))
}
cat("\nEtapes suivantes :\n  Rscript scripts/03_analyse_consolidee.R\n  Rscript scripts/12_validate_pipeline_freshness.R\n")
