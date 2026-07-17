############################################################
# preis_fix_national_cumul.R  (v2 - extraction MULTI-FORMAT robuste)
#
# Re-extrait le CUMUL NATIONAL (cas / deces / letalite) depuis les PDF
# officiels (data/pdf/) et corrige PREIS_indicators_long.csv, sans rien
# inventer. Robuste aux VARIATIONS de format entre SitReps :
#
#   Strategie 1 (primaire) : ratio "(deces / cas)" entre parentheses,
#     RECOUPE avec la letalite affichee (ignore les autres ratios comme
#     le suivi des contacts "10 195 / 12 693").
#   Strategie 2 : note de reconciliation "= X cas ; ... = Y deces".
#   Strategie 3 (secours) : ligne "Total <cas> <deces> <cfr>% .. /140 ..".
#   CFR : "letalite globale [de] X %" sinon 100*deces/cas.
#
# Verifie sur SR60 (2011/754/37,5) ET SR62 (2124/828/39,0).
#
# Usage (console R, racine du projet) :
#   source("preis_fix_national_cumul.R")
# Puis : source("scripts/03_analyse_consolidee.R")
############################################################

suppressPackageStartupMessages({
  library(pdftools); library(stringr); library(dplyr); library(readr); library(tibble)
})

preis_fix_national_cumul <- function(root = NULL,
                                     pdf_dir = "data/pdf",
                                     ind_rel = "data/final/PREIS_indicators_long.csv") {
  if (is.null(root)) {
    root <- if (dir.exists(file.path(getwd(), "data", "pdf"))) getwd()
    else "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
  }
  pdfd  <- file.path(root, pdf_dir)
  indfp <- file.path(root, ind_rel)
  if (!dir.exists(pdfd))   { cat("Dossier PDF introuvable:", pdfd, "\n"); return(invisible(NULL)) }
  if (!file.exists(indfp)) { cat("indicators_long introuvable:", indfp, "\n"); return(invisible(NULL)) }
  
  ci <- function(s) suppressWarnings(as.numeric(gsub("[^0-9]", "", s)))
  cp <- function(s) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", s))))
  
  get_no <- function(f) {
    b <- basename(f)
    m <- str_match(b, "_(\\d{2,3})\\.pdf$")[, 2]
    if (is.na(m)) m <- str_match(b, "(?i)SitRep[ _-]*0*(\\d{1,3})")[, 2]
    if (is.na(m)) m <- str_match(b, "(?i)N[°ºo]?[ _-]*0*(\\d{1,3})")[, 2]
    if (is.na(m)) return(NA_integer_)
    as.integer(m)
  }
  
  # --- Extraction robuste multi-format ------------------------------------
  extract_nat <- function(txt_all) {
    txt <- paste(txt_all, collapse = "\n")
    cas <- NA_real_; dec <- NA_real_; cfr <- NA_real_; rule <- NA_character_
    
    # CFR de reference : "letalite globale [de] X %"
    cfr_ref <- NA_real_
    mc <- str_match(txt, "[Ll][ée]talit[ée][[:space:]]+globale[[:space:]]+(?:de[[:space:]]+)?([0-9]+[.,][0-9]+)[[:space:]]*%")
    if (!is.na(mc[1, 2])) cfr_ref <- cp(mc[1, 2])
    
    # 1) ratio (deces / cas) recoupe avec cfr_ref
    mm <- str_match_all(txt, "\\([[:space:]]*([0-9][0-9   ]*)[[:space:]]*/[[:space:]]*([0-9][0-9   ]*)[[:space:]]*\\)")[[1]]
    if (!is.null(mm) && nrow(mm) >= 1) {
      aa <- vapply(mm[, 2], ci, numeric(1), USE.NAMES = FALSE)
      bb <- vapply(mm[, 3], ci, numeric(1), USE.NAMES = FALSE)
      keep <- !is.na(aa) & !is.na(bb) & bb > aa & bb > 100
      aa <- aa[keep]; bb <- bb[keep]
      if (length(bb) > 0) {
        if (!is.na(cfr_ref)) {
          d <- abs(100 * aa / bb - cfr_ref); k <- which.min(d)
          if (length(k) == 1 && d[k] <= 3) { dec <- aa[k]; cas <- bb[k]; rule <- "ratio+cfr" }
        }
        if (is.na(cas)) {
          pl <- which(aa / bb >= 0.10 & aa / bb <= 0.75)
          if (length(pl) > 0) { k <- pl[which.max(bb[pl])]; dec <- aa[k]; cas <- bb[k]; rule <- "ratio" }
        }
      }
    }
    # 2) note de reconciliation : "= X cas" et "deces = Y"
    if (is.na(cas)) {
      m <- str_match(txt, "=[[:space:]]*([0-9][0-9   ]*)[[:space:]]*cas\\b")
      if (!is.na(m[1, 2])) { cas <- ci(m[1, 2]); if (is.na(rule)) rule <- "note" }
    }
    if (is.na(dec)) {
      m <- str_match(txt, "d[ée]c[èe]s[[:space:]]*=[[:space:]]*([0-9][0-9   ]*)")
      if (!is.na(m[1, 2])) dec <- ci(m[1, 2])
    }
    # 3) ligne Total ... /140 ou sur 140 (secours)
    if (is.na(cas) || is.na(dec)) {
      for (ln in unlist(strsplit(txt, "\n"))) {
        if (grepl("^[[:space:]]*Total[[:space:]]+[0-9]", ln) && grepl("(/|sur)[[:space:]]*140", ln)) {
          j <- gsub("([0-9])[   ]([0-9]{3})(?![0-9])", "\\1\\2", ln, perl = TRUE)
          nums <- unlist(regmatches(j, gregexpr("[0-9]+(?:[.,][0-9]+)?", j)))
          if (length(nums) >= 3) {
            if (is.na(cas)) cas <- ci(nums[1])
            if (is.na(dec)) dec <- ci(nums[2])
            if (is.na(cfr)) cfr <- cp(nums[3])
            if (is.na(rule)) rule <- "total"
          }
          break
        }
      }
    }
    # CFR final
    if (!is.na(cfr_ref)) cfr <- cfr_ref
    else if (is.na(cfr) && !is.na(dec) && !is.na(cas) && cas > 0) cfr <- round(100 * dec / cas, 1)
    
    list(cas = cas, deces = dec, cfr = cfr, rule = rule)
  }
  
  files <- list.files(pdfd, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) { cat("Aucun PDF dans", pdfd, "\n"); return(invisible(NULL)) }
  
  rows <- list()
  cat(sprintf("%-6s %-8s %-8s %-7s %s\n", "SitRep", "cas", "deces", "cfr", "regle"))
  cat(strrep("-", 52), "\n")
  for (f in files) {
    no <- get_no(f); if (is.na(no) || no < 1 || no > 300) next
    txt <- tryCatch(pdftools::pdf_text(f), error = function(e) NULL)
    if (is.null(txt)) next
    r <- extract_nat(txt)
    if (is.na(r$cas) && is.na(r$deces) && is.na(r$cfr)) next
    rows[[length(rows) + 1]] <- tibble::tibble(sitrep_no = no, cas = r$cas,
                                               deces = r$deces, cfr = r$cfr, rule = r$rule)
    cat(sprintf("%-6d %-8s %-8s %-7s %s\n", no,
                ifelse(is.na(r$cas), "-", format(r$cas)),
                ifelse(is.na(r$deces), "-", format(r$deces)),
                ifelse(is.na(r$cfr), "-", format(r$cfr)),
                ifelse(is.na(r$rule), "-", r$rule)))
  }
  if (length(rows) == 0) { cat("\nAucun cumul national extrait.\n"); return(invisible(NULL)) }
  found <- dplyr::bind_rows(rows) %>% dplyr::arrange(sitrep_no) %>%
    dplyr::distinct(sitrep_no, .keep_all = TRUE)
  
  ind <- readr::read_csv(indfp, show_col_types = FALSE)
  bak <- paste0(indfp, ".BAK_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  readr::write_csv(ind, bak)
  
  set_val <- function(df, sn, code, val) {
    if (is.na(val)) return(df)
    sel <- df$sitrep_no == sn & df$indicator_code == code
    if (any(sel, na.rm = TRUE)) {
      df$value[which(sel)] <- val
    } else {
      df <- dplyr::bind_rows(df, tibble::tibble(
        sitrep_no = as.integer(sn), indicator_code = code, value = as.numeric(val),
        value_source = "observed", source_type = "pdf_fix",
        extraction_rule = "fix_national_cumul_v2", priority = 1L))
    }
    df
  }
  n_before <- nrow(ind)
  skipped <- integer(0)
  for (i in seq_len(nrow(found))) {
    sn <- found$sitrep_no[i]; rl <- found$rule[i]
    # CONFIANCE : n'ecrire QUE les extractions fiables, pour ne jamais ecraser
    # une bonne valeur ancienne avec un sous-ratio douteux.
    ok_write <- !is.na(rl) && (rl == "ratio+cfr" || rl == "note" ||
                                 (rl == "total" && sn >= 56))
    if (!ok_write) { skipped <- c(skipped, sn); next }
    ind <- set_val(ind, sn, "cumulative_confirmed_cases", found$cas[i])
    ind <- set_val(ind, sn, "cumulative_deaths",          found$deces[i])
    ind <- set_val(ind, sn, "case_fatality_ratio",        found$cfr[i])
  }
  if (length(skipped) > 0)
    cat("Extractions peu fiables ignorees (valeurs existantes conservees) : ",
        paste(sort(unique(skipped)), collapse = ", "), "\n", sep = "")
  readr::write_csv(ind, indfp)
  
  cat("\n", strrep("=", 52), "\n", sep = "")
  cat(sprintf("SitRep traites : %d | indicators_long : %d -> %d lignes\n",
              nrow(found), n_before, nrow(ind)))
  cat("Sauvegarde indicators_long :", basename(bak), "\n")
  cat("Etape suivante :  source('scripts/03_analyse_consolidee.R')\n")
  invisible(found)
}

preis_fix_national_cumul()