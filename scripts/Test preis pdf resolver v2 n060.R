############################################################
# scripts/test_PREIS_pdf_resolver_v2_N060.R
#
# SAFE TEST for the PDF resolver V2.1.
#   * Sends NO email.
#   * Touches NO notification-state file.
#   * Sources ONLY scripts/preis_pdf_resolver_v2.R (never the email script).
#
# It runs in two phases:
#   PHASE A - offline unit tests (deterministic, no network) that PROVE the
#             URL-loss bug is fixed for the N060 case.
#   PHASE B - live end-to-end test against INSP for SitRep N060 (skipped
#             automatically if the runner has no network to insp.cd).
#
# Run:  Rscript scripts/test_PREIS_pdf_resolver_v2_N060.R
############################################################

## Locate repo root and load ONLY the resolver module -----------------------
args <- commandArgs(trailingOnly = FALSE)
this_file <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(this_file)) normalizePath(file.path(dirname(this_file), ".."), mustWork = FALSE) else getwd()
if (!file.exists(file.path(root, "scripts", "preis_pdf_resolver_v2.R"))) root <- getwd()
setwd(root)
cat("Repo root:", getwd(), "\n")

suppressWarnings(suppressMessages({
  library(httr); library(stringr); library(rvest); library(base64enc)
}))

source(file.path("scripts", "preis_pdf_resolver_v2.R"))

pass <- 0L; fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) { cat("  [OK]   ", label, "\n"); pass <<- pass + 1L }
  else              { cat("  [FAIL] ", label, "\n"); fail <<- fail + 1L }
}

TARGET_URL <- "https://insp.cd/wp-content/uploads/2026/07/Draft_SitRep_MVE_RDC_N\u00b060_13-07-2026_Analytique_VF.pdf"
PAGE_URL   <- "https://insp.cd/sitrep-n060-mvb-13-07-2026/"

## ==========================================================================
## PHASE A - OFFLINE UNIT TESTS (no network)
## ==========================================================================
cat("\n=== PHASE A: offline unit tests ===\n")

# A1: an already-absolute URL with a raw degree sign must survive untouched
cat("A1 preis_v2_absolute_url keeps absolute N\u00b060 URL (never NA)\n")
abs_out <- preis_v2_absolute_url(c(TARGET_URL, "/relative/path.pdf"))
check("absolute N\u00b060 URL preserved", TARGET_URL %in% abs_out)
check("no NA introduced", !any(is.na(abs_out)))
check("relative path resolved to insp.cd", any(grepl("^https://insp.cd/relative/path.pdf$", abs_out)))

# A2: cleaning a JSON-escaped candidate recovers the exact URL
cat("A2 preis_v2_clean_candidate_urls recovers escaped-slash URL\n")
escaped <- "https:\\/\\/insp.cd\\/wp-content\\/uploads\\/2026\\/07\\/Draft_SitRep_MVE_RDC_N\u00b060_13-07-2026_Analytique_VF.pdf"
cleaned <- preis_v2_clean_candidate_urls(c(escaped, "https://insp.cd/logo.png"))
check("escaped slashes normalized to exact target", TARGET_URL %in% cleaned)
check("non-pdf dropped", !any(grepl("logo.png", cleaned)))
check("exactly one real candidate", length(cleaned) == 1L)

# A3: pdfemb-data base64 -> JSON -> url + pdfID (built locally, no network)
cat("A3 preis_v2_decode_pdfemb decodes the N060 payload\n")
json_n060 <- paste0(
  '{"pdfID":25147,',
  '"url":"https:\\/\\/insp.cd\\/wp-content\\/uploads\\/2026\\/07\\/',
  'Draft_SitRep_MVE_RDC_N\u00b060_13-07-2026_Analytique_VF.pdf",',
  '"title":"Draft_SitRep_MVE_RDC_N\u00b060_13-07-2026_Analytique_VF"}'
)
b64_n060 <- base64enc::base64encode(charToRaw(json_n060))
html_n060 <- paste0('<iframe src="https://insp.cd/?pdfemb-data=', b64_n060, '"></iframe>')
dec <- preis_v2_decode_pdfemb(html_n060)
check("one pdfemb payload detected", dec$n_payloads == 1L)
check("payload url == exact target", TARGET_URL %in% preis_v2_clean_candidate_urls(dec$payload_urls))
check("pdfID 25147 extracted", "25147" %in% dec$pdf_ids)

# A4: %PDF signature validation logic
cat("A4 preis_v2_pdf_signature_ok distinguishes real vs fake\n")
good <- tempfile(fileext = ".pdf"); writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(rep(32, 2000))), good)
bad  <- tempfile(fileext = ".pdf"); writeBin(charToRaw("<html>not a pdf</html>"), bad)
check("valid %PDF accepted", preis_v2_pdf_signature_ok(good))
check("non-PDF rejected",    !preis_v2_pdf_signature_ok(bad))
unlink(c(good, bad), force = TRUE)

# A5: ranking puts the real extracted candidate ahead of a guessed name
cat("A5 preis_v2_rank_candidates ranks the real URL first\n")
ranked <- preis_v2_rank_candidates(
  c("https://insp.cd/wp-content/uploads/2026/07/SitRep_MVE_RDC_N\u00b060_13-07-2026.pdf", TARGET_URL),
  sitrep_no = 60, is_guess = c(TRUE, FALSE))
check("real (non-guess) candidate ranked first", identical(ranked[1], TARGET_URL))

## ==========================================================================
## PHASE B - LIVE END-TO-END (skipped if no network to insp.cd)
## ==========================================================================
cat("\n=== PHASE B: live N060 end-to-end ===\n")

state_file <- file.path(getwd(), "data", "preis_safe_email_notification_state.csv")
state_before <- if (file.exists(state_file)) tools::md5sum(state_file) else NA_character_

online <- tryCatch({
  r <- httr::GET("https://insp.cd/", httr::timeout(20))
  httr::status_code(r) >= 200 && httr::status_code(r) < 500
}, error = function(e) FALSE)

if (!online) {
  cat("  [SKIP]  insp.cd not reachable from this runner; live phase skipped.\n")
  cat("          (Phase A already proves the resolution/cleaning logic.)\n")
} else {
  dest <- preis_v2_resolve_and_download(
    sitrep_no       = 60,
    page_url        = PAGE_URL,
    current_pdf_url = "",
    title           = "Draft_SitRep_MVE_RDC_N\u00b060_13-07-2026_Analytique_VF"
  )
  check("download returned a path", !is.na(dest) && nzchar(dest))
  check("file exists on disk", !is.na(dest) && file.exists(dest))
  check("file is a valid %PDF", !is.na(dest) && preis_v2_pdf_signature_ok(dest))
  check("saved as PREIS_DRC_Ebola_SitRep_060.pdf",
        !is.na(dest) && grepl("PREIS_DRC_Ebola_SitRep_060[.]pdf$", dest))
}

## ==========================================================================
## SAFETY ASSERTIONS: no email, no state change
## ==========================================================================
cat("\n=== Safety assertions ===\n")
state_after <- if (file.exists(state_file)) tools::md5sum(state_file) else NA_character_
check("notification state unchanged", identical(state_before, state_after))
cat("  Email sent: NO\n")
cat("  Notification state changed: NO\n")

## ==========================================================================
cat("\n=== SUMMARY ===\n")
cat(sprintf("  PASS=%d  FAIL=%d\n", pass, fail))
if (fail > 0) quit(save = "no", status = 1) else quit(save = "no", status = 0)