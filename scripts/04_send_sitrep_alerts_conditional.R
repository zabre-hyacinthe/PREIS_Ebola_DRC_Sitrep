############################################################
# PREIS EBOLA DRC
# 04_send_sitrep_alerts_conditional.R
#
# Adapted from 63_send_alerts_conditional.R (PREIS V10 logic).
# Conditional per-SitRep sending, with deduplication.
#
# LOGIC:
#   - sent_log_sitrep.csv = source of truth (never sends the same
#     SitRep twice to the same address)
#   - For each active recipient: sends only the SitReps not yet
#     sent to THAT address
#   - Reuses preis_send_email() from R/60_email.R
#   - Body = summary + signals + recommendations + dashboard link
#
# Recipients: data/final/alert_recipients.csv
#   columns: active, type, name, email
#
# CHANGE (this version): email subject/body translated to English
# (previously French). No logic, column names, or file paths were
# changed — only the visible text. See claude/PREIS_dashboard_inventaire.md
# ("Emails automatiques ... -> reecrire les gabarits en anglais").
############################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tibble)
})

ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Paths (consistent with the PREIS Ebola pipeline)
BASE_DIR    <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
DATA_FINAL  <- file.path(BASE_DIR, "data/final")
OUT_DIR     <- file.path(BASE_DIR, "outputs/analyse")
SERIE_FP    <- file.path(OUT_DIR, "serie_temporelle_nationale.csv")
RECIP_FP    <- file.path(BASE_DIR, "data", "alert_recipients.csv")
SENT_LOG_FP <- file.path(DATA_FINAL, "sent_log_sitrep.csv")

if (!file.exists(SERIE_FP)) stop("Series not found: ", SERIE_FP,
                                 "\nRun 03_analyse_consolidee.R first.")
if (!file.exists(RECIP_FP)) stop("Recipients file not found: ", RECIP_FP)

# Centralised send function (reuses your existing infra).
# Your scripts live in scripts/ -- look for 60_email.R there first.
email_candidates <- c(
  file.path(BASE_DIR, "scripts", "60_email.R"),
  file.path(BASE_DIR, "scripts", "60_email_FV.R"),
  file.path(ROOT, "scripts", "60_email.R"),
  file.path(ROOT, "R", "60_email.R")
)
email_fn <- email_candidates[file.exists(email_candidates)][1]
if (is.na(email_fn) || length(email_fn) == 0) {
  stop("Send function not found (looked for 60_email.R in scripts/). ",
       "Check the exact file name, or use 04_send_email_alert.R (blastula).")
}
source(email_fn)
cat("[alerts] Email function loaded from:", email_fn, "\n")

dash_url <- Sys.getenv("PREIS_DASHBOARD_URL", "")

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble())
  tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) tibble())
}
fmt <- function(x) if (is.na(x)) "not available" else format(x, big.mark = ",")

# ------------------------------------------------------------
# 1) Load series + recipients + log
# ------------------------------------------------------------
serie <- read_csv(SERIE_FP, show_col_types = FALSE) %>% arrange(sitrep_no)

recips <- safe_read_csv(RECIP_FP)
for (nm in c("active","type","name","email")) {
  if (!nm %in% names(recips)) recips[[nm]] <- NA_character_
}
recips <- recips %>%
  mutate(
    active = toupper(as.character(active)),
    type   = ifelse(is.na(type) | type == "", "to", tolower(type)),
    name   = as.character(name),
    email  = as.character(email)
  ) %>%
  filter(active %in% c("TRUE","T","1","YES","OUI"),
         !is.na(email), email != "")

if (nrow(recips) == 0) stop("No active recipients in ", RECIP_FP)

sent_log <- safe_read_csv(SENT_LOG_FP)
log_cols <- c("date","recipient_name","recipient_email",
              "message_type","sitrep_no","status")
if (nrow(sent_log) == 0) {
  sent_log <- tibble(date=character(), recipient_name=character(),
                     recipient_email=character(), message_type=character(),
                     sitrep_no=character(), status=character())
} else {
  for (nm in log_cols) if (!nm %in% names(sent_log)) sent_log[[nm]] <- NA_character_
  sent_log <- sent_log %>% mutate(across(all_of(log_cols), as.character))
}

# ------------------------------------------------------------
# 2) Build the body (summary + signals + recommendations)
# ------------------------------------------------------------
build_sitrep_body <- function(rec_name, sno) {
  row  <- serie %>% filter(sitrep_no == sno)
  prev <- serie %>% filter(sitrep_no < sno) %>% slice_tail(n = 1)
  if (nrow(row) == 0) return(NULL)

  d_cas  <- if (nrow(prev)) row$cas_cumules   - prev$cas_cumules   else NA
  d_dec  <- if (nrow(prev)) row$deces_cumules - prev$deces_cumules else NA
  ndeces <- if ("nouveaux_deces" %in% names(row)) row$nouveaux_deces else NA
  ncas   <- if ("nouveaux_cas"   %in% names(row)) row$nouveaux_cas   else NA

  signaux <- character(); recos <- character()
  if (!is.na(row$cfr) && row$cfr >= 15) {
    signaux <- c(signaux, sprintf("[RED] High lethality: CFR %.1f%%", row$cfr))
    recos <- c(recos, paste0(
      "High CFR -- check the presentation-to-care delay, proportion of ",
      "community deaths, and case-management capacity in active zones."))
  }
  if (!is.na(ndeces) && ndeces > 0) {
    signaux <- c(signaux, sprintf("[RED] %d new death(s)", as.integer(ndeces)))
    recos <- c(recos, paste0(
      "New deaths -- investigate each death (location, delay, known contact ",
      "status) to distinguish active transmission from community deaths."))
  }
  if (!is.na(ncas) && ncas > 0) {
    signaux <- c(signaux, sprintf("[ORANGE] %d new confirmed case(s)", as.integer(ncas)))
    recos <- c(recos, paste0(
      "New cases -- confirm the share originating from already-listed contacts ",
      "(controlled transmission) versus cases outside the list (unexplained chains)."))
  }
  if (!length(signaux)) signaux <- "[GREEN] No critical signal on this SitRep."
  if (!length(recos))   recos   <- "Continue routine surveillance."

  delta_txt <- function(d) if (is.na(d)) "" else sprintf(" (%+d vs previous)", as.integer(d))

  lines <- c(
    sprintf("PREIS Ebola DRC -- SitRep Alert No. %d", sno),
    sprintf("Report date: %s", row$date),
    "17th outbreak (Bundibugyo -- Ituri / North Kivu / South Kivu)",
    "",
    "KEY DATA (national cumulative totals, validated INRB source):",
    sprintf("- Cumulative confirmed cases: %s%s", fmt(row$cas_cumules), delta_txt(d_cas)),
    sprintf("- Cumulative deaths         : %s%s", fmt(row$deces_cumules), delta_txt(d_dec)),
    sprintf("- New deaths                : %s", fmt(ndeces)),
    sprintf("- Case-fatality ratio (prov.): %s", ifelse(is.na(row$cfr),"n/a",paste0(row$cfr,"%"))),
    "",
    "OPERATIONAL SIGNALS:",
    paste0("  ", signaux),
    "",
    "RECOMMENDATIONS:",
    paste0("  - ", recos),
    "",
    if (nzchar(dash_url)) paste0("Dashboard: ", dash_url) else "",
    "",
    "---",
    "Provisional CFR: some recent cases may still evolve; not the final case-fatality ratio.",
    "Probable drivers only -- no established causality.",
    "National cumulative totals = validated INRB data; zone-level detail = PDF extraction pending validation.",
    sprintf("Automatically generated on %s.", Sys.Date())
  )
  paste(lines[lines != "" | TRUE], collapse = "\n")
}

# ------------------------------------------------------------
# 3) Recipient loop: send SitReps not yet sent
# ------------------------------------------------------------
all_snos <- sort(unique(serie$sitrep_no))
new_log <- list()

# Detect attachment support in your send function
send_formals <- tryCatch(names(formals(preis_send_email)), error = function(e) character(0))
supports_attach <- "attachments" %in% send_formals

# Default attachments: the analysis charts
default_attach <- file.path(OUT_DIR, c(
  "g1_courbe_epidemique.png","g2_courbe_mortalite.png",
  "g3_evolution_cfr.png","carte_zones_intensite.png"))
default_attach <- default_attach[file.exists(default_attach)]

for (i in seq_len(nrow(recips))) {
  addr   <- recips$email[i]
  nm_rec <- recips$name[i]

  # SitReps already sent to THIS address
  done <- sent_log %>%
    filter(recipient_email == addr,
           toupper(message_type) == "SITREP",
           status == "sent") %>%
    pull(sitrep_no) %>% as.integer() %>% unique()

  to_send <- setdiff(all_snos, done)

  # By default: send ONLY the latest unsent SitRep (avoids flooding
  # on first run). To send everything, set
  # PREIS_SEND_ALL_SITREPS=true in .Renviron.
  send_all <- tolower(Sys.getenv("PREIS_SEND_ALL_SITREPS","false")) %in% c("true","1","yes")
  if (!send_all && length(to_send) > 0) to_send <- max(to_send)

  if (length(to_send) == 0) {
    cat(sprintf("[alerts] No new SitRep for %s (%s)\n", nm_rec, addr))
    next
  }

  for (sno in sort(to_send)) {
    body <- build_sitrep_body(nm_rec, sno)
    if (is.null(body)) next

    row <- serie %>% filter(sitrep_no == sno)
    subject <- sprintf("[PREIS Ebola DRC] SitRep No. %d -- %s cases, %s deaths (CFR %s%%)",
                       sno, fmt(row$cas_cumules), fmt(row$deces_cumules),
                       ifelse(is.na(row$cfr),"n/a",row$cfr))

    ok <- TRUE
    tryCatch({
      if (supports_attach && length(default_attach) > 0) {
        preis_send_email(to = addr, subject = subject,
                         body_text = body, attachments = default_attach,
                         dry_run = FALSE)
      } else {
        preis_send_email(to = addr, subject = subject,
                         body_text = body, dry_run = FALSE)
      }
      cat(sprintf("[alerts] SitRep %d sent to %s (%s)\n", sno, nm_rec, addr))
    }, error = function(e) {
      ok <<- FALSE
      cat(sprintf("[alerts] FAILED SitRep %d for %s: %s\n",
                  sno, addr, conditionMessage(e)))
    })

    if (ok) {
      new_log[[length(new_log)+1]] <- tibble(
        date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        recipient_name = nm_rec, recipient_email = addr,
        message_type = "SITREP", sitrep_no = as.character(sno),
        status = "sent")
    }
  }
}

# ------------------------------------------------------------
# 4) Update the log
# ------------------------------------------------------------
if (length(new_log) > 0) {
  add <- bind_rows(new_log)
  sent_log <- if (nrow(sent_log) > 0) bind_rows(sent_log, add) else add
  write_csv(sent_log, SENT_LOG_FP, na = "")
  cat(sprintf("[alerts] sent_log updated: %d new row(s)\n", nrow(add)))
} else {
  cat("[alerts] No email sent.\n")
}
