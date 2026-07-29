## ============================================================
## PREIS Ebola DRC — Africa CDC-style SitRep generator (FINAL v5)
## 06_generate_africa_cdc_sitrep_final.R
##
## Same visual identity as v2/v3/v4 (colors/font/layout extracted
## from the real model docx). This version ADDS/CHANGES:
##
##   A. Expert epidemiological narrative under every chart/table --
##      not just factual captions, but an operational reading: what
##      it means and what a responder should watch for.
##
##   B. "OPERATIONAL WATCH — PRIORITY ZONES FOR VERIFICATION"
##      Zone-level RED/ORANGE flags computed from data already in
##      PREIS_daily_indicators.csv (case acceleration, lethality
##      level/trend) -- see operational_watch.R. Each flag comes
##      with a concrete verification action, framed as something to
##      CHECK, never a diagnosis (PREIS never fabricates causality).
##      This is real, computed, verified against the live pipeline.
##      Table columns use FIXED widths (set_table_properties(layout
##      = "fixed")) -- without this, flextable/Word recalculates
##      column widths on open and crushes short columns next to any
##      long free-text column on the same page.
##
##   (Section C, "Operational Gaps -- village-level drill-down", was
##   added then REMOVED at the user's request: the underlying gap
##   register's geography fields were almost entirely "Not
##   documented", so the table looked precise but wasn't reliable.
##   See the comment left in place, right before "DETECTED SIGNALS"
##   below, for how to reinstate it once that data is fixed.)
##
## Requires: officer, flextable, dplyr, readr, stringr, tidyr,
## ggplot2. Also source()s operational_watch.R (same folder).
##
## NOTE: sections A/B use data verified against the live pipeline
## and mirror patterns already tested earlier in this conversation.
## Section C is new and unverified -- test it in isolation first,
## see the test snippet at the bottom of this file.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr)
  library(officer); library(flextable); library(ggplot2)
})

# ---- Paths ----------------------------------------------------
BASE_DIR    <- Sys.getenv("PREIS_BASE_DIR", "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
DATA_FINAL  <- file.path(BASE_DIR, "data/final")
OUT_DIR     <- file.path(BASE_DIR, "outputs/rapports")
CHART_DIR   <- file.path(OUT_DIR, "charts_tmp")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
if (!dir.exists(CHART_DIR)) dir.create(CHART_DIR, recursive = TRUE)

DAILY_FP    <- file.path(DATA_FINAL, "PREIS_daily_indicators.csv")
SIGNALS_FP  <- file.path(DATA_FINAL, "PREIS_signals.csv")
REGISTRY_FP <- file.path(DATA_FINAL, "sitrep_registry.csv")
LOGO_FP     <- Sys.getenv("PREIS_EIU_LOGO", file.path(BASE_DIR, "assets", "africacdc_eiu_header.png"))
OW_HELPER_FP <- file.path(BASE_DIR, "scripts", "operational_watch.R")

stopifnot(file.exists(DAILY_FP), file.exists(REGISTRY_FP))
if (file.exists(OW_HELPER_FP)) source(OW_HELPER_FP) else stop(
  "operational_watch.R not found at ", OW_HELPER_FP, " -- copy it into scripts/ first.")

# ---- Real Africa CDC identity (extracted from the model docx) --
FONT      <- "Encode Sans"
MAROON    <- "#9F2241"
GREEN     <- "#1A5632"
CREAM     <- "#F5F3EE"
GOLD      <- "#C3A465"
GREYTXT   <- "#8A8D90"
BODYTXT   <- "#53575A"
RULECLR   <- "#E4E2DC"

# ---- Load pipeline data -----------------------------------------
daily    <- read_csv(DAILY_FP, show_col_types = FALSE)
signals  <- if (file.exists(SIGNALS_FP)) read_csv(SIGNALS_FP, show_col_types = FALSE) else tibble()
registry <- read_csv(REGISTRY_FP, show_col_types = FALSE) %>% arrange(desc(sitrep_no))

latest_sno  <- registry$sitrep_no[1]
latest_date <- registry$date_raw[1]
today_str   <- format(Sys.Date(), "%d %B %Y")

nat <- daily %>% filter(level == "National") %>% arrange(date) %>% mutate(date = as.Date(date))
last_date <- daily %>% filter(level == "National") %>% pull(date) %>% max()
nat_last  <- nat[nrow(nat), ]
nat_prev  <- if (nrow(nat) >= 2) nat[nrow(nat) - 1, ] else nat_last

# FIX 2026-07-29 : PREIS_daily_indicators.csv's National rows and its
# Zone/Province rows can be one refresh cycle apart (confirmed on
# SitRep 74: national already at 27 July, zone-level still at 26
# July). Using the SAME "last_date" for both used to silently label
# stale zone data as same-day -- now zone-level sections use their
# OWN latest available date, and the document says so explicitly
# wherever that date differs from the national one, instead of
# hiding the lag.
zone_last_date <- daily %>% filter(level == "Zone") %>% pull(date) %>% max()
zone_data_lagging <- as.character(zone_last_date) != as.character(last_date)

provinces <- daily %>% filter(level == "Province", date == zone_last_date) %>% arrange(desc(cum_cases))
zones     <- daily %>% filter(level == "Zone", date == zone_last_date) %>% arrange(desc(cum_cases)) %>% slice_head(n = 8)
zones_all_latest <- daily %>% filter(level == "Zone", date == zone_last_date, cum_cases > 0)
recent_signals <- if (nrow(signals) > 0) signals %>% arrange(desc(detected_on)) %>% slice_head(n = 6) else tibble()

# B: operational watch (real, computable now) -- uses the zone-level
# date, not the national one, for the same reason as above.
ow <- compute_operational_watch(daily, zone_last_date, min_cases = 10)

pc <- function(x) if (is.na(x)) "n/a" else paste0(round(x, 1), "%")

# ---- Styling helpers ---------------------------------------------
styled_par <- function(doc, text, color = BODYTXT, bold = FALSE, italic = FALSE, size = 10, align = "left") {
  body_add_fpar(doc, fpar(
    ftext(text, fp_text(font.family = FONT, color = color, bold = bold, italic = italic, font.size = size)),
    fp_p = fp_par(text.align = align)
  ))
}
bullet <- function(doc, text, color = BODYTXT) styled_par(doc, paste0("\u2022   ", text), color = color, size = 10)

section_bar <- function(doc, title, tone = "green") {
  bg <- if (tone == "red") MAROON else GREEN
  ft <- flextable(data.frame(x = toupper(title))) %>%
    delete_part(part = "header") %>%
    font(fontname = FONT, part = "all") %>%
    bg(bg = bg, part = "body") %>%
    color(color = "white", part = "body") %>%
    bold(part = "body") %>%
    fontsize(size = 12, part = "body") %>%
    border_remove() %>%
    width(width = 6.4) %>%
    set_table_properties(layout = "fixed", width = 1)
  body_add_flextable(doc, ft)
}

data_table <- function(doc, df, first_col_frac = 0.24, total_width = 6.4) {
  n <- nrow(df); ncols <- ncol(df)
  first_w <- total_width * first_col_frac
  rest_w  <- (total_width - first_w) / (ncols - 1)
  ft <- flextable(df) %>%
    font(fontname = FONT, part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    bg(bg = GREEN, part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    color(color = BODYTXT, part = "body") %>%
    align(align = "center", part = "all") %>%
    valign(valign = "center", part = "all") %>%
    border_remove()
  ft <- width(ft, j = 1, width = first_w)
  if (ncols > 1) for (j in 2:ncols) ft <- width(ft, j = j, width = rest_w)
  for (i in seq_len(n)) if (i %% 2 == 1) ft <- bg(ft, i = i, bg = CREAM, part = "body")
  ft <- set_table_properties(ft, layout = "fixed", width = 1)
  body_add_flextable(doc, ft)
}

# Same as data_table but tints RED/ORANGE severity rows -- used for
# the Operational Watch table so the priority level is visible at a
# glance. IMPORTANT: the table itself only carries SHORT category
# tags (not the full sentence) -- explicit widths + long free text
# in the same table was what crushed every other column to
# single-character width in the previous version. Full explanations
# now live in a short legend printed once below the table instead of
# being repeated (often identically) on every row.
.short_flag <- function(reason) {
  has_active <- grepl("High-lethality active transmission", reason)
  has_accel  <- grepl("Rapid case acceleration", reason)
  has_rising <- grepl("Rising lethality", reason)
  has_sust   <- grepl("Sustained high lethality", reason)
  dplyr::case_when(
    has_active & has_accel ~ "Active + accelerating",
    has_active             ~ "Active, high CFR",
    has_accel & has_rising ~ "Accelerating + rising CFR",
    has_accel               ~ "Accelerating",
    has_rising               ~ "Rising CFR",
    has_sust                 ~ "Sustained high CFR",
    TRUE                      ~ "Flagged"
  )
}
watch_table <- function(doc, df, total_width = 6.4) {
  n <- nrow(df)
  widths <- c(1.00, 0.90, 0.60, 0.95, 0.85, 2.10)  # Zone/Province/Sev/New/CFR/Flag; sums to 6.4in exactly (verified)
  ft <- flextable(df) %>%
    font(fontname = FONT, part = "all") %>%
    fontsize(size = 8.5, part = "all") %>%
    bg(bg = MAROON, part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    color(color = BODYTXT, part = "body") %>%
    align(align = "left", part = "all") %>%
    align(j = c("Sev.", "New (24h/7d)", "CFR"), align = "center", part = "all") %>%
    valign(valign = "center", part = "all") %>%
    border_remove()
  for (j in seq_len(ncol(df))) ft <- width(ft, j = j, width = widths[j])
  for (i in seq_len(n)) {
    row_bg <- if (df$Sev.[i] == "RED") "#FBE4E7" else "#FDF1DC"
    ft <- bg(ft, i = i, bg = row_bg, part = "body")
    ft <- color(ft, i = i, j = "Sev.", color = if (df$Sev.[i] == "RED") MAROON else "#B8860B", part = "body")
    ft <- bold(ft, i = i, j = "Sev.", part = "body")
  }
  ft <- set_table_properties(ft, layout = "fixed", width = 1)
  body_add_flextable(doc, ft)
}


kpi_row <- function(doc, values, labels, polarity) {
  n <- length(values)
  col_w <- 6.4 / n
  colors <- ifelse(polarity == "bad", MAROON, GREEN)
  num_df <- as.data.frame(setNames(as.list(values), paste0("c", seq_len(n))), check.names = FALSE)
  lab_df <- as.data.frame(setNames(as.list(labels), paste0("c", seq_len(n))), check.names = FALSE)

  ft_num <- flextable(num_df) %>% delete_part(part = "header") %>%
    font(fontname = FONT, part = "all") %>% fontsize(size = 22, part = "all") %>%
    bold(part = "all") %>% align(align = "center", part = "all") %>%
    valign(valign = "center", part = "all") %>%
    border_remove() %>% padding(padding.top = 6, padding.bottom = 0, part = "all")
  for (i in seq_len(n)) ft_num <- color(ft_num, j = i, color = colors[i], part = "all")
  for (j in seq_len(n)) ft_num <- width(ft_num, j = j, width = col_w)
  ft_num <- set_table_properties(ft_num, layout = "fixed", width = 1)
  doc <- body_add_flextable(doc, ft_num)

  ft_lab <- flextable(lab_df) %>% delete_part(part = "header") %>%
    font(fontname = FONT, part = "all") %>% fontsize(size = 8, part = "all") %>%
    color(color = GREYTXT, part = "all") %>% align(align = "center", part = "all") %>%
    valign(valign = "center", part = "all") %>%
    border_remove() %>% padding(padding.top = 0, padding.bottom = 10, part = "all")
  for (j in seq_len(n)) ft_lab <- width(ft_lab, j = j, width = col_w)
  ft_lab <- set_table_properties(ft_lab, layout = "fixed", width = 1)
  body_add_flextable(doc, ft_lab)
}

footer_block <- function(doc, label, paragraphs) {
  doc <- body_add_fpar(doc, fpar(
    ftext("", fp_text(font.size = 1)),
    fp_p = fp_par(border.top = fp_border(color = RULECLR, width = 0.75), padding.top = 8)
  ))
  doc <- styled_par(doc, toupper(label), color = MAROON, bold = TRUE, size = 11)
  for (t in paragraphs) doc <- styled_par(doc, t, size = 9)
  doc
}

chart_section <- function(doc, title, plot_obj, filename, width = 6.4, height = 2.8, captions = NULL) {
  doc <- section_bar(doc, title)
  fp <- file.path(CHART_DIR, filename)
  ggsave(fp, plot_obj, width = width, height = height, dpi = 200, bg = "white")
  doc <- body_add_img(doc, src = fp, width = width, height = height)
  if (!is.null(captions)) for (cap in captions) doc <- styled_par(doc, cap, color = GREYTXT, italic = TRUE, size = 8)
  doc
}

ggplot_base_theme <- theme_minimal(base_size = 10) +
  theme(text = element_text(family = FONT, color = BODYTXT),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 9, color = BODYTXT),
        legend.position = "none")

# ------------------------------------------------------------
# Charts
# ------------------------------------------------------------
epicurve_df <- nat %>% filter(!is.na(new_cases))
epicurve_plot <- ggplot(epicurve_df, aes(x = date)) +
  geom_col(aes(y = new_cases), fill = GREEN, alpha = 0.55, width = 0.8) +
  geom_line(aes(y = ma7_new_cases), color = MAROON, linewidth = 1, na.rm = TRUE) +
  labs(x = NULL, y = "New confirmed cases (bars) / 7-day average (line)") +
  ggplot_base_theme

cfr_scatter_plot <- ggplot(zones_all_latest, aes(x = cum_cases, y = cfr, size = cum_cases, color = cfr)) +
  geom_point(alpha = 0.75) +
  scale_x_log10() +
  scale_color_gradient(low = GREEN, high = MAROON) +
  scale_size(range = c(2, 10)) +
  labs(x = "Cumulative confirmed cases (log scale)", y = "Provisional CFR (%)") +
  ggplot_base_theme

# Days since last case (unchanged logic from v3)
zone_hist <- daily %>% filter(level == "Zone") %>% arrange(zone, province, date) %>%
  group_by(zone, province) %>%
  mutate(new_case_day = replace_na(cum_cases - lag(cum_cases), 0) > 0) %>%
  ungroup()
days_since_df <- zone_hist %>% filter(new_case_day) %>%
  group_by(zone, province) %>%
  summarise(last_case_date = max(date), .groups = "drop") %>%
  mutate(days_since_last_case = as.integer(as.Date(zone_last_date) - as.Date(last_case_date))) %>%
  arrange(desc(days_since_last_case)) %>%
  slice_head(n = 8)

# Compute a couple of narrative numbers used in the interpretation text below
nat_growth_txt <- {
  n <- nrow(nat)
  if (n >= 14) {
    inc7 <- sum(pmax(tail(nat$new_cases, 7), 0), na.rm = TRUE)
    inc7prev <- sum(pmax(nat$new_cases[(n-13):(n-7)], 0), na.rm = TRUE)
    if (inc7prev > 0) sprintf("%.0f%% vs. the prior 7 days", 100*(inc7-inc7prev)/inc7prev) else "not estimable (insufficient prior data)"
  } else "not estimable (fewer than 14 reports)"
}
n_red <- if (nrow(ow) > 0) sum(ow$severity == "RED") else 0
n_orange <- if (nrow(ow) > 0) sum(ow$severity == "ORANGE") else 0

# ------------------------------------------------------------
# Build the document
# ------------------------------------------------------------
doc <- read_docx()

hdr_df <- data.frame(
  logo = c("", ""),
  txt = c("EPIDEMIC INTELLIGENCE UNIT \u00b7 SURVEILLANCE & DISEASE INTELLIGENCE DIVISION",
          "Safeguarding Africa's Health")
)
ft_hdr <- flextable(hdr_df) %>%
  delete_part(part = "header") %>%
  font(fontname = FONT, part = "all") %>%
  bg(bg = GREEN, part = "body") %>%
  border_remove() %>%
  width(j = 1, width = 1.9) %>% width(j = 2, width = 4.5) %>%
  valign(valign = "center", part = "body") %>%
  padding(padding.top = 4, padding.bottom = 4, part = "body")

# Two rows in the text column: small white eyebrow line (row 1),
# larger gold tagline (row 2) -- plain per-row styling only, no
# compose()/as_chunk() multi-run text (that combination is what
# crashed with "Can't convert `x`, a list matrix, to a function" on
# 2026-07-29; this simpler, textbook approach avoids it entirely).
ft_hdr <- color(ft_hdr, i = 1, j = 2, color = "white", part = "body")
ft_hdr <- bold(ft_hdr, i = 1, j = 2, part = "body")
ft_hdr <- fontsize(ft_hdr, i = 1, j = 2, size = 8, part = "body")
ft_hdr <- color(ft_hdr, i = 2, j = 2, color = GOLD, part = "body")
ft_hdr <- bold(ft_hdr, i = 2, j = 2, part = "body")
ft_hdr <- fontsize(ft_hdr, i = 2, j = 2, size = 12, part = "body")

# Logo: a single, standard compose()+as_image() call (the well-
# documented flextable pattern for images in cells), then merge the
# logo cell vertically across both rows so it reads as one band.
if (file.exists(LOGO_FP)) {
  ft_hdr <- flextable::compose(ft_hdr, i = 1, j = 1, value = as_paragraph(as_image(src = LOGO_FP, width = 1.5, height = 0.7)))
  ft_hdr <- merge_at(ft_hdr, i = 1:2, j = 1)
} else {
  message("[sitrep-cdc-final] Logo not found at ", LOGO_FP, " -- header band will show without it.")
}
doc <- body_add_flextable(doc, set_table_properties(ft_hdr, layout = "fixed", width = 1))

doc <- doc %>%
  styled_par("", size = 4) %>%
  styled_par("Bundibugyo Virus Disease Outbreak", color = MAROON, bold = TRUE, size = 18) %>%
  styled_par("DRC \u00b7 SITUATIONAL REPORT \u2014 PREIS AUTOMATED EPIDEMIOLOGICAL SUPPLEMENT",
             color = MAROON, bold = TRUE, size = 11) %>%
  styled_par(sprintf("Date of report: %s   |   Generated: %s   |   SitRep No. %s",
                      latest_date, today_str, latest_sno), color = GREYTXT, size = 9) %>%
  styled_par("", size = 6)

## -- KEY UPDATES ------------------------------------------------
doc <- section_bar(doc, "Key updates in the last 24 hours")
doc <- bullet(doc, sprintf("%s new confirmed cases nationally.", nat_last$new_cases))
doc <- bullet(doc, sprintf("%s new deaths.", nat_last$new_deaths))
doc <- bullet(doc, sprintf("Provisional national CFR %s (vs %s at previous SitRep).",
                            pc(nat_last$cfr), pc(nat_prev$cfr)))
doc <- doc %>% styled_par("", size = 6)

## -- NATIONAL OVERVIEW --------------------------------------------
doc <- section_bar(doc, paste0("National overview \u2014 as of ", toupper(format(as.Date(last_date), "%d %B %Y"))))
doc <- kpi_row(doc,
  values   = c(format(nat_last$cum_cases, big.mark = ","), format(nat_last$cum_deaths, big.mark = ","), pc(nat_last$cfr)),
  labels   = c("CONFIRMED CASES", "DEATHS", "CASE FATALITY (PROVISIONAL)"),
  polarity = c("good", "bad", "bad"))
doc <- doc %>% styled_par(
  "Contacts under follow-up, recoveries and healthcare-worker cases are outside PREIS's current DRC feed and are not shown here.",
  color = GREYTXT, italic = TRUE, size = 8)
doc <- doc %>% styled_par("", size = 6)

## -- EPIDEMIC CURVE (now with expert narrative) ---------------------
doc <- chart_section(doc, "Epidemic curve \u2014 national",
  epicurve_plot, "epicurve.png", width = 6.4, height = 2.6,
  captions = c(
    "Bars: daily new confirmed cases. Line: 7-day moving average (smooths day-to-day reporting noise).",
    sprintf("Reading: 7-day incidence changed by %s. A rising average over multiple SitReps signals active, uncontrolled transmission and warrants review of case-finding and contact-tracing coverage in the leading provinces; a falling average should be corroborated by contact follow-up data before being read as genuine control.", nat_growth_txt)
  ))
doc <- doc %>% styled_par("", size = 6)

## -- PROVINCIAL BREAKDOWN -------------------------------------------
doc <- section_bar(doc, "Provincial breakdown")
if (zone_data_lagging) {
  doc <- doc %>% styled_par(sprintf(
    "Zone-level indicators below are as of %s (most recent available refresh at generation time) \u2014 one reporting cycle behind the %s national headline figures above.",
    format(as.Date(zone_last_date), "%d %B %Y"), format(as.Date(last_date), "%d %B %Y")),
    color = GREYTXT, italic = TRUE, size = 8)
}
prov_df <- provinces %>% transmute(
  Province = province,
  `Cum. cases`  = format(cum_cases, big.mark = ","),
  `Cum. deaths` = format(cum_deaths, big.mark = ","),
  CFR = sapply(cfr, pc),
  `New (24h)` = paste0("+", new_cases, " / +", new_deaths)
)
doc <- data_table(doc, prov_df)
doc <- doc %>% styled_par(
  "Reading: compare each province's CFR to the national figure above -- a province running persistently hotter than the national CFR is a case-management/access-to-care question, not just an epidemiological one.",
  color = GREYTXT, italic = TRUE, size = 8)
doc <- doc %>% styled_par("", size = 8)

## -- MOST AFFECTED HEALTH ZONES --------------------------------------
doc <- section_bar(doc, "Most affected health zones")
zones_df <- zones %>% transmute(
  `Health zone` = zone, Province = province,
  `Cum. cases`  = format(cum_cases, big.mark = ","),
  `Cum. deaths` = format(cum_deaths, big.mark = ","),
  CFR = sapply(cfr, pc),
  `Share of national` = paste0(round(100 * cum_cases / nat_last$cum_cases, 1), "%")
)
doc <- data_table(doc, zones_df)
doc <- doc %>% styled_par(
  "Reading: cumulative case share identifies where volume is highest, not where the situation is most urgent right now -- cross-check against the Operational Watch below, which flags trajectory, not just size.",
  color = GREYTXT, italic = TRUE, size = 8)
doc <- doc %>% styled_par("", size = 8)

## -- CFR VS CASES SCATTER (now with expert narrative) -----------------
doc <- chart_section(doc, "Case-fatality ratio vs. case count \u2014 by health zone",
  cfr_scatter_plot, "cfr_scatter.png", width = 6.4, height = 3.0,
  captions = c(
    "Each point = one health zone. Size and color both encode CFR intensity.",
    "Reading: points in the upper-left (few cases, high CFR) are statistically unstable -- a single death can swing the ratio 10+ points -- and should not drive resource decisions on their own. Points in the upper-right (many cases AND high CFR) are the genuine priority: sustained lethality at volume, most likely reflecting a real access-to-care or case-management gap rather than small-sample noise."
  ))
doc <- doc %>% styled_par("", size = 6)

## -- TRANSMISSION STATUS BY ZONE -----------------------------------
if (nrow(days_since_df) > 0) {
  doc <- section_bar(doc, "Transmission status by zone \u2014 days since last new case")
  ts_df <- days_since_df %>% transmute(
    `Health zone` = zone, Province = province,
    `Last new case` = as.character(last_case_date),
    `Days since` = days_since_last_case
  )
  doc <- data_table(doc, ts_df)
  doc <- doc %>% styled_par(
    "Reading: zones nearing 21 days warrant a targeted verification (active surveillance still running, contacts fully followed up) before being deprioritised -- silence in the data can mean genuine control OR a reporting gap, and only field verification tells them apart.",
    color = GREYTXT, italic = TRUE, size = 8)
  doc <- doc %>% styled_par(
    "Descriptive indicator only. PREIS does not declare a zone or an outbreak \u2018over\u2019 \u2014 that determination rests with MoH DRC / WHO under the standard incubation-period-based protocol.",
    color = GREYTXT, italic = TRUE, size = 8)
  doc <- doc %>% styled_par("", size = 6)
}

## -- B. OPERATIONAL WATCH — PRIORITY ZONES FOR VERIFICATION -----------
doc <- section_bar(doc, "Operational watch \u2014 priority zones for verification", tone = "red")
if (nrow(ow) > 0) {
  n_shown <- min(8, nrow(ow))
  doc <- doc %>% styled_par(sprintf(
    "%d zone(s) flagged RED (immediate verification recommended) and %d flagged ORANGE (verify within 48h). Showing the top %d by severity and case volume. Flags are computed purely from case/death trajectories already in the pipeline -- they identify WHERE to look, not WHAT is wrong; field verification remains required before any conclusion.",
    n_red, n_orange, n_shown), size = 9)
  ow_df <- ow %>% slice_head(n = 8) %>% transmute(
    `Health zone` = zone, Province = province, `Sev.` = severity,
    `New (24h/7d)` = paste0(new_24h, " / ", new_7d), CFR = sapply(cfr, pc),
    Flag = .short_flag(reason)
  )
  doc <- watch_table(doc, ow_df)
  doc <- doc %>% styled_par("", size = 4)
  doc <- doc %>% styled_par("What each flag means / what to verify:", bold = TRUE, size = 9, color = MAROON)
  doc <- doc %>% bullet(paste0("Active, high CFR / Active + accelerating \u2014 new case(s) in the last 24h with CFR \u226550%. ",
    "Verify time-to-care and isolation delay; check the community-death ratio and ETC/IPC capacity in this zone."), color = BODYTXT)
  doc <- doc %>% bullet(paste0("Accelerating \u2014 7-day new cases at least doubled vs. the previous 7 days. ",
    "Investigate transmission chains and contact-tracing coverage; confirm whether new cases trace to already-listed contacts or unlisted chains."), color = BODYTXT)
  doc <- doc %>% bullet(paste0("Rising CFR \u2014 CFR rose \u226515 points over 7 days. ",
    "Check the care-seeking-delay trend and community-death ratio; confirm no backlog in sample transport or lab turnaround."), color = BODYTXT)
  doc <- doc %>% bullet(paste0("Sustained high CFR \u2014 CFR \u226540% without a new trigger. ",
    "Monitor; confirm case-management capacity is not saturated in this zone."), color = BODYTXT)
} else {
  doc <- doc %>% styled_par("No zone met a RED/ORANGE threshold at this SitRep.", size = 9)
}
doc <- doc %>% styled_par("", size = 8)

doc <- doc %>% styled_par("", size = 8)

## Section C (Operational Gaps -- village-level drill-down) has been
## REMOVED at the user's request: the underlying gap register's
## geographic fields (province/health_zone/health_area/village) are
## almost entirely "Not documented" today, so the table looked
## precise but wasn't actually reliable. Re-add it once
## app_gap_tracker_module.R's source data has real geo-enrichment --
## the code above this comment (data_table/gap_widths pattern) can be
## reused as-is at that point.

## -- DETECTED SIGNALS --------------------------------------------------
if (nrow(recent_signals) > 0) {
  doc <- section_bar(doc, "Detected signals")
  for (i in seq_len(nrow(recent_signals))) {
    s <- recent_signals[i, ]
    where <- if (!is.na(s$zone)) paste0(s$zone, " (", s$province, ")") else "National"
    sev_color <- switch(tolower(s$severity), high = MAROON, moderate = "#B8860B", BODYTXT)
    doc <- bullet(doc, sprintf("[%s] %s \u2014 %s: %s", toupper(s$severity), s$type, where, s$detail), color = sev_color)
  }
  doc <- doc %>% styled_par(
    sprintf("Most recent automated signal-detection run: %s.", max(recent_signals$detected_on)),
    color = GREYTXT, italic = TRUE, size = 8)
}

## -- FOOTER: NOTES TO THE READER + SOURCES (thin rule, not a bar) ------
doc <- footer_block(doc, "Notes to the reader", c(
  "The case-fatality ratio is always provisional during an active outbreak: recent cases may still evolve.",
  "INRB downward revisions to cumulative totals (reclassification) are detected and flagged by PREIS, never hidden or smoothed over.",
  "Data are aggregated by health zone and date; no individual line list is used for the epidemiological sections, so no sex, age or symptom-onset breakdown is available there.",
  "Operational Watch flags identify WHERE to verify, not confirmed root causes -- they are hypotheses for field teams to check, never automated diagnoses or recommendations.",
  if (zone_data_lagging) sprintf(
    "National figures (Key Updates, National Overview, epidemic curve) are current as of %s. Zone/province-level sections use the most recent available refresh (%s) -- one reporting cycle behind -- and are labelled accordingly rather than presented as same-day data.",
    format(as.Date(last_date), "%d %B %Y"), format(as.Date(zone_last_date), "%d %B %Y"))
  else sprintf("All sections in this document reflect data as of %s.", format(as.Date(last_date), "%d %B %Y"))
))
doc <- footer_block(doc, "Sources", c(
  sprintf("MoH DRC (INSP/INRB), consolidated automatically via the PREIS pipeline \u2014 SitRep No. %s, %s.", latest_sno, latest_date)
))
doc <- doc %>% styled_par("", size = 8) %>%
  styled_par(paste0(
    "This document is the DRC quantitative supplement generated automatically by PREIS on ", Sys.Date(),
    ". It does not include Uganda data or Africa CDC operational/coordination updates, ",
    "which are compiled separately by the EIU team."), color = GREYTXT, italic = TRUE, size = 8)

# ------------------------------------------------------------
# Save (resilient: falls back to a timestamped filename if the
# primary target is locked -- e.g. still open in Word -- instead
# of crashing the whole script).
# ------------------------------------------------------------
out_fp <- file.path(OUT_DIR, sprintf("PREIS_AfricaCDC_SitRep_%03d_FINAL.docx", latest_sno))
save_ok <- tryCatch({
  print(doc, target = out_fp)
  TRUE
}, error = function(e) {
  message("[sitrep-cdc-final] Could not write to ", out_fp, " (", conditionMessage(e), ").")
  FALSE
})
if (!save_ok) {
  alt_fp <- file.path(OUT_DIR, sprintf("PREIS_AfricaCDC_SitRep_%03d_FINAL_%s.docx",
                                        latest_sno, format(Sys.time(), "%Y%m%d_%H%M%S")))
  message("[sitrep-cdc-final] Target was locked (likely still open in Word) -- falling back to: ", alt_fp)
  print(doc, target = alt_fp)
  out_fp <- alt_fp
}
cat("[sitrep-cdc-final] Final document written to:", out_fp, "\n")
