## ============================================================
## PREIS Ebola DRC — Operational Watch helper (shared)
## operational_watch.R
##
## Computes a zone-level "priority verification" flag using ONLY
## data already in PREIS_daily_indicators.csv (cases/deaths/CFR by
## zone/date) -- no new source connector needed. This is a lighter
## complement to your existing patient-level gap tracker
## (app_gap_tracker_module.R, which drills to health_area/village
## using source_line_list/*.csv) -- it works at zone level, on the
## aggregated SitRep data, so it is available even when the
## line-list files are not loaded.
##
## RULES (each framed as something to VERIFY, never a diagnosis --
## consistent with PREIS's stated principle: facts + hypotheses,
## never automated conclusions):
##
##   RED   -- new case(s) in the last 24h AND CFR >= 50%
##            "High-lethality active transmission"
##   RED   -- 7-day new cases >= 2x the previous 7 days
##            (cumulative cases >= min_cases, to avoid noise on
##            tiny zones) -- "Rapid case acceleration"
##   ORANGE-- CFR rose >= 15 points over the last 7 days
##            (cumulative cases >= min_cases) -- "Rising lethality"
##   ORANGE-- CFR >= 40% sustained, cumulative cases >= min_cases,
##            no other flag already raised -- "Sustained high
##            lethality"
##
## Used by:
##   - 06_generate_africa_cdc_sitrep_final.R (full table + actions)
##   - 04_send_sitrep_alerts_conditional.R  (short bullet list)
##
## source() this file before calling compute_operational_watch().
## ============================================================

suppressPackageStartupMessages({ library(dplyr) })

.ow_action_for <- function(reason) {
  # Verification prompts, not directives -- matches PREIS's
  # "hypotheses to investigate, never a diagnosis" principle.
  dplyr::case_when(
    grepl("High-lethality active transmission", reason) ~
      "Verify time-to-care and isolation delay; check the community-death ratio and ETC/IPC capacity in this zone.",
    grepl("Rapid case acceleration", reason) ~
      "Investigate transmission chains and contact-tracing coverage; confirm whether new cases come from already-listed contacts or unlisted chains.",
    grepl("Rising lethality", reason) ~
      "Check the care-seeking-delay trend and community-death ratio; confirm no backlog in sample transport or lab turnaround.",
    grepl("Sustained high lethality", reason) ~
      "Monitor; confirm case-management capacity is not saturated in this zone.",
    TRUE ~ "Verify with field team."
  )
}

#' Compute the zone-level operational watch table.
#' @param daily The PREIS_daily_indicators.csv data (level/province/zone/date/cum_cases/cum_deaths/new_cases/new_deaths/cfr).
#' @param last_date The latest report date (as.Date or ISO string).
#' @param min_cases Minimum cumulative cases for a zone to be eligible for acceleration/lethality flags (avoids noise on 1-2 case zones).
#' @return data.frame: zone, province, cum_cases, cfr, new_24h, new_7d, severity ("RED"/"ORANGE"), reason, action.
compute_operational_watch <- function(daily, last_date, min_cases = 10) {
  last_date <- as.Date(last_date)
  zones <- daily %>%
    dplyr::filter(level == "Zone") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::arrange(zone, province, date)

  out <- list()
  for (key in split(seq_len(nrow(zones)), paste(zones$zone, zones$province))) {
    g <- zones[key, ]
    latest <- g[g$date == last_date, ]
    if (nrow(latest) == 0 || is.na(latest$cum_cases[1]) || latest$cum_cases[1] <= 0) next
    cum   <- latest$cum_cases[1]
    cfr_now <- latest$cfr[1]
    new24 <- ifelse(is.na(latest$new_cases[1]), 0, latest$new_cases[1])

    g7     <- g[g$date > last_date - 7, ]
    gprev7 <- g[g$date <= last_date - 7 & g$date > last_date - 14, ]
    new7     <- sum(pmax(g7$new_cases, 0), na.rm = TRUE)
    newprev7 <- sum(pmax(gprev7$new_cases, 0), na.rm = TRUE)

    g_7ago  <- g[g$date <= last_date - 7, ]
    cfr_7ago <- if (nrow(g_7ago) > 0) tail(g_7ago$cfr, 1) else NA

    flags <- character()
    if (new24 > 0 && !is.na(cfr_now) && cfr_now >= 50)
      flags <- c(flags, "High-lethality active transmission")
    if (newprev7 > 0 && new7 >= 2 * newprev7 && cum >= min_cases)
      flags <- c(flags, "Rapid case acceleration (7d doubling)")
    if (!is.na(cfr_7ago) && !is.na(cfr_now) && (cfr_now - cfr_7ago) >= 15 && cum >= min_cases)
      flags <- c(flags, sprintf("Rising lethality (+%.0fpts/7d)", cfr_now - cfr_7ago))
    if (length(flags) == 0 && !is.na(cfr_now) && cfr_now >= 40 && cum >= min_cases)
      flags <- c(flags, "Sustained high lethality")

    if (length(flags) > 0) {
      severity <- if (any(grepl("High-lethality active transmission|Rapid case acceleration", flags))) "RED" else "ORANGE"
      reason <- paste(flags, collapse = "; ")
      out[[length(out) + 1]] <- data.frame(
        zone = g$zone[1], province = g$province[1], cum_cases = cum, cfr = cfr_now,
        new_24h = new24, new_7d = new7, severity = severity, reason = reason,
        action = .ow_action_for(reason), stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) return(data.frame())
  res <- dplyr::bind_rows(out)
  res$sev_order <- ifelse(res$severity == "RED", 1, 2)
  res <- res %>% dplyr::arrange(sev_order, dplyr::desc(cum_cases)) %>% dplyr::select(-sev_order)
  res
}
