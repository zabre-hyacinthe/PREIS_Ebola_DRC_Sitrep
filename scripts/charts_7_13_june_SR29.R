## ============================================================
## PREIS EBOLA DRC
## charts_7_13_june_SR29.R
##
## Generates 5 publication-quality figures (300 dpi, English) from
## the case/death table by health zone, week of 7-13 June 2026
## (SitRep N29, data as of 13 June).
##
## F1. New confirmed cases by health zone (top 10)
## F2. New cases vs new deaths by health zone (grouped)
## F3. Cumulative cases & deaths by province (stacked)
## F4. Case-fatality ratio vs case count (scatter, log x)
## F5. REAL MAP of DRC with proportional-symbol health zones
##
## Output: outputs/analyse/*.png
##
## Map note: zone coordinates from the project geo file (7 zones)
## plus Beni (verified). Zones without reliable coordinates are
## listed in the caption -- NOT invented.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tidyr); library(scales)
})
has_sf     <- requireNamespace("sf", quietly = TRUE)
has_repel  <- requireNamespace("ggrepel", quietly = TRUE)

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
OUT_DIR  <- file.path(BASE_DIR, "outputs", "analyse")
GEO_DIR  <- file.path(BASE_DIR, "data", "curated")     # africa_countries_rcc.geojson
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Data (SitRep N29, Table 2, cumulative as of 13/06) + new 7-13/06
# ------------------------------------------------------------
d <- tibble::tribble(
  ~province,    ~zone,        ~cum_cases, ~cum_deaths, ~new_cases, ~new_deaths,
  "Ituri",      "Bunia",          212,  20,  70,  6,
  "Ituri",      "Rwampara",       149,  27,  51,  8,
  "Ituri",      "Mongbwalu",      164,  73,  72, 48,
  "Ituri",      "Nyankunde",       38,   1,  14,  0,
  "Ituri",      "Bambu",            7,   2,   2,  0,
  "Ituri",      "Aru",              3,   1,   0,  0,
  "Ituri",      "Kilo",             4,   1,   0,  0,
  "Ituri",      "Nizi",            11,   1,   6,  1,
  "Ituri",      "Mangala",          5,   3,   4,  3,
  "Ituri",      "Damas",            4,   0,   1,  0,
  "Ituri",      "Aungba",           2,   1,   1,  1,
  "Ituri",      "Gety",             1,   0,   0,  0,
  "Ituri",      "Komanda",          6,   0,   3,  0,
  "Ituri",      "Lita",             6,   0,   2,  0,
  "Ituri",      "Logo",             2,   0,   0,  0,
  "Ituri",      "Mambasa",          2,   1,   0,  0,
  "Ituri",      "Rimba",            3,   0,   0,  0,
  "Ituri",      "Tchomia",          2,   0,   2,  0,
  "Ituri",      "Kambala",          1,   1,   1,  1,
  "Ituri",      "Nia-nia",          1,   1,   1,  1,
  "North Kivu", "Katwa",           19,  12,   8,  4,
  "North Kivu", "Beni",            14,  11,   9,  8,
  "North Kivu", "Butembo",         18,   7,  14,  5,
  "North Kivu", "Oicha",            2,   2,   0,  0,
  "North Kivu", "Kalunguta",        2,   1,   1,  0,
  "North Kivu", "Kyondo",           2,   1,   1,  1,
  "North Kivu", "Goma",             1,   0,   0,  0,
  "North Kivu", "Masereka",         1,   0,   1,  0,
  "North Kivu", "Vuhovi",           1,   1,   1,  1,
  "North Kivu", "Mabalako",         1,   0,   1,  0,
  "South Kivu", "Miti-Murhesa",     3,   1,   0,  0
) %>%
  mutate(cfr = ifelse(cum_cases > 0, round(100 * cum_deaths / cum_cases, 1), NA))

# Africa CDC / African Union official palette: AU Green & AU Red
# (Pan-African references: green #00843E, red #E31C23) + gold accent.
AU_GREEN      <- "#00843E"   # AU Green
AU_GREEN_DARK <- "#005A2B"   # darker green (print legibility)
AU_RED        <- "#E31C23"   # AU Red
AU_RED_DARK   <- "#A3151A"   # darker red
AU_GOLD       <- "#F0B323"   # AU Gold (accent / third province)

# Province palette in Africa CDC colours (Ituri = main focus = red,
# North Kivu = green, South Kivu = gold accent)
pal_prov <- c("Ituri" = AU_RED, "North Kivu" = AU_GREEN, "South Kivu" = AU_GOLD)

# Scientific theme
theme_sci <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#1a1a1a"),
    plot.subtitle = element_text(colour = "grey35", size = 11, margin = margin(b = 8)),
    plot.caption  = element_text(colour = "grey45", size = 8, hjust = 0,
                                 margin = margin(t = 10)),
    axis.title    = element_text(size = 11, colour = "grey25"),
    axis.text     = element_text(colour = "grey30"),
    legend.position = "top",
    legend.title  = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92"),
    plot.margin   = margin(15, 20, 12, 15)
  )

CAP <- paste0("Source: SitRep N29 (data as of 13 June 2026). ",
              "New cases/deaths = cumulative 13 Jun minus cumulative 06 Jun. ",
              "CFR is provisional (active outbreak).")

save_png <- function(plot, name, w = 9, h = 6)
  ggsave(file.path(OUT_DIR, name), plot, width = w, height = h, dpi = 300, bg = "white")

# ============================================================
# F1 - New confirmed cases by health zone (top 10)
# ============================================================
f1 <- d %>% filter(new_cases > 0) %>% slice_max(new_cases, n = 10) %>%
  ggplot(aes(reorder(zone, new_cases), new_cases, fill = province)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = new_cases), hjust = -0.25, size = 4, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(values = pal_prov) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "New confirmed Ebola cases by health zone",
       subtitle = "Week of 7-13 June 2026 - ten most active health zones",
       x = NULL, y = "New confirmed cases (7-13 June)", caption = CAP) +
  theme_sci
save_png(f1, "F1_new_cases_by_zone.png")

# ============================================================
# F2 - New cases vs new deaths by zone (grouped)
# ============================================================
f2 <- d %>% filter(new_cases > 0 | new_deaths > 0) %>%
  slice_max(new_cases + new_deaths, n = 10) %>%
  select(zone, `New cases` = new_cases, `New deaths` = new_deaths) %>%
  pivot_longer(-zone, names_to = "indicator", values_to = "n") %>%
  ggplot(aes(reorder(zone, n), n, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75),
            hjust = -0.25, size = 3.4, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(values = c("New cases" = AU_GREEN, "New deaths" = AU_RED)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "New cases versus new deaths by health zone",
       subtitle = "Week of 7-13 June 2026 - Mongbwalu shows deaths nearly matching cases",
       x = NULL, y = "Count (7-13 June)", caption = CAP) +
  theme_sci
save_png(f2, "F2_cases_vs_deaths.png")

# ============================================================
# F3 - Cumulative cases & deaths by province (stacked, official totals)
# ============================================================
f3 <- tibble::tribble(
  ~province,    ~Cases, ~Deaths,
  "Ituri",        717,   143,
  "North Kivu",    61,    35,
  "South Kivu",     3,     1
) %>%
  pivot_longer(-province, names_to = "indicator", values_to = "n") %>%
  ggplot(aes(indicator, n, fill = province)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            size = 4, colour = "white", fontface = "bold") +
  scale_fill_manual(values = pal_prov) +
  labs(title = "Cumulative confirmed cases and deaths by province",
       subtitle = "As of 13 June 2026 - Ituri accounts for the large majority",
       x = NULL, y = "Cumulative count", caption = CAP) +
  theme_sci
save_png(f3, "F3_province_totals.png", w = 8)

# ============================================================
# F4 - CFR vs case count (scatter, log x) : reliability of CFR
# ============================================================
lab_pts <- d %>% filter(cum_cases >= 14 | cfr >= 60)
label_layer <- if (has_repel) {
  ggrepel::geom_text_repel(
    data = lab_pts, aes(label = zone),
    size = 3.6, fontface = "bold", show.legend = FALSE,
    max.overlaps = Inf, box.padding = 0.7, point.padding = 0.4,
    min.segment.length = 0, segment.color = "grey70", segment.size = 0.3,
    force = 3, seed = 42)
} else {
  geom_text(data = lab_pts, aes(label = zone), size = 3.3,
            fontface = "bold", vjust = -1.1, show.legend = FALSE,
            check_overlap = TRUE)
}

f4 <- d %>% filter(cum_cases > 0) %>%
  ggplot(aes(cum_cases, cfr, colour = province, size = cum_cases)) +
  geom_hline(yintercept = 22.9, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 150, y = 26.5, label = "National CFR 22.9%",
           size = 3.2, colour = "grey45", fontface = "italic") +
  geom_point(alpha = 0.85, stroke = 0.6) +
  geom_point(shape = 21, fill = NA, colour = "white", stroke = 0.5,
             show.legend = FALSE) +
  label_layer +
  scale_colour_manual(values = pal_prov) +
  scale_size_continuous(range = c(2.5, 14), guide = "none") +
  scale_x_log10(expand = expansion(mult = c(0.08, 0.12))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  labs(title = "Provisional case-fatality ratio versus case count",
       subtitle = "Zones on the left (few cases) have statistically unstable CFR",
       x = "Cumulative confirmed cases (log scale)",
       y = "Provisional CFR (%)",
       caption = NULL) +
  theme_sci + theme(legend.position = "top")
save_png(f4, "F4_cfr_vs_cases.png")

# ============================================================
# F5 - REAL MAP of DRC, proportional symbols by health zone
# ============================================================
# Verified coordinates: 7 from project geo file + Beni (web-verified).
coords <- tibble::tribble(
  ~zone,        ~lat,   ~lon,
  "Mongbwalu",  1.95,  30.03,
  "Nyankunde",  1.31,  29.56,
  "Rwampara",   1.56,  30.25,
  "Bunia",      1.57,  30.25,
  "Butembo",    0.15,  29.28,
  "Katwa",      0.10,  29.25,
  "Goma",      -1.68,  29.23,
  "Beni",       0.50,  29.47
)
map_d <- d %>% inner_join(coords, by = "zone")
n_mapped <- nrow(map_d); n_total <- sum(d$cum_cases > 0)

if (has_sf) {
  # Cherche le geojson à plusieurs emplacements possibles (racine OU dashboard)
  geo_candidates <- c(
    file.path(GEO_DIR, "africa_countries_rcc.geojson"),
    file.path(BASE_DIR, "data", "curated", "africa_countries_rcc.geojson"),
    file.path(BASE_DIR, "dashboard_ebola", "data", "curated", "africa_countries_rcc.geojson"),
    file.path(BASE_DIR, "africa_countries_rcc.geojson")
  )
  geo_fp <- geo_candidates[file.exists(geo_candidates)][1]
  if (!is.na(geo_fp) && length(geo_fp) == 1 && file.exists(geo_fp)) {
    sf::sf_use_s2(FALSE)
    africa <- sf::st_read(geo_fp, quiet = TRUE)
    drc <- africa[africa$iso3 == "COD", ]
    neighbours <- africa[africa$iso3 %in% c("UGA","RWA","BDI","SSD","TZA","COD"), ]

    f5 <- ggplot() +
      geom_sf(data = neighbours, fill = "grey96", colour = "grey80", linewidth = 0.3) +
      geom_sf(data = drc, fill = "#EAF3EC", colour = AU_GREEN_DARK, linewidth = 0.6) +
      geom_point(data = map_d,
                 aes(lon, lat, size = cum_cases, colour = cfr), alpha = 0.85) +
      scale_size_continuous(range = c(3, 18), name = "Cumulative cases") +
      scale_colour_gradient(low = AU_GREEN, high = AU_RED, name = "Provisional CFR (%)") +
      coord_sf(xlim = c(28.5, 31.2), ylim = c(-2.2, 2.6), expand = FALSE) +
      labs(title = "Geographic distribution of Ebola in eastern DRC",
           subtitle = paste0("Health zones by cumulative cases and CFR, as of 13 June 2026 (",
                             n_mapped, " of ", n_total, " affected zones mapped)"),
           x = NULL, y = NULL,
           caption = paste0(CAP,
             " Map shows zones with verified coordinates; smaller zones without ",
             "reliable coordinates are omitted (not invented).")) +
      theme_sci +
      theme(panel.grid.major = element_line(colour = "grey94"),
            legend.position = "right", legend.title = element_text(size = 9))

    if (has_repel) {
      f5 <- f5 + ggrepel::geom_text_repel(data = map_d, aes(lon, lat, label = zone),
                                          size = 3.2, colour = "grey20", max.overlaps = 20)
    } else {
      f5 <- f5 + geom_text(data = map_d, aes(lon, lat, label = zone),
                           size = 3, colour = "grey20", vjust = -1.2)
    }

    save_png(f5, "F5_map_drc_health_zones.png", w = 9, h = 8)
    cat("F5 map written.\n")
  } else {
    cat("WARNING: africa_countries_rcc.geojson introuvable - carte F5 sautee.\n")
    cat("  Emplacements cherches :\n")
    for (cand in geo_candidates) cat("   -", cand, "\n")
  }
} else {
  cat("WARNING: package 'sf' not installed - F5 map skipped.\n")
  cat("  Install with: install.packages('sf')\n")
}

cat("\nFigures written to:", OUT_DIR, "\n")
cat("  F1_new_cases_by_zone.png\n  F2_cases_vs_deaths.png\n")
cat("  F3_province_totals.png\n  F4_cfr_vs_cases.png\n  F5_map_drc_health_zones.png\n")
