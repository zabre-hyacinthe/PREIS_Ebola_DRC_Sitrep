# Reproducibility — PREIS Ebola DRC manuscript (JMIR)

This folder lets a reviewer regenerate every table and figure in the manuscript.

## Quick start (reproduces all tables & figures)

```bash
Rscript reproduce_manuscript.R
```

Outputs are written to `manuscript_repro/`:

| Manuscript item | Output file | Produced from |
|---|---|---|
| Table 1 — rules & thresholds | `table1_rules.csv` | rule definitions (mirror of `13_signal_detection.R`) |
| Table 2 — signals detected | `table2_signals.csv` | `PREIS_validation_signals.csv` (from `14_retrospective_validation.R`) |
| Table 3 — Mongbwalu progression | `table3_mongbwalu.csv` | zone-level series |
| Figure 2 — national epidemic curve | `Fig2_epidemic_curve.png` | national time series |
| Figure 3 — signal timeline | `Fig3_signal_timeline.png` | `PREIS_validation_signals.csv` |
| Figure 4 — Mongbwalu case study | `Fig4_mongbwalu.png` | zone-level series |

Figure 1 (architecture) is a conceptual diagram, provided as a static image.

## Data resolution order

The script tries, in order:
1. Local pipeline outputs (`data/final/…`, `outputs/analyse/…`)
2. The public GitHub raw copies (`PREIS_GH_RAW_BASE`)
3. Embedded manuscript-reported values (so the script always runs end-to-end)

To force a specific repository/branch:

```bash
PREIS_GH_RAW_BASE="https://raw.githubusercontent.com/<user>/<repo>/refs/heads/main" \
  Rscript reproduce_manuscript.R
```

## Full production system (beyond the manuscript)

The complete real-time surveillance pipeline is organised as:

| Script | Role |
|---|---|
| `00_PREIS_MASTER_AUTOMATION.R` | Scrape INSP, decode PDF URL, download, extract |
| `08_cloud_sitrep_monitor.R` | Cloud entry point: detect new SitRep, disseminate, run analysis |
| `11_daily_indicators.R` | Daily indicators (cases, deaths, CFR, moving averages) |
| `13_signal_detection.R` | Six early-warning rules (Table 1) |
| `14_retrospective_validation.R` | Day-by-day replay; produces `PREIS_validation_signals.csv` (Table 2, Fig 3) |
| `03_analyse_consolidee.R` | Consolidated national/zone series (Fig 2) |
| `12_build_health_zones_geo.R` | Health-zone geospatial layer (map) |
| `04_send_sitrep_alerts_conditional.R` | HTML analytical alert email |
| `dashboard_ebola/app.R` | Interactive dashboard (reads data live from GitHub) |

These run automatically every 30 minutes via the cloud workflow
(`preis_sitrep_monitor.yml`). The manuscript reproduction script above is a
standalone subset focused on the published tables and figures.

## Environment

- R >= 4.2
- Packages: `ggplot2`, `dplyr`, `readr`, `tidyr`, `scales` (auto-installed if missing)

## Notes

- All CFRs are provisional (cumulative counts, active outbreak).
- National series and Mongbwalu progression should be confirmed against the
  INSP/INRB validated figures before submission.
