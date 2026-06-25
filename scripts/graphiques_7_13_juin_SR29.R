## ============================================================
## PREIS EBOLA RDC
## graphiques_7_13_juin_SR29.R
##
## Génère 4 graphiques PNG haute résolution (300 dpi) à partir
## du tableau cas/décès par zone — semaine du 7 au 13 juin 2026
## (SitRep N°29, données au 13 juin).
##
## G1. Nouveaux cas par zone (top 10) — où est la transmission active
## G2. Nouveaux cas vs nouveaux décès par zone — fait ressortir Mongbwalu
## G3. Répartition du cumul par province (barres empilées)
## G4. Létalité vs nombre de cas (nuage) — sépare zones fiables / instables
##
## Sortie : outputs/analyse/*.png
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tidyr); library(scales)
})

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
OUT_DIR  <- file.path(BASE_DIR, "outputs", "analyse")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Données (Tableau 2, SitRep N°29, cumul 13/06) + nouveaux 7-13/06
# ------------------------------------------------------------
d <- tibble::tribble(
  ~province,   ~zone,        ~cas_13, ~deces_13, ~nouv_cas, ~nouv_deces,
  "Ituri",     "Bunia",          212,   20,  70,  6,
  "Ituri",     "Rwampara",       149,   27,  51,  8,
  "Ituri",     "Mongbwalu",      164,   73,  72, 48,
  "Ituri",     "Nyankunde",       38,    1,  14,  0,
  "Ituri",     "Bambu",            7,    2,   2,  0,
  "Ituri",     "Aru",              3,    1,   0,  0,
  "Ituri",     "Kilo",             4,    1,   0,  0,
  "Ituri",     "Nizi",            11,    1,   6,  1,
  "Ituri",     "Mangala",          5,    3,   4,  3,
  "Ituri",     "Damas",            4,    0,   1,  0,
  "Ituri",     "Aungba",           2,    1,   1,  1,
  "Ituri",     "Gety",             1,    0,   0,  0,
  "Ituri",     "Komanda",          6,    0,   3,  0,
  "Ituri",     "Lita",             6,    0,   2,  0,
  "Ituri",     "Logo",             2,    0,   0,  0,
  "Ituri",     "Mambasa",          2,    1,   0,  0,
  "Ituri",     "Rimba",            3,    0,   0,  0,
  "Ituri",     "Tchomia",          2,    0,   2,  0,
  "Ituri",     "Kambala",          1,    1,   1,  1,
  "Ituri",     "Nia-nia",          1,    1,   1,  1,
  "Nord-Kivu", "Katwa",           19,   12,   8,  4,
  "Nord-Kivu", "Beni",            14,   11,   9,  8,
  "Nord-Kivu", "Butembo",         18,    7,  14,  5,
  "Nord-Kivu", "Oicha",            2,    2,   0,  0,
  "Nord-Kivu", "Kalunguta",        2,    1,   1,  0,
  "Nord-Kivu", "Kyondo",           2,    1,   1,  1,
  "Nord-Kivu", "Goma",             1,    0,   0,  0,
  "Nord-Kivu", "Masereka",         1,    0,   1,  0,
  "Nord-Kivu", "Vuhovi",           1,    1,   1,  1,
  "Nord-Kivu", "Mabalako",         1,    0,   1,  0,
  "Sud-Kivu",  "Miti-Murhesa",     3,    1,   0,  0
) %>%
  mutate(letalite = ifelse(cas_13 > 0, round(100 * deces_13 / cas_13, 1), NA))

# Palette province cohérente
pal_prov <- c("Ituri" = "#D85A30", "Nord-Kivu" = "#185FA5", "Sud-Kivu" = "#0F6E56")

# Thème commun (lisible pour impression)
theme_preis <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30", size = 11),
    plot.caption  = element_text(color = "grey45", size = 8, hjust = 0),
    axis.title    = element_text(size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

CAP <- "Source : SitRep N°29 (données au 13/06/2026). Nouveaux cas/décès = cumul 13/06 − cumul 06/06. Létalité provisoire."

# ============================================================
# G1 — Nouveaux cas par zone (top 10)
# ============================================================
g1 <- d %>%
  filter(nouv_cas > 0) %>%
  slice_max(nouv_cas, n = 10) %>%
  ggplot(aes(x = reorder(zone, nouv_cas), y = nouv_cas, fill = province)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = nouv_cas), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_manual(values = pal_prov, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Nouveaux cas confirmés par zone de santé",
       subtitle = "Semaine du 7 au 13 juin 2026 — 10 zones les plus actives",
       x = NULL, y = "Nouveaux cas confirmés (7-13 juin)",
       caption = CAP) +
  theme_preis
ggsave(file.path(OUT_DIR, "g1_nouveaux_cas_zone.png"), g1,
       width = 9, height = 6, dpi = 300, bg = "white")

# ============================================================
# G2 — Nouveaux cas vs nouveaux décès par zone (barres groupées)
# ============================================================
g2 <- d %>%
  filter(nouv_cas > 0 | nouv_deces > 0) %>%
  slice_max(nouv_cas + nouv_deces, n = 10) %>%
  select(zone, `Nouveaux cas` = nouv_cas, `Nouveaux décès` = nouv_deces) %>%
  pivot_longer(-zone, names_to = "indicateur", values_to = "n") %>%
  ggplot(aes(x = reorder(zone, n), y = n, fill = indicateur)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("Nouveaux cas" = "#85B7EB",
                               "Nouveaux décès" = "#A32D2D"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Nouveaux cas et décès par zone de santé",
       subtitle = "Semaine du 7 au 13 juin 2026 — Mongbwalu : décès proches des cas",
       x = NULL, y = "Effectif (7-13 juin)",
       caption = CAP) +
  theme_preis
ggsave(file.path(OUT_DIR, "g2_cas_vs_deces_zone.png"), g2,
       width = 9, height = 6, dpi = 300, bg = "white")

# ============================================================
# G3 — Répartition du cumul par province (barres empilées)
# Utilise les TOTAUX OFFICIELS par province (incluant les cas
# non ventilés par zone), pas seulement la somme des zones nommées.
# Totaux SitRep 29 : Ituri 717/143, Nord-Kivu 61/35, Sud-Kivu 3/1.
# ============================================================
g3_data <- tibble::tribble(
  ~province,   ~Cas, ~Deces,
  "Ituri",       717,  143,
  "Nord-Kivu",    61,   35,
  "Sud-Kivu",      3,    1
) %>%
  rename(`Décès` = Deces) %>%
  pivot_longer(-province, names_to = "indicateur", values_to = "n")

g3 <- ggplot(g3_data, aes(x = indicateur, y = n, fill = province)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = pal_prov, name = NULL) +
  labs(title = "Répartition des cas et décès cumulés par province",
       subtitle = "Cumul au 13 juin 2026 — l'Ituri concentre l'essentiel",
       x = NULL, y = "Effectif cumulé",
       caption = CAP) +
  theme_preis
ggsave(file.path(OUT_DIR, "g3_repartition_province.png"), g3,
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# G4 — Létalité vs nombre de cas (nuage) : fiabilité du CFR
# ============================================================
# Étiquetage : ggrepel si dispo (évite chevauchements), sinon geom_text simple
has_repel <- requireNamespace("ggrepel", quietly = TRUE)
label_layer <- if (has_repel) {
  ggrepel::geom_text_repel(
    data = d %>% filter(cas_13 >= 14 | letalite >= 60),
    aes(label = zone), size = 3.5, show.legend = FALSE, max.overlaps = 20)
} else {
  geom_text(
    data = d %>% filter(cas_13 >= 14 | letalite >= 60),
    aes(label = zone), size = 3.3, vjust = -0.8, show.legend = FALSE)
}

g4 <- d %>%
  filter(cas_13 > 0) %>%
  ggplot(aes(x = cas_13, y = letalite, color = province, size = cas_13)) +
  geom_point(alpha = 0.75) +
  label_layer +
  geom_hline(yintercept = 22.9, linetype = "dashed", color = "grey40") +
  annotate("text", x = max(d$cas_13) * 0.75, y = 26,
           label = "Létalité nationale 22,9%", size = 3, color = "grey40") +
  scale_color_manual(values = pal_prov, name = NULL) +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Létalité provisoire selon le nombre de cas par zone",
       subtitle = "Les zones à gauche (peu de cas) ont une létalité statistiquement instable",
       x = "Cas confirmés cumulés (échelle log)",
       y = "Létalité provisoire (%)",
       caption = paste(CAP, "Taille des points = nombre de cas.")) +
  theme_preis
ggsave(file.path(OUT_DIR, "g4_letalite_vs_cas.png"), g4,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("\n4 graphiques générés dans :", OUT_DIR, "\n")
cat("  g1_nouveaux_cas_zone.png\n")
cat("  g2_cas_vs_deces_zone.png\n")
cat("  g3_repartition_province.png\n")
cat("  g4_letalite_vs_cas.png\n")
