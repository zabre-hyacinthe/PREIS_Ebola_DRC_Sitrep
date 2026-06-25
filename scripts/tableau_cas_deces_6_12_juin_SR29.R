## ============================================================
## PREIS EBOLA RDC
## tableau_cas_deces_6_12_juin_SR29.R
##
## Tableau des CAS CONFIRMÉS et DÉCÈS CONFIRMÉS par PROVINCE et
## ZONE DE SANTÉ — 7 derniers jours disponibles : 6 -> 12 juin 2026.
##
## SOURCES :
##   - CUMUL au 12/06 : SitRep N°29 (draft), Tableau 2 (détail par ZS).
##     Saisi manuellement depuis le document Word officiel.
##   - BASELINE au 05/06 : données INRB validées (SitRep 22), par zone,
##     pour calculer les NOUVEAUX cas/décès de la fenêtre 6-12 juin.
##
## NOTES D'EXPERT (transparence) :
##   - Le SitRep 29 indique 94 cas Ituri "en cours de ventilation" :
##     ils figurent en ligne "Autres ZS (non ventilées)" et dans les
##     sous-totaux, mais ne sont pas attribués à une zone précise.
##   - "Nia-nia" et "Mabalako" sont de nouvelles ZS sans baseline au
##     05/06 (nouveaux cas = cumul entier).
##   - Létalité = provisoire (épidémie active).
##
## Sortie : CSV + aperçu console.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr)
})

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
OUT_DIR  <- file.path(BASE_DIR, "outputs", "analyse")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1) CUMUL au 12/06/2026 — SitRep N°29, Tableau 2 (valeurs exactes)
# ------------------------------------------------------------
sr29 <- tibble::tribble(
  ~province,   ~health_zone,                     ~cas_12, ~deces_12,
  "Ituri",     "Bunia",                              212,   20,
  "Ituri",     "Rwampara",                           149,   27,
  "Ituri",     "Mongbwalu",                          164,   73,
  "Ituri",     "Nyankunde",                           38,    1,
  "Ituri",     "Bambu",                                7,    2,
  "Ituri",     "Aru",                                  3,    1,
  "Ituri",     "Kilo",                                 4,    1,
  "Ituri",     "Nizi",                                11,    1,
  "Ituri",     "Mangala",                              5,    3,
  "Ituri",     "Damas",                                4,    0,
  "Ituri",     "Aungba",                               2,    1,
  "Ituri",     "Gety",                                 1,    0,
  "Ituri",     "Komanda",                              6,    0,
  "Ituri",     "Lita",                                 6,    0,
  "Ituri",     "Logo",                                 2,    0,
  "Ituri",     "Mambasa",                              2,    1,
  "Ituri",     "Rimba",                                3,    0,
  "Ituri",     "Tchomia",                              2,    0,
  "Ituri",     "Kambala",                              1,    1,
  "Ituri",     "Nia-nia",                              1,    1,
  "Ituri",     "Autres ZS (non ventilées)",           94,   10,
  "Nord-Kivu", "Katwa",                               19,   12,
  "Nord-Kivu", "Beni",                                14,   11,
  "Nord-Kivu", "Butembo",                             18,    7,
  "Nord-Kivu", "Oicha",                                2,    2,
  "Nord-Kivu", "Kalunguta",                            2,    1,
  "Nord-Kivu", "Kyondo",                               2,    1,
  "Nord-Kivu", "Goma",                                 1,    0,
  "Nord-Kivu", "Masereka",                             1,    0,
  "Nord-Kivu", "Vuhovi",                               1,    1,
  "Nord-Kivu", "Mabalako",                             1,    0,
  "Sud-Kivu",  "Miti-Murhesa",                         3,    1
)

# ------------------------------------------------------------
# 2) BASELINE au 05/06/2026 — INRB (SitRep 22) par zone
#    (pour NOUVEAUX cas/décès de la fenêtre 6-12 juin)
# ------------------------------------------------------------
base05 <- tibble::tribble(
  ~health_zone,   ~cas_05, ~deces_05,
  "Bunia",          138,  14,  "Rwampara",  93, 19,  "Mongbwalu", 74, 21,
  "Nyankunde",       24,   1,  "Katwa",     11,  7,  "Bambu",      5,  2,
  "Nizi",             5,   0,  "Beni",       5,  3,  "Butembo",    4,  2,
  "Kilo",             4,   1,  "Lita",       4,  0,  "Miti-Murhesa",3, 1,
  "Aru",              3,   1,  "Damas",      3,  0,  "Rimba",      3,  0,
  "Komanda",          3,   0,  "Oicha",      2,  2,  "Mambasa",    2,  1,
  "Logo",             2,   0,  "Goma",       1,  0,  "Kalunguta",  1,  1,
  "Kyondo",           1,   0,  "Mangala",    1,  0,  "Aungba",     1,  0,
  "Gety",             1,   0
)

# ------------------------------------------------------------
# 3) Assemblage : cumul 12/06 + nouveaux (12/06 - 05/06)
#    IMPORTANT : la ligne "Autres ZS (non ventilées)" est un reliquat
#    cumulé NON attribué à une zone — ce ne sont PAS des cas survenus
#    pendant la semaine. On NE la compte donc PAS dans les nouveaux
#    (sinon on gonfle l'incidence de +94). Vérif : la somme des nouveaux
#    par zone = 293 cas / 93 décès = national 781-488 / 179-86. OK.
# ------------------------------------------------------------
tab <- sr29 %>%
  left_join(base05, by = "health_zone") %>%
  mutate(
    cas_05    = coalesce(cas_05, 0),
    deces_05  = coalesce(deces_05, 0),
    is_autres = grepl("Autres ZS", health_zone),
    nouv_cas   = ifelse(is_autres, NA_real_, pmax(cas_12   - cas_05,   0)),
    nouv_deces = ifelse(is_autres, NA_real_, pmax(deces_12 - deces_05, 0)),
    letalite   = ifelse(cas_12 > 0, round(100 * deces_12 / cas_12, 1), NA)
  )

tableau <- tab %>%
  transmute(
    Province = province,
    `Zone de santé` = health_zone,
    `Cas confirmés (cumul 12/06)`   = cas_12,
    `Décès confirmés (cumul 12/06)` = deces_12,
    `Létalité (%) provisoire`       = letalite,
    `Nouveaux cas (06-12/06)`       = ifelse(is.na(nouv_cas),   "n/d", as.character(nouv_cas)),
    `Nouveaux décès (06-12/06)`     = ifelse(is.na(nouv_deces), "n/d", as.character(nouv_deces))
  )

# ------------------------------------------------------------
# 4) Totaux par province + national
# ------------------------------------------------------------
tot_prov <- tab %>%
  group_by(province) %>%
  summarise(
    `Cas confirmés (cumul 12/06)`   = sum(cas_12),
    `Décès confirmés (cumul 12/06)` = sum(deces_12),
    `Nouveaux cas (06-12/06)`       = sum(nouv_cas,   na.rm = TRUE),
    `Nouveaux décès (06-12/06)`     = sum(nouv_deces, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(`Létalité (%) provisoire` =
           round(100 * `Décès confirmés (cumul 12/06)` /
                   `Cas confirmés (cumul 12/06)`, 1)) %>%
  rename(Province = province) %>%
  mutate(`Zone de santé` = "-- SOUS-TOTAL --",
         `Nouveaux cas (06-12/06)`   = as.character(`Nouveaux cas (06-12/06)`),
         `Nouveaux décès (06-12/06)` = as.character(`Nouveaux décès (06-12/06)`)) %>%
  select(Province, `Zone de santé`, `Cas confirmés (cumul 12/06)`,
         `Décès confirmés (cumul 12/06)`, `Létalité (%) provisoire`,
         `Nouveaux cas (06-12/06)`, `Nouveaux décès (06-12/06)`)

tot_nat <- tibble(
  Province = "TOTAL NATIONAL", `Zone de santé` = "-- 3 provinces --",
  `Cas confirmés (cumul 12/06)`   = sum(tab$cas_12),
  `Décès confirmés (cumul 12/06)` = sum(tab$deces_12),
  `Létalité (%) provisoire`       = round(100*sum(tab$deces_12)/sum(tab$cas_12),1),
  `Nouveaux cas (06-12/06)`       = as.character(sum(tab$nouv_cas,   na.rm = TRUE)),
  `Nouveaux décès (06-12/06)`     = as.character(sum(tab$nouv_deces, na.rm = TRUE))
)

tableau_complet <- bind_rows(tableau, tot_prov, tot_nat)

out_fp <- file.path(OUT_DIR, "tableau_cas_deces_6_12_juin_2026_SR29.csv")
# Écriture avec BOM UTF-8 : force Excel à lire les accents correctement
# (sinon "é" s'affiche "Ã©"). readr::write_excel_csv ajoute le BOM.
readr::write_excel_csv(tableau_complet, out_fp)

# ------------------------------------------------------------
# 5) Aperçu console
# ------------------------------------------------------------
cat("\n==================================================================\n")
cat("CAS & DÉCÈS CONFIRMÉS PAR PROVINCE ET ZONE DE SANTÉ\n")
cat("Cumul au 12 juin 2026 (SitRep N°29) | Nouveaux : fenêtre 6-12 juin\n")
cat("==================================================================\n\n")
print(as.data.frame(tableau), row.names = FALSE)
cat("\n--- SOUS-TOTAUX PAR PROVINCE ---\n")
print(as.data.frame(tot_prov), row.names = FALSE)
cat("\n--- TOTAL NATIONAL ---\n")
print(as.data.frame(tot_nat), row.names = FALSE)

cat("\nTableau sauvegardé :", out_fp, "\n")
cat("\nNOTES :\n")
cat(" - 94 cas Ituri 'en cours de ventilation' (non attribués à une ZS).\n")
cat(" - Nia-nia & Mabalako : nouvelles ZS (pas de baseline au 05/06).\n")
cat(" - Létalité provisoire (épidémie active) ; cumuls = SitRep 29 draft.\n")
