## ============================================================
## PREIS EBOLA DRC
## 12_build_health_zones_geo.R
##
## Builds a lightweight GeoJSON of eastern-DRC health-zone polygons
## (Ituri, Nord-Kivu, Sud-Kivu) from the official DSNIS shapefile
## (519 zones nationwide). Simplifies geometry and harmonises zone
## names to match the INRB/SitRep data.
##
## Input  : data/curated/RDC_Zones_de_sant_.shp (+ companions .dbf .shx .prj)
## Output : data/curated/rdc_zones_sante_est.geojson  (~0.3 MB)
##
## Run once (or whenever the shapefile is updated).
## ============================================================

suppressPackageStartupMessages({ library(sf); library(dplyr) })

BASE_DIR <- Sys.getenv("GITHUB_WORKSPACE",
                       unset = "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
CURATED  <- file.path(BASE_DIR, "data", "curated")
SHP_FP   <- file.path(CURATED, "RDC_Zones_de_sant_.shp")
OUT_FP   <- file.path(CURATED, "rdc_zones_sante_est.geojson")

if (!file.exists(SHP_FP))
  stop("Shapefile introuvable : ", SHP_FP,
       "\nPlacez RDC_Zones_de_sant_.shp et ses fichiers compagnons ",
       "(.dbf .shx .prj) dans data/curated/.")

sf::sf_use_s2(FALSE)
gdf <- sf::read_sf(SHP_FP, quiet = TRUE)

# Garder les 3 provinces touchées
est <- gdf %>% filter(PROVINCE %in% c("Ituri", "Nord-Kivu", "Sud-Kivu"))

# Simplifier la géométrie (~500 m) pour alléger le dashboard
est <- sf::st_simplify(est, dTolerance = 0.005, preserveTopology = TRUE)

# Colonnes utiles + harmonisation des noms vers les données INRB/SitRep
est <- est %>%
  transmute(zone = trimws(Nom), province = PROVINCE) %>%
  mutate(zone = dplyr::recode(zone,
    "Mongbalu" = "Mongbwalu", "Gethy" = "Gety", "Nia Nia" = "Nia-nia",
    .default = zone))

if (file.exists(OUT_FP)) file.remove(OUT_FP)
sf::st_write(est, OUT_FP, driver = "GeoJSON", quiet = TRUE)

cat("Couche zones de santé Est écrite :", OUT_FP, "\n")
cat("  Zones (3 provinces) :", nrow(est), "\n")
cat("  Taille :", round(file.info(OUT_FP)$size / 1e6, 2), "Mo\n")
