## ============================================================
## PREIS Ebola RDC — DEPLOIEMENT DU DASHBOARD SUR shinyapps.io
## ------------------------------------------------------------
## A LANCER SUR TON ORDINATEUR (PAS dans le cloud GitHub).
## Ce script configure rsconnect et deploie le dashboard.
##
## PREREQUIS (a faire UNE SEULE FOIS) :
##  1. Creer un compte gratuit sur https://www.shinyapps.io
##  2. Une fois connecte : Account -> Tokens -> Show -> Copy
##     Tu obtiens 3 valeurs : name (=account), token, secret
##  3. Colle ces 3 valeurs ci-dessous (entre les guillemets).
## ============================================================

# ---- 1. Identifiants shinyapps.io (a remplir UNE fois) ----
ACCOUNT <- "TON_NOM_DE_COMPTE"      # ex: "hyacinthe-zabre"
TOKEN   <- "TON_TOKEN"              # la longue chaine "token"
SECRET  <- "TON_SECRET"            # la longue chaine "secret"

# ---- 2. Chemin local du dossier dashboard ----
APP_DIR <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26/dashboard_ebola"

# ---- 3. Nom de l'application en ligne (apparaitra dans l'URL) ----
# NOUVEAU deploiement (lien neuf, propre). Tu peux garder ce nom.
APP_NAME <- "preis-ebola-drc-v2"   # URL -> https://TON_COMPTE.shinyapps.io/preis-ebola-drc-v2/

# ============================================================
# A partir d'ici, ne rien changer : le script fait le travail.
# ============================================================

cat("=== Deploiement PREIS dashboard sur shinyapps.io ===\n\n")

# Installer rsconnect si absent
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Installation de rsconnect...\n")
  install.packages("rsconnect")
}
library(rsconnect)

# Verifier que le dossier existe
if (!dir.exists(APP_DIR)) {
  stop("Dossier introuvable : ", APP_DIR,
       "\n-> Corrige APP_DIR en haut du script.")
}

# Verifier que app.R est present
if (!file.exists(file.path(APP_DIR, "app.R"))) {
  stop("app.R introuvable dans ", APP_DIR)
}

# Verifier que les donnees sont presentes (sinon le dashboard sera vide)
data_dir <- file.path(APP_DIR, "data")
if (!dir.exists(data_dir) || length(list.files(data_dir, recursive = TRUE)) == 0) {
  cat("ATTENTION : le dossier data/ du dashboard semble vide.\n")
  cat("  Lance d'abord prepare_dashboard_data.R, ou recupere les donnees\n")
  cat("  depuis GitHub, sinon le dashboard en ligne n'aura pas de donnees.\n\n")
}

# Configurer le compte (une fois ; idempotent)
cat("Configuration du compte shinyapps.io...\n")
rsconnect::setAccountInfo(name = ACCOUNT, token = TOKEN, secret = SECRET)

# Lister les fichiers a deployer (app + data + modules, PAS les gros bruts)
cat("Preparation du deploiement...\n")
app_files <- list.files(APP_DIR, recursive = TRUE)
# Exclure d'eventuels gros fichiers inutiles en ligne (shapefiles bruts, etc.)
exclude_patterns <- c("\\.shp$", "\\.shx$", "\\.dbf$", "\\.prj$",
                      "\\.Rproj$", "\\.git", "rsconnect/")
keep <- app_files[!Reduce(`|`, lapply(exclude_patterns, function(p) grepl(p, app_files)))]
cat("Fichiers a deployer :", length(keep), "\n\n")

# Deployer
cat("Deploiement en cours (cela peut prendre 2-5 minutes)...\n")
rsconnect::deployApp(
  appDir = APP_DIR,
  appName = APP_NAME,
  appFiles = keep,
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("\n=== TERMINE ===\n")
cat("Ton dashboard est en ligne a l'adresse :\n")
cat(sprintf("  https://%s.shinyapps.io/%s/\n", ACCOUNT, APP_NAME))
