# ============================================================
# 90_audit_systeme_preis.R
#
# PREIS Ebola DRC — AUDIT COMPLET DU SYSTÈME
#
# But : produire un inventaire complet et lisible de tout le
# système (scripts, données, indicateurs, sorties, dashboard,
# géographie) pour faciliter les révisions, ajustements et ajouts.
#
# CE SCRIPT NE MODIFIE RIEN. Il lit et inspecte uniquement.
#
# Sortie :
#   - outputs/audit/AUDIT_SYSTEME_PREIS.txt   (rapport complet)
#   - affichage console synthétique
#
# Usage :
#   setwd("D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26")
#   source("scripts/90_audit_systeme_preis.R")
#
# Auteur : Dr R. Hyacinthe ZABRE — PREIS / Africa CDC
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
})

BASE_DIR <- Sys.getenv("PREIS_ROOT", unset = getwd())
OUT_DIR  <- file.path(BASE_DIR, "outputs", "audit")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
REPORT   <- file.path(OUT_DIR, "AUDIT_SYSTEME_PREIS.txt")

# Collecteur de lignes (console + fichier)
.buf <- character(0)
say <- function(...) {
  line <- paste0(...)
  .buf[[length(.buf) + 1]] <<- line
  cat(line, "\n")
}
hr  <- function() say(strrep("=", 60))
sub <- function() say(strrep("-", 60))

# Helpers de formatage
human_size <- function(bytes) {
  if (is.na(bytes)) return("?")
  u <- c("o", "Ko", "Mo", "Go"); i <- 1
  while (bytes >= 1024 && i < length(u)) { bytes <- bytes / 1024; i <- i + 1 }
  sprintf("%.1f %s", bytes, u[i])
}
exists_dir <- function(p) dir.exists(file.path(BASE_DIR, p))
safe_read <- function(p) tryCatch(
  suppressWarnings(readr::read_csv(file.path(BASE_DIR, p), show_col_types = FALSE)),
  error = function(e) NULL)

hr()
say("PREIS EBOLA DRC — AUDIT COMPLET DU SYSTÈME")
say("Genere le : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say("Racine    : ", BASE_DIR)
hr()
say("")

# ------------------------------------------------------------
# 1. ARBORESCENCE PRINCIPALE
# ------------------------------------------------------------
say("1. ARBORESCENCE PRINCIPALE")
sub()
dirs_attendus <- c("scripts", "data", "data/final", "data/pdf",
                   "data/monitor_state", "data/curated",
                   "outputs", "outputs/analyse", "outputs/validation",
                   "dashboard_ebola", "dashboard_ebola/data",
                   ".github/workflows", "logs")
for (d in dirs_attendus) {
  status <- if (exists_dir(d)) "[OK]   " else "[ABSENT]"
  n <- if (exists_dir(d))
    length(list.files(file.path(BASE_DIR, d), recursive = FALSE)) else 0
  say(sprintf("  %s %-28s %s", status, d,
              if (exists_dir(d)) paste0("(", n, " elements)") else ""))
}
say("")

# ------------------------------------------------------------
# 2. SCRIPTS R
# ------------------------------------------------------------
say("2. SCRIPTS R (dossier scripts/)")
sub()
sc_dir <- file.path(BASE_DIR, "scripts")
if (dir.exists(sc_dir)) {
  scripts <- list.files(sc_dir, pattern = "\\.R$", full.names = TRUE)
  say(sprintf("  Total : %d scripts R", length(scripts)))
  say("")
  for (s in sort(scripts)) {
    nm <- basename(s)
    info <- file.info(s)
    nlines <- tryCatch(length(readLines(s, warn = FALSE)), error = function(e) NA)
    # Premiere ligne de commentaire descriptive (apres le shebang/encoding)
    desc <- tryCatch({
      ls <- readLines(s, n = 8, warn = FALSE)
      ls <- ls[grepl("^#", ls)]
      ls <- ls[!grepl("^#\\s*=+\\s*$", ls)]
      if (length(ls) > 0) trimws(sub("^#+\\s*", "", ls[1])) else ""
    }, error = function(e) "")
    say(sprintf("  %-42s %5s lignes  %8s",
                nm, ifelse(is.na(nlines), "?", nlines),
                human_size(info$size)))
    if (nzchar(desc)) say(sprintf("      -> %s", substr(desc, 1, 70)))
  }
} else {
  say("  [ABSENT] dossier scripts/ introuvable")
}
say("")

# ------------------------------------------------------------
# 3. WORKFLOW GITHUB ACTIONS
# ------------------------------------------------------------
say("3. AUTOMATISATION (GitHub Actions)")
sub()
wf_dir <- file.path(BASE_DIR, ".github", "workflows")
if (dir.exists(wf_dir)) {
  wfs <- list.files(wf_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(wfs) == 0) say("  [ATTENTION] aucun fichier .yml trouve")
  for (w in wfs) {
    say(sprintf("  [OK] %s", basename(w)))
    cont <- tryCatch(readLines(w, warn = FALSE), error = function(e) character(0))
    cron <- cont[grepl("cron:", cont)]
    if (length(cron)) say(sprintf("       cron : %s", trimws(cron[1])))
    perm <- cont[grepl("contents:", cont)]
    if (length(perm)) say(sprintf("       permissions : %s", trimws(perm[1])))
  }
} else {
  say("  [ABSENT] .github/workflows/ introuvable (pas d'automatisation locale visible)")
}
say("")

# ------------------------------------------------------------
# 4. DONNEES — fichiers data/final
# ------------------------------------------------------------
say("4. DONNEES (data/final/)")
sub()
df_dir <- file.path(BASE_DIR, "data", "final")
if (dir.exists(df_dir)) {
  csvs <- list.files(df_dir, pattern = "\\.csv$", full.names = TRUE)
  for (f in sort(csvs)) {
    d <- tryCatch(suppressWarnings(readr::read_csv(f, show_col_types = FALSE)),
                  error = function(e) NULL)
    nm <- basename(f)
    if (is.null(d)) {
      say(sprintf("  %-40s [illisible]", nm))
    } else {
      say(sprintf("  %-40s %4d lignes x %2d col.", nm, nrow(d), ncol(d)))
    }
  }
} else say("  [ABSENT] data/final/ introuvable")
say("")

# ------------------------------------------------------------
# 5. INDICATEURS DISPONIBLES (le coeur du systeme)
# ------------------------------------------------------------
say("5. INDICATEURS DISPONIBLES (PREIS_indicators_long.csv)")
sub()
ind <- safe_read("data/final/PREIS_indicators_long.csv")
if (is.null(ind) || !"indicator_code" %in% names(ind)) {
  say("  [ABSENT ou illisible] PREIS_indicators_long.csv")
} else {
  codes <- sort(unique(as.character(ind$indicator_code)))
  say(sprintf("  Total : %d indicateurs distincts", length(codes)))
  if ("sitrep_no" %in% names(ind)) {
    say(sprintf("  SitReps couverts : %s -> %s",
                min(ind$sitrep_no, na.rm = TRUE),
                max(ind$sitrep_no, na.rm = TRUE)))
  }
  say("")
  say("  Liste des indicateurs :")
  for (cd in codes) {
    n <- sum(ind$indicator_code == cd, na.rm = TRUE)
    say(sprintf("    - %-38s (%d valeurs)", cd, n))
  }
  say("")
  # Categorisation rapide pour reperer ce qui manque
  has <- function(motif) any(grepl(motif, codes, ignore.case = TRUE))
  say("  COUVERTURE THEMATIQUE :")
  themes <- list(
    "Cas / deces / CFR"        = has("case|death|cfr|fatality|confirmed"),
    "Alertes"                  = has("alert"),
    "Contacts"                 = has("contact"),
    "Laboratoire"             = has("lab|sample|positiv"),
    "Isolement / hospitalisation" = has("isolation|hospital|admit"),
    "Vaccination"              = has("vaccin|ring"),
    "LITS / capacite CTE"      = has("bed|lit|cte|ctc|occup|capacit"),
    "Recuperation"             = has("recover|gueri"),
    "Geographie (zones/prov.)" = has("ituri|kivu|zone|province|hz_")
  )
  for (nm in names(themes)) {
    say(sprintf("    %-30s %s", nm,
                if (themes[[nm]]) "[present]" else "[ABSENT / a ajouter]"))
  }
}
say("")

# ------------------------------------------------------------
# 6. SORTIES D'ANALYSE (outputs/analyse)
# ------------------------------------------------------------
say("6. SORTIES D'ANALYSE (outputs/analyse/)")
sub()
oa_dir <- file.path(BASE_DIR, "outputs", "analyse")
if (dir.exists(oa_dir)) {
  files <- list.files(oa_dir, full.names = TRUE)
  csvs <- files[grepl("\\.csv$", files)]
  pngs <- files[grepl("\\.png$", files)]
  say(sprintf("  Tableaux (CSV) : %d", length(csvs)))
  for (f in sort(csvs)) say(sprintf("    - %s", basename(f)))
  say(sprintf("  Graphiques (PNG) : %d", length(pngs)))
  for (f in sort(pngs)) say(sprintf("    - %s", basename(f)))
} else say("  [ABSENT] outputs/analyse/ introuvable")
say("")

# ------------------------------------------------------------
# 7. DASHBOARD
# ------------------------------------------------------------
say("7. DASHBOARD (dashboard_ebola/)")
sub()
app_fp <- file.path(BASE_DIR, "dashboard_ebola", "app.R")
if (file.exists(app_fp)) {
  app <- readLines(app_fp, warn = FALSE)
  say(sprintf("  app.R : %d lignes", length(app)))
  # Onglets (tabPanel / menuItem)
  tabs <- app[grepl("tabPanel\\(|menuItem\\(", app)]
  titles <- str_match(tabs, '"([^"]+)"')[, 2]
  titles <- titles[!is.na(titles)]
  if (length(titles)) {
    say(sprintf("  Onglets detectes (%d) :", length(titles)))
    for (t in titles) say(sprintf("    - %s", t))
  }
  # URL GitHub utilisee
  gh <- app[grepl("raw.githubusercontent|GH_RAW", app)]
  if (length(gh)) {
    say("  Source de donnees GitHub :")
    say(sprintf("    %s", trimws(gh[grepl("refs/heads|githubusercontent", gh)][1])))
  }
  # Langues i18n
  lang <- app[grepl("selected\\s*=\\s*\"(en|fr)\"|languages|i18n|tr\\(", app)]
  if (length(lang)) say(sprintf("  i18n / multilingue : %d references detectees", length(lang)))
  # Fichiers de donnees lus
  reads <- app[grepl("serie_temporelle|tableau_zones|indicators_long|daily_indicators|geojson", app)]
  say(sprintf("  Fichiers de donnees referencies : %d", length(unique(reads))))
} else {
  say("  [ABSENT] dashboard_ebola/app.R introuvable")
}
say("")

# ------------------------------------------------------------
# 8. GEOGRAPHIE
# ------------------------------------------------------------
say("8. DONNEES GEOGRAPHIQUES")
sub()
geo_candidates <- c("data/curated/rdc_zones_sante_est.geojson",
                    "dashboard_ebola/data/curated/rdc_zones_sante_est.geojson",
                    "data/curated/africa_countries_rcc.geojson")
for (g in geo_candidates) {
  fp <- file.path(BASE_DIR, g)
  if (file.exists(fp)) {
    say(sprintf("  [OK]    %-50s %s", g, human_size(file.info(fp)$size)))
  } else {
    say(sprintf("  [ABSENT] %s", g))
  }
}
say("")

# ------------------------------------------------------------
# 9. ETAT DU MONITEUR (cloud)
# ------------------------------------------------------------
say("9. ETAT DU MONITEUR")
sub()
st <- safe_read("data/monitor_state/preis_sitrep_email_state.csv")
if (!is.null(st)) {
  say(sprintf("  preis_sitrep_email_state.csv : %d SitReps enregistres", nrow(st)))
  if ("sitrep_no" %in% names(st))
    say(sprintf("  Dernier SitRep envoye : N°%s", max(st$sitrep_no, na.rm = TRUE)))
} else say("  [ABSENT] preis_sitrep_email_state.csv")
sl <- safe_read("data/final/sent_log_sitrep.csv")
if (!is.null(sl)) {
  say(sprintf("  sent_log_sitrep.csv : %d lignes (anti-doublon alertes)", nrow(sl)))
} else say("  [ABSENT] sent_log_sitrep.csv")
say("")

# ------------------------------------------------------------
# 10. SYNTHESE & SUGGESTIONS
# ------------------------------------------------------------
say("10. SYNTHESE & PISTES DE REVISION")
sub()
say("  Cette section liste des pistes d'amelioration possibles,")
say("  a valider selon tes priorites :")
say("")
if (!is.null(ind) && "indicator_code" %in% names(ind)) {
  codes <- tolower(unique(ind$indicator_code))
  if (!any(grepl("bed|lit|cte|occup|capacit", codes))) {
    say("  [+] LITS : ajouter l'extraction des lits (CTE) dans 00 -> permet")
    say("            le taux d'occupation (script 15 deja pret).")
  }
  if (!any(grepl("vaccin|ring", codes))) {
    say("  [+] VACCINATION : aucun indicateur de vaccination detecte.")
  }
  if (!any(grepl("hospital|admit", codes))) {
    say("  [+] HOSPITALISATION : pas d'indicateur d'admission detecte.")
  }
}
say("  [+] DASHBOARD : envisager onglet 'fraicheur des donnees' + feedback.")
say("  [+] DASHBOARD : page 'A propos' avec lien preprint (visibilite).")
say("  [+] EXPORT : permettre le telechargement des graphiques/tableaux.")
say("")
hr()
say("FIN DE L'AUDIT — rapport complet : outputs/audit/AUDIT_SYSTEME_PREIS.txt")
hr()

# Ecriture du rapport
con <- file(REPORT, open = "w", encoding = "UTF-8")
writeLines(.buf, con)
close(con)
cat("\n>> Rapport ecrit : ", REPORT, "\n", sep = "")
