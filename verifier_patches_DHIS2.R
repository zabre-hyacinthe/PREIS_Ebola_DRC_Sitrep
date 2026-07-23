#!/usr/bin/env Rscript
# =====================================================================
# PREIS Ebola RDC — AUTO-CONTRoLE DHIS2 + FRAiCHEUR  (LECTURE SEULE)
# ---------------------------------------------------------------------
# Ce script NE MODIFIE RIEN. Il repond a l'etape 1 de la checklist :
# "les patches sont-ils vraiment en place ?" et il imprime le DERNIER
# SitRep reellement present dans chaque base (pour trancher le point P0).
#
# LANCEMENT (sur ta machine Windows) :
#   Rscript verifier_patches_DHIS2.R
# depuis la racine du depot (ou depuis dashboard_ebola/).
# Puis copie-colle toute la sortie : elle me donne la verite terrain.
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

ok   <- function(x) cat("  [ OK ]  ", x, "\n", sep = "")
ko   <- function(x) cat("  [FAIL]  ", x, "\n", sep = "")
warn <- function(x) cat("  [WARN]  ", x, "\n", sep = "")
inf  <- function(x) cat("  [info]  ", x, "\n", sep = "")
hdr  <- function(x) cat("\n===== ", x, " =====\n", sep = "")

# ---------------------------------------------------------------------
# 0. Localiser la racine du depot et dashboard_ebola/
# ---------------------------------------------------------------------
norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

find_root <- function() {
  cands <- unique(c(getwd(),
                    file.path(getwd(), ".."),
                    "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"))
  for (c in cands) {
    if (file.exists(file.path(c, "dashboard_ebola", "app.R"))) return(norm(c))
    if (basename(norm(c)) == "dashboard_ebola" && file.exists(file.path(c, "app.R")))
      return(norm(file.path(c, "..")))
  }
  NA_character_
}

ROOT <- find_root()
cat("\n#####################################################################\n")
cat("#  PREIS - AUTO-CONTROLE DHIS2 + FRAICHEUR (lecture seule)          #\n")
cat("#####################################################################\n")

if (is.na(ROOT)) {
  ko("Racine du depot introuvable. Lance ce script depuis le dossier du projet")
  inf("  (celui qui contient 'dashboard_ebola/app.R').")
  quit(save = "no", status = 0)
}
DASH <- file.path(ROOT, "dashboard_ebola")
inf(paste0("Racine du depot : ", ROOT))
inf(paste0("Dashboard       : ", DASH))
inf(paste0("Date/heure      : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ---------------------------------------------------------------------
# Utilitaires
# ---------------------------------------------------------------------
read_txt <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
           error = function(e) tryCatch(readLines(path, warn = FALSE),
                                        error = function(e2) NULL))
}
has_pat <- function(lines, pattern) {
  if (is.null(lines)) return(FALSE)
  any(grepl(pattern, lines, perl = TRUE))
}
grep_lines <- function(lines, pattern, n = 6) {
  if (is.null(lines)) return(character(0))
  hit <- lines[grepl(pattern, lines, perl = TRUE)]
  hit <- trimws(hit)
  utils::head(hit, n)
}

# Dernier SitRep + date max presents dans un CSV
last_sitrep <- function(path) {
  if (!file.exists(path)) return("fichier ABSENT")
  tryCatch({
    d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8")
    if (nrow(d) == 0) return("0 ligne")
    nm <- names(d)
    sc <- nm[grepl("sit.?rep|^sno$|sitrep_no|n_?sitrep|^num(ero)?$", nm, ignore.case = TRUE)]
    srtxt <- "pas de colonne SitRep"
    if (length(sc)) {
      v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(d[[sc[1]]]))))
      v <- v[is.finite(v)]
      if (length(v)) srtxt <- paste0("SitRep max = ", max(v), " (col '", sc[1], "')")
    }
    dc <- nm[grepl("date", nm, ignore.case = TRUE)]
    dttxt <- ""
    if (length(dc)) {
      dd <- suppressWarnings(as.Date(as.character(d[[dc[1]]])))
      dd <- dd[!is.na(dd)]
      if (length(dd)) dttxt <- paste0(" | date max = ", format(max(dd)))
    }
    paste0(nrow(d), " lignes | ", srtxt, dttxt)
  }, error = function(e) paste("illisible :", conditionMessage(e)))
}

git <- function(args) {
  if (Sys.which("git") == "") return(NULL)
  tryCatch(system2("git", c("-C", shQuote(ROOT), args), stdout = TRUE, stderr = TRUE),
           error = function(e) NULL)
}

# ---------------------------------------------------------------------
# 1. preis_paths.R present ?
# ---------------------------------------------------------------------
hdr("1. Resolveur partage : dashboard_ebola/preis_paths.R")
pp <- file.path(DASH, "preis_paths.R")
pp_lines <- read_txt(pp)
if (!is.null(pp_lines)) {
  ok("preis_paths.R present")
  for (fn in c("preis_resolve_data", "preis_resolve_module",
               "preis_has_local_linelist", "preis_local_only_panel")) {
    if (has_pat(pp_lines, fn)) ok(paste0("  fonction definie : ", fn))
    else warn(paste0("  fonction MANQUANTE : ", fn))
  }
} else {
  ko("preis_paths.R ABSENT  ->  installe la version canonique fournie (preis_paths.R)")
}

# ---------------------------------------------------------------------
# 2. Patches app.R
# ---------------------------------------------------------------------
hdr("2. Patches dans dashboard_ebola/app.R")
appR <- file.path(DASH, "app.R")
app_lines <- read_txt(appR)
if (is.null(app_lines)) {
  ko("app.R introuvable")
} else {
  if (has_pat(app_lines, "preis_paths\\.R|source\\(.*preis_paths"))
    ok("app.R source bien preis_paths.R") else
    ko("app.R ne source PAS preis_paths.R (patch bloc 1 absent)")

  if (has_pat(app_lines, "preis_resolve_module"))
    ok("app.R resout les modules via preis_resolve_module()") else
    ko("app.R n'appelle PAS preis_resolve_module() (patch bloc 2 absent)")

  # ancien comportement encore actif ?
  old <- grep_lines(app_lines, "\\.\\./source_line_list/scripts/app_dhis2_module\\.R", 3)
  if (length(old)) {
    warn("app.R contient encore l'ancien chemin direct vers source_line_list :")
    for (l in old) cat("        ", l, "\n", sep = "")
    inf("  (OK si c'est le repli DANS le nouveau bloc ; FAIL si c'est l'unique source)")
  }
}

# ---------------------------------------------------------------------
# 3. Patch app_dhis2_module.R
# ---------------------------------------------------------------------
hdr("3. Patch app_dhis2_module.R (.load via preis_resolve_data)")
mod_candidates <- c(
  file.path(DASH, "app_dhis2_module.R"),
  file.path(ROOT, "source_line_list", "scripts", "app_dhis2_module.R"),
  file.path(ROOT, "..", "source_line_list", "scripts", "app_dhis2_module.R")
)
mod_path <- mod_candidates[file.exists(mod_candidates)][1]
if (is.na(mod_path) || is.null(mod_path)) {
  ko("app_dhis2_module.R introuvable dans les emplacements connus :")
  for (m in mod_candidates) inf(paste0("   - ", norm(m)))
} else {
  inf(paste0("Trouve : ", norm(mod_path)))
  mod_lines <- read_txt(mod_path)
  if (has_pat(mod_lines, "preis_resolve_data"))
    ok("app_dhis2_module.R resout via preis_resolve_data(fn, 'aggregate')") else
    ko("app_dhis2_module.R n'utilise PAS preis_resolve_data() (patch absent)")
  cat("  --> fichiers CSV reellement lus par ce module (verite terrain) :\n")
  csvl <- grep_lines(mod_lines, "\\.csv", 12)
  if (length(csvl)) for (l in csvl) cat("        ", l, "\n", sep = "") else
    inf("   (aucune ligne .csv detectee automatiquement)")
}

# ---------------------------------------------------------------------
# 4. Patch prepare_dashboard_data.R
# ---------------------------------------------------------------------
hdr("4. Patch prepare_dashboard_data.R (copie vers dhis2_public/)")
prep_candidates <- c(
  file.path(ROOT, "prepare_dashboard_data.R"),
  file.path(DASH, "prepare_dashboard_data.R"),
  file.path(ROOT, "scripts", "prepare_dashboard_data.R")
)
prep_path <- prep_candidates[file.exists(prep_candidates)][1]
if (is.na(prep_path) || is.null(prep_path)) {
  ko("prepare_dashboard_data.R introuvable")
} else {
  inf(paste0("Trouve : ", norm(prep_path)))
  prep_lines <- read_txt(prep_path)
  if (has_pat(prep_lines, "dhis2_public"))
    ok("prepare_dashboard_data.R copie bien vers dhis2_public/") else
    ko("prepare_dashboard_data.R ne remplit PAS dhis2_public/ (patch absent)")
}

# ---------------------------------------------------------------------
# 5. Exception .gitignore
# ---------------------------------------------------------------------
hdr("5. Exception .gitignore pour dhis2_public/")
gi <- read_txt(file.path(ROOT, ".gitignore"))
if (is.null(gi)) {
  warn(".gitignore introuvable a la racine")
} else if (has_pat(gi, "!dashboard_ebola/data/dhis2_public")) {
  ok("Exception presente : !dashboard_ebola/data/dhis2_public/")
} else {
  ko("Exception ABSENTE -> les 4 CSV agreges ne seront pas versionnes/pousses")
}

# ---------------------------------------------------------------------
# 6. Contenu reel de dhis2_public/
# ---------------------------------------------------------------------
hdr("6. Contenu de dashboard_ebola/data/dhis2_public/")
pub <- file.path(DASH, "data", "dhis2_public")
if (dir.exists(pub)) {
  files <- list.files(pub, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    ko("Dossier present mais VIDE -> lance prepare_dashboard_data.R (etape 2)")
  } else {
    ok(paste0(length(files), " CSV present(s) :"))
    for (f in files) cat("        ", basename(f), "  ->  ", last_sitrep(f), "\n", sep = "")
  }
} else {
  ko("Dossier dhis2_public/ ABSENT -> sera cree par prepare_dashboard_data.R")
}

# ---------------------------------------------------------------------
# 7. Fraicheur des bases (point P0 : bloque a N28 ?)
# ---------------------------------------------------------------------
hdr("7. Dernier SitRep present dans chaque base (point P0)")
targets <- c(
  "outputs/analyse/serie_temporelle_nationale.csv",
  "data/final/PREIS_indicators_long.csv",
  "data/final/PREIS_daily_indicators.csv",
  "dashboard_ebola/data/serie_temporelle_nationale.csv",
  "dashboard_ebola/data/PREIS_indicators_long.csv",
  "dashboard_ebola/data/PREIS_daily_indicators.csv"
)
for (rel in targets) {
  p <- file.path(ROOT, rel)
  cat("  ", sprintf("%-58s", rel), " : ", last_sitrep(p), "\n", sep = "")
}
inf("Regle P0 : si la ligne 'source' (outputs/analyse) est a N67 mais la")
inf("copie 'dashboard_ebola/data/...' est figee (N28 / mi-juin) -> P0 non regle.")

# ---------------------------------------------------------------------
# 8. Etat git
# ---------------------------------------------------------------------
hdr("8. Etat Git")
if (Sys.which("git") == "") {
  warn("git introuvable dans le PATH (ignore cette section)")
} else {
  last <- git(c("log", "-1", "--pretty=format:%h  %ci  %s"))
  if (!is.null(last)) { inf("Dernier commit :"); cat("        ", paste(last, collapse = "\n         "), "\n", sep = "") }
  br <- git(c("rev-parse", "--abbrev-ref", "HEAD"))
  if (!is.null(br)) inf(paste0("Branche : ", paste(br, collapse = " ")))
  tracked <- git(c("ls-files", "dashboard_ebola/data/dhis2_public"))
  if (is.null(tracked) || length(tracked) == 0 || all(!nzchar(tracked)))
    ko("Aucun fichier dhis2_public/ suivi par git (rien a servir en ligne)")
  else { ok(paste0(length(tracked), " fichier(s) dhis2_public/ suivi(s) par git :"))
         for (t in tracked) cat("        ", t, "\n", sep = "") }
  st <- git(c("status", "--porcelain"))
  if (!is.null(st) && length(st) && any(nzchar(st))) {
    warn("Modifications non committees :")
    for (s in utils::head(st, 20)) cat("        ", s, "\n", sep = "")
  } else inf("Arbre de travail propre (rien a committer)")
}

cat("\n#####################################################################\n")
cat("#  FIN. Copie-colle toute cette sortie pour que je finalise l'etat. #\n")
cat("#####################################################################\n")
