#!/usr/bin/env Rscript
# =====================================================================
# PREIS Ebola RDC — INSTALLEUR : DHIS2 Situation Room agrege en ligne
# ---------------------------------------------------------------------
# Applique les 6 modifications, en une passe, SANS RIEN CASSER :
#   1. depose  dashboard_ebola/preis_paths.R (resolveur)
#   2. patch   dashboard_ebola/app.R          (source resolveur + pre-source module)
#   3. embarque+patch app_dhis2_module.R -> dashboard_ebola/ (.load via resolveur)
#   4. patch   prepare_dashboard_data.R       (copie des 4 CSV vers dhis2_public/)
#   5. ajoute  l'exception .gitignore
#
# Sauvegardes horodatees de chaque fichier modifie. Idempotent : relançable
# sans effet secondaire (detecte les patches deja poses). Repli = ancien
# comportement local -> zero regression.
#
# PRE-REQUIS : mettre 'preis_paths.R' (fourni) dans le MEME dossier que cet
# installeur (ou a la racine du depot). LANCEMENT depuis la racine du depot :
#   Rscript INSTALL_DHIS2_situation_room.R
# =====================================================================

STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")
norm  <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
ok    <- function(x) cat("  [ OK ]  ", x, "\n", sep = "")
ko    <- function(x) cat("  [FAIL]  ", x, "\n", sep = "")
skip  <- function(x) cat("  [skip]  ", x, "\n", sep = "")
inf   <- function(x) cat("  [info]  ", x, "\n", sep = "")
hdr   <- function(x) cat("\n===== ", x, " =====\n", sep = "")

# --- dossier de cet installeur (pour trouver preis_paths.R a cote) ----
this_file <- function() {
  a <- commandArgs(FALSE)
  m <- grep("^--file=", a, value = TRUE)
  if (length(m)) return(norm(sub("^--file=", "", m[1])))
  NA_character_
}
INSTALLER_DIR <- tryCatch({ tf <- this_file(); if (is.na(tf)) getwd() else dirname(tf) },
                          error = function(e) getwd())

# --- racine du depot + dashboard ------------------------------------
find_root <- function() {
  cands <- unique(c(getwd(), file.path(getwd(), ".."), INSTALLER_DIR,
                    file.path(INSTALLER_DIR, ".."),
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
cat("#  PREIS - INSTALLEUR DHIS2 Situation Room agrege                   #\n")
cat("#####################################################################\n")
if (is.na(ROOT)) { ko("Racine du depot introuvable (dashboard_ebola/app.R)."); quit(save="no", status=1) }
DASH <- file.path(ROOT, "dashboard_ebola")
inf(paste0("Depot     : ", ROOT))
inf(paste0("Dashboard : ", DASH))
inf(paste0("Horodatage: ", STAMP))

backup <- function(path) {
  if (file.exists(path)) {
    bp <- paste0(path, ".bak_", STAMP)
    file.copy(path, bp, overwrite = FALSE)
    inf(paste0("sauvegarde : ", basename(bp)))
  }
}
read_lines <- function(p) tryCatch(readLines(p, warn = FALSE, encoding = "UTF-8"),
                                   error = function(e) readLines(p, warn = FALSE))
write_lines <- function(x, p) writeLines(x, p, useBytes = TRUE)
has <- function(lines, pat) any(grepl(pat, lines, perl = TRUE))
first_idx <- function(lines, pat) { w <- which(grepl(pat, lines, perl = TRUE)); if (length(w)) w[1] else NA_integer_ }

# =====================================================================
# 1. preis_paths.R
# =====================================================================
hdr("1. dashboard_ebola/preis_paths.R")
dst_pp <- file.path(DASH, "preis_paths.R")
if (file.exists(dst_pp)) {
  skip("preis_paths.R deja present -> laisse tel quel")
} else {
  src_pp <- c(file.path(INSTALLER_DIR, "preis_paths.R"),
              file.path(getwd(), "preis_paths.R"),
              file.path(ROOT, "preis_paths.R"))
  src_pp <- src_pp[file.exists(src_pp)][1]
  if (is.na(src_pp)) {
    ko("preis_paths.R introuvable ! Place-le a cote de cet installeur, puis relance.")
    quit(save = "no", status = 1)
  }
  file.copy(src_pp, dst_pp, overwrite = FALSE)
  if (file.exists(dst_pp)) ok("preis_paths.R depose dans dashboard_ebola/") else ko("copie echouee")
}

# =====================================================================
# 2. app.R : source du resolveur + pre-source du module via resolveur
# =====================================================================
hdr("2. dashboard_ebola/app.R")
appR <- file.path(DASH, "app.R")
app <- read_lines(appR)
if (has(app, "preis_paths\\.R")) {
  skip("app.R deja patche (source preis_paths.R present)")
} else {
  idx <- first_idx(app, "\\.dhis2_mod\\s*<-\\s*normalizePath")
  if (is.na(idx)) {
    ko("Ancre introuvable dans app.R ('.dhis2_mod <- normalizePath'). Patch NON applique.")
  } else {
    block <- c(
      "# ===== PREIS DHIS2 online patch (installe automatiquement) =====",
      ".preis_paths_fp <- file.path(normalizePath(getwd(), winslash='/', mustWork=FALSE), 'preis_paths.R')",
      "if (file.exists(.preis_paths_fp)) source(.preis_paths_fp)",
      "if (exists('preis_resolve_module')) {",
      "  .m_dhis2 <- preis_resolve_module('app_dhis2_module.R')",
      "  if (!is.na(.m_dhis2) && file.exists(.m_dhis2)) try(source(.m_dhis2, encoding='UTF-8'), silent=TRUE)",
      "}",
      "# ================================================================",
      "")
    new_app <- append(app, block, after = idx - 1)
    backup(appR)
    write_lines(new_app, appR)
    if (has(read_lines(appR), "preis_paths\\.R")) ok(paste0("app.R patche (bloc insere avant la ligne ", idx, ")"))
    else ko("verification post-ecriture : marqueur absent")
  }
}

# =====================================================================
# 3. app_dhis2_module.R : embarque dans dashboard_ebola/ + .load via resolveur
# =====================================================================
hdr("3. app_dhis2_module.R (embarque + patch .load)")
src_mod <- c(file.path(ROOT, "source_line_list", "scripts", "app_dhis2_module.R"),
             file.path(ROOT, "..", "source_line_list", "scripts", "app_dhis2_module.R"),
             file.path(DASH, "app_dhis2_module.R"))
src_mod <- src_mod[file.exists(src_mod)][1]
dst_mod <- file.path(DASH, "app_dhis2_module.R")
if (is.na(src_mod)) {
  ko("app_dhis2_module.R source introuvable.")
} else {
  inf(paste0("source : ", norm(src_mod)))
  mod <- read_lines(src_mod)
  if (has(mod, "preis_resolve_data")) {
    # deja patche : s'assurer qu'il est embarque
    if (!file.exists(dst_mod)) { file.copy(src_mod, dst_mod); ok("copie embarquee (deja patche)") }
    else skip("module deja patche et embarque")
  } else {
    idx <- first_idx(mod, "sr_kpi\\s*<-\\s*\\.load\\(")
    if (is.na(idx)) {
      ko("Ancre 'sr_kpi <- .load(' introuvable dans le module. Patch NON applique.")
    } else {
      wrap <- c(
        "# ===== PREIS DHIS2 online patch: router .load via le resolveur =====",
        "if (exists('preis_resolve_data') && !isTRUE(.PREIS_LOAD_WRAPPED)) {",
        "  .load_orig <- .load",
        "  .load <- function(fn) {",
        "    p <- tryCatch(preis_resolve_data(fn, 'aggregate'), error=function(e) NA_character_)",
        "    if (!is.na(p) && file.exists(p)) return(.load_orig(fn))",
        "    if (!is.na(p) && grepl('^https?://', p)) {",
        "      return(tryCatch(readr::read_csv(p, show_col_types = FALSE),",
        "                      error = function(e) .load_orig(fn)))",
        "    }",
        "    .load_orig(fn)",
        "  }",
        "  .PREIS_LOAD_WRAPPED <- TRUE",
        "}",
        "# ==================================================================",
        "")
      new_mod <- append(mod, wrap, after = idx - 1)
      if (file.exists(dst_mod)) backup(dst_mod)
      write_lines(new_mod, dst_mod)
      if (has(read_lines(dst_mod), "preis_resolve_data"))
        ok(paste0("module patche + embarque dans dashboard_ebola/ (avant ligne ", idx, ")"))
      else ko("verification post-ecriture : marqueur absent")
      inf("(l'original dans source_line_list/ reste intact = repli local)")
    }
  }
}

# =====================================================================
# 4. prepare_dashboard_data.R : copie des 4 CSV vers dhis2_public/
# =====================================================================
hdr("4. prepare_dashboard_data.R")
prep <- c(file.path(ROOT, "prepare_dashboard_data.R"),
          file.path(DASH, "prepare_dashboard_data.R"))
prep <- prep[file.exists(prep)][1]
if (is.na(prep)) {
  ko("prepare_dashboard_data.R introuvable.")
} else {
  inf(paste0("fichier : ", norm(prep)))
  pl <- read_lines(prep)
  if (has(pl, "dhis2_public")) {
    skip("prepare_dashboard_data.R deja patche")
  } else {
    block <- c(
      "",
      "# --- DHIS2 Situation Room : copie des 4 CSV AGREGES (0 donnee individuelle) ---",
      "DHIS2_PUB <- file.path(DASH_DATA, 'dhis2_public')",
      "dir.create(DHIS2_PUB, recursive = TRUE, showWarnings = FALSE)",
      ".agg_names <- c('dhis2_mve_situation_room.csv','dhis2_mve_situation_room_zones.csv',",
      "                'dhis2_mve_situation_room_priority_zones.csv','dhis2_mve_situation_room_epi_curve.csv')",
      ".agg_src_dirs <- c(file.path(BASE_DIR,'source_line_list','data'),",
      "                   file.path(BASE_DIR,'source_line_list'),",
      "                   file.path(BASE_DIR,'outputs','dhis2'),",
      "                   file.path(BASE_DIR,'data','dhis2_public'))",
      "for (.nm in .agg_names) {",
      "  .hit <- NA_character_",
      "  for (.d in .agg_src_dirs) { .p <- file.path(.d, .nm); if (file.exists(.p)) { .hit <- .p; break } }",
      "  if (!is.na(.hit)) copy_if(.hit, file.path(DHIS2_PUB, .nm)) else cat('  MANQUANT (agrege):', .nm, '\\n')",
      "}")
    ins <- first_idx(pl, "Dashboard pr")     # avant le message final si present
    if (is.na(ins)) new_pl <- c(pl, block) else new_pl <- append(pl, block, after = ins - 1)
    backup(prep)
    write_lines(new_pl, prep)
    if (has(read_lines(prep), "dhis2_public")) ok("prepare_dashboard_data.R patche") else ko("marqueur absent")
  }
}

# =====================================================================
# 5. .gitignore : exception dhis2_public/
# =====================================================================
hdr("5. .gitignore (exception dhis2_public/)")
gi_path <- file.path(ROOT, ".gitignore")
gi <- if (file.exists(gi_path)) read_lines(gi_path) else character(0)
if (has(gi, "!dashboard_ebola/data/dhis2_public")) {
  skip("exception deja presente")
} else {
  add <- c("",
           "# Exception : donnees AGREGEES DHIS2 publiables (aucune ligne individuelle)",
           "!dashboard_ebola/data/dhis2_public/",
           "!dashboard_ebola/data/dhis2_public/**")
  if (file.exists(gi_path)) backup(gi_path)
  write_lines(c(gi, add), gi_path)
  ok("exception ajoutee au .gitignore")
}

# =====================================================================
# FIN + prochaines commandes
# =====================================================================
cat("\n#####################################################################\n")
cat("#  INSTALLATION TERMINEE. Etapes suivantes (a lancer manuellement)  #\n")
cat("#####################################################################\n")
cat("
  1) Generer + publier les 4 CSV agreges :
       Rscript prepare_dashboard_data.R
     (verifie : dashboard_ebola/data/dhis2_public/ contient 4 CSV)

  2) Versionner et pousser :
       git add dashboard_ebola/preis_paths.R dashboard_ebola/app.R
       git add dashboard_ebola/app_dhis2_module.R prepare_dashboard_data.R .gitignore
       git add -f dashboard_ebola/data/dhis2_public/
       git status
       git commit -m \"DHIS2 Situation Room agrege en ligne (resolveur + 4 CSV)\"
       git push

  3) Test LOCAL :
       R -e 'shiny::runApp(\"dashboard_ebola\")'
     -> onglet DHIS2 : donnees agregees ; onglets individuels : pleins.

  4) Deploiement + test EN LIGNE :
       R -e 'source(\"deploy_dashboard_shinyapps.R\")'
     -> DHIS2 s'affiche en ligne ; onglets individuels : \"disponible en local\".

  Relance verifier_patches_DHIS2.R a tout moment pour re-controler l'etat.
")
