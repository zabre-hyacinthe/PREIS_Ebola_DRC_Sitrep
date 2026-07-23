## =====================================================================
## PREIS Ebola RDC — Resolveur de chemins partage (preis_paths.R)
## ---------------------------------------------------------------------
## Emplacement : dashboard_ebola/preis_paths.R
## Role : dire A CHAQUE module OU lire un fichier, selon qu'on tourne
##        en LOCAL (dev) ou en LIGNE (shinyapps.io), et selon que la
##        donnee est AGREGEE (publiable) ou INDIVIDUELLE (jamais en ligne).
##
## Aucun effet de bord : ce fichier ne fait que DEFINIR des fonctions.
## Il ne lit, n'ecrit et ne telecharge rien au chargement.
##
## GARANTIE DE CONFIDENTIALITE (regle de gouvernance PREIS) :
##   - tier = "individual"  -> LOCAL uniquement, JAMAIS de source en ligne.
##   - tout nom de fichier ressemblant a de la donnee patient (line list,
##     gap, lifeline, trace, nominatif...) est FORCE en local, meme s'il
##     est appele avec tier = "aggregate" par erreur.
##
## NOTE : version canonique conforme au design (voir doc projet
##   "PREIS_chantier_dhis2_zones.md"). A n'installer QUE si l'auto-controle
##   indique que preis_paths.R est absent chez toi. Si ta version livree
##   existe deja, garde-la.
## =====================================================================

# ---- 1. Racine du dashboard (dossier de app.R) ----------------------
preis_dash_dir <- function() {
  d <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                error = function(e) getwd())
  # si on est lance depuis la racine du depot, descendre dans dashboard_ebola
  if (!file.exists(file.path(d, "app.R")) &&
      file.exists(file.path(d, "dashboard_ebola", "app.R"))) {
    d <- normalizePath(file.path(d, "dashboard_ebola"), winslash = "/", mustWork = FALSE)
  }
  d
}

# Base GitHub raw (depot public) : permet a l'app deployee de lire les
# dernieres donnees agregees poussees par le pipeline, sans redeploiement.
preis_gh_raw_base <- function() {
  Sys.getenv(
    "PREIS_GH_RAW_BASE",
    "https://raw.githubusercontent.com/zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep/refs/heads/main"
  )
}

# ---- 2. Politique de confidentialite --------------------------------
# Motifs indiquant une donnee INDIVIDUELLE (ne DOIT jamais partir en ligne)
PREIS_INDIVIDUAL_PATTERNS <- c(
  "line.?list", "linelist", "patient", "nominatif", "individual",
  "gap_tracker", "gap_", "lifeline", "daily_trace", "_trace"
)

# Liste blanche EXPLICITE des CSV agreges publiables (Situation Room).
# Verrouillage strict (default-deny) : SEULS ces 4 fichiers agreges peuvent
# etre servis en ligne. Noms confirmes par l'auto-controle (section 3).
# Laisser character(0) = mode auto (tout non-individuel autorise).
PREIS_PUBLIC_AGG_FILES <- c(
  "dhis2_mve_situation_room.csv",
  "dhis2_mve_situation_room_zones.csv",
  "dhis2_mve_situation_room_priority_zones.csv",
  "dhis2_mve_situation_room_epi_curve.csv"
)

preis_is_individual <- function(fn) {
  base <- basename(fn)
  any(vapply(PREIS_INDIVIDUAL_PATTERNS,
             function(p) grepl(p, base, ignore.case = TRUE, perl = TRUE),
             logical(1)))
}

preis_is_public_allowed <- function(fn) {
  base <- basename(fn)
  if (preis_is_individual(base)) return(FALSE)
  if (length(PREIS_PUBLIC_AGG_FILES)) return(base %in% PREIS_PUBLIC_AGG_FILES)
  TRUE  # mode auto : tout non-individuel est autorise en agrege
}

# ---- 3. Resolution d'un fichier de donnees --------------------------
# fn   : nom (ou chemin relatif) du fichier de donnees
# tier : "aggregate" (publiable) ou "individual" (local only)
# Renvoie un chemin local OU une URL utilisable par read.csv/read_csv,
# ou NA_character_ si rien de disponible.
preis_resolve_data <- function(fn, tier = c("aggregate", "individual")) {
  tier <- match.arg(tier)
  base <- basename(fn)
  dash <- preis_dash_dir()

  # Candidats LOCAUX (dev) — on essaie plusieurs emplacements usuels
  local_candidates <- c(
    file.path(dash, "data", "dhis2_public", base),
    file.path(dash, "data", base),
    file.path(dash, "..", "source_line_list", "data", base),
    file.path(dash, "..", "source_line_list", base),
    file.path(dash, fn)
  )
  local_hit <- local_candidates[file.exists(local_candidates)]
  if (length(local_hit)) {
    return(normalizePath(local_hit[1], winslash = "/", mustWork = FALSE))
  }

  # Pas de fichier local -> on est probablement DEPLOYE en ligne.
  # Donnee individuelle : on s'arrete ici (jamais de source en ligne).
  if (tier == "individual" || !preis_is_public_allowed(base)) {
    return(NA_character_)
  }

  # Donnee agregee autorisee : GitHub raw dossier dhis2_public
  url <- paste0(preis_gh_raw_base(), "/dashboard_ebola/data/dhis2_public/", base)
  # (le repli "bundle" est deja couvert par local_candidates ci-dessus)
  url
}

# ---- 4. Resolution d'un MODULE (.R a sourcer) -----------------------
# Cherche d'abord le module embarque dans dashboard_ebola/, puis le module
# local historique dans source_line_list/scripts/. Renvoie NA si aucun.
preis_resolve_module <- function(fn) {
  base <- basename(fn)
  dash <- preis_dash_dir()
  candidates <- c(
    file.path(dash, base),
    file.path(dash, "..", "source_line_list", "scripts", base),
    file.path(dash, "..", "..", "source_line_list", "scripts", base),
    fn
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) return(normalizePath(hit[1], winslash = "/", mustWork = FALSE))
  NA_character_
}

# ---- 5. Presence d'une line list locale -----------------------------
preis_has_local_linelist <- function() {
  dash <- preis_dash_dir()
  dirs <- c(
    file.path(dash, "..", "source_line_list", "data"),
    file.path(dash, "..", "source_line_list"),
    file.path(dash, "data")
  )
  for (d in dirs) {
    if (dir.exists(d)) {
      f <- list.files(d, pattern = "line.?list", full.names = TRUE, ignore.case = TRUE)
      if (length(f)) return(TRUE)
    }
  }
  FALSE
}

# ---- 6. Panneau UI "disponible en local" ----------------------------
# A afficher dans les onglets INDIVIDUELS quand on tourne en ligne :
# message propre de protection des donnees, au lieu de "donnees absentes".
preis_local_only_panel <- function(titre = "Donnees individuelles") {
  msg_local <- preis_has_local_linelist()
  txt <- if (msg_local) {
    "Cet onglet s'appuie sur des donnees individuelles (line list) qui restent en LOCAL et ne sont jamais publiees en ligne, par protection des donnees. Lance le dashboard en local pour le contenu complet."
  } else {
    "Onglet base sur des donnees individuelles (line list). Par protection des donnees, ce contenu n'est disponible qu'en execution LOCALE."
  }
  if (requireNamespace("shinydashboard", quietly = TRUE)) {
    shinydashboard::box(
      width = 12, status = "info", solidHeader = TRUE, title = titre,
      shiny::tags$p(txt),
      shiny::tags$p(shiny::tags$em(
        "Regle PREIS : agrege en ligne, individuel en local."))
    )
  } else {
    shiny::wellPanel(shiny::tags$h4(titre), shiny::tags$p(txt))
  }
}

## --- Marqueur de version (utile pour l'auto-controle) ---------------
PREIS_PATHS_VERSION <- "canonique-2026-07-23"
invisible(TRUE)
