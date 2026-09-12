## ============================================================
## PREIS EBOLA RDC — MODULE D'IDENTITÉ DU SITREP
## 10_sitrep_identity.R
##
## RÔLE : la SEULE source de vérité pour identifier un SitRep.
## Toute reconnaissance de SitRep (web ou fichier) passe par ici.
## Objectif : ne JAMAIS se tromper (pas de visa, facture, etc.).
##
## RÈGLES STRICTES :
##   - Un SitRep valide a un numéro entre 1 et 60 (borne réaliste).
##   - L'identification depuis un nom de fichier EXIGE le mot "sitrep".
##   - L'identification depuis un titre web EXIGE "sitrep" + "mvb"/"mve"/"ebola".
##   - Le nommage canonique d'un PDF est : SitRep_NN_2026.pdf
##
## Ce module est sourcé par 08 (moniteur), 00 (pipeline), etc.
## Il est PUR (aucun effet de bord) et TESTABLE.
## ============================================================

# Bornes d'un numéro de SitRep réaliste
# CORRECTIF 2026-09-06 : la borne haute était fixee a 60 tot dans
# l'epidemie ("borne realiste" a l'epoque). L'epidemie a depasse ce
# seuil depuis des semaines (SitRep 113 confirme le 04/09/2026) --
# chaque numero au-dela de 60 etait donc rejete par cette validation
# et traite comme "non identifiable". C'est la cause racine confirmee
# du blocage : le pipeline ne reconnaissait plus aucun SitRep recent.
# 400 plutot qu'une valeur illimitee : garde la protection contre le
# faux positif historique documente dans l'auto-test ci-dessous (752,
# vraisemblablement un numero de visa mal interprete comme un SitRep),
# tout en laissant une marge tres large (plus d'un an de rapports
# quotidiens) au-dela du 113 actuel.
SITREP_MIN <- 1L
SITREP_MAX <- 400L

# Année de l'épidémie en cours (pour le nommage canonique)
SITREP_YEAR <- 2026L

# ------------------------------------------------------------
# 1) Numéro depuis un NOM DE FICHIER
#    Exige "sitrep" dans le nom. Rejette tout le reste.
# ------------------------------------------------------------
sitrep_no_from_filename <- function(path) {
  if (is.null(path) || length(path) != 1 || is.na(path)) return(NA_integer_)
  b <- tolower(basename(as.character(path)))

  # GARDE-FOU n°1 : sans "sitrep", ce n'est pas un SitRep.
  if (!grepl("sitrep", b, fixed = TRUE)) return(NA_integer_)

  # Patterns du plus strict au plus souple
  patterns <- c(
    "sitrep[_ -]*0*([0-9]{1,3})[_ -]*2026",        # SitRep_28_2026.pdf
    "sitrep[_ -]*n?[\u00b0\u00ba o]*0*([0-9]{1,3})", # sitrep n28 / sitrep-28
    "sitrep[^0-9]{0,12}0*([0-9]{1,3})"              # 'sitrep' puis nombre proche (ex: SitRep_MVEBDB_109_...)
  )
  for (p in patterns) {
    m <- regmatches(b, regexec(p, b, perl = TRUE))[[1]]
    if (length(m) >= 2) {
      n <- suppressWarnings(as.integer(m[2]))
      if (!is.na(n) && n >= SITREP_MIN && n <= SITREP_MAX) return(n)
    }
  }
  NA_integer_
}

# ------------------------------------------------------------
# 2) Numéro depuis un TITRE / URL WEB (page INSP)
#    Exige "sitrep" ET un marqueur épidémie (mvb/mve/ebola).
# ------------------------------------------------------------
sitrep_no_from_web <- function(text) {
  if (is.null(text) || length(text) != 1 || is.na(text)) return(NA_integer_)
  t <- tolower(as.character(text))

  # GARDE-FOU n°2 : doit ressembler à un SitRep Ebola/MVB.
  if (!grepl("sitrep", t, fixed = TRUE)) return(NA_integer_)
  if (!grepl("mvb|mve|ebola|bundibugyo", t)) return(NA_integer_)

  patterns <- c(
    "sitrep[ _-]*n[\u00b0\u00ba o]*0*([0-9]{1,3})",  # SitRep N°28
    "sitrep[ _-]*0*([0-9]{1,3})",                     # SitRep 28
    "n[\u00b0\u00ba]\\s*0*([0-9]{1,3})"               # N°28 (avec sitrep déjà confirmé)
  )
  for (p in patterns) {
    m <- regmatches(t, regexec(p, t, perl = TRUE))[[1]]
    if (length(m) >= 2) {
      n <- suppressWarnings(as.integer(m[2]))
      if (!is.na(n) && n >= SITREP_MIN && n <= SITREP_MAX) return(n)
    }
  }
  NA_integer_
}

# ------------------------------------------------------------
# 3) Nom de fichier canonique pour un numéro donné
# ------------------------------------------------------------
sitrep_canonical_filename <- function(sitrep_no) {
  sprintf("SitRep_%02d_%d.pdf", as.integer(sitrep_no), SITREP_YEAR)
}

# ------------------------------------------------------------
# 4) Validation : un fichier est-il un PDF réel ? (magic bytes)
# ------------------------------------------------------------
sitrep_is_valid_pdf <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(FALSE)
  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(close(con), add = TRUE)
  header <- tryCatch(readBin(con, "raw", n = 5L), error = function(e) raw(0))
  identical(header, charToRaw("%PDF-"))
}

# ------------------------------------------------------------
# 5) AUTO-TEST intégré (s'exécute si on lance le fichier seul).
#    Documente le comportement attendu et sert de garde-fou.
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  cat("=== AUTO-TEST module d'identité SitRep ===\n\n")
  cases_file <- list(
    list("C:/Users/AfricaCDC/Downloads/Visa_A3286866_TfjYN752.pdf", NA),  # visa -> NA
    list("data/pdf/SitRep_28_2026.pdf", 28L),
    list("data/pdf/SitRep_07_2026.pdf", 7L),
    list("sitrep-n16-mvb.pdf", 16L),
    list("SITREP_5_2026.pdf", 5L),
    list("facture_2024_999.pdf", NA),
    list("sitrep_752_2026.pdf", NA),       # 752 hors borne -> NA
    list("rapport_ebola_juin.pdf", NA)     # pas 'sitrep' -> NA
  )
  ok <- 0; ko <- 0
  for (c in cases_file) {
    got <- sitrep_no_from_filename(c[[1]])
    exp <- c[[2]]
    pass <- (is.na(got) && is.na(exp)) || (!is.na(got) && !is.na(exp) && got == exp)
    cat(sprintf("  [%s] %-45s -> %s (attendu %s)\n",
                if (pass) "OK" else "KO", basename(c[[1]]),
                ifelse(is.na(got),"NA",got), ifelse(is.na(exp),"NA",exp)))
    if (pass) ok <- ok + 1 else ko <- ko + 1
  }
  cat(sprintf("\nFichiers : %d OK, %d KO\n", ok, ko))

  cat("\n--- Titres web ---\n")
  cases_web <- list(
    list("SitRep N\u00b028/MVB_11/06/2026", 28L),
    list("SitRep N\u00b015/MVE_29/05/2026", 15L),
    list("Communiqu\u00e9 de presse sant\u00e9", NA),         # pas sitrep -> NA
    list("SitRep paludisme N\u00b03", NA)                  # pas mvb/ebola -> NA
  )
  for (c in cases_web) {
    got <- sitrep_no_from_web(c[[1]]); exp <- c[[2]]
    pass <- (is.na(got) && is.na(exp)) || (!is.na(got) && !is.na(exp) && got == exp)
    cat(sprintf("  [%s] %-35s -> %s (attendu %s)\n",
                if (pass) "OK" else "KO", c[[1]],
                ifelse(is.na(got),"NA",got), ifelse(is.na(exp),"NA",exp)))
  }
  cat("\nSi tous les tests sont OK, l'identification est fiable.\n")
}
