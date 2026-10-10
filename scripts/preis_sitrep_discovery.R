############################################################
# PREIS - Decouverte des SitRep INSP hors de la page de categorie
# Fichier : scripts/preis_sitrep_discovery.R
#
# Contexte (incident 10/10/2026) : le SitRep 146 a ete publie par l'INSP
# sans apparaitre sur https://insp.cd/category/sitrep/ ; le moniteur, qui
# ne lisait que cette page, ne l'a jamais vu. Ce fichier ajoute des sources
# SUPPLEMENTAIRES (recherche du site, flux RSS, API WordPress, page
# d'accueil, liste d'URL fournie a la main, sondage de l'URL probable du
# numero suivant). Il ne remplace rien : la lecture de la categorie reste
# la source principale ; ces sources ne font qu'ajouter des candidats.
#
# Regles de securite :
#   - aucune source ne peut faire echouer le moniteur (tout est sous
#     tryCatch) ;
#   - un candidat n'est retenu que si son numero est lisible dans l'URL
#     (ou le titre) sous la forme "SitRep ... N 146" ;
#   - un candidat a un numero improbable (> numero connu + 10) est rejete ;
#   - un sondage dont l'URL finale (apres redirection WordPress) ne porte
#     pas le numero sonde est rejete.
#
# Fonctions pures, avec "fetch" injectable pour les tests hors ligne
# (scripts/tests/test_sitrep_discovery.R).
############################################################

SD_BASE <- "https://insp.cd"

# --- Lecture reseau par defaut : renvoie list(status, url, body) ou NULL -----
sd_default_fetch <- function(url, timeout_sec = 25) {
  resp <- tryCatch(
    httr::GET(
      url,
      httr::timeout(timeout_sec),
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 PREIS-Ebola-DRC-Monitor",
        "Accept" = "text/html,application/xhtml+xml,application/xml,application/json,*/*",
        "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8"
      )
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  body <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"),
                   error = function(e) "")
  list(status = httr::status_code(resp), url = resp$url, body = body)
}

# --- Numero de SitRep : exige un marqueur "n" avant les chiffres ------------
# Accepte : sitrep-n146-mve-bdbv-07-10-2026 ; sitrep-mve-n-003-2026 ;
#           "SitRep N-146 /MVE-BDBV/07/10/2026".
# Refuse : "sitrep du 07-10-2026" (une date n'est pas un numero).
sd_sitrep_no <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NA_integer_)
  x <- paste(as.character(x), collapse = " ")
  x <- tryCatch(utils::URLdecode(x), error = function(e) x)
  x <- stringr::str_to_lower(x)
  m <- stringr::str_match(
    x, "sitrep[^0-9]{0,20}?(?<![a-z])n[\\u00b0\\u00ba]?[^0-9a-z]{0,3}0*(\\d{1,3})(?!\\d)"
  )[, 2]
  if (is.na(m)) return(NA_integer_)
  as.integer(m)
}

sd_date_from_url <- function(x) {
  m <- stringr::str_match(as.character(x), "(\\d{2})-(\\d{2})-(\\d{4})")
  if (is.na(m[1, 1])) return(NA_character_)
  paste0(m[1, 4], "-", m[1, 3], "-", m[1, 2])
}

# --- URL a ecarter : pages de listing, flux, API, commentaires, medias ------
sd_is_listing_url <- function(u) {
  stringr::str_detect(
    stringr::str_to_lower(u),
    "/category/|/tag/|/page/\\d|[?&]s=|[?&]paged=|/feed/?$|/feed/|wp-json|/comments|#|/author/|xmlrpc|/wp-admin|/wp-login"
  )
}

sd_empty <- function() {
  tibble::tibble(
    post_url = character(), post_text = character(),
    sitrep_no = integer(), sitrep_date = character(), source = character()
  )
}

# --- Candidats extraits d'un corps (HTML, RSS, JSON, sitemap) ---------------
sd_candidates_from_body <- function(body, source, base = SD_BASE) {
  if (is.null(body) || length(body) == 0 || is.na(body) || !nzchar(body)) {
    return(sd_empty())
  }
  rows <- list()

  # 1) JSON WordPress : tableau d'objets avec "link" et "title.rendered"
  if (stringr::str_detect(stringr::str_sub(stringr::str_trim(body), 1, 1), "[\\[{]")) {
    js <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE),
                   error = function(e) NULL)
    if (is.list(js) && length(js) > 0) {
      if (!is.null(js$link)) js <- list(js)
      for (it in js) {
        if (!is.list(it) || is.null(it$link)) next
        ttl <- tryCatch(it$title$rendered, error = function(e) NULL)
        if (is.null(ttl)) ttl <- tryCatch(it$title, error = function(e) NULL)
        if (!is.character(ttl)) ttl <- NA_character_
        rows[[length(rows) + 1]] <- tibble::tibble(
          post_url = as.character(it$link)[1], post_text = ttl[1]
        )
      }
    }
  }

  # 2) Toute URL du site presente dans le corps (HTML, RSS, sitemap, JSON brut)
  txt <- gsub("\\\\/", "/", body)
  urls <- unique(unlist(stringr::str_extract_all(
    txt, "https?://(?:www\\.)?insp\\.cd/[^\"'<>\\s\\\\)]+"
  )))
  if (length(urls) > 0) {
    rows[[length(rows) + 1]] <- tibble::tibble(post_url = urls, post_text = NA_character_)
  }

  # 3) Liens relatifs d'une page HTML (ancre + texte)
  if (stringr::str_detect(body, "<a\\s")) {
    doc <- tryCatch(rvest::read_html(body), error = function(e) NULL)
    if (!is.null(doc)) {
      a <- rvest::html_nodes(doc, "a")
      if (length(a) > 0) {
        hrefs <- rvest::html_attr(a, "href")
        txts <- rvest::html_text(a, trim = TRUE)
        ok <- !is.na(hrefs) & nzchar(hrefs)
        if (any(ok)) {
          rows[[length(rows) + 1]] <- tibble::tibble(
            post_url = xml2::url_absolute(hrefs[ok], base), post_text = txts[ok]
          )
        }
      }
    }
  }

  if (length(rows) == 0) return(sd_empty())
  d <- dplyr::bind_rows(rows)
  d <- d[!is.na(d$post_url), , drop = FALSE]
  d <- d[stringr::str_detect(d$post_url, "^https?://(?:www\\.)?insp\\.cd/"), , drop = FALSE]
  d <- d[!sd_is_listing_url(d$post_url), , drop = FALSE]
  if (nrow(d) == 0) return(sd_empty())

  d$sitrep_no <- vapply(seq_len(nrow(d)), function(i) {
    n <- sd_sitrep_no(d$post_url[i])
    if (is.na(n)) n <- sd_sitrep_no(d$post_text[i])
    n
  }, integer(1))
  d <- d[!is.na(d$sitrep_no), , drop = FALSE]
  if (nrow(d) == 0) return(sd_empty())
  d$sitrep_date <- vapply(d$post_url, sd_date_from_url, character(1), USE.NAMES = FALSE)
  d$source <- source
  # un meme numero peut apparaitre en page et en PDF : on garde la page d'abord
  d$is_pdf <- stringr::str_detect(stringr::str_to_lower(d$post_url), "\\.pdf($|\\?)")
  d <- d[order(d$sitrep_no, d$is_pdf, decreasing = c(TRUE, FALSE), method = "radix"), , drop = FALSE]
  d <- d[!duplicated(d$sitrep_no), c("post_url", "post_text", "sitrep_no", "sitrep_date", "source"), drop = FALSE]
  tibble::as_tibble(d)
}

# --- URLs probables du numero suivant ---------------------------------------
# On apprend le gabarit du slug le plus recent connu (numero et date
# remplaces), et on ajoute le gabarit observe le 10/10/2026 :
#   sitrep-n146-mve-bdbv-07-10-2026
sd_probe_urls <- function(known_urls, max_no, dates, ahead = 2L, base = SD_BASE) {
  if (is.na(max_no)) return(character())
  slugs <- character()
  if (length(known_urls) > 0) {
    kn <- known_urls[!is.na(known_urls)]
    kn <- kn[!stringr::str_detect(stringr::str_to_lower(kn), "\\.pdf($|\\?)")]
    if (length(kn) > 0) {
      nums <- vapply(kn, sd_sitrep_no, integer(1))
      kn <- kn[!is.na(nums) & nums == max_no]
      if (length(kn) > 0) {
        slug <- basename(sub("/+$", "", sub("[?#].*$", "", kn[1])))
        slugs <- c(slugs, slug)
      }
    }
  }
  out <- character()
  for (k in seq_len(ahead)) {
    n <- max_no + k
    for (d in as.character(dates)) {
      dd <- format(as.Date(d), "%d-%m-%Y")
      # a) gabarit appris du dernier slug connu
      for (s in slugs) {
        m <- stringr::str_match(stringr::str_to_lower(s),
          "^(.*?sitrep[^0-9]{0,20}?(?<![a-z])n[\\u00b0\\u00ba]?[^0-9a-z]{0,3})(0*)(\\d{1,3})(.*)$")
        if (is.na(m[1, 1])) next
        width <- nchar(m[1, 3]) + nchar(m[1, 4])
        num <- formatC(n, width = width, flag = "0")
        rest <- stringr::str_replace(m[1, 5], "\\d{2}-\\d{2}-\\d{4}", dd)
        out <- c(out, paste0(base, "/", m[1, 2], num, rest, "/"))
      }
      # b) gabarit par defaut
      out <- c(out, paste0(base, "/sitrep-n", n, "-mve-bdbv-", dd, "/"))
    }
  }
  utils::head(unique(out), 40L)
}

# --- Sondage : une URL est acceptee si elle repond 200, parle de SitRep et
#     si l'URL FINALE porte le numero sonde (WordPress redirige parfois un
#     slug inconnu vers un article voisin : ce cas est rejete).
sd_probe <- function(urls, fetch = sd_default_fetch, log = function(...) invisible(NULL)) {
  rows <- list()
  for (u in urls) {
    r <- tryCatch(fetch(u), error = function(e) NULL)
    if (is.null(r) || is.null(r$status) || r$status != 200) next
    body <- if (is.null(r$body)) "" else r$body
    if (!stringr::str_detect(stringr::str_to_lower(body), "sitrep")) next
    final <- if (is.null(r$url) || is.na(r$url)) u else r$url
    want <- sd_sitrep_no(u)
    got <- sd_sitrep_no(final)
    if (is.na(want) || is.na(got) || want != got) {
      log("Probe ", u, " -> redirige vers ", final, " (numero different) : rejete")
      next
    }
    ttl <- stringr::str_match(body, "(?is)<title[^>]*>(.*?)</title>")[, 2]
    h1 <- stringr::str_match(body, "(?is)<h1[^>]*>(.*?)</h1>")[, 2]
    # Garde anti "soft 404" : certains sites repondent 200 sur une page
    # d'erreur dont le menu contient le mot "sitrep". On exige que le titre ou
    # le h1 porte le numero sonde, ou que la page expose un PDF.
    has_no <- identical(sd_sitrep_no(ttl), want) || identical(sd_sitrep_no(h1), want)
    has_pdf <- stringr::str_detect(body, "(?i)pdfemb-data|wp-content/uploads/[^\"'\\s]+\\.pdf")
    if (!has_no && !has_pdf) {
      log("Probe ", u, " : 200 mais ni titre ni PDF ne confirment N", want, " (soft 404 ?) : rejete")
      next
    }
    rows[[length(rows) + 1]] <- tibble::tibble(
      post_url = final,
      post_text = if (is.na(ttl)) NA_character_ else stringr::str_squish(ttl),
      sitrep_no = got, sitrep_date = sd_date_from_url(final), source = "probe"
    )
    log("Probe OK : SitRep N", got, " -> ", final)
  }
  if (length(rows) == 0) return(sd_empty())
  dplyr::bind_rows(rows)
}

# --- Sources supplementaires (hors categorie) -------------------------------
sd_extra_sources <- function(base = SD_BASE) {
  src <- c(
    paste0(base, "/?s=sitrep"),
    paste0(base, "/?s=sitrep+mve"),
    paste0(base, "/?s=sitrep&paged=2"),
    paste0(base, "/feed/"),
    paste0(base, "/wp-json/wp/v2/posts?search=sitrep&per_page=50&orderby=date&order=desc"),
    paste0(base, "/wp-json/wp/v2/posts?per_page=50&orderby=date&order=desc"),
    paste0(base, "/")
  )
  extra <- trimws(unlist(strsplit(Sys.getenv("PREIS_SITREP_EXTRA_SOURCES", ""), "[,;\\s]+")))
  unique(c(src, extra[nzchar(extra)]))
}

# URL fournies a la main (ex. lien transmis par un collegue) : un fichier
# data/monitor_state/extra_sitrep_urls.txt (une URL par ligne, # = commentaire)
# et/ou la variable PREIS_SITREP_EXTRA_URLS.
sd_manual_urls <- function(file = file.path("data", "monitor_state", "extra_sitrep_urls.txt")) {
  u <- character()
  if (file.exists(file)) {
    l <- trimws(readLines(file, warn = FALSE, encoding = "UTF-8"))
    u <- c(u, l[nzchar(l) & !startsWith(l, "#")])
  }
  e <- trimws(unlist(strsplit(Sys.getenv("PREIS_SITREP_EXTRA_URLS", ""), "[,;\\s]+")))
  unique(c(u, e[nzchar(e)]))
}

# --- Orchestration -----------------------------------------------------------
# posts : tibble deja obtenu depuis la categorie (peut etre vide).
# Renvoie posts augmente de candidats supplementaires (jamais d'erreur).
sd_augment <- function(posts,
                       fetch = sd_default_fetch,
                       log = function(...) invisible(NULL),
                       today = as.Date(Sys.time(), tz = "Africa/Lubumbashi"),
                       manual_file = file.path("data", "monitor_state", "extra_sitrep_urls.txt"),
                       base = SD_BASE) {
  if (is.null(posts) || nrow(posts) == 0) {
    posts <- tibble::tibble(post_url = character(), post_text = character(),
                            sitrep_no = integer(), sitrep_date = character())
  }
  posts$sitrep_no <- as.integer(posts$sitrep_no)
  if (!"source" %in% names(posts)) posts$source <- rep("category", nrow(posts))
  known_max <- if (nrow(posts) > 0) max(posts$sitrep_no, na.rm = TRUE) else NA_integer_
  found <- list()

  add <- function(d, label) {
    if (is.null(d) || nrow(d) == 0) return(invisible(NULL))
    found[[length(found) + 1]] <<- d
    log("Source ", label, ": ", nrow(d), " candidat(s), max N", max(d$sitrep_no))
  }

  # (a) sources supplementaires (recherche, RSS, API, accueil, sources en variable)
  for (s in sd_extra_sources(base)) {
    d <- tryCatch({
      r <- fetch(s)
      if (is.null(r) || is.null(r$status) || r$status != 200) {
        log("Source ", s, ": inaccessible")
        NULL
      } else {
        sd_candidates_from_body(r$body, source = s, base = base)
      }
    }, error = function(e) { log("Source ", s, ": erreur ", conditionMessage(e)); NULL })
    add(d, s)
  }

  # (b) URL fournies a la main
  mu <- tryCatch(sd_manual_urls(manual_file), error = function(e) character())
  if (length(mu) > 0) {
    rows <- lapply(mu, function(u) {
      d <- tibble::tibble(post_url = u, post_text = NA_character_)
      n <- sd_sitrep_no(u)
      if (is.na(n)) {
        r <- tryCatch(fetch(u), error = function(e) NULL)
        t <- if (!is.null(r) && !is.null(r$body))
          stringr::str_match(r$body, "(?is)<title[^>]*>(.*?)</title>")[, 2] else NA_character_
        n <- sd_sitrep_no(t)
        d$post_text <- t
      }
      d$sitrep_no <- n
      d$sitrep_date <- sd_date_from_url(u)
      d$source <- "manual"
      d
    })
    add(dplyr::filter(dplyr::bind_rows(rows), !is.na(sitrep_no)), "manuel")
  }

  all_known <- if (length(found) > 0) dplyr::bind_rows(found) else sd_empty()
  cur <- dplyr::bind_rows(
    posts[, intersect(names(posts), c("post_url", "post_text", "sitrep_no", "sitrep_date", "source")), drop = FALSE],
    all_known
  )

  # garde-fou : numero improbable (typo, autre numerotation) -> rejete
  if (!is.na(known_max)) {
    bad <- !is.na(cur$sitrep_no) & cur$sitrep_no > known_max + 10L
    if (any(bad)) log("Rejet de ", sum(bad), " candidat(s) au numero improbable (> N", known_max + 10L, ")")
    cur <- cur[!bad, , drop = FALSE]
  }

  # (c) sondage du/des numero(s) suivant(s)
  max_now <- if (nrow(cur) > 0) max(cur$sitrep_no, na.rm = TRUE) else NA_integer_
  if (!is.na(max_now)) {
    dates <- seq(today, by = "-1 day", length.out = 5)
    pu <- sd_probe_urls(cur$post_url, max_now, dates, ahead = 2L, base = base)
    if (length(pu) > 0) {
      log("Sondage de ", length(pu), " URL probable(s) pour N", max_now + 1L, " et N", max_now + 2L)
      pr <- tryCatch(sd_probe(pu, fetch = fetch, log = log), error = function(e) sd_empty())
      if (nrow(pr) > 0) cur <- dplyr::bind_rows(cur, pr)
    }
  }

  if (nrow(cur) == 0) return(posts)
  cur$is_pdf <- stringr::str_detect(stringr::str_to_lower(cur$post_url), "\\.pdf($|\\?)")
  # a numero egal : la categorie d'abord, puis une page, puis un PDF direct
  cur$prio <- ifelse(cur$source == "category", 0L, ifelse(cur$is_pdf, 2L, 1L))
  cur <- cur[order(-cur$sitrep_no, cur$prio), , drop = FALSE]
  cur <- cur[!duplicated(cur$sitrep_no), , drop = FALSE]
  cur$is_pdf <- NULL
  cur$prio <- NULL
  tibble::as_tibble(cur)
}
