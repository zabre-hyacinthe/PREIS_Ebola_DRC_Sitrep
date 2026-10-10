# Tests hors ligne de scripts/preis_sitrep_discovery.R (reseau simule)
# Lancer : Rscript --vanilla scripts/tests/test_sitrep_discovery.R
suppressPackageStartupMessages({
  library(httr); library(rvest); library(xml2); library(stringr)
  library(dplyr); library(tibble); library(jsonlite)
})
root <- if (file.exists("scripts/preis_sitrep_discovery.R")) "." else ".."
source(file.path(root, "scripts", "preis_sitrep_discovery.R"), encoding = "UTF-8")

ok <- 0L; ko <- 0L
check <- function(name, cond) {
  if (isTRUE(cond)) { ok <<- ok + 1L; cat("PASS ", name, "\n") }
  else { ko <<- ko + 1L; cat("FAIL ", name, "\n") }
}

# --- 1. extraction du numero -------------------------------------------------
check("slug 146", identical(sd_sitrep_no("https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/"), 146L))
check("slug ancien format", identical(sd_sitrep_no("https://insp.cd/sitrep-mve-n-003-2026/"), 3L))
check("titre N deg", identical(sd_sitrep_no("SitRep N°146 /MVE-BDBV/07/10/2026"), 146L))
check("date n'est pas un numero", is.na(sd_sitrep_no("Sitrep du 07-10-2026")))
check("sans sitrep", is.na(sd_sitrep_no("https://insp.cd/communique-n12/")))
check("date extraite", identical(sd_date_from_url("https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/"), "2026-10-07"))

# --- 2. sources : HTML de recherche, RSS, JSON --------------------------------
html_search <- '<html><body>
 <a href="https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/">SitRep N°146</a>
 <a href="/sitrep-n145-mve-bdbv-06-10-2026/">SitRep N°145</a>
 <a href="https://insp.cd/category/sitrep/">Categorie</a>
 <a href="https://insp.cd/page/2/?s=sitrep">Suivant</a>
 <a href="https://insp.cd/autre-article/">Autre</a></body></html>'
d <- sd_candidates_from_body(html_search, "search")
check("html: 2 candidats", nrow(d) == 2 && all(c(145L, 146L) %in% d$sitrep_no))
check("html: categorie et pagination exclues", !any(grepl("category|page/2", d$post_url)))

rss <- '<?xml version="1.0"?><rss><channel><item><title>SitRep N°146</title>
 <link>https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/</link></item></channel></rss>'
d <- sd_candidates_from_body(rss, "feed")
check("rss: 146", nrow(d) == 1 && d$sitrep_no == 146L)

js <- '[{"id":1,"link":"https:\\/\\/insp.cd\\/rapport-journalier-ebola\\/","title":{"rendered":"SitRep N°147 du 08 octobre"}},
        {"id":2,"link":"https:\\/\\/insp.cd\\/autre\\/","title":{"rendered":"Autre"}}]'
d <- sd_candidates_from_body('[{"link":"https:\\/\\/insp.cd\\/rapport-journalier-ebola\\/","title":{"rendered":"SitRep N\\u00b0147 du 08 octobre"}}]', "wp-json")
check("json: numero lu dans le titre (URL sans numero)", nrow(d) == 1 && d$sitrep_no == 147L)

pdf_only <- '<a href="https://insp.cd/wp-content/uploads/2026/10/Sitrep_MVE-N-146.pdf">pdf</a>'
d <- sd_candidates_from_body(pdf_only, "x")
check("pdf direct reconnu", nrow(d) == 1 && d$sitrep_no == 146L)

# --- 3. gabarit de sondage ---------------------------------------------------
pu <- sd_probe_urls("https://insp.cd/sitrep-n145-mve-bdbv-06-10-2026/", 145L,
                    as.Date(c("2026-10-08", "2026-10-07")), ahead = 1L)
check("gabarit appris: 146 / 08-10", "https://insp.cd/sitrep-n146-mve-bdbv-08-10-2026/" %in% pu)
check("gabarit appris: 146 / 07-10", "https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/" %in% pu)
pu2 <- sd_probe_urls("https://insp.cd/sitrep-mve-n-003-2026/", 3L, as.Date("2026-10-08"), ahead = 1L)
check("ancien format: zero-padding conserve", any(grepl("sitrep-mve-n-004-2026", pu2)))
check("pas de sondage sans numero", length(sd_probe_urls(character(), NA_integer_, Sys.Date())) == 0)

# --- 4. sondage : redirection WordPress vers un article voisin ---------------
fetch_redir <- function(u) {
  if (grepl("n147", u)) return(list(status = 200L, url = "https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/",
                                    body = "<html>SitRep 146</html>"))
  NULL
}
pr <- sd_probe(c("https://insp.cd/sitrep-n147-mve-bdbv-08-10-2026/"), fetch = fetch_redir)
check("redirection vers un autre numero rejetee", nrow(pr) == 0)
fetch_ok <- function(u) if (grepl("n146-mve-bdbv-07-10", u))
  list(status = 200L, url = u, body = "<html><title>SitRep N°146 - INSP</title>SitRep</html>") else NULL
pr <- sd_probe(c("https://insp.cd/sitrep-n146-mve-bdbv-08-10-2026/",
                 "https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/"), fetch = fetch_ok)
check("sondage: bonne URL retenue", nrow(pr) == 1 && pr$sitrep_no == 146L && grepl("07-10-2026", pr$post_url))

# soft 404 : 200 + menu contenant "sitrep", mais ni titre ni PDF du numero sonde
fetch_soft <- function(u) list(status = 200L, url = u,
  body = "<html><title>Page introuvable</title><a href='/category/sitrep/'>SitRep</a></html>")
pr <- sd_probe("https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/", fetch = fetch_soft)
check("soft 404 rejete", nrow(pr) == 0)
fetch_pdf <- function(u) list(status = 200L, url = u,
  body = "<html><title>Rapport</title>sitrep<div pdfemb-data=QUJD></div></html>")
pr <- sd_probe("https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/", fetch = fetch_pdf)
check("page avec PDF accepte meme sans numero dans le titre", nrow(pr) == 1)

# --- 5. scenario incident 146 : categorie sans 146, recherche vide, sondage ----
cat_posts <- tibble(post_url = c("https://insp.cd/sitrep-n145-mve-bdbv-06-10-2026/",
                                 "https://insp.cd/sitrep-n144-mve-bdbv-05-10-2026/"),
                    post_text = c("SitRep 145", "SitRep 144"), sitrep_no = c(145L, 144L),
                    sitrep_date = c("2026-10-06", "2026-10-05"))
fetch_146 <- function(u) {
  if (grepl("n146-mve-bdbv-07-10-2026", u))
    return(list(status = 200L, url = u, body = "<html><title>SitRep N°146</title>sitrep</html>"))
  NULL  # tout le reste : inaccessible (403/timeout)
}
res <- sd_augment(cat_posts, fetch = fetch_146, today = as.Date("2026-10-08"),
                  manual_file = tempfile())
check("incident 146: retrouve par sondage", max(res$sitrep_no) == 146L)
check("incident 146: source = probe", res$source[res$sitrep_no == 146L] == "probe")
check("incident 146: 144 et 145 conserves", all(c(144L, 145L) %in% res$sitrep_no))

# --- 6. scenario : trouve par la recherche du site (slug non standard) ---------
fetch_search <- function(u) {
  if (grepl("\\?s=sitrep$", u))
    return(list(status = 200L, url = u,
                body = '<a href="https://insp.cd/rapport-n146-ebola/">SitRep N°146 /MVE-BDBV</a>'))
  NULL
}
res <- sd_augment(cat_posts, fetch = fetch_search, today = as.Date("2026-10-08"),
                  manual_file = tempfile())
check("recherche: 146 trouve", max(res$sitrep_no) == 146L &&
        grepl("rapport-n146", res$post_url[res$sitrep_no == 146L]))

# --- 7. URL manuelle (lien transmis par un collegue) ---------------------------
mf <- tempfile(fileext = ".txt")
writeLines(c("# lien transmis le 10/10", "https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/"), mf)
res <- sd_augment(cat_posts, fetch = function(u) NULL, today = as.Date("2026-10-08"), manual_file = mf)
check("manuel: 146 pris en compte", max(res$sitrep_no) == 146L && res$source[res$sitrep_no == 146L] == "manual")

# --- 8. garde-fous -------------------------------------------------------------
fetch_bad <- function(u) if (grepl("\\?s=sitrep$", u))
  list(status = 200L, url = u, body = '<a href="https://insp.cd/sitrep-n999-faux/">x</a>') else NULL
res <- sd_augment(cat_posts, fetch = fetch_bad, today = as.Date("2026-10-08"), manual_file = tempfile())
check("numero improbable (999) rejete", max(res$sitrep_no) == 145L)

res <- sd_augment(cat_posts, fetch = function(u) stop("reseau en panne"), today = as.Date("2026-10-08"),
                  manual_file = tempfile())
check("reseau en panne: categorie intacte, aucune erreur", identical(sort(res$sitrep_no), c(144L, 145L)))

res <- sd_augment(tibble(), fetch = fetch_146, today = as.Date("2026-10-08"), manual_file = tempfile())
check("categorie injoignable (tibble vide): rien ne casse", is.data.frame(res))
mf2 <- tempfile(fileext = ".txt")
writeLines("https://insp.cd/sitrep-n146-mve-bdbv-07-10-2026/", mf2)
res <- sd_augment(tibble(), fetch = function(u) NULL, today = as.Date("2026-10-08"), manual_file = mf2)
check("categorie injoignable + URL manuelle: 146", nrow(res) == 1 && res$sitrep_no == 146L)

# categorie : prioritaire a numero egal
fetch_dup <- function(u) if (grepl("\\?s=sitrep$", u))
  list(status = 200L, url = u, body = '<a href="https://insp.cd/sitrep-n145-copie/">x</a>') else NULL
res <- sd_augment(cat_posts, fetch = fetch_dup, today = as.Date("2026-10-08"), manual_file = tempfile())
check("categorie prioritaire a numero egal", res$post_url[res$sitrep_no == 145L] == cat_posts$post_url[1])

cat(sprintf("\n%d PASS, %d FAIL\n", ok, ko))
if (ko > 0) quit(status = 1)
