############################################################
# PREIS EBOLA DRC
# 04_send_email_alert.R
#
# Envoie un email de surveillance à chaque exécution :
#   - Résumé du dernier SitRep (cas, décès, CFR, nouveaux)
#   - Variation vs SitRep précédent
#   - Signaux opérationnels (ROUGE/ORANGE)
#   - Recommandations automatiques (drivers probables, prudence)
#   - Lien vers le dashboard Shiny
#
# Identifiants lus depuis .Renviron (jamais en clair) :
#   SMTP_USER, SMTP_PASS, SMTP_HOST, SMTP_PORT,
#   ALERT_FROM, PREIS_DASHBOARD_URL
#
# Destinataire(s) : variable ALERT_TO ci-dessous.
############################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(glue)
})

# ---- Config ----
BASE_DIR   <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
DATA_FINAL <- file.path(BASE_DIR, "data/final")
OUT_DIR    <- file.path(BASE_DIR, "outputs/analyse")

# Destinataire(s) — par défaut, soi-même. Sépare par des virgules pour plusieurs.
ALERT_TO   <- Sys.getenv("ALERT_TO", unset = Sys.getenv("ALERT_FROM"))

# Identifiants SMTP (depuis .Renviron)
smtp_user  <- Sys.getenv("SMTP_USER")
smtp_pass  <- Sys.getenv("SMTP_PASS")
smtp_host  <- Sys.getenv("SMTP_HOST", "smtp.gmail.com")
smtp_port  <- as.integer(Sys.getenv("SMTP_PORT", "465"))
alert_from <- Sys.getenv("ALERT_FROM", smtp_user)
dash_url   <- Sys.getenv("PREIS_DASHBOARD_URL", "")

if (smtp_user == "" || smtp_pass == "") {
  stop("SMTP_USER / SMTP_PASS absents du .Renviron. ",
       "Verifie avec Sys.getenv('SMTP_USER').")
}

# ---- 1. Charger la série nationale consolidée ----
serie_fp <- file.path(OUT_DIR, "serie_temporelle_nationale.csv")
if (!file.exists(serie_fp)) {
  stop("Serie introuvable : ", serie_fp,
       "\nLance d'abord 03_analyse_consolidee.R.")
}
serie <- readr::read_csv(serie_fp, show_col_types = FALSE) %>%
  dplyr::arrange(sitrep_no)

last <- serie %>% dplyr::slice_tail(n = 1)
prev <- serie %>% dplyr::slice_tail(n = 2) %>% dplyr::slice_head(n = 1)

# ---- 2. Construire les signaux + recommandations ----
fmt <- function(x) if (is.na(x)) "non disponible" else format(x, big.mark = " ")
delta <- function(a, b) {
  if (is.na(a) || is.na(b)) return("")
  d <- a - b
  sprintf(" (%+d vs SitRep précédent)", as.integer(d))
}

signaux <- c()
recos   <- c()

# Signal CFR
if (!is.na(last$cfr) && last$cfr >= 15) {
  signaux <- c(signaux, sprintf("🔴 Létalité élevée : CFR %.1f%%", last$cfr))
  recos <- c(recos, paste0(
    "CFR élevé — vérifier délais présentation→soins, proportion de décès ",
    "communautaires, et capacité de prise en charge dans les zones actives."))
}
# Signal nouveaux décès
if (!is.na(last$nouveaux_deces) && last$nouveaux_deces > 0) {
  signaux <- c(signaux, sprintf("🔴 %d nouveau(x) décès depuis le dernier SitRep",
                                as.integer(last$nouveaux_deces)))
  recos <- c(recos, paste0(
    "Nouveaux décès — investiguer chaque décès (lieu, délai, statut contact connu) ",
    "pour distinguer transmission active vs décès communautaires."))
}
# Signal nouveaux cas
if (!is.na(last$nouveaux_cas) && last$nouveaux_cas > 0) {
  signaux <- c(signaux, sprintf("🟠 %d nouveau(x) cas confirmé(s)",
                                as.integer(last$nouveaux_cas)))
  recos <- c(recos, paste0(
    "Nouveaux cas — confirmer la part issue de contacts déjà listés ",
    "(transmission maîtrisée) vs cas hors liste (chaînes non élucidées)."))
}
if (length(signaux) == 0) signaux <- "🟢 Aucun signal critique sur ce SitRep."
if (length(recos)   == 0) recos   <- "Poursuivre la surveillance de routine."

# ---- 3. Corps HTML ----
sig_html  <- paste0("<li>", signaux, "</li>", collapse = "\n")
reco_html <- paste0("<li>", recos,   "</li>", collapse = "\n")

dash_block <- if (nzchar(dash_url)) {
  glue('<p style="margin:18px 0;">
    <a href="{dash_url}" style="background:#C0392B;color:#fff;padding:10px 18px;
    text-decoration:none;border-radius:5px;font-weight:bold;">
    Ouvrir le tableau de bord PREIS →</a></p>')
} else ""

body_html <- glue('
<div style="font-family:Arial,sans-serif;max-width:640px;margin:auto;color:#2C3E50;">
  <h2 style="color:#C0392B;border-bottom:2px solid #C0392B;padding-bottom:6px;">
    PREIS Ebola RDC — Alerte SitRep N°{last$sitrep_no}</h2>
  <p style="color:#888;">Rapport du {last$date} · 17e épidémie (Bundibugyo, Ituri/Nord-Kivu/Sud-Kivu)</p>

  <h3>Données clés (cumuls nationaux, source INRB validée)</h3>
  <table style="border-collapse:collapse;width:100%;">
    <tr><td style="padding:6px;border-bottom:1px solid #eee;">Cas confirmés cumulés</td>
        <td style="padding:6px;border-bottom:1px solid #eee;text-align:right;">
        <b>{fmt(last$cas_cumules)}</b>{delta(last$cas_cumules, prev$cas_cumules)}</td></tr>
    <tr><td style="padding:6px;border-bottom:1px solid #eee;">Décès cumulés</td>
        <td style="padding:6px;border-bottom:1px solid #eee;text-align:right;">
        <b>{fmt(last$deces_cumules)}</b>{delta(last$deces_cumules, prev$deces_cumules)}</td></tr>
    <tr><td style="padding:6px;border-bottom:1px solid #eee;">Nouveaux décès</td>
        <td style="padding:6px;border-bottom:1px solid #eee;text-align:right;">
        <b>{fmt(last$nouveaux_deces)}</b></td></tr>
    <tr><td style="padding:6px;border-bottom:1px solid #eee;">Létalité (CFR provisoire)</td>
        <td style="padding:6px;border-bottom:1px solid #eee;text-align:right;">
        <b>{ifelse(is.na(last$cfr),"n/d",paste0(last$cfr,"%"))}</b></td></tr>
  </table>

  <h3>Signaux opérationnels</h3>
  <ul>{sig_html}</ul>

  <h3>Recommandations</h3>
  <ul>{reco_html}</ul>

  {dash_block}

  <p style="font-size:11px;color:#999;margin-top:24px;border-top:1px solid #eee;padding-top:10px;">
  CFR provisoire : pendant une épidémie active, certains cas récents peuvent encore évoluer ;
  ne pas interpréter comme létalité finale. Drivers probables uniquement — pas de causalité établie.
  Cumuls nationaux = données INRB validées ; détails par zone = extraction PDF à valider.
  Généré automatiquement par le pipeline PREIS le {Sys.Date()}.
  </p>
</div>')

# ---- 4. Envoi via blastula ----
if (!requireNamespace("blastula", quietly = TRUE)) {
  stop("Package 'blastula' requis : install.packages('blastula')")
}
library(blastula)

email <- compose_email(body = md(body_html))

# Pièces jointes : graphiques s'ils existent
for (img in c("g1_courbe_epidemique.png", "g2_courbe_mortalite.png",
              "g3_evolution_cfr.png", "carte_zones_intensite.png")) {
  fp <- file.path(OUT_DIR, img)
  if (file.exists(fp)) email <- add_attachment(email, file = fp)
}

creds <- creds_envvar(
  user       = smtp_user,
  pass_envvar= "SMTP_PASS",
  host       = smtp_host,
  port       = smtp_port,
  use_ssl    = TRUE
)

subject <- sprintf("[PREIS Ebola RDC] SitRep N°%d — %s cas, %s décès (CFR %s%%)",
                   last$sitrep_no, fmt(last$cas_cumules), fmt(last$deces_cumules),
                   ifelse(is.na(last$cfr), "n/d", last$cfr))

cat("Envoi de l'email à :", ALERT_TO, "...\n")
smtp_send(
  email,
  from = alert_from,
  to   = strsplit(ALERT_TO, "[,;] *")[[1]],
  subject = subject,
  credentials = creds
)
cat("Email envoyé avec succès.\n")
