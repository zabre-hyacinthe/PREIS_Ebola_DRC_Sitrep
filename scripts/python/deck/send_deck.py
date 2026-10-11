# -*- coding: utf-8 -*-
"""Envoi SMTP du deck (memes variables que 05b : SMTP_HOST/PORT/USER/PASS, ALERT_FROM, ALERT_TO)."""
import html
import mimetypes
import os
import smtplib
import ssl
from email.message import EmailMessage


def split_addrs(x):
    import re
    return [a for a in dict.fromkeys(t.strip() for t in re.split(r"[,;]", x or "")) if a]


def env_first(*names, default=""):
    for n in names:
        v = os.environ.get(n)
        if v and v.strip():
            return v.strip()
    return default


def recipients(test=False):
    """ops = ALERT_TO ; destinataires du deck = PREIS_DECK_TO si defini, sinon ops seuls (jamais d'envoi large par defaut)."""
    ops = split_addrs(env_first("ALERT_TO", "EMAIL_TO", "PREIS_ALERT_TO"))
    if test:
        return ops, ops
    deck = split_addrs(env_first("PREIS_DECK_TO"))
    return ops, (deck or ops)


def build_message(sender, to, subject, plain, html_body, attachments=()):
    msg = EmailMessage()
    msg["From"] = sender
    msg["To"] = ", ".join(to)
    msg["Subject"] = subject
    msg.set_content(plain)
    msg.add_alternative(html_body, subtype="html")
    for fp in attachments:
        if not os.path.isfile(fp):
            continue
        ctype = {".pptx": "application/vnd.openxmlformats-officedocument.presentationml.presentation",
                 ".csv": "text/csv"}.get(os.path.splitext(fp)[1].lower()) or mimetypes.guess_type(fp)[0] or "application/octet-stream"
        mt, st = ctype.split("/", 1)
        with open(fp, "rb") as f:
            msg.add_attachment(f.read(), maintype=mt, subtype=st, filename=os.path.basename(fp))
    return msg


def smtp_send(msg):
    host = env_first("SMTP_HOST", default="smtp.gmail.com")
    port = int(env_first("SMTP_PORT", default="465"))
    user = env_first("SMTP_USER", "SMTP_USERNAME")
    pw = env_first("SMTP_PASS", "SMTP_PASSWORD")
    if not user or not pw:
        raise RuntimeError("identifiants SMTP manquants (SMTP_USER / SMTP_PASS)")
    use_ssl = env_first("SMTP_SSL").lower() in ("1", "true", "yes") or port == 465
    ctx = ssl.create_default_context()
    if use_ssl:
        with smtplib.SMTP_SSL(host, port, timeout=120, context=ctx) as s:
            s.login(user, pw)
            s.send_message(msg)
    else:
        with smtplib.SMTP(host, port, timeout=120) as s:
            s.ehlo()
            s.starttls(context=ctx)
            s.ehlo()
            s.login(user, pw)
            s.send_message(msg)


def deck_mail(report, test=False):
    """(sujet, texte, html) a partir de deck_report.json."""
    r = report
    ret = r.get("retained") or []
    warns = r.get("warnings") or []
    rec = r.get("reconciliation") or {}
    d = str(r.get("report_date", ""))
    subj = "%s[PREIS] BDBV Dashboard Issue %s - SitRep %s (%s)" % ("[TEST] " if test else "", r.get("issue"), r.get("sitrep_no"), d)
    lines = [
        "Deck BDBV mis a jour automatiquement a partir du SitRep %s (date du rapport %s)." % (r.get("sitrep_no"), d),
        "Comparaisons jour/jour avec le SitRep %s. Verification independante du fichier enregistre : %s controles reussis, 0 echec." % (r.get("prev_sitrep"), r.get("checks_passed")),
        "Reconciliation : cas %s ; deces %s." % (rec.get("cases"), rec.get("deaths")),
        "",
        "Elements NON rafraichis par le SitRep (conserves et signales) : %d" % len(ret),
    ]
    lines += ["  - diapo %s : %s (%s)" % (x.get("slide"), x.get("element"), x.get("reason")) for x in ret]
    if warns:
        lines += ["", "Avertissements de lecture :"] + ["  - %s" % w for w in warns]
    lines += ["", "Pieces jointes : le deck et le fichier de controle (chaque modification : ancien -> nouveau).",
              "Les graphiques et cartes ne sont pas rafraichis automatiquement (a ajouter manuellement)."]
    plain = "\n".join(lines)
    li = "".join("<li>diapo %s : %s <i>(%s)</i></li>" % (x.get("slide"), html.escape(str(x.get("element"))), html.escape(str(x.get("reason")))) for x in ret)
    wi = "".join("<li>%s</li>" % html.escape(str(w)) for w in warns)
    h = ("<p>Deck BDBV mis a jour automatiquement a partir du <b>SitRep %s</b> (date du rapport %s).</p>"
         "<p>Comparaisons jour/jour avec le SitRep %s. Verification independante du fichier enregistre : <b>%s controles reussis, 0 echec</b>.<br>"
         "Reconciliation : cas %s ; deces %s.</p>"
         "<p>Elements <b>non rafraichis</b> par le SitRep (conserves et signales) : %d</p><ul>%s</ul>%s"
         "<p>Pieces jointes : le deck et le fichier de controle (ancien &rarr; nouveau). Graphiques et cartes : non rafraichis automatiquement.</p>") % (
        r.get("sitrep_no"), d, r.get("prev_sitrep"), r.get("checks_passed"), html.escape(str(rec.get("cases"))), html.escape(str(rec.get("deaths"))),
        len(ret), li, ("<p>Avertissements de lecture :</p><ul>%s</ul>" % wi) if warns else "")
    return subj, plain, h


def alert_mail(n, title, detail):
    subj = "[PREIS] Deck BDBV SitRep %s : NON genere - %s" % (n, title)
    plain = ("Le deck du SitRep %s n'a PAS ete genere ni envoye (aucune donnee inventee, aucun fichier partiel).\n\nCause : %s\n\n"
             "Le generateur reessaiera au prochain passage du workflow ; cette alerte n'est envoyee qu'une fois par cause." % (n, detail))
    return subj, plain, "<p>" + html.escape(plain).replace("\n", "<br>") + "</p>"
