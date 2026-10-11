# -*- coding: utf-8 -*-
"""
Orchestrateur du deck (appele par le workflow) : dernier SitRep -> build_deck -> envoi -> etat.
Anti-doublon : data/africa_cdc_deck/email_sent_state.csv (un deck par SitRep) ; alertes operateur dedoublonnees
par (SitRep, cause) dans alerts.csv. Ne leve jamais d'exception vers le workflow (code 0 sauf erreur de config).
Usage : python3 scripts/python/deck/run_deck.py [--sitrep N] [--test] [--dry-run] [--resend]
"""
import argparse
import csv
import datetime as dt
import glob
import hashlib
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import send_deck as sd  # noqa: E402

BASE = "data/africa_cdc_deck"
SENT = BASE + "/email_sent_state.csv"
ALERTS = BASE + "/alerts.csv"
TEMPLATE = BASE + "/templates/deck_template.pptx"
STATE = BASE + "/state.json"
OUT = "outputs/deck_private"


def now():
    return dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def read_rows(fp):
    if not os.path.isfile(fp):
        return []
    with open(fp, encoding="utf-8", newline="") as f:
        return list(csv.DictReader(f))


def append_row(fp, header, row):
    new = not os.path.isfile(fp)
    os.makedirs(os.path.dirname(fp), exist_ok=True)
    with open(fp, "a", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=header)
        if new:
            w.writeheader()
        w.writerow({k: row.get(k, "") for k in header})


def latest_sitrep(pdf_dir):
    ns = []
    for fp in glob.glob(os.path.join(pdf_dir, "*.pdf")):
        m = re.search(r"SitRep_0*(\d+)_2026\.pdf$", os.path.basename(fp))
        if m:
            ns.append(int(m.group(1)))
    return max(ns) if ns else None


def operator_alert(n, title, detail, args):
    key = hashlib.sha1(("%s|%s" % (n, detail)).encode("utf-8")).hexdigest()[:12]
    if any(r.get("sitrep_no") == str(n) and r.get("key") == key for r in read_rows(ALERTS)):
        print("[run_deck] alerte deja envoyee pour cette cause (%s)" % key)
        return
    ops, _ = sd.recipients(test=True)
    subj, plain, h = sd.alert_mail(n, title, detail)
    if args.dry_run or not ops:
        print("[run_deck] ALERTE (non envoyee : %s) : %s" % ("dry-run" if args.dry_run else "ALERT_TO absent", subj))
        return
    try:
        sd.smtp_send(sd.build_message(sd.env_first("ALERT_FROM", "EMAIL_FROM", "SMTP_USER"), ops, subj, plain, h))
        append_row(ALERTS, ["sitrep_no", "key", "sent_at_utc", "title"], dict(sitrep_no=n, key=key, sent_at_utc=now(), title=title))
        print("[run_deck] alerte operateur envoyee")
    except Exception as e:  # noqa
        print("[run_deck] echec envoi alerte : %s" % e)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sitrep", type=int)
    ap.add_argument("--pdf-dir", default="data/pdf")
    ap.add_argument("--test", action="store_true", help="envoi a ALERT_TO uniquement, aucun etat modifie")
    ap.add_argument("--dry-run", action="store_true", help="construit mais n'envoie rien (ecrit un .eml)")
    ap.add_argument("--resend", action="store_true", help="renvoie meme si deja envoye")
    ap.add_argument("--narrative", default="auto", choices=["auto", "off"])
    args = ap.parse_args()

    n = args.sitrep or latest_sitrep(args.pdf_dir)
    if not n:
        print("[run_deck] aucun PDF de SitRep dans %s" % args.pdf_dir)
        return 0
    sent = {r.get("sitrep_no") for r in read_rows(SENT)}
    if str(n) in sent and not (args.resend or args.test):
        print("[run_deck] deck du SitRep %d deja envoye : rien a faire" % n)
        return 0
    if not os.path.isfile(TEMPLATE):
        operator_alert(n, "gabarit absent", "gabarit %s introuvable (coffre non restaure ?). Verifier PREIS_ARCHIVE_KEY et data/africa_cdc_deck/private_store.enc." % TEMPLATE, args)
        return 0
    out = OUT + ("_test" if args.test else "")
    cmd = [sys.executable, os.path.join(HERE, "build_deck.py"), "--sitrep", str(n), "--pdf-dir", args.pdf_dir, "--template", TEMPLATE,
           "--state", STATE, "--out-dir", out, "--narrative", args.narrative]
    if not (args.test or args.dry_run):
        cmd.append("--commit-state")
    rc = subprocess.run(cmd).returncode
    rep_fp = os.path.join(out, "deck_report.json")
    rep = json.load(open(rep_fp, encoding="utf-8")) if os.path.isfile(rep_fp) else {"status": "error", "reason": "pas de rapport (code %s)" % rc}
    if rep.get("status") != "ok" or rc != 0:
        why = rep.get("reason") or "; ".join(rep.get("checks_failed") or []) or "code %s" % rc
        operator_alert(n, "refus de securite" if rep.get("status") == "refused" else "echec", why, args)
        return 0
    ops, to = sd.recipients(test=args.test)
    subj, plain, h = sd.deck_mail(rep, test=args.test)
    atts = [os.path.join(out, rep["file"]), os.path.join(out, rep["control_csv"])]
    sender = sd.env_first("ALERT_FROM", "EMAIL_FROM", "SMTP_USER")
    msg = sd.build_message(sender, to, subj, plain, h, atts)
    if args.dry_run or not to:
        fp = os.path.join(out, "deck_mail.eml")
        open(fp, "wb").write(bytes(msg))
        print("[run_deck] %s -> %s" % ("dry-run" if args.dry_run else "aucun destinataire (ALERT_TO)", fp))
        return 0
    try:
        sd.smtp_send(msg)
    except Exception as e:  # noqa
        print("[run_deck] ECHEC ENVOI : %s" % e)
        operator_alert(n, "envoi SMTP en echec", str(e), args)
        return 0
    print("[run_deck] deck SitRep %d envoye a %d destinataire(s)" % (n, len(to)))
    if not args.test:
        append_row(SENT, ["sitrep_no", "issue", "report_date", "sent_at_utc", "to_count", "file", "checks_passed", "retained"],
                   dict(sitrep_no=n, issue=rep.get("issue"), report_date=rep.get("report_date"), sent_at_utc=now(), to_count=len(to),
                        file=rep["file"], checks_passed=rep.get("checks_passed"), retained=len(rep.get("retained") or [])))
    return 0


if __name__ == "__main__":
    sys.exit(main())
