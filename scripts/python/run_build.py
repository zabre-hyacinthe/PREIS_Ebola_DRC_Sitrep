# -*- coding: utf-8 -*-
"""
Pilote en ligne de commande : reponse du modele -> 4 .docx verifies.

Appele depuis scripts/06b_generate_africa_cdc_sitrep_brief_full.R (system2).

Entree : les 2 gabarits .docx (SitRep + Brief) et la reponse BRUTE du modele
(JSON d'edits, eventuellement entoure de ```). Ce script :
  1. extrait et valide strictement le JSON (index, types, tailles) ;
  2. refuse une reponse trop pauvre (un nouveau SitRep change forcement
     beaucoup de chiffres) ;
  3. construit dans un dossier TEMPORAIRE les 4 fichiers (propre + suivi
     des modifications pour le SitRep et pour le Brief) ;
  4. verifie leur integrite (archive, XML, equivalence propre/suivi,
     commentaires) et le numero d'Issue attendu ;
  5. seulement alors copie les 4 fichiers dans --out-dir.
Rien d'incomplet n'atteint donc jamais le dossier des livrables.

Code retour : 0 = succes ; 2 = reponse du modele inexploitable (JSON, index,
reponse trop pauvre, numero d'Issue) ; 1 = erreur de construction/verification.
Sortie (derniere ligne de stdout) : un objet JSON a une ligne :
  {"paths": {...4 chemins...}, "n_sitrep_edits": n, "n_brief_edits": n,
   "anomalies": [...], "summary_fr": "...", "warnings": [...]}

Usage :
  python3 run_build.py --sitrep-template S.docx --brief-template B.docx \
     --response-text reponse.txt --out-dir outputs/rapports \
     --issue-label 132_03_October2026 --date-iso 2026-10-03T08:00:00Z \
     --expected-issue-no 132 --prev-issue-no 131 \
     [--min-sitrep-edits 10] [--min-brief-edits 5]
"""
import argparse
import contextlib
import io
import json
import os
import re
import shutil
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import africa_cdc_build_docx as bd
import concordance


def fail(code, msg):
    print("ERREUR run_build: " + msg, file=sys.stderr)
    sys.exit(code)


def parse_model_json(text):
    """Tolerant sur l'emballage (```json ... ```, texte parasite), strict sur le contenu."""
    t = text.strip().lstrip("﻿")
    t = re.sub(r"^```[a-zA-Z]*\s*", "", t)
    t = re.sub(r"\s*```$", "", t).strip()
    a, b = t.find("{"), t.rfind("}")
    if a < 0 or b <= a:
        raise ValueError("aucun objet JSON dans la reponse (debut : %r)" % t[:200])
    try:
        obj = json.loads(t[a:b + 1])
    except ValueError as e:
        raise ValueError("JSON invalide (%s) ; debut : %r" % (e, t[:200]))
    if not isinstance(obj, dict):
        raise ValueError("objet JSON attendu a la racine")
    return obj


def changed_count(template_dir, edits):
    """Nombre d'edits qui changent REELLEMENT le texte d'un run (hors simples commentaires)."""
    with open(os.path.join(template_dir, "word", "document.xml"), encoding="utf-8") as f:
        runs = bd.get_runs(f.read())
    n = 0
    for idx, spec in edits.items():
        if spec["new"] is None:
            continue
        _, _, _, old = bd.split_run(runs[idx].group(0))
        old = (old.replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">")
                  .replace("&quot;", '"').replace("&apos;", "'"))
        if old != spec["new"]:
            n += 1
    return n


def _plain(x):
    return (x.replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">")
             .replace("&quot;", '"').replace("&apos;", "'"))


def ensure_issue_number(template_dir, edits, prev, expected):
    """Garantit de facon DETERMINISTE le passage du numero d'Issue (ex. 131 -> 132).
    Si, apres les edits du modele, un paragraphe affiche encore 'Issue No. <prev>', le chiffre
    est remplace dans le run qui le contient (y compris si le nombre est coupe sur plusieurs runs).
    Un numero deja change par le modele (meme faux) n'est jamais touche : il sera verifie ensuite.
    Retourne la liste des corrections appliquees (pour information)."""
    with open(os.path.join(template_dir, "word", "document.xml"), encoding="utf-8") as f:
        xml = f.read()
    runs = bd.get_runs(xml)
    paras = [(m.start(), m.end()) for m in re.finditer(r"<w:p\b[^>]*>.*?</w:p>", xml, re.S)]
    per_para = {}
    for i, r in enumerate(runs):
        for pi, (a, b) in enumerate(paras):
            if a <= r.start() < b:
                per_para.setdefault(pi, []).append(i)
                break

    def cur(i):
        sp = edits.get(i)
        if sp and sp["new"] is not None:
            return sp["new"]
        return _plain(bd.split_run(runs[i].group(0))[3])

    pat = re.compile(r"Issue\s*No\.?\s*(\d+)", re.I)
    fixed = []
    for pi, idxs in per_para.items():
        texts = [cur(i) for i in idxs]
        joined = "".join(texts)
        nums = {int(m.group(1)) for m in pat.finditer(joined)}
        if prev not in nums or expected in nums:
            continue
        m = next(x for x in pat.finditer(joined) if int(x.group(1)) == prev)
        start, end = m.start(1), m.end(1)
        pos = 0
        for i, t in zip(idxs, texts):
            lo, hi = pos, pos + len(t)
            pos = hi
            if hi <= start or lo >= end:
                continue
            if lo <= start:      # run qui contient le debut du numero : on y met tout le nouveau numero
                new = t[:start - lo] + str(expected) + (t[end - lo:] if end <= hi else "")
            else:                # run suivant couvert par l'ancien numero (ex. '1' | '31') : on retire ces chiffres
                new = t[end - lo:] if end < hi else ""
            old = edits.get(i) or {"new": None, "comment": None}
            com = old["comment"] or ("Numero d'Issue mis a jour automatiquement (%d -> %d)." % (prev, expected))
            edits[i] = {"new": new, "comment": com}
            fixed.append("run %d : Issue No. %d -> %d" % (i, prev, expected))
    return fixed


BRIEF_MAX_CHARS_WARN = int(os.environ.get("PREIS_BRIEF_MAX_CHARS", "10900"))
BRIEF_MAX_CHARS_FAIL = int(os.environ.get("PREIS_BRIEF_MAX_CHARS_FAIL", "11800"))
CREDIT = "Dr. R. Hyacinthe ZABRE"      # mention definitive de la ligne « Prepared by » (demande du proprietaire du projet)
CREDIT_ANCHOR = "Merawi Aragaw"        # le nom est insere juste apres celui-ci


def ensure_credit(template_dir, edits):
    """Garantit que la ligne « Prepared by » cite CREDIT juste apres CREDIT_ANCHOR. Le gabarit la contient
    deja ; ceci ne sert que si le modele reecrit la ligne et l'omet. Retourne le nombre de corrections."""
    with open(os.path.join(template_dir, "word", "document.xml"), encoding="utf-8") as f:
        xml = f.read()
    runs = bd.get_runs(xml)
    texts = []
    for i, r in enumerate(runs):
        sp = edits.get(i)
        texts.append(sp["new"] if sp and sp["new"] is not None else _plain(bd.split_run(r.group(0))[3]))
    if any("ZABRE" in t for t in texts):
        return 0
    for i, t in enumerate(texts):
        if CREDIT_ANCHOR in t:
            old = edits.get(i) or {"new": None, "comment": None}
            edits[i] = {"new": t.replace(CREDIT_ANCHOR, CREDIT_ANCHOR + ", " + CREDIT, 1),
                        "comment": old["comment"] or "Mention « %s » (definitive) remise automatiquement." % CREDIT}
            return 1
    return 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sitrep-template", required=True)
    ap.add_argument("--brief-template", required=True)
    ap.add_argument("--response-text", help="reponse brute du modele (fichier texte)")
    ap.add_argument("--edits-json", help="alternative : JSON d'edits deja propre")
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--issue-label", required=True, help='ex. "132_03_October2026"')
    ap.add_argument("--date-iso", required=True)
    ap.add_argument("--expected-issue-no", type=int, default=None)
    ap.add_argument("--prev-issue-no", type=int, default=None)
    ap.add_argument("--min-sitrep-edits", type=int, default=10)
    ap.add_argument("--min-brief-edits", type=int, default=5)
    args = ap.parse_args()

    src = args.response_text or args.edits_json
    if not src:
        fail(1, "--response-text ou --edits-json requis")
    with open(src, encoding="utf-8") as f:
        raw = f.read()
    try:
        payload = parse_model_json(raw)
    except ValueError as e:
        fail(2, str(e))
    for k in ("sitrep_edits", "brief_edits"):
        if k not in payload:
            fail(2, "cle obligatoire absente de la reponse : %s" % k)

    work = tempfile.mkdtemp(prefix="africa_cdc_build_")
    stage = os.path.join(work, "stage")
    os.makedirs(stage)
    try:
        sitrep_dir = bd.unzip_docx(args.sitrep_template, os.path.join(work, "sitrep_src"))
        brief_dir = bd.unzip_docx(args.brief_template, os.path.join(work, "brief_src"))
        s_edits, e1 = bd.validate_edits(payload["sitrep_edits"], bd.count_runs(sitrep_dir), "sitrep_edits")
        b_edits, e2 = bd.validate_edits(payload["brief_edits"], bd.count_runs(brief_dir), "brief_edits")
        errors = e1 + e2
        if errors:
            fail(2, "%d erreur(s) de validation, ex. :\n  - %s" % (len(errors), "\n  - ".join(errors[:10])))

        warnings = []
        if args.expected_issue_no is not None and args.prev_issue_no is not None \
                and args.expected_issue_no != args.prev_issue_no:
            for nm, d, ed in (("SitRep", sitrep_dir, s_edits), ("Brief", brief_dir, b_edits)):
                for note in ensure_issue_number(d, ed, args.prev_issue_no, args.expected_issue_no):
                    warnings.append("%s : numero d'Issue corrige automatiquement (%s)" % (nm, note))
        for nm, d, ed in (("SitRep", sitrep_dir, s_edits), ("Brief", brief_dir, b_edits)):
            if ensure_credit(d, ed):
                warnings.append("%s : mention « %s » remise automatiquement" % (nm, CREDIT))
        n_s, n_b = changed_count(sitrep_dir, s_edits), changed_count(brief_dir, b_edits)
        if n_s < args.min_sitrep_edits or n_b < args.min_brief_edits:
            fail(2, "reponse trop pauvre : %d run(s) SitRep modifie(s) (minimum %d), %d run(s) Brief "
                    "(minimum %d). Un nouveau SitRep change beaucoup plus de chiffres : reponse "
                    "refusee plutot que d'envoyer un document quasi inchange."
                 % (n_s, args.min_sitrep_edits, n_b, args.min_brief_edits))

        lab = args.issue_label
        names = {
            "sitrep_propre": "BVD_SitRep_%s.docx" % lab,
            "sitrep_suivi": "BVD_SitRep_%s_suivi_modifications.docx" % lab,
            "brief_propre": "BVD_Executive_Brief_%s.docx" % lab,
            "brief_suivi": "BVD_Executive_Brief_%s_suivi_modifications.docx" % lab,
        }
        p = {k: os.path.join(stage, v) for k, v in names.items()}
        c_s = os.path.join(stage, "_comments_sitrep_%s.txt" % lab)
        c_b = os.path.join(stage, "_comments_brief_%s.txt" % lab)
        try:
            with contextlib.redirect_stdout(io.StringIO()):
                bd.build(sitrep_dir, s_edits, p["sitrep_propre"], p["sitrep_suivi"], c_s, args.date_iso)
                bd.build(brief_dir, b_edits, p["brief_propre"], p["brief_suivi"], c_b, args.date_iso)
        except Exception as e:
            fail(1, "construction impossible (gabarit structurellement incompatible ?) : %s: %s"
                 % (type(e).__name__, e))
        for d in (p["sitrep_propre"], p["sitrep_suivi"], p["brief_propre"], p["brief_suivi"]):
            shutil.rmtree(d + "_src", ignore_errors=True)

        try:
            s_txt = bd.verify_pair(p["sitrep_propre"], p["sitrep_suivi"])
            b_txt = bd.verify_pair(p["brief_propre"], p["brief_suivi"])
        except Exception as e:
            fail(1, "verification d'integrite echouee : %s" % e)

        for nm, txt in (("SitRep", s_txt), ("Brief", b_txt)):
            if not any("ZABRE" in t for t in txt):
                fail(2, "%s : la mention « %s » (definitive) est absente du document genere" % (nm, CREDIT))
        try:
            c_err, c_warn = concordance.check(p["sitrep_propre"], p["brief_propre"])
        except Exception as e:  # le controle ne doit jamais masquer un livrable valide : avertissement
            c_err, c_warn = [], ["concordance : controle non execute (%s: %s)" % (type(e).__name__, e)]
        warnings.extend(c_warn)
        if c_err:
            fail(2, "concordance SitRep/Brief : " + " ; ".join(c_err[:12]))
        # Executive Brief = 2 pages. Mesure : nombre total de caracteres de texte du document.
        # References (rendu verifie) : 10 609 = 2 pages avec marge ; 10 896 = limite ; 12 337 = 3 pages.
        brief_chars = sum(len(t) for t in b_txt)
        if brief_chars > BRIEF_MAX_CHARS_FAIL:
            fail(2, "Executive Brief trop long : %d caracteres (limite 2 pages ~ %d, refus au-dela de %d). "
                    "Raccourcir les paragraphes narratifs sans rien inventer." %
                 (brief_chars, BRIEF_MAX_CHARS_WARN, BRIEF_MAX_CHARS_FAIL))
        if brief_chars > BRIEF_MAX_CHARS_WARN:
            warnings.append("Brief : %d caracteres (limite 2 pages ~ %d) : le document risque de depasser 2 pages, "
                            "a verifier avant diffusion" % (brief_chars, BRIEF_MAX_CHARS_WARN))
        if args.expected_issue_no is not None:
            for nm, txt in (("SitRep", s_txt), ("Brief", b_txt)):
                nums = bd.issue_numbers(txt)
                if args.expected_issue_no not in nums:
                    fail(2, "%s : le numero d'Issue attendu (No. %d) est introuvable dans le document "
                            "genere (numeros presents : %s)" % (nm, args.expected_issue_no, sorted(nums) or "aucun"))
                if args.prev_issue_no is not None and args.prev_issue_no in nums \
                        and args.prev_issue_no != args.expected_issue_no:
                    warnings.append("%s : l'ancien numero d'Issue (No. %d) apparait encore a cote du nouveau"
                                    % (nm, args.prev_issue_no))

        os.makedirs(args.out_dir, exist_ok=True)
        # Seules les versions finales (sans suivi des modifications) sont livrees ; la version suivi
        # ne sert qu'au controle d'integrite (verify_pair) et reste dans le dossier temporaire.
        final = {}
        for k in ("sitrep_propre", "brief_propre"):
            final[k] = os.path.join(args.out_dir, os.path.basename(p[k]))
            shutil.copy2(p[k], final[k])
        for cpath in (c_s, c_b):
            if os.path.exists(cpath):
                shutil.copy2(cpath, os.path.join(args.out_dir, os.path.basename(cpath)))
    finally:
        shutil.rmtree(work, ignore_errors=True)

    anomalies = payload.get("anomalies") or []
    if not isinstance(anomalies, list):
        anomalies = [anomalies]
    anomalies = [bd.clean_text(str(a))[:600] for a in anomalies if isinstance(a, (str, int, float))][:50]
    summary = payload.get("summary_fr")
    summary = bd.clean_text(summary)[:2000] if isinstance(summary, str) else ""

    print(json.dumps({"paths": final, "n_sitrep_edits": n_s, "n_brief_edits": n_b,
                      "anomalies": anomalies, "summary_fr": summary, "warnings": warnings},
                     ensure_ascii=False))


if __name__ == "__main__":
    main()
