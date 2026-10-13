# -*- coding: utf-8 -*-
"""
Regles de maison du SitRep / Executive Brief Africa CDC (demandes du Dr Merawi Aragaw, 12-13/10/2026).

Trois regles DETERMINISTES, appliquees apres le modele (jamais laissees a son seul jugement) :

  1. AGENTS DE SANTE (HCW) : les deux tuiles « HCW infected » / « HCW deaths » reprennent les valeurs du
     weekly briefing d'Africa CDC (data/africa_cdc_brief/external_values.json : 270 infectes / 50 deces).
     Ces valeurs ne figurent pas dans le SitRep RDC : elles sont imposees, signalees en commentaire.
  2. TITRES COLLES AU TEXTE : « Titre.Texte » / « Titre. Texte » / « Titre:Texte »  ->  « Titre: Texte »
     (deux-points + espace). SitRep : titres en gras suivis du texte dans le meme paragraphe.
     Brief : paragraphes de la section « Response operations » dont le titre est dans le meme run.
  3. NOTE « ACTIVE HZs » (Brief) : la note sous le tableau des provinces est ecrite de facon deterministe
     (texte : external_values.json, cle active_hz.note) ; elle ne cite plus le SitRep RDC comme source d'une
     definition qu'il ne contient pas et signale la valeur de 46 zones / 21 jours indiquee par le Dr Merawi.
  4. SECTION KENYA (SitRep) : un titre « Kenya update » + un court paragraphe, inseres avant
     « Response highlights » et alimentes par external_values.json (jamais par le modele : ses edits
     sur ces runs sont ignores). Une date de mise a jour trop ancienne est signalee.

Chaque fonction travaille sur le dossier d'un .docx dezippe (word/document.xml) et sur le dict d'edits
{idx_run: {"new":..., "comment":...}} de africa_cdc_build_docx.build : elle ne touche jamais la mise en forme
et retourne la liste de ses interventions (pour information). Une structure non reconnue n'est jamais
une erreur : l'intervention est sautee et signalee.

CLI (appelee par 06b avant l'extraction des runs) :
  python3 house_rules.py preflight --sitrep IN.docx --values external_values.json --out OUT.docx
  -> derniere ligne de stdout : {"changed": bool, "texts": [...], "chars": n, "warnings": [...]}
"""
import argparse
import bisect
import datetime
import json
import os
import re
import sys
import tempfile
import shutil

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import africa_cdc_build_docx as bd

_P_RE = re.compile(r"<w:p\b[^>]*?(?:/>|>.*?</w:p>)", re.S)
_BOLD_RE = re.compile(r"<w:b(?:\s+[^>]*)?/>")
_BOLD_OFF_RE = re.compile(r'<w:b\s+w:val="(?:0|false|off)"')
KENYA_HEADING = "Kenya update"
KENYA_LEAD = "First imported case: "
ANCHOR_HEADING = "Response highlights"
STALE_DAYS = 7


# ------------------------------------------------------------------ utilitaires
def _plain(x):
    return (x.replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">")
             .replace("&quot;", '"').replace("&apos;", "'"))


def load_values(path):
    """Lit external_values.json ; fichier absent ou illisible -> {} (jamais d'erreur)."""
    if not path or not os.path.exists(path):
        return {}
    try:
        with open(path, encoding="utf-8") as f:
            d = json.load(f)
        return d if isinstance(d, dict) else {}
    except Exception:
        return {}


def _read(template_dir):
    with open(os.path.join(template_dir, "word", "document.xml"), encoding="utf-8") as f:
        return f.read()


def _is_bold(run_xml):
    m = re.search(r"<w:rPr>(.*?)</w:rPr>", run_xml, re.S)
    if not m:
        return False
    rpr = m.group(1)
    return bool(_BOLD_RE.search(rpr)) and not _BOLD_OFF_RE.search(rpr)


class Doc(object):
    """Runs, paragraphes et texte COURANT (edits du modele inclus) d'un gabarit."""

    def __init__(self, template_dir, edits):
        self.xml = _read(template_dir)
        self.runs = bd.get_runs(self.xml)
        self.edits = edits
        self.paras = list(_P_RE.finditer(self.xml))
        starts = [m.start() for m in self.paras]
        self.para_runs = [[] for _ in self.paras]
        for i, r in enumerate(self.runs):
            k = bisect.bisect_right(starts, r.start()) - 1
            if k >= 0 and r.start() < self.paras[k].end():
                self.para_runs[k].append(i)
        self.bold = [_is_bold(r.group(0)) for r in self.runs]

    def text(self, i):
        sp = self.edits.get(i)
        if sp and sp.get("new") is not None:
            return sp["new"]
        return _plain(bd.split_run(self.runs[i].group(0))[3])

    def orig(self, i):
        return _plain(bd.split_run(self.runs[i].group(0))[3])

    def ptext(self, k):
        return "".join(self.text(i) for i in self.para_runs[k])

    def optext(self, k):
        """Texte du paragraphe dans le GABARIT (sans les edits du modele)."""
        return "".join(self.orig(i) for i in self.para_runs[k])

    def set(self, i, new, comment=None):
        old = self.edits.get(i) or {"new": None, "comment": None}
        self.edits[i] = {"new": new, "comment": comment if comment else old.get("comment")}


# ------------------------------------------------------------------ 1. HCW
def _fmt(n):
    return "{:,}".format(int(n))


def enforce_hcw(template_dir, edits, values):
    """Tuiles « HCW infected » et « HCW deaths » = valeurs imposees. Retourne la liste des interventions."""
    h = (values or {}).get("hcw")
    notes = []
    if not isinstance(h, dict):
        return notes
    try:
        spec = {
            "HCW infected": (_fmt(h["infected_total"]),
                             "DRC %s · Uganda %s" % (_fmt(h["infected_drc"]), _fmt(h["infected_uganda"]))),
            "HCW deaths": (_fmt(h["deaths_total"]),
                           "DRC %s · Uganda %s" % (_fmt(h["deaths_drc"]), _fmt(h["deaths_uganda"]))),
        }
    except (KeyError, TypeError, ValueError):
        return ["HCW : valeurs imposees incompletes dans external_values.json (rien applique)"]
    src = h.get("source") or "Africa CDC weekly briefing"
    d = Doc(template_dir, edits)
    for label, (num, detail) in spec.items():
        ks = [k for k in range(len(d.paras)) if d.ptext(k).strip() == label]
        if not ks:
            continue                      # ce document n'a pas cette tuile (ex. « HCW deaths » absente du Brief)
        for k in ks:
            if k < 1 or k + 1 >= len(d.paras):
                notes.append("%s : structure de tuile non reconnue (bord de document), valeur non imposee" % label)
                continue
            pn, pd_ = k - 1, k + 1
            rn, rd = d.para_runs[pn], d.para_runs[pd_]
            # structure verifiee sur le TEXTE D'ORIGINE du gabarit (les edits du modele ne la redefinissent pas)
            if len(rn) != 1 or len(rd) != 1 or not re.match(r"^\s*[\d,]+\s*$", d.orig(rn[0])) \
                    or not d.orig(rd[0]).strip().startswith("DRC"):
                notes.append("%s : structure de tuile non reconnue, valeur non imposee" % label)
                continue
            for run_i, new, what in ((rn[0], num, "total"), (rd[0], detail, "detail")):
                cur = d.text(run_i)
                if cur.strip() == new:
                    continue
                com = None
                if d.orig(run_i).strip() != new:
                    com = ("VALEUR IMPOSEE — %s : %s %s ; non presente dans le SitRep RDC. %s"
                           % (src, label, num,
                              "Detail RDC = total − Ouganda (VALEUR CALCULEE)." if what == "detail" else
                              "Valeur du cycle precedent remplacee."))
                d.set(run_i, new, com)
                notes.append("%s (%s) : %r -> %r" % (label, what, cur.strip(), new))
    return notes


# ------------------------------------------------------------------ 2. titres
def _fix_bold_lead(d, k):
    """Paragraphe dont le debut est un titre en gras suivi du texte : titre -> « Titre: ». Retourne (run, new) ou None."""
    idx = d.para_runs[k]
    if len(idx) < 2 or not d.bold[idx[0]]:
        return None
    j = 0
    while j < len(idx) and d.bold[idx[j]] and d.text(idx[j]).strip() != "":
        j += 1
    if j == 0 or j >= len(idx):
        return None                                       # tout en gras (intitule seul) ou pas de texte apres
    lead = "".join(d.text(i) for i in idx[:j]).strip()
    rest = "".join(d.text(i) for i in idx[j:])
    if (not lead or len(lead) > 110 or re.search(r"\d", lead) or not lead[0].isalpha() or not lead[0].isupper()
            or len(rest.strip()) < 40):
        return None
    last = idx[j - 1]
    cur = d.text(last)
    core = cur.rstrip()
    core = core.rstrip(".,;:–—-").rstrip()
    new = core + ":" + ("" if rest[:1].isspace() else " ")
    # si le texte du run en gras se terminait deja par ': ' ou ':' + texte commencant par un espace : rien a faire
    return (last, new) if new != cur else None


def _fix_plain_lead(d, k):
    """Titre dans le meme run que le texte (Brief) : « Titre. Texte » -> « Titre: Texte »."""
    idx = d.para_runs[k]
    if not idx or d.bold[idx[0]]:
        return None
    cur = d.text(idx[0])
    m = re.match(r"^([A-Z][^.:]{2,90})\.\s+(?=[A-Z0-9])", cur)
    if not m:
        return None
    return idx[0], m.group(1) + ": " + cur[m.end():]


def enforce_title_colons(template_dir, edits):
    notes = []
    d = Doc(template_dir, edits)
    plain_zone = False
    for k in range(len(d.paras)):
        t = d.ptext(k).strip()
        if not t:
            continue
        if t.startswith("Response operations"):
            plain_zone = True
            continue
        if t.startswith("Risks to watch"):
            plain_zone = False
        fix = _fix_bold_lead(d, k)
        if fix is None and plain_zone:
            fix = _fix_plain_lead(d, k)
        if fix:
            run_i, new = fix
            d.set(run_i, new, None)
            notes.append("titre : %r -> %r" % (d.orig(run_i)[:60], new[:60]))
    return notes


# ------------------------------------------------------------------ 3. note Active HZs
_HZ_RE = re.compile(r"(\d+/\d+)\s+historically affected health zones?\s*\(SitRep No\.\s*(\d+)\)")


def enforce_active_hz_note(template_dir, edits, values):
    """Note sous le tableau des provinces (Brief) : texte impose, avec le « HZ historiques » et le n° de SitRep RDC
    repris du texte COURANT. Structure non reconnue -> rien d'ecrit a l'aveugle, signale."""
    a = (values or {}).get("active_hz")
    notes = []
    if not isinstance(a, dict) or not a.get("note"):
        return notes
    d = Doc(template_dir, edits)
    for k in range(len(d.paras)):
        if not d.optext(k).strip().startswith("*Active HZs"):
            continue
        idx = d.para_runs[k]
        if len(idx) != 1:
            notes.append("note Active HZs : structure non reconnue (plusieurs runs), texte non impose")
            continue
        m = _HZ_RE.search(d.text(idx[0])) or _HZ_RE.search(d.orig(idx[0]))
        if not m:
            notes.append("note Active HZs : valeurs « HZ historiques / n\u00b0 SitRep » introuvables, texte non impose")
            continue
        new = str(a["note"]).replace("{hz}", m.group(1)).replace("{n}", m.group(2))
        cur = d.text(idx[0])
        if cur != new:
            com = None
            if d.orig(idx[0]) != new:
                com = ("NOTE IMPOSEE \u2014 « Active HZs » : aucune base dans le SitRep RDC ; le Dr Merawi (12/10/2026) indique "
                       "46 zones actives sur 21 jours selon l'analyse actuelle d'Africa CDC. A reconcilier avec le tableau par province.")
            d.set(idx[0], new, com)
            notes.append("note Active HZs mise a jour")
    return notes


# ------------------------------------------------------------------ 4. Kenya
def kenya_texts(values):
    k = (values or {}).get("kenya")
    if not isinstance(k, dict) or not k.get("body"):
        return None
    return [KENYA_HEADING, KENYA_LEAD, str(k["body"]).strip()]


def _find_kenya(d):
    """Indices de paragraphe (titre, corps) de la section Kenya, ou None."""
    for k in range(len(d.paras) - 1):
        if d.optext(k).strip() == KENYA_HEADING:
            return k, k + 1
    return None


def _kenya_paragraphs(d):
    """Paragraphes d'une section Kenya (inseree par le script OU ajoutee a la main dans le gabarit) :
    - paragraphe dont le 1er run est en gras et commence par « Kenya » (titre seul -> on y ajoute les
      paragraphes suivants jusqu'au prochain titre en gras seul) ou « Kenya: texte » (paragraphe unique)."""
    out = []
    n = len(d.paras)
    k = 0
    while k < n:
        idx = d.para_runs[k]
        if idx and d.bold[idx[0]] and re.match(r"^\s*Kenya\b", d.orig(idx[0])):
            out.append(k)
            if len(idx) == 1 and len(d.orig(idx[0]).strip()) <= 60:      # titre seul : on verrouille son contenu
                m = k + 1
                while m < n:
                    jdx = d.para_runs[m]
                    if len(jdx) == 1 and d.bold[jdx[0]] and d.orig(jdx[0]).strip():
                        break                                             # prochain titre
                    out.append(m)
                    m += 1
                k = m
                continue
        k += 1
    return out


def lock_kenya(template_dir, edits):
    """Supprime les edits du modele sur la section Kenya (texte fourni par Africa CDC / external_values.json)."""
    d = Doc(template_dir, edits)
    n = 0
    for k in _kenya_paragraphs(d):
        for i in d.para_runs[k]:
            if i in edits:
                del edits[i]
                n += 1
    return n


def _set_run_texts(par_xml, texts):
    """Remplace le texte des 1ers runs <w:t> du paragraphe par `texts` ; supprime les runs en trop."""
    runs = bd.get_runs(par_xml)
    if len(runs) < len(texts):
        raise ValueError("paragraphe modele avec moins de %d runs" % len(texts))
    out, last = [], 0
    for n, r in enumerate(runs):
        out.append(par_xml[last:r.start()])
        last = r.end()
        if n < len(texts):
            ro, rpr, _ta, _old = bd.split_run(r.group(0))
            out.append(bd.make_run(ro, rpr, texts[n]))
    out.append(par_xml[last:])
    return "".join(out)


def _strip_ids(par_xml):
    """Un paragraphe clone ne doit pas dupliquer les identifiants Word (paraId/textId)."""
    return re.sub(r'\s+w14:(?:paraId|textId)="[^"]*"', "", par_xml, count=2)


def ensure_kenya_section(template_dir, values):
    """Insere (ou met a jour) la section Kenya dans le SitRep dezippe. Retourne (modifie, textes, avertissements)."""
    texts = kenya_texts(values)
    if not texts:
        return False, [], []
    fp = os.path.join(template_dir, "word", "document.xml")
    xml = _read(template_dir)
    paras = list(_P_RE.finditer(xml))
    ptxt = lambda m: _plain("".join(re.findall(r"<w:t[^>]*>([^<]*)</w:t>", m.group(0)))).strip()
    ktxt = [ptxt(m) for m in paras]
    if KENYA_HEADING in ktxt:
        k = ktxt.index(KENYA_HEADING)
        body = paras[k + 1]
        try:
            new_body = _set_run_texts(body.group(0), [texts[1], texts[2]])
        except ValueError as e:
            return False, texts, ["Kenya : %s" % e]
        if new_body == body.group(0):
            return False, texts, []
        xml = xml[:body.start()] + new_body + xml[body.end():]
        with open(fp, "w", encoding="utf-8") as f:
            f.write(xml)
        return True, texts, []
    if ANCHOR_HEADING not in ktxt:
        return False, texts, ["Kenya : titre « %s » introuvable dans le gabarit SitRep, section non inseree" % ANCHOR_HEADING]
    h = ktxt.index(ANCHOR_HEADING)
    # paragraphe modele du corps : 1er paragraphe apres le titre avec un run en gras suivi d'un run de texte
    body_src = None
    for k in range(h + 1, min(h + 6, len(paras))):
        rr = bd.get_runs(paras[k].group(0))
        if len(rr) >= 2 and _is_bold(rr[0].group(0)) and not _is_bold(rr[1].group(0)):
            body_src = paras[k]
            break
    if body_src is None:
        return False, texts, ["Kenya : paragraphe modele introuvable apres « %s », section non inseree" % ANCHOR_HEADING]
    try:
        head_xml = _set_run_texts(_strip_ids(paras[h].group(0)), [texts[0]])
        body_xml = _set_run_texts(_strip_ids(body_src.group(0)), [texts[1], texts[2]])
    except ValueError as e:
        return False, texts, ["Kenya : %s" % e]
    spacer = ""
    if h >= 1 and not ktxt[h - 1] and "<w:t" not in paras[h - 1].group(0):
        spacer = _strip_ids(paras[h - 1].group(0))        # meme espacement entre sections que le gabarit
    xml = xml[:paras[h].start()] + head_xml + body_xml + spacer + xml[paras[h].start():]
    with open(fp, "w", encoding="utf-8") as f:
        f.write(xml)
    return True, texts, []


def kenya_stale_warning(values, today=None):
    k = (values or {}).get("kenya")
    if not isinstance(k, dict) or not k.get("as_of"):
        return None
    try:
        d0 = datetime.date.fromisoformat(str(k["as_of"])[:10])
    except ValueError:
        return "Kenya : date « as_of » illisible dans external_values.json"
    today = today or datetime.datetime.utcnow().date()
    age = (today - d0).days
    if age > STALE_DAYS:
        return ("Kenya : la section Kenya date du %s (%d jours) ; a actualiser dans "
                "data/africa_cdc_brief/external_values.json (texte « kenya.body » + « as_of »)." % (k["as_of"], age))
    return None


# ------------------------------------------------------------------ CLI
def _cli_preflight(args):
    values = load_values(args.values)
    work = tempfile.mkdtemp(prefix="hr_preflight_")
    try:
        d = bd.unzip_docx(args.sitrep, os.path.join(work, "src"))
        changed, texts, warns = ensure_kenya_section(d, values)
        chars = 0
        if texts:
            chars = sum(len(t) for t in (texts[0], texts[1] + texts[2]))
        os.makedirs(os.path.dirname(os.path.abspath(args.out)), exist_ok=True)
        if changed:
            bd.zip_docx(d, args.out)
        else:
            shutil.copy2(args.sitrep, args.out)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    print(json.dumps({"changed": changed, "texts": texts, "chars": chars, "warnings": warns}, ensure_ascii=False))


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("preflight")
    p.add_argument("--sitrep", required=True)
    p.add_argument("--values", required=True)
    p.add_argument("--out", required=True)
    args = ap.parse_args()
    if args.cmd == "preflight":
        _cli_preflight(args)


if __name__ == "__main__":
    main()
