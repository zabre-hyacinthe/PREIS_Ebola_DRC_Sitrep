# -*- coding: utf-8 -*-
"""Diapositives « <Province> — Weekly Trends » (tableaux des zones de sante). Reperees par leur TITRE (pas par leur numero) :
l'ordre des diapositives peut changer sans casser la mise a jour. Source : Tableau 2 du SitRep (deja recoupe par province dans
sitrep_parse). Regles : jamais de valeur inventee ; zone du tableau absente du Tableau 2 -> tableau conserve et signale."""
import re
import unicodedata

TITLE_RE = re.compile(r"^\s*(.+?)\s+[—–-]\s+Weekly Trends\s*$")
NOTE_RE = re.compile(r"^\d+\s+\w+\s+table\s+\(SitRep\s+\d+\)")
MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
UNALLOC = "a ventiler"


def _key(s):
    s = unicodedata.normalize("NFKD", s or "")
    return re.sub(r"[^a-z0-9]", "", "".join(c for c in s if not unicodedata.combining(c)).lower())


def find_slides(prs):
    """[(numero de diapo, province, table, forme-note)]"""
    out = []
    for i, sl in enumerate(prs.slides, 1):
        prov = tbl = note = None
        for sh in sl.shapes:
            if sh.has_text_frame:
                m = TITLE_RE.match(sh.text_frame.text.replace("\n", " "))
                if m:
                    prov = m.group(1)
                elif NOTE_RE.match(sh.text_frame.text.strip()):
                    note = sh
            if sh.has_table:
                tbl = sh.table
        if prov and tbl is not None:
            out.append((i, prov, tbl, note))
    return out


def _province_key(S, name):
    for p in S["t2"]["zones"]:
        if _key(p) == _key(name):
            return p
    return None


def _shown(rows):
    return rows[1:-1]


def _set(cell, txt):
    runs = cell.text_frame.paragraphs[0].runs
    if not runs:
        raise ValueError("cellule sans texte de reference : %r" % cell.text)
    old = cell.text
    runs[0].text = txt
    for r in runs[1:]:
        r.text = ""
    return old


def plan(S, prov, rows, rh):
    """calcule (lignes, sous-total, omises, non_allouees) ou leve ValueError (tableau a conserver)."""
    t2 = S.get("t2")
    if not t2 or not t2.get("ok"):
        raise ValueError("Tableau 2 du SitRep non recoupe (sommes) : tableau conserve")
    pk = _province_key(S, prov)
    if not pk:
        raise ValueError("province %r absente du Tableau 2" % prov)
    zs = {z: d for z, d in t2["zones"][pk].items() if _key(z) != _key(UNALLOC)}
    unalloc = next((d.get("deaths") for z, d in t2["zones"][pk].items() if _key(z) == _key(UNALLOC)), None)
    if any(d["cases"] is None or d["deaths"] is None for d in zs.values()):
        raise ValueError("valeurs de zone illisibles")
    prior = [r.cells[2].text for r in _shown(rows)]
    bykey = {_key(z): z for z in zs}
    missing = [n for n in prior if _key(n) not in bykey]
    if missing:
        raise ValueError("zones du tableau absentes du SitRep : %s" % ", ".join(missing))
    n = len(prior)
    pri = {_key(z): i for i, z in enumerate(prior)}
    order = sorted(zs, key=lambda z: (-zs[z]["cases"], pri.get(_key(z), 10 ** 6), z))
    shown, omitted = order[:n], order[n:]
    out = []
    for z in shown:
        d = zs[z]
        cfr = rh(100.0 * d["deaths"] / d["cases"]) if d["cases"] else 0.0
        if d.get("cfr") is not None and abs(cfr - d["cfr"]) > 0.11:
            raise ValueError("CFR %s : calcule %.1f != publie %.1f" % (z, cfr, d["cfr"]))
        out.append((z, d["cases"], d["deaths"], cfr))
    sc, sd = sum(o[1] for o in out), sum(o[2] for o in out)
    return out, (sc, sd, rh(100.0 * sd / sc) if sc else 0.0), [(z, zs[z]["cases"], zs[z]["deaths"]) for z in omitted], unalloc, len(zs)


def note_text(S, prior_note, omitted, unalloc, total_zones, shown_n):
    D = S["report_date"]
    head = "%d %s table (SitRep %d)" % (D.day, MON[D.month - 1], S["sitrep_no"])
    if not omitted:
        zt = "all affected zones"
    elif len(omitted) <= 3:
        zt = "%d of %d zones shown (%s omitted)" % (shown_n, total_zones, ", ".join("%s %d/%d" % o for o in omitted))
    else:
        zt = "top %d of %d zones shown" % (shown_n, total_zones)
    parts = [head, zt]
    if unalloc:
        parts[-1] += "; %d deaths not yet allocated" % unalloc
    parts.append("attack rates ND (no denominators)" if "no denominators" in (prior_note or "") else "attack rates ND")
    parts.append("historical plots retained")
    return " · ".join(parts)


def update(dk, S, rh):
    """met a jour tous les tableaux trouves ; renvoie le nombre de tableaux mis a jour."""
    done = 0
    for sn, prov, tbl, note in find_slides(dk.p):
        rows = list(tbl.rows)
        try:
            lines, sub, omitted, unalloc, tz = plan(S, prov, rows, rh)
            for k, (z, c, d, cfr) in enumerate(lines):
                r = rows[1 + k]
                for col, v in ((2, z), (3, str(c)), (4, str(d)), (5, "%.1f" % cfr)):
                    old = _set(r.cells[col], v)
                    if old != v:
                        dk.log.append((sn, "tableau %s" % prov, "ligne %d col %d" % (k + 1, col), old, v, ""))
            for col, v in ((3, str(sub[0])), (4, str(sub[1])), (5, "%.1f" % sub[2])):
                old = _set(rows[-1].cells[col], v)
                if old != v:
                    dk.log.append((sn, "tableau %s" % prov, "sous-total col %d" % col, old, v, ""))
            if note is not None:
                pr = note.text_frame.paragraphs[0]
                old = "".join(r.text for r in pr.runs)
                pr.runs[0].text = note_text(S, old, omitted, unalloc, tz, len(lines))
                for r in pr.runs[1:]:
                    r.text = ""
                dk.log.append((sn, "note tableau %s" % prov, 0, old, pr.runs[0].text, ""))
            done += 1
        except ValueError as e:
            dk.retained.append((sn, "tableau %s" % prov, "NON mis a jour : %s" % e))
    return done


def verify(prs, S):
    """relecture independante du pptx enregistre : (nb_ok, [echecs])."""
    ok, fails = 0, []
    t2 = (S.get("t2") or {})
    for sn, prov, tbl, note in find_slides(prs):
        pk = _province_key(S, prov)
        rows = list(tbl.rows)
        if not pk or not t2.get("ok"):
            continue
        zs = t2["zones"][pk]
        kz = {_key(z): d for z, d in zs.items()}
        # on ne controle que les tableaux effectivement mis a jour (note de la forme « d Mon table (SitRep N) » avec le bon N)
        if note is None or ("(SitRep %d)" % S["sitrep_no"]) not in note.text_frame.text:
            continue
        sc = sd = 0
        prev = None
        for r in _shown(rows):
            z, c, d, f = (r.cells[i].text for i in (2, 3, 4, 5))
            ref = kz.get(_key(z))
            if not ref or str(ref["cases"]) != c or str(ref["deaths"]) != d:
                fails.append("tableau %s diapo %d : %s %s/%s != Tableau 2 %s" % (prov, sn, z, c, d, ref and (ref["cases"], ref["deaths"])))
                continue
            if prev is not None and int(c) > prev:
                fails.append("tableau %s : ordre non decroissant a %s" % (prov, z))
            prev = int(c)
            sc += int(c)
            sd += int(d)
            ok += 1
        st = rows[-1]
        if (st.cells[3].text, st.cells[4].text) == (str(sc), str(sd)):
            ok += 1
        else:
            fails.append("tableau %s : sous-total %s/%s != somme %d/%d" % (prov, st.cells[3].text, st.cells[4].text, sc, sd))
    return ok, fails
