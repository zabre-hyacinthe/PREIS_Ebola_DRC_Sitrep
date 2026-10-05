# -*- coding: utf-8 -*-
"""
Controle de CONCORDANCE entre le SitRep Africa CDC et l'Executive Brief d'un meme cycle.

Les deux documents derivent du meme SitRep RDC : tout chiffre commun doit etre identique.
  ERREURS (bloquantes)  : tuiles d'indicateurs de meme libelle avec valeur ou detail differents ;
                          lignes de province de meme nom avec cas / deces / letalite differents ;
                          totaux RDC (somme des provinces) differents d'un document a l'autre.
  AVERTISSEMENTS        : arithmetique interne (somme des provinces = total, deces/cas = letalite,
                          RDC + Ouganda = continental). Un avertissement peut venir de la SOURCE
                          elle-meme (ex. total de contacts) : il est signale, jamais corrige.
Ne depend que de la bibliotheque standard. Aucune structure reconnue => aucun controle (jamais d'erreur
inventee) ; c'est signale en avertissement.
"""
import re
import zipfile
import xml.etree.ElementTree as ET

W = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
_BIG = re.compile(r"^\s*([\d][\d,]*(?:\.\d+)?%?)(?:\s+[A-Za-z]+)?\s*$")


def _p_text(p):
    return "".join(t.text or "" for t in p.iter(W + "t")).strip()


def read_tables(docx_path):
    with zipfile.ZipFile(docx_path) as z:
        root = ET.fromstring(z.read("word/document.xml"))
    tables = []
    for tbl in root.iter(W + "tbl"):
        rows = []
        for tr in tbl.findall(W + "tr"):
            row = []
            for tc in tr.findall(W + "tc"):
                row.append([t for t in (_p_text(p) for p in tc.findall(W + "p")) if t])
            rows.append(row)
        tables.append(rows)
    return tables


def _num(s):
    """'6,370' -> 6370.0 ; '46.0%' -> 46.0 ; '2,933 (46.0%)' -> 2933.0 ; sinon None."""
    if s is None:
        return None
    m = re.match(r"^\s*([\d][\d,]*(?:\.\d+)?)", s)
    return float(m.group(1).replace(",", "")) if m else None


def _pct_in_paren(s):
    m = re.search(r"\(([\d.]+)%\)", s or "")
    return float(m.group(1)) if m else None


def tiles(tables):
    out = {}
    for rows in tables:
        for row in rows:
            for cell in row:
                if len(cell) >= 2 and _BIG.match(cell[0]):
                    label = re.sub(r"\s+", " ", cell[1]).strip().lower()
                    out[label] = (_BIG.match(cell[0]).group(1), " ".join(cell[2:]).strip())
    return out


def _prov_key(s):
    s = re.sub(r"^(drc|uganda)\s*[·\-]\s*", "", s.strip(), flags=re.I)
    return re.sub(r"[\s\-_]+", "", s).lower()


def province_table(tables):
    """-> dict {cle: {'name','new_cases','new_deaths','cases','deaths','cfr','uganda'}} + total_row ou ({}, None)."""
    for rows in tables:
        if not rows or not rows[0]:
            continue
        head = [" ".join(c).strip().lower() for c in rows[0]]
        if "new cases" not in head:
            continue
        col = {}
        for i, h in enumerate(head):
            if h == "new cases":
                col["new_cases"] = i
            elif h == "new deaths":
                col["new_deaths"] = i
            elif h in ("cases", "cumulative cases"):
                col["cases"] = i
            elif h.startswith("deaths"):
                col["deaths"] = i
            elif h == "cfr":
                col["cfr"] = i
        data, total = {}, None
        for row in rows[1:]:
            cells = [" ".join(c).strip() for c in row]
            if not cells or not cells[0]:
                continue
            rec = {"name": cells[0], "uganda": cells[0].lower().startswith("uganda")}
            for k, i in col.items():
                rec[k] = cells[i] if i < len(cells) else None
            first = cells[0].lower()
            if first.startswith("total") or "total" in first.split("/")[0]:
                total = rec
            else:
                data[_prov_key(cells[0])] = rec
        return data, total
    return {}, None


def _sum(recs, field):
    vals = [_num(r.get(field)) for r in recs]
    return None if any(v is None for v in vals) or not vals else sum(vals)


def check(sitrep_docx, brief_docx):
    """-> (errors[list[str]], warnings[list[str]])"""
    errors, warnings = [], []
    ts, tb = read_tables(sitrep_docx), read_tables(brief_docx)

    # ---- 1. tuiles d'indicateurs de meme libelle
    a, b = tiles(ts), tiles(tb)
    common = sorted(set(a) & set(b))
    if not common:
        warnings.append("concordance : aucune tuile commune reconnue, controle des tuiles ignore")
    for lab in common:
        if a[lab][0] != b[lab][0]:
            errors.append("tuile « %s » : SitRep %s != Brief %s" % (lab, a[lab][0], b[lab][0]))
        elif a[lab][1] and b[lab][1] and a[lab][1] != b[lab][1] and "drc" in a[lab][1].lower() \
                and "drc" in b[lab][1].lower():
            errors.append("tuile « %s » (detail) : SitRep « %s » != Brief « %s »" % (lab, a[lab][1], b[lab][1]))

    # ---- 2. tableau des provinces
    pa, ta = province_table(ts)
    pb, tbr = province_table(tb)
    if not pa or not pb:
        warnings.append("concordance : tableau des provinces non reconnu dans un des documents, controle ignore")
    else:
        for key in sorted(set(pa) & set(pb)):
            ra, rb = pa[key], pb[key]
            for field, lab in (("new_cases", "nouveaux cas"), ("cases", "cas cumules")):
                if _num(ra.get(field)) != _num(rb.get(field)):
                    errors.append("%s, %s : SitRep %s != Brief %s" % (ra["name"], lab, ra.get(field), rb.get(field)))
            da, db = _num(ra.get("deaths")), _num(rb.get("deaths"))
            if da != db:
                errors.append("%s, deces : SitRep %s != Brief %s" % (ra["name"], ra.get("deaths"), rb.get("deaths")))
            ca = _pct_in_paren(ra.get("deaths")) or _num(ra.get("cfr"))
            cb = _num(rb.get("cfr")) or _pct_in_paren(rb.get("deaths"))
            if ca is not None and cb is not None and abs(ca - cb) > 0.05:
                errors.append("%s, letalite : SitRep %s%% != Brief %s%%" % (ra["name"], ca, cb))
        for key in sorted(set(pa) ^ set(pb)):
            who = pa.get(key) or pb.get(key)
            if not who["uganda"]:
                warnings.append("province « %s » presente dans un seul des deux tableaux" % who["name"])
        # totaux RDC (hors Ouganda) : somme des provinces
        drc_a = [r for r in pa.values() if not r["uganda"]]
        drc_b = list(pb.values())
        for field, lab in (("new_cases", "nouveaux cas"), ("cases", "cas cumules")):
            sa, sb = _sum(drc_a, field), _sum(drc_b, field)
            if sa is not None and sb is not None and sa != sb:
                errors.append("total RDC, %s : SitRep %s != Brief %s (somme des provinces)" % (lab, sa, sb))
        sda, sdb = _sum(drc_a, "deaths"), _sum(drc_b, "deaths")
        if sda is not None and sdb is not None and sda != sdb:
            errors.append("total RDC, deces : SitRep %s != Brief %s (somme des provinces)" % (sda, sdb))

        # ---- 3. arithmetique interne (avertissements)
        for tag, recs, total, has_ug in (("SitRep", list(pa.values()), ta, True), ("Brief", drc_b, tbr, False)):
            if total is None:
                continue
            for field, lab in (("new_cases", "nouveaux cas"), ("cases", "cas cumules"), ("deaths", "deces")):
                s_all = _sum(recs, field)
                t = _num(total.get(field))
                if s_all is not None and t is not None and s_all != t:
                    warnings.append("%s : somme des lignes (%s) != total affiche (%s) pour %s" %
                                    (tag, int(s_all), int(t), lab))
            for r in recs:
                c, d = _num(r.get("cases")), _num(r.get("deaths"))
                shown = _pct_in_paren(r.get("deaths")) or _num(r.get("cfr"))
                if c and d is not None and shown is not None and abs(round(100.0 * d / c, 1) - shown) > 0.15:
                    warnings.append("%s, %s : letalite affichee %s%% != deces/cas %.1f%%" %
                                    (tag, r["name"], shown, 100.0 * d / c))
    # ---- 4. tuiles : RDC + Ouganda = continental
    for tag, t in (("SitRep", a), ("Brief", b)):
        for lab in ("confirmed cases", "deaths", "recovered"):
            if lab in t:
                m = re.search(r"DRC\s*([\d,]+)\s*·\s*Uganda\s*([\d,]+)", t[lab][1])
                if m:
                    tot = _num(t[lab][0])
                    if tot is not None and _num(m.group(1)) + _num(m.group(2)) != tot:
                        warnings.append("%s, tuile « %s » : RDC %s + Ouganda %s != %s" %
                                        (tag, lab, m.group(1), m.group(2), t[lab][0]))
    return errors, warnings


if __name__ == "__main__":
    import sys
    e, w = check(sys.argv[1], sys.argv[2])
    for x in e:
        print("ERREUR     :", x)
    for x in w:
        print("AVERTISSEMENT:", x)
    print("concordance : %d erreur(s), %d avertissement(s)" % (len(e), len(w)))
    sys.exit(1 if e else 0)
