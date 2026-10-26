# -*- coding: utf-8 -*-
"""
Lecture STRICTE d'un SitRep RDC (texte `pdftotext -layout`) -> dictionnaire de valeurs.

Regles de rigueur (projet PREIS) :
  * aucune valeur n'est inventee : un champ introuvable vaut None (le generateur ecrit alors ND
    ou conserve l'ancien paragraphe en le signalant) ;
  * les champs OBLIGATOIRES (totaux nationaux, tableau 1) font echouer l'analyse (ParseError)
    s'ils sont absents ou incoherents -> aucun deck n'est produit ;
  * toute valeur lue a plusieurs endroits du SitRep est recoupee (totaux, sommes par province,
    echantillons, isolement...). Un ecart est enregistre dans S["warnings"] (valeur concernee
    mise a None si elle ne peut plus etre garantie) ou, pour les totaux, leve ParseError.
"""
import datetime as _dt
import re
import unicodedata

PROVINCES = ["Ituri", "Nord-Kivu", "Haut-Uélé", "Tshopo", "Sud-Kivu", "Bas-Uélé", "Sud Ubangi", "Nord Ubangi"]
_MONTHS = {"janvier": 1, "février": 2, "fevrier": 2, "mars": 3, "avril": 4, "mai": 5, "juin": 6,
           "juillet": 7, "août": 8, "aout": 8, "septembre": 9, "octobre": 10, "novembre": 11, "décembre": 12,
           "decembre": 12}


class ParseError(Exception):
    pass


def _fold(s):
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    return re.sub(r"[\s\-_]+", " ", s.lower()).strip()


_PROV_KEYS = {_fold(p): p for p in PROVINCES}
_PROV_KEYS.update({"haut uele": "Haut-Uélé", "bas uele": "Bas-Uélé", "sud ubangi": "Sud Ubangi", "nord ubangi": "Nord Ubangi",
                   "nord kivu": "Nord-Kivu", "sud kivu": "Sud-Kivu"})


def prov_name(s):
    return _PROV_KEYS.get(_fold(s))


def num(s):
    """'8 787' / '8787' / '15,0' / '15,0 %' -> int ou float ; None si non numerique."""
    if s is None:
        return None
    s = str(s).replace(" ", " ").replace("\xa0", " ").strip().rstrip("%").strip()
    s = re.sub(r"(?<=\d)\s+(?=\d)", "", s).replace(",", ".")
    try:
        f = float(s)
    except ValueError:
        return None
    return int(f) if re.fullmatch(r"-?\d+", s) else f


def _clean(text):
    return text.replace(" ", " ").replace("\xa0", " ").replace("\r", "")


def _cells(line):
    return [c for c in re.split(r"\s{2,}", line.strip()) if c != ""]


def _need(cond, msg):
    if not cond:
        raise ParseError(msg)


def _warn(S, msg):
    S["warnings"].append(msg)


# ----------------------------------------------------------------------------------------------
def parse_sitrep(text, expected_no=None):
    text = _clean(text)
    S = {"warnings": [], "sitrep_no": None}
    lines = text.split("\n")

    # ---- identite ---------------------------------------------------------------------------
    m = re.search(r"SitRep\s*N[°º]?\s*(\d+)\s*/\s*MVE[- ]?BDBV\s*/\s*(\d{2})/(\d{2})/(\d{4})", text)
    _need(m, "numero de SitRep introuvable (SitRep N°xxx /MVE-BDBV/jj/mm/aaaa)")
    S["sitrep_no"] = int(m.group(1))
    if expected_no is not None:
        _need(S["sitrep_no"] == int(expected_no), "le PDF est le SitRep %d et non %s" % (S["sitrep_no"], expected_no))
    m2 = re.search(r"Date d[eu] rapport\w*\s*:\s*(\d{1,2})\s+([A-Za-zéûô]+)\s+(\d{4})", text)
    _need(m2 and _fold(m2.group(2)) in {_fold(k) for k in _MONTHS}, "date du rapport introuvable")
    mo = [v for k, v in _MONTHS.items() if _fold(k) == _fold(m2.group(2))][0]
    S["report_date"] = _dt.date(int(m2.group(3)), mo, int(m2.group(1)))
    fn = _dt.date(int(m.group(4)), int(m.group(3)), int(m.group(2)))
    S["date_warning"] = None
    if fn != S["report_date"]:
        # 14/10/2026 : l'INSP imprime parfois « Date du rapport » = J+1 alors que l'identifiant et les tableaux sont de J
        # (SitRep 150 : titre 11/10, en-tete 12/10, « Tableau 1 ... au 11 octobre »). Arbitrage strict : on n'accepte que si la
        # legende du Tableau 1 porte la date du TITRE ; la date retenue est alors celle du titre et l'ecart est signale.
        mt = re.search(r"Tableau\s*1\..{0,160}?\bau\s+(\d{1,2})\s+([A-Za-zéûô]+)\s+(\d{4})", re.sub(r"\s+", " ", text))
        td = None
        if mt and _fold(mt.group(2)) in {_fold(k) for k in _MONTHS}:
            tmo = [v for k, v in _MONTHS.items() if _fold(k) == _fold(mt.group(2))][0]
            td = _dt.date(int(mt.group(3)), tmo, int(mt.group(1)))
        _need(td == fn, "date du titre (%s) differente de la date du rapport (%s)" % (fn, S["report_date"]))
        S["date_warning"] = "en-tete : date du rapport %s ; identifiant et Tableau 1 : %s (date retenue : %s)" % (S["report_date"], fn, fn)
        S["report_date"] = fn
    m3 = re.search(r"Date de publication\s*:\s*(\d{1,2})\s+([A-Za-zéûô]+)\s+(\d{4})", text)
    if m3:
        mo3 = [v for k, v in _MONTHS.items() if _fold(k) == _fold(m3.group(2))]
        if mo3:
            S["pub_date"] = _dt.date(int(m3.group(3)), mo3[0], int(m3.group(1)))

    # ---- bandeau de chiffres cles (page 1) --------------------------------------------------
    head = "\n".join(lines[:60])
    kpi = re.search(r"\n\s*(\d[\d ]*?)\s{2,}(\d[\d ]*?)\s{2,}([\d,]+)\s*%\s{2,}(\d[\d ]*?)\s{2,}(\d[\d ]*?)\s{2,}([\d,]+)\s*%", head)
    _need(kpi, "bandeau de chiffres cles (cas, deces, letalite, isolement, gueris, suivi) introuvable")
    S["cases_cum"], S["deaths_cum"], S["cfr"] = num(kpi.group(1)), num(kpi.group(2)), num(kpi.group(3))
    S["isolation_total"], S["recovered_cum"], S["contacts_pct_kpi"] = num(kpi.group(4)), num(kpi.group(5)), num(kpi.group(6))

    # ---- faits saillants --------------------------------------------------------------------
    flat = re.sub(r"\s+", " ", head)
    # SitRep 159+ (22/10/2026) : formulation compacte « Au total, 51 nouveaux cas confirmes et 30 deces (12 deces communautaires et 18 intra-CTE) »
    mn = re.search(r"\(?(\d+)\)?\s+nouveaux\s+cas\s+confirm[ée]s\s+et\s+(\d+)\s+d[ée]c[èe]s\s*\(\s*(\d+)\s+d[ée]c[èe]s\s+communautaires\s+et\s+(\d+)\s+(?:d[ée]c[èe]s\s+)?intra[- ]?CTE\s*\)", flat)
    if mn:
        _need(int(mn.group(3)) + int(mn.group(4)) == int(mn.group(2)), "faits saillants incoherents : deces communautaires + intra-CTE != total des deces")
    m = re.search(r"\((\d+)\)\s+nouveaux.{0,30}?cas confirm[ée]s.*?dont\s+(\d+)\s+d[ée]c[èe]s\s+communautaires", flat)
    if mn:
        S["new_cases"], S["new_comm_deaths"] = int(mn.group(1)), int(mn.group(3))
    elif m:
        S["new_cases"], S["new_comm_deaths"] = int(m.group(1)), int(m.group(2))
    else:
        # PDF a l'ordre de lecture melange (SitRep 150 publie : « ... dont 37 deces communautaires, ont (88) nouveaux ete notifies »).
        # Repli : les deux nombres sont cherches separement ; le recoupement avec le total du Tableau 1 (plus bas) reste bloquant.
        ma = re.search(r"\((\d+)\)\s+nouveaux", flat)
        mb = re.search(r"dont\s+(\d+)\s+d[ée]c[èe]s\s+communautaires", flat)
        _need(ma and mb, "nouveaux cas / deces communautaires introuvables dans les faits saillants")
        S["new_cases"], S["new_comm_deaths"] = int(ma.group(1)), int(mb.group(1))
    m = re.search(r"(\d+)\s+d[ée]c[èe]s\s+ont\s+[ée]t[ée]\s+enregistr[ée]s\s+dans\s+les\s+CTE", flat)
    _need(m or mn, "deces intra-CTE introuvables dans les faits saillants")
    S["new_cte_deaths"] = int(mn.group(4)) if mn else int(m.group(1))
    S["new_deaths"] = S["new_comm_deaths"] + S["new_cte_deaths"]
    m = re.search(r"total de\s+(\d[\d ]*?)\s+nouveaux [ée]chantillons.*?(\d+)\s+de ces [ée]chantillons se sont r[ée]v[ée]l[ée]s positifs.*?([\d,]+)\s*%\s+de positivit", flat)
    S["samples"], S["positives"], S["positivity"] = (num(m.group(1)), num(m.group(2)), num(m.group(3))) if m else (None, None, None)
    m = re.search(r"(\d+)\s+Zones de sant[ée]\s+touch[ée]es\s+sur\s+(\d+)\s+\(([\d,]+)\s*%\)", flat)
    if not m:
        # SitRep 159+ : le bandeau est decoupe par la mise en page ; la phrase de synthese donne la meme valeur
        # (« ... 66 zones de santé sur 178 (37,1 %), réparties dans huit provinces, ont été touchées »)
        m = re.search(r"(\d+)\s+zones de sant[ée]\s+sur\s+(\d+)\s+\(([\d,]+)\s*%\)", flat, re.I)
    S["hz_affected"], S["hz_total"] = (int(m.group(1)), int(m.group(2))) if m else (None, None)
    S["hz_pct"] = num(m.group(3)) if m else None
    m = re.search(r"(\d+)\s+Provinces touch[ée]es", flat)
    S["n_provinces"] = int(m.group(1)) if m else None
    S["_flat_head"] = flat
    # guéris du jour : "23 patients (15 en Ituri et 8 au Nord Kivu) ont été déclarés guéris"
    m = re.search(r"(\d+)\s+patients\s*\(([^)]*)\)\s+ont\s+[ée]t[ée]\s+d[ée]clar[ée]s\s+gu[ée]ris", re.sub(r"\s+", " ", text))
    S["recovered_new"], S["recovered_by_prov"] = None, None
    if m:
        S["recovered_new"] = int(m.group(1))
        byp = {}
        for n_, p_ in re.findall(r"(\d+)\s+(?:en|au|à la|a la|à|a)\s+([A-Za-zÀ-ÿ\- ]+?)(?:\s+et\s+|,|$)", m.group(2)):
            pn = prov_name(p_)
            if pn:
                byp[pn] = int(n_)
        S["recovered_by_prov"] = byp or None

    # ---- tableau 1 --------------------------------------------------------------------------
    t1 = {}
    i1 = next((i for i, l in enumerate(lines) if re.search(r"Tableau\s*1\.", l)), None)
    _need(i1 is not None, "Tableau 1 introuvable")
    i1e = next((i for i, l in enumerate(lines) if i > i1 and re.search(r"Tableau\s*2\.", l)), i1 + 40)
    for l in lines[i1 + 1: i1e]:
        c = _cells(l)
        if len(c) >= 6 and prov_name(c[0]):
            hz = re.match(r"(\d+)/(\d+)\s*\(([\d,]+)\s*%\)", c[5]) if len(c) > 5 else None
            t1[prov_name(c[0])] = dict(new=num(c[1]), cases=num(c[2]), deaths=num(c[3]), cfr=num(c[4]),
                                       hz_aff=int(hz.group(1)) if hz else None, hz_tot=int(hz.group(2)) if hz else None)
    # ligne Total : chiffres sur une ligne, le mot "Total" sur la suivante
    tot = None
    for k, l in enumerate(lines[i1 + 1: i1e]):
        c = _cells(l)
        if len(c) >= 4 and c[0].replace(" ", "").isdigit() and re.fullmatch(r"[\d,]+%", c[3].replace(" ", "")) and not prov_name(c[0]):
            tot = dict(new=num(c[0]), cases=num(c[1]), deaths=num(c[2]), cfr=num(c[3].rstrip("%")))
            nxt = _cells(lines[i1 + 1 + k + 1]) if i1 + 2 + k < len(lines) else []
            mm = re.match(r"(\d+)/(\d+)", nxt[1]) if len(nxt) > 1 and nxt[0] == "Total" else None
            if mm:
                tot["hz_aff"], tot["hz_tot"] = int(mm.group(1)), int(mm.group(2))
            break
    if tot is None:
        # variante (SitRep 151) : « cas  deces  letalite% » sur une ligne, puis « Total  <nouveaux cas> ... <zones touchees> » sur la suivante
        for k, l in enumerate(lines[i1 + 1: i1e]):
            c = _cells(l)
            if len(c) == 3 and c[0].replace(" ", "").isdigit() and re.fullmatch(r"[\d,]+%", c[2].replace(" ", "")) and not prov_name(c[0]):
                nxt = _cells(lines[i1 + 1 + k + 1]) if i1 + 2 + k < len(lines) else []
                if len(nxt) >= 2 and nxt[0] == "Total" and nxt[1].replace(" ", "").isdigit():
                    tot = dict(new=num(nxt[1]), cases=num(c[0]), deaths=num(c[1]), cfr=num(c[2].rstrip("%")))
                    mm = re.match(r"(\d+)/(\d+)", nxt[-1])
                    if mm:
                        tot["hz_aff"], tot["hz_tot"] = int(mm.group(1)), int(mm.group(2))
                    break
    _need(len(t1) >= 3 and tot, "Tableau 1 incomplet (provinces ou ligne Total)")
    S["t1"], S["t1_total"] = t1, tot
    _need(sum(v["cases"] for v in t1.values()) == tot["cases"], "Tableau 1 : somme des cas par province != total")
    _need(sum(v["deaths"] for v in t1.values()) == tot["deaths"], "Tableau 1 : somme des deces par province != total")
    _need(sum(v["new"] for v in t1.values()) == tot["new"], "Tableau 1 : somme des nouveaux cas != total")
    _need((tot["cases"], tot["deaths"]) == (S["cases_cum"], S["deaths_cum"]), "Tableau 1 (%s/%s) != bandeau (%s/%s)" % (tot["cases"], tot["deaths"], S["cases_cum"], S["deaths_cum"]))
    _need(tot["new"] == S["new_cases"], "nouveaux cas : tableau 1 (%s) != faits saillants (%s)" % (tot["new"], S["new_cases"]))
    if tot.get("hz_aff") is not None:
        if S["hz_affected"] is None:
            S["hz_affected"], S["hz_total"] = tot["hz_aff"], tot["hz_tot"]
            S["hz_pct"] = round(100.0 * tot["hz_aff"] / tot["hz_tot"], 1)
    S["nat_cfr_calc"] = round(100.0 * S["deaths_cum"] / S["cases_cum"], 1)
    if abs(S["nat_cfr_calc"] - S["cfr"]) > 0.15:
        _warn(S, "letalite publiee %.1f != recalcul %.1f" % (S["cfr"], S["nat_cfr_calc"]))
    if S["hz_affected"] is not None and tot.get("hz_aff") not in (None, S["hz_affected"]):
        _warn(S, "zones touchees : tableau 1 (%s) != faits saillants (%s)" % (tot.get("hz_aff"), S["hz_affected"]))

    # ---- tableau 2 (zones de santé) ---------------------------------------------------------
    S["t2"] = _parse_table2(lines, S, t1, tot)

    # nouvelle zone de sante citee dans les faits saillants (nom valide par le Tableau 2)
    S["new_hz"] = None
    m = re.search(r"zone de sant[ée]\s+touch[ée]e\s*:\s*([A-Za-zÀ-ÿ\-']+)", S.pop("_flat_head"))
    if m and S["t2"]:
        for pn, zs in S["t2"]["zones"].items():
            if m.group(1) in zs:
                S["new_hz"] = {"zone": m.group(1), "province": pn, "cases": zs[m.group(1)]["cases"], "deaths": zs[m.group(1)]["deaths"]}
    # ---- tableau 3 (alertes) ----------------------------------------------------------------
    S["t3"], S["t3_total"] = _parse_table3(lines, S)

    # ---- section 2.1 : contacts -------------------------------------------------------------
    S.update(_parse_contacts(text, S))

    # ---- tableau 4 (PoE/PoC) ----------------------------------------------------------------
    S["t4"] = _parse_table4(lines)

    # ---- sections narratives ----------------------------------------------------------------
    S["sections"] = _split_sections(lines)
    S["lab"] = _parse_lab(S)
    S["care"] = _parse_care(S)
    S["ipc"] = _parse_ipc(S)
    S["vacc"] = _parse_vacc(S)
    S["poe_text"] = _parse_poe_text(S)
    S["hz_threshold_note"] = None
    return S


# ----------------------------------------------------------------------------------------------
def _tokens(line):
    return [(m.group(0), m.start(), m.end()) for m in re.finditer(r"\S+", line)]


_PCT = re.compile(r"^[\d,]+%$")


def _parse_table2(lines, S, t1, tot):
    """Tableau 2 (zones). Les colonnes vides font disparaitre des cellules : on ne peut donc pas se fier au
    nombre de cellules. Les colonnes « nouveaux / communautaires / intra-CTE / total » sont attribuees par
    POSITION, calibree page par page (l'echelle horizontale change d'une page a l'autre) a partir des lignes
    qui ont les 4 valeurs. Une ligne dont l'attribution est ambigue est marquee non fiable (None)."""
    i2 = next((i for i, l in enumerate(lines) if re.search(r"Tableau\s*2\.", l)), None)
    i3 = next((i for i, l in enumerate(lines) if re.search(r"Tableau\s*3\.", l)), None)
    if i2 is None or i3 is None:
        _warn(S, "Tableau 2 introuvable : details par zone de sante non disponibles")
        return None
    # --- pages (saut de page = \f) ---
    pages, cur = [], []
    for l in lines[i2 + 1: i3]:
        if "\f" in l:
            pages.append(cur)
            cur = []
            l = l.replace("\f", "")
        cur.append(l)
    pages.append(cur)
    for pl in pages:   # numero de page en pied de page
        for j in range(len(pl) - 1, -1, -1):
            if pl[j].strip():
                if re.fullmatch(r"\s*\d{1,3}\s*", pl[j]):
                    pl.pop(j)
                break
    rows = []   # dict(name, cases, deaths, cfr, extras[(val, centre_rel)], page)
    for pg, pl in enumerate(pages):
        pend_name = None
        prev_row = None
        items = []
        for l in pl:
            if not l.strip():
                continue
            toks = _tokens(l)
            pct = [t for t in toks if _PCT.match(t[0])]
            alpha_only = not re.search(r"\d", l)
            if pct:
                k = toks.index(pct[0])
                lead = toks[:k]
                # nombres avant le pourcentage : cas, deces ; le reste du debut = nom
                nums_before = []
                while lead and re.fullmatch(r"\d+|NA", lead[-1][0]) and len(nums_before) < 2:
                    nums_before.insert(0, lead.pop())
                name = " ".join(t[0] for t in lead)
                after = [t for t in toks[k + 1:] if re.fullmatch(r"\d+", t[0])]
                ref = pct[0][2]
                row = dict(name=name, cases=num(nums_before[0][0]) if len(nums_before) == 2 and nums_before[0][0] != "NA" else None,
                           deaths=num(nums_before[-1][0]) if nums_before else None, cfr=num(pct[0][0]),
                           extras=[(int(t[0]), (t[1] + t[2]) / 2.0 - ref) for t in after], page=pg, frag_before=pend_name, ref=ref)
                pend_name = None
                items.append(row)
                prev_row = row
            elif re.match(r"\s*A ventiler", l):
                nums = [t[0] for t in toks[2:]]
                items.append(dict(name="A ventiler", cases=None, deaths=num(nums[1]) if len(nums) > 1 else None, cfr=None, extras=[], page=pg, frag_before=None))
                prev_row = items[-1]
            elif alpha_only and len(toks) <= 2:
                # fragment de nom (ex. "Boma" / "Mangbetu", "Makiso-" / "Kisangani")
                if items and items[-1]["name"] == "" and items[-1].get("frag_after") is None:
                    items[-1]["frag_after"] = " ".join(t[0] for t in toks)
                else:
                    pend_name = " ".join(t[0] for t in toks)
            elif not re.search(r"[A-Za-zÀ-ÿ]", l) and prev_row is not None:
                # ligne de chiffres orpheline : appartient a la ligne precedente
                ref = None
                prev_row.setdefault("orphans", []).append([(int(t[0]), (t[1] + t[2]) / 2.0) for t in toks if re.fullmatch(r"\d+", t[0])])
        rows.extend(items)
        # calibration de la page
    # recomposer les noms coupes
    for r in rows:
        if r["name"] == "":
            fb, fa = r.get("frag_before"), r.get("frag_after")
            nm = ((fb or "") + ("" if (fb or "").endswith("-") else " ") + (fa or "")).strip() if fb or fa else ""
            if fb and fa and fb.endswith("-"):
                nm = fb + fa
            r["name"] = nm
    # --- calibration par page ---
    import statistics
    calib = {}
    for pg in set(r["page"] for r in rows):
        full = [r for r in rows if r["page"] == pg and len(r["extras"]) == 4]
        if full:
            calib[pg] = [statistics.median([r["extras"][k][1] for r in full]) for k in range(4)]
    for r in rows:
        r.update(new=None, comm=None, cte=None, totdeaths=None, reliable=True)
        ex = list(r["extras"])
        for orp in r.get("orphans", []):     # chiffres decales sur la ligne suivante (ex. Adja, Fataki)
            ex.extend((v, ctr - r["ref"]) for v, ctr in orp)
        ex.sort(key=lambda e: e[1])
        if len(ex) == 4:
            r["new"], r["comm"], r["cte"], r["totdeaths"] = [e[0] for e in ex]
        elif ex:
            cal = calib.get(r["page"])
            if cal:
                used = set()
                ok = True
                for val, ctr in ex:
                    k = min(range(4), key=lambda j: abs(ctr - cal[j]))
                    if k in used or abs(ctr - cal[k]) > 6:
                        ok = False
                        break
                    used.add(k)
                    r[("new", "comm", "cte", "totdeaths")[k]] = val
                if not ok:
                    r.update(new=None, comm=None, cte=None, totdeaths=None, reliable=False)
            else:
                if len(ex) == 1:
                    r["totdeaths"] = ex[0][0]
                else:
                    r["reliable"] = False
    # --- construire les resultats ---
    zones_by_prov, prov_rows, order, out = {}, {}, [], {}
    cur_prov = None
    for r in rows:
        name = r["name"]
        if not name:
            continue
        row = dict(cases=r["cases"], deaths=r["deaths"], cfr=r["cfr"], new=r["new"], comm=r["comm"], cte=r["cte"], totdeaths=r["totdeaths"], reliable=r["reliable"])
        if name == "Total":
            out["__total__"] = row
            continue
        pn = prov_name(name)
        if pn and pn in prov_rows:
            pn = None
        if pn:
            cur_prov = pn
            prov_rows[pn] = dict(row, zone=name)
            zones_by_prov.setdefault(pn, {})
            order.append(pn)
        elif cur_prov:
            zones_by_prov[cur_prov][name] = row
    ok = True
    for pn, tr in t1.items():
        pr, zs = prov_rows.get(pn), zones_by_prov.get(pn)
        if not pr or not zs:
            continue
        if (pr["cases"], pr["deaths"]) != (tr["cases"], tr["deaths"]):
            _warn(S, "Tableau 2 %s (%s/%s) != Tableau 1 (%s/%s)" % (pn, pr["cases"], pr["deaths"], tr["cases"], tr["deaths"]))
            ok = False
        sc = sum(z["cases"] or 0 for z in zs.values())
        if sc != tr["cases"]:
            _warn(S, "Tableau 2 %s : somme des zones (%s) != total province (%s)" % (pn, sc, tr["cases"]))
            ok = False
        if any(not z["reliable"] for z in zs.values()):
            _warn(S, "Tableau 2 %s : colonnes du jour ambigues pour %s" % (pn, ", ".join(k for k, z in zs.items() if not z["reliable"])))
            ok = False
        elif pr.get("new") is not None and sum(z["new"] or 0 for z in zs.values()) != pr["new"]:
            _warn(S, "Tableau 2 %s : somme des nouveaux cas par zone (%s) != ligne province (%s)" % (pn, sum(z["new"] or 0 for z in zs.values()), pr["new"]))
            ok = False
    tt = out.get("__total__")
    if tt and (tt["cases"], tt["deaths"]) != (tot["cases"], tot["deaths"]):
        _warn(S, "Tableau 2 total != Tableau 1 total")
        ok = False
    if not prov_rows:
        _warn(S, "Tableau 2 illisible")
        return None
    return {"ok": ok, "prov": prov_rows, "zones": zones_by_prov, "total": tt}


def _parse_table3(lines, S):
    i3 = next((i for i, l in enumerate(lines) if re.search(r"Tableau\s*3\.", l)), None)
    if i3 is None:
        _warn(S, "Tableau 3 introuvable")
        return None, None
    res, total = {}, None
    for l in lines[i3 + 1: i3 + 40]:
        c = _cells(l)
        if len(c) < 9:
            continue
        pn = prov_name(c[0])
        vals = [num(x) for x in c[1:10]]
        if len(vals) < 9 or None in vals:
            continue
        d = dict(alive=vals[0], dead=vals[1], total=vals[2], val_alive=vals[3], val_dead=vals[4], inv_alive=vals[5],
                 inv_dead=vals[6], investigated=vals[7], transferred=vals[8])
        if pn:
            res[pn] = d
        elif c[0] == "Total":
            total = d
    for pn, d in res.items():
        # vivants + decedes = total ; validees + invalidees <= total (des alertes peuvent ne pas etre encore verifiees : SitRep 151 Haut-Uele 98/149)
        if d["alive"] + d["dead"] != d["total"] or d["val_alive"] + d["val_dead"] + d["inv_alive"] + d["inv_dead"] > d["total"]:
            _warn(S, "Tableau 3 %s : colonnes incoherentes" % pn)
    if total is None:
        # ligne Total Général : chiffres puis le mot "Général" sur la ligne suivante
        for l in lines[i3 + 1: i3 + 40]:
            c = _cells(l)
            if len(c) >= 9 and c[0] == "Total":
                pass
    if total is None:
        # variante (SitRep 159+) : 9 chiffres sur une ligne sans libelle, « Total Général » sur la suivante
        for k, l in enumerate(lines[i3 + 1: i3 + 44]):
            c = _cells(l)
            nxt = lines[i3 + 2 + k] if i3 + 2 + k < len(lines) else ""
            vals = [num(x) for x in c]
            if len(c) == 9 and None not in vals and re.match(r"\s*Total\b", nxt):
                total = dict(alive=vals[0], dead=vals[1], total=vals[2], val_alive=vals[3], val_dead=vals[4], inv_alive=vals[5],
                             inv_dead=vals[6], investigated=vals[7], transferred=vals[8])
                break
    if total is None:
        for l in lines[i3 + 1: i3 + 45]:
            if re.match(r"\s*Total\b", l):
                c = _cells(l)
                vals = [num(x) for x in c[1:10]]
                if len(vals) >= 9 and None not in vals:
                    total = dict(alive=vals[0], dead=vals[1], total=vals[2], val_alive=vals[3], val_dead=vals[4], inv_alive=vals[5],
                                 inv_dead=vals[6], investigated=vals[7], transferred=vals[8])
    if not res:
        _warn(S, "Tableau 3 illisible")
        return None, None
    if total:
        for key in ("total", "investigated", "transferred"):
            if sum(d[key] for d in res.values()) != total[key]:
                _warn(S, "Tableau 3 : somme des provinces != total (%s)" % key)
                total = None
                break
    return res, total


def _parse_contacts(text, S):
    flat = re.sub(r"\s+", " ", text)
    out = {"contacts": None}
    m = re.search(r"Parmis?\s+(?:les\s+)?(\d[\d ]*?)\s+contacts en cours de suivi([^,]{0,90}),\s+(\d[\d ]*?)\s+(?:contacts\s+)?ont [ée]t[ée] vus.*?(?:proportion|taux) de suivi de\s+([\d,]+)\s*%(?:.*?\(([\d,]+)\s*%\))?", flat)
    if not m:
        # SitRep 159+ : « 36 218 contacts étaient sous suivi dans les cinq provinces ayant transmis leurs données ; 32 023 ont été vus ..., soit un taux de suivi de 88,4 % »
        m = re.search(r"(\d[\d ]*?)\s+contacts [ée]taient sous suivi([^;]{0,120});\s+(\d[\d ]*?)\s+(?:contacts\s+)?ont [ée]t[ée] vus.*?taux de suivi de\s+([\d,]+)\s*%", flat)
    alt = None
    if not m:
        # SitRep 163 : « 32 697 contacts sur 37 552 ont ete vus au cours des dernieres 24 heures, soit un taux de suivi de 87,1 % »
        alt = re.search(r"(\d[\d ]*?)\s+contacts\s+sur\s+(\d[\d ]*?)\s+ont\s+[ée]t[ée]\s+vus.*?taux de suivi de\s+([\d,]+)\s*%", flat)
    if not m and not alt:
        _warn(S, "phrase de suivi des contacts introuvable")
        return out
    if alt:
        class _M:  # meme interface que le motif principal
            def __init__(s_, a): s_.a = a
            def group(s_, i): return {1: s_.a.group(2), 2: "", 3: s_.a.group(1), 4: s_.a.group(3)}.get(i)
            def end(s_): return s_.a.end()
        m = _M(alt)
    listed, seen, pct = num(m.group(1)), num(m.group(3)), num(m.group(4))
    cov = m.group(2) or ""
    if abs(100.0 * seen / listed - pct) > 0.06:
        _warn(S, "suivi des contacts : %s/%s = %.1f != %.1f publie" % (seen, listed, 100.0 * seen / listed, pct))
    if S.get("contacts_pct_kpi") is not None and abs(S["contacts_pct_kpi"] - pct) > 0.06:
        _warn(S, "suivi des contacts : bandeau %.1f != texte %.1f" % (S["contacts_pct_kpi"], pct))
    byp = {}
    seg = flat[m.end(): m.end() + 900]
    seg = re.split(r"\s2\.2\.\s", seg)[0]
    mm = re.search(r"(?:Par province, )?[Cc]e(?:tte)? (?:proportion|taux) [ée]tait de (.*?)\. (?:Aucune|Seul|Les ZS|Le |La |Au |En |\d)", seg + " . 0")
    pairs = []
    if mm:
        pairs = re.findall(r"([\d,]+)\s*%\s*\((\d[\d ]*?)/(\d[\d ]*?)\)\s+(?:au|à la|a la|en|à|a)\s+([A-Za-zÀ-ÿ\- ]+?)(?=\s*[,;.]| et |$)", mm.group(1))
    else:
        # SitRep 159+ : « Par province, le taux de suivi est de 97,7 % au Bas-Uélé (758/776), 93,8 % en Ituri (13 450/14 333), ... »
        mm = re.search(r"Par province, le taux de suivi est de (.*?)\.\s", seg + ". ")
        if mm:
            pairs = [(p_, s_, l_, n_) for p_, n_, s_, l_ in re.findall(r"([\d,]+)\s*%\s+(?:au|à la|a la|en|à|a)\s+([A-Za-zÀ-ÿ\- ]+?)\s*\((\d[\d ]*?)/(\d[\d ]*?)\)", mm.group(1))]
    if mm:
        for p_, s_, l_, name in pairs:
            pn = prov_name(name.strip())
            if pn:
                byp[pn] = dict(pct=num(p_), seen=num(s_), listed=num(l_))
        for pn, d in list(byp.items()):
            if abs(100.0 * d["seen"] / d["listed"] - d["pct"]) > 0.06:
                _warn(S, "suivi %s : %s/%s != %.1f" % (pn, d["seen"], d["listed"], d["pct"]))
                del byp[pn]
        if byp and (sum(d["seen"] for d in byp.values()), sum(d["listed"] for d in byp.values())) != (seen, listed):
            _warn(S, "suivi par province : somme != total (%s/%s)" % (seen, listed))
    prev = re.search(r"par rapport [àa] la veille\s*\(([\d,]+)\s*%\)", flat)
    thr = re.search(r"seuil requis de\s*([\d,]+)\s*%", flat)
    nonrep = re.search(r"(Les ZS de [^.]*n.ont pas transmis leur rapport de suivi)", flat)
    bypp = {}
    if not byp and alt:  # SitRep 163 : seuls les taux par province sont publies (pas les effectifs)
        k_ = seg.find("le plus faible")
        sent = ""
        if k_ >= 0:
            pre = seg[max(0, k_ - 80):k_]
            pre = pre[pre.rfind(". ") + 2:] if ". " in pre else pre
            sent = re.split(r"\.\s+[A-ZÉ]", seg[k_:k_ + 300])[0]
            sent = pre + sent
        for pn_ in PROVINCES:
            mq = re.search(r"(?:%s|%s)[^()]{0,60}\(([\d,]+)\s*%%\)" % (re.escape(pn_), re.escape(pn_.replace(" ", "-"))), sent)
            if mq:
                bypp[pn_] = num(mq.group(1))
    out["contacts"] = dict(listed=listed, seen=seen, pct=pct, by_prov_pct=bypp or None, prev_pct_stated=num(prev.group(1)) if prev else None,
                           threshold=num(thr.group(1)) if thr else None, by_prov=byp or None,
                           nonreporting_text=nonrep.group(1) if nonrep else None)
    zs = re.findall(r"(Mutwanga|Makiso-Kisangani)", nonrep.group(1)) if nonrep else []
    m2 = re.search(r"ZS de ([A-Za-zÀ-ÿ\-]+) \(([A-Za-zÀ-ÿ\- ]+),\s*(\d[\d ]*?) contacts\)", nonrep.group(1)) if nonrep else None
    out["contacts"]["nonreporting_zs"] = [z for z in re.findall(r"(?:ZS de|et de)\s+([A-Za-zÀ-ÿ\-]+)", nonrep.group(1))] if nonrep else []
    out["contacts"]["nonreporting_contacts"] = (m2.group(1), num(m2.group(3))) if m2 else None
    # couverture partielle : « Parmi N contacts ... dans les provinces ayant transmis leurs donnees » (SitRep 149 : Ituri absent)
    mp = re.search(r";\s*([^;.]{3,80}?)\s+n[\u2019'](?:a|ont)\s+pas\s+transmis", seg)
    missing = []
    if mp:
        for piece in re.split(r"\s+et\s+", mp.group(1)):
            pn_ = prov_name(re.sub(r"^(?:l[\u2019']|le\s+|la\s+|les\s+)", "", piece.strip(), flags=re.I))
            if pn_:
                missing.append(pn_)
    mp2 = re.search(r"Aucune\s+donn[ée]es?\s+n[\u2019']a\s+[ée]t[ée]\s+transmises?\s+par\s+(?:le\s+|la\s+|l[\u2019'])?([A-Za-zÀ-ÿ\- ]+?)\s*(?:[.;,]|$)", seg)
    if mp2:
        pn_ = prov_name(mp2.group(1).strip())
        if pn_ and pn_ not in missing:
            missing.append(pn_)
    if "transmis" in cov and missing:
        part = out["contacts"]
        part["missing_prov"] = missing
        _warn(S, "suivi des contacts : couverture partielle (%s sans donnees) : %s/%s = %.1f %% non comparable a la veille -> blocs nationaux conserves, blocs provinciaux rafraichis" % (", ".join(missing) or "provinces non precisees", seen, listed, pct))
        return {"contacts": None, "contacts_partial": part}
    return out


T4_ROWS = [
    ("completeness", r"Compl[ée]tude des rapports"),
    ("functional", r"%\s*des PoE/PoC strat[ée]giques"),
    ("travellers", r"Nombre de personnes pass[ée]es"),
    ("screened", r"%\s*des voyageurs scre+n[ée]s"),
    ("handwashing", r"%\s*des voyageurs ayant lav[ée]"),
    ("sensitised", r"%\s*des voyageurs sensibilis[ée]s"),
    ("alerts", r"Nombre d.alertes notifi[ée]es"),
    ("validated", r"Nombre d.alertes vivantes valid[ée]es"),
    ("bodies", r"Nombre de corps sans vie intercept[ée]s"),
    ("transferred_pct", r"%\s*des cas suspects transf[ée]r[ée]s"),
    ("bodies_swabbed_pct", r"%\s*des corps sans vie swab[ée]s"),
    ("invest4h", r"%\s*des cas suspects investigu[ée]s dans"),
]


def _parse_table4_legacy(lines):
    i4 = next((i for i, l in enumerate(lines) if re.search(r"Tableau\s*4\.", l)), None)
    if i4 is None:
        return None
    hdr = None
    for l in lines[i4 + 1: i4 + 8]:
        c = _cells(l)
        if c and re.match(r"Indicateurs", c[0]):
            hdr = [("Global" if x.lower().startswith("global") else (prov_name(x) or x)) for x in c[1:]]
            break
    if not hdr:
        return None
    res = {h: {} for h in hdr}
    for l in lines[i4 + 1: i4 + 45]:
        c = _cells(l)
        if len(c) < 2:
            continue
        for key, pat in T4_ROWS:
            if re.match(pat, c[0], re.I):
                vals = c[1:]
                if len(vals) != len(hdr):
                    break
                for h, v in zip(hdr, vals):
                    res[h][key] = v
                break
    return res


_T4_VAL = re.compile(r"^(?:\d[\d ]*|\d+(?:,\d+)?%(?: \(\d+/\d+\))?|\(\d+/\d+\)|ND|NA)$")
_T4_PATS = [
    ("completeness", r"compl[ée]tude des rapports"),
    ("functional", r"%\s*des\s+poe/poc\s+strat"),  # « PoC/PoE stratégiques » (SitRep 161+) = effectif, pas un % : non interprete
    ("travellers", r"nombre (?:de personnes|des? voyageurs) pass"),
    ("screened", r"(?:%|proportion)\s*des voyageurs scre+n"),
    ("handwashing", r"%\s*des voyageurs ayant lav"),
    ("sensitised", r"%\s*des voyageurs sensibilis"),
    ("alerts", r"nombre d.alertes notifi"),
    ("validated", r"nombre d.alertes vivantes valid"),
    ("bodies", r"nombre (?:de|des) corps sans vie intercept"),
    ("transferred_pct", r"%\s*des cas suspects transf"),
    ("bodies_swabbed_pct", r"%\s*des? corps sans vie swab"),
    ("invest4h", r"%\s*des cas suspects investigu"),
]


def _t4_split(line):
    """[(texte, centre)] des cellules separees par 2 espaces ou plus."""
    return [(m.group(0), (m.start() + m.end()) / 2.0) for m in re.finditer(r"\S+(?: \S+)*", line)
            if True] if False else [(m.group(0), (m.start() + m.end()) / 2.0) for m in re.finditer(r"(?:\S+ ?)+(?=\s{2,}|\s*$)", line)]


def _parse_table4(lines):
    """Tableau 4 (PoE/PoC). Mise en page variable d'un SitRep a l'autre (libelle avant/apres les valeurs, ratio sur la ligne suivante) :
    une ligne a autant de valeurs que de colonnes ouvre une rangee ; les lignes suivantes (ratios, suite du libelle) lui sont rattachees
    jusqu'a la rangee suivante. Une valeur n'est retenue que si le libelle de la rangee est reconnu : sinon la cle reste absente."""
    i4 = next((i for i, l in enumerate(lines) if re.search(r"Tableau\s*4\.", l)), None)
    if i4 is None:
        return None
    hdr = None
    hi = None
    for k, l in enumerate(lines[i4 + 1: i4 + 8], start=i4 + 1):
        c = _cells(l)
        if c and re.match(r"Indicateurs", c[0]):
            hdr = [("Global" if x.lower().startswith("global") else (prov_name(x) or x)) for x in c[1:]]
            hi = k
            break
    if not hdr:
        return None
    n = len(hdr)
    rows = []  # {"label": [..], "vals": [(txt, centre)], "extra": [(txt, centre)]}
    for l in lines[hi + 1: hi + 60]:
        if not l.strip():
            continue
        if re.match(r"\s*(?:\d+(?:\.\d+)+\.?\s*\S|[▪•])", l):  # fin du tableau : titre de section ou puce
            break
        cells = _t4_split(l)
        if cells and not _T4_VAL.match(cells[0][0].strip()):
            # SitRep 163 : libelle et 1re valeur separes par un seul espace (« Nombre des voyageurs 161 879 »)
            mp = re.match(r"^(.*?[A-Za-zÀ-ÿ)]) +(\d[\d ]*|\d+(?:,\d+)?%(?: \(\d+/\d+\))?)\s*$", cells[0][0])
            if mp and _T4_VAL.match(mp.group(2)):
                cut = len(mp.group(1))
                cells[0:1] = [(mp.group(1), cells[0][1] - (len(cells[0][0]) - cut) / 2.0 - 1), (mp.group(2), cells[0][1] + cut / 2.0 + 1)] if False else [(mp.group(1), 0.0), (mp.group(2), cells[0][1] + (cut - len(cells[0][0])) / 2.0 + len(cells[0][0]) / 2.0)]
        lab = []
        vals = []
        for t, ctr in cells:
            if _T4_VAL.match(t.strip()) and (vals or lab or True):
                vals.append((t.strip(), ctr))
            else:
                if vals:  # texte apres des valeurs : hors tableau
                    lab = None
                    break
                lab.append(t.strip())
        if lab is None:
            break
        if len(vals) == n:
            rows.append({"label": list(lab), "vals": vals, "extra": []})
        elif rows:
            rows[-1]["label"] += lab
            rows[-1]["extra"] += vals
        # lignes avant la 1re rangee : libelle isole ignore
    res = {h: {} for h in hdr}
    for r in rows:
        label = re.sub(r"\s+", " ", " ".join(r["label"])).strip().lower()
        key = next((k for k, pat in _T4_PATS if re.search(pat, label)), None)
        if not key:
            continue
        cols = [list(v) for v in r["vals"]]
        for t, ctr in r["extra"]:
            j = min(range(n), key=lambda q: abs(cols[q][1] - ctr))
            if re.match(r"^\(\d+/\d+\)$", t) and re.match(r"^\d+(?:,\d+)?%$", cols[j][0]):
                cols[j][0] = cols[j][0] + " " + t
        for h, c in zip(hdr, cols):
            res[h][key] = c[0]
    # repli par cle sur l'ancienne lecture (mise en page libelle + valeurs sur la meme ligne)
    leg = _parse_table4_legacy(lines) or {}
    for h in res:
        for k, v in (leg.get(h) or {}).items():
            res[h].setdefault(k, v)
    return res


def t4_value(t4, col, key):
    """valeur brute de la cellule (texte) ou None."""
    try:
        v = t4[col][key]
    except (KeyError, TypeError):
        return None
    return None if v in (None, "", "NA") else v


def t4_pct(v):
    m = re.match(r"([\d,]+)\s*%", v or "")
    return num(m.group(1)) if m else None


def t4_ratio(v):
    m = re.match(r"([\d,]+)\s*%\s*\((\d+)/(\d+)\)", v or "")
    return (num(m.group(1)), int(m.group(2)), int(m.group(3))) if m else None


# ----------------------------------------------------------------------------------------------
_SEC_RE = [
    ("coord", r"^\s*(?:1\.\s*)?COORDINATION"),
    ("surv", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Surveillance"),
    ("poe", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Point sur les activit"),
    ("lab", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Laboratoire"),
    ("pci", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Pr[ée]vention et Contr"),
    # numerotation variable d'un SitRep a l'autre (SitRep 151 : 2.5 Prise en charge, 2.6 Vaccination... 2.13 PSEA) :
    # on reconnait les titres par leur libelle, quel que soit le numero
    ("care", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Prise en charge"),
    ("vacc", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Vaccination"),
    ("cont", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Continuit"),
    ("crec", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Communication"),
    ("smsps", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Sant[ée] mentale"),
    ("log", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*Logistique"),
    ("sec", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*S[ée]curit"),
    ("psea", r"(?<![\d.])\d\.\d{1,2}(?:\.\d{1,2})?\.?\s*PSEA"),
    ("msg", r"MESSAGES CLES|MESSAGES CL[ÉE]S"),
]


def _split_sections(lines):
    marks = []
    for i, l in enumerate(lines):
        for key, pat in _SEC_RE:
            if re.search(pat, l) and not l.strip().startswith("Tableau"):
                marks.append((i, key))
                break
    out = {}
    marks.sort()
    for n, (i, key) in enumerate(marks):
        j = marks[n + 1][0] if n + 1 < len(marks) else len(lines)
        body = [l for l in lines[i:j]]
        # un meme titre peut apparaitre plusieurs fois (2.4 puis 2.4.3) : on garde la 1re occurrence
        out.setdefault(key, "\n".join(body))
    return out


def _bullets(sec_text):
    """liste de (province|None, texte aplati) pour les puces d'une section."""
    items, cur = [], None
    for l in sec_text.split("\n"):
        s = l.strip()
        if re.fullmatch(r"\d{1,2}", s) or not s:
            continue
        if re.match(r"^[•▪]\s*", s) or re.match(r"^\.?\s*[•▪]", s):
            if cur is not None:
                items.append(cur)
            cur = re.sub(r"^\W*[•▪]\s*", "", s)
        else:
            cur = (cur + " " + s) if cur is not None else s
    if cur is not None:
        items.append(cur)
    res = []
    for it in items:
        it = re.sub(r"\s+", " ", it)
        m = re.match(r"^(?:(?:En|Au|À|A|Dans)\s+(?:la\s+)?)?(Ituri|Nord[- ]Kivu|Haut[- ]U[ée]l[ée]|Tshopo|Bas[- ]U[ée]l[ée]|Sud[- ]Ubangi|Nord[- ]Ubangi|Sud[- ]Kivu)\b", it, re.I)
        res.append((prov_name(m.group(1)) if m else None, it))
    return res


def _by_prov(sec_text):
    d = {}
    for pn, t in _bullets(sec_text):
        if pn:
            d.setdefault(pn, t)
    return d


def _parse_lab(S):
    sec = S["sections"].get("lab")
    if not sec:
        return None
    res = {}
    for pn, t in _by_prov(sec).items():
        s = p = None
        neg = re.search(r"(?:tous\s+(?:sont\s+)?|les\s+r[ée]sultats\s+(?:sont|se\s+sont)\s+)(?:revenus?|r[ée]v[ée]l[ée]s?)\s+n[ée]gatifs?", t)
        m = re.search(r"(\d+)\s+nouveaux?\s+r[ée]sultats?\s+positifs?", t)
        if m:
            p = int(m.group(1))
        m = re.search(r"(?:sur\s+)?(\d+)\s+(?:nouveaux\s+)?[ée]ch\w+", t)
        if m:
            s = int(m.group(1))
        if neg:
            p = 0
            m = re.search(r"(\d+)\s+(?:nouveaux\s+)?[ée]chantillons", t)
            if m:
                s = int(m.group(1))
        m = re.search(r"positivit[ée]\s*:\s*([\d,]+)\s*%", t)
        res[pn] = dict(samples=s, positives=p, positivity_stated=num(m.group(1)) if m else None)
    if S.get("samples") is not None and res:
        if all(v["samples"] is not None and v["positives"] is not None for v in res.values()):
            ss, pp = sum(v["samples"] for v in res.values()), sum(v["positives"] for v in res.values())
            if (ss, pp) != (S["samples"], S["positives"]):
                _warn(S, "laboratoire : somme des provinces (%s/%s) != faits saillants (%s/%s) -> detail par province ND" % (pp, ss, S["positives"], S["samples"]))
                return None
        else:
            _warn(S, "laboratoire : lecture incomplete d'au moins une province")
    return res


def _parse_care(S):
    sec = S["sections"].get("care")
    if not sec:
        return None
    # n'utiliser que les puces "Principales actions" (avant 'Défis')
    body = re.split(r"\d\.\d{1,2}\.\d{1,2}(?:\.\d{1,2})?\.?\s*D[ée]fis", sec)[0]
    res = {}
    for pn, t in _by_prov(body).items():
        d = {}
        m = re.search(r"(\d[\d ]*?)\s+nouvelles?\s+admissions?", t)
        d["admissions"] = num(m.group(1)) if m else None
        m = re.search(r"contre\s+(\d[\d ]*?)\s+sorties?", t)
        d["exits"] = num(m.group(1)) if m else None
        if d["exits"] is None and d["admissions"] is not None and re.search(r"aucune\s+sortie\s+n.a\s+[ée]t[ée]\s+rapport[ée]e|sans\s+aucune\s+sortie", t, re.I):
            d["exits"] = 0  # le SitRep l'ecrit explicitement : « Aucune sortie n'a ete rapportee »
        m = re.search(r"(?:dont|contre)\s+(\d+)\s+gu[ée]ri(?:sons?|s)\b", t) or re.search(r"\bsorties?,\s+(une)\s+gu[ée]rison", t)
        d["recovered"] = (1 if m.group(1) == "une" else int(m.group(1))) if m else (0 if re.search(r"aucune gu[ée]rison|sans\s+gu[ée]rison", t) else None)
        # SitRep <= 149 : « N patients demeuraient en isolement » ; SitRep 151 : « Ainsi, N patients (x confirmes et y suspects) sont hospitalises / restent en isolement »
        m = re.search(r"(\d[\d ]*?)\s+patients?(?:\s*\([^)]*\))?\s+(?:demeur(?:aient|ent)\s+en\s+isolement|sont\s+hospitalis[ée]s|restent\s+en\s+isolement|sont\s+pris\s+en\s+charge)", t)
        if not m:  # SitRep 161 (Bas-Uele) : « portant a 106 le nombre de patients en isolement »
            m = re.search(r"portant\s+[àa]\s+(\d[\d ]*?)\s+le\s+nombre\s+de\s+patients\s+en\s+isolement", t)
        d["isolation"] = num(m.group(1)) if m else None
        m = re.search(r"taux d.occupation\s+(?:global\s+)?(?:des\s+(?:lits|CTE/CT)\s+)?de\s+([\d,]+)\s*%", t)
        d["bed_pct"] = num(m.group(1)) if m else None
        m = re.search(r"dont\s+(\d+)\s+malades critiques", t)
        d["critical"] = int(m.group(1)) if m else None
        m = re.search(r"(\d+)\s+patients\s*\(([\d,]+)\s*%\)\s+sont pris en charge dans des ESS", t)
        d["outside_cte"] = (int(m.group(1)), num(m.group(2))) if m else None
        if d["outside_cte"] is None:  # SitRep 161 : « 138 patients sur 477 (28,9 %) sont pris en charge hors CTE/CT » (rubrique Defis du meme paragraphe province)
            m = re.search(r"(\d+)\s+patients\s+sur\s+\d+\s*\(([\d,]+)\s*%\)\s+sont\s+pris\s+en\s+charge\s+hors\s+CTE/CT", re.sub(r"\s+", " ", sec)) if pn == "Nord-Kivu" else None
            d["outside_cte"] = (int(m.group(1)), num(m.group(2))) if m else None
        # Bas-Uele (SitRep 151) : « aucun nouveau cas confirme n'a ete admis ; 11 nouveaux suspects ont ete admis ... contre 3 sorties »
        if d["admissions"] is None and pn == "Bas-Uélé":
            ms = re.search(r"(\d+)\s+nouveaux?\s+suspects?\s+ont\s+[ée]t[ée]\s+admis", t)
            if ms and re.search(r"aucun\s+nouveau\s+cas\s+confirm[ée]\s+n.a\s+[ée]t[ée]\s+admis", t):
                d["admissions"] = int(ms.group(1))
        # Bas-Uélé : "10 cas suspects sont isolés" (isolement de suspects en centres de transit)
        if d["isolation"] is None and pn == "Bas-Uélé":
            m = re.search(r"(\d+)\s+cas suspects sont isol[ée]s", t)
            d["isolation"] = int(m.group(1)) if m else None
            d["isolation_suspects_only"] = bool(m)
        res[pn] = d
    tot = S.get("isolation_total")
    if res and tot is not None:
        vals = [d["isolation"] for d in res.values()]
        if all(v is not None for v in vals):
            if sum(vals) != tot:
                _warn(S, "isolement : somme des provinces (%s) != bandeau (%s) -> detail ND" % (sum(vals), tot))
                for d in res.values():
                    d["isolation"] = None
        else:
            _warn(S, "isolement : au moins une province illisible")
    return res


def _pct_ratio(t, pat):
    m = re.search(pat, t)
    return m


def _parse_ipc(S):
    sec = S["sections"].get("pci")
    if not sec:
        return None
    body = re.split(r"\d\.\d{1,2}\.\d{1,2}(?:\.\d{1,2})?\.?\s*D[ée]fis", sec)[0]
    res = {}
    for pn, t in _by_prov(body).items():
        d = {}
        m = re.search(r"(\d+)\s+rings ont [ée]t[ée] ouverts sur\s+(\d+)\s*\(([\d,]+)\s*%\)", t)
        d["rings"] = (int(m.group(1)), int(m.group(2)), num(m.group(3))) if m else None
        m = re.search(r"(\d+)\s+m[ée]nages d[ée]contamin[ée]s sur\s+(\d+)\s*\(([\d,]+)\s*%\)", t)
        d["hh"] = (int(m.group(1)), int(m.group(2)), num(m.group(3))) if m else None
        m = re.search(r"Sur\s+(\d+)\s+alertes EDS.*?(\d+)\s+EDS ont [ée]t[ée] r[ée]alis[ée]s\s*\(([\d,]+)\s*%\)\s+et\s+(\d+)\s+corps swab[ée]s", t)
        d["eds"] = (int(m.group(2)), int(m.group(1)), num(m.group(3)), int(m.group(4))) if m else None
        m = re.search(r"(\d+)\s+ESS sur\s+(\d+)\s*\(100\s*%\)\s+(?:ont [ée]t[ée] )?d[ée]contamin[ée]es", t)
        d["ess"] = (int(m.group(1)), int(m.group(2))) if m else None
        m = re.search(r"(\d+)\s+ESS sur\s+(\d+)\s*\(100\s*%\)\s+et\s+(\d+)\s+m[ée]nages sur\s+(\d+)\s*\(([\d,]+)\s*%\)", t)
        d["tshopo_dec"] = (int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4)), num(m.group(5))) if m else None
        m = re.search(r"(\d+)\s+ESS sur\s+(\d+)\s*\(([\d,]+)\s*%\)\s+ont re[çc]u des kits PCI", t)
        d["kits"] = (int(m.group(1)), int(m.group(2)), num(m.group(3))) if m else None
        m = re.search(r"sur\s+(\d+)\s+corps attendus.*?,\s*(\d+)\s+ont [ée]t[ée] swab[ée]s\s*\(([\d,]+)\s*%\)", t)
        d["bodies_expected"] = (int(m.group(2)), int(m.group(1)), num(m.group(3))) if m else None
        res[pn] = d
    return res


def _parse_vacc(S):
    sec = S["sections"].get("vacc")
    if not sec:
        return None
    flat = re.sub(r"\s+", " ", sec)
    out = {}
    m = re.search(r"cumul provincial [àa]\s+(\d[\d ]*?)\s+personnes vaccin[ée]es sur une cible de\s+(\d[\d ]*?)\s*\(([\d,]+)\s*%\)", flat)
    if m:
        out["bas_uele"] = dict(vaccinated=num(m.group(1)), target=num(m.group(2)), pct=num(m.group(3)), zones=[])
        seg = flat[m.end(): m.end() + 250]
        for z, n_, p_ in re.findall(r"([A-ZÀ-Ý][A-Za-zÀ-ÿ\-]+)\s*\((\d[\d ]*?)\s*;\s*([\d,]+)\s*%\)", seg):
            out["bas_uele"]["zones"].append((z, num(n_), num(p_)))
    return out or None


def _parse_poe_text(S):
    sec = S["sections"].get("poe")
    return re.sub(r"\s+", " ", sec) if sec else None


# ----------------------------------------------------------------------------------------------
def quick_kpi(text):
    """(numero, date, cas_cumules, deces_cumules) lus de facon TOLERANTE (anciens formats inclus),
    pour reconstruire l'historique des cumuls ; None si illisible. N'alimente que les semaines epi."""
    text = _clean(text)
    m = re.search(r"SitRep\s*N[°º]?\s*(\d+)", text)
    d = re.search(r"Date d[eu] rapport\w*\s*:\s*(\d{1,2})\s+([A-Za-zéûô]+)\s+(\d{4})", text)
    if not (m and d):
        return None
    mo = [v for k, v in _MONTHS.items() if _fold(k) == _fold(d.group(2))]
    if not mo:
        return None
    head = text.split("\n")[:70]
    cases = deaths = None
    for i, l in enumerate(head):
        c = _cells(l)
        if len(c) >= 6 and all(re.fullmatch(r"[\d ,]+%?", x) for x in c[:6]):
            cases, deaths = num(c[0]), num(c[1])
            break
        if len(c) == 2 and all(re.fullmatch(r"\d[\d ]*", x) for x in c):
            nxt = " ".join(head[i + 1: i + 3])
            if re.search(r"\d+\s*,?\d*\s*%", nxt):
                cases, deaths = num(c[0]), num(c[1])
                break
    if cases is None:
        return None
    rd = _dt.date(int(d.group(3)), mo[0], int(d.group(1)))
    # meme arbitrage que parse_sitrep : en-tete J+1 mais identifiant et legende du Tableau 1 de J -> date = J
    mt_ = re.search(r"/\s*MVE[- ]?BDBV\s*/\s*(\d{2})/(\d{2})/(\d{4})", text)
    mc_ = re.search(r"Tableau\s*1\..{0,160}?\bau\s+(\d{1,2})\s+([A-Za-zéûô]+)\s+(\d{4})", re.sub(r"\s+", " ", text))
    if mt_ and mc_:
        try:
            fn_ = _dt.date(int(mt_.group(3)), int(mt_.group(2)), int(mt_.group(1)))
            mo_ = [v for k, v in _MONTHS.items() if _fold(k) == _fold(mc_.group(2))]
            if mo_ and fn_ != rd and _dt.date(int(mc_.group(3)), mo_[0], int(mc_.group(1))) == fn_:
                rd = fn_
        except ValueError:
            pass
    return int(m.group(1)), rd, cases, deaths
