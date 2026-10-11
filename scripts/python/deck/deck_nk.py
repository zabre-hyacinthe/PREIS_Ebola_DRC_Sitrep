# -*- coding: utf-8 -*-
"""Diapositive PROVINCE Nord-Kivu (tuiles + pouls 24 h + 3 bandeaux). Les formes sont reperees par leur CONTENU et leur geometrie
(titre « Nord-Kivu: ... cases », etiquettes CASES/DEATHS/CFR/HEALTH ZONES, en-tetes SURVEILLANCE / LABORATORY & CARE /
POINTS OF ENTRY), jamais par un numero de forme : la diapositive peut etre deplacee ou re-dessinee sans casser la mise a jour.
Tout champ non retrouve ou non verifiable -> texte conserve et signale. Aucune valeur inventee."""
import re

PROV = "Nord-Kivu"
TITLE_RE = re.compile(r"^\s*Nord-Kivu:\s.*\bcases\b", re.I)


def n0(x):
    return "{:,}".format(int(x))


def find_slide(prs):
    for i, sl in enumerate(prs.slides, 1):
        texts = [sh.text_frame.text for sh in sl.shapes if sh.has_text_frame]
        if any(TITLE_RE.match(t) for t in texts) and any(t.strip().upper().startswith("24-HOUR PULSE") for t in texts):
            return i, sl
    return None, None


def _boxes(sl):
    return [sh for sh in sl.shapes if sh.has_text_frame and sh.text_frame.text.strip()]


def _by_text(boxes, pred):
    return [b for b in boxes if pred(b.text_frame.text.strip())]


def _below(boxes, hdr, tol=60000):
    """zone de texte la plus proche SOUS l'en-tete hdr, meme colonne (bord gauche proche)."""
    c = [b for b in boxes if b is not hdr and b.top > hdr.top and abs(b.left - hdr.left) <= tol and not re.match(r"^[A-Z &/]+$", b.text_frame.text.strip())]
    return min(c, key=lambda b: b.top) if c else None


def _above(boxes, lab, tol=80000):
    c = [b for b in boxes if b is not lab and b.top < lab.top and abs(b.left - lab.left) <= tol and re.match(r"^[\d,.\s/%]+$", b.text_frame.text.strip())]
    return max(c, key=lambda b: b.top) if c else None


def _put(dk, sn, sh, new, para=0, note=""):
    pr = sh.text_frame.paragraphs[para]
    runs = pr.runs
    old = "".join(r.text for r in runs)
    if not runs:
        dk.retained.append((sn, "forme %s" % sh.name, "aucun texte de reference"))
        return
    runs[0].text = new
    for r in runs[1:]:
        r.text = ""
    dk.log.append((sn, sh.name, para, old, new, note))
    dk.dynamic.append((sn, sh.name, new))


def _put_lines(dk, sn, sh, lines, label):
    paras = sh.text_frame.paragraphs
    if len(paras) < len(lines):
        dk.retained.append((sn, label, "structure du bandeau differente (%d paragraphes pour %d lignes) : conserve" % (len(paras), len(lines))))
        return
    for i, t in enumerate(lines):
        _put(dk, sn, sh, t, para=i)
    for j in range(len(lines), len(paras)):
        old = "".join(r.text for r in paras[j].runs)
        if old:
            dk.retained.append((sn, label, "ligne %d conservee : %r" % (j + 1, old[:40])))


def texts(S, rh):
    """tous les textes de la diapositive, calcules depuis le SitRep ; None quand un champ n'est pas verifiable."""
    t1 = S["t1"][PROV]
    c = S["cases_cum"]
    out, why = {}, {}
    cfrs = {p: v["cfr"] for p, v in S["t1"].items() if v.get("cfr") is not None}
    ct = (S.get("contacts") or {}).get("by_prov") or {}
    ctp = ct.get(PROV)
    hi = " highest CFR" if cfrs and t1["cfr"] >= max(cfrs.values()) else ""
    if ctp:
        lowest = ctp["pct"] <= min(v["pct"] for v in ct.values())
        fu = ("lowest contact follow-up at %.1f%%" if lowest else "contact follow-up at %.1f%%") % ctp["pct"]
        out["title"] = "Nord-Kivu: %s cases;%s, %s" % (n0(t1["cases"]), hi, fu) if hi else "Nord-Kivu: %s cases; %s" % (n0(t1["cases"]), fu)
    else:
        why["title"] = "suivi des contacts illisible"
    out.update(cases=n0(t1["cases"]), deaths=n0(t1["deaths"]), cfr="%.1f%%" % t1["cfr"], hz="%d / %d" % (t1["hz_aff"], t1["hz_tot"]),
               share="%.1f%% of national" % rh(100.0 * t1["cases"] / c), aff="%.1f%% affected" % rh(100.0 * t1["hz_aff"] / t1["hz_tot"]))
    # pouls 24 h
    t2p = (S.get("t2") or {}).get("prov", {}).get(PROV) or {}
    lb = (S.get("lab") or {}).get(PROV)
    cr = (S.get("care") or {}).get(PROV) or {}
    cm, cte, tt = t2p.get("comm"), t2p.get("cte"), t2p.get("totdeaths")
    share = rh(100.0 * t1["new"] / S["new_cases"]) if S["new_cases"] else 0.0
    if lb and lb.get("samples"):
        out["pulse1"] = "%d new cases · %d positives / %d samples · positivity %.1f%%" % (t1["new"], lb["positives"], lb["samples"], rh(100.0 * lb["positives"] / lb["samples"]))
    else:
        why["pulse1"] = "laboratoire illisible"
    if None not in (cm, cte, tt) and cr.get("recovered") is not None:
        out["pulse2"] = "%d deaths (%d community, %d CTE) · %d recovered · %.1f%% of new cases" % (tt, cm, cte, cr["recovered"], share)
    else:
        why["pulse2"] = "deces communautaires/CTE ou guerisons illisibles"
    # zones les plus actives (groupes par nombre de cas, jusqu'a 3 zones)
    zs = [(z, d["new"], d["cases"]) for z, d in ((S.get("t2") or {}).get("zones", {}).get(PROV) or {}).items() if d.get("new")]
    if zs:
        groups = {}
        for z, n, cc in zs:
            groups.setdefault(n, []).append((cc, z))
        parts, shown = [], 0
        for n in sorted(groups, reverse=True):
            names = [z for cc, z in sorted(groups[n], key=lambda x: (-x[0], x[1]))]
            parts.append(("%s %d" % (names[0], n)) if len(names) == 1 else "%s and %s %d each" % (", ".join(names[:-1]), names[-1], n))
            shown += len(names)
            if shown >= 3:
                break
        out["hot"] = "Hot zones: %s." % "; ".join(parts)
    else:
        why["hot"] = "aucune zone avec nouveaux cas lue"
    # response (rings, ecart, PoC Butuhe)
    ipc = ((S.get("ipc") or {}).get(PROV) or {}).get("rings")
    if ipc:
        sec = re.sub(r"\s+", " ", S["sections"].get("pci", "") or "")
        gap = re.search(re.escape(PROV) + r"\s*:\s*\d+ rings ont [ée]t[ée] ouverts sur \d+ \([\d,]+ ?%\), avec un gap important [àa] ([A-Za-zÀ-ÿ\-]+) et [àa] ([A-Za-zÀ-ÿ\-]+)", sec)
        poe = re.sub(r"\s+", " ", S["sections"].get("poe", "") or "")
        but = re.search(r"le PoC de Butuhe ne fonctionne plus depuis l.incident s[ée]curitaire du (\d+) (septembre|octobre)", poe)
        r = "Response: %d/%d IPC rings opened (%.1f%%)" % (ipc[0], ipc[1], ipc[2])
        if gap:
            r += ", with a major gap in %s and %s" % (gap.group(1), gap.group(2))
        if but:
            r += "; Butuhe PoC still closed after the %s %s security incident" % (but.group(1), "Sep" if but.group(2) == "septembre" else "Oct")
        out["response"] = r + "."
    else:
        why["response"] = "anneaux IPC non retrouves"
    # bandeaux
    a = (S.get("t3") or {}).get(PROV)
    if a and ctp:
        val = a["val_alive"] + a["val_dead"]
        out["surv"] = "Alerts %s · suspects %d · investigated %d · transferred %d · follow-up %.1f%% · contacts seen %s / %s" % (n0(a["total"]), val, a["investigated"], a["transferred"], ctp["pct"], n0(ctp["seen"]), n0(ctp["listed"]))
    else:
        why["surv"] = "alertes ou suivi illisibles"
    if lb and lb.get("samples") and None not in (cr.get("admissions"), cr.get("exits"), cr.get("isolation"), cr.get("bed_pct")) and cr.get("recovered") is not None:
        out["lab"] = ["Tests %d · positive %d" % (lb["samples"], lb["positives"]), "Positivity %.1f%%" % rh(100.0 * lb["positives"] / lb["samples"]),
                      "Admissions %d / exits %d" % (cr["admissions"], cr["exits"]), "Recovered %d" % cr["recovered"], "Isolation %d (beds %.1f%%)" % (cr["isolation"], cr["bed_pct"])]
    else:
        why["lab"] = "laboratoire / prise en charge illisibles"
    import sitrep_parse as sp
    t4 = S.get("t4") or {}
    col = t4.get(PROV)
    comp = sp.t4_ratio(sp.t4_value(t4, PROV, "completeness")) if col else None
    g = (lambda k: sp.t4_value(t4, PROV, k)) if col else (lambda k: None)
    trv, scr, hw, sens = ("{:,}".format(int(re.sub(r"\D", "", g("travellers") or "0") or 0)) if g("travellers") else None, sp.t4_pct(g("screened") or ""), sp.t4_pct(g("handwashing") or ""), sp.t4_pct(g("sensitised") or "")) if comp else (None,) * 4
    if comp and None not in (trv, scr, hw, sens) and g("alerts") is not None and g("validated") is not None and g("bodies") is not None:
        hws = "Handwashing/sensitised %.1f%%" % hw if hw == sens else "Handwashing %.1f%% · sensitised %.1f%%" % (hw, sens)
        out["poe"] = ["Reporting %.1f%% (%d/%d)" % (comp[0], comp[1], comp[2]), "Travellers %s · screened %.1f%%" % (trv, scr), hws,
                      "Alerts %s · validated %s" % (g("alerts"), g("validated")), "Bodies intercepted %s" % g("bodies")]
    else:
        why["poe"] = "valeurs du tableau 4 incompletes"
    return out, why


def update(dk, S, rh):
    sn, sl = find_slide(dk.p)
    if sl is None:
        return False
    try:
        T, why = texts(S, rh)
    except Exception as e:  # lecture incomplete -> on ne touche a rien
        dk.retained.append((sn, "diapositive Nord-Kivu", "NON mise a jour : %s" % e))
        return False
    bx = _boxes(sl)
    for k, r in why.items():
        dk.retained.append((sn, "Nord-Kivu : %s" % k, "NON rafraichi : %s" % r))

    def one(key, box, label, para=0):
        if key not in T:
            return
        if box is None:
            dk.retained.append((sn, "Nord-Kivu : %s" % label, "forme non retrouvee : conserve"))
            return
        _put(dk, sn, box, T[key], para)

    one("title", next(iter(_by_text(bx, lambda t: bool(TITLE_RE.match(t)))), None), "titre")
    for lab, kk in (("CASES", "cases"), ("DEATHS", "deaths"), ("CFR", "cfr"), ("HEALTH ZONES", "hz")):
        lb_ = next(iter(_by_text(bx, lambda t, L=lab: t.upper() == L)), None)
        one(kk, _above(bx, lb_) if lb_ else None, "tuile " + lab)
    one("share", next(iter(_by_text(bx, lambda t: bool(re.match(r"^[\d.]+% of national$", t)))), None), "part nationale")
    one("aff", next(iter(_by_text(bx, lambda t: bool(re.match(r"^[\d.]+% affected$", t)))), None), "zones affectees")
    one("pulse1", next(iter(_by_text(bx, lambda t: bool(re.match(r"^\d+ new cases ·.*positives\s*/\s*\d+ samples", t)))), None), "pouls ligne 1")
    one("pulse2", next(iter(_by_text(bx, lambda t: bool(re.match(r"^\d+ deaths \(.*community.*CTE\)", t)))), None), "pouls ligne 2")
    one("hot", next(iter(_by_text(bx, lambda t: t.startswith("Hot zones"))), None), "zones actives")
    one("response", next(iter(_by_text(bx, lambda t: t.startswith("Response:"))), None), "ligne Response")
    for hdr_txt, key, label in (("SURVEILLANCE", "surv", "SURVEILLANCE"), ("LABORATORY & CARE", "lab", "LABORATORY & CARE"), ("POINTS OF ENTRY / CONTROL", "poe", "POINTS OF ENTRY")):
        if key not in T:
            continue
        hd = next(iter(_by_text(bx, lambda t, H=hdr_txt: t.upper() == H)), None)
        body = _below(bx, hd) if hd else None
        if body is None:
            dk.retained.append((sn, "Nord-Kivu : bandeau %s" % label, "forme non retrouvee : conserve"))
        elif isinstance(T[key], list):
            _put_lines(dk, sn, body, T[key], "Nord-Kivu : bandeau %s" % label)
        else:
            _put(dk, sn, body, T[key])
    return True


def verify(prs, S, rh):
    """relecture independante : chaque valeur cle lue sur la diapositive enregistree doit egaler le SitRep (si elle a ete mise a jour)."""
    sn, sl = find_slide(prs)
    if sl is None:
        return 0, []
    alltext = "\n".join(sh.text_frame.text for sh in sl.shapes if sh.has_text_frame)
    t1 = S["t1"][PROV]
    if ("%s cases" % n0(t1["cases"])) not in alltext.split("\n")[0] + alltext and n0(t1["cases"]) not in alltext:
        return 0, []  # non mise a jour (lecture refusee) : le controle global signale deja les conserves
    ok, fails = 0, []
    for label, frag in (("cas", n0(t1["cases"])), ("deces", n0(t1["deaths"])), ("CFR", "%.1f%%" % t1["cfr"]), ("zones", "%d / %d" % (t1["hz_aff"], t1["hz_tot"])),
                        ("nouveaux cas", "%d new cases" % t1["new"])):
        if frag in alltext:
            ok += 1
        else:
            fails.append("Nord-Kivu diapo %d : %s %r absent" % (sn, label, frag))
    ctp = ((S.get("contacts") or {}).get("by_prov") or {}).get(PROV)
    if ctp and ("%.1f%%" % ctp["pct"]) not in alltext:
        fails.append("Nord-Kivu diapo %d : suivi %.1f%% absent" % (sn, ctp["pct"]))
    elif ctp:
        ok += 1
    stale = [m for m in re.findall(r"\b\d[\d,]*\b", alltext) if False]
    return ok, fails
