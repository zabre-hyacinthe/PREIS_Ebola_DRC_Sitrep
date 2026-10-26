# -*- coding: utf-8 -*-
"""
Mise a jour AUTOMATIQUE du deck Africa CDC (« BDBV SitRep Dashboard ») a partir d'un SitRep RDC.

Principes (identiques a la mise a jour manuelle validee le 11/10/2026 pour le SitRep 146) :
  * seul le TEXTE des formes existantes est remplace (aucun changement de format, de forme ni d'image) ;
  * chaque chiffre vient du SitRep (tableaux 1-4, faits saillants, sections) ou d'un calcul explicite
    (pourcentages, variations, semaines epi, comparaison avec le SitRep precedent) ;
  * valeur absente du SitRep -> « ND » ; paragraphe narratif non regenerable -> ancien texte CONSERVE et
    SIGNALE (rapport de controle + corps du mail) ; jamais d'invention ;
  * les valeurs de la veille proviennent de l'etat (state.json), alimente par les SitRep deja traites ;
  * a la fin : controle de tracabilite (chaque nombre du deck est retrouve dans le SitRep, l'etat, la
    configuration ou un calcul enregistre) ; rapport CSV + JSON.
"""
import copy
import csv
import datetime as dt
import json
import os
import re
import sys

from pptx import Presentation

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sitrep_parse as sp  # noqa: E402

NARR_WHY = {}  # cle narrative -> raison du refus (renseigne par build_deck.py)
MONTHS = ["", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
NUMW = {1: "One", 2: "Two", 3: "Three", 4: "Four", 5: "Five", 6: "Six", 7: "Seven", 8: "Eight", 9: "Nine", 10: "Ten", 11: "Eleven", 12: "Twelve"}
TOP_PROV = 4  # « X reported none » : parmi les 4 provinces les plus touchees (cumul)


class DeckError(Exception):
    pass


# ---------------------------------------------------------------------------------------------
def rh(x, nd=1):
    """arrondi « moitie vers le haut » (6,25 -> 6,3) comme les SitRep ; le % de Python arrondit 6,25 a 6,2."""
    from decimal import Decimal, ROUND_HALF_UP
    return float(Decimal(repr(float(x))).quantize(Decimal(1).scaleb(-nd), rounding=ROUND_HALF_UP))


def _thou(v):
    """'138 855' (SitRep) -> '138,855' ; None si non numerique."""
    d = re.sub(r"\D", "", v or "")
    return "{:,}".format(int(d)) if d else None


def n0(x):
    return "{:,}".format(int(x))


def p1(x):
    return "%.1f%%" % x


def sg(x):
    """variation signee avec vrai signe moins (convention du deck)."""
    return ("+" if x > 0 else "−" if x < 0 else "") + "%.1f%%" % abs(x)


def sg0(x):
    return ("+" if x > 0 else "−" if x < 0 else "") + "%d%%" % round(abs(x))


def chg(a, b):
    return None if not b else rh(100.0 * (a - b) / b)


def share(a, b):
    raw = 100.0 * a / b
    s = "%.1f" % rh(raw)
    if s == "100.0" and a < b:
        s = "%.2f" % rh(raw, 2)
    return s


def dstr(d):
    return "%d %s %d" % (d.day, MONTHS[d.month], d.year)


def to_date(x):
    return x if isinstance(x, dt.date) else dt.date.fromisoformat(str(x)[:10])


def join_and(items):
    items = list(items)
    if len(items) <= 1:
        return "".join(items)
    return ", ".join(items[:-1]) + " and " + items[-1]


def jsonable(o):
    if isinstance(o, dict):
        return {str(k): jsonable(v) for k, v in o.items()}
    if isinstance(o, (list, tuple)):
        return [jsonable(v) for v in o]
    if isinstance(o, (dt.date, dt.datetime)):
        return o.isoformat()
    return o


def numtokens(text):
    """nombres d'un texte, normalises (espaces/virgules de milliers retires, virgule decimale -> point)."""
    t = text.replace(" ", " ").replace("\xa0", " ").replace("−", "-")
    t = re.sub(r"(?<=\d)[ ,](?=\d{3}(?!\d))", "", t)      # milliers : 8 787 / 8,787
    t = re.sub(r"(?<=\d),(?=\d)", ".", t)                  # decimale francaise
    out = set()
    for m in re.finditer(r"\d+(?:\.\d+)?", t):
        out.add(m.group(0))
        if "." in m.group(0):
            out.add(m.group(0).rstrip("0").rstrip("."))
    return out


# ---------------------------------------------------------------------------------------------
class Deck:
    def __init__(self, path):
        self.p = Presentation(path)
        self.log = []        # (slide, element, para, ancien, nouveau, remarque)
        self.retained = []   # (slide, element, raison)
        self.derived = set() # nombres calcules (tracabilite)
        self.dynamic = []    # (slide_no, shape_name, texte) pour le controle final

    def shp(self, slide_no, shape_id, nth=0):
        s = self.p.slides[slide_no - 1]
        c = [x for x in s.shapes if x.shape_id == shape_id]
        if not c:
            raise DeckError("diapo %d : forme %d introuvable (gabarit modifie ?)" % (slide_no, shape_id))
        return c[nth]

    def setp(self, slide_no, shape_id, new, nth=0, para=0, note=""):
        if new is None:
            self.retained.append((slide_no, "forme %d" % shape_id, note or "valeur indisponible : ancien texte conserve"))
            return
        self.nset = getattr(self, "nset", 0) + 1
        sh = self.shp(slide_no, shape_id, nth)
        pr = sh.text_frame.paragraphs[para]
        old = "".join(r.text for r in pr.runs)
        runs = pr.runs
        if isinstance(new, str):
            runs[-1].text = new
        else:
            if len(new) > len(runs):
                raise DeckError("diapo %d forme %d : %d segments pour %d runs" % (slide_no, shape_id, len(new), len(runs)))
            for r, t in zip(runs, new):
                r.text = t
            for r in runs[len(new):]:
                r.text = ""
        cur = "".join(r.text for r in pr.runs)
        self.log.append((slide_no, sh.name, para, old, cur, note))
        self.dynamic.append((slide_no, sh.name, cur))
        self.derived |= numtokens(cur) if False else set()

    def setall(self, slide_no, shape_id, texts, nth=0, note=""):
        for i, t in enumerate(texts):
            if t is None:
                continue
            self.setp(slide_no, shape_id, t, nth=nth, para=i, note=note)

    def cell(self, table, r, c, text):
        tc = table.cell(r, c)
        pr = tc.text_frame.paragraphs[0]
        old = tc.text
        pr.runs[0].text = text
        for x in pr.runs[1:]:
            x.text = ""
        return old

    def notes(self, slide_no, text):
        s = self.p.slides[slide_no - 1]
        if not s.has_notes_slide or s.notes_slide.notes_text_frame is None:
            return
        tf = s.notes_slide.notes_text_frame
        old = tf.text
        tf.text = text
        self.log.append((slide_no, "NOTES", 0, old[:90] + "…", text[:90] + "…", "notes du presentateur reecrites"))
        self.dynamic.append((slide_no, "NOTES", text))


# ---------------------------------------------------------------------------------------------
class Ctx:
    """valeurs courantes (S), veille (P), historique (H), configuration (C) et calculs derives."""

    def __init__(self, S, P, H, C):
        self.S, self.P, self.H, self.C = S, P, H, C
        self.D = S["report_date"]
        self.N = S["sitrep_no"]
        self.issue = self.N - 10
        self.notes_missing = []

    # --- semaines epi (lundi-dimanche) -----------------------------------------------------
    def cum(self, d):
        v = self.H.get(d.isoformat())
        return (v[0], v[1]) if v else None

    def week_info(self):
        D = self.D
        mon = D - dt.timedelta(days=D.weekday())
        out = {"epiweek": D.isocalendar()[1], "mon": mon}
        sun_before = mon - dt.timedelta(days=1)
        cur = self.cum(D) or (self.S["cases_cum"], self.S["deaths_cum"])
        base = self.cum(sun_before)
        out["so_far"] = None
        if base and D.weekday() != 6:
            out["so_far"] = dict(cases=cur[0] - base[0], deaths=cur[1] - base[1], days=D.weekday() + 1)
        # derniere semaine complete (se terminant un dimanche <= D)
        end = D if D.weekday() == 6 else sun_before
        start = end - dt.timedelta(days=6)
        before = start - dt.timedelta(days=1)
        a, b = self.cum(before), (cur if end == D else self.cum(end))
        out["last"] = None
        if a and b:
            out["last"] = dict(start=start, end=end, cases=b[0] - a[0], deaths=b[1] - a[1], epiweek=end.isocalendar()[1])
        out["prev"] = None
        pend = start - dt.timedelta(days=1)
        pstart = pend - dt.timedelta(days=6)
        pa, pb = self.cum(pstart - dt.timedelta(days=1)), self.cum(pend)
        if pa and pb:
            out["prev"] = dict(start=pstart, end=pend, cases=pb[0] - pa[0], deaths=pb[1] - pa[1])
        return out


def prov_order_new(S):
    return sorted([(p, v["new"]) for p, v in S["t1"].items() if v["new"] and v["new"] > 0], key=lambda x: -x[1])


def top_zones(S):
    """zones ayant le plus de nouveaux cas (toutes provinces), [(zone, n)] ; ex-aequo conserves."""
    t2 = S.get("t2")
    if not t2 or not t2["ok"]:
        return None
    items = [(z, v["new"], p) for p, zs in t2["zones"].items() for z, v in zs.items() if v.get("new")]
    if not items:
        return []
    mx = max(i[1] for i in items)
    return [(z, n, p) for z, n, p in items if n == mx]


# ---------------------------------------------------------------------------------------------
def build(S, P, H, C, template, out_path, narrative=None, prior_narr=None):
    """retourne (Deck, Ctx, week)."""
    cx = Ctx(S, P, H, C)
    dk = Deck(template)
    dk.prior_narr = prior_narr or {}
    wk = cx.week_info()
    N, D, issue = cx.N, cx.D, cx.issue
    PN = P["sitrep_no"]
    prev_ok = (PN == N - 1)
    if not prev_ok:
        raise DeckError("SitRep precedent N-1 (%d) indisponible dans l'etat (dernier : %d) : comparaisons jour/jour impossibles" % (N - 1, PN))
    pD = to_date(P["report_date"])
    mon = MONTHS[D.month]

    c, d = S["cases_cum"], S["deaths_cum"]
    pc, pd_ = P["cases_cum"], P["deaths_cum"]
    new, nd = S["new_cases"], S["new_deaths"]
    pnew, pnd = P["new_cases"], P["new_deaths"]
    comm, cte = S["new_comm_deaths"], S["new_cte_deaths"]
    pcomm, pcte = P["new_comm_deaths"], P["new_cte_deaths"]
    cfr, pcfr = S["nat_cfr_calc"], round(rh(100.0 * pd_ / pc), 1)
    rec, prec = S["recovered_cum"], P["recovered_cum"]
    iso, piso = S["isolation_total"], P["isolation_total"]
    hz, hzt, hzp = S["hz_affected"], S["hz_total"], S["hz_pct"]
    t3t = S.get("t3_total")
    ct = S.get("contacts")
    ug = C["uganda"]
    cases_c = chg(new, pnew)
    deaths_c = chg(nd, pnd)

    def word(a, b, up, down, same):
        return up if a > b else down if a < b else same

    # ---- mots de tendance (titres) ---------------------------------------------------------
    cw = "Ease to" if (cases_c is not None and cases_c <= -5) else "Rise to" if (cases_c is not None and cases_c >= 5) else "Hold at"
    pos, ppos = S.get("positivity"), None
    if P.get("samples"):
        ppos = round(rh(100.0 * P["positives"] / P["samples"]), 1)
    pw = None
    if pos is not None and ppos is not None:
        pw = "Rises to" if pos - ppos >= 0.5 else "Falls to" if pos - ppos <= -0.5 else "Holds at"
    dw = "Up to" if nd > pnd else "Down to" if nd < pnd else "Steady at"
    iw = word(iso, piso, "Rises", "Falls", "Holds")

    # =========================== diapo 1 : couverture ======================================
    dk.setp(1, 4, "SITUATION REPORT NO. %d" % issue, note="numero d'Issue = SitRep - 10")
    dk.setp(1, 9, "%02d %s %d" % (D.day, mon, D.year))

    # =========================== diapo 2 : synthese ========================================
    title2 = None
    if pw:
        title2 = ["Executive Summary: ", "Daily Cases %s %d; Positivity %s %.1f%%" % (cw, new, pw, pos)]
    else:
        title2 = ["Executive Summary: ", "Daily Cases %s %d; Positivity ND" % (cw, new)]
    dk.setp(2, 502, title2)
    ew = wk["epiweek"]
    dk.setp(2, 504, "Situation Report No. %d | As of %s · Epi-week %d" % (issue, dstr(D), ew))
    tot_c, tot_d = c + ug["cases"], d + ug["deaths"]
    dk.setp(2, 512, "The DRC accounts for %s%% of cases and %s%% of deaths. Uganda declared its outbreak over on 28 July 2026 after all 831 contacts completed 21-day follow-up." % (share(c, tot_c), share(d, tot_d)),
            note="part RDC recalculee ; phrase Ouganda conservee (hors SitRep)")
    sf = wk["so_far"]
    wl = wk["last"]
    if sf and wl:
        avg_sf, avg_l = sf["cases"] / float(sf["days"]), wl["cases"] / 7.0
        cmpw = ("below week %d’s %.1f: too early to call a decline" % (wl["epiweek"], avg_l)) if avg_sf < avg_l else \
               ("above week %d’s %.1f: no decline yet" % (wl["epiweek"], avg_l)) if avg_sf > avg_l else ("level with week %d’s %.1f" % (wl["epiweek"], avg_l))
        weektxt = " Week %d averages %.1f cases a day (%d in %d days), %s." % (ew, avg_sf, sf["cases"], sf["days"], cmpw)
    elif wl and D.weekday() == 6:
        weektxt = " Week %d totalled %d cases (%.1f a day)." % (ew, wl["cases"], wl["cases"] / 7.0)
    else:
        weektxt = ""
    pcd = "%s and %s" % (sg(cases_c), sg(deaths_c)) if (cases_c is not None and deaths_c is not None) else None
    traj = "%s added %d confirmed cases and %d deaths, versus %d and %d the previous day%s.%s" % (
        dstr(D)[: -5] if False else "%d %s" % (D.day, mon), new, nd, pnew, pnd, " (%s)" % pcd if pcd else "", weektxt)
    dk.setp(2, 515, traj)
    prov_list = prov_order_new(S)
    tz = top_zones(S)
    hzchg = "the count is unchanged" if hz == P["hz_affected"] else "the count is up from %d" % P["hz_affected"] if hz > P["hz_affected"] else "the count is down from %d" % P["hz_affected"]
    sp_from = join_and(["%s (%d)" % (p, n) for p, n in prov_list])
    led = ""
    if tz:
        led = ", led by " + join_and(["%s (%d)" % (z, n) for z, n, _ in sorted(tz, key=lambda x: 0)])
    nprov = S.get("n_provinces")
    provw = NUMW.get(nprov, str(nprov)) if nprov else None
    if provw:
        dk.setp(2, 518, "%s provinces and %d of %d health zones (%.1f%%) are affected; %s. New cases came from %s%s." % (provw, hz, hzt, hzp, hzchg, sp_from, led))
    else:
        dk.setp(2, 518, None, note="nombre de provinces touchees non lu")
    # detection
    if pos is not None and ppos is not None and t3t:
        val = t3t["val_alive"] + t3t["val_dead"]
        inv = t3t["investigated"]
        tr, trp = t3t["transferred"], rh(100.0 * t3t["transferred"] / inv) if inv else None
        ptr = P.get("t3_total")
        ptp = rh(100.0 * ptr["transferred"] / ptr["investigated"]) if ptr and ptr.get("investigated") else None
        sam_c = chg(S["samples"], P["samples"])
        invtxt = ("All %d validated suspects were investigated" % val) if inv == val else ("%d of %d validated suspects were investigated" % (inv, val))
        trtxt = ""
        if trp is not None:
            trtxt = "; %d (%.1f%%) were transferred%s" % (tr, trp, (", %s %.1f%%" % ("up from" if trp > ptp else "down from" if trp < ptp else "unchanged from", ptp)) if ptp is not None else "")
        dk.setp(2, 521, "Daily positivity %s to %.1f%% (%d/%d), from %.1f%% (%d/%d), as testing %s %.1f%%. %s%s." % (
            word(pos, ppos, "rose", "fell", "held"), pos, S["positives"], S["samples"], ppos, P["positives"], P["samples"],
            word(S["samples"], P["samples"], "rose", "fell", "held"), abs(sam_c), invtxt, trtxt))
    else:
        dk.setp(2, 521, None, note="positivite / alertes illisibles")
    # tracing
    trc = _tracing(cx)
    dk.setp(2, 524, trc["s2"] if trc else None, note="suivi des contacts illisible")
    # response
    if iso is not None:
        pr_note = "" if P.get("isolation_by_prov") else ", now with a provincial breakdown"
        rn = S.get("recovered_new")
        rtxt = ", and %d recovered" % rn if rn is not None else ""
        dk.setp(2, 527, "Isolation %s to %s patients (%s previously)%s%s. Community deaths account for %d of %d deaths (%s). Early referral remains a priority." % (
            word(iso, piso, "rose", "fell", "held at" if False else "held"), n0(iso), n0(piso), pr_note, rtxt, comm, nd, p1(rh(100.0 * comm / nd)) if nd else "ND"))
    wnote = _weeknote(cx, wk)
    dk.notes(2, "Executive summary, %s: %d new confirmed cases and %d deaths; cumulative %s cases and %s deaths (CFR %.1f%%). Positivity %s to %.1f%% on %s%% %s samples; contact follow-up %s to %.1f%%; isolation %s to %s." % (
        dstr(D), new, nd, n0(c), n0(d), cfr, word(pos or 0, ppos or 0, "rose", "fell", "held"), pos or 0,
        "%d" % round(abs(chg(S["samples"], P["samples"]))) if S.get("samples") else "ND", "fewer" if S["samples"] < P["samples"] else "more",
        word(ct["pct"], P["contacts"]["pct"], "rose", "fell", "held") if ct else "ND", ct["pct"] if ct else 0, word(iso, piso, "rose", "fell", "held"), n0(iso)) if (S.get("samples") and ct and P.get("contacts")) else "Executive summary, %s." % dstr(D))

    # =========================== diapo 3 : dernieres 24 h ===================================
    dk.setp(3, 502, "Changes in the Last 24 Hours: Cases %s %d, Deaths %s %d" % (cw, new, dw, nd))
    dk.setp(3, 504, "Day-on-day movements, %s to %s" % (("%d" % pD.day) if pD.month == D.month else "%d %s" % (pD.day, MONTHS[pD.month]), dstr(D)))
    cfr_t = ("CFR rose from %.1f%% to %.1f%%" % (pcfr, cfr)) if cfr > pcfr else ("CFR fell from %.1f%% to %.1f%%" % (pcfr, cfr)) if cfr < pcfr else ("CFR holds at %.1f%%" % cfr)
    dk.setp(3, 512, "Confirmed cases rose from %s to %s (+%d) and deaths from %s to %s (+%d). %s. Recoveries rose from %s to %s (+%d). This is outbreak day %d." % (
        n0(pc), n0(c), c - pc, n0(pd_), n0(d), d - pd_, cfr_t, n0(prec), n0(rec), rec - prec, N), note="'outbreak day' = numero du SitRep (convention du deck)")
    shares = join_and_each([(p, rh(100.0 * n / new)) for p, n in prov_list])
    if cases_c is not None and deaths_c is not None:
        dk.setp(3, 515, "New cases %s %.1f%% (%d to %d) while deaths %s %.1f%% (%d to %d). Community deaths %s from %d to %d; intra-CTE deaths %s from %d to %d. %s." % (
            word(new, pnew, "rose", "fell", "held"), abs(cases_c), pnew, new, word(nd, pnd, "rose", "fell", "held"), abs(deaths_c), pnd, nd,
            word(comm, pcomm, "rose", "fell", "held"), pcomm, comm, word(cte, pcte, "rose", "fell", "held"), pcte, cte, shares))
    else:
        dk.setp(3, 515, None, note="veille a zero : variation non calculable")
    # propagation
    pnewp = {p: v["new"] for p, v in P["t1"].items() if v.get("new") is not None}
    parts = []
    for p, n in prov_list:
        pn_ = pnewp.get(p)
        if pn_ is None:
            parts.append("%s %d" % (p, n))
        elif pn_ == 0:
            parts.append("%s %d (versus 0)" % (p, n))
        else:
            parts.append("%s %d (%s versus %d)" % (p, n, sg(chg(n, pn_)), pn_))
    top = sorted(S["t1"].items(), key=lambda kv: -kv[1]["cases"])[:TOP_PROV]
    zero = [p for p, v in top if v["new"] == 0]
    zt = ("; %s reported none" % join_and(zero)) if zero else ""
    first = parts[0].replace(" ", " added ", 1) if parts else ""
    if parts:
        lst = [parts[0].replace(" ", " added ", 1)] if False else None
    plist = []
    for i, ptxt in enumerate(parts):
        nm, rest = ptxt.split(" ", 1) if not ptxt.split(" ")[0] in ("Haut-Uélé", "Nord-Kivu", "Bas-Uélé", "Sud-Kivu") else (ptxt.split(" ", 1)[0], ptxt.split(" ", 1)[1])
        plist.append(ptxt)
    added = [("%s added %s" % (p_.split(" ", 1)[0], p_.split(" ", 1)[1])) if i == 0 else p_ for i, p_ in enumerate(plist)]
    hzs = ("Affected health zones are unchanged at %d of %d (%.1f%%)." if hz == P["hz_affected"] else "Affected health zones are now %d of %d (%.1f%%)." ) % (hz, hzt, hzp)
    if hz != P["hz_affected"]:
        hzs = "Affected health zones are now %d of %d (%.1f%%), %s from %d." % (hz, hzt, hzp, "up" if hz > P["hz_affected"] else "down", P["hz_affected"])
    dk.setp(3, 518, "%s %s%s." % (hzs, join_and(added), zt) if parts else None, note="aucune province avec nouveaux cas")
    if pos is not None and ppos is not None and t3t:
        dk.setp(3, 521, "Positivity %s from %.1f%% to %.1f%% as samples %s from %d to %d. %s alerts were received; %s validated suspects were investigated and %d transferred (%.1f%%%s)." % (
            word(pos, ppos, "rose", "fell", "held"), ppos, pos, word(S["samples"], P["samples"], "rose", "fell", "held"), P["samples"], S["samples"],
            n0(t3t["total"]), ("all %d" % val) if inv == val else "%d of %d" % (inv, val), tr, trp,
            (", %s %.1f%%" % ("up from" if trp > ptp else "down from" if trp < ptp else "unchanged from", ptp)) if ptp is not None else ""))
    else:
        dk.setp(3, 521, None, note="positivite / alertes illisibles")
    dk.setp(3, 524, trc["s3"] if trc else None, note="suivi des contacts illisible")
    isoprov = _iso_list(S)
    if iso is not None:
        dk.setp(3, 527, "Isolation counts %s from %s to %s (%s%d)%s %s" % (
            word(iso, piso, "rose", "fell", "held"), n0(piso), n0(iso), "+" if iso >= piso else "−", abs(iso - piso),
            (": " + isoprov + ".") if isoprov else ".", "SitRep %d gives no total bed capacity, so aggregate occupancy cannot be updated." % N), note="capacite totale de lits non publiee")

    # =========================== diapo 4 : totaux pays ======================================
    dk.setp(4, 3, ["DRC Accounts for ", share(c, tot_c), "% of Cases; Uganda Has Closed Out"])
    dk.setp(4, 9, n0(tot_c)); dk.setp(4, 10, "DRC %s · Uganda %d" % (n0(c), ug["cases"]))
    dk.setp(4, 13, n0(tot_d)); dk.setp(4, 14, "DRC %s · UG %d (overall CFR %.1f%%)" % (n0(d), ug["deaths"], rh(100.0 * tot_d / tot_c)))
    dk.setp(4, 17, n0(rec + ug["recoveries"])); dk.setp(4, 18, "DRC %s · Uganda %d" % (n0(rec), ug["recoveries"]))
    if ct:
        dk.setp(4, 21, "%.1f%%" % ct["pct"]); dk.setp(4, 22, "%s/%s" % (n0(ct["seen"]), n0(ct["listed"])))
    dk.setp(4, 33, ["The outbreak is mainly in the DRC, with %s%% of cases and %s%% of deaths, predominantly in " % (share(c, tot_c), share(d, tot_d)), "Ituri", " and with continued high transmission in Nord-Kivu"], para=0)
    dk.setp(4, 33, ["CFR\xa0remains high at %.1f%%; late presentation and access-to-care constraints remain important response\xa0concerns" % cfr], para=1)
    if ct:
        dk.setp(4, 2, "Historical graphic retained. Contact follow-up measures contacts seen among those listed, not completeness of contact identification. DRC: %s/%s = %.1f%%. Uganda and HCW totals retain their prior reporting cut-offs. Confirmed cases only; suspected and probable totals are not supplied." % (n0(ct["seen"]), n0(ct["listed"]), ct["pct"]))
    dk.notes(4, "Combined totals as of %s: %s confirmed cases (DRC %s, Uganda %d), %s deaths (DRC %s, Uganda %d), %s recoveries (DRC %s, Uganda %d). DRC CFR %.1f%%; combined CFR %.1f%%. Uganda and HCW figures are retained from prior reporting (not in SitRep %d)." % (
        dstr(D), n0(tot_c), n0(c), ug["cases"], n0(tot_d), n0(d), ug["deaths"], n0(rec + ug["recoveries"]), n0(rec), ug["recoveries"], cfr, rh(100.0 * tot_d / tot_c), N))

    # =========================== diapos 5 et 6 : 24 h =======================================
    rows5 = []
    for sn, ids in ((5, dict(asof=5, c=9, cc=10, d=13, dc=14, r=17, rc=18, big=20, bigt=21, foot=26)),
                    (6, dict(asof=5, c=24, cc=25, d=29, dc=30, r=33, rc=34, big=20, bigt=21, foot=26))):
        dk.setp(sn, ids["asof"], "AS OF %s" % dstr(D).upper())
        dk.setp(sn, ids["c"], "%d" % new); dk.setp(sn, ids["cc"], "Cumulative %s (DRC)" % n0(c))
        dk.setp(sn, ids["d"], "%d" % nd); dk.setp(sn, ids["dc"], "Cumulative %s" % n0(d))
        dk.setp(sn, ids["r"], "%d" % S["recovered_new"] if S.get("recovered_new") is not None else None, note="gueris du jour non lus")
        dk.setp(sn, ids["rc"], "Cumulative %s" % n0(rec))
        dk.setp(sn, ids["big"], "%d" % comm)
        dk.setp(sn, ids["bigt"], "of the %d deaths reported on %d %s occurred in the community; the other %d occurred inside treatment centres." % (nd, D.day, mon, cte))
        dk.setp(sn, ids["foot"], "%d %s update · map retains its 2 Oct data cut-off" % (D.day, MONTHS[D.month][:3] if D.month != 9 else "Sep"))
    t = dk.shp(5, 2).table
    deaths_by_prov = _deaths_by_prov(S)
    for i in range(1, len(t.rows)):
        name = t.cell(i, 0).text
        if name == "Total":
            vals = (new, nd)
        else:
            pn_ = sp.prov_name(name)
            if not pn_ or pn_ not in S["t1"] or deaths_by_prov is None:
                dk.retained.append((5, "tableau nouveaux cas, ligne %s" % name, "province absente"))
                continue
            vals = (S["t1"][pn_]["new"], deaths_by_prov.get(pn_, 0))
        ob, oc = dk.cell(t, i, 1, str(vals[0])), dk.cell(t, i, 2, str(vals[1]))
        dk.log.append((5, "New cases by province", i, "%s: %s / %s" % (name, ob, oc), "%s: %s / %s" % (name, vals[0], vals[1]), "ligne de tableau"))
    # alerte « risque epidemiologique »
    bz = _zone_bits(S)
    pcomm_pct = rh(100.0 * pcomm / pnd) if pnd else None
    dk.setp(5, 24, "Community deaths %s to %.1f%% (%d of %d), from %.1f%% (%d of %d) the previous day; intra-CTE deaths %s to %d.%s Early detection, prompt referral and safe burials remain priorities." % (
        "rise" if pcomm_pct is not None and rh(100.0 * comm / nd) > pcomm_pct else "ease" if pcomm_pct is not None and rh(100.0 * comm / nd) < pcomm_pct else "hold",
        rh(100.0 * comm / nd), comm, nd, pcomm_pct, pcomm, pnd, "rose" if cte > pcte else "fell" if cte < pcte else "held", cte, bz) if (nd and pnd) else None, para=0, note="deces du jour ou de la veille a zero")
    nkiso = (S.get("care") or {}).get("Nord-Kivu", {})
    lead = prov_list[0] if prov_list else None
    second = prov_list[1] if len(prov_list) > 1 else None
    if lead:
        t2s = "%s contributed %d of %d new cases (%.1f%%)%s." % (lead[0], lead[1], new, rh(100.0 * lead[1] / new), (" and %s %d" % (second[0], second[1])) if second else "")
        t2s += " Isolation %s to %s patients (from %s)" % (word(iso, piso, "rose", "fell", "held"), n0(iso), n0(piso))
        if nkiso.get("isolation") is not None and nkiso.get("bed_pct") is not None:
            t2s += "; Nord-Kivu holds %s at %.1f%% bed occupancy." % (n0(nkiso["isolation"]), nkiso["bed_pct"])
        else:
            t2s += "."
        dk.setp(5, 24, t2s, para=1)
    nh = S.get("new_hz")
    base = ("Affected health zones unchanged at %d of %d (%.1f%%);\xa0" % (hz, hzt, hzp)) if hz == P["hz_affected"] else ("Affected health zones now %d of %d (%.1f%%), %s from %d;\xa0" % (hz, hzt, hzp, "up" if hz > P["hz_affected"] else "down", P["hz_affected"]))
    if nh:
        dk.setp(5, 27, [base, " %s named as newly affected." % nh["zone"]], note="zone citee dans les faits saillants du SitRep")
    else:
        dk.setp(5, 27, [base.rstrip(";\xa0") + ".", ""])
    first5 = "Last 24 hours (%s): %d new confirmed cases, %d deaths (%d community, %d intra-CTE)%s." % (dstr(D), new, nd, comm, cte, ", %d recoveries" % S["recovered_new"] if S.get("recovered_new") is not None else "")
    dk.notes(5, first5 + " The map image keeps its 2 October cut-off.")
    dk.notes(6, first5 + " Historical graphics keep their original cut-offs.")

    # =========================== diapo 7 ====================================================
    dk.setp(7, 6, ["The ", "epicentre", ": %s confirmed cases across %d health zones in %s provinces. Daily cases %s to %d and positivity %s to %.1f%%, while %d community deaths and %.1f%% contact follow-up signal persistent gaps." % (
        n0(c), hz, (NUMW.get(nprov, str(nprov)).lower() if nprov else "affected"), "eased" if cw == "Ease to" else "rose" if cw == "Rise to" else "held at", new,
        "rose" if pw == "Rises to" else "fell" if pw == "Falls to" else "held at", pos, comm, ct["pct"])]) if (pw and ct and nprov) else dk.setp(7, 6, None, note="positivite/suivi illisibles")
    dk.notes(7, "Section: Democratic Republic of the Congo — the epicentre. %s cumulative confirmed cases across %d health zones in %s provinces as of %s." % (n0(c), hz, (NUMW.get(nprov, "").lower() if nprov else "affected"), dstr(D)))

    # =========================== diapo 8 : tableau de bord RDC ==============================
    dk.setp(8, 3, "Daily Cases %s %d, Deaths %s %d; Isolation %s to %s" % (
        "Ease to" if cw == "Ease to" else "Rise to" if cw == "Rise to" else "Hold at", new, "Rise to" if nd > pnd else "Fall to" if nd < pnd else "Hold at", nd, iw, n0(iso)))
    dk.setp(8, 10, "%d" % new); dk.setp(8, 11, "Cum. %s" % n0(c))
    dk.setp(8, 14, "%d" % nd); dk.setp(8, 15, "Cum. %s" % n0(d))
    dk.setp(8, 18, "%d" % S["recovered_new"] if S.get("recovered_new") is not None else None, note="gueris du jour non lus")
    dk.setp(8, 19, "Cum. %s" % n0(rec))
    dk.setp(8, 35, n0(c))
    order = [("Ituri", "Ituri"), ("Nord-Kivu", "NK"), ("Haut-Uélé", "Haut-Uélé"), ("Tshopo", "Tshopo"), ("Sud-Kivu", "SK"), ("Bas-Uélé", "Bas-Uélé"), ("Sud Ubangi", "Sud Ubangi"), ("Nord Ubangi", "Nord Ubangi")]
    dk.setp(8, 36, " · ".join("%s %s" % (lab, n0(S["t1"][p]["cases"])) for p, lab in order if p in S["t1"]))
    dk.setp(8, 39, n0(d)); dk.setp(8, 40, "CFR %.1f%% · %d HZ affected" % (cfr, hz))
    dk.setp(8, 43, n0(iso))
    ip = _iso_short(S)
    dk.setp(8, 44, ip, note="isolement par province non lisible" if ip is None else "")
    if ct:
        dk.setp(8, 47, "%.1f%%" % ct["pct"]); dk.setp(8, 48, "%s of %s" % (n0(ct["seen"]), n0(ct["listed"])))
    ptxt = "%d %s: %d confirmed cases versus %d the previous day (%s), and %d deaths versus %d (%s). Community deaths %s to %d. Affected health zones %s. %s" % (
        D.day, mon, new, pnew, sg0(cases_c) if cases_c is not None else "ND", nd, pnd, sg0(deaths_c) if deaths_c is not None else "ND",
        "rise" if comm > pcomm else "fall" if comm < pcomm else "hold", comm,
        ("unchanged at %d" % hz) if hz == P["hz_affected"] else ("now %d (was %d)" % (hz, P["hz_affected"])),
        ("Week %d so far: %d cases in %s." % (ew, sf["cases"], ("%s days" % NUMW.get(sf["days"], str(sf["days"])).lower()) if sf["days"] > 1 else "one day")) if sf else "")
    dk.setp(8, 51, ptxt.strip())
    # semaine complete
    if wl:
        # tuiles « derniere semaine complete » (ids 22.. selon le gabarit)
        _set_week_tiles(dk, wl, wk)
    if wl and wk["prev"]:
        cc_, dd_ = chg(wl["cases"], wk["prev"]["cases"]), chg(wl["deaths"], wk["prev"]["deaths"])
        dk.setp(8, 32, "Week %d (%s): %d cases and %d deaths, derived from cumulative SitRep totals, versus %d and %d in week %d (%s and %s). Latest day, %d %s: %d cases, %d deaths (%d community, %d intra-CTE)%s. Cumulative: %s cases, %s deaths, %s recoveries." % (
            wl["epiweek"], _rng(wl["start"], wl["end"], long=True), wl["cases"], wl["deaths"], wk["prev"]["cases"], wk["prev"]["deaths"], wl["epiweek"] - 1,
            sg(cc_), sg(dd_), D.day, mon, new, nd, comm, cte,
            (" and %d recoveries" % S["recovered_new"]) if S.get("recovered_new") is not None else "", n0(c), n0(d), n0(rec)))
    sfx = ("Week %d to date: %d cases in %s days (%.1f/day)." % (ew, sf["cases"], NUMW.get(sf["days"], str(sf["days"])).lower(), sf["cases"] / float(sf["days"]))) if sf else ""
    lastw = (" Last complete week remains week %d (%d cases, %d deaths)." % (wl["epiweek"], wl["cases"], wl["deaths"])) if wl else ""
    dk.notes(8, "DRC dashboard as of %s. Last 24 hours: %d new cases (%d the day before), %d deaths (%s %d), %s, affected zones %s at %d.%s %s Cumulative: %s cases, %s deaths, CFR %.1f%%. %s patients in isolation%s. Contact follow-up %.1f%% (%s of %s)." % (
        dstr(D), new, pnew, nd, "up from" if nd > pnd else "down from" if nd < pnd else "unchanged from", pnd,
        ("%d recoveries" % S["recovered_new"]) if S.get("recovered_new") is not None else "recoveries ND", "unchanged" if hz == P["hz_affected"] else "changed", hz, lastw, sfx,
        n0(c), n0(d), cfr, n0(iso), " (provincial split reported)" if isoprov else "", ct["pct"], n0(ct["seen"]), n0(ct["listed"])) if ct else "DRC dashboard as of %s." % dstr(D))
    dk.setp(8, 67, "Total Affected %d" % hz)  # le nom de la ZS (ex. Alimbongo) etait une information datee : retire, jamais conserve perime
    # tuile « nouvelles ZS » : conservee sauf nouvelle ZS enregistree (voir etat)
    if nh and hz != P["hz_affected"]:
        dk.retained.append((8, "tuile NEW HZ · LAST 7 DAYS / Total Affected", "nouvelle ZS : tuile a verifier manuellement (historique des ZS non suivi)"))
    else:
        dk.retained.append((8, "tuiles NEW HZ · LAST 7 DAYS, HCW UPDATE, Total Affected", "statiques : non fournies/non suivies par le SitRep"))

    # =========================== diapo 9 : provinces ========================================
    t = dk.shp(9, 148).table
    for i in range(1, len(t.rows)):
        name = t.cell(i, 0).text
        if name == "Total":
            r = ("%d/%d" % (hz, hzt), "%d" % new, n0(c), "%s (%.1f%%)" % (n0(d), cfr))
        else:
            pn_ = sp.prov_name(name.replace("DRC · ", ""))
            if not pn_ or pn_ not in S["t1"]:
                dk.retained.append((9, "tableau provinces, ligne %s" % name, "province absente du SitRep"))
                continue
            v = S["t1"][pn_]
            r = ("%d/%d" % (v["hz_aff"], v["hz_tot"]), "%d" % v["new"], n0(v["cases"]), "%s (%.1f%%)" % (n0(v["deaths"]), v["cfr"]))
        olds = [dk.cell(t, i, j + 1, r[j]) for j in range(4)]
        dk.log.append((9, "Table 147", i, name + ": " + " | ".join(olds), name + ": " + " | ".join(r), "ligne de tableau"))
    _slide9_text(dk, S, cx)
    dk.setp(9, 4, "%d %s update · historical graphics retain their original data cut-offs" % (D.day, mon[:3] if D.month != 9 else "Sep"))
    ituri, nk = S["t1"].get("Ituri"), S["t1"].get("Nord-Kivu")
    if ituri and nk:
        dk.notes(9, "Province table as of %s: Ituri %s cases (%.1f%%, CFR %.1f%%); Nord-Kivu %s (CFR %.1f%%, %s). %s reported new cases. National totals: %s cases, %s deaths (%.1f%%)." % (
            dstr(D), n0(ituri["cases"]), rh(100.0 * ituri["cases"] / c), ituri["cfr"], n0(nk["cases"]), nk["cfr"],
            "highest" if nk["cfr"] == max(v["cfr"] for v in S["t1"].values() if v["cases"] >= 20) else "not the highest",
            join_and(["%s (%d)" % (p, n) for p, n in prov_list]), n0(c), n0(d), cfr))

    # =========================== diapos 11-15 : provinces ===================================
    _province_slides(dk, cx, narrative)

    # =========================== diapo 16 : reponse quotidienne =============================
    _slide16(dk, cx, narrative, trc)
    # =========================== diapo 17 : lacunes =========================================
    _slide17(dk, cx, narrative, trc)

    # notes communes (source + reconciliation) sur les diapos qui en portent
    src = _src_note(cx)
    recon = _recon_note(cx, wk)
    for sn in (1, 2, 4, 5, 6, 7, 8, 9):
        try:
            s = dk.p.slides[sn - 1]
            if s.has_notes_slide and s.notes_slide.notes_text_frame is not None:
                first = s.notes_slide.notes_text_frame.text.split("\n")[0]
                if sn == 1:
                    first = "Situation report %d: data to %s (SitRep %d), published %s." % (issue, dstr(D), N, dstr(S["pub_date"]) if S.get("pub_date") else "ND")
                dk.notes(sn, first + "\n" + src + "\n" + recon)
        except Exception:
            pass
    # ---- diapositives « Weekly Trends » (tableaux des zones de sante), reperees par titre -----------
    import deck_tables
    deck_tables.update(dk, S, rh)
    import deck_nk
    deck_nk.update(dk, S, rh)
    dk.p.save(out_path)
    return dk, cx, wk


# ---------------------------------------------------------------------------------------------
def join_and_each(pairs):
    names = [p for p, _ in pairs]
    if not pairs:
        return "No province reported new cases"
    first = "%s contributed %.1f%% of new cases" % (pairs[0][0], pairs[0][1])
    rest = ["%s %.1f%%" % (p, v) for p, v in pairs[1:]]
    return first + ((", " + join_and(rest)) if rest else "") if len(rest) < 2 else first + ", " + ", ".join(rest[:-1]) + " and " + rest[-1]


def _rng(a, b, long=False):
    if long:
        if a.month == b.month:
            return "%d–%d %s" % (a.day, b.day, MONTHS[b.month])
        return "%d %s–%d %s" % (a.day, MONTHS[a.month], b.day, MONTHS[b.month])
    return "%d %s–%d %s" % (a.day, MONTHS[a.month][:3], b.day, MONTHS[b.month][:3])


def _set_week_tiles(dk, wl, wk):
    """tuiles « LAST COMPLETE WEEK » (diapo 8) : ids du gabarit."""
    dk.setp(8, 20, "LAST COMPLETE WEEK (%s)" % _rng(wl["start"], wl["end"]))
    dk.setp(8, 23, "%d" % wl["cases"]); dk.setp(8, 24, "%.1f cases/day average" % (wl["cases"] / 7.0))
    dk.setp(8, 27, "%d" % wl["deaths"]); dk.setp(8, 28, "%.1f deaths/day average" % (wl["deaths"] / 7.0))


def _tracing(cx):
    S, P = cx.S, cx.P
    ct, pct_ = S.get("contacts"), P.get("contacts")
    if ct and not ct.get("by_prov") and ct.get("by_prov_pct"):
        # SitRep 163+ : seuls les taux par province sont publies -> aucun effectif par province, aucune comparaison provinciale inventee
        bpp = ct["by_prov_pct"]
        low = min(bpp.items(), key=lambda kv: kv[1])
        thr = ct.get("threshold") or 95.0
        rel = "below" if ct["pct"] < thr else "above"
        pp = (pct_ or {}).get("pct") if pct_ else ct.get("prev_pct_stated")
        mv = (" (%s %.1f%%)" % ("down from" if ct["pct"] < pp else "up from" if ct["pct"] > pp else "unchanged from", pp)) if pp is not None else ""
        s2 = "%s of %s contacts were seen (%.1f%%)%s, %s the %d%% target. %s is lowest at %.1f%%." % (n0(ct["seen"]), n0(ct["listed"]), ct["pct"], mv, rel, thr, low[0], low[1])
        s3 = ("Follow-up %s from %.1f%% to %.1f%% (%s/%s contacts under follow-up). %s is lowest at %.1f%%; provincial contact counts are not published, so no provincial comparison is shown." % (
            "fell" if ct["pct"] < pp else "rose" if ct["pct"] > pp else "held", pp, ct["pct"], n0(ct["seen"]), n0(ct["listed"]), low[0], low[1])) if pp is not None else (
            "Follow-up stands at %.1f%% (%s/%s contacts under follow-up). %s is lowest at %.1f%%." % (ct["pct"], n0(ct["seen"]), n0(ct["listed"]), low[0], low[1]))
        return {"s2": s2, "s3": s3, "low": (low[0], {"pct": low[1]}), "bp": {k: {"pct": v} for k, v in bpp.items()}, "pct_only": True}
    if not ct or not ct.get("by_prov"):
        return None
    bp = ct["by_prov"]
    if not pct_:
        # SitRep precedent a couverture partielle : pas de comparaison (rien n'est invente)
        low = min(bp.items(), key=lambda kv: kv[1]["pct"])
        thr = ct.get("threshold") or 95.0
        rel = "below" if ct["pct"] < thr else "above"
        s2 = "%s of %s contacts were seen (%.1f%%), %s the %d%% target. %s is lowest at %.1f%%." % (n0(ct["seen"]), n0(ct["listed"]), ct["pct"], rel, thr, low[0], low[1]["pct"])
        s3 = "Follow-up stands at %.1f%% (%s/%s contacts under follow-up). %s is lowest at %.1f%%; previous-day coverage was partial, so no comparison is shown." % (
            ct["pct"], n0(ct["seen"]), n0(ct["listed"]), low[0], low[1]["pct"])
        return {"s2": s2, "s3": s3, "low": low, "bp": bp}
    pb = {k: v["pct"] for k, v in (pct_.get("by_prov") or {}).items()}
    low = min(bp.items(), key=lambda kv: kv[1]["pct"])
    plow = min(pb.items(), key=lambda kv: kv[1]) if pb else None
    thr = ct.get("threshold") or 95.0
    rel = "below" if ct["pct"] < thr else "above"
    mv = "down from" if ct["pct"] < pct_["pct"] else "up from" if ct["pct"] > pct_["pct"] else "unchanged from"
    s2 = "%s of %s contacts were seen (%.1f%%), %s %.1f%% and %s the %d%% target." % (n0(ct["seen"]), n0(ct["listed"]), ct["pct"], mv, pct_["pct"], rel, thr)
    second = ""
    if plow:
        if plow[0] == low[0]:
            s2 += " %s remains lowest at %.1f%%." % (low[0], low[1]["pct"])
        else:
            s2 += " %s is now lowest at %.1f%%;" % (low[0], low[1]["pct"])
            pp = bp.get(plow[0])
            if pp:
                s2 += " %s %s to %.1f%%." % (plow[0], "improved" if pp["pct"] > plow[1] else "declined" if pp["pct"] < plow[1] else "held at", pp["pct"])
    else:
        s2 += " %s is lowest at %.1f%%." % (low[0], low[1]["pct"])
    # carte diapo 3
    mvv = "fell" if ct["pct"] < pct_["pct"] else "rose" if ct["pct"] > pct_["pct"] else "held"
    s3 = "Follow-up %s from %.1f%% to %.1f%% (%s/%s) while contacts under follow-up %s from %s to %s." % (
        mvv, pct_["pct"], ct["pct"], n0(ct["seen"]), n0(ct["listed"]), "fell" if ct["listed"] < pct_["listed"] else "rose" if ct["listed"] > pct_["listed"] else "held", n0(pct_["listed"]), n0(ct["listed"]))
    if plow and plow[0] != low[0] and plow[0] in bp:
        s3 += " %s is now lowest at %.1f%% (from %.1f%%); %s %s from %.1f%% to %.1f%%." % (
            low[0], low[1]["pct"], pb.get(low[0], float("nan")), plow[0], "improved" if bp[plow[0]]["pct"] > plow[1] else "declined", plow[1], bp[plow[0]]["pct"])
    elif plow:
        s3 += " %s remains lowest at %.1f%% (from %.1f%%)." % (low[0], low[1]["pct"], plow[1])
    return {"s2": s2, "s3": s3, "low": low, "bp": bp}


def _iso_short(S):
    care = S.get("care") or {}
    lab = {"Ituri": "Ituri", "Nord-Kivu": "NK", "Haut-Uélé": "HU", "Tshopo": "Tshopo", "Sud-Kivu": "SK", "Bas-Uélé": "Bas-Uélé", "Sud Ubangi": "Sud Ubangi", "Nord Ubangi": "Nord Ubangi"}
    parts = []
    for p in sp.PROVINCES:
        d = care.get(p)
        if d and d.get("isolation") is not None:
            parts.append("%s %s" % (lab[p], n0(d["isolation"])))
    return " · ".join(parts) if parts and sum(int(x.rsplit(" ", 1)[1].replace(",", "")) for x in parts) == S["isolation_total"] else None


def _iso_list(S):
    care = S.get("care") or {}
    parts = []
    for p in sp.PROVINCES:
        d = care.get(p)
        if d and d.get("isolation") is not None:
            parts.append("%s %s" % (p, n0(d["isolation"])))
    return ", ".join(parts) if parts and sum(care[p]["isolation"] for p in care if care[p].get("isolation") is not None) == S["isolation_total"] else None


def _deaths_by_prov(S):
    t2 = S.get("t2")
    if not t2 or not t2["ok"]:
        return None
    out = {}
    for p, row in t2["prov"].items():
        out[p] = row.get("totdeaths") if row.get("totdeaths") is not None else 0
    if sum(out.values()) != S["new_deaths"]:
        return None
    return out


def _zone_bits(S):
    t2 = S.get("t2")
    if not t2 or not t2["ok"]:
        return ""
    items = [(z, v["new"], v["comm"] or 0, v.get("cases") or 0) for p, zs in t2["zones"].items() for z, v in zs.items() if v.get("new")]
    items.sort(key=lambda x: (-x[1], -x[3], x[0]))  # departage : cumul de cas (zone la plus touchee), puis nom
    if not items:
        return ""
    mx = items[0][1]
    lead = [i for i in items if i[1] == mx]
    nxt = [i for i in items if i[1] < mx][:1]
    txt = ""
    if len(lead) >= 2:
        names = join_and([i[0] for i in lead])
        txt = " %s each reported %d new cases (%s community deaths)" % (names, mx, join_and(["%d" % i[2] for i in lead]))
        if nxt:
            txt += ", %s %d cases and %d community death%s" % (nxt[0][0], nxt[0][1], nxt[0][2], "" if nxt[0][2] == 1 else "s")
        txt += "."
    else:
        i = lead[0]
        txt = " %s reported %d new cases and %d community death%s" % (i[0], i[1], i[2], "" if i[2] == 1 else "s")
        if nxt:
            txt += ", %s %d cases and %d" % (nxt[0][0], nxt[0][1], nxt[0][2])
        txt += "."
    return txt


def _slide9_text(dk, S, cx):
    t1, t2 = S["t1"], S.get("t2")
    c = S["cases_cum"]
    zones = t2["zones"] if t2 and t2["ok"] else {}

    def lead(p, k):
        zs = sorted([(z, v["cases"]) for z, v in zones.get(p, {}).items() if v.get("cases")], key=lambda x: -x[1])[:k]
        return ", ".join("%s %s" % (z, n0(n)) for z, n in zs) if zs else None
    paras = []
    it, nk, hu, ts = t1.get("Ituri"), t1.get("Nord-Kivu"), t1.get("Haut-Uélé"), t1.get("Tshopo")
    maxcfr = max(v["cfr"] for v in t1.values() if v["cases"] >= 20)
    L = {}
    L[0] = ("Ituri: %s cases (%.1f%%), CFR %.1f%%. %s." % (n0(it["cases"]), rh(100.0 * it["cases"] / c), it["cfr"], lead("Ituri", 5))) if it and lead("Ituri", 5) else None
    L[1] = ("Nord-Kivu: %s cases (%.1f%%), CFR %.1f%%%s. %s." % (n0(nk["cases"]), rh(100.0 * nk["cases"] / c), nk["cfr"], ", the highest provincial CFR" if nk["cfr"] == maxcfr else "", lead("Nord-Kivu", 3))) if nk and lead("Nord-Kivu", 3) else None
    L[2] = ("Haut-Uélé: %s cases, CFR %.1f%%. %s." % (n0(hu["cases"]), hu["cfr"], lead("Haut-Uélé", 3))) if hu and lead("Haut-Uélé", 3) else None
    cteT = (S.get("t2") or {"prov": {}})["prov"].get("Tshopo", {}).get("cte") if t2 else None
    tz = lead("Tshopo", 2)
    L[3] = ("Tshopo: %s cases, CFR %.1f%%%s. %s." % (n0(ts["cases"]), ts["cfr"], (" after a CTE death on %d %s" % (cx.D.day, MONTHS[cx.D.month])) if (cteT or 0) > 0 else "", tz)) if ts and tz else None
    zero = [p for p, v in t1.items() if v["new"] == 0]
    L[4] = ("%s reported no new cases on %d %s." % (join_and(zero), cx.D.day, MONTHS[cx.D.month])) if zero else None
    for i in range(5):
        if L.get(i) is None:
            dk.retained.append((9, "texte provinces, paragraphe %d" % (i + 1), "donnees de zone indisponibles ou non applicables : ancien texte conserve"))
            continue
        dk.setp(9, 137, L[i], para=i)


def _src_note(cx):
    S, P = cx.S, cx.P
    D = cx.D
    pub = dstr(S["pub_date"]) if S.get("pub_date") else "ND"
    return ("Source: SitRep N°%d/MVE-BDBV/%02d/%02d/%d (INSP/COUSP, report date %d %s, published %s). Day-on-day comparisons use the values published in SitRep %d (%d cases, %d deaths, %s / %s cumulative)." % (
        cx.N, D.day, D.month, D.year, D.day, MONTHS[D.month], pub, P["sitrep_no"], P["new_cases"], P["new_deaths"], n0(P["cases_cum"]), n0(P["deaths_cum"])))


def _weeknote(cx, wk):
    return ""


def _recon_note(cx, wk):
    S, P = cx.S, cx.P
    c, d, rec = S["cases_cum"], S["deaths_cum"], S["recovered_cum"]
    t2 = S.get("t2")
    zone_txt = ""
    if t2 and t2["ok"]:
        it, nk = S["t1"].get("Ituri"), S["t1"].get("Nord-Kivu")
        av = t2["zones"].get("Ituri", {}).get("A ventiler", {}).get("deaths")
        zone_txt = "Zone tables sum to provincial totals (Ituri %s / %s%s; Nord-Kivu %s / %s). " % (
            n0(it["cases"]), n0(it["deaths"]), (" incl. %d unallocated deaths" % av) if av else "", n0(nk["cases"]), n0(nk["deaths"]))
    elif t2:
        zone_txt = "Zone-table cross-check did not fully reconcile (see control report). "
    ct = S.get("contacts")
    ctt = ("Contacts %s/%s = %.1f%%. " % (n0(ct["seen"]), n0(ct["listed"]), ct["pct"])) if ct else ""
    post = ("Positivity %d/%d = %.1f%%. " % (S["positives"], S["samples"], S["positivity"])) if S.get("samples") else ""
    isot = _iso_list(S)
    isn = ("SitRep %d reports isolation by province (%s = %s; %s on %d %s) but no total bed capacity, so the aggregate occupancy shown previously (60.4%% of 1,764) cannot be updated. " % (
        cx.N, isot, n0(S["isolation_total"]), n0(P["isolation_total"]), to_date(P["report_date"]).day, MONTHS[to_date(P["report_date"]).month][:3])) if isot else ""
    wl, sf = wk.get("last"), wk.get("so_far")
    wtxt = ""
    if wl:
        wtxt = "Week %d (%s) = %d cases / %d deaths, derived from cumulative totals" % (wl["epiweek"], _rng(wl["start"], wl["end"]), wl["cases"], wl["deaths"])
        wtxt += ("; week %d to date (%s–%d %s) = %d cases (%s)" % (wk["epiweek"], "%d" % wk["mon"].day, cx.D.day, MONTHS[cx.D.month][:3], sf["cases"], " + ".join(str(x) for x in _daily_new(cx, wk)))) if sf and _daily_new(cx, wk) else ""
        wtxt += ". "
    ug = cx.C["uganda"]
    hc = cx.C["hcw"]
    return ("Reconciliation: %s + %d = %s cases; %s + %d = %s deaths (%d community, %d intra-CTE); recoveries %s + %d = %s. %s%s%s%s%sFigures not in SitRep %d (Uganda %d/%d/%d, HCW %d+%d infections / %d deaths) are retained from prior reporting. Historical charts, maps and analytical cohorts are not refreshed." % (
        n0(P["cases_cum"]), S["new_cases"], n0(c), n0(P["deaths_cum"]), S["new_deaths"], n0(d), S["new_comm_deaths"], S["new_cte_deaths"],
        n0(P["recovered_cum"]), S["recovered_new"] or 0, n0(rec), zone_txt, ctt, post, isn, wtxt, cx.N, ug["cases"], ug["deaths"], ug["recoveries"], hc["drc_infections"], hc["uganda_infections"], hc["deaths"]))


def _daily_new(cx, wk):
    out, d0 = [], wk["mon"]
    prev = cx.cum(d0 - dt.timedelta(days=1))
    d_ = d0
    while d_ <= cx.D and prev:
        cur = cx.cum(d_) or ((cx.S["cases_cum"], cx.S["deaths_cum"]) if d_ == cx.D else None)
        if not cur:
            return None
        out.append(cur[0] - prev[0])
        prev, d_ = cur, d_ + dt.timedelta(days=1)
    return out


# ---------------------------------------------------------------------------------------------
def _province_slides(dk, cx, narrative):
    S, P = cx.S, cx.P
    t1, t3, t4, care, lab, ipc, ct = S["t1"], S.get("t3") or {}, S.get("t4") or {}, S.get("care") or {}, S.get("lab") or {}, S.get("ipc") or {}, S.get("contacts") or S.get("contacts_partial") or {}
    cb = (ct.get("by_prov") or {})
    c = S["cases_cum"]
    zones = S["t2"]["zones"] if S.get("t2") and S["t2"]["ok"] else {}
    prov_list = prov_order_new(S)
    bp_new = S["new_cases"]
    t2p = S["t2"]["prov"] if S.get("t2") else {}

    def deaths_split(p):
        r = t2p.get(p) or {}
        return r.get("comm"), r.get("cte"), r.get("totdeaths")

    def alerts_txt(p, with_transferred=True):
        a = t3.get(p)
        if not a:
            return None
        val = a["val_alive"] + a["val_dead"]
        return a["total"], val, a["investigated"], a["transferred"]

    def tshare(p):
        return rh(100.0 * t1[p]["new"] / bp_new) if bp_new else 0.0

    def t4rows(p):
        col = t4.get(p)
        if not col:
            return None
        g = lambda k: sp.t4_value(t4, p, k)
        comp = sp.t4_ratio(g("completeness"))
        if not comp:
            return None
        return col, g, comp

    # ------------------ diapo 11 : Ituri ----------------------------------------------------
    p = "Ituri"
    v = t1[p]
    tz = [(z, zv["new"]) for z, zv in zones.get(p, {}).items() if zv.get("new")]
    tzmax = max(tz, key=lambda x: x[1]) if tz else None
    if tzmax and v["new"]:
        ties = [z for z, n in tz if n == tzmax[1]]
        if len(ties) == 1:
            dk.setp(11, 3, "Ituri remains the epicentre; %s leads with %d of %d new cases" % (tzmax[0], tzmax[1], v["new"]))
        else:
            dk.setp(11, 3, "Ituri remains the epicentre; %s lead with %d new cases each" % (join_and(ties), tzmax[1]))
    elif v["new"] == 0:
        dk.setp(11, 3, "Ituri: no new confirmed cases reported")
    dk.setp(11, 9, n0(v["cases"])); dk.setp(11, 11, "%.1f%% of national" % (rh(100.0 * v["cases"] / c))); dk.setp(11, 12, n0(v["deaths"]))
    dk.setp(11, 14, "%.1f%%" % v["cfr"]); dk.setp(11, 16, "%d / %d" % (v["hz_aff"], v["hz_tot"])); dk.setp(11, 18, "%.1f%% affected" % (rh(100.0 * v["hz_aff"] / v["hz_tot"])))
    _pulse(dk, 11, 21, 22, 26, 29, 32, "Ituri", S, t1, t3, care, lab, cb, t4, deaths_split, tshare, kind="full")
    _response(dk, 11, 8, "Ituri", ipc, S)

    # ------------------ diapo 12 : Haut-Uele ------------------------------------------------
    p = "Haut-Uélé"
    v = t1[p]
    zn = sorted([(z, zv["new"]) for z, zv in zones.get(p, {}).items() if zv.get("new")], key=lambda x: -x[1])
    cm, ctt, tt = deaths_split(p)
    rc = (care.get(p) or {}).get("recovered")
    if v["new"] > 0 and zn and tt is not None:
        dk.setp(12, 3, "%s: %d new case%s (%s); %d deaths, %s" % (p, v["new"], "" if v["new"] == 1 else "s", ", ".join("%s %d" % z for z in zn), tt, "no recoveries" if rc == 0 else ("%d recoveries" % rc) if rc is not None else "recoveries ND"))
    dk.setp(12, 9, n0(v["cases"])); dk.setp(12, 11, "%.1f%% of national" % (rh(100.0 * v["cases"] / c))); dk.setp(12, 12, n0(v["deaths"]))
    dk.setp(12, 14, "%.1f%%" % v["cfr"]); dk.setp(12, 16, "%d / %d" % (v["hz_aff"], v["hz_tot"])); dk.setp(12, 18, "%.1f%% affected" % (rh(100.0 * v["hz_aff"] / v["hz_tot"])))
    _pulse(dk, 12, 21, 22, 26, 29, 32, p, S, t1, t3, care, lab, cb, t4, deaths_split, tshare, kind="hu")
    _response(dk, 12, 8, p, ipc, S)

    # ------------------ diapo 13 : Tshopo ---------------------------------------------------
    p = "Tshopo"
    v = t1[p]
    cm, ctt, tt = deaths_split(p)
    att = bool(re.search(r"agressions? physiques? contre les [ée]quipes", " ".join(t for _, t in sp._bullets(S["sections"].get("sec", "")) if _ == p)))
    if tt is not None:
        dk.setp(13, 3, "Tshopo: %s; %s%s" % ("no new cases" if v["new"] == 0 else "%d new cases" % v["new"],
                                              ("%d CTE death" % ctt + ("s" if ctt != 1 else "")) if (ctt and not cm) else ("%d deaths" % tt),
                                              ("; attacks on response teams continue" if att else "")))
    dk.setp(13, 9, n0(v["cases"])); dk.setp(13, 11, "%.2f%% of national" % (rh(100.0 * v["cases"] / c, 2))); dk.setp(13, 12, n0(v["deaths"]))
    dk.setp(13, 14, "%.1f%%" % v["cfr"]); dk.setp(13, 16, "%d / %d" % (v["hz_aff"], v["hz_tot"])); dk.setp(13, 18, "%.1f%% affected" % (rh(100.0 * v["hz_aff"] / v["hz_tot"])))
    _pulse(dk, 13, 21, 22, 26, 29, 32, p, S, t1, t3, care, lab, cb, t4, deaths_split, tshare, kind="ts")
    _response(dk, 13, 8, p, ipc, S)

    # ------------------ diapo 14 : Bas-Uele -------------------------------------------------
    p = "Bas-Uélé"
    v = t1[p]
    cb_ = care.get(p) or {}
    vac = (S.get("vacc") or {}).get("bas_uele")
    susp = cb_.get("isolation")
    dk.setp(14, 3, "Bas-Uélé: %s; %s" % ("no new cases" if v["new"] == 0 else "%d new cases" % v["new"],
                                          (("%d suspects isolated in non-standard transit centres" if cb_.get("isolation_suspects_only") else "%d patients in isolation") % susp) if susp is not None else "isolation ND"))
    _tiles(dk, 14, v, c)
    _bas_uele(dk, S, vac)
    _pulse(dk, 14, 23, 24, 28, 31, 34, p, S, t1, t3, care, lab, cb, t4, deaths_split, tshare, kind="bu")

    # ------------------ diapo 15 : Sud-Ubangi -----------------------------------------------
    p = "Sud Ubangi"
    v = t1[p]
    a = t3.get(p)
    if a:
        dk.setp(15, 3, "Sud-Ubangi: %s; %d suspects investigated, %s" % ("no new cases" if v["new"] == 0 else "%d new cases" % v["new"], a["investigated"], "none transferred" if a["transferred"] == 0 else "%d transferred" % a["transferred"]))
    _tiles(dk, 15, v, c)
    _pulse(dk, 15, 23, 24, 28, 31, 34, p, S, t1, t3, care, lab, cb, t4, deaths_split, tshare, kind="su")


def _tiles(dk, sn, v, c):
    """tuiles chiffrees des diapos 14 et 15 (ids = numero de la forme + 1)."""
    dk.setp(sn, 11, n0(v["cases"])); dk.setp(sn, 13, "%.2f%% of national" % (rh(100.0 * v["cases"] / c, 2)))
    dk.setp(sn, 14, n0(v["deaths"])); dk.setp(sn, 16, "%.1f%%" % v["cfr"])
    dk.setp(sn, 18, "%d / %d" % (v["hz_aff"], v["hz_tot"])); dk.setp(sn, 20, "%.1f%% affected" % (rh(100.0 * v["hz_aff"] / v["hz_tot"])))


def _pulse(dk, sn, id_a, id_b, id_s, id_l, id_p, p, S, t1, t3, care, lab, cb, t4, deaths_split, tshare, kind):
    """bandeaux « 24-HOUR PULSE / SURVEILLANCE / LABORATORY & CARE / POINTS OF ENTRY » d'une province."""
    v = t1[p]
    cm, cte, tt = deaths_split(p)
    lb = lab.get(p) if (lab := S.get("lab")) else None
    cr = (S.get("care") or {}).get(p) or {}
    a = (S.get("t3") or {}).get(p)
    cbp = (S.get("contacts") or S.get("contacts_partial") or {}).get("by_prov") or {}
    ctp = cbp.get(p)
    if ctp is None:
        _pp = ((S.get("contacts") or {}).get("by_prov_pct") or {}).get(p)
        if _pp is not None:
            ctp = {"pct": _pp, "seen": None, "listed": None}  # taux seul (SitRep 163+)
    # ---- ligne 1 (id_a) : nouveaux cas / positifs / deces ----
    if kind == "full":
        if lb and lb["samples"] is not None and tt is not None:
            dk.setp(sn, id_a, "%d new cases · %d positives / %d samples · positivity %s" % (v["new"], lb["positives"], lb["samples"], ("%.1f%%" % (rh(100.0 * lb["positives"] / lb["samples"]))) if lb["samples"] else "ND"))
            rc = cr.get("recovered")
            dk.setp(sn, id_b, "%d deaths (%d community, %d CTE) · %s recovered · %.1f%% of new cases" % (tt, cm, cte, rc if rc is not None else "ND", tshare(p)) if (cm is not None and cte is not None) else None, note="deces communautaires/CTE non lus")
    elif kind == "hu":
        if lb and lb["samples"] is not None and tt is not None:
            dk.setp(sn, id_a, "%d new case%s · %d positive%s / %d samples · %d deaths" % (v["new"], "" if v["new"] == 1 else "s", lb["positives"], "" if lb["positives"] == 1 else "s", lb["samples"], tt))
            rc = cr.get("recovered")
            dk.setp(sn, id_b, "%d total deaths (%d community, %d CTE) · %s recoveries · %.1f%% of new cases" % (tt, cm, cte, rc if rc is not None else "ND", tshare(p)) if (cm is not None and cte is not None) else None)
    elif kind == "ts":
        if lb and lb["samples"] is not None and tt is not None:
            dk.setp(sn, id_a, "%d new cases · %d positives / %d samples · %d community deaths · %d CTE death%s" % (v["new"], lb["positives"], lb["samples"], cm, cte, "" if cte == 1 else "s") if (cm is not None and cte is not None) else None)
            rc = cr.get("recovered")
            dk.setp(sn, id_b, "%d total death%s · %s · %.1f%% of new cases" % (tt, "" if tt == 1 else "s", ("%d recoveries reported" % rc) if rc is not None else "0 recoveries reported", tshare(p)))
    elif kind == "bu":
        if lb and lb["samples"] is not None and tt is not None:
            dk.setp(sn, id_a, "%d new cases  •  %d new positives  •  %d community deaths  •  %d intra-CTE deaths" % (v["new"], lb["positives"], cm or 0, cte or 0))
            dk.setp(sn, id_b, "%d deaths · 0 recoveries reported · %.1f%% of national new cases" % (tt, tshare(p)) if cr.get("recovered") is None else "%d deaths · %d recoveries · %.1f%% of national new cases" % (tt, cr["recovered"], tshare(p)))
    elif kind == "su":
        dk.setp(sn, id_a, "%d new cases  •  %d new positives  •  %d community deaths  •  %d intra-CTE deaths" % (v["new"], 0, cm or 0, cte or 0) if tt is not None else None, note="positifs non publies pour cette province : ligne conservee") if False else None
        if tt is not None:
            dk.retained.append((sn, "ligne 24-HOUR PULSE (positifs)", "positifs de la province non publies : ND conserve"))
            dk.setp(sn, id_b, "%d total deaths  •  No recovery reported  •  %.1f%% of national new" % (tt, tshare(p)))
    # ---- surveillance ----
    if a and ctp:
        val = a["val_alive"] + a["val_dead"]
        if kind in ("full", "hu", "ts"):
            sep = " · "
            lab_seen = {"full": "contacts seen %s / %s", "hu": "seen %s/%s", "ts": "contacts seen %s/%s"}[kind]
            if ctp["seen"] is None:
                seen_txt = None
            else:
                seen_txt = lab_seen % ((n0(ctp["seen"]), n0(ctp["listed"])))
                if kind == "full":
                    seen_txt = "contacts seen %s / %s" % (n0(ctp["seen"]), n0(ctp["listed"]))
            dk.setp(sn, id_s, "Alerts %s · suspects %d · investigated %d · transferred %d · follow-up %.1f%%%s" % (n0(a["total"]), val, a["investigated"], a["transferred"], ctp["pct"], (" · " + seen_txt) if seen_txt else ""))
        else:
            dk.setall(sn, id_s, ["Alerts %s · suspects %d" % (n0(a["total"]), val), "Investigated %d (%s)" % (a["investigated"], "100%" if a["investigated"] == val else "%.0f%%" % (rh(100.0 * a["investigated"] / val))),
                                 "Transferred %d" % a["transferred"], "Follow-up %.1f%%" % ctp["pct"], ("Contacts seen %s/%s" % (n0(ctp["seen"]), n0(ctp["listed"]))) if ctp["seen"] is not None else "Contact counts not published"])
    else:
        dk.retained.append((sn, "bandeau SURVEILLANCE %s" % p, "alertes ou suivi illisibles"))
    # ---- laboratoire & soins ----
    if kind == "full":
        if lb and lb["samples"] is not None and cr.get("admissions") is not None and cr.get("exits") is not None and cr.get("isolation") is not None and cr.get("bed_pct") is not None:
            dk.setall(sn, id_l, ["Tests %d · positive %d" % (lb["samples"], lb["positives"]), "Positivity %.1f%%" % (rh(100.0 * lb["positives"] / lb["samples"])),
                                 "Admissions %d / exits %d" % (cr["admissions"], cr["exits"]), ("Recovered %d" % cr["recovered"]) if cr.get("recovered") is not None else "Recovered ND",
                                 "Isolation %d (beds %.1f%%)" % (cr["isolation"], cr["bed_pct"])])
        else:
            dk.retained.append((sn, "bandeau LABORATORY & CARE %s" % p, "donnees de laboratoire/prise en charge illisibles"))
    elif kind == "hu":
        if lb and lb["samples"] and cr.get("admissions") is not None and cr.get("isolation") is not None and cr.get("bed_pct") is not None:
            dk.setall(sn, id_l, ["Tests %d · positive %d (%.1f%%)" % (lb["samples"], lb["positives"], rh(100.0 * lb["positives"] / lb["samples"])),
                                 "Admissions %d / exits %s · recovered %s" % (cr["admissions"], cr["exits"] if cr.get("exits") is not None else "ND", cr["recovered"] if cr.get("recovered") is not None else "ND"),
                                 "Isolation %d (beds %.1f%%)" % (cr["isolation"], cr["bed_pct"])])
        else:
            dk.retained.append((sn, "bandeau LABORATORY & CARE %s" % p, "donnees illisibles"))
    elif kind == "ts":
        if lb and lb["samples"] is not None and cr.get("admissions") is not None and cr.get("isolation") is not None and cr.get("bed_pct") is not None:
            dk.setall(sn, id_l, ["Tests %d · positive %d" % (lb["samples"], lb["positives"]), "Positivity %.1f%%" % (rh(100.0 * lb["positives"] / lb["samples"])) if lb["samples"] else "Positivity ND",
                                 "Admissions %d / exits %d" % (cr["admissions"], cr["exits"]), "No recovery reported" if cr.get("recovered") is None else "Recovered %d" % cr["recovered"],
                                 "Isolation %d (beds %.1f%%)" % (cr["isolation"], cr["bed_pct"])])
        else:
            dk.retained.append((sn, "bandeau LABORATORY & CARE %s" % p, "donnees illisibles"))
    elif kind == "bu":
        if lb and lb["samples"] is not None and cr.get("isolation") is not None:
            dk.setall(sn, id_l, ["Tests %d · positive %d" % (lb["samples"], lb["positives"]), "Positivity %.1f%%" % (rh(100.0 * lb["positives"] / lb["samples"])) if lb["samples"] else "Positivity ND",
                                 "Admissions / exits not reported" if cr.get("admissions") is None else "Admissions %d / exits %d" % (cr["admissions"], cr["exits"]),
                                 "No recovery reported" if cr.get("recovered") is None else "Recovered %d" % cr["recovered"],
                                 "Isolation %d suspects (transit centres)" % cr["isolation"] if cr.get("isolation_suspects_only") else "Isolation %d" % cr["isolation"]])
        else:
            dk.retained.append((sn, "bandeau LABORATORY & CARE %s" % p, "donnees illisibles"))
    elif kind == "su":
        if not lb and not cr:
            dk.setall(sn, id_l, ["Samples / positivity ND", "Admissions / exits ND", "No recovery reported", "Beds ND", "Isolation count not reported"])
    # ---- PoE / PoC ----
    _poe(dk, sn, id_p, p, t4, kind, S)


def _poe(dk, sn, id_p, p, t4, kind, S):
    n_ret, n_set = len(dk.retained), getattr(dk, "nset", 0)
    _poe_inner(dk, sn, id_p, p, t4, kind, S)
    if len(dk.retained) == n_ret and getattr(dk, "nset", 0) == n_set:  # jamais de texte perime conserve en silence
        dk.retained.append((sn, "bandeau POINTS OF ENTRY %s" % p, "tableau 4 illisible pour cette province : ancien texte conserve"))


def _poe_inner(dk, sn, id_p, p, t4, kind, S):
    col = t4.get(p) if t4 else None
    g = (lambda k: sp.t4_value(t4, p, k)) if col else (lambda k: None)
    comp = sp.t4_ratio(g("completeness")) if col else None
    if (kind in ("full", "ts") or (kind == "hu" and col)) and comp:
        trv, scr, hw, sens = _thou(g("travellers")), sp.t4_pct(g("screened") or ""), sp.t4_pct(g("handwashing") or ""), sp.t4_pct(g("sensitised") or "")
        if None in (trv, scr, hw, sens) or g("alerts") is None or g("validated") is None:
            dk.retained.append((sn, "bandeau POINTS OF ENTRY %s" % p, "valeurs du tableau 4 incompletes"))
            return
        sens_t = "100%" if sens == 100.0 else "%.1f%%" % sens
        if kind == "full":
            dk.setall(sn, id_p, ["Reporting %.1f%% (%d/%d)" % (comp[0], comp[1], comp[2]), "Travellers %s · screened %.1f%%" % (trv, scr), "Handwashing %.1f%% · sensitised %s" % (hw, sens_t),
                                 "Alerts %s · validated %s" % (g("alerts"), g("validated")), "Bodies intercepted %s" % g("bodies")])
        else:
            fn = g("functional")
            dk.setall(sn, id_p, ["Reporting %.1f%% (%d/%d)" % (comp[0], comp[1], comp[2]), "Travellers %s · screened %.1f%%" % (trv, scr), "Handwashing %.1f%% · sensitised %s" % (hw, sens_t),
                                 "Alerts %s · validated %s" % (g("alerts"), g("validated")), ("Strategic PoC functional %s" % fn) if fn else "Strategic PoC functional ND"])
    elif kind == "hu":
        if col is None:
            ipc = (S.get("ipc") or {}).get("Haut-Uélé") or {}
            amb = re.search(r"(\d+) courses d.ambulance ont permis de transporter (\d+) patients", S["sections"].get("log", "") and re.sub(r"\s+", " ", S["sections"]["log"]) or "")
            pk = re.search(r"un corps intercept[ée] au PoC (\w+) \(ZS d.([A-Za-zÀ-ÿ\-]+)\) a [ée]t[ée] s[ée]curis[ée]", re.sub(r"\s+", " ", S["sections"].get("pci", "")))
            eds, ess = ipc.get("eds"), ipc.get("ess")
            dk.setall(sn, id_p, ["No PoE/PoC table in SitRep %d" % S["sitrep_no"],
                                 ("Body at PoC %s (%s) secured" % (pk.group(1), pk.group(2))) if pk else "Bodies intercepted ND",
                                 ("Ambulance runs %s (%s patients)" % (amb.group(1), amb.group(2))) if amb else "Ambulance runs ND",
                                 ("ESS decontaminated %d of %d" % ess) if ess else "ESS decontaminated ND",
                                 ("EDS done: %d of %d" % (eds[0], eds[1])) if eds else "EDS done: ND"])
    elif kind == "bu":
        txt = sp._parse_poe_text(S) or ""
        pcs = re.search(r"seuls\s+(\d+)\s+PoC sur\s+(\d+)\s+sites de notification sont fonctionnels\s*\(([\d,]+)\s*%\)", re.sub(r"\s+", " ", S["sections"].get("poe", "")))
        poe = re.search(r"aucun PoE n.est renforc[ée]\s*\((\d+)/(\d+)\)", re.sub(r"\s+", " ", S["sections"].get("poe", "")))
        cmp_ = re.search(r"compl[ée]tude des rapports n.est que de\s*([\d,]+)\s*%\s*\((\d+)/(\d+)\)", re.sub(r"\s+", " ", S["sections"].get("poe", "")))
        pnhf = re.search(r"services du PNHF ne sont pas op[ée]rationnels", re.sub(r"\s+", " ", S["sections"].get("poe", "")))
        if pcs and poe and cmp_:
            dk.setall(sn, id_p, ["PoC functional %s/%s (%.1f%%)" % (pcs.group(1), pcs.group(2), sp.num(pcs.group(3))), "PoE reinforced %s/%s" % (poe.group(1), poe.group(2)),
                                 "Completeness %.1f%% (%s/%s)" % (sp.num(cmp_.group(1)), cmp_.group(2), cmp_.group(3)),
                                 "PNHF services not operational" if pnhf else None, "Bodies intercepted ND"])
        else:
            dk.retained.append((sn, "bandeau POINTS OF ENTRY Bas-Uele", "phrase des defis PoE/PoC non retrouvee"))
    elif kind == "su":
        dk.setall(sn, id_p, ["Reporting completeness  ND"] if False else [None])
        dk.retained.append((sn, "bandeau POINTS OF ENTRY Sud-Ubangi", "province sans donnee PoE/PoC : ND conserves"))


def _response(dk, sn, sid, p, ipc, S):
    d = (ipc or {}).get(p) or {}
    if p == "Ituri":
        rg, hh, eds = d.get("rings"), d.get("hh"), d.get("eds")
        if rg and hh and eds:
            dk.setp(sn, sid, "Response: %d/%d IPC rings (%.1f%%); %d/%d households decontaminated (%.1f%%); %d/%d EDS (%.1f%%), %d bodies swabbed." % (rg[0], rg[1], rg[2], hh[0], hh[1], hh[2], eds[0], eds[1], eds[2], eds[3]))
        else:
            dk.retained.append((sn, "ligne Response %s" % p, "rings / menages / EDS non retrouves"))
    elif p == "Haut-Uélé":
        ess, eds = d.get("ess"), d.get("eds")
        if ess and eds:
            dk.setp(sn, sid, "Response: %d of %d facilities decontaminated; %d of %d EDS done (%.1f%%), %d bodies swabbed." % (ess[0], ess[1], eds[0], eds[1], eds[2], eds[3]))
        else:
            dk.retained.append((sn, "ligne Response %s" % p, "ESS / EDS non retrouves"))
    elif p == "Tshopo":
        td, bd, kt = d.get("tshopo_dec"), d.get("bodies_expected"), d.get("kits")
        if td and bd and kt:
            dk.setp(sn, sid, "Response: %d/%d facilities and %d/%d households decontaminated within 48 hours; %d of %d bodies swabbed (%.1f%%); %d of %d facilities received IPC kits." % (td[0], td[1], td[2], td[3], bd[0], bd[1], bd[2], kt[0], kt[1]))
        else:
            dk.retained.append((sn, "ligne Response %s" % p, "decontamination / corps / kits non retrouves"))


def _bas_uele(dk, S, vac):
    if vac and vac["zones"]:
        zt = ", ".join("%s %d" % (z, n) for z, n, _ in vac["zones"])
        trained = bool(re.search(r"formation.*PCI.*Viadana", re.sub(r"\s+", " ", S["sections"].get("pci", ""))))
        dk.setp(14, 10, "Response: %s of %s vaccinated (%.1f%%): %s%s." % (n0(vac["vaccinated"]), n0(vac["target"]), vac["pct"], zt, "; IPC training continued in Viadana" if trained else ""))
    else:
        if re.search(r"donn[ée]es\s+du\s+Nord-Kivu\s+et\s+du\s+Bas-Uélé\s+ne\s+sont\s+pas\s+parvenues", re.sub(r"\s+", " ", (S.get("sections") or {}).get("vacc", ""))):
            dk.setp(14, 10, "Response: vaccination data not received in SitRep %d." % S["sitrep_no"])  # le SitRep l'ecrit explicitement
        else:
            dk.retained.append((14, "ligne Response Bas-Uele", "vaccination non retrouvee"))


# ---------------------------------------------------------------------------------------------
def _slide16(dk, cx, narrative, trc):
    S, P = cx.S, cx.P
    t3, t3t, lab, care, ct, t4 = S.get("t3") or {}, S.get("t3_total"), S.get("lab") or {}, S.get("care") or {}, S.get("contacts") or {}, S.get("t4") or {}
    dk.setp(16, 3, "All %d Validated Suspect Alerts Investigated" % (t3t["val_alive"] + t3t["val_dead"]) if (t3t and t3t["investigated"] == t3t["val_alive"] + t3t["val_dead"]) else "%s of %s Validated Suspect Alerts Investigated" % (t3t["investigated"], t3t["val_alive"] + t3t["val_dead"]) if t3t else None)
    # coordination (narratif + vaccination Bas-Uele)
    _narr(dk, narrative, 16, 9, "coordination")
    if t3t:
        val = t3t["val_alive"] + t3t["val_dead"]
        order = sorted(t3.items(), key=lambda kv: -kv[1]["total"])
        lab_p = {"Sud Ubangi": "Sud-Ubangi", "Nord Ubangi": "Nord-Ubangi"}
        det = "; ".join("%s %s / %d / %d / %d" % (lab_p.get(p, p), n0(a["total"]), a["val_alive"] + a["val_dead"], a["investigated"], a["transferred"]) for p, a in order)
        t4g = t4.get("Global") or {}
        poe_txt = ""
        if t4g.get("alerts") is not None and t4g.get("validated") is not None:
            pa = [(p, int(t4[p]["alerts"])) for p in ("Ituri", "Nord-Kivu", "Tshopo", "Haut-Uélé", "Sud-Kivu", "Bas-Uélé") if p in t4 and t4[p].get("alerts") not in (None, "NA") and int(t4[p]["alerts"]) > 0]
            poe_txt = " PoE/PoC reported %s alerts (%s); %s suspects were validated and all investigated." % (t4g["alerts"], ", ".join("%d %s" % (n, p) for p, n in pa), t4g["validated"]) if pa else ""
            inv4 = t4g.get("invest4h")
            if inv4 and sp.t4_pct(inv4) != 100.0:
                poe_txt = poe_txt.replace(" and all investigated.", " (investigated within 4 h: %s)." % inv4)
        dk.setp(16, 12, "%s alerts were received and verified. %s validated suspect alerts were investigated (%s); %d were transferred (%.1f%%). Alerts / suspects / investigated / transferred: %s.%s" % (
            n0(t3t["total"]), ("All %d" % val) if t3t["investigated"] == val else "%d of %d" % (t3t["investigated"], val), "100%" if t3t["investigated"] == val else "%.1f%%" % (rh(100.0 * t3t["investigated"] / val)),
            t3t["transferred"], rh(100.0 * t3t["transferred"] / t3t["investigated"]), det, poe_txt))
    if ct and ct.get("by_prov") and trc:
        bp = trc["bp"]
        srt = sorted(bp.items(), key=lambda kv: kv[1]["pct"])
        low = srt[0]
        rest = ["%s %.1f%%" % (lab_p_(p), v["pct"]) for p, v in srt[1:]]
        thr = ct.get("threshold") or 95.0
        nonrep = ct.get("nonreporting_zs") or []
        nrt = ""
        if nonrep:
            nc = ct.get("nonreporting_contacts")
            nrt = " %s did not submit follow-up reports." % join_and([("%s (%s contacts)" % (nc[0], n0(nc[1]))) if (nc and z == nc[0]) else z for z in nonrep])
        reach = [p for p, v in bp.items() if v["pct"] >= thr]
        pc_ = P.get("contacts")
        mv = ("down from" if ct["pct"] < pc_["pct"] else "up from" if ct["pct"] > pc_["pct"] else "unchanged from") if pc_ else None
        dk.setp(16, 15, "%s of %s listed contacts were seen (%.1f%%)%s. %s is %slowest at %.1f%%, followed by %s. %s%s" % (
            n0(ct["seen"]), n0(ct["listed"]), ct["pct"], (", %s %.1f%%" % (mv, pc_["pct"])) if pc_ else " (previous-day coverage was partial, so no comparison is shown)", low[0], "now " if pc_ else "", low[1]["pct"], join_and(rest),
            ("No province reaches the %d%% target%s" % (thr, ";" if nrt else ".")) if not reach else ("%s reach%s the %d%% target%s" % (join_and(reach), "" if len(reach) > 1 else "es", thr, ";" if nrt else ".")), nrt.replace(" did not", " did not", 1) if not nrt else nrt.replace("", "", 1)))
    # laboratoire
    if lab and S.get("samples") is not None:
        parts = []
        for p in ("Ituri", "Nord-Kivu", "Haut-Uélé", "Tshopo", "Bas-Uélé"):
            if p in lab and lab[p]["samples"] is not None:
                l = lab[p]
                parts.append("%s: %d/%d (%.1f%%)" % (p, l["positives"], l["samples"], rh(100.0 * l["positives"] / l["samples"])) if p in ("Ituri", "Nord-Kivu") else "%s: %d/%d" % (p, l["positives"], l["samples"]))
        sc = chg(S["samples"], P["samples"])
        ppos = rh(100.0 * P["positives"] / P["samples"])
        dk.setp(16, 18, "%s samples were analysed and %d were positive (%.1f%%). %s. Testing %s %.1f%% and positivity %s from %.1f%%." % (
            n0(S["samples"]), S["positives"], S["positivity"], "; ".join(parts), "fell" if sc < 0 else "rose" if sc > 0 else "held", abs(sc),
            "rose" if S["positivity"] > ppos else "fell" if S["positivity"] < ppos else "held", ppos))
    else:
        dk.retained.append((16, "paragraphe LABORATORY", "laboratoire par province illisible"))
    # prise en charge
    isoprov = []
    for p in ("Ituri", "Nord-Kivu", "Haut-Uélé", "Tshopo"):
        d = care.get(p)
        if not d or d.get("isolation") is None or d.get("bed_pct") is None:
            isoprov = None
            break
        extra = ", %d critical" % d["critical"] if d.get("critical") is not None else ""
        isoprov.append("%s %s (%s%.1f%%%s)" % (p, n0(d["isolation"]), "beds " if p == "Ituri" else "", d["bed_pct"], extra))
    bu = care.get("Bas-Uélé") or {}
    rbp = S.get("recovered_by_prov")
    if isoprov and bu.get("isolation") is not None and rbp and S.get("recovered_new") is not None:
        isot = ", ".join(isoprov) + (" and %d suspects in Bas-Uélé non-standard transit centres" if bu.get("isolation_suspects_only") else " and %d patients in Bas-Uélé") % bu["isolation"]
        rec_t = " and ".join("%d in %s" % (n, p) for p, n in sorted(rbp.items(), key=lambda kv: -kv[1]))
        dk.setp(16, 21, "%s patients were in isolation, %s %s: %s. There were %d recoveries: %s; cumulative recoveries reach %s." % (
            n0(S["isolation_total"]), "down from" if S["isolation_total"] < P["isolation_total"] else "up from" if S["isolation_total"] > P["isolation_total"] else "unchanged from", n0(P["isolation_total"]), isot, S["recovered_new"], rec_t, n0(S["recovered_cum"])))
    else:
        dk.retained.append((16, "paragraphe CASE MANAGEMENT", "isolement/gueris par province illisibles"))
    _narr(dk, narrative, 16, 24, "logistics")


def lab_p_(p):
    return {"Sud Ubangi": "Sud-Ubangi", "Nord Ubangi": "Nord-Ubangi"}.get(p, p)


def _slide17(dk, cx, narrative, trc):
    S, P = cx.S, cx.P
    ct, t4, care = S.get("contacts") or {}, S.get("t4") or {}, S.get("care") or {}
    _narr(dk, narrative, 17, 10, "resistance")
    if trc and ct:
        bp = trc["bp"]
        srt = sorted(bp.items(), key=lambda kv: kv[1]["pct"])
        low = srt[0]
        top3 = [(p, v) for p, v in srt[1:] if p in ("Ituri", "Haut-Uélé", "Tshopo", "Nord-Kivu")][:3]
        bench = "below 85%% and %d%% benchmarks" % (ct.get("threshold") or 95.0) if ct["pct"] < 85 else "below the %d%% benchmark" % (ct.get("threshold") or 95.0)
        dk.setp(17, 18, "Follow-up is %.1f%% (%s/%s), %s. %s is weakest at %.1f%%; %s." % (ct["pct"], n0(ct["seen"]), n0(ct["listed"]), bench, low[0], low[1]["pct"], ", ".join("%s %.1f%%" % (p, v["pct"]) for p, v in top3)), para=0)
    _narr(dk, narrative, 17, 26, "hcw_ipc")
    g = t4.get("Global") or {}
    comp = sp.t4_ratio(g.get("completeness")) if g else None
    scr, hw = sp.t4_pct(g.get("screened") or ""), sp.t4_pct(g.get("handwashing") or "")
    if g.get("travellers") and comp and scr is not None and hw is not None:
        bu = bool(re.search(r"PoC de Butuhe ne fonctionne plus", re.sub(r"\s+", " ", S["sections"].get("poe", ""))))
        dk.setp(17, 34, "%s travellers: screening %.1f%%, handwashing %.1f%%. Completeness %.1f%% (%d/%d)%s." % (_thou(g["travellers"]), scr, hw, comp[0], comp[1], comp[2], "; Butuhe PoC remains closed" if bu else ""), para=0)
    nk = care.get("Nord-Kivu") or {}
    if care.get("Nord-Kivu") and nk.get("isolation") is not None and nk.get("bed_pct") is not None and nk.get("outside_cte"):
        dk.setp(17, 42, "%s patients in isolation, %s %s; Nord-Kivu %s at %.1f%% bed occupancy, with %d patients (%.1f%%) held outside CTE/CT." % (
            n0(S["isolation_total"]), "down from" if S["isolation_total"] < P["isolation_total"] else "up from" if S["isolation_total"] > P["isolation_total"] else "unchanged from", n0(P["isolation_total"]),
            n0(nk["isolation"]), nk["bed_pct"], nk["outside_cte"][0], nk["outside_cte"][1]), para=0)
    _narr(dk, narrative, 17, 50, "security")


def _narr(dk, narrative, sn, sid, key):
    """paragraphes narratifs : texte verifie fourni par narrative.py ; sinon dernier texte verifie connu (etat) ;
    sinon le texte du gabarit. Dans les deux derniers cas le paragraphe est SIGNALE comme conserve."""
    txt = (narrative or {}).get(key)
    if txt:
        dk.setp(sn, sid, txt, note="narratif genere et verifie (tous les nombres retrouves dans le SitRep)")
        return
    prior = getattr(dk, "prior_narr", {}).get(key)
    if prior:
        dk.setp(sn, sid, prior, note="dernier texte verifie (etat)")
    dk.retained.append((sn, "paragraphe narratif \u00ab %s \u00bb" % key, "NON rafraichi (%s) : texte du %s conserve" % (NARR_WHY.get(key) or NARR_WHY.get("*") or "texte propose non retenu", "dernier SitRep traite" if prior else "gabarit")))
