# -*- coding: utf-8 -*-
"""
Couche narrative OPTIONNELLE du deck. Rafraichit les 5 paragraphes de texte libre (diapos 16-17) a partir du
SitRep du jour. Le modele ne produit JAMAIS un chiffre qui ne soit pas dans le SitRep : chaque texte propose
est verifie (nombres, noms propres, longueur) ; en cas de doute le paragraphe est refuse et build_deck.py garde
le dernier texte verifie, signale comme « non rafraichi ». Aucune exception ne remonte (jamais bloquant).
"""
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
CALL = os.path.join(os.path.dirname(HERE), "claude_call.py")

# cle -> (indication de contenu, section du SitRep a fournir)
KEYS = {
    "coordination": "Response pillars (vaccination, clinical trials, coordination) across affected provinces. 2 sentences.",
    "logistics": "Logistics / ambulances / CTE construction / IPC supplies / stock-outs, by province. 2-3 sentences.",
    "resistance": "One short line: community resistance / attacks on response teams, with provinces. Max 90 characters.",
    "hcw_ipc": "One sentence on health-worker / IPC / staffing gaps with the figures given in the SitRep.",
    "security": "One sentence on security incidents / non-functional PoC / provinces reported calm.",
}


def _norm_num(tok):
    t = re.sub(r"[\s  ]", "", tok)
    if re.fullmatch(r"\d{1,3}(,\d{3})+", t):
        t = t.replace(",", "")
    t = t.replace(",", ".")
    if "." in t:
        t = t.rstrip("0").rstrip(".") if re.search(r"\.\d*0$", t) else t
    return t


NUM_RE = re.compile(r"\d[\d\s  ]*(?:[.,]\d+)?")


def numbers_of(txt):
    out = set()
    for m in re.finditer(r"\d+(?:[\s  ]\d{3})*(?:[.,]\d+)?", txt):
        out.add(_norm_num(m.group(0)))
    return out


STARTERS = {"the", "a", "an", "in", "at", "on", "no", "all", "both", "two", "three", "four", "five", "some", "most", "only", "there",
            "this", "these", "and", "but", "while", "with", "from", "after", "during", "no", "new", "several", "many", "one", "each"}


def proper_nouns(txt):
    """mots capitalises hors debut de phrase (acronymes inclus)."""
    out = set()
    for sent in re.split(r"(?<=[.!?])\s+", txt):
        words = re.findall(r"[A-Za-zÀ-ÿ][\w\-À-ÿ']*", sent)
        for i, w in enumerate(words):
            wl = w.strip("'").lower()
            if w[0].isupper() and not (i == 0 and wl in STARTERS):
                out.add(wl)
    return out


def verify(cand, src_text, prior):
    """renvoie (ok, raison). Tous les nombres et noms propres doivent exister dans le SitRep (ou le texte precedent
    pour les noms propres seulement) ; longueur bornee pour ne pas deborder de la forme."""
    if not isinstance(cand, str) or not cand.strip():
        return False, "vide"
    cand = cand.strip()
    if "\n" in cand:
        return False, "retours a la ligne"
    lim = max(60, int(len(prior) * 1.3)) if prior else 400
    if len(cand) > lim:
        return False, "trop long (%d > %d)" % (len(cand), lim)
    src_nums = numbers_of(src_text)
    bad = sorted(n for n in numbers_of(cand) if n not in src_nums)
    if bad:
        return False, "nombres absents du SitRep : %s" % ", ".join(bad)
    low = src_text.lower() + " " + (prior or "").lower()
    low = low.replace("’", "'")
    badw = sorted(w for w in proper_nouns(cand) if w not in low)
    if badw:
        return False, "noms propres absents du SitRep : %s" % ", ".join(badw)
    return True, ""


SYSTEM = (
    "You write short neutral situation-report sentences in English for an epidemic dashboard slide. "
    "Use ONLY facts stated in the SitRep text provided (in French). Never compute, estimate or infer a number: "
    "copy numbers exactly as published (write digits, thousands with commas, decimals with a dot). "
    "Do not name any place, organisation or product that does not appear in the SitRep. "
    "No bullet points, no line breaks, no markdown. If the SitRep says nothing on a topic, return an empty string for it. "
    "Answer with a single JSON object {key: text} and nothing else."
)


def _call(spec, out_path, timeout=300):
    sp = out_path + ".spec.json"
    json.dump(spec, open(sp, "w", encoding="utf-8"), ensure_ascii=False)
    env = dict(os.environ, CLAUDE_CALL_DEADLINE_S=os.environ.get("CLAUDE_CALL_DEADLINE_S", "240"),
               CLAUDE_CALL_MAX_RETRIES=os.environ.get("CLAUDE_CALL_MAX_RETRIES", "2"))
    subprocess.run([sys.executable, CALL, "--spec", sp, "--out", out_path], env=env, timeout=timeout,
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return json.load(open(out_path, encoding="utf-8"))


def generate(S, text, prior, log_path, call=None):
    """renvoie {cle: texte_verifie} (cles absentes = non rafraichies)."""
    call = call or _call
    log = {"requested": list(KEYS), "accepted": {}, "rejected": {}}
    try:
        user = ["SitRep text (French, pdftotext):\n" + text[:60000], "\nTasks (one entry per key, same style/length as the previous text shown):"]
        for k, hint in KEYS.items():
            user.append('- "%s": %s Previous text: "%s"' % (k, hint, (prior or {}).get(k, "")))
        res = call({"system": SYSTEM, "user_text": "\n".join(user), "max_tokens": 2000,
                    "models": ["claude-sonnet-5-5", "claude-sonnet-5"]}, log_path + ".result.json")
        if not res.get("ok"):
            log["error"] = "%s: %s" % (res.get("category"), res.get("message"))
            return {}
        raw = res["text"].strip()
        raw = re.sub(r"^```(?:json)?|```$", "", raw, flags=re.M).strip()
        data = json.loads(raw)
    except Exception as e:  # noqa
        log["error"] = "%s: %s" % (type(e).__name__, e)
        return {}
    finally:
        try:
            json.dump(log, open(log_path, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
        except Exception:
            pass
    out = {}
    for k in KEYS:
        cand = data.get(k)
        ok, why = verify(cand, text, (prior or {}).get(k, ""))
        if ok:
            out[k] = cand.strip()
            log["accepted"][k] = out[k]
        else:
            log["rejected"][k] = why
    try:
        json.dump(log, open(log_path, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    except Exception:
        pass
    return out
