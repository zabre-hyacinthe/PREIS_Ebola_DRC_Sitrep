# -*- coding: utf-8 -*-
"""
Client minimal et robuste pour l'API Claude (Messages, streaming SSE).
Bibliotheque standard uniquement : rien a installer sur le runner GitHub.

Pourquoi un script dedie (et pas un appel direct depuis R) :
  - streaming : recommande par Anthropic pour les longues reponses (evite les
    coupures de connexion inactive d'une requete longue) ;
  - nouvelles tentatives avec attente progressive sur 429 / 5xx / 529 et sur
    les erreurs reseau ;
  - repli automatique sur un autre identifiant de modele si le premier est
    inconnu de l'API (404 not_found_error) ;
  - delai GLOBAL borne : le workflow GitHub a un timeout et son dernier step
    (commit des resultats) ne doit jamais etre prive de temps par ce script ;
  - la cle API n'est jamais ecrite dans les journaux.

Usage :
  python3 claude_call.py --spec spec.json --out result.json [--text-out reponse.txt]

spec.json :
  {"system": "...", "user_text": "...", "pdf_path": "chemin.pdf" (optionnel),
   "max_tokens": 48000, "models": ["claude-sonnet-5-5", "claude-sonnet-5"]}

Environnement :
  ANTHROPIC_API_KEY            obligatoire
  ANTHROPIC_API_URL            (tests) remplace https://api.anthropic.com/v1/messages
  CLAUDE_CALL_DEADLINE_S       delai global, defaut 540 s
  CLAUDE_CALL_READ_TIMEOUT_S   attente max entre deux octets, defaut 180 s
  CLAUDE_CALL_BACKOFF_BASE_S   attente de base entre tentatives, defaut 20 s
  CLAUDE_CALL_MAX_RETRIES      tentatives supplementaires par modele, defaut 4

Sortie result.json :
  succes : {"ok": true, "text": "...", "model": "...", "stop_reason": "...",
            "usage": {...}, "request_id": "...", "attempts": n}
  echec  : {"ok": false, "category": "...", "http_status": n|null, "message": "...",
            "request_id": "...", "attempts": n}
  categories : config | auth | request | input | model | truncated | refusal |
               empty | overloaded | network | timeout | api
Code retour : 0 si ok, 3 sinon.
"""
import argparse
import base64
import http.client
import json
import os
import random
import socket
import ssl
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

API_VERSION = "2023-06-01"
DEFAULT_URL = "https://api.anthropic.com/v1/messages"
RETRYABLE_HTTP = {408, 409, 429, 500, 502, 503, 504, 529}
RETRYABLE_STREAM_ERRORS = {"overloaded_error", "api_error", "rate_limit_error"}
MAX_PDF_BYTES = 22 * 1024 * 1024  # base64 +33 % ; la requete est limitee a 32 Mo


def log(msg):
    print("[claude_call] " + msg, file=sys.stderr, flush=True)


class CallFailure(Exception):
    def __init__(self, category, message, http_status=None, retryable=False,
                 retry_after=None, next_model=False, request_id=None):
        super().__init__(message)
        self.category = category
        self.message = message
        self.http_status = http_status
        self.retryable = retryable
        self.retry_after = retry_after
        self.next_model = next_model
        self.request_id = request_id


def redact(text, key):
    text = str(text)
    if key:
        text = text.replace(key, "***")
    return text


def _parse_error_body(raw):
    try:
        d = json.loads(raw)
        e = d.get("error") or {}
        return e.get("type"), e.get("message") or raw[:300]
    except Exception:
        return None, raw[:300]


def _retry_after(headers):
    try:
        v = headers.get("retry-after")
        return float(v) if v else None
    except Exception:
        return None


def _stream_request(url, key, body_bytes, read_timeout):
    req = urllib.request.Request(
        url, data=body_bytes, method="POST",
        headers={"x-api-key": key, "anthropic-version": API_VERSION,
                 "content-type": "application/json", "accept": "text/event-stream"})
    try:
        resp = urllib.request.urlopen(req, timeout=read_timeout)
    except urllib.error.HTTPError as e:
        raw = e.read().decode("utf-8", "replace")[:2000]
        etype, emsg = _parse_error_body(raw)
        rid = e.headers.get("request-id") if e.headers else None
        ra = _retry_after(e.headers) if e.headers else None
        msg = "HTTP %s %s : %s" % (e.code, etype or "", emsg)
        if e.code in (401, 403):
            raise CallFailure("auth", msg, e.code, request_id=rid)
        if e.code == 404 and etype == "not_found_error":
            raise CallFailure("model", msg, e.code, next_model=True, request_id=rid)
        if e.code == 413:
            raise CallFailure("input", msg, e.code, request_id=rid)
        if e.code in RETRYABLE_HTTP:
            cat = "overloaded" if e.code in (429, 529) else "api"
            raise CallFailure(cat, msg, e.code, retryable=True, retry_after=ra, request_id=rid)
        raise CallFailure("request", msg, e.code, request_id=rid)
    except (urllib.error.URLError, socket.timeout, TimeoutError, ConnectionError,
            http.client.HTTPException, ssl.SSLError, OSError) as e:
        raise CallFailure("network", "connexion impossible : %s" % e, retryable=True)

    rid = resp.headers.get("request-id")
    text_parts, usage, stop_reason, model_used, saw_stop = [], {}, None, None, False
    try:
        with resp:
            for raw_line in resp:
                line = raw_line.decode("utf-8", "replace").rstrip("\r\n")
                if not line.startswith("data:"):
                    continue
                payload = line[5:].strip()
                if not payload or payload == "[DONE]":
                    continue
                try:
                    ev = json.loads(payload)
                except ValueError:
                    continue
                t = ev.get("type")
                if t == "message_start":
                    m = ev.get("message") or {}
                    model_used = m.get("model") or model_used
                    usage.update(m.get("usage") or {})
                elif t == "content_block_delta":
                    d = ev.get("delta") or {}
                    if d.get("type") == "text_delta":
                        text_parts.append(d.get("text", ""))
                elif t == "message_delta":
                    stop_reason = (ev.get("delta") or {}).get("stop_reason") or stop_reason
                    usage.update(ev.get("usage") or {})
                elif t == "message_stop":
                    saw_stop = True
                elif t == "error":
                    err = ev.get("error") or {}
                    et = err.get("type")
                    raise CallFailure(
                        "overloaded" if et in RETRYABLE_STREAM_ERRORS else "api",
                        "erreur dans le flux : %s %s" % (et, err.get("message", "")),
                        retryable=et in RETRYABLE_STREAM_ERRORS, request_id=rid)
    except CallFailure:
        raise
    except (socket.timeout, TimeoutError, ConnectionError, http.client.HTTPException,
            ssl.SSLError, OSError) as e:
        raise CallFailure("network", "flux interrompu : %s" % e, retryable=True, request_id=rid)
    if not saw_stop:
        raise CallFailure("network", "flux termine sans message_stop (coupure)", retryable=True,
                          request_id=rid)
    return {"text": "".join(text_parts), "model": model_used, "stop_reason": stop_reason,
            "usage": usage, "request_id": rid}


def _write_json(path, obj):
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(obj, f, ensure_ascii=False)
    os.replace(tmp, path)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--spec", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--text-out", default=None)
    args = ap.parse_args()

    key = os.environ.get("ANTHROPIC_API_KEY", "").strip()
    attempts = 0

    def fail(category, message, http_status=None, request_id=None):
        message = redact(message, key)
        log("ECHEC (%s) : %s" % (category, message))
        _write_json(args.out, {"ok": False, "category": category, "http_status": http_status,
                               "message": message, "request_id": request_id, "attempts": attempts})
        sys.exit(3)

    if not key:
        fail("config", "ANTHROPIC_API_KEY absente de l'environnement")

    url = os.environ.get("ANTHROPIC_API_URL", "").strip() or DEFAULT_URL
    host = urllib.parse.urlparse(url).hostname or ""
    if not url.startswith("https://") and host not in ("localhost", "127.0.0.1", "::1"):
        fail("config", "URL d'API refusee (https obligatoire hors localhost) : %s" % url)

    deadline = time.monotonic() + float(os.environ.get("CLAUDE_CALL_DEADLINE_S", "540"))
    read_timeout = float(os.environ.get("CLAUDE_CALL_READ_TIMEOUT_S", "180"))
    backoff_base = float(os.environ.get("CLAUDE_CALL_BACKOFF_BASE_S", "20"))
    max_retries = int(os.environ.get("CLAUDE_CALL_MAX_RETRIES", "4"))

    with open(args.spec, encoding="utf-8") as f:
        spec = json.load(f)
    models = spec.get("models") or ["claude-sonnet-5-5"]
    if isinstance(models, str):
        models = [models]

    content = []
    pdf_path = spec.get("pdf_path")
    if pdf_path:
        size = os.path.getsize(pdf_path)
        if size > MAX_PDF_BYTES:
            fail("input", "PDF trop volumineux (%d octets > %d)" % (size, MAX_PDF_BYTES))
        with open(pdf_path, "rb") as f:
            b64 = base64.b64encode(f.read()).decode("ascii")
        content.append({"type": "document",
                        "source": {"type": "base64", "media_type": "application/pdf", "data": b64}})
    content.append({"type": "text", "text": spec["user_text"]})

    last = None
    for model in models:
        body = json.dumps({
            "model": model, "max_tokens": int(spec.get("max_tokens", 48000)), "stream": True,
            "system": spec.get("system", ""), "messages": [{"role": "user", "content": content}],
        }, ensure_ascii=False).encode("utf-8")
        retries = 0
        while True:
            if time.monotonic() > deadline:
                fail("timeout", "delai global depasse (%s)" % (last.message if last else "aucune reponse"),
                     last.http_status if last else None, last.request_id if last else None)
            attempts += 1
            log("appel modele=%s tentative=%d" % (model, attempts))
            try:
                r = _stream_request(url, key, body, read_timeout)
            except CallFailure as e:
                e.message = redact(e.message, key)
                last = e
                if e.next_model:
                    log("modele %s inconnu -> modele suivant" % model)
                    break
                if e.retryable and retries < max_retries:
                    wait = e.retry_after if e.retry_after else backoff_base * (2 ** retries)
                    wait = min(wait, 120.0) * random.uniform(0.85, 1.15)
                    if backoff_base == 0:
                        wait = 0
                    if time.monotonic() + wait > deadline:
                        fail("timeout", "delai global depasse apres : %s" % e.message,
                             e.http_status, e.request_id)
                    log("%s (%s) -> nouvelle tentative dans %.0f s" % (e.category, e.message[:160], wait))
                    time.sleep(wait)
                    retries += 1
                    continue
                fail(e.category, e.message, e.http_status, e.request_id)

            if r["stop_reason"] == "max_tokens":
                fail("truncated", "reponse tronquee (max_tokens atteint) : JSON incomplet",
                     None, r["request_id"])
            if r["stop_reason"] == "refusal":
                fail("refusal", "le modele a refuse de repondre (stop_reason=refusal)", None, r["request_id"])
            if not r["text"].strip():
                fail("empty", "reponse vide", None, r["request_id"])
            if args.text_out:
                with open(args.text_out, "w", encoding="utf-8") as f:
                    f.write(r["text"])
            _write_json(args.out, {"ok": True, "text": r["text"], "model": r["model"] or model,
                                   "stop_reason": r["stop_reason"], "usage": r["usage"],
                                   "request_id": r["request_id"], "attempts": attempts})
            log("OK modele=%s tokens=%s" % (r["model"] or model, json.dumps(r["usage"])))
            return
    fail("model", "aucun des modeles essayes n'est reconnu par l'API : %s" % ", ".join(models),
         404, last.request_id if last else None)


if __name__ == "__main__":
    main()
