# -*- coding: utf-8 -*-
"""
Serveurs factices pour tester la chaine Africa CDC SitRep+Brief SANS cle API
ni compte e-mail reel :

  MockClaudeAPI : imite POST /v1/messages (streaming SSE) avec des scenarios
                  de reussite et de panne, consommes dans l'ordre des appels.
  MockSMTP      : serveur SMTP-sur-TLS minimal (certificat auto-signe) qui
                  conserve les messages recus pour verification.

Utilise par test_e2e.py (le client reel, claude_call.py / le script R, n'est
PAS modifie pour les tests : seule l'URL est redirigee via ANTHROPIC_API_URL
et SMTP_HOST/SMTP_PORT).
"""
import base64
import email
import json
import socketserver
import ssl
import subprocess
import tempfile
import threading
import os
from email import policy
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


# ---------------------------------------------------------------- API ----
class MockClaudeAPI:
    """scenarios : liste consommee dans l'ordre ; une fois vide, repond 'ok'.
    Valeurs : ok | ok_fenced | 529 | 429 | 401 | 400_credit | 404_model |
              midstream | cutoff | truncated | badjson | refusal"""

    def __init__(self, ok_text, scenarios=None):
        self.ok_text = ok_text
        self.scenarios = list(scenarios or [])
        self.requests = []          # {"model":..., "has_pdf":..., "bytes":...}
        self.lock = threading.Lock()
        outer = self

        class H(BaseHTTPRequestHandler):
            def log_message(self, *a):
                pass

            def _json_err(self, code, etype, msg, headers=None):
                body = json.dumps({"type": "error", "error": {"type": etype, "message": msg}}).encode()
                self.send_response(code)
                self.send_header("content-type", "application/json")
                self.send_header("request-id", "req_mock_%d" % len(outer.requests))
                for k, v in (headers or {}).items():
                    self.send_header(k, v)
                self.send_header("content-length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def _sse(self, ev, data):
                self.wfile.write(("event: %s\ndata: %s\n\n" % (ev, json.dumps(data))).encode("utf-8"))
                self.wfile.flush()

            def do_POST(self):
                n = int(self.headers.get("content-length", "0"))
                raw = self.rfile.read(n)
                body = json.loads(raw)
                key = self.headers.get("x-api-key", "")
                content = body["messages"][0]["content"]
                with outer.lock:
                    outer.requests.append({"model": body.get("model"), "bytes": n, "stream": body.get("stream"),
                                           "has_pdf": any(c.get("type") == "document" for c in content),
                                           "key": key, "version": self.headers.get("anthropic-version"),
                                           "max_tokens": body.get("max_tokens"), "thinking": body.get("thinking")})
                    sc = outer.scenarios.pop(0) if outer.scenarios else "ok"
                if key != "sk-test-valid" and sc != "401":
                    return self._json_err(401, "authentication_error", "invalid x-api-key")
                if sc == "401":
                    return self._json_err(401, "authentication_error", "invalid x-api-key")
                if sc == "529":
                    return self._json_err(529, "overloaded_error", "Overloaded")
                if sc == "429":
                    return self._json_err(429, "rate_limit_error", "rate limited", {"retry-after": "0"})
                if sc == "400_credit":
                    return self._json_err(400, "invalid_request_error",
                                          "Your credit balance is too low to access the Anthropic API.")
                if sc == "400_thinking" and body.get("thinking"):
                    return self._json_err(400, "invalid_request_error", "thinking: parametre non pris en charge")
                if sc == "404_model":
                    return self._json_err(404, "not_found_error", "model: %s" % body.get("model"))
                self.send_response(200)
                self.send_header("content-type", "text/event-stream")
                self.send_header("request-id", "req_mock_%d" % len(outer.requests))
                self.end_headers()
                self._sse("message_start", {"type": "message_start", "message": {
                    "id": "msg_1", "model": body.get("model"), "usage": {"input_tokens": 1234, "output_tokens": 1}}})
                self._sse("ping", {"type": "ping"})
                text = outer.ok_text
                if sc == "ok_fenced":
                    text = "```json\n" + text + "\n```"
                if sc == "badjson":
                    text = "Desole, je ne peux pas produire ce JSON."
                if sc == "refusal":
                    text = ""
                chunks = [text[i:i + 700] for i in range(0, len(text), 700)] or [""]
                if sc in ("midstream", "cutoff"):
                    chunks = chunks[:max(1, len(chunks) // 3)]
                self._sse("content_block_start", {"type": "content_block_start", "index": 0,
                                                  "content_block": {"type": "text", "text": ""}})
                for c in chunks:
                    if c:
                        self._sse("content_block_delta", {"type": "content_block_delta", "index": 0,
                                                          "delta": {"type": "text_delta", "text": c}})
                if sc == "midstream":
                    return self._sse("error", {"type": "error", "error": {"type": "overloaded_error",
                                                                          "message": "Overloaded"}})
                if sc == "cutoff":
                    self.wfile.flush()
                    self.close_connection = True
                    return
                self._sse("content_block_stop", {"type": "content_block_stop", "index": 0})
                stop = "max_tokens" if sc == "truncated" else ("refusal" if sc == "refusal" else "end_turn")
                self._sse("message_delta", {"type": "message_delta", "delta": {"stop_reason": stop},
                                            "usage": {"output_tokens": 4321}})
                self._sse("message_stop", {"type": "message_stop"})

        self.httpd = ThreadingHTTPServer(("127.0.0.1", 0), H)
        self.port = self.httpd.server_address[1]
        self.url = "http://127.0.0.1:%d/v1/messages" % self.port
        threading.Thread(target=self.httpd.serve_forever, daemon=True).start()

    def stop(self):
        self.httpd.shutdown()
        self.httpd.server_close()


# --------------------------------------------------------------- SMTP ----
def make_selfsigned_cert(dirpath):
    crt, key = os.path.join(dirpath, "mock.crt"), os.path.join(dirpath, "mock.key")
    subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-keyout", key, "-out", crt,
                    "-days", "2", "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1"],
                   check=True, capture_output=True)
    return crt, key


class MockSMTP:
    """SMTP implicite-TLS (comme le port 465). Conserve les messages dans self.messages."""

    def __init__(self, certdir, fail_next=0):
        self.crt, self.key = make_selfsigned_cert(certdir)
        self.messages = []
        self.fail_next = fail_next      # refuse les N prochains messages (451)
        outer = self
        ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        ctx.load_cert_chain(self.crt, self.key)

        class H(socketserver.StreamRequestHandler):
            def handle(self):
                try:
                    conn = ctx.wrap_socket(self.request, server_side=True)
                except Exception:
                    return
                f = conn.makefile("rwb")

                def w(s):
                    f.write((s + "\r\n").encode()); f.flush()
                w("220 mock ESMTP")
                rcpts, sender = [], ""
                while True:
                    line = f.readline()
                    if not line:
                        return
                    cmd = line.decode("utf-8", "replace").strip()
                    up = cmd.upper()
                    if up.startswith("EHLO") or up.startswith("HELO"):
                        f.write(b"250-localhost\r\n250-SIZE 52428800\r\n250 AUTH PLAIN LOGIN\r\n"); f.flush()
                    elif up.startswith("AUTH PLAIN"):
                        w("235 2.7.0 ok")
                    elif up.startswith("AUTH LOGIN"):
                        w("334 VXNlcm5hbWU6"); f.readline(); w("334 UGFzc3dvcmQ6"); f.readline(); w("235 ok")
                    elif up.startswith("MAIL FROM"):
                        sender = cmd; rcpts = []; w("250 ok")
                    elif up.startswith("RCPT TO"):
                        rcpts.append(cmd.split(":", 1)[1].strip().strip("<>")); w("250 ok")
                    elif up == "DATA":
                        w("354 go")
                        data = b""
                        while True:
                            l = f.readline()
                            if l in (b".\r\n", b""):
                                break
                            data += l[1:] if l.startswith(b"..") else l
                        if outer.fail_next > 0:
                            outer.fail_next -= 1
                            w("451 4.3.0 temporary failure")
                        else:
                            outer.messages.append({"rcpts": rcpts, "raw": data})
                            w("250 queued")
                    elif up == "QUIT":
                        w("221 bye"); return
                    elif up == "RSET":
                        w("250 ok")
                    else:
                        w("250 ok")

        class S(socketserver.ThreadingMixIn, socketserver.TCPServer):
            allow_reuse_address = True
            daemon_threads = True

        self.srv = S(("127.0.0.1", 0), H)
        self.port = self.srv.server_address[1]
        threading.Thread(target=self.srv.serve_forever, daemon=True).start()

    def parsed(self):
        out = []
        for m in self.messages:
            msg = email.message_from_bytes(m["raw"], policy=policy.default)
            atts = [p.get_filename() for p in msg.iter_attachments()]
            out.append({"subject": str(msg["Subject"]), "to": str(msg["To"]), "rcpts": m["rcpts"],
                        "attachments": atts, "msg": msg})
        return out

    def stop(self):
        self.srv.shutdown()
        self.srv.server_close()
