# -*- coding: utf-8 -*-
"""
Tests de bout en bout de la chaine Africa CDC SitRep + Brief automatique
(06b -> claude_call.py -> run_build.py -> 05b), SANS cle API ni e-mail reels.

Chaque test copie les VRAIS scripts et les VRAIS gabarits dans un espace de
travail temporaire, redirige l'API vers MockClaudeAPI et le SMTP vers MockSMTP,
puis execute le VRAI script R par Rscript.

Lancer (Python 3.9+, R avec jsonlite/dplyr/readr, openssl) depuis la racine du depot :
    python3 scripts/python/tests/test_e2e.py            # tous les tests
    python3 scripts/python/tests/test_e2e.py -v TestE2E.test_happy_path
"""
import csv
import glob
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
import zipfile
from xml.sax.saxutils import unescape

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
sys.path.insert(0, os.path.join(REPO, "scripts", "python"))
sys.path.insert(0, HERE)
import africa_cdc_build_docx as bd          # noqa: E402
from mock_servers import MockClaudeAPI, MockSMTP   # noqa: E402

TEMPLATES = os.path.join(REPO, "data", "africa_cdc_brief", "templates")
SITREP_T = os.path.join(TEMPLATES, "BVD_SitRep_latest_propre.docx")
BRIEF_T = os.path.join(TEMPLATES, "BVD_Executive_Brief_latest_propre.docx")
RAPPORTS = "outputs/rapports"


def sha(p):
    with open(p, "rb") as f:
        return hashlib.sha256(f.read()).hexdigest()


def runs_and_paras(docx):
    d = tempfile.mkdtemp()
    try:
        bd.unzip_docx(docx, d)
        with open(os.path.join(d, "word", "document.xml"), encoding="utf-8") as fh:
            xml = fh.read()
    finally:
        shutil.rmtree(d, ignore_errors=True)
    runs = bd.get_runs(xml)
    texts = [unescape(bd.split_run(r.group(0))[3], {"&quot;": '"', "&apos;": "'"}) for r in runs]
    paras = [(m.start(), m.end()) for m in re.finditer(r"<w:p\b[^>]*>.*?</w:p>", xml, re.S)]
    return runs, texts, paras


def make_edits(docx, new_issue, prev_issue, n_edits, update_issue=True):
    """Edits realistes sur le VRAI gabarit : mise a jour du numero d'Issue + n_edits runs chiffres modifies."""
    runs, texts, paras = runs_and_paras(docx)
    in_para = {}
    for i, r in enumerate(runs):
        for pi, (a, b) in enumerate(paras):
            if a <= r.start() < b:
                in_para[i] = pi
                break
    edits, issue_runs = {}, []
    target = None
    for pi in sorted(set(in_para.values())):
        joined = "".join(texts[i] for i, p in in_para.items() if p == pi)
        if re.search(r"Issue\s*No\.?\s*%d" % prev_issue, joined):
            target = pi
            issue_runs = [i for i, p in in_para.items() if p == pi]
            joined_new = joined.replace(str(prev_issue), str(new_issue))
            break
    assert target is not None, "paragraphe d'Issue introuvable dans le gabarit"
    if update_issue:
        edits[str(issue_runs[0])] = {"new": joined_new, "comment": None}
        for i in issue_runs[1:]:
            edits[str(i)] = {"new": "", "comment": None}
    count = 0
    for i, t in enumerate(texts):
        if count >= n_edits:
            break
        if i in issue_runs or not re.search(r"\d", t) or len(t) > 400:
            continue
        edits[str(i)] = {"new": t + " (maj)", "comment": "Valeur recalculee depuis le SitRep RDC" if count % 5 == 0 else None}
        count += 1
    return edits


def ok_payload(new_issue=132, prev_issue=131, n_sitrep=14, n_brief=7, update_issue=True):
    return json.dumps({
        "sitrep_edits": make_edits(SITREP_T, new_issue, prev_issue, n_sitrep, update_issue),
        "brief_edits": make_edits(BRIEF_T, new_issue, prev_issue, n_brief, update_issue),
        "summary_fr": "SitRep RDC 142 utilise (test).",
        "anomalies": ["Anomalie de test 1", "Anomalie de test 2"]}, ensure_ascii=False)


def find_pdf():
    c = sorted(glob.glob(os.path.join(REPO, "data", "pdf", "SitRep_141_2026.pdf")) +
               glob.glob(os.path.join(REPO, "data", "pdf", "SitRep_*_2026.pdf")))
    return c[0] if c else None


class TestE2E(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        for need in (SITREP_T, BRIEF_T):
            if not os.path.exists(need):
                raise unittest.SkipTest("gabarit absent : %s" % need)
        cls.pdf = find_pdf()
        if not cls.pdf:
            raise unittest.SkipTest("aucun PDF de SitRep dans data/pdf/")
        cls.certdir = tempfile.mkdtemp(prefix="mocktls_")

    def setUp(self):
        self.ws = tempfile.mkdtemp(prefix="africa_cdc_ws_")
        for sub in ("scripts/python", "docs/africa_cdc", "data/final", "data/pdf", "data/africa_cdc_brief/templates"):
            os.makedirs(os.path.join(self.ws, sub), exist_ok=True)
        for f in ("05b_send_africacdc_sitrep_brief_full_email.R", "06b_generate_africa_cdc_sitrep_brief_full.R"):
            shutil.copy(os.path.join(REPO, "scripts", f), os.path.join(self.ws, "scripts", f))
        for f in ("africa_cdc_build_docx.py", "claude_call.py", "extract_runs.py", "run_build.py", "concordance.py"):
            shutil.copy(os.path.join(REPO, "scripts", "python", f), os.path.join(self.ws, "scripts", "python", f))
        shutil.copy(os.path.join(REPO, "docs", "africa_cdc", "PREIS_procedure_SitRep_AfricaCDC.md"),
                    os.path.join(self.ws, "docs", "africa_cdc"))
        shutil.copy(SITREP_T, os.path.join(self.ws, "data/africa_cdc_brief/templates"))
        shutil.copy(BRIEF_T, os.path.join(self.ws, "data/africa_cdc_brief/templates"))
        shutil.copy(self.pdf, os.path.join(self.ws, "data/pdf/SitRep_142_2026.pdf"))
        with open(os.path.join(self.ws, "data/final/sitrep_registry.csv"), "w", encoding="utf-8") as f:
            f.write("sitrep_no,pdf_url,date_raw,local_pdf\n"
                    "142,https://insp.cd/wp-content/uploads/2026/10/SitRep_MVEBDB_142_03_10_2026.pdf,,"
                    "/home/runner/work/x/data/pdf/SitRep_142_2026.pdf\n"
                    "141,https://insp.cd/wp-content/uploads/2026/10/SitRep_MVEBDB_141_02_10_2026.pdf,,"
                    "/home/runner/work/x/data/pdf/SitRep_141_2026.pdf\n")
        with open(os.path.join(self.ws, "data/africa_cdc_brief/state.csv"), "w", encoding="utf-8") as f:
            f.write("sitrep_no_source,issue_no,generated_at_utc,anomalies_count,sitrep_propre_file,brief_propre_file\n"
                    "141,131,2026-10-04T14:00:00Z,5,BVD_SitRep_131_02_October2026_propre.docx,"
                    "BVD_Executive_Brief_131_02_October2026_propre.docx\n")
        with open(os.path.join(self.ws, "data/africa_cdc_brief/email_sent_state.csv"), "w", encoding="utf-8") as f:
            f.write("issue_no,sitrep_no_source,sent_utc\n131,141,2026-10-04T14:05:00 UTC\n")   # = graine livree dans le depot
        self.servers = []

    def tearDown(self):
        for s in self.servers:
            s.stop()
        shutil.rmtree(self.ws, ignore_errors=True)

    # ------------------------------------------------------------ outils
    def api(self, scenarios=None, text=None):
        a = MockClaudeAPI(text if text is not None else ok_payload(), scenarios)
        self.servers.append(a)
        return a

    def smtp(self, **kw):
        s = MockSMTP(self.certdir, **kw)
        self.servers.append(s)
        return s

    def r(self, script, api=None, smtp=None, mode="normal", key="sk-test-valid", extra=None, timeout=300):
        env = dict(os.environ)
        env.update({"PREIS_BASE_DIR": self.ws, "PREIS_AFRICACDC_MODE": mode, "CLAUDE_CALL_BACKOFF_BASE_S": "0",
                    "CLAUDE_CALL_DEADLINE_S": "60", "ANTHROPIC_API_KEY": key})
        env.pop("GITHUB_WORKSPACE", None)
        if api:
            env["ANTHROPIC_API_URL"] = api.url
        if smtp:
            env.update({"SMTP_HOST": "localhost", "SMTP_PORT": str(smtp.port), "SMTP_SSL": "true",
                        "SMTP_USER": "u@example.org", "SMTP_PASS": "pw", "ALERT_FROM": "preis@example.org",
                        "ALERT_TO": "ops@example.org", "SSL_CERT_FILE": smtp.crt})
        env.update(extra or {})
        p = subprocess.run(["Rscript", "--vanilla", os.path.join("scripts", script)], cwd=self.ws, env=env,
                           capture_output=True, text=True, timeout=timeout)
        return p.returncode, p.stdout + p.stderr

    def f(self, rel):
        return os.path.join(self.ws, rel)

    def attempts(self):
        p = self.f("data/africa_cdc_brief/attempts.csv")
        if not os.path.exists(p):
            return []
        with open(p, encoding="utf-8") as fh:
            return list(csv.DictReader(fh))

    def state_rows(self):
        with open(self.f("data/africa_cdc_brief/state.csv"), encoding="utf-8") as fh:
            return list(csv.DictReader(fh))

    def assert_untouched(self, t0, b0):
        self.assertEqual(len(self.state_rows()), 1, "l'etat ne doit pas etre modifie")
        self.assertEqual(sha(self.f("data/africa_cdc_brief/templates/BVD_SitRep_latest_propre.docx")), t0)
        self.assertEqual(sha(self.f("data/africa_cdc_brief/templates/BVD_Executive_Brief_latest_propre.docx")), b0)
        self.assertFalse(glob.glob(self.f(RAPPORTS + "/BVD_*")), "aucun livrable ne doit exister apres un echec")

    def hashes(self):
        return (sha(self.f("data/africa_cdc_brief/templates/BVD_SitRep_latest_propre.docx")),
                sha(self.f("data/africa_cdc_brief/templates/BVD_Executive_Brief_latest_propre.docx")))

    # ============================================================ TESTS
    def test_happy_path(self):
        t0, b0 = self.hashes()
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertIn("2 documents generes et verifies", out)
        rows = self.state_rows()
        self.assertEqual((rows[-1]["sitrep_no_source"], rows[-1]["issue_no"], rows[-1]["anomalies_count"]), ("142", "132", "2"))
        names = sorted(os.path.basename(p) for p in glob.glob(self.f(RAPPORTS + "/BVD_*.docx")))
        self.assertEqual(len(names), 2, names)
        self.assertFalse([n for n in names if "suivi" in n or "propre" in n], names)
        # Libelle depuis la date du nom de fichier du registre (03/10/2026), mois en anglais
        self.assertTrue(all("132_03_October2026" in n for n in names), names)
        self.assertNotEqual(self.hashes(), (t0, b0), "les gabarits doivent etre remplaces")
        self.assertEqual(len(api.requests), 1)
        q = api.requests[0]
        self.assertTrue(q["has_pdf"] and q["stream"] is True and q["version"] == "2023-06-01")
        self.assertEqual(q["model"], "claude-sonnet-5-5")
        # le nouveau gabarit (Issue 132) sert bien de base : numero present
        runs, texts, paras = runs_and_paras(self.f("data/africa_cdc_brief/templates/BVD_SitRep_latest_propre.docx"))
        self.assertIn("132", " ".join(texts))
        # 2e execution : rien a refaire, aucun appel API
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertIn("deja traite", out)
        self.assertEqual(len(api.requests), 1)
        # integrite Word du document final : le fichier s'ouvre dans LibreOffice
        if shutil.which("soffice"):
            suivi = glob.glob(self.f(RAPPORTS + "/BVD_SitRep_*.docx"))[0]
            outdir = tempfile.mkdtemp()
            subprocess.run(["soffice", "--headless", "--convert-to", "pdf", "--outdir", outdir, suivi],
                           capture_output=True, timeout=180)
            self.assertTrue(glob.glob(os.path.join(outdir, "*.pdf")), "LibreOffice n'a pas pu ouvrir le .docx final")

    def test_transient_errors_then_success_fenced(self):
        api = self.api(["529", "429", "ok_fenced"])
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertEqual(len(api.requests), 3)
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132")

    def test_midstream_error_and_cutoff_are_retried(self):
        api = self.api(["midstream", "cutoff", "ok"])
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertEqual(len(api.requests), 3)
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132")

    def test_unknown_model_falls_back(self):
        api = self.api(["404_model", "ok"])
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertEqual([q["model"] for q in api.requests], ["claude-sonnet-5-5", "claude-sonnet-5"])
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132")

    def test_auth_failure_counts_attempts_then_stops_calling_api(self):
        t0, b0 = self.hashes()
        api = self.api()
        for n in (1, 2, 3):
            rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api, key="sk-wrong")
            self.assertEqual(rc, 0, out)
            self.assertEqual(self.attempts()[0]["attempts"], str(n))
            self.assertEqual(self.attempts()[0]["last_category"], "auth")
        self.assertEqual(len(api.requests), 3, "401 n'est pas rejoue a l'interieur d'un cycle")
        self.assert_untouched(t0, b0)
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api, key="sk-wrong")
        self.assertIn("Plus d'essai automatique", out)
        self.assertEqual(len(api.requests), 3, "plus aucun appel apres la limite de tentatives")
        with open(self.f("data/africa_cdc_brief/attempts.csv"), encoding="utf-8") as fh:
            self.assertNotIn("sk-wrong", fh.read())
        # alerte operateur envoyee une seule fois
        sm = self.smtp()
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm)
        self.assertEqual(rc, 0, out)
        msgs = sm.parsed()
        self.assertEqual(len(msgs), 1, out)
        self.assertIn("ALERTE", msgs[0]["subject"])
        self.assertEqual(msgs[0]["rcpts"], ["ops@example.org"])
        body = msgs[0]["msg"].get_body(preferencelist=("plain",)).get_content()
        self.assertIn("142", body)
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm)
        self.assertEqual(len(sm.parsed()), 1, "pas de deuxieme alerte")
        # relance manuelle : remet les compteurs a zero et reussit avec la bonne cle
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api, mode="retry")
        self.assertEqual(rc, 0, out)
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132")

    def test_credit_error_message_is_surfaced(self):
        api = self.api(["400_credit"])
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertEqual(self.attempts()[0]["last_category"], "request")
        self.assertIn("credit balance", self.attempts()[0]["last_error"])

    def test_issue_number_is_forced_when_model_forgets_it(self):
        """Cas reel observe : le modele ne touche pas 'Issue No. 131'. Le numero est corrige de facon
        deterministe (131 -> 132) ; un numero deja change mais FAUX (999) reste refuse."""
        api = self.api(text=ok_payload(update_issue=False))
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertIn("2 documents generes et verifies", out, out)
        self.assertIn("corrige automatiquement", out)
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132")
        p = glob.glob(self.f(RAPPORTS + "/BVD_SitRep_132_*.docx"))[0]
        root = bd._load_doc(p)[0]
        self.assertEqual(bd.issue_numbers(bd.paragraph_texts(root)), {132})

    def _xml(self, docx):
        d = tempfile.mkdtemp()
        try:
            bd.unzip_docx(docx, d)
            with open(os.path.join(d, "word", "document.xml"), encoding="utf-8") as fh:
                return fh.read()
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def test_credit_and_formatting_are_permanent(self):
        """Le nom « Dr. R. Hyacinthe ZABRE » (apres Dr Merawi), le texte justifie et les tableaux centres
        font partie du gabarit : ils doivent survivre a la generation."""
        t_s, t_b = self._xml(SITREP_T), self._xml(BRIEF_T)
        self.assertIn("Merawi Aragaw, Dr. R. Hyacinthe ZABRE, Wazih", t_s)
        self.assertIn("Merawi Aragaw, Dr. R. Hyacinthe ZABRE, Wazih", t_b)
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        for kind, tpl in (("SitRep", t_s), ("Executive_Brief", t_b)):
            f = glob.glob(self.f(RAPPORTS + "/BVD_%s_132_*.docx" % kind))[0]
            x = self._xml(f)
            self.assertIn("Dr. R. Hyacinthe ZABRE", x)
            self.assertEqual(x.count('w:jc w:val="both"'), tpl.count('w:jc w:val="both"'))
            self.assertEqual(x.count('w:jc w:val="center"'), tpl.count('w:jc w:val="center"'))
        self.assertGreaterEqual(t_s.count('w:jc w:val="both"'), 15)
        self.assertGreaterEqual(t_s.count('w:jc w:val="center"'), 40)

    def test_credit_is_restored_if_model_rewrites_prepared_by_without_it(self):
        payload = json.loads(ok_payload())
        runs, texts, _ = runs_and_paras(SITREP_T)
        i = [k for k, t in enumerate(texts) if "Merawi Aragaw" in t][0]
        payload["sitrep_edits"][str(i)] = {"new": texts[i].replace(", Dr. R. Hyacinthe ZABRE", ""), "comment": None}
        api = self.api(text=json.dumps(payload))
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertIn("remise automatiquement", out)
        f = glob.glob(self.f(RAPPORTS + "/BVD_SitRep_132_*.docx"))[0]
        self.assertIn("Merawi Aragaw, Dr. R. Hyacinthe ZABRE", self._xml(f))

    def test_last_response_is_attached_to_failure_mail_in_test_mode(self):
        api = self.api(text=json.dumps({"sitrep_edits": {"0": {"new": "x", "comment": None}}, "brief_edits": {}}))
        smtp = self.smtp()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api, smtp, mode="test")
        self.assertEqual(rc, 0, out)
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", None, smtp, mode="test")
        self.assertEqual(rc, 0, out)
        msgs = smtp.parsed()
        self.assertEqual(len(msgs), 1, out)
        self.assertIn("last_response.txt", msgs[0]["attachments"])

    def test_bad_model_outputs_are_rejected_and_nothing_is_written(self):
        t0, b0 = self.hashes()
        cases = {
            "truncated": (["truncated"], None, "truncated"),
            "badjson": (["badjson"], None, "output"),
            "refusal": (["refusal"], None, "refusal"),
            "poor": ([], json.dumps({"sitrep_edits": {"0": {"new": "x", "comment": None}}, "brief_edits": {}}), "output"),
            "wrong_issue": ([], ok_payload(new_issue=999, prev_issue=131), "output"),
            "bad_index": ([], json.dumps({"sitrep_edits": {"99999": {"new": "x", "comment": None}}, "brief_edits": {}}), "output"),
        }
        for name, (sc, text, cat) in cases.items():
            with self.subTest(name):
                if os.path.exists(self.f("data/africa_cdc_brief/attempts.csv")):
                    os.remove(self.f("data/africa_cdc_brief/attempts.csv"))
                api = self.api(sc, text)
                rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
                self.assertEqual(rc, 0, out)
                self.assertEqual(self.attempts()[0]["last_category"], cat, out)
                self.assertEqual(self.attempts()[0]["attempts"], "1")
                self.assert_untouched(t0, b0)
                if name == "truncated":   # diagnostic : tailles + reponse partielle conservee pour le mail
                    self.assertIn("sortie=4321 tokens", self.attempts()[0]["last_error"])
                    self.assertTrue(os.path.getsize(self.f("outputs/rapports/africa_cdc_test/last_response.txt")) > 0)

    def test_missing_key_is_config_problem_not_an_attempt(self):
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", key="")
        self.assertEqual(rc, 0, out)
        a = self.attempts()[0]
        self.assertEqual((a["last_category"], a["attempts"]), ("config", "0"))
        sm = self.smtp()
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm)
        self.assertEqual(len(sm.parsed()), 1, out)
        self.assertIn("ANTHROPIC_API_KEY", sm.parsed()[0]["msg"].get_body(preferencelist=("plain",)).get_content())
        # la cle est ajoutee plus tard : plus aucun blocage
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132", out)

    def test_missing_pdf_is_silent_and_not_an_attempt(self):
        os.remove(self.f("data/pdf/SitRep_142_2026.pdf"))
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(rc, 0, out)
        self.assertIn("introuvable", out)
        self.assertEqual(len(api.requests), 0)
        self.assertEqual(self.attempts(), [])

    def test_alternate_pdf_name_is_found(self):
        os.rename(self.f("data/pdf/SitRep_142_2026.pdf"), self.f("data/pdf/PREIS_DRC_Ebola_SitRep_142.pdf"))
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        self.assertEqual(self.state_rows()[-1]["issue_no"], "132", out)

    def test_normal_email_then_no_duplicate_and_smtp_retry(self):
        api = self.api()
        self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
        sm = self.smtp(fail_next=1)    # 1er envoi refuse par le serveur (451)
        extra = {"PREIS_AFRICACDC_TO": "africacdc@example.org, eiu@example.org"}
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm, extra=extra)
        self.assertEqual(rc, 2, "un echec SMTP doit etre visible (code 2)")
        self.assertEqual(len(sm.parsed()), 0)
        with open(self.f("data/africa_cdc_brief/email_sent_state.csv"), encoding="utf-8") as fh:
            self.assertNotIn("132", fh.read(), "l'Issue 132 ne doit pas etre marquee envoyee apres un echec SMTP")
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)       # cycle suivant : rien a regenerer
        self.assertIn("deja traite", out)
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm, extra=extra)    # renvoi automatique
        self.assertEqual(rc, 0, out)
        m = sm.parsed()
        self.assertEqual(len(m), 1)
        self.assertEqual(sorted(m[0]["rcpts"]), ["africacdc@example.org", "eiu@example.org"])
        self.assertEqual(len(m[0]["attachments"]), 2)
        self.assertIn("Issue No.132", m[0]["subject"])
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm, extra=extra)
        self.assertEqual(len(sm.parsed()), 1, "jamais deux fois la meme Issue")
        # PREIS_FORCE_SEND (vrai par defaut en lancement manuel) ne doit PAS declencher un renvoi externe
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm, extra=dict(extra, PREIS_FORCE_SEND="true"))
        self.assertEqual(len(sm.parsed()), 1)

    # ------------------------------------------------ persistance multi-cycles
    def test_two_cycles_persist_via_encrypted_store_in_real_git(self):
        """Simule 2 executions GitHub Actions successives sur des checkouts NEUFS, avec le vrai
        .gitignore du depot (*.docx et /outputs/* ignores) : les gabarits doivent survivre
        UNIQUEMENT via le coffre chiffre, et aucun .docx ne doit etre versionne."""
        def sh(cmd, cwd, **kw):
            e = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@e.org",
                     GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@e.org", PREIS_ARCHIVE_KEY="pass-test-1")
            e.update(kw.get("env", {}))
            p = subprocess.run(cmd, cwd=cwd, shell=True, capture_output=True, text=True, env=e)
            self.assertEqual(p.returncode, 0, "%s\n%s%s" % (cmd, p.stdout, p.stderr))
            return p.stdout
        store = "data/africa_cdc_brief/private_store.enc"
        ps = "python3 scripts/python/private_store.py"
        origin = tempfile.mkdtemp(prefix="origin_") + "/o.git"
        sh("git init -q --bare -b main %s" % origin, "/")
        # --- graine : ce que l'utilisateur committe une fois
        shutil.copy(os.path.join(REPO, ".gitignore"), self.ws)
        shutil.copy(os.path.join(REPO, "scripts", "python", "private_store.py"), self.f("scripts/python"))
        sh("git init -q -b main && git remote add origin %s" % origin, self.ws)
        sh("%s save --store %s --dir data/africa_cdc_brief/templates" % (ps, store), self.ws)
        sh("git add -A && git commit -q -m seed && git push -q origin main", self.ws)
        self.assertEqual(sh("git ls-files | grep -ci '\\.docx$' || true", self.ws).strip(), "0")
        self.assertIn(store, sh("git ls-files", self.ws))
        seed_ws = self.ws
        results = []
        for cycle, (sno, issue, prev) in enumerate([(142, 132, 131), (143, 133, 132)], 1):
            c = tempfile.mkdtemp(prefix="cycle%d_" % cycle)
            sh("git clone -q %s %s" % (origin, c), "/")
            self.assertFalse(glob.glob(os.path.join(c, "data/africa_cdc_brief/templates/*.docx")), "checkout neuf = pas de gabarit")
            self.ws = c
            sh("%s restore --store %s --dir data/africa_cdc_brief/templates" % (ps, store), c)
            tpl = os.path.join(c, "data/africa_cdc_brief/templates")
            st, bt = (os.path.join(tpl, "BVD_SitRep_latest_propre.docx"), os.path.join(tpl, "BVD_Executive_Brief_latest_propre.docx"))
            self.assertTrue(os.path.exists(st) and os.path.exists(bt), "gabarits restaures")
            os.makedirs(os.path.join(c, "data/final"), exist_ok=True)
            reg = "sitrep_no,pdf_url,date_raw,local_pdf\n"
            for n, d in ((143, "04_10"), (142, "03_10"), (141, "02_10")):
                if n <= sno:
                    reg += "%d,https://insp.cd/x/SitRep_MVEBDB_%d_%s_2026.pdf,,/h/data/pdf/SitRep_%d_2026.pdf\n" % (n, n, d, n)
            with open(os.path.join(c, "data/final/sitrep_registry.csv"), "w", encoding="utf-8") as fh:
                fh.write(reg)
            os.makedirs(os.path.join(c, "data/pdf"), exist_ok=True)
            shutil.copy(self.pdf, os.path.join(c, "data/pdf/SitRep_%d_2026.pdf" % sno))
            payload = json.dumps({"sitrep_edits": make_edits(st, issue, prev, 14), "brief_edits": make_edits(bt, issue, prev, 7),
                                  "summary_fr": "ok", "anomalies": []})
            api = self.api(text=payload)
            rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api)
            self.assertEqual(rc, 0, out)
            self.assertIn("2 documents generes et verifies", out, out)
            sh("%s save --store %s --dir data/africa_cdc_brief/templates" % (ps, store), c)
            sh("for f in state.csv attempts.csv email_sent_state.csv private_store.enc private_store.enc.sha256; do "
               "git add data/africa_cdc_brief/$f 2>/dev/null || true; done; git add outputs/rapports/ || true", c)
            self.assertEqual(sh("git ls-files | grep -ci '\\.docx$' || true", c).strip(), "0", "aucun docx ne doit etre versionne")
            self.assertEqual(sh("git status --short | grep -v '^[AM] ' | grep -ci 'docx\\|africa_cdc_test' || true", c).strip(), "0")
            sh("git commit -q -m 'cycle %d' && git push -q origin main" % cycle, c)
            self.assertEqual(self.state_rows()[-1]["issue_no"], str(issue))
            results.append(c)
        self.ws = seed_ws
        # un 3e checkout neuf : le coffre contient bien le gabarit du cycle 2 (Issue 133)
        c3 = tempfile.mkdtemp(prefix="cycle3_")
        sh("git clone -q %s %s" % (origin, c3), "/")
        sh("%s restore --store %s --dir data/africa_cdc_brief/templates" % (ps, store), c3)
        paras = bd._load_doc(os.path.join(c3, "data/africa_cdc_brief/templates/BVD_SitRep_latest_propre.docx"))
        paras = paras[0]
        self.assertIn(133, bd.issue_numbers(bd.paragraph_texts(paras)), "le gabarit du cycle 2 (Issue 133) doit etre dans le coffre")
        # idempotence : un cycle sans changement ne cree aucun commit
        sh("%s save --store %s --dir data/africa_cdc_brief/templates" % (ps, store), c3)
        self.assertEqual(sh("git status --short", c3).strip(), "")
        for x in results + [c3, os.path.dirname(origin)]:
            shutil.rmtree(x, ignore_errors=True)


    def test_test_mode_success_sends_only_to_operator_and_records_nothing(self):
        t0, b0 = self.hashes()
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api, mode="test")
        self.assertEqual(rc, 0, out)
        with open(self.f("outputs/rapports/africa_cdc_test/test_run.json"), encoding="utf-8") as fh:
            tr = json.load(fh)
        self.assertEqual(tr["status"], "ok")
        self.assertEqual(len(self.state_rows()), 1)
        self.assertEqual(self.hashes(), (t0, b0))
        self.assertFalse(os.path.exists(self.f("data/africa_cdc_brief/attempts.csv")))
        sm = self.smtp()
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm, mode="test",
                         extra={"PREIS_AFRICACDC_TO": "external@example.org"})
        self.assertEqual(rc, 0, out)
        m = sm.parsed()
        self.assertEqual(len(m), 1)
        self.assertEqual(m[0]["rcpts"], ["ops@example.org"], "le test ne doit jamais ecrire aux destinataires Africa CDC")
        self.assertTrue(m[0]["subject"].startswith("[TEST]"))
        self.assertEqual(len(m[0]["attachments"]), 2)

    def test_test_mode_failure_is_reported_by_email(self):
        api = self.api()
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api, mode="test", key="sk-wrong")
        self.assertEqual(rc, 0, out)
        sm = self.smtp()
        rc, out = self.r("05b_send_africacdc_sitrep_brief_full_email.R", smtp=sm, mode="test")
        m = sm.parsed()
        self.assertEqual(len(m), 1, out)
        self.assertIn("ECHEC", m[0]["subject"])
        self.assertIn("Cle API refusee", m[0]["msg"].get_body(preferencelist=("html",)).get_content())
        self.assertEqual(self.attempts(), [])

    def test_python_client_global_deadline(self):
        api = self.api(["529"] * 50)
        rc, out = self.r("06b_generate_africa_cdc_sitrep_brief_full.R", api,
                         extra={"CLAUDE_CALL_BACKOFF_BASE_S": "1", "CLAUDE_CALL_DEADLINE_S": "5"})
        self.assertEqual(rc, 0, out)
        self.assertIn(self.attempts()[0]["last_category"], ("timeout", "overloaded"))


if __name__ == "__main__":
    unittest.main(verbosity=2)
