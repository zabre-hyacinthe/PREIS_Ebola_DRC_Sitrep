# -*- coding: utf-8 -*-
"""Tests du generateur de deck. Lancer : python3 -m unittest discover -s scripts/python/tests -v
Le test de bout en bout avec le gabarit exige PREIS_DECK_TEMPLATE=<deck valide> (gabarit confidentiel, absent du depot)."""
import json
import os
import re
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
DECK = os.path.join(os.path.dirname(HERE), "deck")
FIX = os.path.join(HERE, "fixtures")
sys.path.insert(0, DECK)
import sitrep_parse as sp  # noqa: E402
import deck_build as db  # noqa: E402
import narrative as nv  # noqa: E402
import send_deck as sd  # noqa: E402
import deck_tables as dt  # noqa: E402
import deck_nk as nk  # noqa: E402


def fx(n):
    return open(os.path.join(FIX, "sitrep%d_layout.txt" % n), encoding="utf-8").read()


class Parse(unittest.TestCase):
    def test_146_values(self):
        S = sp.parse_sitrep(fx(146), 146)
        self.assertEqual((S["cases_cum"], S["deaths_cum"], S["new_cases"], S["new_deaths"]), (8787, 4242, 59, 37))
        self.assertEqual(S["warnings"], [])
        self.assertTrue(S["t2"]["ok"])
        self.assertEqual(S["contacts"]["by_prov"]["Nord-Kivu"]["pct"], 76.0)

    def test_145_values_and_contacts_by_prov(self):
        S = sp.parse_sitrep(fx(145), 145)
        self.assertEqual((S["cases_cum"], S["deaths_cum"]), (8728, 4205))
        self.assertEqual(S["contacts"]["by_prov"]["Tshopo"]["pct"], 79.8)
        self.assertEqual(S["warnings"], [])

    def test_consecutive_cumuls_reconcile(self):
        a, b = sp.parse_sitrep(fx(145), 145), sp.parse_sitrep(fx(146), 146)
        self.assertEqual(a["cases_cum"] + b["new_cases"], b["cases_cum"])
        self.assertEqual(a["deaths_cum"] + b["new_deaths"], b["deaths_cum"])

    def test_refuses_wrong_number(self):
        with self.assertRaises(sp.ParseError):
            sp.parse_sitrep(fx(146), 147)

    def test_refuses_inconsistent_text(self):
        bad = fx(146).replace("8 787", "8 797", 1)  # un seul endroit modifie : le bandeau ne colle plus au reste
        if bad == fx(146):
            self.skipTest("motif absent")
        try:
            S = sp.parse_sitrep(bad, 146)
        except sp.ParseError:
            return
        self.assertTrue(S["warnings"], "une incoherence doit lever une erreur ou un avertissement")

    def test_garbage_refused(self):
        with self.assertRaises(sp.ParseError):
            sp.parse_sitrep("rien a voir avec un SitRep\n" * 50, 146)


class Rounding(unittest.TestCase):
    def test_half_up(self):
        self.assertEqual(db.rh(6.25), 6.3)
        self.assertEqual(db.rh(0.5804, 2), 0.58)

    def test_share_never_prints_100_when_not_all(self):
        self.assertEqual(db.share(4242, 4244), "99.95")
        self.assertEqual(db.share(8787, 8807), "99.8")
        self.assertEqual(db.share(10, 10), "100.0")


class Narrative(unittest.TestCase):
    T = fx(146)

    def test_rejects_unknown_number(self):
        ok, why = nv.verify("Ituri received 999 trucks.", self.T, "x" * 80)
        self.assertFalse(ok)
        self.assertIn("999", why)

    def test_rejects_unknown_place(self):
        self.assertFalse(nv.verify("Zorbland was attacked.", self.T, "x" * 80)[0])

    def test_rejects_too_long_and_multiline(self):
        self.assertFalse(nv.verify("a b\nc", self.T, "x" * 80)[0])
        self.assertFalse(nv.verify("word " * 200, self.T, "x" * 80)[0])

    def test_generate_with_mock(self):
        good = {"resistance": "Refusals persist in Tshopo and Nord-Kivu.", "logistics": "Ituri got 999 trucks"}
        with tempfile.TemporaryDirectory() as d:
            r = nv.generate({}, self.T, {"resistance": "Refusals persist; teams attacked."}, os.path.join(d, "log.json"),
                            call=lambda s, o: {"ok": True, "text": json.dumps(good)})
            self.assertEqual(list(r), ["resistance"])
            log = json.load(open(os.path.join(d, "log.json")))
            self.assertIn("logistics", log["rejected"])

    def test_generate_api_failure_is_silent(self):
        with tempfile.TemporaryDirectory() as d:
            r = nv.generate({}, self.T, {}, os.path.join(d, "log.json"), call=lambda s, o: {"ok": False, "category": "auth", "message": "x"})
            self.assertEqual(r, {})


class Mail(unittest.TestCase):
    def test_default_recipients_are_ops_only(self):
        os.environ["ALERT_TO"] = "ops@example.org"
        os.environ.pop("PREIS_DECK_TO", None)
        self.assertEqual(sd.recipients()[1], ["ops@example.org"])
        os.environ["PREIS_DECK_TO"] = "a@x.org; b@x.org"
        self.assertEqual(sd.recipients()[1], ["a@x.org", "b@x.org"])
        self.assertEqual(sd.recipients(test=True)[1], ["ops@example.org"])  # le mode test n'envoie jamais au public
        os.environ.pop("PREIS_DECK_TO")

    def test_message_attachments(self):
        with tempfile.TemporaryDirectory() as d:
            fp = os.path.join(d, "x.pptx")
            open(fp, "wb").write(b"PK")
            m = sd.build_message("a@x.org", ["b@x.org"], "s", "p", "<p>h</p>", [fp, os.path.join(d, "absent.csv")])
            self.assertEqual([p.get_filename() for p in m.iter_attachments()], ["x.pptx"])


class Tables(unittest.TestCase):
    S = sp.parse_sitrep(fx(146), 146)

    def _prs(self, zones, title="Haut-Uélé — Weekly Trends"):
        from pptx import Presentation
        from pptx.util import Inches
        p = Presentation()
        sl = p.slides.add_slide(p.slide_layouts[6])
        tb = sl.shapes.add_textbox(0, 0, Inches(4), Inches(1)); tb.text_frame.text = title
        n = sl.shapes.add_textbox(0, Inches(6), Inches(4), Inches(1)); n.text_frame.text = "6 Oct table (SitRep 145) · all affected zones · attack rates ND"
        rows = len(zones) + 2
        t = sl.shapes.add_table(rows, 7, 0, Inches(1), Inches(6), Inches(3)).table
        for r in range(rows):
            for c in range(7):
                t.cell(r, c).text = "0"
        for i, z in enumerate(zones):
            t.cell(i + 1, 2).text = z
        t.cell(rows - 1, 2).text = "Shown zones"
        return p

    class _Dk:
        def __init__(self, p):
            self.p, self.log, self.retained = p, [], []

    def test_updates_and_verifies(self):
        zs = ["Wamba", "Isiro", "Pawa", "Boma Mangbetu", "Rungu", "Gombari", "Dungu"]
        dk = self._Dk(self._prs(zs))
        self.assertEqual(dt.update(dk, self.S, db.rh), 1)
        ok, fails = dt.verify(dk.p, self.S)
        self.assertEqual(fails, [])
        self.assertGreater(ok, 7)
        t = [sh for sh in dk.p.slides[0].shapes if sh.has_table][0].table
        self.assertEqual([t.cell(r, 3).text for r in range(1, 9)], ["119", "102", "95", "44", "4", "3", "1", "368"])

    def test_unknown_zone_keeps_table(self):
        dk = self._Dk(self._prs(["Wamba", "Zorbaland"]))
        self.assertEqual(dt.update(dk, self.S, db.rh), 0)
        self.assertTrue(dk.retained)
        t = [sh for sh in dk.p.slides[0].shapes if sh.has_table][0].table
        self.assertEqual(t.cell(1, 3).text, "0")  # rien n'a ete modifie

    def test_verify_detects_tampering(self):
        zs = ["Wamba", "Isiro", "Pawa", "Boma Mangbetu", "Rungu", "Gombari", "Dungu"]
        dk = self._Dk(self._prs(zs))
        dt.update(dk, self.S, db.rh)
        t = [sh for sh in dk.p.slides[0].shapes if sh.has_table][0].table
        t.cell(1, 3).text = "999"
        self.assertTrue(dt.verify(dk.p, self.S)[1])

    def test_slide_found_by_title_not_position(self):
        p = self._prs(["Wamba"], title="Tshopo — Weekly Trends")
        self.assertEqual([x[1] for x in dt.find_slides(p)], ["Tshopo"])


class NordKivuSlide(unittest.TestCase):
    S = sp.parse_sitrep(fx(146), 146)

    class _Dk:
        def __init__(self, p):
            self.p, self.log, self.retained, self.dynamic = p, [], [], []

    def _slide(self, extra=None):
        from pptx import Presentation
        from pptx.util import Inches
        p = Presentation()
        sl = p.slides.add_slide(p.slide_layouts[6])
        def box(x, y, txt):
            b = sl.shapes.add_textbox(Inches(x), Inches(y), Inches(3), Inches(0.5)); b.text_frame.text = txt
        box(4, 0, "Nord-Kivu: 1,785 cases; highest CFR, follow-up up to 80.2%")
        for i, (v, l) in enumerate([("1,785", "CASES"), ("1,021", "DEATHS"), ("57.2%", "CFR"), ("17 / 34", "HEALTH ZONES")]):
            box(4 + 2 * i, 1, v); box(4 + 2 * i, 1.6, l)
        box(4, 2.2, "20.5% of national"); box(10, 2.2, "50.0% affected"); box(4, 2.8, "24-HOUR PULSE")
        box(4, 3.2, "30 new cases · 30 positives / 265 samples · positivity 11.3%")
        box(4, 3.8, "8 deaths (7 community, 1 CTE) · 2 recovered · 47.6% of new cases")
        for x, h in ((0.5, "SURVEILLANCE"), (4.5, "LABORATORY & CARE"), (8.5, "POINTS OF ENTRY / CONTROL")):
            box(x, 5, h)
        box(0.5, 5.6, "Alerts 1,320 · suspects 229 · investigated 229 · transferred 125 · follow-up 80.2% · contacts seen 11,452 / 14,281")
        b = sl.shapes.add_textbox(Inches(4.5), Inches(5.6), Inches(3), Inches(1)); b.text_frame.text = "Tests 265 · positive 30"
        for t in ("Positivity 11.3%", "Admissions / exits not reported", "Recovered 2", "Care not reported"):
            b.text_frame.add_paragraph().text = t
        b = sl.shapes.add_textbox(Inches(8.5), Inches(5.6), Inches(3), Inches(1)); b.text_frame.text = "Reporting 95.2% (20/21)"
        for t in ("Travellers 92,375 · screened 98.0%", "Handwashing/sensitised 98.0%", "Alerts 2 · validated 0", "Bodies intercepted 0"):
            b.text_frame.add_paragraph().text = t
        box(0.5, 4, "Hot zones: Beni 9; Biena, Butembo and Mutwanga 3 each.")
        box(0.5, 4.5, "Response: no IPC/EDS update in SitRep 145.")
        return p

    def test_found_and_updated(self):
        dk = self._Dk(self._slide())
        self.assertTrue(nk.update(dk, self.S, db.rh))
        txt = "\n".join(sh.text_frame.text for sh in dk.p.slides[0].shapes if sh.has_text_frame)
        for frag in ("1,811 cases; highest CFR, lowest contact follow-up at 76.0%", "1,036", "20.6% of national", "26 new cases · 26 positives / 180 samples · positivity 14.4%",
                     "15 deaths (11 community, 4 CTE) · 8 recovered · 44.1% of new cases", "Hot zones: Beni 11; Katwa 5; Butembo 3.", "contacts seen 11,481 / 15,116",
                     "Admissions 142 / exits 116", "Travellers 94,168 · screened 98.1%", "Bodies intercepted 1", "25/38 IPC rings opened (65.8%), with a major gap in Katwa and Beni"):
            self.assertIn(frag, txt)
        self.assertNotIn("1,785", txt)
        self.assertEqual(nk.verify(dk.p, self.S, db.rh)[1], [])

    def test_hot_zone_groups_reproduce_145_wording(self):
        # regle de regroupement : jusqu'a 3 zones, ex-aequo regroupes
        S = {"t1": self.S["t1"], "cases_cum": 8787, "new_cases": 59, "t2": {"zones": {"Nord-Kivu": {"Beni": {"new": 9, "cases": 400}, "Biena": {"new": 3, "cases": 20}, "Butembo": {"new": 3, "cases": 280}, "Mutwanga": {"new": 3, "cases": 19}}}, "prov": {}}, "sections": {}}
        T, why = nk.texts(S, db.rh)
        self.assertEqual(T["hot"], "Hot zones: Beni 9; Butembo, Biena and Mutwanga 3 each.")

    def test_absent_slide_is_ignored(self):
        from pptx import Presentation
        dk = self._Dk(Presentation())
        self.assertFalse(nk.update(dk, self.S, db.rh))

    def test_unknown_structure_keeps_text(self):
        p = self._slide()
        for sh in p.slides[0].shapes:
            if sh.has_text_frame and sh.text_frame.text.startswith("Tests"):
                for pp in list(sh.text_frame.paragraphs)[1:]:
                    pp._p.getparent().remove(pp._p)
        dk = self._Dk(p)
        nk.update(dk, self.S, db.rh)
        self.assertTrue(any("LABORATORY" in r[1] for r in dk.retained))


@unittest.skipUnless(os.environ.get("PREIS_DECK_TEMPLATE") and os.path.isfile(os.environ.get("PREIS_DECK_TEMPLATE", "")),
                     "PREIS_DECK_TEMPLATE absent (gabarit confidentiel)")
class EndToEnd(unittest.TestCase):
    def test_build_146_passes_all_checks(self):
        import subprocess
        with tempfile.TemporaryDirectory() as d:
            r = subprocess.run([sys.executable, os.path.join(DECK, "build_deck.py"), "--sitrep", "146", "--text", os.path.join(FIX, "sitrep146_layout.txt"),
                                "--prev-text", os.path.join(FIX, "sitrep145_layout.txt"), "--template", os.environ["PREIS_DECK_TEMPLATE"],
                                "--state", os.path.join(d, "s.json"), "--out-dir", d, "--narrative", "off"], capture_output=True, text=True)
            self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
            rep = json.load(open(os.path.join(d, "deck_report.json")))
            self.assertEqual(rep["status"], "ok")
            self.assertEqual(rep["checks_failed"], [])
            self.assertGreaterEqual(rep["checks_passed"], 50)


class Workflow(unittest.TestCase):
    def test_installer_idempotent(self):
        import subprocess
        with tempfile.TemporaryDirectory() as d:
            os.makedirs(os.path.join(d, ".github", "workflows"))
            wf = os.path.join(d, ".github", "workflows", "w.yml")
            open(wf, "w").write("steps:\n      # PREIS WATCHDOG START\n      - name: w\n")
            for _ in range(2):
                subprocess.run([sys.executable, os.path.join(DECK, "install_deck_workflow.py"), wf], cwd=d, check=True, capture_output=True)
            s = open(wf).read()
            self.assertEqual(s.count("PREIS DECK START"), 1)
            self.assertLess(s.index("PREIS DECK START"), s.index("PREIS WATCHDOG START"))
            self.assertEqual(open(os.path.join(d, ".gitignore")).read().count("outputs/deck_private/"), 1)


if __name__ == "__main__":
    unittest.main()
