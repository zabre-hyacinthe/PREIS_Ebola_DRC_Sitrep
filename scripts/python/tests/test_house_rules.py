# -*- coding: utf-8 -*-
"""Tests de house_rules.py (HCW impose, deux-points apres les titres, section Kenya) sur des documents SYNTHETIQUES
(aucun contenu confidentiel) qui reproduisent la structure reelle des gabarits Africa CDC."""
import datetime
import os
import re
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
import africa_cdc_build_docx as bd
import house_rules as hr

NS = ('xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" '
      'xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"')


def R(text, bold=False):
    rpr = "<w:rPr><w:b/><w:bCs/></w:rPr>" if bold else "<w:rPr><w:color w:val=\"000000\"/></w:rPr>"
    return '<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>' % (rpr, text)


def P(*runs, pid=None):
    attr = ' w14:paraId="%s" w14:textId="77777777"' % pid if pid else ""
    return "<w:p%s><w:pPr><w:jc w:val=\"both\"/></w:pPr>%s</w:p>" % (attr, "".join(runs))


def make_dir(body):
    d = tempfile.mkdtemp(prefix="hr_test_")
    os.makedirs(os.path.join(d, "word"))
    with open(os.path.join(d, "word", "document.xml"), "w", encoding="utf-8") as f:
        f.write('<?xml version="1.0" encoding="UTF-8"?><w:document %s><w:body>%s</w:body></w:document>' % (NS, body))
    return d


def runs_of(d):
    with open(os.path.join(d, "word", "document.xml"), encoding="utf-8") as f:
        xml = f.read()
    ET.fromstring(xml.encode("utf-8"))          # bien forme
    return [bd.split_run(r.group(0))[3] for r in bd.get_runs(xml)], xml


NOTE = ("*Active HZs: not reported in the DRC SitRep. Carried over, unverified; Africa CDC's analysis indicates 46 active "
        "health zones over the last 21 days. The province table reports {hz} historically affected health zones (SitRep No. {n}).")
VALUES = {
    "active_hz": {"note": NOTE},
    "hcw": {"source": "Weekly briefing", "infected_total": 270, "infected_drc": 266, "infected_uganda": 4,
            "deaths_total": 50, "deaths_drc": 50, "deaths_uganda": 0},
    "kenya": {"as_of": "2026-10-11", "body": "Kenya confirmed its first imported case on 6 October 2026."},
}


def tiles_body(num_runs=1):
    inf = P(R("258", True)) if num_runs == 1 else P(R("2", True), R("58", True))
    return (inf + P(R("HCW infected", True)) + P(R("DRC 254 · Uganda 4"))
            + P(R("49", True)) + P(R("HCW deaths", True)) + P(R("DRC 49 · Uganda 0")))


class HCW(unittest.TestCase):
    def test_imposes_270_50_and_comments_the_change(self):
        d = make_dir(tiles_body())
        ed = {}
        notes = hr.enforce_hcw(d, ed, VALUES)
        runs, _ = runs_of(d)
        got = {runs[i]: ed[i]["new"] for i in ed}
        self.assertEqual(got, {"258": "270", "DRC 254 · Uganda 4": "DRC 266 · Uganda 4",
                               "49": "50", "DRC 49 · Uganda 0": "DRC 50 · Uganda 0"})
        self.assertTrue(all("VALEUR IMPOSEE" in (e["comment"] or "") for e in ed.values()))
        self.assertEqual(len(notes), 4)

    def test_already_aligned_template_is_untouched(self):
        d = make_dir(tiles_body().replace(">258<", ">270<").replace("DRC 254", "DRC 266")
                     .replace(">49<", ">50<").replace("DRC 49", "DRC 50"))
        ed = {}
        self.assertEqual(hr.enforce_hcw(d, ed, VALUES), [])
        self.assertEqual(ed, {})

    def test_model_edit_with_other_value_is_overridden_and_keeps_its_comment(self):
        d = make_dir(tiles_body())
        ed = {0: {"new": "999", "comment": "commentaire du modele"}}
        hr.enforce_hcw(d, ed, VALUES)
        self.assertEqual(ed[0]["new"], "270")
        self.assertEqual(ed[0]["comment"] and "VALEUR IMPOSEE" in ed[0]["comment"], True)

    def test_unrecognised_tile_structure_is_skipped_and_reported(self):
        d = make_dir(tiles_body(num_runs=2))
        ed = {}
        notes = hr.enforce_hcw(d, ed, VALUES)
        self.assertTrue(any("non reconnue" in n for n in notes))
        runs, _ = runs_of(d)
        self.assertNotIn("270", [ed[i]["new"] for i in ed])   # rien d'ecrit a l'aveugle
        self.assertIn("50", [ed[i]["new"] for i in ed])       # l'autre tuile, reconnue, est traitee

    def test_missing_tile_or_values_is_a_noop(self):
        d = make_dir(P(R("Hello", True)) + P(R("world")))
        ed = {}
        self.assertEqual(hr.enforce_hcw(d, ed, VALUES), [])
        self.assertEqual(hr.enforce_hcw(d, ed, {}), [])
        self.assertEqual(hr.enforce_hcw(d, ed, {"hcw": {"infected_total": 1}})[0].startswith("HCW : valeurs"), True)


class ActiveHZNote(unittest.TestCase):
    OLD = ("*Active HZs reflect zones that reported at least one confirmed case during the previous 42 days, as defined in "
           "SitRep No. 139. The province table reports 65/167 historically affected health zones (SitRep No. 149); the "
           "42-day count could not be recalculated and the previous count (56 in 6 provinces) is retained without verification.")

    def test_note_is_rewritten_with_current_hz_and_sitrep_number(self):
        d = make_dir(P(R("DRC total", True)) + P(R(self.OLD)))
        ed = {}
        notes = hr.enforce_active_hz_note(d, ed, VALUES)
        (i, e), = ed.items()
        self.assertEqual(e["new"], NOTE.replace("{hz}", "65/167").replace("{n}", "149"))
        self.assertNotIn("as defined in", e["new"])
        self.assertIn("NOTE IMPOSEE", e["comment"])
        self.assertEqual(notes, ["note Active HZs mise a jour"])

    def test_numbers_come_from_the_model_edit_when_it_updated_them(self):
        d = make_dir(P(R(self.OLD)))
        ed = {0: {"new": self.OLD.replace("65/167", "66/167").replace("No. 149", "No. 150"), "comment": None}}
        hr.enforce_active_hz_note(d, ed, VALUES)
        self.assertIn("66/167", ed[0]["new"])
        self.assertIn("SitRep No. 150", ed[0]["new"])

    def test_idempotent_on_next_cycle(self):
        done = NOTE.replace("{hz}", "65/167").replace("{n}", "149")
        d = make_dir(P(R(done)))
        ed = {}
        self.assertEqual(hr.enforce_active_hz_note(d, ed, VALUES), [])
        self.assertEqual(ed, {})

    def test_unrecognised_note_is_not_written_blindly(self):
        d = make_dir(P(R("*Active HZs: something else entirely.")))
        ed = {}
        notes = hr.enforce_active_hz_note(d, ed, VALUES)
        self.assertEqual(ed, {})
        self.assertTrue(notes and "introuvables" in notes[0])
        self.assertEqual(hr.enforce_active_hz_note(d, {}, {}), [])


class Titles(unittest.TestCase):
    LONG = " The detailed narrative text follows here and is long enough to be a real paragraph."

    def fix(self, body):
        d = make_dir(body)
        ed = {}
        hr.enforce_title_colons(d, ed)
        runs, _ = runs_of(d)
        return {runs[i]: ed[i]["new"] for i in ed}, ed

    def test_bold_titles_get_colon_and_space(self):
        got, _ = self.fix(
            P(R("Surveillance and PoE/PoC.", True), R("In the last 24 hours, teams received alerts." + self.LONG))
            + P(R("Capacity gaps. ", True), R("Contact follow-up remained low." + self.LONG))
            + P(R("RCCE:", True), R("Reinforce community engagement." + self.LONG)))
        self.assertEqual(got, {"Surveillance and PoE/PoC.": "Surveillance and PoE/PoC: ",
                               "Capacity gaps. ": "Capacity gaps: ",
                               "RCCE:": "RCCE: "})

    def test_already_correct_forms_are_untouched(self):
        got, _ = self.fix(
            P(R("Mortality remains high: ", True), R("DRC CFR is 48.4%." + self.LONG))
            + P(R("Uganda:", True), R(" no new cases or deaths; totals remain 20 cases." + self.LONG)))
        self.assertEqual(got, {})

    def test_not_titles_are_never_touched(self):
        got, _ = self.fix(
            P(R("Latest update", True))                                          # intitule seul
            + P(R("30.6% ", True), R("Ituri", True))                             # tuile : tout en gras
            + P(R("12,345", True), R(" confirmed cases in total and counting" + self.LONG))   # chiffre en gras
            + P(R("Short.", True), R("tiny"))                                    # texte trop court
            + P(R("DRC reported 69 new cases." + self.LONG)))                    # pas de gras
        self.assertEqual(got, {})

    def test_brief_plain_titles_only_inside_response_operations(self):
        got, _ = self.fix(
            P(R("Response operations — snapshot", True))
            + P(R("Case management. DRC reports 946 patients in isolation." + self.LONG))
            + P(R("PoE/PoC, laboratory and vaccination. PoE/PoC recorded 247,706 travellers." + self.LONG))
            + P(R("Risks to watch", True))
            + P(R("Care capacity is strained. North Kivu suspect beds are saturated." + self.LONG)))
        self.assertEqual(list(got.values()), [
            "Case management: DRC reports 946 patients in isolation." + self.LONG,
            "PoE/PoC, laboratory and vaccination: PoE/PoC recorded 247,706 travellers." + self.LONG])

    def test_idempotent_after_generation(self):
        d = make_dir(P(R("Capacity gaps.", True), R("Contact follow-up remained low." + self.LONG)))
        ed = {}
        hr.enforce_title_colons(d, ed)
        d2 = make_dir(P(R("Capacity gaps: ", True), R("Contact follow-up remained low." + self.LONG)))
        ed2 = {}
        hr.enforce_title_colons(d2, ed2)
        self.assertEqual(ed2, {})


def sitrep_body():
    return (P(R("Latest update", True)) + P(R("x")) + P(pid="AAAA0001") +
            P(R("Response highlights", True), pid="AAAA0002") +
            P(R("Surveillance and PoE/PoC.", True), R("In the last 24 hours." + " detail" * 12), pid="AAAA0003") +
            P(R("Key challenges", True)))


class Kenya(unittest.TestCase):
    def setUp(self):
        self.body = (P(R("Situation overview", True)) + P(pid="BBBB0001")
                     + P(R("Response highlights", True), pid="BBBB0002")
                     + P(R("Surveillance and PoE/PoC.", True), R("In the last 24 hours." + " detail" * 12), pid="BBBB0003")
                     + P(R("Key challenges", True)))

    def test_inserts_section_before_response_highlights_with_clean_xml(self):
        d = make_dir(self.body)
        n0 = len(runs_of(d)[0])
        changed, texts, warns = hr.ensure_kenya_section(d, VALUES)
        runs, xml = runs_of(d)
        self.assertTrue(changed)
        self.assertEqual(warns, [])
        self.assertEqual(len(runs), n0 + 3)
        i = runs.index("Kenya update")
        self.assertEqual(runs[i + 1], hr.KENYA_LEAD)
        self.assertEqual(runs[i + 2], VALUES["kenya"]["body"])
        self.assertEqual(runs[i + 3], "Response highlights")
        # aucun identifiant Word duplique par les paragraphes clones
        ids = re.findall(r'w14:paraId="([^"]+)"', xml)
        self.assertEqual(len(ids), len(set(ids)))
        npar = lambda x: len(re.findall(r"<w:p[ >]", x))
        self.assertEqual(npar(xml), npar(self.body) + 3)                    # titre + corps + espacement

    def test_second_run_is_idempotent_and_updates_body_only(self):
        d = make_dir(self.body)
        hr.ensure_kenya_section(d, VALUES)
        before = runs_of(d)[1]
        self.assertEqual(hr.ensure_kenya_section(d, VALUES)[0], False)
        self.assertEqual(runs_of(d)[1], before)
        v2 = {"kenya": {"as_of": "2026-10-14", "body": "Updated Kenya text."}}
        changed, _t, _w = hr.ensure_kenya_section(d, v2)
        runs, xml = runs_of(d)
        self.assertTrue(changed)
        self.assertIn("Updated Kenya text.", runs)
        self.assertEqual(runs.count("Kenya update"), 1)
        self.assertEqual(xml.count("Kenya confirmed"), 0)

    def test_missing_anchor_or_values_never_breaks(self):
        d = make_dir(P(R("Hello", True)))
        changed, _t, warns = hr.ensure_kenya_section(d, VALUES)
        self.assertFalse(changed)
        self.assertTrue(warns and "introuvable" in warns[0])
        self.assertEqual(hr.ensure_kenya_section(d, {}), (False, [], []))

    def test_model_edits_on_kenya_runs_are_dropped(self):
        d = make_dir(self.body)
        hr.ensure_kenya_section(d, VALUES)
        runs, _ = runs_of(d)
        i = runs.index("Kenya update")
        ed = {i: {"new": "X", "comment": None}, i + 2: {"new": "invented", "comment": None},
              i + 4: {"new": "Keep", "comment": None}}
        self.assertEqual(hr.lock_kenya(d, ed), 2)
        self.assertEqual(list(ed), [i + 4])

    def test_hand_added_kenya_section_is_protected(self):
        """Section Kenya ajoutee a la main (Dativa) : titre seul + paragraphes ; ou paragraphe « Kenya: texte »."""
        d = make_dir(P(R("Latest update", True)) + P(R("Uganda:", True), R(" no new cases." + " x" * 30))
                     + P(R("Kenya:", True), R(" imported case." + " y" * 30))
                     + P(R("Kenya situation", True)) + P(R("Alert. ", True), R("Text A." + " a" * 30)) + P(R("Text B."))
                     + P(R("Response highlights", True)) + P(R("Keep this", True), R(" text." + " z" * 30)))
        runs, _ = runs_of(d)
        ed = {i: {"new": "X", "comment": None} for i in range(len(runs))}
        n = hr.lock_kenya(d, ed)
        kept = sorted(runs[i] for i in ed)
        self.assertEqual(n, 6)     # "Kenya:"+texte (2) ; titre (1) ; "Alert."+texte (2) ; "Text B." (1)
        self.assertNotIn("Kenya:", kept)
        self.assertNotIn("Kenya situation", kept)
        self.assertNotIn("Text B.", kept)
        for r in ("Uganda:", "Response highlights", "Keep this", "Latest update"):
            self.assertIn(r, kept)

    def test_titles_rule_leaves_kenya_untouched(self):
        d = make_dir(self.body)
        hr.ensure_kenya_section(d, VALUES)
        ed = {}
        hr.enforce_title_colons(d, ed)
        runs, _ = runs_of(d)
        self.assertNotIn(runs.index(hr.KENYA_LEAD), ed)
        self.assertNotIn(runs.index("Kenya update"), ed)

    def test_stale_warning(self):
        t = datetime.date(2026, 10, 13)
        self.assertIsNone(hr.kenya_stale_warning(VALUES, today=t))
        w = hr.kenya_stale_warning(VALUES, today=datetime.date(2026, 10, 25))
        self.assertTrue(w and "14 jours" in w)
        self.assertIsNone(hr.kenya_stale_warning({}, today=t))


if __name__ == "__main__":
    unittest.main()
