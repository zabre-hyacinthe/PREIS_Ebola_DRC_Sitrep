# -*- coding: utf-8 -*-
"""Tests : concordance SitRep/Brief + mise en forme permanente des tuiles (gras, justifie)."""
import os, re, shutil, sys, tempfile, unittest, zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
sys.path.insert(0, os.path.join(HERE, ".."))
import concordance

TPL = os.path.join(REPO, "data", "africa_cdc_brief", "templates")
S = os.path.join(TPL, "BVD_SitRep_latest_propre.docx")
B = os.path.join(TPL, "BVD_Executive_Brief_latest_propre.docx")


def _xml(path):
    return zipfile.ZipFile(path).read("word/document.xml").decode("utf-8")


def _rewrite(src, dst, old, new, count=1):
    zin = zipfile.ZipFile(src)
    x = zin.read("word/document.xml").decode("utf-8")
    assert old in x, old
    x = x.replace(old, new, count)
    with zipfile.ZipFile(dst, "w", zipfile.ZIP_DEFLATED) as zo:
        for it in zin.infolist():
            zo.writestr(it, x.encode("utf-8") if it.filename == "word/document.xml" else zin.read(it.filename))


@unittest.skipUnless(os.path.exists(S) and os.path.exists(B), "gabarits absents")
class TestConcordance(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp(prefix="conc_")

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_templates_are_concordant(self):
        err, _ = concordance.check(S, B)
        self.assertEqual(err, [], err)

    def test_tile_mismatch_is_an_error(self):
        s2 = os.path.join(self.d, "s.docx")
        _rewrite(S, s2, ">8,462<", ">8,463<")
        err, _ = concordance.check(s2, B)
        self.assertTrue(any("8,463" in e or "8,462" in e for e in err), err)

    def test_province_row_mismatch_is_an_error(self):
        s2 = os.path.join(self.d, "s.docx")
        _rewrite(S, s2, ">6,370<", ">6,371<")
        err, _ = concordance.check(s2, B)
        self.assertTrue(err, "une divergence de cas cumules entre les 2 documents doit bloquer")

    def test_unrecognised_structure_only_warns(self):
        s2 = os.path.join(self.d, "s.docx")
        zin = zipfile.ZipFile(S)
        x = re.sub(r"<w:tbl>.*?</w:tbl>", "", zin.read("word/document.xml").decode("utf-8"), flags=re.S)
        with zipfile.ZipFile(s2, "w", zipfile.ZIP_DEFLATED) as zo:
            for it in zin.infolist():
                zo.writestr(it, x.encode("utf-8") if it.filename == "word/document.xml" else zin.read(it.filename))
        err, warn = concordance.check(s2, B)
        self.assertEqual(err, [], err)
        self.assertTrue(warn)


@unittest.skipUnless(os.path.exists(S) and os.path.exists(B), "gabarits absents")
class TestTileFormatting(unittest.TestCase):
    def _tile(self, path):
        return re.findall(r"<w:tbl>.*?</w:tbl>", _xml(path), re.S)[1]

    def test_all_big_numbers_bold_and_text_justified(self):
        for path in (S, B):
            t = self._tile(path)
            big = [r for r in re.findall(r"<w:r\b[^>]*>.*?</w:r>", t, re.S)
                   if re.search(r'<w:sz w:val="(2[2-9]|[3-9]\d)"', r)]
            self.assertGreaterEqual(len(big), 8, path)
            for r in big:
                self.assertIn("<w:b/>", r, "grand chiffre non gras : %s" % re.findall(r"<w:t[^>]*>([^<]*)", r))
            for p in re.findall(r"<w:p\b.*?</w:p>", t, re.S):
                self.assertIn('<w:jc w:val="both"/>', p)


if __name__ == "__main__":
    unittest.main()
