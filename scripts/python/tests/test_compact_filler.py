import os, sys, tempfile, unittest
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
import run_build

P = lambda t: "<w:p><w:r><w:t>%s</w:t></w:r></w:p>" % t if t else "<w:p/>"


class CompactFiller(unittest.TestCase):
    def test_keeps_one_empty_before_credit(self):
        with tempfile.TemporaryDirectory() as d:
            os.makedirs(os.path.join(d, "word"))
            body = P("A") + P("") * 8 + P("Prepared by Dr X") + P("")
            fp = os.path.join(d, "word", "document.xml")
            open(fp, "w", encoding="utf-8").write("<w:document><w:body>%s</w:body></w:document>" % body)
            run_build.compact_filler_before_credit(d)
            x = open(fp, encoding="utf-8").read()
            self.assertEqual(x.count("<w:p/>"), 2)  # 1 conserve + 1 apres le credit
            self.assertIn("Prepared by Dr X", x)
            self.assertIn(">A<", x)


if __name__ == "__main__":
    unittest.main()
