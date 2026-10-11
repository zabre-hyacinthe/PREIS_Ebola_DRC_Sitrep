# -*- coding: utf-8 -*-
import os, sys, tempfile, unittest, zipfile, shutil
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, ".."))
import docx_package as dp

CTX = ('<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
       '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
       '<Default Extension="xml" ContentType="application/xml"/>'
       '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/></Types>')
RELS = ('<?xml version="1.0"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/></Relationships>')
DOC = '<?xml version="1.0"?><w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body><w:p><w:r><w:t>x</w:t></w:r></w:p></w:body></w:document>'


def make(path, extra=None):
    with zipfile.ZipFile(path, "w") as z:
        z.writestr("word/document.xml", DOC)          # volontairement pas en premier
        z.writestr("[Content_Types].xml", CTX)
        z.writestr("_rels/.rels", RELS)
        for n, c in (extra or {}).items():
            z.writestr(n, c)


class T(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_valid_package_ok(self):
        p = os.path.join(self.d, "a.docx"); make(p); dp.check_package(p)

    def test_stray_file_detected(self):
        p = os.path.join(self.d, "b.docx"); make(p, {"wt_list.txt": "debug"})
        with self.assertRaises(ValueError) as c:
            dp.check_package(p)
        self.assertIn("wt_list.txt", str(c.exception))

    def test_repair_removes_stray_and_keeps_text(self):
        p = os.path.join(self.d, "c.docx"); make(p, {"wt_list.txt": "debug"})
        removed = dp.repair(p, os.path.join(self.d, "c2.docx"))
        self.assertEqual(removed, ["wt_list.txt"])
        dp.check_package(os.path.join(self.d, "c2.docx"))
        with zipfile.ZipFile(os.path.join(self.d, "c2.docx")) as z:
            self.assertEqual(z.read("word/document.xml").decode(), DOC)
            self.assertEqual(z.namelist()[0], "[Content_Types].xml")

    def test_dangling_relationship_detected(self):
        p = os.path.join(self.d, "e.docx")
        bad = RELS.replace("</Relationships>", '<Relationship Id="rId2" Type="x" Target="word/absent.xml"/></Relationships>')
        with zipfile.ZipFile(p, "w") as z:
            z.writestr("[Content_Types].xml", CTX); z.writestr("_rels/.rels", bad); z.writestr("word/document.xml", DOC)
        with self.assertRaises(ValueError):
            dp.check_package(p)

    def test_orphan_tasks_removed(self):
        p = os.path.join(self.d, "f.docx")
        ct = CTX.replace("</Types>", '<Override PartName="/word/documenttasks/documenttasks1.xml" ContentType="application/vnd.ms-office.documenttasks+xml"/></Types>')
        drel = ('<?xml version="1.0"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                '<Relationship Id="rId9" Type="http://schemas.microsoft.com/office/2019/05/relationships/documenttasks" Target="documenttasks/documenttasks1.xml"/></Relationships>')
        with zipfile.ZipFile(p, "w") as z:
            z.writestr("[Content_Types].xml", ct); z.writestr("_rels/.rels", RELS); z.writestr("word/document.xml", DOC)
            z.writestr("word/_rels/document.xml.rels", drel); z.writestr("word/documenttasks/documenttasks1.xml", "<t/>")
        dp.check_package(p)  # valide tel quel
        removed = dp.repair(p, os.path.join(self.d, "f2.docx"))
        self.assertIn("word/documenttasks/documenttasks1.xml", removed)
        dp.check_package(os.path.join(self.d, "f2.docx"))


if __name__ == "__main__":
    unittest.main()
