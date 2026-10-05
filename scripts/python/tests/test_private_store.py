import os, subprocess, sys, tempfile, unittest, tarfile, io
HERE = os.path.dirname(os.path.abspath(__file__))
PS = os.path.join(HERE, "..", "private_store.py")


def run(action, store, d, key="k1"):
    env = dict(os.environ)
    env.pop("PREIS_ARCHIVE_KEY", None)
    if key is not None:
        env["PREIS_ARCHIVE_KEY"] = key
    return subprocess.run([sys.executable, PS, action, "--store", store, "--dir", d],
                          capture_output=True, text=True, env=env)


class T(unittest.TestCase):
    def setUp(self):
        self.t = tempfile.mkdtemp()
        self.src = os.path.join(self.t, "src"); os.makedirs(self.src)
        self.store = os.path.join(self.t, "s", "p.enc")
        with open(os.path.join(self.src, "a.docx"), "wb") as f: f.write(b"AAA\x00BBB" * 100)
        with open(os.path.join(self.src, "b.docx"), "wb") as f: f.write(b"second")

    def test_roundtrip_and_encrypted(self):
        self.assertEqual(run("save", self.store, self.src).returncode, 0)
        self.assertNotIn(b"second", open(self.store, "rb").read())
        dst = os.path.join(self.t, "dst")
        r = run("restore", self.store, dst); self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual(open(os.path.join(dst, "b.docx"), "rb").read(), b"second")
        self.assertEqual(open(os.path.join(dst, "a.docx"), "rb").read(), b"AAA\x00BBB" * 100)

    def test_no_churn(self):
        run("save", self.store, self.src)
        b1 = open(self.store, "rb").read()
        r = run("save", self.store, self.src)
        self.assertIn("inchang", r.stdout)
        self.assertEqual(open(self.store, "rb").read(), b1)
        with open(os.path.join(self.src, "b.docx"), "wb") as f: f.write(b"changed")
        run("save", self.store, self.src)
        self.assertNotEqual(open(self.store, "rb").read(), b1)

    def test_wrong_key_and_missing_key(self):
        run("save", self.store, self.src)
        self.assertEqual(run("restore", self.store, os.path.join(self.t, "d"), key="bad").returncode, 5)
        self.assertEqual(run("restore", self.store, os.path.join(self.t, "d"), key=None).returncode, 4)

    def test_absent_store_ok(self):
        self.assertEqual(run("restore", self.store, os.path.join(self.t, "d")).returncode, 0)

    def test_empty_dir_does_not_wipe(self):
        run("save", self.store, self.src)
        b1 = open(self.store, "rb").read()
        empty = os.path.join(self.t, "e"); os.makedirs(empty)
        self.assertEqual(run("save", self.store, empty).returncode, 0)
        self.assertEqual(open(self.store, "rb").read(), b1)

    def test_path_traversal_refused(self):
        sys.path.insert(0, os.path.join(HERE, ".."))
        import private_store as ps
        buf = io.BytesIO()
        with tarfile.open(fileobj=buf, mode="w") as tf:
            ti = tarfile.TarInfo("../evil"); ti.size = 1; tf.addfile(ti, io.BytesIO(b"x"))
        with self.assertRaises(ValueError):
            ps.unpack(buf.getvalue(), os.path.join(self.t, "x"))
        self.assertFalse(os.path.exists(os.path.join(self.t, "evil")))


if __name__ == "__main__":
    unittest.main()
