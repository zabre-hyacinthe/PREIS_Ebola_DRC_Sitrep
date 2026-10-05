# -*- coding: utf-8 -*-
"""
Coffre chiffre pour les gabarits Africa CDC (.docx).

Pourquoi : .gitignore exclut *.docx et /outputs/*, et le depot est PUBLIC.
Les gabarits (documents internes Africa CDC) ne doivent pas etre publies en
clair, mais doivent survivre d'une execution GitHub Actions a la suivante.
Solution : une archive chiffree (AES-256-CBC, PBKDF2, via openssl) versionnee
dans le depot : data/africa_cdc_brief/private_store.enc

Usage :
  PREIS_ARCHIVE_KEY=... python3 private_store.py restore --store S.enc --dir templates/
  PREIS_ARCHIVE_KEY=... python3 private_store.py save    --store S.enc --dir templates/

restore : code 0 (restaure), 0 aussi si le coffre est absent (message "absent"), 
          4 si cle absente, 5 si dechiffrement impossible (mauvaise cle/corruption).
save    : archive deterministe (tri, mtime=0, uid=0) ; ne reecrit le coffre
          QUE si le contenu a change (hash du contenu clair stocke dans S.enc.sha256),
          afin de ne pas creer un commit inutile a chaque execution.
La cle n'est jamais affichee ni passee en argument (variable d'environnement
lue par openssl via -pass env:).
"""
import argparse
import hashlib
import io
import os
import subprocess
import sys
import tarfile
import tempfile

ENV = "PREIS_ARCHIVE_KEY"


def _openssl(args, data):
    env = dict(os.environ)
    p = subprocess.run(["openssl", "enc"] + args + ["-pbkdf2", "-iter", "200000", "-pass", "env:" + ENV],
                       input=data, capture_output=True, env=env)
    return p.returncode, p.stdout, p.stderr.decode("utf-8", "replace")


def pack(src_dir):
    buf = io.BytesIO()
    with tarfile.open(fileobj=buf, mode="w", format=tarfile.PAX_FORMAT) as tf:
        for name in sorted(os.listdir(src_dir)):
            fp = os.path.join(src_dir, name)
            if not os.path.isfile(fp) or name.startswith("."):
                continue
            ti = tarfile.TarInfo(name)
            ti.size = os.path.getsize(fp)
            ti.mtime = 0
            ti.mode = 0o644
            ti.uid = ti.gid = 0
            ti.uname = ti.gname = ""
            with open(fp, "rb") as f:
                tf.addfile(ti, f)
    return buf.getvalue()


def unpack(raw, dest):
    os.makedirs(dest, exist_ok=True)
    n = 0
    with tarfile.open(fileobj=io.BytesIO(raw), mode="r") as tf:
        for m in tf.getmembers():
            # garde anti path-traversal : fichiers reguliers a plat uniquement
            if not m.isreg() or "/" in m.name or "\\" in m.name or m.name in ("", ".", "..") \
                    or m.name.startswith("."):
                raise ValueError("membre d'archive refuse : %r" % m.name)
            data = tf.extractfile(m).read()
            tmp = os.path.join(dest, m.name + ".tmp")
            with open(tmp, "wb") as f:
                f.write(data)
            os.replace(tmp, os.path.join(dest, m.name))
            n += 1
    return n


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("action", choices=["restore", "save"])
    ap.add_argument("--store", required=True)
    ap.add_argument("--dir", required=True)
    a = ap.parse_args()
    if not os.environ.get(ENV):
        print("%s non defini : coffre ignore." % ENV, file=sys.stderr)
        return 4
    hash_fp = a.store + ".sha256"

    if a.action == "restore":
        if not os.path.exists(a.store):
            print("coffre absent (%s) : rien a restaurer." % a.store)
            return 0
        with open(a.store, "rb") as f:
            enc = f.read()
        rc, out, err = _openssl(["-d", "-aes-256-cbc"], enc)
        if rc != 0 or not out:
            print("dechiffrement impossible (mauvaise %s ou coffre corrompu) : %s" % (ENV, err.strip()[:200]),
                  file=sys.stderr)
            return 5
        try:
            n = unpack(out, a.dir)
        except Exception as e:
            print("archive illisible : %s" % e, file=sys.stderr)
            return 5
        print("coffre restaure : %d fichier(s) dans %s" % (n, a.dir))
        return 0

    # save
    if not os.path.isdir(a.dir) or not any(os.path.isfile(os.path.join(a.dir, x)) for x in os.listdir(a.dir)):
        print("rien a sauvegarder (dossier vide/absent) : coffre inchange.")
        return 0
    plain = pack(a.dir)
    digest = hashlib.sha256(plain).hexdigest()
    if os.path.exists(a.store) and os.path.exists(hash_fp):
        with open(hash_fp) as f:
            if f.read().strip() == digest:
                print("contenu inchange : coffre non reecrit.")
                return 0
    rc, enc, err = _openssl(["-e", "-aes-256-cbc", "-salt"], plain)
    if rc != 0 or not enc:
        print("chiffrement impossible : %s" % err.strip()[:200], file=sys.stderr)
        return 5
    d = os.path.dirname(os.path.abspath(a.store))
    os.makedirs(d, exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=d, suffix=".tmp")
    with os.fdopen(fd, "wb") as f:
        f.write(enc)
    os.replace(tmp, a.store)
    with open(hash_fp, "w") as f:
        f.write(digest + "\n")
    print("coffre mis a jour (%d octets chiffres)." % len(enc))
    return 0


if __name__ == "__main__":
    sys.exit(main())
