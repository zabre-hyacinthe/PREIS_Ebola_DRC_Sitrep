# -*- coding: utf-8 -*-
"""
Branche docx_package.py sur africa_cdc_build_docx.py (ajout en fin de fichier, idempotent,
fins de ligne conservees) : tout .docx produit est nettoye puis verifie ; un paquet invalide
fait echouer la generation (le document n'est jamais livre corrompu).
Usage : python scripts/python/install_docx_hygiene.py
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
TARGET = os.path.join(HERE, "africa_cdc_build_docx.py")
MARK = "# PREIS DOCX HYGIENE"
BLOCK = '''
{m} START (docx_package.py) -- ne pas modifier a la main
import docx_package as _dp

_zip_docx_raw = zip_docx
_verify_pair_raw = verify_pair


def zip_docx(src_dir, out_path):
    """Nettoie (parties orphelines, taches Word orphelines), zippe ([Content_Types].xml en premier), puis verifie."""
    removed = _dp.sanitize_dir(src_dir)
    if removed:
        print("docx : parties retirees du paquet -> %s" % ", ".join(removed))
    _dp.zip_dir(src_dir, out_path)
    _dp.check_package(out_path)


def verify_pair(propre_path, suivi_path):
    _dp.check_package(propre_path)
    _dp.check_package(suivi_path)
    return _verify_pair_raw(propre_path, suivi_path)
{m} END
'''.format(m=MARK)


def main():
    with open(TARGET, "rb") as f:
        raw = f.read()
    txt = raw.decode("utf-8")
    if MARK + " START" in txt:
        print("deja installe")
        return 0
    crlf = "\r\n" in txt
    block = BLOCK.replace("\n", "\r\n") if crlf else BLOCK
    if "def zip_docx" not in txt or "def verify_pair" not in txt:
        print("ERREUR : zip_docx / verify_pair introuvables dans %s" % TARGET)
        return 1
    if not txt.endswith("\n"):
        txt += "\r\n" if crlf else "\n"
    with open(TARGET, "wb") as f:
        f.write((txt + block).encode("utf-8"))
    print("hygiene docx installee dans %s%s" % (TARGET, " (CRLF conserves)" if crlf else ""))
    return 0


if __name__ == "__main__":
    sys.exit(main())
