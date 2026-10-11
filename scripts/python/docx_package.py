# -*- coding: utf-8 -*-
"""
Hygiene du paquet .docx (OPC) : evite le message Word "Document endommage".

Cause demontree (validate.py du skill docx, sur les gabarits et le Brief 133) : un fichier
residuel `wt_list.txt` a la racine de l'archive, reference par AUCUNE relation et sans type
de contenu declare -> paquet OPC invalide -> Word en ligne (SharePoint) annonce "Document
endommage". Les gabarits SitRep portent en plus word/documenttasks (taches Word orphelines :
elles pointent des commentaires qui n'existent plus).

Ce module :
  - sanitize_dir(dir)      : retire du dossier decompresse les parties non referencees et les
                             taches orphelines (+ leur relation et leur type de contenu) ;
  - zip_dir(dir, out)      : zippe avec [Content_Types].xml en premier (convention OPC) ;
  - check_package(docx)    : leve ValueError si le paquet est invalide (partie orpheline,
                             relation vers une partie absente, partie sans type de contenu,
                             entree dupliquee, XML mal forme) ;
  - repair(src, dst)       : repare un .docx existant (ex. fichiers deja deposes dans SharePoint).
Usage CLI :  python docx_package.py check  fichier.docx
             python docx_package.py repair fichier.docx [sortie.docx]
"""
import os
import posixpath
import re
import shutil
import sys
import tempfile
import xml.etree.ElementTree as ET
import zipfile

CT = "[Content_Types].xml"
REL_NS = "{http://schemas.openxmlformats.org/package/2006/relationships}"
CT_NS = "{http://schemas.openxmlformats.org/package/2006/content-types}"
TASKS_REL = "documenttasks"


def _rels_files(names):
    return [n for n in names if n.endswith(".rels")]


def _owner_dir(rels_name):
    """'word/_rels/document.xml.rels' -> 'word' ; '_rels/.rels' -> ''."""
    d = posixpath.dirname(posixpath.dirname(rels_name))
    return d


def _targets(z_read, rels_name):
    """[(Id, Type, Target absolu dans l'archive, Mode)] d'un fichier .rels."""
    root = ET.fromstring(z_read(rels_name))
    out = []
    base = _owner_dir(rels_name)
    for r in root.iter(REL_NS + "Relationship"):
        tgt, mode = r.get("Target", ""), r.get("TargetMode", "Internal")
        if mode == "External":
            out.append((r.get("Id"), r.get("Type", ""), None, mode))
            continue
        full = tgt.lstrip("/") if tgt.startswith("/") else posixpath.normpath(posixpath.join(base, tgt))
        out.append((r.get("Id"), r.get("Type", ""), full, mode))
    return out


def analyse(names, read):
    """Retourne dict(unreferenced, dangling, no_ctype, dup, bad_xml)."""
    names_l = list(names)
    res = {"unreferenced": [], "dangling": [], "no_ctype": [], "dup": [], "bad_xml": []}
    seen = set()
    for n in names_l:
        if n in seen:
            res["dup"].append(n)
        seen.add(n)
    nameset = set(names_l)
    referenced = set()
    for rf in _rels_files(names_l):
        try:
            for _id, _t, full, mode in _targets(read, rf):
                if full is None:
                    continue
                referenced.add(full)
                if full not in nameset:
                    res["dangling"].append("%s -> %s" % (rf, full))
        except ET.ParseError:
            res["bad_xml"].append(rf)
    for n in names_l:
        if n == CT or n.endswith(".rels") or n.endswith("/"):
            continue
        if n not in referenced:
            res["unreferenced"].append(n)
    # types de contenu
    try:
        ct = ET.fromstring(read(CT))
        defaults = {d.get("Extension", "").lower() for d in ct.iter(CT_NS + "Default")}
        overrides = {o.get("PartName", "").lstrip("/") for o in ct.iter(CT_NS + "Override")}
        for n in names_l:
            if n == CT or n.endswith("/"):
                continue
            ext = n.rsplit(".", 1)[-1].lower() if "." in posixpath.basename(n) else ""
            if n not in overrides and ext not in defaults:
                res["no_ctype"].append(n)
    except (KeyError, ET.ParseError):
        res["bad_xml"].append(CT)
    for n in names_l:
        if n.endswith((".xml", ".rels")) and n not in res["bad_xml"]:
            try:
                ET.fromstring(read(n))
            except ET.ParseError:
                res["bad_xml"].append(n)
    return res


def check_package(docx_path):
    with zipfile.ZipFile(docx_path) as z:
        bad = z.testzip()
        if bad:
            raise ValueError("%s : entree corrompue (%s)" % (os.path.basename(docx_path), bad))
        r = analyse(z.namelist(), z.read)
    probs = ["%s : %s" % (k, ", ".join(v)) for k, v in r.items() if v]
    if probs:
        raise ValueError("%s : paquet .docx invalide (Word afficherait 'Document endommage') -> %s"
                         % (os.path.basename(docx_path), " | ".join(probs)))


def _read_dir(d):
    def read(n):
        with open(os.path.join(d, *n.split("/")), "rb") as f:
            return f.read()
    return read


def _list_dir(d):
    out = []
    for root, _dirs, files in os.walk(d):
        for fn in files:
            out.append(os.path.relpath(os.path.join(root, fn), d).replace(os.sep, "/"))
    return sorted(out)


def _drop_tasks(d):
    """Retire word/documenttasks/*, sa relation et son type de contenu."""
    removed = []
    for rf in _rels_files(_list_dir(d)):
        p = os.path.join(d, *rf.split("/"))
        with open(p, encoding="utf-8") as f:
            x = f.read()
        new = re.sub(r'<Relationship\b[^>]*Type="[^"]*%s[^"]*"[^>]*/>' % TASKS_REL, "", x)
        if new != x:
            with open(p, "w", encoding="utf-8") as f:
                f.write(new)
    tdir = os.path.join(d, "word", TASKS_REL)
    if os.path.isdir(tdir):
        removed += ["word/%s/%s" % (TASKS_REL, n) for n in os.listdir(tdir)]
        shutil.rmtree(tdir)
    p = os.path.join(d, CT)
    if os.path.isfile(p):
        with open(p, encoding="utf-8") as f:
            x = f.read()
        new = re.sub(r'<Override\b[^>]*PartName="[^"]*%s[^"]*"[^>]*/>' % TASKS_REL, "", x)
        if new != x:
            with open(p, "w", encoding="utf-8") as f:
                f.write(new)
    return removed


def sanitize_dir(d):
    """Nettoie un .docx decompresse. Retourne la liste des fichiers retires."""
    removed = _drop_tasks(d)
    for _ in range(3):  # retirer une partie orpheline peut en orpheliner une autre
        r = analyse(_list_dir(d), _read_dir(d))
        todo = r["unreferenced"]
        if not todo:
            break
        for n in todo:
            os.remove(os.path.join(d, *n.split("/")))
            removed.append(n)
    return removed


def zip_dir(src_dir, out_path):
    if os.path.exists(out_path):
        os.remove(out_path)
    files = _list_dir(src_dir)
    first = [n for n in (CT, "_rels/.rels") if n in files]
    rest = [n for n in files if n not in first]
    with zipfile.ZipFile(out_path, "w", zipfile.ZIP_DEFLATED) as z:
        for n in first + rest:
            z.write(os.path.join(src_dir, *n.split("/")), n)


def repair(src, dst=None):
    dst = dst or src
    tmp = tempfile.mkdtemp(prefix="docx_repair_")
    try:
        with zipfile.ZipFile(src) as z:
            z.extractall(tmp)
        removed = sanitize_dir(tmp)
        out = dst + ".tmp" if dst == src else dst
        zip_dir(tmp, out)
        check_package(out)
        if dst == src:
            os.replace(out, dst)
        return removed
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    if len(sys.argv) >= 3 and sys.argv[1] == "check":
        try:
            check_package(sys.argv[2]); print("OK : paquet valide")
        except ValueError as e:
            print("INVALIDE :", e); sys.exit(1)
    elif len(sys.argv) >= 3 and sys.argv[1] == "repair":
        rem = repair(sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else sys.argv[2].replace(".docx", "_repare.docx"))
        print("repare ; retire :", rem or "rien")
    else:
        print(__doc__); sys.exit(2)
