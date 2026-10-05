# -*- coding: utf-8 -*-
"""
Générateur réutilisable — SitRep / Brief Africa CDC (BVD).

Applique un dict EDITS indexé par nœud à word/document.xml d'un gabarit .docx,
et produit une version "propre" (modifications acceptées) et une version
"suivi_modifications" (Suivi des modifications + commentaires, auteur "Claude"),
conformément à docs/africa_cdc/PREIS_procedure_SitRep_AfricaCDC.md.

Appelé par run_build.py (lui-même invoqué depuis
scripts/06b_generate_africa_cdc_sitrep_brief_full.R). Prérequis : le gabarit
(.docx dézippé) doit avoir exactement un <w:t> par <w:r> dans les runs
concernés — get_runs()/split_run() lèvent une erreur explicite sinon, pour ne
jamais produire un document corrompu silencieusement.
"""
import re
import os
import shutil
import zipfile
from xml.sax.saxutils import escape as xml_escape


def get_runs(doc_xml):
    runs = list(re.finditer(r'<w:r\b[^>]*>.*?</w:r>', doc_xml, re.DOTALL))
    return [r for r in runs if '<w:t' in r.group(0)]


def split_run(run_xml):
    """Return (open_tag, rpr_block_or_empty, t_attrs, text) for a <w:r>...</w:r> run
    containing exactly one <w:t>...</w:t>."""
    m = re.match(
        r'(<w:r\b[^>]*>)(<w:rPr>.*?</w:rPr>)?(<w:lastRenderedPageBreak/>)?<w:t([^>]*)>(.*?)</w:t></w:r>$',
        run_xml, re.DOTALL)
    if not m:
        raise ValueError("Unexpected run structure: " + run_xml[:200])
    r_open, rpr, t_attrs, text = m.group(1), m.group(2) or "", m.group(4), m.group(5)
    # lastRenderedPageBreak (group 3) intentionnellement ignoré lors de la
    # reconstruction : simple indice de rendu que Word recalcule à
    # l'ouverture, et invalide comme enfant direct de w:del/w:ins de toute façon.
    return r_open, rpr, t_attrs, text


def make_run(r_open, rpr, text, preserve=True):
    attrs = ' xml:space="preserve"' if preserve else ''
    return f'{r_open}{rpr}<w:t{attrs}>{xml_escape(text)}</w:t></w:r>'


def make_del_run(r_open, rpr, text):
    return f'{r_open}{rpr}<w:delText xml:space="preserve">{xml_escape(text)}</w:delText></w:r>'


def build(template_dir, edits, out_propre, out_suivi, comments_list_out, date_iso):
    """
    template_dir : dossier contenant un .docx dézippé (le gabarit de référence).
    edits        : {idx_run: {"new": str_ou_None, "comment": str_ou_None}}
    date_iso     : ex. "2026-10-05T00:00:00Z" — utilisé comme w:date pour ins/del/comments.
    """
    doc_path = os.path.join(template_dir, 'word', 'document.xml')
    with open(doc_path, encoding='utf-8') as f:
        doc_xml = f.read()

    runs = get_runs(doc_xml)
    if edits:
        assert len(runs) >= max(edits.keys()) + 1, \
            f"attendu >= {max(edits.keys()) + 1} runs, obtenu {len(runs)}"

    propre_xml = doc_xml
    suivi_xml = doc_xml
    ins_id = 9001
    comment_id = 0
    comments = []  # (id, author, date, text)

    # Traiter dans l'ordre d'index décroissant pour garder les offsets valides.
    for idx in sorted(edits.keys(), reverse=True):
        spec = edits[idx]
        run = runs[idx]
        start, end = run.start(), run.end()
        run_xml = run.group(0)
        r_open, rpr, t_attrs, old_text = split_run(run_xml)
        old_text_unescaped = (old_text.replace('&amp;', '&').replace('&lt;', '<')
                               .replace('&gt;', '>').replace('&quot;', '"').replace('&apos;', "'"))
        new_text = spec.get("new")
        comment_text = spec.get("comment")

        # --- version propre ---
        if new_text is not None:
            propre_run = make_run(r_open, rpr, new_text)
        else:
            propre_run = run_xml
        propre_xml = propre_xml[:start] + propre_run + propre_xml[end:]

        # --- version suivi ---
        pieces = []
        this_comment_id = None
        if comment_text:
            this_comment_id = comment_id
            comment_id += 1
            comments.append((this_comment_id, "Claude", date_iso, comment_text))
            pieces.append(f'<w:commentRangeStart w:id="{this_comment_id}"/>')

        if new_text is not None and new_text != old_text_unescaped:
            del_run = make_del_run(r_open, rpr, old_text_unescaped)
            ins_run = make_run(r_open, rpr, new_text)
            pieces.append(f'<w:del w:id="{ins_id}" w:author="Claude" w:date="{date_iso}">{del_run}</w:del>')
            ins_id += 1
            pieces.append(f'<w:ins w:id="{ins_id}" w:author="Claude" w:date="{date_iso}">{ins_run}</w:ins>')
            ins_id += 1
        else:
            pieces.append(run_xml)

        if this_comment_id is not None:
            pieces.append(f'<w:commentRangeEnd w:id="{this_comment_id}"/>')
            pieces.append(f'<w:r><w:rPr><w:rStyle w:val="CommentReference"/></w:rPr>'
                           f'<w:commentReference w:id="{this_comment_id}"/></w:r>')

        suivi_run = ''.join(pieces)
        suivi_xml = suivi_xml[:start] + suivi_run + suivi_xml[end:]

    # --- écrire l'arbre "propre" ---
    propre_dir = out_propre + "_src"
    if os.path.exists(propre_dir):
        shutil.rmtree(propre_dir)
    shutil.copytree(template_dir, propre_dir)
    with open(os.path.join(propre_dir, 'word', 'document.xml'), 'w', encoding='utf-8') as f:
        f.write(propre_xml)
    zip_docx(propre_dir, out_propre)

    # --- écrire l'arbre "suivi" (comments.xml + content-types + rels) ---
    suivi_dir = out_suivi + "_src"
    if os.path.exists(suivi_dir):
        shutil.rmtree(suivi_dir)
    shutil.copytree(template_dir, suivi_dir)
    with open(os.path.join(suivi_dir, 'word', 'document.xml'), 'w', encoding='utf-8') as f:
        f.write(suivi_xml)

    comment_paras = []
    for cid, author, date, text in comments:
        comment_paras.append(
            f'<w:comment w:id="{cid}" w:author="{xml_escape(author)}" w:date="{date}" w:initials="C">'
            f'<w:p><w:r><w:t xml:space="preserve">{xml_escape(text)}</w:t></w:r></w:p></w:comment>'
        )
    comments_xml = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                     '<w:comments xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">'
                     + ''.join(comment_paras) + '</w:comments>')
    with open(os.path.join(suivi_dir, 'word', 'comments.xml'), 'w', encoding='utf-8') as f:
        f.write(comments_xml)

    ct_path = os.path.join(suivi_dir, '[Content_Types].xml')
    with open(ct_path, encoding='utf-8') as f:
        ct = f.read()
    if '/word/comments.xml' not in ct:
        override = ('<Override PartName="/word/comments.xml" '
                     'ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"/>')
        ct = ct.replace('</Types>', override + '</Types>')
        with open(ct_path, 'w', encoding='utf-8') as f:
            f.write(ct)

    rels_path = os.path.join(suivi_dir, 'word', '_rels', 'document.xml.rels')
    with open(rels_path, encoding='utf-8') as f:
        rels = f.read()
    if 'comments.xml' not in rels:
        ids = [int(m) for m in re.findall(r'Id="rId(\d+)"', rels)]
        new_id = max(ids) + 1 if ids else 1
        rel = (f'<Relationship Id="rId{new_id}" '
               f'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments" '
               f'Target="comments.xml"/>')
        rels = rels.replace('</Relationships>', rel + '</Relationships>')
        with open(rels_path, 'w', encoding='utf-8') as f:
            f.write(rels)

    zip_docx(suivi_dir, out_suivi)

    with open(comments_list_out, 'w', encoding='utf-8') as f:
        for cid, author, date, text in comments:
            f.write(f"[{cid}] {text}\n\n")

    print(f"OK: {len(edits)} nœuds modifiés, {len(comments)} commentaires, {ins_id - 9001} runs ins/del.")
    print(f"-> {out_propre}")
    print(f"-> {out_suivi}")


def zip_docx(src_dir, out_path):
    if os.path.exists(out_path):
        os.remove(out_path)
    with zipfile.ZipFile(out_path, 'w', zipfile.ZIP_DEFLATED) as z:
        for root, dirs, files in os.walk(src_dir):
            for fn in files:
                full = os.path.join(root, fn)
                arc = os.path.relpath(full, src_dir)
                z.write(full, arc)


def unzip_docx(docx_path, dest_dir):
    if os.path.exists(dest_dir):
        shutil.rmtree(dest_dir)
    os.makedirs(dest_dir, exist_ok=True)
    with zipfile.ZipFile(docx_path) as z:
        z.extractall(dest_dir)
    return dest_dir


# ======================================================================
# Controles qualite (ajoutes pour la mise en production automatique)
# ======================================================================
import xml.etree.ElementTree as ET

W_NS = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
_XML_ILLEGAL = re.compile('[\x00-\x08\x0b\x0c\x0e-\x1f\ud800-\udfff￾￿]')
MAX_TEXT_LEN = 6000


def clean_text(s):
    """Retire les caracteres interdits en XML 1.0 (sinon Word refuse d'ouvrir le fichier)."""
    return _XML_ILLEGAL.sub('', s)


def count_runs(template_dir):
    with open(os.path.join(template_dir, 'word', 'document.xml'), encoding='utf-8') as f:
        return len(get_runs(f.read()))


def validate_edits(raw, n_runs, label):
    """
    raw    : dict {"12": {"new": str|None, "comment": str|None}, ...} (tel que recu du modele)
    Retourne (edits_propres {int: {"new","comment"}}, erreurs [str]).
    Strict : un index hors limites, un type inattendu ou un texte demesure est une ERREUR
    (jamais corrige silencieusement -- un index decale signifierait du contenu au mauvais endroit).
    """
    errors, clean = [], {}
    if not isinstance(raw, dict):
        return {}, ["%s : objet JSON attendu (cles = index de run)" % label]
    for k, spec in raw.items():
        try:
            idx = int(str(k).strip())
        except ValueError:
            errors.append("%s : cle non numerique %r" % (label, k))
            continue
        if not 0 <= idx < n_runs:
            errors.append("%s : index %d hors limites (le gabarit a %d runs)" % (label, idx, n_runs))
            continue
        if not isinstance(spec, dict):
            errors.append("%s[%d] : objet attendu" % (label, idx))
            continue
        new, com = spec.get("new"), spec.get("comment")
        ok = True
        for nm, v in (("new", new), ("comment", com)):
            if v is not None and not isinstance(v, str):
                errors.append("%s[%d].%s : texte ou null attendu" % (label, idx, nm))
                ok = False
            elif isinstance(v, str) and len(v) > MAX_TEXT_LEN:
                errors.append("%s[%d].%s : texte demesure (%d caracteres)" % (label, idx, nm, len(v)))
                ok = False
        if not ok:
            continue
        new = clean_text(new) if isinstance(new, str) else None
        com = clean_text(com).strip() if isinstance(com, str) else None
        if not com:
            com = None
        if new is None and com is None:
            continue  # entree vide : rien a faire
        clean[idx] = {"new": new, "comment": com}
    return clean, errors


def paragraph_texts(root):
    """Texte 'modifications acceptees' de chaque paragraphe (w:t uniquement : w:delText exclu)."""
    return ["".join(t.text or "" for t in p.iter(W_NS + "t")) for p in root.iter(W_NS + "p")]


def _load_doc(docx_path):
    with zipfile.ZipFile(docx_path) as z:
        bad = z.testzip()
        if bad:
            raise ValueError("%s : entree corrompue dans l'archive (%s)" % (os.path.basename(docx_path), bad))
        names = set(z.namelist())
        if "word/document.xml" not in names:
            raise ValueError("%s : word/document.xml absent" % os.path.basename(docx_path))
        root = ET.fromstring(z.read("word/document.xml"))
        comments = ET.fromstring(z.read("word/comments.xml")) if "word/comments.xml" in names else None
    return root, comments


def verify_pair(propre_path, suivi_path):
    """
    Verifie un couple (propre, suivi) et retourne la liste des paragraphes 'propre'.
    Invariants controles :
      - les deux archives sont valides et leur XML est bien forme ;
      - propre : aucune trace de suivi de modifications ni de commentaire ;
      - suivi  : en acceptant toutes les modifications on retrouve EXACTEMENT le texte
                 de 'propre' (aucune divergence possible entre les deux versions) ;
      - suivi  : chaque reference de commentaire a son commentaire, et inversement.
    Leve ValueError au premier probleme.
    """
    p_root, _ = _load_doc(propre_path)
    s_root, s_comments = _load_doc(suivi_path)
    for tag in ("ins", "del", "commentReference", "commentRangeStart"):
        if next(p_root.iter(W_NS + tag), None) is not None:
            raise ValueError("%s : contient <w:%s> alors que c'est la version propre" %
                             (os.path.basename(propre_path), tag))
    p_txt, s_txt = paragraph_texts(p_root), paragraph_texts(s_root)
    if p_txt != s_txt:
        diff = next((i for i, (a, b) in enumerate(zip(p_txt, s_txt)) if a != b), min(len(p_txt), len(s_txt)))
        raise ValueError("%s : le texte 'modifications acceptees' differe de la version propre "
                         "(premier ecart : paragraphe %d)" % (os.path.basename(suivi_path), diff))
    refs = {e.get(W_NS + "id") for e in s_root.iter(W_NS + "commentReference")}
    defined = {e.get(W_NS + "id") for e in s_comments.iter(W_NS + "comment")} if s_comments is not None else set()
    if refs != defined:
        raise ValueError("%s : commentaires incoherents (references=%d, definis=%d)" %
                         (os.path.basename(suivi_path), len(refs), len(defined)))
    ids = [e.get(W_NS + "id") for tag in ("ins", "del") for e in s_root.iter(W_NS + tag)]
    if len(ids) != len(set(ids)):
        raise ValueError("%s : identifiants de modifications dupliques" % os.path.basename(suivi_path))
    return p_txt


_ISSUE_RE = re.compile(r"Issue\s*No\.?\s*(\d+)", re.I)


def issue_numbers(paragraphs):
    found = set()
    for t in paragraphs:
        for m in _ISSUE_RE.finditer(t):
            found.add(int(m.group(1)))
    return found
