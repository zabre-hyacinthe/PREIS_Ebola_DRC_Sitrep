# -*- coding: utf-8 -*-
"""
Extrait la liste des runs texte (<w:r><w:t>...) de word/document.xml d'un
.docx, dans l'ordre, sous forme de JSON [{"index": 0, "text": "..."}, ...].

Utilise les memes get_runs()/split_run() que africa_cdc_build_docx.py, pour
garantir que les index envoyes a l'API Claude correspondent EXACTEMENT a ceux
que run_build.py utilisera ensuite pour appliquer les edits (meme fonction,
meme gabarit, meme ordre).

Usage : python3 extract_runs.py chemin/vers/modele.docx
Sortie (stdout) : JSON sur une ligne.
"""
import json
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import africa_cdc_build_docx as bd


def main():
    if len(sys.argv) != 2:
        print("Usage: extract_runs.py <template.docx>", file=sys.stderr)
        sys.exit(2)
    docx_path = sys.argv[1]

    work = tempfile.mkdtemp(prefix="extract_runs_")
    template_dir = bd.unzip_docx(docx_path, os.path.join(work, "src"))
    doc_path = os.path.join(template_dir, "word", "document.xml")
    with open(doc_path, encoding="utf-8") as f:
        doc_xml = f.read()

    runs = bd.get_runs(doc_xml)
    out = []
    for i, run in enumerate(runs):
        try:
            _, _, _, text = bd.split_run(run.group(0))
        except ValueError as e:
            print(f"ERREUR extract_runs: run {i}: {e}", file=sys.stderr)
            sys.exit(1)
        text = (text.replace('&amp;', '&').replace('&lt;', '<')
                     .replace('&gt;', '>').replace('&quot;', '"').replace('&apos;', "'"))
        out.append({"index": i, "text": text})

    print(json.dumps(out, ensure_ascii=False))


if __name__ == "__main__":
    main()
