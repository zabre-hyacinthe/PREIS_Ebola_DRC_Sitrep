# -*- coding: utf-8 -*-
"""Insere (idempotent) le bloc « deck » dans le workflow et les regles .gitignore. Usage (racine du depot) :
python3 scripts/python/deck/install_deck_workflow.py [.github/workflows/preis_sitrep_monitor_v2.yml]"""
import os
import sys

BLOCK = '''      # PREIS DECK START
      # Deck BDBV (diapositives) mis a jour a chaque nouveau SitRep : lecture du PDF, remplissage du gabarit, verification
      # independante, envoi par mail, etat anti-doublon. Gabarit confidentiel : coffre chiffre (depot public).
      # continue-on-error : n'interrompt jamais le reste du pipeline.
      - name: Deck BDBV - dependances (poppler-utils, python-pptx)
        if: always()
        continue-on-error: true
        timeout-minutes: 5
        run: |
          sudo apt-get install -y poppler-utils
          python3 -m pip install --quiet python-pptx

      - name: Deck BDBV - restaurer le gabarit (coffre chiffre)
        if: always()
        continue-on-error: true
        timeout-minutes: 3
        run: |
          python3 scripts/python/private_store.py restore \\
            --store data/africa_cdc_deck/private_store.enc \\
            --dir data/africa_cdc_deck/templates || echo "coffre deck non restaure (non bloquant)"

      - name: Deck BDBV - generer, verifier, envoyer (run_deck.py)
        if: always() && env.PREIS_SEND_EMAIL == 'true'
        continue-on-error: true
        timeout-minutes: 12
        env:
          PREIS_DECK_TO: ${{ secrets.PREIS_DECK_TO }}
        run: |
          if [ -f scripts/python/deck/run_deck.py ]; then
            python3 scripts/python/deck/run_deck.py ${{ github.event_name == 'workflow_dispatch' && inputs.africa_cdc_mode == 'test' && '--test' || '' }} || echo "deck en erreur (non bloquant)"
          else
            echo "run_deck.py absent - skip"
          fi

      - name: Deck BDBV - committer l'etat anti-doublon
        if: always()
        continue-on-error: true
        run: |
          set +e
          git config user.name "PREIS Ebola Automation"
          git config user.email "actions@github.com"
          git add data/africa_cdc_deck/state.json data/africa_cdc_deck/email_sent_state.csv data/africa_cdc_deck/alerts.csv 2>/dev/null
          git diff --cached --quiet && echo "Pas de changement d'etat deck" && exit 0
          git commit -m "PREIS deck: etat anti-doublon [skip ci]"
          for i in 1 2 3; do
            git pull --rebase --autostash origin main && git push && exit 0
            sleep 5
          done
          echo "push de l'etat deck impossible (risque de renvoi au prochain passage)"
          exit 0
      # PREIS DECK END
'''
IGN = ["", "# deck BDBV : gabarit confidentiel (coffre chiffre) et sorties privees", "data/africa_cdc_deck/templates/", "outputs/deck_private/", "outputs/deck_private_test/"]


def _read(fp):
    b = open(fp, "rb").read()
    crlf = b"\r\n" in b
    return b.decode("utf-8").replace("\r\n", "\n"), crlf


def _write(fp, text, crlf):
    open(fp, "wb").write((text.replace("\n", "\r\n") if crlf else text).encode("utf-8"))


def main():
    wf = sys.argv[1] if len(sys.argv) > 1 else ".github/workflows/preis_sitrep_monitor_v2.yml"
    s, crlf = _read(wf)
    if "PREIS DECK START" in s:
        print("deja installe : %s" % wf)
    else:
        mk = "      # PREIS WATCHDOG START"
        if mk not in s:
            sys.exit("repere « PREIS WATCHDOG START » introuvable dans %s : rien modifie" % wf)
        s = s.replace(mk, BLOCK + mk, 1)
        _write(wf, s, crlf)
        print("bloc deck insere dans %s (fins de ligne %s conservees)" % (wf, "CRLF" if crlf else "LF"))
    gi = ".gitignore"
    cur, gcrlf = _read(gi) if os.path.isfile(gi) else ("", False)
    add = [l for l in IGN if l and l not in cur.splitlines()]
    if add:
        extra = "\n".join([""] + IGN[1:2] + [l for l in IGN[2:] if l in add]) + "\n"
        if cur and not cur.endswith("\n"):
            extra = "\n" + extra
        _write(gi, cur + extra, gcrlf)
        print(".gitignore complete : %s" % ", ".join(add))


if __name__ == "__main__":
    main()
