# PREIS - Deck BDBV automatique (livraison du 11/10/2026)

## Ce que fait le systeme
SitRep N (PDF dans data/pdf) -> `pdftotext` -> `sitrep_parse.py` (lecture stricte + recoupements) -> `deck_build.py` (remplit le gabarit, memes textes et memes regles que le deck 146 valide) -> `build_deck.py` (relit le .pptx enregistre et reverifie ~56 faits, ecrit `Controle_*.csv` ancien->nouveau) -> `send_deck.py` (mail) -> etat anti-doublon `data/africa_cdc_deck/email_sent_state.csv`. Orchestre par `run_deck.py`, appele par le workflow (bloc `PREIS DECK START/END`, avant le watchdog, `continue-on-error`).

## Regles de rigueur (code, pas intention)
- Aucune valeur inventee : champ obligatoire illisible ou incoherent (sommes par province, bandeau vs Tableau 1, echantillons, contacts, isolement) -> REFUS : pas de deck, une alerte operateur (une seule par cause).
- Champ optionnel illisible -> ND ou ancien texte CONSERVE et liste dans le mail ("non rafraichi").
- Comparaisons jour/jour : uniquement avec le SitRep dont la date de rapport = D-1 (PDF officiel). Absent -> refus.
- Arrondi "moitie vers le haut" comme les SitRep ; jamais "100 %" si a < b.
- Narratif (5 paragraphes libres, diapos 16-17) : optionnel (ANTHROPIC_API_KEY). Chaque nombre et nom propre doit exister dans le SitRep, longueur bornee ; sinon texte precedent conserve et signale. Sans cle : tout reste conserve et signale.
- Destinataires : ALERT_TO seulement, tant que le secret PREIS_DECK_TO n'est pas defini. Mode test (workflow_dispatch africa_cdc_mode=test) : jamais au-dela de ALERT_TO, aucun etat modifie.

## Elements toujours statiques / conserves (non fournis par le SitRep)
Ouganda 20/2/18, agents de sante 266+4 / 49 (reglables dans state.json "config"), tuiles NEW HZ 7 jours / HCW / Total Affected (diapo 8), graphiques et cartes (a ajouter par vos scripts), bandeaux PoE quand le Tableau 4 est incomplet.

## Installation (une fois)
1. `git apply preis_deck_auto.patch` dans D:\PREIS_clean (ajoute scripts/python/deck, tests, workflow, .gitignore, state.json).
2. Gabarit = le deck 146 corrige (383/192) : copier dans `data\africa_cdc_deck\templates\deck_template.pptx`, puis `$env:PREIS_ARCHIVE_KEY="<meme cle que le secret>"; python scripts\python\private_store.py save --store data\africa_cdc_deck\private_store.enc --dir data\africa_cdc_deck\templates` ; committer `private_store.enc` et `.sha256` (le dossier templates est dans .gitignore).
3. Optionnel : secret `PREIS_DECK_TO` (destinataires du deck). Ajouter `private_store.enc` au `git add` si besoin (le bloc deck ne committe que state/sent/alerts ; le coffre ne change pas).

## Validation
- Tests : `python -m unittest discover -s scripts/python/tests -v` (17 tests ; end-to-end avec `PREIS_DECK_TEMPLATE=<deck>`).
- Essai a blanc local (poppler + python-pptx) : `python scripts\python\deck\run_deck.py --dry-run --narrative off` -> `outputs/deck_private/deck_mail.eml`.
- GitHub : Actions > Run workflow, africa_cdc_mode=test -> mail "[TEST]" a ALERT_TO ; verifier pieces jointes, `deck_report.json` (checks_failed vide), Controle_*.csv.
- Normal : apres un nouveau SitRep, un deck par SitRep ; relance du workflow = "deja envoye : rien a faire".
