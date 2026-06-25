# GUIDE D'UPLOAD — SitRep 30 (à suivre dans l'ordre)

But : mettre le dashboard à jour (SitRep 30) ET rendre le système robuste
pour les prochains SitReps.

═══════════════════════════════════════════════════════════
ÉTAPE 0 — SAUVEGARDE (sécurité, 2 min)
═══════════════════════════════════════════════════════════
Sur GitHub, ce n'est pas nécessaire (l'historique Git garde tout).
Sur ton PC : copie tes 4 anciens scripts dans un dossier
scripts/_backup_20260615/  (au cas où).

═══════════════════════════════════════════════════════════
ÉTAPE 1 — UPLOADER LES 4 SCRIPTS (→ système robuste)
═══════════════════════════════════════════════════════════
Sur GitHub :
1. Va dans le dossier  scripts/
2. Clique "Add file" → "Upload files"
3. Glisse-dépose les 4 fichiers :
     - 00_PREIS_MASTER_AUTOMATION.R
     - 02_fetch_inrb_reference_data.R
     - 11_daily_indicators.R
     - 03_analyse_consolidee.R
4. En bas : "Commit changes" (bouton vert)

GitHub remplacera automatiquement les anciens par les nouveaux.

═══════════════════════════════════════════════════════════
ÉTAPE 2 — UPLOADER LES DONNÉES (→ dashboard à jour)
═══════════════════════════════════════════════════════════
Ces fichiers sont sur ton PC dans
D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26/

  A) Vers GitHub  outputs/analyse/
     - serie_temporelle_nationale.csv   ← LE PLUS IMPORTANT
     - tableau_zones_sante.csv

  B) Vers GitHub  data/final/
     - PREIS_indicators_long.csv
     - PREIS_daily_indicators.csv
     - INRB_reference_national.csv
     - PREIS_signals.csv
     - PREIS_signals_text.txt
     - PREIS_validation_signals.csv

⚠️ SI LE DOSSIER outputs/analyse/ N'EXISTE PAS SUR GITHUB :
   - "Add file" → "Upload files"
   - Glisse serie_temporelle_nationale.csv
   - DANS LE CHAMP DU NOM en haut, écris devant le fichier :
       outputs/analyse/serie_temporelle_nationale.csv
     (le "/" crée les dossiers automatiquement)
   - Commit

═══════════════════════════════════════════════════════════
ÉTAPE 3 — VÉRIFIER
═══════════════════════════════════════════════════════════
1. Ouvre ce lien (doit montrer le SitRep 30 / 2026-06-13 en bas) :
   https://raw.githubusercontent.com/zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep/refs/heads/main/outputs/analyse/serie_temporelle_nationale.csv

2. Ouvre le dashboard et fais Ctrl+Shift+R :
   https://zrhyacinthe25.shinyapps.io/preis-ebola-drc-v2/
   → le curseur SitRep doit aller jusqu'à 30, 782 cas.

═══════════════════════════════════════════════════════════
ÉTAPE 4 (optionnel) — TESTER LE CLOUD
═══════════════════════════════════════════════════════════
GitHub → onglet "Actions" → ton workflow → "Run workflow"
→ doit être vert. Confirme que le prochain SitRep sera géré seul.

═══════════════════════════════════════════════════════════
EN CAS DE PROBLÈME
═══════════════════════════════════════════════════════════
- Un script casse un run cloud → remets l'ancien (backup) en 30 s.
- Le dashboard ne bouge pas → Ctrl+Shift+R (cache navigateur).
- Lien raw en 404 → vérifie que le fichier est bien commité,
  attends 1-2 min (cache GitHub).
