PREIS EBOLA DRC SITREP — VERSION PRODUCTION R
================================================

Objectif
--------
Pipeline stable pour :
1) scraper les SitReps INSP 2026,
2) télécharger les PDF,
3) extraire texte + tableaux PDF,
4) produire des indicateurs validés,
5) bloquer les fausses valeurs par QC,
6) générer rapports TXT + Excel pour email/dashboard.

Installation minimale
---------------------
1. Installer R
2. Installer RStudio
3. Installer Rtools
4. Installer Java 64-bit si vous voulez utiliser tabulizer
5. Ouvrir RStudio et lancer :

   source("install_PREIS_dependencies.R")

Utilisation
-----------
1. Copier tout ce dossier dans :
   D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26/

2. Ouvrir RStudio.

3. Lancer :
   source("00_RUN_ALL_PRODUCTION.R")

Outputs principaux
------------------
- data/final/PREIS_indicator_candidates.csv
- data/final/PREIS_indicators_validated.csv
- data/final/PREIS_health_zones.csv
- data/final/PREIS_QC_by_sitrep.csv
- data/final/PREIS_QC_issues.csv
- outputs/PREIS_Report_LATEST_SitRep_XX.txt
- outputs/PREIS_Output_YYYYMMDD.xlsx

Point important
---------------
Cette version ne laisse plus une valeur fausse comme "cas cumulés = 2" entrer dans les calculs dérivés.
Si un cumul baisse par rapport au dernier cumul valide, la valeur est marquée invalidée et bloquée.
Les nouveaux cas/décès ne sont dérivés que si le SitRep précédent valide est consécutif.
