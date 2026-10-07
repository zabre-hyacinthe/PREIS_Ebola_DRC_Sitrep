# Procédure — Mise à jour du SitRep + Executive Brief Africa CDC (BVD) à partir du SitRep RDC (INSP)

Copie de référence vivant DANS LE DÉPÔT (lue à chaque exécution par
`scripts/06b_generate_africa_cdc_sitrep_brief_full.R`, exécuté automatiquement
par le pipeline GitHub Actions `preis_sitrep_monitor_v2.yml` dès qu'un nouveau
SitRep RDC est détecté par le système existant). Une copie miroir existe dans
les fichiers du projet claude.ai « PREIS Revision »
(`claude/PREIS_procedure_SitRep_AfricaCDC.md`) pour toute génération manuelle
en conversation ; les deux doivent rester identiques — modifier l'une, reporter
le changement dans l'autre.

Couvre **deux documents**, toujours mis à jour ensemble à partir du même
SitRep RDC : le **SitRep Africa CDC** (« Bundibugyo Virus Disease Outbreak —
Situation Report ») et l'**Executive Brief Africa CDC** associé. Les deux
partagent le MÊME numéro d'Issue (vérifié sur les cycles 130/138 et 131/141 —
le Brief n'a pas de numérotation séparée malgré l'apparence de son en-tête,
où le numéro est simplement scindé sur deux runs texte consécutifs dans le
.docx d'origine).

## RÔLE
Mettre à jour le SitRep Africa CDC ET l'Executive Brief Africa CDC (en
anglais) à partir du dernier SitRep RDC de l'INSP (« Rapport de Situation de
la 17e Épidémie MVE / RDC », SGI MVE17). Reprendre EXACTEMENT le modèle du
dernier SitRep/Brief Africa CDC et ne changer que les données. Même niveau de
rigueur et de professionnalisme pour les deux documents — ni l'un ni l'autre
n'est un sous-produit simplifié (c'est le rôle du « supplément quantitatif »
distinct généré par `06_generate_africa_cdc_sitrep_final.R`, qui ne couvre
que des indicateurs RDC et reste inchangé par cette procédure).

## SOURCES
1. SitRep RDC : en mode automatisé, le PDF le plus récent dans `data/pdf/`
   du dépôt (nommé `PREIS_DRC_Ebola_SitRep_<NNN>.pdf`), dont le numéro est
   déterminé par la même logique robuste que `06_generate_africa_cdc_sitrep_final.R`
   (registre `data/final/sitrep_registry.csv`, recalé sur
   `outputs/analyse/serie_temporelle_nationale.csv` si ce dernier est plus à
   jour). En conversation manuelle : le PDF joint, sinon le plus récent du
   flux https://insp.cd/category/sitrep/feed/. Le PDF est fourni nativement
   (lecture visuelle intégrée, tableaux et mise en page compris — y compris
   un texte présent dans la couche extractible du PDF mais non visible à
   l'affichage, anomalie déjà rencontrée sur le SitRep 141 : « 246 Aires de
   santé »).
2. Modèles : en mode automatisé, `data/africa_cdc_brief/templates/BVD_SitRep_latest_propre.docx`
   et `.../BVD_Executive_Brief_latest_propre.docx` (les versions « propre »
   du cycle précédent, remplacées après chaque génération réussie — jamais
   lors d'un échec). En conversation manuelle : les .docx joints, sinon le
   fichier du projet claude.ai au numéro le plus élevé
   (BVD_SitRep_<n°>_*.docx / BVD_Executive_Brief_<n°>_*.docx, en excluant les
   versions « _suivi_modifications »).
3. Ouganda : non couvert par le SitRep RDC. Garder 20 cas, 2 décès, 18 guéris
   (épidémie déclarée terminée le 28/07/2026), avec un commentaire, dans les
   deux documents, tant qu'aucune source RDC ne dit le contraire.

## CONTRÔLE DU NUMÉRO
Lire la ligne « Sources » du modèle SitRep (« DRC MVE SitRep No. X »). Si le
SitRep RDC a un numéro ≤ X : rien à produire (déjà traité). S'il manque un
numéro entre X et le nouveau : le signaler en commentaire (les chiffres 24h
ne couvrent que le dernier jour, mais les cumuls couvrent plusieurs jours).
Numéro Africa CDC (Issue) = numéro du modèle + 1, pour le SitRep ET pour le
Brief (même numéro, cf. note ci-dessus).

## CORRESPONDANCE RDC → SITREP AFRICA CDC
- En-tête : Issue No. ; « Reporting period through <Date du rapport> ·
  Published <Date de publication> » ; « CONTINENTAL KEY INDICATORS, AS OF
  <date> ».
- Latest update et Situation overview : faits saillants, résumé des points
  clés et Tableau 1 (nouveaux cas par province, décès communautaires +
  intra-CTE, zones et aires de santé touchées, cumuls, guéris, patients en
  isolement, part de l'Ituri).
- Indicateurs : cas, décès, létalité, guéris, en isolement (bandeau du
  SitRep RDC) ; totaux continentaux = RDC + Ouganda, létalité = décès / cas ;
  occupation des lits Ituri et Nord-Kivu (§2.5) ; suivi des contacts
  (§2.1.1) ; alertes 24h « reçues · vérifiées · suspects » (§2.1.1, comparées
  au Tableau 3) ; agents de santé infectés ou décédés seulement s'ils sont
  rapportés.
- Tableau par province : nouveaux cas ; nouveaux décès = communautaires +
  intra-CTE (Tableau 2) ; cas, décès (létalité) (Tableau 1) ; contacts à
  suivre et contacts vus (%) (§2.1.1) ; ligne totale.
- Response highlights :
  - Surveillance and PoE/PoC = §2.1 + §2.2 + Tableau 4
  - Laboratory, vaccination and care = §2.3 + §2.5 + §2.6
  - IPC/EDS and RCCE = §2.4 + §2.8
  - Coordination, continuity of care, psychosocial support, logistics,
    security and PSEA = §1 + §2.7 + §2.9 à §2.12
  - Pilier non rapporté ce jour-là : retirer la phrase de l'ancien SitRep,
    ne jamais la garder par défaut.
- Key challenges et Priorities : défis des piliers + messages clés du SitRep
  RDC, sans ajouter d'action absente de la source.
- Ligne Sources : « DRC MVE SitRep No. <n°>, reporting through <date> and
  published <date> ».

## CORRESPONDANCE RDC → EXECUTIVE BRIEF AFRICA CDC
Méthode établie par rétro-ingénierie des couples SitRep 138 → Issue 130 puis
vérifiée sur SitRep 141 → Issue 131 (deux cycles concordants).
- En-tête : « Situation Report · Issue No. <n° — même numéro que le SitRep> » ;
  « Reporting date <date SitRep RDC> · Published <date de génération> ».
- « Bottom line for decision-makers » : une phrase de synthèse (cas/décès/
  guéris cumulés continentaux + létalité), reprise/actualisée du modèle.
- Grille des indicateurs continentaux (« Continental cumulative indicators ») :
  chaque case = RDC (SitRep RDC, bandeau + Tableau 1/2) + Ouganda (valeur
  fixe, cf. SOURCES §3) ; létalité continentale recalculée = décès
  continentaux / cas continentaux ; part par province des nouveaux cas
  (ex. « Ituri 32 · North Kivu 17 · Haut-Uélé 4 ») reprise du Tableau 1 ;
  part communautaire/intra-CTE des décès du jour reprise du Tableau 2.
- Tableau « DRC province epidemiological profile » : nouveaux cas, cas
  cumulés, décès, létalité par province = Tableau 1 du SitRep RDC ; colonne
  « Historically affected HZs » = zones de santé historiquement touchées
  (Tableau 1 ou texte §2.x) ; colonne « Active HZs » (fenêtre 42 jours,
  astérisque) = **AUCUNE base dans le SitRep RDC à ce jour** (vérifié sur 138
  et 141) → ne jamais recalculer ni inventer cette colonne ; conserver les
  valeurs du modèle précédent avec commentaire « DONNÉE MANQUANTE — aucune
  base dans le SitRep RDC source, valeur de l'Issue précédente conservée
  sans modification ; à vérifier auprès de la source Africa CDC propre à cet
  indicateur ».
- « Situation at a glance » (4 puces : transmission, mortalité, vérification/
  investigation, suivi des contacts) : rédigées à partir de §2.1
  (vérification/investigation, suivi des contacts) et des Tableaux 1-2
  (transmission, mortalité) du SitRep RDC.
- « Response operations — operational snapshot » : paragraphe par pilier,
  construit directement sur §2.1 à §2.12 du SitRep RDC (même découpage par
  pilier que pour le SitRep Africa CDC ci-dessus) ; pilier non rapporté ce
  jour-là → retirer, jamais garder par défaut.
- « Risks to watch » / « Calls to action » (tableau 2 colonnes) : risques et
  actions reconstruits sur les chiffres et faits du SitRep RDC du jour
  (transmission, suivi des contacts, mortalité, incidents opérationnels
  §2.7-2.12) ; un item sans équivalent dans le nouveau SitRep RDC est retiré
  (jamais gardé par défaut, cf. règle « rien d'inventé ») ; la liste
  « no new cases » des calls-to-action est recalculée à partir du Tableau 1
  du jour (une province à 0 nouveau cas dans l'ancien SitRep peut en avoir
  dans le nouveau, et inversement).
- Ligne « Prepared by » / Source : reprendre les noms du modèle ; mettre à
  jour la référence « Source : DRC MVE SitRep No. <n°> ».

## RÈGLE ABSOLUE : RIEN D'INVENTÉ (SitRep ET Brief)
- Chaque chiffre vient du SitRep RDC, ou d'un calcul simple sur ses chiffres
  (signalé en commentaire « VALEUR CALCULÉE — <détail> »).
- Donnée absente ou sans base dans la source RDC (ex. Active HZs du Brief,
  agents de santé infectés si non rapportés, Ouganda) : garder l'ancienne
  valeur avec le commentaire « DONNÉE MANQUANTE — non mise à jour », ou
  retirer la phrase.
- Contradiction entre texte et tableau de la source RDC : retenir le texte et
  signaler l'écart en commentaire. Contradiction interne à la source RDC
  elle-même (deux passages qui se contredisent) : signaler les deux valeurs
  en commentaire sans trancher à la place de la source.
- Anomalie de la source RDC elle-même (erreur arithmétique, texte présent
  dans le PDF mais non visible à l'écran, etc.) : signaler en commentaire, NE
  PAS corriger silencieusement le chiffre affiché par Africa CDC — reporter
  l'anomalie telle qu'elle est dans la source, flaguée.
- Source illisible ou gabarit structurellement incompatible (ex. nombre de
  runs insuffisant, structure XML inattendue empêchant l'édition
  positionnelle fiable) : s'arrêter, ne rien livrer/envoyer de fabriqué, et
  le dire clairement (en mode automatisé : le script s'arrête sans mettre à
  jour l'état ni les gabarits, voir § AUTOMATISATION).

## MISE EN FORME ET MENTIONS PERMANENTES (ne jamais modifier)
- La ligne « Prepared by » cite « Dr. R. Hyacinthe ZABRE » juste après « Dr. Merawi
  Aragaw » (mention définitive). Quand tu édites cette ligne (sources, numéro de SitRep
  RDC), conserve tous les noms ; ne retire jamais celui-ci.
- Les paragraphes de texte sont justifiés et le contenu des tableaux de données est
  centré (sauf la première colonne). Tu ne modifies que le texte des runs, jamais
  l'alignement ni la mise en forme.
- Tableau « CONTINENTAL KEY INDICATORS » (tuiles) : tous les grands chiffres sont en
  gras et le texte est justifié. Quand tu remplaces un chiffre, garde le run (donc le gras).

## LONGUEUR DE L'EXECUTIVE BRIEF : 2 PAGES (limite stricte)
L'Executive Brief tient sur DEUX pages. Le gabarit actuel est calibré pour cela : le texte
total du Brief ne doit pas dépasser environ 10 900 caractères (tous paragraphes confondus ;
le gabarit en compte environ 10 600). Au-delà de 11 800 caractères le document est refusé.
- Ne rallonge JAMAIS une cellule narrative par rapport au gabarit : si tu ajoutes une
  information, retire-en une moins prioritaire. Les cellules narratives les plus longues
  (RCCE/continuité des soins/SMSPS, IPC/EDS/logistique/sécurité/PSEA) restent ≤ 1 550 caractères.
- Pour raccourcir, SUPPRIME des détails secondaires (chiffres par site, séances individuelles,
  stocks, actions déjà décrites ailleurs) ; ne reformule pas avec de nouvelles informations
  et n'invente rien. Garde en priorité : cas, décès, létalité, suivi des contacts, hospitalisation,
  laboratoire, vaccination, risques et actions prioritaires.
- Ces limites sont propres au Brief ; le SitRep a sa propre limite (section suivante).

## LONGUEUR DU SITREP AFRICA CDC : 2 PAGES MAXIMUM (limite stricte, demande du propriétaire 07/10/2026)
Dr Merawi : « not more than 2 and a half pages » ; le propriétaire a fixé la cible à 2 PAGES pour le SitRep
comme pour le Brief. Le gabarit hérité fait 3 à 4 pages (17 800–19 300 caractères) : il faut le RACCOURCIR.
- Mesure : nombre total de caractères de texte du document (même mesure que le Brief). Visé : ≈ 9 500.
  Avertissement au-dessus de 10 000 (≈ 2 pages) ; document REFUSÉ au-delà de 10 800.
  Chaque paragraphe reçoit un budget individuel (« BUDGET DE LONGUEUR » du prompt) ; les lignes de crédit
  et de sources sont exemptées. Les paragraphes vides avant la ligne « Prepared by » sont compactés
  automatiquement (un seul conservé) pour éviter une ligne orpheline en page 3.
- Raccourcis d'abord le narratif : « Response highlights » (4 paragraphes les plus longs), « Key challenges »,
  « Priorities ». Fusionne les phrases redondantes, retire les détails secondaires (chiffres par site,
  séances individuelles, stocks, actions déjà décrites ailleurs). Ne reformule pas avec de nouvelles
  informations, n'invente rien.
- Ne retire JAMAIS un chiffre clé : tuiles, tableau par province, cas, décès, létalité, guéris, patients en
  isolement, suivi des contacts, alertes, laboratoire, vaccination, risques et actions prioritaires.
- Même une fois sous la limite, ne rallonge jamais un paragraphe par rapport au cycle précédent.

## AUCUN JOUR MANQUANT (demande Dr Merawi, 07/10/2026)
« Let's not miss a day — there is no 3rd of October and 30 of September. » La série des Issues est
quotidienne : un Issue par SitRep RDC, dans l'ordre chronologique.
- Le pipeline traite le SitRep RDC non traité le plus ANCIEN dont le PDF est disponible (rattrapage
  chronologique), puis les suivants aux cycles suivants. Il ne saute plus un jour parce qu'un SitRep plus
  récent est apparu entre deux cycles.
- La date de l'Issue est celle du rapport figurant dans le SitRep RDC traité (jamais la date du jour).
- Un SitRep RDC ABSENT de la série (ex. No. 142 du 03/10, non publié par l'INSP) ne peut pas être inventé :
  signale « jour manquant dans la série quotidienne » en commentaire ET dans la liste « anomalies », et
  n'utilise aucune donnée d'un autre jour pour le combler.
- Les chiffres « dernières 24 h » ne couvrent que le dernier jour ; les cumuls couvrent tous les jours.

## HIÉRARCHIE DES SOURCES (règle absolue)
Le SitRep RDC traité fait TOUJOURS foi. Toute valeur du SitRep RDC prime sur le gabarit,
sur l'Issue précédente et sur ta mémoire : si une valeur reportée ou un total du gabarit
contredit le SitRep RDC, la valeur du SitRep RDC est utilisée (puis signalée en anomalie
si la source elle-même est incohérente : tu la reprends alors telle quelle, sans la
corriger de toi-même). Une valeur reportée du cycle précédent (Ouganda, agents de santé,
zones de santé incluant l'Ouganda, colonne « Active HZs ») n'est conservée que si le
SitRep RDC ne dit rien à ce sujet ; tout total qui la combine à une valeur du SitRep RDC
(ex. total continental = RDC + Ouganda, zones affectées = RDC + Ouganda) est RECALCULÉ à
partir des valeurs RDC du SitRep traité.

## CONCORDANCE SITREP / BRIEF (bloquante)
Les deux documents dérivent du MÊME SitRep RDC : tout chiffre commun est identique.
- Chaque chiffre vient du SitRep RDC. Seules exceptions : les valeurs externes reportées
  du cycle précédent (Ouganda, agents de santé infectés/décédés, zones de santé
  affectées incluant l'Ouganda, colonne « Active HZs ») : elles doivent être IDENTIQUES
  dans les deux documents et signalées dans les commentaires (anomalies) comme « non
  présentes dans le SitRep RDC ».
- Recalcule toute valeur dérivée (totaux continentaux = RDC + Ouganda, létalités, %).
- N'invente jamais un pourcentage absent de la source ; si la source est incohérente
  (ex. PSEA), reprends la formulation de la source sans le pourcentage contesté et
  signale-le dans les anomalies.
- Contrôle automatique après construction (concordance.py) : tuiles de même libellé,
  lignes de province (nouveaux cas/décès, cas, décès, létalité) et totaux RDC doivent
  être identiques ; sinon le cycle échoue (re-essai, puis alerte).

## CONTRÔLES AVANT LIVRAISON (SitRep ET Brief)
Somme des provinces = total (nouveaux cas, nouveaux décès, cumuls, contacts) ;
létalités et % recalculés à une décimale ; patients en isolement par
province = total ; format anglais des nombres (8,376 ; 48.3%) ; aucune date
ni aucun numéro de l'ancien cycle ne subsiste ; cohérence entre le SitRep et
le Brief du même cycle (mêmes chiffres clés, même date, même numéro de
SitRep RDC source, même numéro d'Issue).

## PRODUCTION
1. Éditer le .docx du modèle (dézipper, puis modifier word/document.xml) en
   gardant mise en page, styles, tableaux et gras des débuts de paragraphe —
   un seul `<w:t>` par `<w:r>` attendu dans les runs édités.
2. Chaque modification en suivi des modifications (auteur « Claude »), avec
   commentaires en français : données manquantes, incohérences de la source,
   valeurs calculées, numéros manquants.
   (Mode automatisé : seule la version finale, sans suivi, est envoyée par e-mail, sous
   les noms BVD_SitRep_<n°>_<jj>_<Month><aaaa>.docx et BVD_Executive_Brief_<...>.docx ;
   la version suivi sert au contrôle d'intégrité. Chaque anomalie/donnée manquante doit
   donc figurer dans la liste « anomalies » de ta réponse : c'est elle qui est envoyée.)
3. Produire quatre fichiers (deux par document) :
   - BVD_SitRep_<n°>_<jj>_<Month><aaaa>_suivi_modifications.docx
   - BVD_SitRep_<n°>_<jj>_<Month><aaaa>_propre.docx
   - BVD_Executive_Brief_<n°>_<jj>_<Month><aaaa>_suivi_modifications.docx
   - BVD_Executive_Brief_<n°>_<jj>_<Month><aaaa>_propre.docx
4. Message court : SitRep RDC utilisé (n°, date), chiffres clés (nouveaux
   cas, nouveaux décès, cumul cas/décès, létalité, suivi des contacts), liste
   des données manquantes et des incohérences — pour les deux documents.
5. Les deux versions « propre » deviennent le gabarit du cycle suivant.

## AUTOMATISATION — intégrée au pipeline PREIS existant
Ajoutée au pipeline GitHub Actions `preis_sitrep_monitor_v2.yml`, à côté du
supplément quantitatif déjà en production (`06_generate_africa_cdc_sitrep_final.R`
/ `05_send_africacdc_sitrep_email.R`), avec le même fonctionnement (continue-on-error,
non bloquant pour le reste du pipeline) :

- **Détection (jamais modifiée, jamais dupliquée)** : la détection d'un
  nouveau SitRep RDC reste entièrement assurée par le système existant
  (`08_cloud_sitrep_monitor.R`). `06b_generate_africa_cdc_sitrep_brief_full.R`
  lit uniquement `data/final/sitrep_registry.csv` /
  `outputs/analyse/serie_temporelle_nationale.csv` (déjà produits par le
  pipeline existant) pour connaître le dernier SitRep RDC réellement
  disponible — même logique que `06_generate_africa_cdc_sitrep_final.R`.
- **Anti-doublon** : `data/africa_cdc_brief/state.csv`. Si le SitRep RDC le
  plus récent a déjà été traité (numéro ≤ au dernier enregistré), le script
  ne fait rien.
- **Génération** : `06b_generate_africa_cdc_sitrep_brief_full.R` lit les
  gabarits (`data/africa_cdc_brief/templates/`), extrait leurs runs
  (`scripts/python/extract_runs.py`), appelle l'API Claude (modèle et clé via
  `ANTHROPIC_MODEL` / secret `ANTHROPIC_API_KEY`) avec le PDF source natif +
  cette procédure complète + les listes de runs, reçoit un JSON d'edits,
  construit les 4 .docx via `scripts/python/run_build.py` (basé sur
  `scripts/python/africa_cdc_build_docx.py`).
- **Anomalies de fiabilité des données** (texte invisible, indicateur sans
  base traçable, incohérence interne à la source, etc.) : jamais bloquantes
  — signalées systématiquement en commentaire Word, les 4 fichiers sont
  livrés normalement. Seul un gabarit/source structurellement incompatible
  arrête le script SANS mettre à jour `state.csv` ni les gabarits (donc sans
  jamais envoyer un document fabriqué ou partiel).
- **Envoi** : `05b_send_africacdc_sitrep_brief_full_email.R`, étape séparée
  (comme pour le supplément quantitatif), avec son propre anti-doublon
  (`data/africa_cdc_brief/email_sent_state.csv`). Destinataires :
  `PREIS_AFRICACDC_TO` (ou `ALERT_TO` par défaut), mêmes identifiants SMTP
  que le reste du pipeline.
- **Persistance (dépôt public)** : `.gitignore` exclut `*.docx` et `/outputs/*`,
  et le dépôt est public : les gabarits (documents internes Africa CDC) ne sont
  donc JAMAIS commités en clair. Ils sont conservés dans une archive chiffrée
  (AES-256, `scripts/python/private_store.py`) `data/africa_cdc_brief/private_store.enc`,
  déchiffrée au début de l'exécution (étape « Restaurer le coffre ») avec le
  secret `PREIS_ARCHIVE_KEY` et rechiffrée à la fin (étape « Sauvegarder le
  coffre », sans nouveau commit si rien n'a changé). Le commit existant ajoute
  par chemins explicites : `state.csv`, `attempts.csv`, `email_sent_state.csv`,
  `private_store.enc(.sha256)`. Les livrables `.docx` ne sont envoyés que par
  e-mail (pièces jointes), jamais versionnés.
- **Échecs, alertes, modes** : un échec n'envoie jamais de document partiel ; il
  est compté dans `attempts.csv`. Après 3 échecs sur le même SitRep (ou dès
  qu'un problème de configuration est détecté : clé API, gabarits, coffre), UN
  e-mail d'alerte en français est envoyé à `ALERT_TO`, avec la cause probable.
  `PREIS_AFRICACDC_MODE` (lancement manuel du workflow) : `normal` ; `test`
  (essai complet, résultat envoyé uniquement à `ALERT_TO`, aucun état ni
  gabarit modifié) ; `retry` (remet le compteur d'échecs à zéro).
- **Archivage des SitRep RDC détectés** : déjà assuré par le système existant
  (`data/pdf/` du dépôt) — rien à dupliquer côté claude.ai pour cette
  automatisation (voir note dans `claude/africa_cdc_state.md` sur le projet
  claude.ai, qui ne sert plus que de suivi manuel/miroir).
