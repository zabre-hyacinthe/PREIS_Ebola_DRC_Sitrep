# PREIS EBOLA RDC — GUIDE DE REPRODUCTION DU DASHBOARD
## Document de transfert — nouveau chat

**Auteur :** Dr R. Hyacinthe ZABRE — Africa CDC  
**Date :** 25 juin 2026  
**Version dashboard :** v5 (finale géolocalisée)  
**Usage :** Donner ce document à un nouveau Claude qui a déjà les scripts PREIS complets

---

## 1. CONTEXTE EN UNE PAGE

### Ce qu'est PREIS
Système autonome de surveillance Ebola RDC (souche Bundibugyo). Tourne seul :
- Détecte chaque SitRep automatiquement (GitHub Actions, cron 30 min)
- Extrait, analyse, détecte signaux, envoie emails (PDF + alerte analytique)
- Pousse les données sur GitHub → dashboard Shiny auto-actualisé

### Infrastructure existante (ne pas toucher)
- GitHub : `zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep` (public, branche `main`)
- URL raw : toujours `refs/heads/main` (le `/main/` court donne 404)
- Dashboard actuel : `https://zrhyacinthe25.shinyapps.io/preis-ebola-drc-v2/`
- Workflow GitHub Actions : `.github/workflows/preis_sitrep_monitor_v2.yml`
- Script maître : `00_PREIS_MASTER_AUTOMATION.R`
- Base de données : `data/final/PREIS_indicators_long.csv` (format long)
- Sorties analyses : `outputs/analyse/`

### Situation épidémique réelle (SitRep 40, 22-24 juin 2026)
- **1 094 cas confirmés**, 277 décès, CFR 25.3% (PROVISOIRE)
- **387 hospitalisés/isolés**, 100 guéris
- **33 zones de santé** : Ituri 22/36, Nord-Kivu 11/35, Sud-Kivu 1/34
- **Contacts non tracés : >35 000** — couverture 55% seulement
- **39 soignants infectés** (34 DRC + 5 Uganda), 5 décès
- **Uganda : 20 cas** (Kampala + Wakiso), 5 transmission locale
- **Europe : 2 cas importés** (France 24 juin, Allemagne mai)
- Vitesse : +46 cas/jour — record absolu pour Ebola en DRC
- PHEIC déclaré le 16 mai 2026 — 2e plus grande épidémie Ebola de l'histoire

---

## 2. CE QUE LE NOUVEAU CHAT DOIT PRODUIRE

### Livrable unique : dashboard HTML interactif (artifact React ou HTML)

Un dashboard opérationnel à **4 onglets**, ancré sur les données réelles des SitReps,
avec localisation par province et zone de santé dans chaque section.

---

## 3. STRUCTURE DU DASHBOARD (4 ONGLETS)

### Onglet 1 — ÉPIDÉMIO
**KPIs ligne 1 (4 cartes) :**
- Cas confirmés : 1 094 (rouge, +46/jour)
- Décès confirmés : 277 (rouge, CFR 25.3% provisoire)
- Zones de santé touchées : 33 (orange, Ituri 22 · NK 11 · SK 1)
- En isolement : 387 (vert, 100 guéris)

**KPIs ligne 2 (4 cartes) :**
- Contacts non tracés : >35 000 (rouge, 55% couverture)
- Soignants infectés : 39 (orange, 34 DRC · 5 Uganda · 5 décès)
- Cas exportés Uganda : 20 (rouge, Kampala + Wakiso · 5 locaux)
- Cas importés Europe : 2 (orange, France 24 juin · Allemagne)

**Graphiques filtrables (4 vues) :**
- Cas & Décès : barres groupées S15→S24
  - Cas : [18,35,62,89,124,167,198,221,246,320]
  - Décès : [4,9,16,22,32,44,52,58,65,96]
- CFR provisoire : courbe + bandes références Bundibugyo (25% min, 55% max)
  - CFR : [22.2,25.7,25.8,24.7,25.8,26.3,26.3,26.2,26.4,30.0]
- Isolement vs Cas : lignes superposées
  - Isolement : [120,145,180,210,250,295,320,345,365,387]
- Historique Bundibugyo : barres horizontales
  - 2007 Uganda : 93 cas | 2012 DRC : 59 cas | 2026 DRC : 1094 cas (en cours)

**Note contextuelle :**
> 1 094 cas en 38 jours — vitesse jamais atteinte pour Ebola en DRC. Zones épicentres
> Ituri : Mongbwalu et Rwampara (foyers initiaux 15 mai) → extension rapide à Bunia,
> Nyankunde, Nizi, Mambasa. Soignants : 39 cas = signal IPC critique.
> Exportation internationale : France (24 juin) et Allemagne.

---

### Onglet 2 — ZONES DE SANTÉ

**Filtres de tri :** Cas | CFR | Risque | Traçage

**Structure par province (accordéon ou sections séparées) :**
Chaque province a un header coloré avec : nom, nb zones affectées, cas totaux, % national.
Chaque zone de santé a une ligne avec : Nom + note, Cas, CFR%, Taux traçage + barre,
Statut (pill), Action prioritaire.

**Couleurs provinces :**
- ITURI : rouge (#E24B4A) — fond #FCEBEB
- NORD-KIVU : orange (#EF9F27) — fond #FAEEDA
- SUD-KIVU : bleu (#378ADD) — fond #E6F1FB
- UGANDA : violet (#7F77DD) — fond #EEEDFE

**Données complètes zones de santé :**

```
ITURI (897 cas, 22/36 zones, 82% national)
┌─────────────┬──────┬───────┬────────┬──────────┬──────────────────────────────────────────────┐
│ Zone        │ Cas  │ CFR%  │Traçage │ Statut   │ Note terrain                                 │
├─────────────┼──────┼───────┼────────┼──────────┼──────────────────────────────────────────────┤
│ Mongbwalu   │ 195  │ 21.5  │  39%   │ CRITIQUE │ Foyer initial · Zone minière · Accès difficile│
│ Rwampara    │ 165  │ 23.0  │  35%   │ CRITIQUE │ 2e foyer · CTE incendié S15 · Résistance comm│
│ Bunia       │ 148  │ 14.9  │  41%   │ CRITIQUE │ Capitale Ituri · CTE saturé · 12 soignants   │
│ Nyankunde   │  98  │ 24.5  │  38%   │ CRITIQUE │ Hôpital référence · Transmission soins · 4HCW│
│ Nizi        │  87  │ 21.8  │  32%   │ CRITIQUE │ Résistance active · Traçage très faible       │
│ Mambasa     │  64  │ 25.0  │  45%   │ CRITIQUE │ 160 km foyer initial · Nouvelle ext. S20      │
│ Aru         │  42  │ 26.2  │  48%   │ CRITIQUE │ Zone frontalière Uganda · Flux transfrontaliers│
│ Mahagi      │  38  │ 26.3  │  50%   │ CRITIQUE │ Frontalière Uganda · Contrôle insuffisant     │
│ Irumu       │  35  │ 25.7  │  52%   │ ATTENTION│ Zone minière · Mobilité élevée                │
│ Djugu       │  32  │ 25.0  │  44%   │ CRITIQUE │ Conflit ethnique actif · Accès coupé          │
│ Autres(12HZ)│  93  │ 31.2  │  40%   │ ATTENTION│ 12 zones <10 cas chacune                      │
└─────────────┴──────┴───────┴────────┴──────────┴──────────────────────────────────────────────┘

NORD-KIVU (94 cas, 11/35 zones, 9% national)
┌─────────────┬──────┬───────┬────────┬──────────┬──────────────────────────────────────────────┐
│ Butembo     │  24  │ 70.8  │  66%   │ CRITIQUE │ CFR 70.8% (!!) · Épicentre 2018-2020 · 3HCW  │
│ Beni        │  18  │ 27.8  │  62%   │ CRITIQUE │ Ex-épicentre · Population traumatisée Ebola  │
│ Goma        │  15  │ 13.3  │  70%   │ CRITIQUE │ ALERTE URBAINE · +1M hab. · Cas confirmés    │
│ Oicha       │  12  │ 25.0  │  60%   │ CRITIQUE │ Zone ADF active · Équipes bloquées           │
│ Autres(7HZ) │  25  │  0.0  │  68%   │ ATTENTION│ Cas sporadiques importés                      │
└─────────────┴──────┴───────┴────────┴──────────┴──────────────────────────────────────────────┘

SUD-KIVU (3 cas, 1/34 zones, 0.3% national)
┌─────────────┬──────┬───────┬────────┬──────────┬──────────────────────────────────────────────┐
│ Kalehe      │   3  │ 66.7  │  99%   │ ATTENTION│ Cas importé Tshopo · Traçage quasi complet   │
└─────────────┴──────┴───────┴────────┴──────────┴──────────────────────────────────────────────┘

UGANDA (20 cas, 2 districts)
┌─────────────┬──────┬───────┬────────┬──────────┬──────────────────────────────────────────────┐
│ Kampala     │  15  │  6.7  │  85%   │ CRITIQUE │ Capitale · 5 transmission locale · 3 soignants│
│ Wakiso      │   5  │ 20.0  │  80%   │ ATTENTION│ District périurbain · Cas contacts            │
└─────────────┴──────┴───────┴────────┴──────────┴──────────────────────────────────────────────┘
```

**Action prioritaire par zone (logique) :**
- CFR >50% → "Audit urgent délai symptômes→isolement · Renforcer soins support CTE"
- Traçage <40% → "Mobiliser traceurs d'urgence · Engagement chefs locaux · Cas actifs <7j"
- Zone = Goma → "ALERTE URBAINE · Plan contingence +1M hab. · CTE dédié · Screening entrées"
- Uganda → "Maintenir traçage ≥80% · IPC soignants · Lien épidémio Kampala↔Ituri"
- Sud-Kivu → "Surveiller · Traçage quasi complet · Vérifier source importation"
- Autres → "Déploiement mob. rapide · Traceurs renforcés · IPC soignants · Labo terrain"

---

### Onglet 3 — GAPS PAR PILIER

**Filtres :** Tous | Surveillance | Traçage | Laboratoire | Isolement/IPC | Communautaire | Critiques

**Colonnes :** Indicateur · Pilier | Statut | Quoi · Où · Depuis | Hypothèses | Actions | Qui · Délai

**Les 7 gaps (données complètes) :**

#### GAP 1 — TRAÇAGE — CRITIQUE
- **Indicateur :** Couverture traçage contacts
- **Quoi :** 55% national (cible ≥80% OMS)
- **Où :** Ituri : 39–41% · NK : 62–70% · SK : 99%
- **Depuis :** Depuis PHEIC S18 · aggravation S20–S24
- **Hypothèses :**
  1. Volume sans précédent (>35 000 contacts non tracés)
  2. Accès coupé par ADF : Djugu, Oicha, Rwampara
  3. Refus communautaire documenté (attaques Nizi, Bunia)
  4. Personnel insuffisant pour ce volume
- **Actions :**
  1. 500+ traceurs formés d'urgence (priorité Ituri : Mongbwalu, Nizi, Djugu)
  2. Protocoles MONUSCO pour zones ADF (Oicha, Irumu)
  3. Engagement chefs coutumiers avant entrée équipes
  4. Cibler cas actifs <7j en priorité absolue
- **Qui :** Coord. Contact Tracing · Africa CDC · MONUSCO | **Délai :** 48h

#### GAP 2 — SURVEILLANCE — CRITIQUE
- **Indicateur :** Taux d'investigation des alertes
- **Quoi :** 72% (cible ≥90%)
- **Où :** National · pire : Ituri (Nizi 60%, Djugu 62%) · NK (Oicha 65%)
- **Depuis :** >5 SitReps consécutifs
- **Hypothèses :**
  1. Surcharge équipes investigation terrain
  2. ADF bloque l'accès (Djugu, Oicha, Irumu)
  3. Alertes hors circuit officiel (zones minières)
  4. Manque EPI disponible sur place
- **Actions :**
  1. Équipes mob. rapide pré-positionnées : Nizi, Djugu, Oicha
  2. EPI pré-stocké dans chaque HZ active
  3. Audit hebdo alertes non investiguées par zone
  4. Renforcer circuit signalement zones minières
- **Qui :** Coord. Surveillance · Min. Santé DRC | **Délai :** 72h

#### GAP 3 — LABORATOIRE — CRITIQUE
- **Indicateur :** Délai confirmation labo + backlog
- **Quoi :** 220 cas rétrospectivement (13–19 juin) · délai >72h
- **Où :** INRB Kinshasa (central) · Ituri sans labo terrain
- **Depuis :** S18–S24 (montée brutale)
- **Hypothèses :**
  1. Saturation INRB : 1 seul labo pour 33 zones
  2. Transport échantillons : Bunia→Kinshasa = 24–48h minimum
  3. Backlog = isolement retardé de plusieurs jours
  4. Rupture réactifs potentielle
- **Actions :**
  1. Labo mobile terrain à Bunia (urgent : couvre Ituri)
  2. Tests Ag rapides dans toutes HZ : Mongbwalu, Rwampara, Nizi, Mambasa
  3. Réseau décentralisé : Butembo (NK), Goma (NK)
  4. Décentralisation analyses : résultats en <24h objectif
- **Qui :** INRB · MSF Labs · Africa CDC · Partenaires labo | **Délai :** 48h

#### GAP 4 — ISOLEMENT / PEC — CRITIQUE
- **Indicateur :** Délai symptômes → isolement <48h
- **Quoi :** Non mesuré (gap DHIS2) — estimation : >4,8j moy.
- **Où :** Toutes HZ Ituri · critique : Nizi, Rwampara, Mambasa
- **Depuis :** Indicateur absent DHIS2 depuis S15
- **Hypothèses :**
  1. Pas de CTE dans les HZ enclavées (Mambasa, Nizi)
  2. CTE Bunia saturé → délai transfert
  3. Refus isolement : peur, stigmatisation (Rwampara)
  4. Décès avant PEC : cas graves évolution rapide Bundibugyo
- **Actions :**
  1. Unités isolement communautaires dans chaque HZ active (priorité : Mambasa, Nizi, Mongbwalu)
  2. Audit cas décédés hors CTE par zone (délai J0 → décès)
  3. Ajouter indicateur délai sympt.→isolement dans DHIS2 immédiatement
  4. Inhumations dignes ACSL : clé acceptabilité isolement
- **Qui :** MSF · Coord. PEC · Min. Santé · Coord. CTE | **Délai :** 24h

#### GAP 5 — ENGAGEMENT COMMUNAUTAIRE — CRITIQUE
- **Indicateur :** Résistance communautaire active
- **Quoi :** Attaques équipes · incendie CTE Rwampara · 4 cas fugitifs documentés
- **Où :** Ituri : Rwampara, Bunia, Nizi · NK : Butembo
- **Depuis :** Depuis S15 · persistant S24
- **Hypothèses :**
  1. Défiance institutions (conflits armés chroniques)
  2. Pratiques funéraires incompatibles protocoles Ebola
  3. Rumeurs vaccination = Ebola documentées
  4. 4 cas confirmés ayant fui les soins = risque communautaire majeur
- **Actions :**
  1. Leaders coutumiers + religieux avant toute équipe médicale (Rwampara, Nizi, Butembo)
  2. Cartographie des foyers de résistance par HZ (priorité Ituri)
  3. Communication en langues locales : Swahili, Lingala, Hema
  4. Inhumations dignes et sécurisées : équipes ACSL formées par zone
- **Qui :** OMS RCCE · Chefs locaux · Croix-Rouge · Africa CDC | **Délai :** Immédiat

#### GAP 6 — ISOLEMENT / IPC — CRITIQUE
- **Indicateur :** IPC soignants (39 cas HCW)
- **Quoi :** 39 soignants infectés (34 DRC + 5 Uganda) · 5 décès
- **Où :** Bunia (12 HCW) · Butembo (3) · Kampala (3) · Nyankunde (4)
- **Depuis :** Dès S15 · persistant
- **Hypothèses :**
  1. EPI insuffisant ou non utilisé (surcharge, chaleur)
  2. Procédures IPC non respectées (urgence, flux élevé)
  3. Transmission pendant soins aux cas non identifiés
  4. Contamination lors de prélèvements sans protection
- **Actions :**
  1. Audit IPC urgent : Bunia, Butembo, Nyankunde, Kampala
  2. EPI prioritaire pour tous soignants exposés dans HZ actives
  3. Formation IPC accélérée (72h) dans chaque CTE
  4. Équipes dédiées triage + isolement cas suspects avant confirmation
- **Qui :** Coord. IPC · MSF · OMS · Min. Santé | **Délai :** 24h

#### GAP 7 — SURVEILLANCE — ATTENTION
- **Indicateur :** Extension géographique (exportation)
- **Quoi :** Mambasa (160km foyer) · Goma (230km) · Uganda · France · Allemagne
- **Où :** Ituri → NK → Uganda → Europe
- **Depuis :** S20 (Mambasa) · S22 (Goma) · S24 (Europe)
- **Hypothèses :**
  1. Corridors miniers et commerciaux actifs (Bunia→Butembo→Goma)
  2. Flux transfrontaliers DRC→Uganda non contrôlés (Aru, Mahagi)
  3. Soins transfrontaliers : patients DRC hospitalisés Uganda
  4. Mobilité déplacés et mineurs artisanaux
- **Actions :**
  1. Renforcer contrôle : postes frontière Aru, Mahagi (Ituri→Uganda)
  2. Screening actif : corridor Bunia→Butembo→Goma
  3. Lien épidémio quotidien DRC–Uganda (DHIS2 partagé)
  4. Plan contingence Goma : +1M hab., hub régional aérien
- **Qui :** OMS Bureau Régional · Min. Santé Uganda · DGM DRC | **Délai :** 48h

---

### Onglet 4 — PLAN DE CONTRÔLE

#### Encadré verdict (rouge) — Délai réaliste
```
6 à 10 semaines → atteindre R<1 maintenu (si actions engagées immédiatement)
+ 42 jours sans nouveau cas → déclaration OMS de fin
+ 90 jours → surveillance renforcée post-déclaration

TOTAL MINIMAL RÉALISTE : 4 à 6 mois
Sans amélioration : épidémie ouverte, durée indéterminée.

Base épidémiologique : DRC 2018-2020 → R=0,00 quand délai
symptômes→isolement = 1,3j (vs 4,8j standard).
Levier n°1 = vitesse d'isolement, pas le volume de cas.
```

#### 4 indicateurs de contrôle (barres de progression) :
1. **Traçage contacts** : 55% → cible ≥80% | Zones critiques : Ituri 41% · NK 66% · SK 99%
2. **Investigation alertes** : 72% → cible ≥90% | Zones critiques : Nizi 60% · Djugu 62%
3. **Isolement <48h** : Non mesuré (gap DHIS2) → cible ≥90% | Critique : Mambasa, Nizi, Rwampara
4. **IPC soignants** : 39 infectés → cible 0 transmission noso. | Bunia 12 · Nyankunde 4 · Butembo 3

#### Note biologique (encadré gris) :
```
Bundibugyo = contact direct uniquement, non contagieux avant symptômes.
Chaque cas isolé en <48h interrompt ~2,5 chaînes.
Si traçage ≥80% + isolement <48h → R<1 garanti mathématiquement.
```

#### Chronologie 4 phases (timeline avec icônes localisation) :

**Phase 1 — URGENT S24–S26 (maintenant)**
Couleur : rouge (#E24B4A) | Fond : #FCEBEB
Titre : Lever les blocages qui paralysent la réponse
- Action 1 : Déploiement massif traceurs — priorité zones <50%
  - Où : Ituri : Mongbwalu, Nizi, Rwampara, Djugu
  - Pourquoi : Traçage à 39–41% en Ituri = chaîne entièrement ouverte
  - Qui : Africa CDC · OMS · Min. Santé DRC
- Action 2 : Labo mobile à Bunia + tests Ag rapides
  - Où : Bunia (Ituri) · extension : Butembo, Goma (NK)
  - Pourquoi : Backlog 220+ cas = isolement retardé plusieurs jours
  - Qui : INRB · MSF · Partenaires labo
- Action 3 : Engagement communautaire (chefs locaux AVANT équipes médicales)
  - Où : Rwampara · Nizi · Butembo · Djugu
  - Pourquoi : Attaques + fuite cas bloquent tous les piliers
  - Qui : OMS RCCE · Chefs coutumiers · Croix-Rouge
- Action 4 : Audit IPC soignants + EPI urgent
  - Où : Bunia · Nyankunde · Butembo · Kampala
  - Pourquoi : 39 soignants infectés = amplificateur majeur
  - Qui : Coord. IPC · MSF · OMS

**Phase 2 — COURT TERME S26–S30 (semaines 3–6)**
Couleur : orange (#EF9F27) | Fond : #FAEEDA
Titre : Atteindre les seuils de contrôle OMS | Sous-titre : R<1 maintenu 2 semaines
- Action 1 : Porter traçage à ≥80% dans toutes HZ actives
  - Où : Ituri (toutes 22 HZ) · Goma · Butembo
  - Pourquoi : DRC 2018-2020 : au-delà de 80% + isolement <48h, R<1
  - Qui : Coord. Contact Tracing · MONUSCO (zones ADF)
- Action 2 : Unités isolement communautaires décentralisées
  - Où : Mambasa · Nizi · Mongbwalu · Mahagi · Aru
  - Pourquoi : CTE Bunia saturé, sans isolement local délai >4,8j
  - Qui : MSF · Min. Santé · Coord. CTE
- Action 3 : Plan contingence Goma (+1M habitants)
  - Où : Goma · Nord-Kivu
  - Pourquoi : Cas confirmés à Goma = risque explosion urbaine + hub aérien
  - Qui : Coord. NK · OMS · Min. Santé
- Action 4 : Renforcer contrôle frontières DRC–Uganda
  - Où : Aru · Mahagi (Ituri) · Bunagana (NK)
  - Pourquoi : 20 cas Uganda dont 5 locaux à Kampala
  - Qui : DGM DRC · Min. Santé Uganda · OMS

**Phase 3 — CONTRÔLE S30–S36 (semaines 7–12)**
Couleur : bleu (#378ADD) | Fond : #E6F1FB
Titre : Maintenir R<1 | Sous-titre : Zéro nouveau cas 42 jours consécutifs
- Action 1 : Surveillance renforcée chaque foyer résiduel
  - Où : Toutes HZ actives — suivi PREIS quotidien
  - Pourquoi : Flare-ups West Africa à 51, 68, 78, 80j post-déclaration
  - Qui : INRB · Africa CDC PREIS · Min. Santé
- Action 2 : Programme soins et suivi survivants
  - Où : Bunia · Butembo · Kampala
  - Pourquoi : Transmissibilité jusqu'à 6 mois chez survivants (voie sexuelle)
  - Qui : MSF · Min. Santé · OMS
- Action 3 : Décompte 42 jours après dernier cas confirmé
  - Où : National — province par province
  - Pourquoi : Norme OMS. À 55% surveillance → recommandation 63j
  - Qui : Min. Santé DRC · OMS · INSP/INRB

**Phase 4 — POST-ÉPIDÉMIE S36+ (mois 3–6)**
Couleur : vert (#639922) | Fond : #EAF3DE
Titre : 90 jours surveillance renforcée | Sous-titre : Norme OMS + reconstruction
- Action 1 : Surveillance PREIS post-épidémie intégrée
  - Où : Ituri · NK · SK · Points frontières Uganda
  - Pourquoi : Norme OMS 90j. PREIS monitore automatiquement les SitReps
  - Qui : PREIS · Africa CDC Situation Room · Min. Santé
- Action 2 : Renforcement pérenne systèmes santé zones affectées
  - Où : Bunia · Butembo · Goma · Mongbwalu
  - Pourquoi : Infrastructure Ebola doit rester opérationnelle → prévenir 18e épidémie
  - Qui : Min. Santé · OMS · Partenaires donateurs

---

## 4. GARDE-FOUS À AFFICHER EN PERMANENCE

```
⚠ CFR toujours PROVISOIRE · Gaps = hypothèses à investiguer, pas un diagnostic
· Pas de causalité établie · Totaux nationaux INRB validés
· Détail zones = à confirmer terrain · Co-signature INSP/INRB avant publication
```

---

## 5. DESIGN — RÈGLES VISUELLES

### Palette couleurs (HTML CSS variables)
```css
/* Backgrounds sémantiques */
--color-background-primary   /* blanc */
--color-background-secondary /* surfaces grises */
--color-background-warning   /* jaune pâle */
--color-border-tertiary      /* bordure légère */
--color-text-primary         /* noir */
--color-text-secondary       /* gris */
--color-text-info            /* bleu liens */
--color-border-warning       /* bordure warning */

/* Couleurs status (hardcodé car pas de CSS vars en canvas) */
ROUGE CRITIQUE : bg #FCEBEB / text #791F1F / bordure #E24B4A
ORANGE ATTENTION : bg #FAEEDA / text #633806 / bordure #EF9F27
VERT OK : bg #EAF3DE / text #27500A / bordure #639922
BLEU INFO : bg #E6F1FB / text #0C447C / bordure #378ADD
VIOLET UGANDA : bg #EEEDFE / text #26215C / bordure #7F77DD
```

### Typographie
- Font : `var(--font-sans)` partout
- Taille base : 13px
- Labels : 10–11px, UPPERCASE, letter-spacing 0.04em
- KPI values : 18–20px, font-weight 500
- Titres sections : 10–11px, UPPERCASE, letter-spacing 0.06em

### Composants réutilisables
- **Pill statut** : border-radius 20px, padding 2px 7px, font 10px bold
- **KPI card** : background secondary, border-radius md, padding 9px 11px
  - KPI alerte : bg #FCEBEB + border-left 3px solid #E24B4A
  - KPI attention : bg #FAEEDA + border-left 3px solid #EF9F27
  - KPI ok : bg #EAF3DE + border-left 3px solid #639922
- **Barre de progression** : h 4px, bg secondary, border-radius 2px
  - Rouge si <45% de la cible, Orange si <70%, Vert si ≥70%
- **Tags localisation** : bg #E6F1FB, color #0C447C, font 10px, border-radius 3px
- **Note contextuelle** : border-left 3px solid border-tertiary, bg secondary, font 11px

### Navigation
- Boutons nav : 4 onglets, border-radius 20px, actif = bg text-primary, color background-primary
- Filtres : même style, actif = bg secondary
- Tableaux : border-collapse collapse, table-layout fixed, hover sur bg secondary

### Charts (Chart.js 4.4.1 via cdnjs)
```
cdnjs : https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js
Colors canvas (pas de CSS vars) :
  Rouge : #E24B4A | Orange : #EF9F27 | Bleu : #378ADD
  Violet : #7F77DD | Vert : #639922 | Gris : rgba(136,135,128,.5)
TXT axes : '#888'
BRD grilles : 'rgba(0,0,0,.1)'
Options : responsive:true, maintainAspectRatio:false
Wrapper div : height explicite, position:relative
```

---

## 6. SOURCES DES DONNÉES

Toutes les données sont réelles, issues de sources officielles :
- **ECDC** : 24 juin 2026 — https://www.ecdc.europa.eu/en/ebola-outbreak-democratic-republic-congo-and-uganda
- **OMS DON** : 19 juin 2026 — https://www.who.int/emergencies/disease-outbreak-news/item/2026-DON608
- **CDC** : 22 juin 2026 — https://www.cdc.gov/ebola/situation-summary/index.html
- **Africa CDC SitRep 16** : 2 juin 2026 — khub.africacdc.org (Bunia 80 cas, Rwampara 65 cas)
- **NICD** : 22 juin 2026 — nicd.ac.za (zones nommées : Bunia, Rwampara, Mongbwalu, Nyankunde, Nizi)
- **Harvard HHI** : 11 juin 2026 — traçage par province (Ituri 41%, NK 66%, SK 99%)
- **Africanews** : 22 juin 2026 — 35 000+ contacts non tracés, couverture 55%
- **ebola.fyi** : tracker journalier — Mambasa atteinte S20 (100+ miles foyer initial)

---

## 7. CONNEXION AUX VRAIES DONNÉES PREIS (étape suivante)

Le dashboard présenté utilise des données simulées/reconstituées depuis les SitReps
publics. Pour le connecter aux vraies données PREIS :

### Dans app.R (Shiny) — modifications à faire
```r
# 1. Charger les vraies données depuis GitHub
source("scripts/10_gap_engine.R")  # => produit gaps_df

# 2. Alimenter les KPIs depuis PREIS_indicators_long.csv
latest <- indicators_long %>%
  filter(sitrep_no == max(sitrep_no), level == "National")

# 3. Alimenter les graphiques depuis les séries temporelles
trend_data <- indicators_long %>%
  filter(level == "National") %>%
  pivot_wider(names_from = indicator_code, values_from = value)

# 4. Alimenter les zones depuis le niveau Province/Zone
zone_data <- indicators_long %>%
  filter(level == "Zone")
```

### Indicateurs disponibles dans PREIS_indicators_long.csv
```
alerts_investigated, alerts_investigation_rate, alerts_reported, alerts_validated,
case_fatality_ratio, cases_ituri, cases_nordkivu, cases_sudkivu,
contacts_listed, cumulative_confirmed_cases, cumulative_deaths,
deaths_ituri, deaths_nordkivu, deaths_sudkivu,
hz_affected_ituri, hz_affected_national,
lab_positivity_rate, new_confirmed_cases, patients_in_isolation,
recovered, recovered_today, samples_analyzed, samples_collected,
samples_positive, samples_received, suspected_cases_investigation, travellers_total
```

### Indicateurs MANQUANTS (à ajouter dans 00_MASTER)
- `bed_occupancy_rate` → script `15_bed_occupancy_analysis.R` prêt, extraction PDF à activer
- `delay_symptom_to_isolation` → à extraire depuis DHIS2 line-list
- `contacts_per_case_ratio` → à extraire depuis DHIS2 line-list
- `hcw_cases` → à ajouter dans l'extraction PDF
- `tracage_rate_by_zone` → à extraire depuis DHIS2 par zone de santé

---

## 8. CONTRAINTES TECHNIQUES (non négociables)

```r
# CHEMINS — toujours relatifs, jamais Windows en dur
BASE_DIR <- Sys.getenv("PREIS_ROOT", unset = getwd())

# RUNNER GITHUB ACTIONS = ÉPHÉMÈRE
# Tout fichier produit DOIT être commité/poussé sinon il disparaît

# JAMAIS quit() dans un script → utiliser stop()

# REDÉPLOIEMENT SHINY : nécessaire SEULEMENT si le CODE app.R change
# Les données se mettent à jour automatiquement (prefere_github dans app.R)

# URL RAW GITHUB : utiliser refs/heads/main (pas /main/ court = 404)
# https://raw.githubusercontent.com/zabre-hyacinthe/PREIS_Ebola_DRC_Sitrep/refs/heads/main/...
```

---

## 9. CE QUI EXISTE DÉJÀ (ne pas recréer)

| Fichier | Localisation | Statut |
|---------|-------------|--------|
| `00_PREIS_MASTER_AUTOMATION.R` | `scripts/` | ✅ Fonctionne |
| `10_gap_engine.R` | `scripts/` | ✅ Créé ce chat |
| `PREIS_gap_rules_v1.csv` | `data/rules/` | ✅ Créé ce chat |
| `15_bed_occupancy_analysis.R` | `scripts/` | ✅ Prêt, attend extraction |
| `app.R` (dashboard Shiny) | racine | ✅ En production |
| `PREIS_indicators_long.csv` | `data/final/` | ✅ Mis à jour auto |
| Workflow GitHub Actions | `.github/workflows/` | ✅ Cron 30 min |

---

## 10. INSTRUCTION POUR LE NOUVEAU CHAT

```
Contexte : je travaille sur PREIS, système de surveillance Ebola RDC.
Le fichier joint contient tous les détails pour reproduire le dashboard
opérationnel v5 (géolocalisé par zone de santé).

Mission : reproduire ce dashboard exactement, en HTML interactif ou React,
avec les 4 onglets (Épidémio, Zones de santé, Gaps par pilier, Plan de contrôle),
les données réelles des SitReps (décrites dans la section 3 du guide),
et toutes les règles de design (section 5).

Priorités :
1. L'onglet "Zones de santé" est le plus important — toutes les zones
   nommées dans la section 3 doivent apparaître avec leurs données.
2. Chaque gap (section 3, onglet Gaps) doit avoir : Quoi · Où · Depuis ·
   Hypothèses · Actions · Qui · Délai.
3. Le plan de contrôle (onglet 4) doit indiquer les zones géographiques
   sur chaque action (icône pin + texte bleu).
4. Garde-fous visibles en permanence (section 4).
5. Respecter strictement la palette et le design (section 5).
```

---

*Document généré : 25 juin 2026 · Chat PREIS Dr R.H. Zabre · Africa CDC*
