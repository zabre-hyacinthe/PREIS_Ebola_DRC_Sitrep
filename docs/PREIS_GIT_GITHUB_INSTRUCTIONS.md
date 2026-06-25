# PREIS Ebola DRC — Instructions Git, GitHub et GitHub Actions

Date de sauvegarde: 2026-06-13 09:27:19

## 1. Situation corrigée

Le script `scripts/08_cloud_sitrep_monitor.R` a été corrigé pour résoudre deux problèmes:

1. Erreur UTF-8 dans GitHub Action:

```r
Error in gsub("&amp;", "&", x, fixed = TRUE) :
  input string 1 is invalid UTF-8
```

2. Erreur de mauvais dossier racine:

```r
Production pipeline not found; skipping: C:/Users/AfricaCDC/OneDrive/Documents/00_RUN_ALL_PRODUCTION.R
```

La correction envoyée sur GitHub est confirmée par le commit:

```text
3e8f6be Fix UTF-8 and project root detection in SitRep monitor
```

## 2. Dossiers importants

Le vrai dépôt GitHub local est:

```r
D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep
```

L'ancien dossier de travail local est:

```r
D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26
```

Important: les corrections destinées à GitHub doivent être faites ou copiées dans:

```r
D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep
```

Si on modifie seulement l'ancien dossier de travail, GitHub ne recevra pas les changements tant qu'on ne fait pas copie + commit + push.

## 3. Vérifier Git dans RStudio

Après installation de Git, vérifier dans RStudio:

```r
Sys.which("git")
system("git --version")
```

Résultat attendu:

```text
git version 2.54.0.windows.1
```

## 4. Vérifier le dépôt GitHub local

Dans RStudio:

```r
setwd("D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep")

system("git status")
system("git log -1 --oneline")
```

Résultat attendu:

```text
On branch main
Your branch is up to date with 'origin/main'.
nothing to commit, working tree clean
```

## 5. Workflow normal après chaque correction

Toujours travailler dans le dépôt GitHub local:

```r
setwd("D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep")
```

Puis vérifier les changements:

```r
system("git status")
```

Ajouter, committer et pousser:

```r
system("git add .")
system('git commit -m "Message clair de correction"')
system("git push")
```

Exemple pour le monitor SitRep:

```r
system("git add scripts/08_cloud_sitrep_monitor.R")
system('git commit -m "Fix SitRep monitor"')
system("git push")
```

## 6. Tester localement le monitor SitRep

Tester le script avant de pousser:

```r
setwd("D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep")
source("scripts/08_cloud_sitrep_monitor.R")
```

Résultat normal si aucun nouveau SitRep:

```text
Latest online SitRep: 28
No new SitRep to send. Already sent SitRep: 28
```

## 7. Relancer GitHub Action

Après chaque push:

1. Aller sur GitHub.
2. Ouvrir le dépôt `PREIS_Ebola_DRC_Sitrep`.
3. Aller dans `Actions`.
4. Relancer le workflow du SitRep monitor.

Résultat attendu si tout va bien:

```text
Latest online SitRep: 28
No new SitRep to send. Already sent SitRep: 28
```

## 8. Si GitHub demande une connexion

Si une fenêtre `Connect to GitHub` apparaît:

1. Cliquer sur `Sign in with your browser`.
2. Se connecter au compte GitHub.
3. Cliquer sur `Authorize git-ecosystem`.
4. Retourner dans RStudio.
5. Relancer:

```r
system("git push")
```

## 9. Copier une correction depuis l'ancien dossier vers le dépôt GitHub

Si un script est corrigé dans l'ancien dossier, le copier vers le dépôt GitHub local:

```r
OLD_FILE <- "D:/PREIS_Ebola_DRC_Sitrep_FV_12.06.26/scripts/08_cloud_sitrep_monitor.R"
NEW_FILE <- "D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep/scripts/08_cloud_sitrep_monitor.R"

file.copy(OLD_FILE, NEW_FILE, overwrite = TRUE)

setwd("D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep")
system("git status")
system("git add scripts/08_cloud_sitrep_monitor.R")
system('git commit -m "Update corrected SitRep monitor"')
system("git push")
```

## 10. Règle principale

Pour éviter les erreurs:

- travailler directement dans `D:/GitHub_PREIS/PREIS_Ebola_DRC_Sitrep`;
- tester localement avec `source()`;
- vérifier avec `git status`;
- faire `git add`, `git commit`, puis `git push`;
- relancer GitHub Action après le push.

