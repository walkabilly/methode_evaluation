---
title: "Analyse DD (Différences dans les Différences) - Stata"
author: "Daniel Fuller (daniel.fuller@mun.ca) | Tarik Benmarhnia (tbenmarhnia@ucsd.edu) | Britt McKinnon (britt.mckinnon@sickkids.ca)"
output:
      html_document:
        keep_md: true
---

## Code Stata pour les analyses

### Importer les données

```
cd "/Users/dfuller/Dropbox/Books/Methodes_Evaluation/Analysis/"
	log using /Users/dfuller/Dropbox/Books/Methodes_Evaluation/Analysis/DiDStata.txt, replace
	insheet using DD.csv
```
#### Nombre de consomation par état en moyenne dans le temps 2010 et 2011 

```
bysort etatid effettemps: summarize consparsem
```

#### Moyenne du nombre de consomation par semaine 
```
bysort loi: summarize consparsem 
```

#### Moyenne du nombre de consomation par semaine par province
```
bysort effettemps region: summarize consparsem
```

### Regression

#### Modèle 1
```
nbreg consparsem i.loi
```

#### Modèle 2
```
nbreg consparsem i.loi i.effettemps
```
#### Modèle 3
```
nbreg consparsem i.loi i.effettemps i.etatid
```

#### Modèle 4
```
nbreg consparsem i.loi i.effettemps i.etatid connaisance age
```

