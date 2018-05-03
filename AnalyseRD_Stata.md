---
title: "Analyse RD (Regression avec Discontinuité)"
author: "Daniel Fuller (daniel.fuller@mun.ca) | Tarik Benmarhnia (tbenmarhnia@ucsd.edu) | Britt McKinnon (britt.mckinnon@sickkids.ca)"
output:
      html_document:
        keep_md: true
---

## Code Stata - Analyse RD (Regression avec Discontinuité)

```
****************************************
*title: "Analyse RD (Regression avec Discontinuité)"
*author: "Daniel Fuller (daniel.fuller@usask.ca) & Tarik Benmarhnia (tarikcontact@gmail.com)"
*date: "25 aout, 2016"
****************************************

*Importer les données*

	cd "/Users/dfuller/Dropbox/Books/Methodes_Evaluation/Analysis/"
	log using /dfuller/DogLeg/Dropbox/Books/Methodes_Evaluation/Analysis/RDStata.txt, replace
	insheet using RD.csv
	net install rdrobust, from(http://www-personal.umich.edu/~cattaneo/software/rdrobust/stata) replace

* Nombre de consomation par état en moyenne dans le temps 2010 et 2011 
	bysort dage: summarize propalcjour
	bysort dage: summarize fruit
	bysort dage: summarize cigarette

********************************
*Graphique*
********************************
	rdplot propalcjour age, c(21)

********************************
*Selection du kernel*
********************************
	rdbwselect propalcjour age, c(21) kernel(tri) all
	rdbwselect propalcjour age, c(21) kernel(epa) all
	rdbwselect propalcjour age, c(21) kernel(uni) all

********************************
*Régression*
********************************

	*Kernel Triangulaire
		rdrobust propalcjour age, c(21) kernel(tri) deriv(0) all
		rdrobust propalcjour age, c(21) kernel(tri) deriv(0) all bwselect(cerrd)

	* Kernel Epa
		rdrobust propalcjour age, c(21) kernel(epa) deriv(0) all
		rdrobust propalcjour age, c(21) kernel(epa) deriv(0) all bwselect(cerrd)

	* Kernel Uniforme
		rdrobust propalcjour age, c(21) kernel(uni) deriv(0) all
		rdrobust propalcjour age, c(21) kernel(uni) deriv(0) all bwselect(cerrd)

	* Fruit
		rdrobust fruit age, c(21) kernel(uni) deriv(0) all
		rdrobust fruit age, c(21) kernel(uni) deriv(0) all bwselect(cerrd)
```

