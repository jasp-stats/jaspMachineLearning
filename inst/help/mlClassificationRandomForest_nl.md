Random Forest Classificatie
===

Random Forest is een classificatiemethode die beslisbomen maakt die bestaan uit een groot aantal individuele bomen welke als ensemble opereren. Elke individuele boom in het random forest geeft een klasse voorspelling en de klasse met de meeste stemmen wordt de uiteindelijke voorspelling van het model.


### Assumpties
- De target variabele is een nominale of ordinale variabele.
- De voorspellende variabelen bestaan uit continue, nominale, of ordinale variabelen.

### Invoer 
-------
#### Invoerveld 
- Target: In dit veld vult u de variabele in die voorspeld wordt. 
- Voorspellers: In dit veld vult u de variabelen in die informatie geven over de target. 

#### Tabellen  
- Confusion matrix: Toont een tabel die de geobserveerde klassen uitgezet tegen de voorspelde klassen laat zien. Wordt gebruikt om de Precisie van het model te bepalen.
- Klasse proporties: Toont een tabel die de proporties van elke klasse laat zien in de data-, trainings- (en validatie-), en testset.
- Evaluatiemetrieken: Toont regelmatig gebruikte classificatie evaluatiemetrieken zoals precision, recall, de F1-score, support en AUC (gebied onder de ROC kromme).
- Variabele van belang: Toont de gemiddelde daling van de Precisie en de totale stijging van de node purity voor alle voorspellervariabelen. Dit zijn indicatoren voor het belang van de voorspellers.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Out-of-bag verbetering: Plot het aantal beslisbomen tegen de out-of-bag classificatie Precisieverbetering van het model. Precisie is bepaald voor de trainingsset.
- ROC kromme: Toont een ROC kromme voor elke klasse voorspelt tegenover alle andere klassen.
- Andrews kromme: Een manier om de structuur in hoger dimensionele data te visualiseren. Lijnen die clusteren, zijn observaties die meer overeenkomen. 
- Gemiddelde daling van de Precisie: Toont de variabele gemiddelde daling van de Precisie van het model.
- Totale toename in knotzuiverheid: Toont de variabele totale toename van knotzuiverheid van het model.
- Beslissingsseparatrix: Maakt een *n* x *n* grafiek die visualiseert hoe elke observatie zou zijn geclassificeerd zoals het huidige model voorspelt. Grenzen tussen klassen zijn gevisualiseerd. Kan enkel gemaakt worden voor numerieke voorspellers.


### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

#### Trainings- en Validatiedata
- Steekproef *x*% van validatiedata: Selecteer aselect een percentage als steekproef van de overgebleven data (na het selecteren van de testset).

### Parameters Trainen
#### Algoritme instellingen
- Schattingsmethode: Zet de estimator voor het algoritme. Is één van de moments voor standaard estimators van het gemiddelde en de variantie: "mle", "mve", of "t" voor robuuste schattingen gebaseerd op een t-distributie. Zie ook de MASS package.
- Voorspellers per splitsing: Geef het aantal voorspellervariabelen die gebruikt worden binnen elke splitsing in de beslisboom. 
- Schaal kenmerken: Standaardiseert de continue kenmerken in de dataset. Standaardisatie zorgt ervoor dat waarden van kenmerken van verschillende schalen in een specifieke vergelijkbare schaal vallen. Standaardisatie zorgt daardoor voor numerieke stabiliteit. JASP gebruikt de Z-score standaardisatie van een gemiddelde van 0 en een standaardafwijking van 1. Deze optie is standaard geselecteerd.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.


##### Aantal Beslisbomen
- Fixed: Stelt u in staat om een eigen gespecificeerd aantal beslisbomen te gebruiken. 
- Optimalisatie: Stelt u in staat om de voorspellingsfout te optimaliseren van een validatie dataset met betrekking tot het aantal beslisbomen. 
- Max. aantal beslisbomen: Het maximum aantal mogelijke beslisbomen. Als standaardoptie staat dit op 100.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Random Forest Classification Model Tabel
- De eerste kolom laat het aantal beslisbomen zien.
- Voorspellers per splitsing: Geef het aantal voorspellervariabelen die gebruikt worden binnen elke splitsing in de beslisboom. 
- n(Train): Het aantal observaties in de trainingsset.
- n(Validatie): Het aantal observaties in de validatieset (zichtbaar wanneer model geoptimaliseerd is).
- n(Test): Het aantal observaties in de testset.
- Validatie Precisie: De classificatie Precisie voor de validatieset (zichtbaar wanneer model geoptimaliseerd is).
- Testset Precisie: De classificatie Precisie voor de testset.
- OOB Precisie: De out-of-bag classificatie Precisie voor de testset.

#### Evaluatiemetrieken
- Precision: Ratio van correcte positieve voorspellingen en het totaal aantal positieve voorspellingen.
- Recall: Ratio van correcte positieve voorspellingen en het totale aantal positieve observaties.
- F1 Score: Het harmonische gemiddelde van de precision- en recallscores.
- Matthews Correlatiecoëfficiënt: Een alternatief voor F1 of nauwkeurigheid dat betrouwbaarder is voor onevenwichtige datasets.
	zie https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-6413-7
- Support: Het aantal observaties van een klasse in de testset.
- AUC: Gebied onder de ROC kromme. Elke klasse is voorspeld tegen alle andere klassen. Zie ook ROC kromme.

### Referenties
-------- 
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Breiman, Leo. (2001). Random forests. Machine Learning. Springer, 45(1), 5-32

### R-packages 
--- 
- randomForest
- ROCR

### Voorbeeld 
---
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Telco's Overstappende Klanten`.
