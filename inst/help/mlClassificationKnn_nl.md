K-Naaste Buren Classificatie
===

K-naaste buren is een classificatiemethode die kijkt naar het aantal, *k*, voorspelde observaties die het meest gelijk zijn aan nieuwe observaties om een voorspelling te maken over de klasse. Het aantal naaste buren is intrinsiek verbonden aan de complexiteit van het model, lage getallen verhogen de flexibiliteit van het model.

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

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Gewichtsfunctie: laat zien hoe de gewichten worden toegekend als functie van de afstand.
- Classificatie Precisie: Plot het aantal naaste buren tegen de classificatie Precisie van het model. Precisie is verkregen met de trainingsset (en validatieset).
- ROC kromme: Toont een ROC kromme voor elke klasse voorspelt tegenover alle andere klassen.
- Andrews kromme: Een manier om de structuur in hoger dimensionele data te visualiseren. Lijnen die clusteren, zijn observaties die meer overeenkomen. 
- Beslissingsseparatrix: Maakt een *n* x *n* grafiek die visualiseert hoe elke observatie zou zijn geclassificeerd zoals het huidige model voorspelt. Grenzen tussen klassen zijn gevisualiseerd. Kan enkel gemaakt worden voor numerieke voorspellers.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

#### Trainings- and Validatiedata
- Steekproef *x*% van validatiedata: Selecteer aselect een percentage als steekproef van de overgebleven data (na het selecteren van de testset).
- K-fold met *k* folds: Deel de overgebleven data op in *k* delen.
- Laat-één-weg: Deel de overgebleven data op in *n* delen.

### Parameters Trainen
#### Algoritme Instellingen
- Gewichten: Zet het gewichtsschema voor de naaste buren. De standaardoptie, rectangular, geeft de standaard knn, terwijl de andere opties het algorithme uitbreiden door de naaste buren af te wegen. Zie ook de kknn package.
- Afstand: De afstandsmetriek om de gelijkheid tussen naaste buren te bepalen. Kan met de Euclidean- of Manhattan afstand.
- Schaal kenmerken: Standaardiseert de continue kenmerken in de dataset. Standaardisatie zorgt ervoor dat waarden van kenmerken van verschillende schalen in een specifieke vergelijkbare schaal vallen. Standaardisatie zorgt daardoor voor numerieke stabiliteit. JASP gebruikt de Z-score standaardisatie van een gemiddelde van 0 en een standaardafwijking van 1. Deze optie is standaard geselecteerd.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Aantal Naaste Buren
- Fixed: Stelt u in staat om een eigen gespecificeerd aantal naaste buren te gebruiken. 
- Optimalisatie: Stelt u in staat om de voorspellingsfout te optimaliseren van een validatie dataset met betrekking tot het aantal naaste buren. 
- Max. aantal naaste buren: Het maximum aantal mogelijke naaste buren. Als standaardoptie staat dit op 10.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### K-Naaste Buren Classificatie Model Tabel
- De eerste kolom laat het aantal naaste buren zien.
- Weights: Het gewichtsschema.
- Astand: De gebruikte afstand.
- n(Train): Het aantal observaties in de trainingsset.
- n(Validatie): Het aantal observaties in de validatieset (zichtbaar wanneer model geoptimaliseerd is).
- n(Test): Het aantal observaties in de testset.
- Validatie Precisie: De classificatie Precisie voor de validatieset (zichtbaar wanneer model geoptimaliseerd is).
- Testset Precisie: De classificatie Precisie voor de testset.

#### Evaluatiemetrieken
- Precision: Ratio van correcte positieve voorspellingen en het totaal aantal positieve voorspellingen.
- Recall: Ratio van correcte positieve voorspellingen en het totale aantal positieve observaties.
- F1 Score: Het harmonische gemiddelde van de precision- en recallscores.
- Matthews Correlatiecoëfficiënt: Een alternatief voor F1 of nauwkeurigheid dat betrouwbaarder is voor onevenwichtige datasets.
	zie https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-6413-7
- Support: Het aantal observaties van een klasse in de testset.
- AUC: Gebied onder de ROC kromme. Elke klasse is voorspeld tegen alle andere klassen. Zie ook ROC kromme.

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- kknn
- ROCR

### Voorbeeld 
---
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Telco's Overstappende Klanten`.
