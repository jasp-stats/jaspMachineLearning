Beslisboom regressie
===

Decision Trees is een leeralgoritme onder toezicht dat een beslissingsboom gebruikt als een voorspellend model om van observaties over een item (weergegeven in de wortels van de boom) naar conclusies over de doelwaarde van het item (weergegeven in de eindpunten van de boom) te gaan.

### Assumpties
- De target variabele is een nominale of ordinale variabele.
- De voorspellende variabelen bestaan uit continue, nominale, of ordinale variabelen.

### Invoer 
-------
#### Invoerveld 
- Target: In dit veld vult u de variabele in die voorspeld wordt. 
- Voorspellers: In dit veld vult u de variabelen in die informatie geven over de target. 

#### Tabellen  
- Evaluatiemetrieken: Toon regelmatig gebruikte classificatie evaluatiemetrieken zoals gemiddelde kwadraatsom fout (MSE), wortel gemiddelde kwadraatsom fout (RMSE) en R<sup>2</sup>.
- Splitsing in boomstructuur: toont de splitsvariabelen, hun splitspunt en het aantal waarnemingen (die niet ontbreken en een positief gewicht hebben) die door de splitsing naar links of rechts zijn verzonden. Het toont ook de verbetering in deviantie die door deze splitsing wordt gegeven.
- Variabele belang: toont het relatieve belang van de voorspellers.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Gemiddelde kwadraatsom fout: Plot het aantal naaste buren tegen de MSE van het model. Precisie is bepaald voor de trainingsset (en validatieset).
- Beslisboom: Creëert een plot die de beslisboom en zijn bladeren visualiseert.
- Voorspellingsvermogen: Laat de observaties van de geselecteerde testset tegen de voorspelde waarden zien.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele die aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

### Trainingsparameters
#### Algoritmische instellingen
- Min. waarnemingen per splitsing: het minimum aantal waarnemingen dat in een knooppunt moet bestaan ​​om een ​​splitsing mogelijk te maken.
- Min. observaties in terminal: het minimum aantal observaties in een terminalknooppunt.
- Maximaal. interactiediepte: Stel de maximale diepte in van elk knooppunt van de laatste boom.
- Complexiteitsparameter: Elke splitsing die het algehele gebrek aan fit niet met een factor van deze parameter vermindert, wordt niet geprobeerd.
- Schaal kenmerken: Standaardiseert de continue kenmerken in de dataset. Standaardisatie zorgt ervoor dat waarden van kenmerken van verschillende schalen in een specifieke vergelijkbare schaal vallen. Standaardisatie zorgt daardoor voor numerieke stabiliteit. JASP gebruikt de Z-score standaardisatie van een gemiddelde van 0 en een standaardafwijking van 1. Deze optie is standaard geselecteerd.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitgang
-------

#### Beslisboom Regressie Model Tabel
- De eerste kolom toont het aantal splits in de beslisboom.
- n(Train): Het aantal observaties in de trainingsset.
- n(Test): Het aantal observaties in de testset.
- Testset MSE: De MSE van de testset.

#### Evaluatiemetrieken
- MSE: De gemiddelde kwadraatsom fout van het model.
- RMSE: De wortel van de kwadratische gemiddelde fout van het model.
- MAE / MAD: De gemiddelde absolute fout van het model.
- MAPE: De gemiddelde absolute percentagefout van het model.
- R<sup>2</sup>: De proportie variabelen van een afhankelijke variabele die is uitgelegd door de onafhankelijke variabele(n).

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). Een inleiding tot statistisch leren. Springer New York.

### R-pakketten
---
- rpart