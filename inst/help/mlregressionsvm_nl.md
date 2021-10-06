Support Vector Machine Regressie
===

Support Vector Machines is een leeralgoritme onder toezicht dat trainingsvoorbeelden toewijst aan punten in de ruimte om de breedte van de kloof tussen de twee categorieën te maximaliseren. Nieuwe voorbeelden worden vervolgens in dezelfde ruimte in kaart gebracht en voorspeld dat ze tot een categorie behoren op basis van aan welke kant van de kloof ze vallen.

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
- Ondersteuningsvectoren: toont een tabel met de gegevens (punten) die door het algoritme als ondersteuningsvectoren zijn aangegeven.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Gemiddelde kwadraatsom fout: Plot het aantal naaste buren tegen de MSE van het model. Precisie is bepaald voor de trainingsset (en validatieset).
- Voorspellingsvermogen: Laat de observaties van de geselecteerde testset tegen de voorspelde waarden zien.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele die aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

### Trainingsparameters
#### Algoritmische instellingen
- Kernel: de kernel die wordt gebruikt bij training en voorspelling. Mogelijke kernels zijn 'lineair', 'radiaal', 'polynoom' en 'sigmoid'.
- Kosten van beperkingen overtreding: de 'C'-constante van de regularisatietermijn.
- Tolerantie van informatiecriterium: De tolerantie van beëindigingscriterium.
- Epsilon: de epsilon-parameter in de ongevoelig-verliesfunctie.
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waarden van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitgang
-------

#### Support Vector Machine Regressie Model Tabel
- De eerste kolom toont het aantal ondersteuningsvectoren.
- n(Train): Het aantal observaties in de trainingsset.
- n(Test): Het aantal observaties in de testset.
- Testset MSE: De MSE van de testset.

#### Evaluatiemetrieken
- MSE: De gemiddelde kwadraatsom fout van het model.
- RMSE: De wortel van de kwadratische gemiddelde fout van het model.
- MAE: De gemiddelde absolute fout van het model.
- MAPE: De gemiddelde absolute percentagefout van het model.
- R<sup>2</sup>: De proportie variabelen van een afhankelijke variabele die is uitgelegd door de onafhankelijke variabele(n).

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). Een inleiding tot statistisch leren. Springer New York.

### R-pakketten
---
- e1071