Geregulariseerde Lineaire Regressie
===

Geregulariseerde lineaire regressie is een aanpassing van de lineaire regressie waarin coëfficiënten worden verkleind richting 0. Dit is gedaan door een straf te geven (e.g., ridge, lasso, of elastic net). De parameter λ controlleert de mate waarin parameters zijn verkleind.

### Assumpties
- De target variabele is een nominale of ordinale variabele.
- De voorspellende variabelen bestaan uit continue, nominale, of ordinale variabelen.

### Invoer 
-------
#### Invoerveld 
- Target: In dit veld vult u de variabele in die voorspeld wordt. 
- Voorspellers: In dit veld vult u de variabelen in die informatie geven over de target. 
- Gewichten: In dit veld vult u een optionele variabele in die gewichten voor casussen bevatten.

#### Tabellen  
- Evaluatiemetrieken: Toon regelmatig gebruikte classificatie evaluatiemetrieken zoals gemiddelde kwadraatsom fout (MSE), wortel gemiddelde kwadraatsom fout (RMSE) en R<sup>2</sup>.
- Regressiecoëfficiënten: Geeft de regressiecoëfficiënt voor elke voorspeller.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Voorspellingsvermogen: Laat de observaties van de geselecteerde testset tegen de voorspelde waarden zien.
- Variabele spoor: Toont de ontwikkeling van de coëfficiënten als lambda stijgt.
- \u03BB evaluatie: Toont de the kruis-gevalideerde MSE bij verschillende waarden van \u03BB, inclusief een indicatie van de optimale \u03BB waarden.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

#### Trainings- and Validatiedata
- Steekproef *x*% van validatiedata: Selecteer aselect een percentage als steekproef van de overgebleven data (na het selecteren van de testset).

#### Parameters Trainen
#### Algoritme Instellingen
- Straf: Specificeer welke straf wordt gebruikt om de regressiecoëfficiënten te verkleinen. De opties zijn ridge, lasso, en elastische net (see James, Witten, Hastie, & Tibshirani, 2013).
- Fit intercept: Specificeer of de regressiefunctie een intercept heeft.
- Lambda (\u03BB): Specificeer de shrinkage parameter. Het kan vastgezet worden op een specifieke waarde, geoptimaliseerd worden door het een waarde te geven met de laagste MSE in kruis-validatie, of het wordt de hoogste waarde die binnen 1 standaardfout (Std. Fout) van de minimum MSE in kruis-validatie is.
- Schaal kenmerken: Standaardiseert de continue kenmerken in de dataset. Standaardisatie zorgt ervoor dat waarden van kenmerken van verschillende schalen in een specifieke vergelijkbare schaal vallen. Standaardisatie zorgt daardoor voor numerieke stabiliteit. JASP gebruikt de Z-score standaardisatie van een gemiddelde van 0 en een standaardafwijking van 1. Deze optie is standaard geselecteerd.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Geregulariseerde Lineaire Regressie Tabel
- Straf: Toont de straf die van toepassing is op het model.
- \u03BB: Geeft de gekozen waarde van de shrinkage parameter \u03BB.
- n(Train): Het aantal observaties in de trainingsset.
- n(Validatie): Het aantal observaties in de validatieset (mogelijk wanneer model is geoptimaliseerd).
- n(Test): Het aantal observaties in de testset.
- Validation MSE: De MSE van de validatieset (mogelijk wanneer model is geoptimaliseerd).
- Testset MSE: De MSE van de testset.

#### Evaluatiemetrieken
- MSE: De gemiddelde kwadraatsom fout van het model.
- RMSE: De wortel van de kwadratische gemiddelde fout van het model.
- MAE / MAD: De gemiddelde absolute fout van het model.
- MAPE: De gemiddelde absolute percentagefout van het model.
- R<sup>2</sup>: De proportie variabelen van een afhankelijke variabele die is uitgelegd door de onafhankelijke variabele(n).

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- glmnet

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Studentcijfers`.  
