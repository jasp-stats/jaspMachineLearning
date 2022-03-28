Support Vector Machine Classificatie
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
- Confusion matrix: Toont een tabel die de geobserveerde klassen uitgezet tegen de voorspelde klassen laat zien. Wordt gebruikt om de Precisie van het model te bepalen.
- Klasse proporties: Toont een tabel die de proporties van elke klasse laat zien in de data-, trainings- (en validatie-), en testset.
- Evaluatiemetrieken: Toont regelmatig gebruikte classificatie evaluatiemetrieken zoals precision, recall, de F1-score, support en AUC (gebied onder de ROC kromme).
- Ondersteuningsvectoren: toont een tabel met de gegevens (punten) die door het algoritme als ondersteuningsvectoren zijn aangegeven.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Classificatie Precisie: Plot het aantal naaste buren tegen de classificatie Precisie van het model. Precisie is verkregen met de trainingsset (en validatieset).
- ROC kromme: Toont een ROC kromme voor elke klasse voorspelt tegenover alle andere klassen.
- Andrews kromme: Een manier om de structuur in hoger dimensionele data te visualiseren. Lijnen die clusteren, zijn observaties die meer overeenkomen.
- Beslissingsseparatrix: Maakt een *n* x *n* grafiek die visualiseert hoe elke observatie zou zijn geclassificeerd zoals het huidige model voorspelt. Grenzen tussen klassen zijn gevisualiseerd. Kan enkel gemaakt worden voor numerieke voorspellers.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

### Parameters Trainen
#### Algoritme Instellingen
- Kernel: de kernel die wordt gebruikt bij training en voorspelling. Mogelijke kernels zijn 'lineair', 'radiaal', 'polynoom' en 'sigmoid'.
- Kosten van beperkingen overtreding: de 'C'-constante van de regularisatietermijn.
- Tolerantie van informatiecriterium: De tolerantie van beëindigingscriterium.
- Epsilon: de epsilon-parameter in de ongevoelig-verliesfunctie.
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waarden van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### K-Naaste Buren Classificatie Model Tabel
- De eerste kolom laat het aantal ondersteuningsvectoren zien.
- Weights: Het gewichtsschema.
- Astand: De gebruikte afstand.
- n(Train): Het aantal observaties in de trainingsset.
- n(Test): Het aantal observaties in de testset.
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
- e1071
- ROCR
