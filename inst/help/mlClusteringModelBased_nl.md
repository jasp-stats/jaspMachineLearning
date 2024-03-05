Modelgebaseerde Clustering
===

Modelgebaseerd clusteren is gebaseerd op geparametriseerde Gaussische mengmodellen. De modellen worden geschat door een EM-algoritme dat wordt geïnitialiseerd door hiërarchische modelgebaseerde agglomeratieve clustering.

### Assumpties
- De data bestaat uit continue variabelen.
- (Normaal verdeelde data helpt het clusteringsproces).

### Invoer 
-------
#### Invoerveld 
- Variabelen: In dit veld vult u de variabelen in die meegenomen dienen te worden in het clusteringsalgoritme. 

#### Tabellen  
- Clusterinformatie: Toont de grootte van elke cluster. Dit is de standaardoptie. 
- Binnen kwadratensom: Toont de binnen kwadratensom van elk cluster. Dit is de standaardoptie.
- Silhouette score: Toont de silhouettescore van elke cluster.
- Tussen kwadratensom: Geeft de tussen kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Totale kwadratensom: Geeft de totale kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Parameterschattingen: Toont tabellen met de modelparameters voor elk cluster en elke kenmerkvariabele.

#### Grafieken
- Elleboogmethode: Genereert een grafiek met de totale binnen kwadratensom op de y-as en het aantal clusters op de x-as. De grafiek kan gebruikt worden om het optimale aantal clusters te bepalen. De grafiek laat 3 lijnen zien door AIC, BIC, en 'elleboogmethode' te optimaliseren.
- t-SNE clustergrafiek: Genereert een t-SNE grafiek van de clustering output. t-SNE grafieken worden gebruikt voor het visualiseren van hoog-dimensionale data in een laag-dimensionale ruimte van twee dimensies om de relatieve afstand tussen observaties te laten zien. De t-SNE twee-dimensionale ruimte maakt de assen oninterpreteerbaar. Een t-SNE grafiek geeft een indruk van de relatieve afstanden tussen observaties en clusters. Om dezelfde t-SNE grafiek nog een keer te maken voor meerdere clusteringanalyses, kunt u een toevalsgenerator beginwaarde gebruiken, aangezien het t-SNE algoritme willekeurige startpunten gebruikt.
- Legenda: Geeft een legenda met het clusternummer voor elke observatie. Dit is de standaardoptie.
- Labels: Geeft de clusteringlabels van de verschillende observaties.

#### Train parameters
#### Algoritme Instellingen
- Model: Kies het modeltype dat moet worden toegepast in de EM-stap van de clustering. De optie 'Auto' bepaalt automatisch het best passende modeltype. De volgende modellen zijn inbegrepen:
  - EII: Bolvormig, gelijk volume
  - VII: Bolvormig, ongelijk volume
  - EEI: Diagonaal, gelijk volume en vorm
  - VEI: Diagonaal, wisselend volume, gelijke vorm
  - EVI: Diagonaal, gelijk volume, variërende vorm
  - VII: Diagonaal, variërend volume en vorm
  - EEE: Ellipsoïdaal, gelijk volume, vorm en oriëntatie
  - VEE: Ellipsoïdaal, gelijke vorm en oriëntatie
  - EVE: Ellipsoïdaal, gelijk volume en oriëntatie
  - VVE: Ellipsoïdaal, gelijke oriëntatie
  - EEV: Ellipsoïdaal, gelijk volume en gelijke vorm
  - VEV: Ellipsoïdaal, gelijke vorm
  - EVV: Ellipsoïdaal, gelijk volume
  - VVV: Ellipsoïdaal, variërend volume, vorm en oriëntatie
- Max. iteraties: Bepaal het maximale aantal iteraties. Het maximale aantal iteraties geeft het aantal mogelijke keren dat het algoritme iterateert om de optimale clusteroplossing te vinden. De standaardoptie is 25.
- Schaal kenmerken: Standaardiseert de continue kenmerken in de dataset. Standaardisatie zorgt ervoor dat waarden van kenmerken van verschillende schalen in een specifieke vergelijkbare schaal vallen. Standaardisatie zorgt daardoor voor numerieke stabiliteit. JASP gebruikt de Z-score standaardisatie van een gemiddelde van 0 en een standaardafwijking van 1. Deze optie is standaard geselecteerd.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Bepalings parameters 
- Vast: Stelt u in staat om een eigen gespecificeerd aantal clusters te gebruiken. Hiermee kunt u handmatig optimaliseren.
- Optimalisatie: Stelt u in staat om een optimalisatiemethode kiezen. De opties zijn AIC, BIC, en silhouette. De AIC gebruikt de binnen kwadratensom (within-cluster variatie), het aantal gegenereerde clusters en het aantal dimensies voor het optimaliseren van de clustering output. De BIC gebruikt de binnen kwadratensom (within-cluster variatie), het aantal gegenereerde clusters, het aantal dimensies, en de steekproefgrootte voor het optimaliseren van de clustering output. De silhouette waarde gebruikt de gelijkheid van de observaties binnen een cluster en de ongelijkheid aan andere clusters voor het optimaliseren van de clustering output. BIC optimalisatie is de standaardoptie.
- Max. clusters: Bepaalt het maximum aantal mogelijke clusters om te genereren. De standaardoptie is 10.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Modelgebaseerde Clustering Model Tabel
- De eerste kolom geeft het aantal gegenereerde clusters.
- N: De steekproefgrootte.
- R<sup>2</sup>: Geeft de hoeveelheid verklaarde variantie door het model.
- AIC: De AIC waarde van het model. Lage waarden betekenen beter clusterende output.
- BIC: De BIC waarde van het model. Lage waarden betekenen beter clusterende output.
- Silhouette: De Silhouette waarde van het model. De Silhouette waarde spreiden van -1 tot 1, waar 1 een perfecte score is.

#### Modelgebaseerde Cluster Informatie
- Grootte: De grootte van elk cluster.
- Binnen kwadratensom: Toont de binnen kwadratensom van elk cluster. Dit is de standaardoptie.
- Silhouette score: Toont de silhouettescore van elke cluster.
- Centroïden: Toont de centroïde per variabele van elke cluster, wat staat voor het gemiddelde van een cluster.
- Tussen kwadratensom: Geeft de tussen kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Totale kwadratensom: Geeft de totale kwadratensom van het clustermodel onder de clusterinformatie tabel.

#### Evaluatiemetrieken Tabel
- Maximum diameter: De maximum cluster diameter in *euclidische* afstand.
- Minimum scheiding: De minimum cluster scheiding in *euclidische* afstand.
- Pearson's \u03B3: Correlatie tussen afstanden en een 0-1-vector waar 0 betekent dezelfde cluster, 1 betekent andere clusters. 
- Dunn index: Minimum scheiding / maximum diameter. 
- Entropie: Entropie van de distributie van clusterlidmaatschappen.
- Calinski-Harabasz index: De variantie ratio criterium van de clusterlidmaatschappen.

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Akaike, H. (1987). Factor analysis and AIC. Psychometrika, 52(3), 317–332.
- Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 63(2), 411–423.
- Matthiesen, R. (Ed.). (2010). Bioinformatics methods in clinical research. Humana Press.
- Schwarz, G., et al. (1978). Estimating the dimension of a model. The annals of statistics, 6(2), 461–464.
- Scrucca L., Fop M., Murphy T. B. and Raftery A. E. (2016) mclust 5: clustering, classification and density estimation using Gaussian finite mixture models, The R Journal, 8/1, pp. 289-317.
- Scrucca L., Fraley C., Murphy T. B. and Raftery A. E. (2023) Model-Based Clustering, Classification, and Density Estimation Using mclust in R. Chapman & Hall/CRC, ISBN: 978-1032234953, https://mclust-org.github.io/book/
- Fraley C. and Raftery A. E. (2002) Model-based clustering, discriminant analysis and density estimation, Journal of the American Statistical Association, 97/458, pp. 611-631.

### R-packages 
--- 
- cluster
- mclust
- Rtsne

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Iris Bloemen`.

