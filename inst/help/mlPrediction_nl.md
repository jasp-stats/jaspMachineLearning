Voorspellen
===

### Invoer

#### Getraind model
Dit veld kan een getraind machine learning model laden. Merk op dat het model in JASP moet zijn gemaakt en moet worden opgeslagen met de extensie *.jaspML*.

#### Opdrachtbox
- Voorspellers: In dit vak moeten de voorspellers worden ingevoerd die in aanmerking moeten worden genomen voor de voorspellingen.

#### Algoritmische instelling
- Schaal predictoren: Schaalt de continue predictoren. Standaardiseren zorgt dat waarden van predictoren met verschillende schalen, worden geschaald in een specifieke gelijke schaal. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.

#### Tabellen:
- Voorspellingen voor nieuwe data: geeft de voorspelde resultaten voor de data weer.
  - Predictoren toevoegen: Voegt de waarden van de predictoren toe aan de tabel.

### Resultaten exporteren
- Voorspelde resultaten aan data toevoegen: maakt een nieuwe variabele in de data set die de voorspelde waarden voor de data bevat.

### Uitvoer

#### Samenvattingstabel
- Methode: De methode die in het model wordt gebruikt.
- Specifieke informatie per model.
- n(Trein): Aantal waarnemingen in trainings data.
- n(Nieuw): Aantal waarnemingen in nieuwe data.

#### Voorspellingen voor nieuwe data
- Rij: Het rij nummer.
- Voorspeld: de voorspelde waarde / label.
