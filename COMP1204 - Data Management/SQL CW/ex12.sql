
INSERT INTO Date(dateRep, day, month, year)
SELECT DISTINCT dateRep, day, month, year
FROM dataset WHERE dateRep != 'dateRep';

INSERT INTO ReducedCountryInformation(countriesAndTerritories, geoId, countryterritoryCode, continentExp)
SELECT DISTINCT countriesAndTerritories, geoId, countryterritoryCode, continentExp
FROM dataset WHERE dateRep != 'dateRep';

INSERT INTO Population(geoId, popData2020)
SELECT DISTINCT geoId, popData2020
FROM dataset WHERE dateRep != 'dateRep';

INSERT INTO ReducedCovidData(dateRep, cases, deaths, countriesAndTerritories)
SELECT dateRep, cases, deaths, countriesAndTerritories
FROM dataset WHERE dateRep != 'dateRep';