
CREATE TABLE ReducedCovidData(
"dateRep" TEXT NOT NULL,
"cases" INTEGER,
"deaths" INTEGER,
"countriesAndTerritories" TEXT NOT NULL,

CONSTRAINT ReducedCovidData_pKey PRIMARY KEY (dateRep, countriesAndTerritories),
CONSTRAINT Date_fKey FOREIGN KEY (dateRep) REFERENCES Date(dateRep) ON UPDATE CASCADE ON DELETE RESTRICT,
CONSTRAINT ReducedCountryInformation_fKey FOREIGN KEY (countriesAndTerritories) REFERENCES ReducedCountryInformation(countriesAndTerritories) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE INDEX idx_ReducedCovidData_cases
ON ReducedCovidData(cases);

CREATE INDEX idx_ReducedCovidData_deaths
ON ReducedCovidData(deaths);
        
CREATE INDEX idx_ReducedCovidData_dateRep_cases
ON ReducedCovidData(dateRep, cases);

CREATE INDEX idx_ReducedCovidData_dateRep_deaths
ON ReducedCovidData(dateRep, deaths);

CREATE INDEX idx_ReducedCovidData_countriesAndTerritories_cases
ON ReducedCovidData(countriesAndTerritories, cases);

CREATE INDEX idx_ReducedCovidData_countriesAndTerritories_deaths
ON ReducedCovidData(countriesAndTerritories, deaths);

CREATE TABLE Date(
"dateRep" TEXT NOT NULL,
"day" INTEGER NOT NULL,
"month" INTEGER NOT NULL,
"year" INTEGER NOT NULL,

CONSTRAINT Date_pKey PRIMARY KEY (dateRep)
);

CREATE TABLE ReducedCountryInformation(
"countriesAndTerritories" TEXT NOT NULL,
"geoId" TEXT NOT NULL,
"countryterritoryCode" TEXT,
"continentExp" TEXT NOT NULL,

CONSTRAINT ReducedCountryInformation_fKey PRIMARY KEY (countriesAndTerritories),
CONSTRAINT Population_fKey FOREIGN KEY (geoId) REFERENCES Population(geoId) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE Population(
"geoId" PRIMARY KEY NOT NULL,
"popData2020" INTEGER
);