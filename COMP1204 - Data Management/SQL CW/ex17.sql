SELECT ReducedCovidData.countriesAndTerritories AS 'Country', ROUND((SUM(cases) * 100.0) / (Population.popData2020), 3) AS '%_Cases_of_Population', ROUND((SUM(deaths) * 100.0) / (Population.popData2020), 3)AS '%_Deaths_of_Population'
FROM ReducedCovidData
	INNER JOIN ReducedCountryInformation ON ReducedCovidData.countriesAndTerritories = ReducedCountryInformation.countriesAndTerritories
	INNER JOIN Population ON ReducedCountryInformation.geoId = Population.geoId
GROUP BY ReducedCovidData.countriesAndTerritories;