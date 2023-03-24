SELECT ReducedCovidData.countriesAndTerritories AS 'Country', ReducedCovidData.dateRep AS 'Date', SUM(cases) AS 'Number_Of_Cases', SUM(deaths) AS 'Number_Of_Deaths'
FROM ReducedCovidData
	INNER JOIN Date ON ReducedCovidData.dateRep = Date.dateRep
	INNER JOIN ReducedCountryInformation ON ReducedCovidData.countriesAndTerritories = ReducedCountryInformation.countriesAndTerritories
GROUP BY ReducedCovidData.dateRep, ReducedCovidData.countriesAndTerritories
ORDER BY year ASC, month ASC, day ASC;