SELECT ReducedCovidData.countriesAndTerritories AS 'Country', ROUND((SUM(deaths) * 100.0) / (SUM(cases) * 1.0), 3) AS '%_Deaths_of_Country_Cases'
FROM ReducedCovidData
GROUP BY countriesAndTerritories
ORDER BY 2 DESC
LIMIT 10;