SELECT ReducedCovidData.dateRep AS 'Date', SUM(cases) OVER(ORDER BY year ASC, month ASC, day ASC) AS 'Cumulative_UK_cases', SUM(deaths) OVER(ORDER BY year ASC, month ASC, day ASC) AS 'Cumulative_UK_deaths'
FROM ReducedCovidData
	INNER JOIN Date ON ReducedCovidData.dateRep = Date.dateRep
WHERE countriesAndTerritories = 'United_Kingdom'
ORDER BY year ASC, month ASC, day ASC;