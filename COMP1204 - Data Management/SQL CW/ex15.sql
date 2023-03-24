SELECT ReducedCovidData.dateRep AS 'Date', cases AS 'Number_Of_Cases'
FROM ReducedCovidData INNER JOIN Date ON ReducedCovidData.dateRep = Date.dateRep
WHERE countriesAndTerritories = 'United_Kingdom'
ORDER BY year ASC, month ASC, day ASC;