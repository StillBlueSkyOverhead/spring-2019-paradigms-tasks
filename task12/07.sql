-- Выведите названия стран в лексикографическом порядке, большинство населения
-- которых не проживает в городах (имеются в виду города, информация о которых
-- есть в базе данных), если в базе данных для страны нет ни одного города,
-- то ее городское население считается равным 0 (будьте внимательны, при этом
-- население страны тоже может быть равным 0, в этом случае выводить такую
-- страну не нужно). (0,5 баллов)
SELECT Country.Name
    FROM Country
    JOIN (
        SELECT CountryCode, SUM(Population) AS InCity
        FROM City
        GROUP BY CountryCode
    ) ON Code = CountryCode AND 2 * InCity < Country.Population
    ORDER BY Country.Name DESC;
