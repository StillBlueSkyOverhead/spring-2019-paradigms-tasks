-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name
    FROM Country
    JOIN Capital ON Code = Capital.CountryCode
    JOIN City ON CityId = Id
    WHERE Country.Name = "Malaysia";
