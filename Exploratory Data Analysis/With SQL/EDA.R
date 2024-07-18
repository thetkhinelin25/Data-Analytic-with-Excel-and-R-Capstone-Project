
# Load RSQLite and DBI package
library("RSQLite")
library("DBI")
library("readr")

# Create Database connection
con <- dbConnect(RSQLite::SQLite(),"Capstone_Project.sqlite")
con

#------------------------------------------------------------------------------------------------

# Loading Data

# Load csv files
SEOUL_BIKE_SHARING_df <- read_csv("seoul_bike_sharing.csv")
CITIES_WEATHER_FORECAST_df <- read_csv("cities_weather_forecast.csv")
BIKE_SHARING_SYSTEMS_df <- read_csv("bike_sharing_systems.csv")
WORLD_CITIES_df <- read_csv("world_cities.csv")

# Tables creation by loading csv files
dbWriteTable(con, "SEOUL_BIKE_SHARING", SEOUL_BIKE_SHARING_df, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "CITIES_WEATHER_FORECAST", CITIES_WEATHER_FORECAST_df, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "BIKE_SHARING_SYSTEMS", BIKE_SHARING_SYSTEMS_df, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "WORLD_CITIES", WORLD_CITIES_df, overwrite = TRUE, row.names = FALSE)

dbListTables(con)

#------------------------------------------------------------------------------------------------

# Task 1 - Record Count
# Determine how many records are in the seoul_bike_sharing dataset.
dbGetQuery(con, "SELECT COUNT(*) AS Nums_records_seoul_bike_sharing FROM SEOUL_BIKE_SHARING")

# Task 2 - Operational Hours
# Determine how many hours had non-zero rented bike count.
dbGetQuery(con, 
           "SELECT SUM(HOUR) AS Total_hours_without_zero_rented_bike_count FROM SEOUL_BIKE_SHARING
           WHERE RENTED_BIKE_COUNT != 0")

# Task 3 - Weather Outlook
# Query the the weather forecast for Seoul over the next 3 hours.
dbGetQuery(con,
           "SELECT CITY, WEATHER FROM CITIES_WEATHER_FORECAST
           LIMIT(1)")

# Task 4 - Seasons
# Find which seasons are included in the seoul bike sharing dataset.
dbGetQuery(con,
           "SELECT DISTINCT(SEASONS) FROM SEOUL_BIKE_SHARING")

# Task 5 - Date Range
# Find the first and last dates in the Seoul Bike Sharing dataset.
dbGetQuery(con,
           "SELECT MIN(DATE) AS First_date, MAX(DATE) AS Last_date 
           FROM SEOUL_BIKE_SHARING")

# Task 6 - Subquery - 'all-time high'
# determine which date and hour had the most bike rentals.
dbGetQuery(con,
           "SELECT DATE, HOUR, RENTED_BIKE_COUNT 
           FROM SEOUL_BIKE_SHARING
           WHERE RENTED_BIKE_COUNT = (SELECT MAX(RENTED_BIKE_COUNT) FROM SEOUL_BIKE_SHARING)")

# Task 7 - Hourly popularity and temperature by season
# Determine the average hourly temperature and the average number of bike rentals per hour 
# over each season. List the top ten results by average bike count.
dbGetQuery(con,
           "SELECT SEASONS, HOUR, AVG(TEMPERATURE) AS AVG_TEMP, AVG(RENTED_BIKE_COUNT) AS AVG_BIKE_COUNT
           FROM SEOUL_BIKE_SHARING
           GROUP BY SEASONS, HOUR
           ORDER BY AVG_BIKE_COUNT
           LIMIT(10)")

# Task 8 - Rental Seasonality
# Find the average hourly bike count during each season.
# Also include the minimum, maximum, and standard deviation of the hourly bike count for each season.
dbGetQuery(con,
           "SELECT SEASONS, HOUR, MIN(RENTED_BIKE_COUNT) AS MIN_BIKE_RENT, 
           MAX(RENTED_BIKE_COUNT) AS MAX_BIKE_RENT, AVG(RENTED_BIKE_COUNT) AS AVG_BIKE_COUNT,
           SQRT(AVG(RENTED_BIKE_COUNT*RENTED_BIKE_COUNT) - AVG(RENTED_BIKE_COUNT)*AVG(RENTED_BIKE_COUNT) ) AS STDDEV_BIKE_COUNT
           FROM SEOUL_BIKE_SHARING
           GROUP BY SEASONS, HOUR
           ORDER BY AVG_BIKE_COUNT
           LIMIT(10)") 

# Task 9 - Weather Seasonality
# Consider the weather over each season. On average, what were the TEMPERATURE, HUMIDITY, WIND_SPEED, 
# VISIBILITY, DEW_POINT_TEMPERATURE, SOLAR_RADIATION, RAINFALL, and SNOWFALL per season?
dbGetQuery(con,
           "SELECT SEASONS, AVG(TEMPERATURE), AVG(HUMIDITY), AVG(WIND_SPEED),
           AVG(VISIBILITY), AVG(DEW_POINT_TEMPERATURE), AVG(SOLAR_RADIATION),
           AVG(RAINFALL), AVG(SNOWFALL)
           FROM SEOUL_BIKE_SHARING
           GROUP BY SEASONS")

# Task 10 - Total Bike Count and City Info for Seoul
# Use an implicit join across the WORLD_CITIES and the BIKE_SHARING_SYSTEMS tables to 
# determine the total number of bikes avaialble in Seoul, plus the following city information about Seoul: 
# CITY, COUNTRY, LAT, LON, POPULATION, in a single view.
dbGetQuery(con,
           "SELECT SUM(BSS.BICYCLES) AS TOTAL_NUM_BICYCLES_SEOUL, WC.CITY, WC.COUNTRY, WC.LAT, WC.LNG, WC.POPULATION 
           FROM WORLD_CITIES WC, BIKE_SHARING_SYSTEMS BSS
           WHERE WC.CITY = BSS.CITY AND UPPER(BSS.CITY) = 'SEOUL'")

# Task 11 - Find all city names and coordinates with comparable bike scale to Seoul's bike sharing system
# Find all cities with total bike counts between 15000 and 20000. Return the city and country names, 
# plus the coordinates (LAT, LNG), population, and number of bicycles for each city.
dbGetQuery(con,
           "SELECT WC.CITY, WC.COUNTRY, WC.LAT, WC.LNG, WC.POPULATION, SUM(BSS.BICYCLES) AS TOTAL_BICYCLES
           FROM WORLD_CITIES WC, BIKE_SHARING_SYSTEMS BSS
           WHERE WC.CITY = BSS.CITY
           GROUP BY WC.CITY")

#------------------------------------------------------------------------------------------------

# Disconnect 
dbDisconnect(con)






