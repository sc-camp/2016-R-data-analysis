library(tidyr)
library(dplyr)


WEATHER <- read.csv("data/UKWeatherData.csv")
# str(WEATHER)

## Exercise 1
WEATHER_T = WEATHER %>% select(Location, Year, Month, TempMax) %>% arrange(desc(Year), desc(TempMax)) %>% filter(Year <= 2014)
WEATHER_T_MAX = WEATHER_T %>% distinct(Year)

# - Find the location with the lowest minimum monthly temperature for each month of 1975. o Think about which variables to select and how you should filter the data, as well as
# how to arrange the rows before selecting distinct ones.
WEATHER %>% filter(Year == 1975) %>% select(Location, Month, TempMin) %>% arrange(Month, TempMin) %>% distinct(Month)

## Exercise 2
# Find the top 5 locations with the highest total rainfall from 2000-2014
WEATHER %>% select(Location, Year, Month, Rainfall) %>% filter(Year >= 2000 & Year <= 2014) %>% group_by(Location) %>% 
  summarise(sum_rain = sum(Rainfall, na.rm=TRUE)) %>% arrange(desc(sum_rain)) %>% head(n=5)


## Exercise 3: Which locations have 2014’s biggest monthly temperature range?
WEATHER %>% select(Location, Year, Month, TempMax, TempMin) %>% filter(Year == 2014) %>% mutate(TempRange = TempMax-TempMin) %>% 
  arrange(desc(TempRange)) %>% distinct(Month)

## Exercise 4. For the whole of the 20th century, which location has
## the highest ratio of total hours of sun to total mm of rainfall?
as.character((WEATHER %>% select(Location, Year, Month, Sun, Rainfall) %>% filter(Year>=1900 & Year<=1999) %>% group_by(Location) %>% 
  summarise(sum_rain = sum(Rainfall, na.rm=TRUE), sum_sun = sum(Sun, na.rm=TRUE)) %>% 
  mutate(sun_rain_ratio = sum_sun/sum_rain) %>% select(-c(sum_rain, sum_sun)) %>% arrange(desc(sun_rain_ratio)) %>% head(n=1))$Location)
## Bonus:
WEATHER %>% select(Location, Year, Month, Sun, Rainfall) %>% filter(Year>=1900 & Year<=1999) %>% group_by(Location) %>% 
                summarise(sum_rain = sum(Rainfall, na.rm=TRUE), sum_sun = sum(Sun, na.rm=TRUE)) %>% 
                mutate(sun_rain_ratio = sum_sun/sum_rain) %>% select(-c(sum_rain, sum_sun)) %>% 
                mutate(isnice = ifelse(sun_rain_ratio<2, 0, 1))


## Exercise 5. Is there a relationship between location ‘northernliness’ and temperature?
STATIONS=read.csv("data/WeatherStations.csv")
WEATHERLOC <- left_join(WEATHER, STATIONS, by = "Location")
WEATHERLOC %>% filter(Year == 2014 & Month == 6) %>% select(DegN, TempMax, TempMin) %>% mutate(TempRange = TempMax-TempMin)
