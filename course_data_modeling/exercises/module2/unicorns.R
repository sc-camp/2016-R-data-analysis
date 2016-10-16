library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)

load("data/Unicorns.RData")

p1 = ggplot(UNICORNS, aes(x = HornLength)) + geom_histogram(binwidth=2) + theme_bw()

p2 = ggplot(UNICORNS, aes(x = Height, y = HornLength)) + geom_point(alpha=1, aes(color=Magic)) + theme_bw()

p3 = ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + geom_boxplot() + theme_bw()

p4 = ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + geom_boxplot() + theme_bw() + stat_summary(fun.y="mean", geom = "point")
p4_2 = p4 + stat_summary(fun.y="min", geom = "point", shape=3, size=3) + stat_summary(fun.y="max", geom = "point", shape=3, size=3)

p5 = p2 + geom_smooth(method="lm")


p5_dens = p2 + stat_density2d(aes(fill=..level..), geom="polygon", n=100)
p5_hex = p2 + geom_hex()

p5_2 = p5 + facet_grid(. ~ Magic)
p5_3 = p5 + facet_grid(ManeStyle ~ Magic)

p6 = p5 + facet_wrap(~Magic, scales = "free")


WEATHER = read.csv("../module1/data/UKWeatherData.csv")
STATIONS = read.csv("../module1/data/WeatherStations.csv")

weather_stations = left_join(WEATHER, STATIONS, by="Location")
leaflet() %>%
  addTiles() %>% addCircles(lng = weather_stations$DegE, lat = weather_stations$DegN, radius = weather_stations$Rainfall*100, stroke = FALSE)


## Create a chain of commands that selects just location, year, month, and maximum temperature; filter for data from years 2000-2010. Instead of ending this here, make a scatterplot of maximum temperature against year. Try adding a smoother to this as well.

weather_stations %>% select(Location, Year, Month, TempMax) %>% filter(Year >= 2000 & Year <= 2010) %>% arrange(desc(TempMax)) %>% distinct(Year) %>% ggplot(aes(x=Year, y=TempMax)) + geom_point() + theme_bw() + geom_smooth(method = "loess") + scale_x_continuous(breaks = c(2000:2010), labels = c(2000:2010))
