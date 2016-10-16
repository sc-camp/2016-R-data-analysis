
##
#
# 'Advancing in R' course
#
# Manipulating data
#
##

# Install packages if necessary
# install.packages(c("tidyr", "dplyr"))

# Load tidyr and dplyr packages
library(tidyr)
library(dplyr)

##### Tidying data: Peppered moth counts ####

# import the data; note that read.csv works for comma delimited files
MOTHS <- read.csv("MothCount.csv")

# examine data
MOTHS

##
# First, sex is already in a column, but needs to be named
#  (it was left blank in the original spreadsheet).
# Second, when we rearrange our morph types into key/value pairs,
#  we don't want each value to be 'x.morph'; we want 'x'.
#
# Let's use dplyr's 'rename' function to specify new column names.
#
# We assign these changes to the MOTHS object, overwriting the 
#  original version.
##
MOTHS <- rename(MOTHS, 
               Sex = X,
               Dark = Dark.morph, 
               Light = Light.morph)

# Check these changes:
MOTHS

##
# Convert our data into key/value pairs using 'gather'
# - 1. data [MOTHS]
# - 2. key / value column names to create [Morph, Count]
# - 3. columns to gather [Dark:Light]
##

MOTHS_T <- gather(MOTHS, 
                 Morph, Count, 
                 Dark:Light)

MOTHS_T


## 'spread' is the complement of 'gather' (reverts back to 'wide' format)
spread(MOTHS_T, Morph, Count)

##
# Tidy format enables us to start exploring our data more easily...
##

MOTHS_T %>%
  select(Sex, Count) %>%	
  group_by(Sex) %>%
  summarise(SexCount = n()) %>%
  mutate(Proportion = SexCount/sum(SexCount))
         


##### Tidying data: Time series (city weather) ####

# import the data
CITYTEMPS <- read.csv("TOM_CITIES.csv")

# examine data
CITYTEMPS

# look at the initial conversion to 'long' format
gather(CITYTEMPS, 
       Day, Temperature, 
       Day.1:Day.10)


# Having 'Day.x' isn't actually all that helpful!
# Use 'separate' to extract the value required
gather(CITYTEMPS, 
       Day, Temperature, 
       Day.1:Day.10) %>% 
  separate(Day, into=c("tmp","Day")) 



##### Manipulating data: Exploring weather data ####

##
# Load in the weather data file
##

WEATHER <- read.csv("UKWeatherData.csv")

str(WEATHER)


########## Q1 List the locations with the highest temperature in each year #######

## 
# First, there are a lot of variables that we're not interested in,
#  so we can just select those that we want to keep...
##

## Select Location, Year, Month, and TempMax
WEATHER %>% 
  select(Location, Year, Month, TempMax) 

##
# Too many rows! Chain 'head' to the end of our command
#  to get the first few rows.
# Use the documentation to see how you can change the 
#  number that are returned (default is 6)
## 
WEATHER %>% 
  select(Location, Year, Month, TempMax) %>%
  head(n = 10)

## ... which is equivalent to using
WEATHER %>% 
  select(-c(TempMin, AF, Rainfall, Sun)) %>%
  head(n = 10)

WEATHER %>% 
  select(-TempMin, -AF, -Rainfall, -Sun) %>%
  head(n = 10)

## We could also take advantage of the order of the variables:
WEATHER %>% 
  select(Location:TempMax) %>%
  head(n = 10)

## Now arrange these in descending order of year & TempMax
WEATHER %>% 
  select(Location, Year, Month, TempMax) %>%
  arrange(desc(Year), desc(TempMax)) %>%
  head(n = 10)


## 
# You'll notice some of these temperatures seem quite low
#  - this is because we sorted first by Year, and the data set
#    only gives readings up until March 2015.
##


WEATHER %>% 
  select(Month, Year) %>% 
  distinct(Month, Year) %>% 
  arrange(desc(Year), desc(Month)) %>% 
  head(n = 20)

##
#  We can use a filter to only use values until the end of 2014.
##

WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  filter(Year <= 2014) %>%
  arrange(desc(Year), desc(TempMax)) %>%
  head(n = 10)

## 
# That seems more like it!
# Also note that we put the filter before the arrange command,
#  as this reduces the number of records that R has to sort.
##

WEATHER <- WEATHER %>% 
  filter(Year <= 2014)


##
# Now, let's find the location with the highest monthly max temp
#  in each year. using the 'distinct' function.
##
WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  arrange(desc(Year), desc(TempMax)) %>%
  distinct(Year) 


##
# Check the documentation to see more about 'distinct'
# - in particular, think about why this is placed AFTER the 
# 'arrange' command.
##

##
# How would you find the location with the minimum temperature for 
#  each month of 1967?
#
# HINT: Think about how you should filter the data, as well as how
#        to arrange the rows before selecting a 'distinct' one.
##

WEATHER %>% 
  select(Location, Year, Month, TempMin) %>% 
  filter(Year == 1975) %>%
  arrange(Month, TempMin) %>%
  distinct(Month) 

#
# How would you find the year with the highest monthly max temp
#  for each location?
#
# HINT: Think about how the rows are arranged, as well as the variable
#        you are selecting as 'distinct'.
##

WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  arrange(desc(TempMax)) %>%
  distinct(Location) 






########## Q2 Top 5 locations with highest total rainfall from 2000-2014 #######

##
# Use the 'split-apply-combine' technique:
##

WEATHER %>%
  filter(Year >= 2000) %>%
  group_by(Location) %>%
  summarise(totalrain = sum(Rainfall, na.rm = TRUE)) %>%
  arrange(desc(totalrain)) %>%
  head(n = 5)

##
# - split data by location
# - apply a summary function to each group
# - combine these summary statistics
##

##
# Bonus questions
##

## Find the average monthly rainfall for each location in each year, 2010-2014
##  HINT: You can group by more than one variable...

WEATHER %>%
  select(Location, Year, Month, Rainfall) %>%
  filter(Year >= 2010) %>%
  group_by(Location, Year) %>%
  summarise(mean_rain = mean(Rainfall, na.rm = TRUE)) %>%
  arrange(desc(Year)) 

## Add the standard deviation; reduce the number of decimal places
WEATHER %>%
  select(Location, Year, Month, Rainfall) %>%
  filter(Year >= 2010) %>%
  group_by(Location, Year) %>%
  summarise(mean_rain = round(mean(Rainfall, na.rm = TRUE), digits = 2),
            sd_rain = round(sd(Rainfall, na.rm = TRUE), digits = 2)) %>%
  arrange(desc(Year)) 


## How many hours of sun were recorded each month in 2014?
WEATHER %>%
  filter(Year == 2014) %>%
  group_by(Month) %>%
  summarise(totalsun = sum(Sun, na.rm = TRUE)) 

## For each year, how much rainfall has been recorded in total
##  (rainfall of all locations across all months)
WEATHER %>%
  group_by(Year) %>%
  summarise(totalrain = sum(Rainfall, na.rm = TRUE)) %>%
  arrange(desc(Year)) %>%
  head(n = 10)

## 
# For a given group of stations, what are the annual averages and
#  standard deviations of hours of sun in 2014?
##
WEATHER %>%
  select(Location, Year, Month, Sun) %>%
  filter(Year == 2014,
         Location %in% c("Eskdalemuir", "Leuchars",
                         "Paisley", "Stornoway")) %>% 
  group_by(Location, Year) %>%
  summarise(mean_sun = round(mean(Sun, na.rm = TRUE), digits = 2),
            sd_sun = round(sd(Sun, na.rm = TRUE), digits = 2)) %>%
  arrange(desc(Year)) 


######### Q3 Which locations have 2014's biggest monthly temperature range? ##########

WEATHER %>%
  select(Location, Year, Month, contains("Temp")) %>%
  filter(Year == 2014) %>%
  mutate(TempRange = TempMax - TempMin) %>%
  arrange(desc(TempRange)) %>%
  head(n = 10)

# Bonus: find the average annual temperature range across all locations
WEATHER %>%
  select(Location, Year, Month, contains("Temp")) %>%
  filter(Year >= 2010) %>%
  mutate(TempRange = TempMax - TempMin) %>% 
  group_by(Year) %>% 
  summarise(mean_range = mean(TempRange)) 


######### Q4 Which location has the highest total sun: total rainfall ratio for the 20th century? ##########


WEATHER %>%
  filter(Year >= 1900, Year <= 1999) %>%
  group_by(Location) %>%
  summarise(totalrain = sum(Rainfall, na.rm = TRUE),
            totalsun = sum(Sun, na.rm = TRUE)) %>%
  mutate(Sun2Rain = totalsun/totalrain) %>%
  select(-starts_with("total")) %>% 
  arrange(desc(Sun2Rain)) 

## Use mutate to add a categorical variable
WEATHER %>%
  filter(Year >= 1900, Year <= 1999) %>%
  group_by(Location) %>%
  summarise(totalrain = sum(Rainfall, na.rm = TRUE),
            totalsun = sum(Sun, na.rm = TRUE)) %>%
  mutate(Sun2Rain = totalsun/totalrain) %>%
  select(-starts_with("total")) %>% 
  arrange(desc(Sun2Rain)) %>% 
  mutate(SunBin = ifelse(Sun2Rain >= 2, 1, 0))


######### Q5 Merge 

STATIONS <- read.csv("WeatherStations.csv")

str(STATIONS)
str(WEATHER)

left_join(WEATHER, STATIONS,
           by = "Location") %>% 
  head()

WEATHERLOC <- inner_join(WEATHER, STATIONS,
                         by = "Location")

WEATHERLOC %>%
  filter(Year == 2014 & Month == 6) %>%
  select(Location, Month, DegN, TempMax) %>%
  arrange(desc(DegN))
  

##
# One facet of grouping and summarising variables is that it ditches
#  additional information that you may want to keep... for example, if
#  we want to keep 'DegN' info then we have to group by this as well as
#  by location. 
# But, this means that 'arrange' will not work with 'DegN' afterwards as
#  R considers each to be a separate grouping. We can add the 'ungroup'
#  command to the chain to remove these groupings, allowing us to act on
#  the data frame as normal
##
WEATHERLOC %>%
  filter(Year == 2014, !is.na(Sun)) %>%
  select(Location, Month, DegN, Sun) %>%
  group_by(Location, DegN) %>% 
  summarise(total_sun = sum(Sun, na.rm = TRUE)) %>% 
  #ungroup() %>% 
  arrange(desc(DegN)) %>% 
  print.data.frame()


###### Supplementary exercises ######

##### SE (bonus): Finish reshaping earlier time series data ####

gather(CITYTEMPS, 
       Day, Temperature, 
       Day.1:Day.10) %>% 
  separate(Day, into=c("tmp","Day")) %>% 
  select(-tmp) %>% 
  mutate(Day = as.numeric(Day)) %>% 
  arrange(City, Day)


##### SE1: Reshaping and separating time series data #####


OBK <- read.csv("OBrienKaiser.csv")

str(OBK)

# Check how the default sep works...
OBK %>%
  rename(ID = X) %>%
  gather(Time, Measurement, pre.1:fup.5) %>%
  separate(Time, into = c("Period", "Time")) %>%
  head(30)

# Here is how you specify '.' as separator if desired
OBK %>%
  rename(ID = X) %>%
  gather(Time, Measurement, pre.1:fup.5) %>%
  separate(Time, into = c("Period", "Time"), sep = "\\.") %>%
  head(30)

# Use 'mutate' to convert some variable types
OBK %>%
  rename(ID = X) %>%
  gather(Time, Measurement, pre.1:fup.5) %>%
  separate(Time, into = c("Period", "Time")) %>%
  mutate(ID = factor(ID),
         Period = factor(Period),
         Time = as.numeric(Time),
         Measurement = as.numeric(Measurement)) %>% 
  str()


##### SE 2: Creating 'quarterly' variable for Weather data ######

# Using 'ifelse'
WEATHER %>% 
  mutate(Q = ifelse(Month <= 3,1,
                    ifelse(Month <= 6,2,
                           ifelse(Month <= 9,3,4)))) %>% 
  head(10)

# Using left_join
df_Q <- data_frame(Month = c(1:12),
                   Q = rep(1:4, each = 3))

left_join(WEATHER, df_Q) %>% 
  head(10)



###### SE 3: Joining by more than one variable ######

## CONTENT


###### Bonus 1: summarising individual-level moth data ######

MOTH_MASS <- read.csv("MOTH_MASS.csv")

str(MOTH_MASS)

## Find the sex-specific means and standard deviations of mass
MOTH_MASS %>%
  group_by(SEX) %>%
  summarise(Mass_mean = mean(MASS),
            Mass_sd = round(sd(MASS), digits = 2))

## Add further information from separate data frames

MOTH_MORPHS <- read.csv("MOTH_MORPHS.csv")

str(MOTH_MORPHS)

##
# Use 'left join' to add information from MOTH_MORPHS to MOTH_MASS.
#
# You can specify 'by = "ID"' if you want, but dplyr is clever and
#  will find matching columns automatically!
##

left_join(MOTH_MASS,
          MOTH_MORPHS)

##
# You can then chain further commands on to this, if you like...
##

left_join(MOTH_MASS,
          MOTH_MORPHS) %>%
  group_by(MORPH) %>%
  summarise(Mass_mean = round(mean(MASS), digits = 2),
            Mass_sd = round(sd(MASS), digits = 2))


##### Bonus 2: Chaining ggplot2 commands ######

library(ggplot2)

WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  filter(Year >= 2000 & Year <= 2010) %>%
  ggplot(aes(x = Month, y = TempMax, colour = Year)) +
  geom_point(size = 4, alpha = 0.5) + 
  stat_smooth() 

WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  filter(Year >= 2000 & Year <= 2010) %>%
  ggplot(aes(x = Month, y = TempMax, colour = factor(Year))) +
  geom_point(size = 4, alpha = 0.5) + 
  stat_smooth() 

WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  filter(Year >= 2000 & Year <= 2010) %>%
  ggplot(aes(x = Month, y = TempMax, colour = factor(Year))) +
  geom_point(size = 4, alpha = 0.5) + 
  stat_smooth() +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb",
                                "Mar", "Apr",
                                "May", "Jun",
                                "Jul", "Aug",
                                "Sep", "Oct",
                                "Nov", "Dec")) +
  ylab("Maximum temperature (degrees C)") +
  theme_bw()

## axis ticks??