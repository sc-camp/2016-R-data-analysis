library(tidyr) 
library(dplyr)


## Exercise 1
MOTHS <- read.csv("data/MothCount.csv")
MOTHS <- rename(MOTHS, Sex = X, Dark = Dark.morph, Light = Light.morph)
# > MOTHS
#      Sex Dark Light
# 1   Male    8     2
# 2 Female    4     6

##
# Convert our data into key/value pairs using 'gather'
# - 1. data [MOTHS]
# - 2. key / value column names to create [Morph, Count] 
# - 3. columns to gather [Dark:Light]
##
MOTHS_T <- gather(MOTHS, Morph, Count, Dark:Light)
# > MOTHS_T
#      Sex Morph Count
# 1   Male  Dark     8
# 2 Female  Dark     4
# 3   Male Light     2
# 4 Female Light     6

# > spread(MOTHS_T, Morph, Count)
#      Sex Dark Light
# 1 Female    4     6
# 2   Male    8     2

## Exercise 2
CITYTEMPS = read.csv("data/TOM_CITIES.csv")
CITYTEMPS_T = gather(CITYTEMPS, Day, Temperature, Day.1:Day.10) %>% separate(Day, into=c("V1","V2")) %>% select(-V1) %>% rename("Day"=V2)

