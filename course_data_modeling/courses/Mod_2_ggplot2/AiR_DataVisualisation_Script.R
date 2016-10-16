
#### Overview ####
# 
# Script for the practical from Module 2 'Effective data visualisation'
#  from Advancing in R course, PR~Statistics
#
###

#### Load libraries ####

library(ggplot2)
library(dplyr)

#### Load data ####

load("Unicorns.RData")

str(UNICORNS)


###### Investigating geoms (geometric representations of data) ######

###### Histograms ######

ggplot(UNICORNS, aes(x = HornLength)) + 
  geom_histogram()

# Modify bin size
ggplot(UNICORNS, aes(x = HornLength)) + 
  geom_histogram(binwidth = 4)

# Play with other parameters of the histogram
ggplot(UNICORNS, aes(x = HornLength)) + 
  geom_histogram(binwidth = 4,
                 fill = "white",
                 colour = "black")

# Try changing the theme
ggplot(UNICORNS, aes(x = HornLength)) + 
  geom_histogram(binwidth = 4,
                 fill = "white",
                 colour = "black") +
  theme_bw()


###### Scatterplots ######

ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter()

# You can play with the amount of jitter on the x axis...
ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02))

ggplot(UNICORNS, aes(x = Height, y = HornLength,
                     colour = Magic)) + 
  geom_jitter(position = position_jitter(width = 0.02))


###### Boxplots ######

ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + 
  geom_boxplot() 

# Add jittered points
ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.1))


ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + 
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2),
              alpha = 0.4)


###### Statistical transformations ######

###### Summarising data ######

ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + 
  geom_boxplot() +
  stat_summary(fun.y = "mean", 
               geom = "point",
               shape = 5,
               size = 4)


# Identify min/max values
ggplot(UNICORNS, aes(x = Magic, y = HornLength)) + 
  geom_boxplot() +
  stat_summary(fun.y = "min", 
               geom = "point",
               shape = 2,
               size = 4) +
  stat_summary(fun.y = "max", 
               geom = "point",
               shape = 6,
               size = 4)



###### Smoothers ######

ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm")


# Colour the points (according to variable Magic), but retain a single smoother
ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02),
              aes(colour = Magic)) + 
  stat_smooth(method = "lm")


# Colour the points (according to variable Magic), 
#  with a smoother for each level of variable Magic
ggplot(UNICORNS, aes(x = Height, y = HornLength,
                     colour = Magic)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm")



###### Faceting ######

## Facet grids
ggplot(UNICORNS, aes(x = Height, y = HornLength, colour = Magic)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm") +
  facet_grid(. ~ Magic)

# Redundancy is MAGIC: note above that it has been specified twice
ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm") +
  facet_grid(. ~ Magic)

# Facet grid also enables us to facet by two variables
# - this is particularly useful for multiple interactions
ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm") +
  facet_grid(ManeStyle ~ Magic)


## Facet wraps
ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm") +
  facet_wrap(~ Magic)

## What is the difference when we free the scales for each panel?
ggplot(UNICORNS, aes(x = Height, y = HornLength)) + 
  geom_jitter(position = position_jitter(width = 0.02)) + 
  stat_smooth(method = "lm") +
  facet_wrap(~ Magic, scales = "free")


###### Supplementary exercises ######

###### Time series data ######

library(tidyr)

OBK <- read.csv("OBrienKaiser.csv")

OBK_tidy <- OBK %>%
  rename(ID = X) %>%
  gather(Time, Measurement, pre.1:fup.5) %>%
  separate(Time, into = c("Period", "Time")) 

OBK_tidy %>% 
  ggplot(aes(x = Measurement)) +
  geom_histogram(binwidth = 1.5)

OBK_tidy %>% 
  ggplot(aes(x = Measurement)) +
  geom_histogram(binwidth = 1.5) +
  facet_grid(.~treatment)

OBK_tidy %>% 
  ggplot(aes(x = Measurement, fill = treatment)) +
  geom_density(alpha = 0.4)

OBK_tidy %>%
  ggplot(aes(x = Time,
             y = Measurement,
             colour = treatment)) +
  geom_point(size = 4,
             alpha = 0.7) +
  geom_line(aes(group = ID)) +
  facet_grid(. ~ Period)

OBK_tidy %>%
  ggplot(aes(x = Time,
             y = Measurement,
             colour = treatment)) +
  geom_point(size = 4,
             alpha = 0.7) +
  geom_line(aes(group = ID)) +
  facet_grid(gender ~ Period)

OBK_tidy %>%
  ggplot(aes(x = Time,
             y = Measurement,
             colour = treatment)) +
  geom_point(size = 4,
             alpha = 0.7) +
  geom_line(aes(group = ID)) +
  facet_grid(Period ~ gender)


OBK_tidy %>%
  ggplot(aes(x = Time,
             y = Measurement,
             colour = treatment)) +
  geom_point(size = 4,
             alpha = 0.7) +
  geom_line(aes(group = ID)) +
  facet_grid(Period ~ gender,
             labeller = label_both)


###### Piping data manipulations into scatterplots ######


##
# Requires loading your data from the previous module
##

WEATHER %>% 
  select(Location, Year, Month, TempMax) %>% 
  filter(Year <= 2014) %>%
  arrange(desc(Year), desc(TempMax)) %>%
  ggplot(aes(x = Month, y = TempMax,
             colour = Location)) +
  geom_point() +
  stat_smooth(method = "lm")
