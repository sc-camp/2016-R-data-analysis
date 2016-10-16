
##
# Using ggplot2 to make the graphics for air / soil temperature
#  against elevation, as at the end of Practical 3.
#
# Assumes that you have done all the previous steps in practical 3
#  such that you have models for AIR.MOD and SOIL.MOD, as well
#  as having the data loaded into the environment.
##

#### Libraries ####

library(tidyr)
library(dplyr)

library(ggplot2)
library(gridExtra)

#### Create predicted values ####

summary(CGFIX)
NEWXVARS.AIR<-seq(650,1113,1)
# generate predictions of model
NEWYVARS.AIR<-predict(AIR.MOD,newdata=list(ELEVATION=NEWXVARS.AIR),int="c")

# same for SOIL
NEWXVARS.SOIL<-seq(650,1113,1)
# generate predictions of model
NEWYVARS.SOIL<-predict(SOIL.MOD,newdata=list(ELEVATION=NEWXVARS.SOIL),int="c")

##
# Convert to data frames
##

##
#  newx: your range of new x values
#  newy: predicted y values for your range of x
#  newupr: the upper 95% confidence intervals
#  newlwr: the lower 95% confidence intervals
##
df_AIR_pred <- data_frame(newx = NEWXVARS.AIR,
                         newy = as.numeric(NEWYVARS.AIR[,"fit"]),
                         newupr = as.numeric(NEWYVARS.AIR[,"upr"]),
                         newlwr = as.numeric(NEWYVARS.AIR[,"lwr"]))

df_SOIL_pred <- data_frame(newx = NEWXVARS.SOIL,
                          newy = as.numeric(NEWYVARS.SOIL[,"fit"]),
                          newupr = as.numeric(NEWYVARS.SOIL[,"upr"]),
                          newlwr = as.numeric(NEWYVARS.SOIL[,"lwr"]))


#### Method 1: Create separate plots and draw them side-by-side ####

##
# Create plots and assign to objects
##

gg_AIR <- ggplot(df_AIR_pred, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr),
              alpha = 0.25) +
  geom_point(data = CGFIX, 
             aes(x = ELEVATION,
                 y = AIR.TEMP.CORR)) +
  labs(x = "Altitude (m)",
       y = "Corrected Air Temperature (degs C)",
       title = "Air Temperature") +
  ylim(c(-6,6)) +
  theme_classic()

gg_SOIL <- ggplot(df_SOIL_pred, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr),
              alpha = 0.25) +
  geom_point(data = CGFIX, 
             aes(x = ELEVATION,
                 y = SOIL.TEMP.CORR)) +
  labs(x = "Altitude (m)",
       y = "Corrected Soil Temperature (degs C)",
       title = "Soil Temperature") +
  ylim(c(-6,6)) +
  theme_classic()

##
# 'grid.arrange' needs ggplot objects, and we can then tell it how 
#   many columns we want these objects to be plotted into
##

grid.arrange(gg_AIR,
             gg_SOIL,
             ncol = 2)

## Gives a warning message that a point has been removed - why?

CGFIX %>% 
  arrange(desc(SOIL.TEMP.CORR)) %>% 
  head()

# One of the soil values is >6, so has been cut off when we specified the limits
#  of (6,-6) for our y axis.



#### Method 2: Create a single data frame and facet by temperature type ####

##
# Need to create a long-format data set, with a modifier variable 'Type' to
#  tell us whether we are looking at air or soil temperature
##

df_temp_pred <- bind_rows(df_AIR_pred,
                          df_SOIL_pred)

df_temp_pred$Type <- factor(c(rep("Air", times = nrow(df_AIR_pred)),
                              rep("Soil", times = nrow(df_SOIL_pred))))

str(df_temp_pred)

## 
# Now also need to turn the raw data into long format, and add a
#  'Type' variable so that ggplot knows how to break up the data 
#  when we facet it.
#
# The values within Type must match across our two data frames in
#  order for the facet_grid specification to work, so let's rename
#  the columns in CGFIX before we gather them into long format
##
CGFIX_T <- CGFIX %>% 
  rename(Air = AIR.TEMP.CORR,
         Soil = SOIL.TEMP.CORR) %>% 
  gather(., Type, Temperature, Air:Soil)

##
# Now plot the predicted lines, with the raw data, and split by Type
##
ggplot(df_temp_pred, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr),
              alpha = 0.25) +
  geom_point(data = CGFIX_T, 
             aes(x = ELEVATION,
                 y = Temperature)) +
  labs(x = "Altitude (m)",
       y = "Corrected Temperature (degs C)") +
  ylim(c(-6,6)) +
  facet_grid(.~Type) +
  theme_bw()
