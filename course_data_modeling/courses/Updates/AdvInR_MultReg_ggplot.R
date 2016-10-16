

##
# Using ggplot2 to make the graphics for the partial effect of infection score
#  on pups born
#
# Assumes that you have done all the previous steps in practical 4
#  such that you have the correct model (here 'MOD.3'), as well
#  as having the data loaded into the environment.
##

#### Libraries ####

library(tidyr)
library(dplyr)
library(ggplot2)

#### Create data frame for predictions ####

##
# As in Luc's script...
##

summary(WP)
NEWWORMS<-seq(0.01,1.22,0.01)

# the next line tells me how many rows are in NEWWORMS
length(NEWWORMS)
# the next line will create a vector with the same number of rows, but all set to the mean value for CALVES
MEANCALVES<-rep(mean(WP$CALVES),122)

# now use predict to generate y-variables
# note that the newdata statement below must include a reference for ALL predictors in the original model
# by setting the CALVES column to be a series of means, we're effectively controlling for the influence of CALVES, and only getting the partial effect of WORMS
NEWYUSINGWORMS<-predict(MOD.3,newdata=list(WORMS=NEWWORMS,CALVES=MEANCALVES),int="c")

## Create new DF
df_worms_pred <- data_frame(newx = NEWWORMS,
                            newy = as.numeric(NEWYUSINGWORMS[,"fit"]),
                            newupr = as.numeric(NEWYUSINGWORMS[,"upr"]),
                            newlwr = as.numeric(NEWYUSINGWORMS[,"lwr"]))


#### Plot predictions with raw data ####

# Plot predicted lines with original data
#  - point size and shape changed to mirror base style (gross)
ggplot(df_worms_pred, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr),
              alpha = 0.15) +
  geom_point(data = WP, 
             aes(x = WORMS,
                 y = PUPS),
             size = 4,
             shape = 21) +
  labs(x = "Infection score",
       y = "Pups born") +
  theme_classic()


#### Resids-resids model ####

## Create two models required
RESWORMSONOTHERPREDICTORS <- lm(WORMS ~ CALVES, data=WP)
RESPUPSNOWORMS <- lm(PUPS ~ CALVES, data=WP)

## Create new data frame with residuals from that model
df_worms_resids <- data_frame(resid_worms = RESWORMSONOTHERPREDICTORS$residuals,
                              resid_pups = RESPUPSNOWORMS$residuals)

## Plot points and linear regression smoother for these
ggplot(df_worms_resids, aes(x = resid_worms,
                            y = resid_pups)) +
  geom_point() +
  stat_smooth(method = "lm",
              colour = "black") +
  labs(x = "Residual infection score",
       y = "Residual pups born") +
  theme_classic()

##
# stat_smooth automatically gives 95% CIs;
#  you could use predict on a regression instead but it is ok
#  to just use this when we are actually just using y~x for the model
##



