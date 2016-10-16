
#### Dance fly silhouettes ####

library(dplyr)
library(ggplot2)
library(broom)

DF <- read.csv("FLYSEX.csv")

# assumes model is ok
ORN.MOD.Q<-glm(ATTR~LEG*ABDOMEN,data=DF,family=quasipoisson)

summary(ORN.MOD.Q)

#### .Predict ####

# let's use predict instead...

##
# Set up our predictor sequences...
# I like to do all the predictions for a single figure in one go
##
df_ABD_BY_LEG <- expand.grid(NEW_ABD = seq(-3,3, length = 100),
                             LEG_MINMAX = c(-2,0,2))

df_LEG_BY_ABD <- expand.grid(NEW_LEG = seq(-3,3, length = 100),
                             ABD_MINMAX = c(-2,0,2))

##
# Predict!
##

## Continuous abdomen by specific values of leg

PREDS_ABD_BY_LEG <- predict(ORN.MOD.Q,
                            newdata=list(ABDOMEN = df_ABD_BY_LEG$NEW_ABD,
                                         LEG = df_ABD_BY_LEG$LEG_MINMAX),
                            se.fit=TRUE)

df_ABD_BY_LEG <- cbind(df_ABD_BY_LEG,
                       PREDS_ABD_BY_LEG)

df_ABD_BY_LEG %>% 
  head()


## Continuous leg by specific values of abdomen

PREDS_LEG_BY_ABD <- predict(ORN.MOD.Q,
                            newdata=list(LEG = df_LEG_BY_ABD$NEW_LEG,
                                         ABDOMEN = df_LEG_BY_ABD$ABD_MINMAX),
                            se.fit=TRUE)

df_LEG_BY_ABD <- cbind(df_LEG_BY_ABD,
                       PREDS_LEG_BY_ABD)


#### .Plot predictions ####

gg_abd_log <- df_ABD_BY_LEG %>% 
ggplot(.,
       aes(x = NEW_ABD, y = fit, colour = factor(LEG_MINMAX))) +
  geom_line() +
  labs(x = "Abdomen",
       y = "Attractiveness (log-transformed)",
       title = "Effect of abdomen on attractiveness\n
       for different values of leg") +
  guides(colour = guide_legend(title = "Leg")) +
  theme_classic()

gg_leg_log <- df_LEG_BY_ABD %>% 
  ggplot(.,
         aes(x = NEW_LEG, y = fit, colour = factor(ABD_MINMAX))) +
  geom_line() +
  labs(x = "Leg",
       y = "Attractiveness (log-transformed)",
       title = "Effect of leg on attractiveness\n
       for different values of abdomen") +
  guides(colour = guide_legend(title = "Abdomen")) +
  theme_classic()

##
# Do the same plots, but back-transform the results
##

gg_abd_raw <- df_ABD_BY_LEG %>% 
  ggplot(.,
         aes(x = NEW_ABD, y = exp(fit), colour = factor(LEG_MINMAX))) +
  geom_line() +
  labs(x = "Abdomen",
       y = "Attractiveness",
       title = "Effect of abdomen on attractiveness\n
       for different values of leg") +
  guides(colour = guide_legend(title = "Leg")) +
  theme_classic()

gg_leg_raw <- df_LEG_BY_ABD %>% 
  ggplot(.,
         aes(x = NEW_LEG, y = exp(fit), colour = factor(ABD_MINMAX))) +
  geom_line() +
  labs(x = "Leg",
       y = "Attractiveness",
       title = "Effect of leg on attractiveness\n
       for different values of abdomen") +
  guides(colour = guide_legend(title = "Abdomen")) +
  theme_classic()

##
# Plot them all together!
##

library(gridExtra)

##
# We use grid.arrange from the gridExtra package to
#  place all four of our plots on a single panel,
#  separated into two columns
##
grid.arrange(gg_abd_log, gg_leg_log,
             gg_abd_raw, gg_leg_raw,
             ncol = 2)
