library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(boot)
library(data.table)
library(gridExtra)
library(GGally)
library(visreg)

grouse = data.table(read.csv("data/grouse_shooting_mod.csv"))
print(summary(grouse))
print(str(grouse))

ggpairs(grouse)

hist(grouse$nr_shot)
hist(grouse$totalcount)
hist(grouse$km2)
hist(grouse$prev)

# The data are currently not yet ready for analysis, because the number of birds shot is not yet standardized by the size of the sampling location

grouse$shot_per_km2 = grouse$nr_shot/grouse$km2

# You will want to log-transform both this newly generated variable and the predictor for the number of birds estimated in the earlier survey (totalcount)
grouse$log_shot_per_km2 = log(grouse$shot_per_km2)
grouse$log_totalcount = log(grouse$totalcount)

plot(log_shot_per_km2~log_totalcount, data=grouse)
plot(log_shot_per_km2~prev, data=grouse) # prev is a factorial

ggplot(grouse, aes(x = log_totalcount, y = log_shot_per_km2)) + 
  geom_point() + theme_bw() +
  stat_smooth(method = "lm") +
  facet_grid(. ~ prev)

lm_1 = lm(log_shot_per_km2~log_totalcount*prev, data=grouse)
summary(lm_1)

par(mfrow=c(2,2))
plot(lm_1)
par(mfrow=c(1,1))

## is there a more simple model?
lm_2 = update(lm_1, ~. -log_totalcount:prev)
anova(lm_1, lm_2)
AIC(lm_1)
AIC(lm_2)
## yes, lm_2 is simpler AND ok.

visreg(lm_2, xvar="log_totalcount", by="prev", overlay=TRUE)

summary(lm_2)
par(mfrow=c(2,2))
plot(lm_2)
par(mfrow=c(1,1))

## now we try the fixed model with all 
lm_fix = lm(log_shot_per_km2~moor+drive+log_totalcount+prev, data=grouse)
summary(lm_fix)

visreg(lm_fix)
visreg(lm_fix, xvar="log_totalcount", by="prev", overlay=TRUE)


## time for mix model
library(lme4)
g1.mixed<-lmer(log_shot_per_km2~prev*log_totalcount+(1|moor)+(1|drive),data=grouse)
summary(g1.mixed)

## interaction??
g2.mixed<-update(g1.mixed, ~. -prev:log_totalcount)

anova(g1.mixed, g2.mixed)
## g2 is ok. better AIC
summary(g2.mixed)

# can we get rid of something else?
anova(g2.mixed, update(g2.mixed, ~. -prev))
anova(g2.mixed, update(g2.mixed, ~. -log_totalcount))
## NO

visreg(g2.mixed, xvar="log_totalcount", by="prev", overlay=TRUE)
plot(g2.mixed)

## test with corrected AIC: AICc takes into account small samples sizes
library(MuMIn)
AICc(g1.mixed)
AICc(g2.mixed)

## First make a function to label our facets nicely
shootlabel <- function(var, value) {
  if(var == "prev"){
    value[value == 1] <- "First shoot"
    value[value == 2] <- "Second shoot"
    value[value == 3] <- "Third/fourth shoot"
  }
  return(value)
}

augment(g2.mixed) %>%
  ggplot(., aes(x = log_totalcount, y = log_shot_per_km2)) +
  geom_point() +
  geom_line(aes(x = log_totalcount, y = .fixed)) +
  labs(y="Grouse shot per km2 (log)",
       x="Grouse counted per km2 (log)") +
  facet_grid(.~prev, labeller = shootlabel) +
  theme_bw()

