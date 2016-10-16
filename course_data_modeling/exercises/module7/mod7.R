library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(boot)
library(data.table)
library(gridExtra)
library(GGally)

tannin = data.table(read.csv("data/tannin.csv"))
print(summary(tannin))
print(str(tannin))
ggpairs(tannin)

hist(tannin$GROWTH)
hist(tannin$TANNIN)

tanfit <- lm(GROWTH ~ TANNIN, data = tannin)
summary(tanfit)
## Get a 'tidy' version of the model coefficients
tidy(tanfit)
glance(tanfit)

tanfit %>%
  augment() %>%
  ggplot(data = .,
         aes(x = TANNIN,
             y = GROWTH)) +
  geom_point() +                  # points from original data frame
  geom_line(aes(y = .fitted)) +   # line from model fit
  geom_ribbon(aes(ymin = .fitted - (2 * .se.fit),
                  ymax = .fitted + (2 * .se.fit)),   # CIs from model fit
              alpha = 0.3) +
  theme_bw()

## OR
visreg(tanfit)


### ex 2
comp = data.table(read.csv("data/compensationo.csv"))
print(summary(comp))
print(str(comp))
ggpairs(comp)

hist(comp$ROOT)
hist(comp$FRUIT)
# hist(comp$GRAZING)

## there was no interesting interaction in the model, we rebuild it.
comp_mod = lm(FRUIT~ROOT+GRAZING, data=comp)
tidy(comp_mod)

### std ggplot
comp_mod %>%
  augment() %>%
  ggplot(data = .,
         aes(x = ROOT,
             y = FRUIT,
             colour = GRAZING,
             fill = GRAZING)) +
  geom_point() +                  # points from original data frame
  geom_line(aes(y = .fitted)) +   # line from model fit
  geom_ribbon(aes(ymin = .fitted - (1.96 * .se.fit),
                  ymax = .fitted + (1.96 * .se.fit)),   # CIs from model fit
              alpha = 0.3,
              linetype = 0) +
  theme_bw()

## OR:
visreg(comp_mod) # by default plot is fixed with the median but can be changed with cond=
median(comp$ROOT)
visreg(comp_mod, xvar = "GRAZING", cond=list(ROOT=10))

visreg(comp_mod, xvar = "ROOT", by="GRAZING") 

### Ex 3:
isle = data.table(read.csv("data/isolation.csv"))
print(summary(isle))
print(str(isle))
ggpairs(isle)

hist(isle$ISOLATION)
hist(isle$INCIDENCE) ## yes or no. it is a binomial distribution
hist(isle$AREA)


anova(glm(INCIDENCE~AREA*ISOLATION,data=isle,binomial), glm(INCIDENCE~AREA+ISOLATION,data=isle,binomial), test="Chi")
## interaction is not present

mod_isol<-glm(INCIDENCE~AREA+ISOLATION,data=isle,binomial)
tidy(mod_isol)
augment(mod_isol)

visreg(mod_isol)

visreg(mod_isol, xvar = "ISOLATION") ## displays in logit scale
visreg(mod_isol, xvar = "AREA")

visreg(mod_isol, xvar = "ISOLATION", scale = "response", rug=FALSE, ylab="Incidence",xlab="Isolation")
visreg(mod_isol, xvar = "AREA", scale = "response", rug=FALSE)

### FLY sex
DF <- read.csv("data/FLYSEX.csv")
str(DF)
# quality control
hist(DF$LEG)
hist(DF$ABDOMEN)
hist(DF$ATTR)
# def poisson

# simple bivar plots
plot(ATTR~LEG,data=DF)
plot(ATTR~ABDOMEN,data=DF)

# prelim look at combined effects
ggplot(DF, aes(x = LEG,
               y = ABDOMEN,
               colour = ATTR)) +
  geom_jitter(alpha = 0.8,
              size = 3)


head(DF)
ORN.MOD<-glm(ATTR~LEG*ABDOMEN,data=DF,family=poisson)
summary(ORN.MOD)
# underdispersion

ORN.MOD2<-glm(ATTR~LEG*ABDOMEN,data=DF,family=quasipoisson)
summary(ORN.MOD2)

library(boot)
glm.diag.plots(ORN.MOD)
glm.diag.plots(ORN.MOD2)


anova(ORN.MOD, ORN.MOD2, test="Chi")
## same in fact.

## simplify model?
ORN.MOD21<-update(ORN.MOD2, ~. -LEG:ABDOMEN)
anova(ORN.MOD2,ORN.MOD21,test="Chi")
## interaction is important we must keep it

visreg(ORN.MOD2, xvar="LEG", by="ABDOMEN", scale="response", overlay=TRUE)
visreg(ORN.MOD2, xvar="ABDOMEN", by="LEG", scale="response", overlay=TRUE)

visreg(ORN.MOD2, xvar="LEG", by="ABDOMEN", overlay=TRUE)
visreg(ORN.MOD2, xvar="ABDOMEN", by="LEG", overlay=TRUE)





