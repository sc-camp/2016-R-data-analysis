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

data_species = data.table(read.csv("data/SPECIES.csv"))
print(summary(data_species))
print(str(data_species))

ggpairs(data_species)

ggplot(data=data_species, aes(x=RICHNESS)) + theme_bw() + geom_bar(binwidth=3)
ggplot(data=data_species, aes(x=BIOMASS)) + theme_bw() + geom_bar(binwidth=1)

## RICHNESS is discrete. Bounded to 0. looks like poisson distribution.
## BIOMASS is continuous. Bounded to 0

p1 = ggplot(data_species, aes(x=BIOMASS, y=RICHNESS, color=PH)) + theme_bw() + geom_point() + geom_smooth(method="lm")
p2 = ggplot(data_species) + theme_bw() + geom_point(aes(x=BIOMASS, y=RICHNESS))
p3 = ggplot(data_species) + theme_bw() + geom_point(aes(x=PH, y=RICHNESS))

grid.arrange(p1, p2, p3, ncol=3)

p_log = ggplot(data_species, aes(x=BIOMASS, y=log(RICHNESS), color=PH)) + theme_bw() + geom_point() + geom_smooth(method="lm", se=FALSE)
print(p_log)
## if we use log space we can see that there is an interaction.


MOD.1<-glm(RICHNESS~BIOMASS*PH,data=data_species,family=poisson)
summary(MOD.1)
## Residual deviance:  83.201  on 84  degrees of freedom so we are good.

### try other stuff to see ho it is going
summary(glm(RICHNESS~BIOMASS+PH,data=data_species,family=poisson))  ## might be ok
summary(glm(RICHNESS~BIOMASS*PH,data=data_species,family=gaussian)) ## overdispersion
summary(glm(RICHNESS~BIOMASS+PH,data=data_species,family=gaussian)) ## overdispersion

par(mfrow=c(2,2))
plot(MOD.1)
## good enough but we prefer the next one:
glm.diag.plots(MOD.1)


## try a model simplification
MOD.2 = update(MOD.1, ~. - BIOMASS:PH)
summary(MOD.2)

## analyze variance difference with X2 because it is a glm: no analytic solution.
anova(MOD.1, MOD.2, test="Chi")
## p-value is 0.0003288 so we have to keep the first model.


data_to_predict = copy(data_species)
data_to_predict[,RICHNESS:=NULL]

## glm so we use link and not response to get correct SE
data_to_predict$PRED_RICHNESS = predict(MOD.1, data_to_predict, type="link", se.fit = TRUE)$fit
data_to_predict$PRED_SE = predict(MOD.1, data_to_predict, type="link", se.fit = TRUE)$se.fit

full_data = merge(data_species, data_to_predict, by=c("PH", "BIOMASS"))

## Get a 'tidy' version of the model coefficients
tidy(MOD.1)

## Get a 'tidy' version of the model summary statistics
glance(MOD.1)

## 'Augment' adds columns to the original data set
augment(MOD.1)


p_final = ggplot(full_data) + theme_bw() + geom_line(aes(x=BIOMASS, y=exp(PRED_RICHNESS), color=PH)) + geom_point(aes(x=BIOMASS, y=RICHNESS, color=PH)) + ylab("RICHNESS") + geom_ribbon(aes(x=BIOMASS, ymin = exp(PRED_RICHNESS-1.96*PRED_SE), ymax = exp(PRED_RICHNESS+1.96*PRED_SE), group=PH, fill=PH), alpha=.3)

print(p_final)

visreg(MOD.1, xvar = "BIOMASS", scale = "response", rug=FALSE, by="PH", overlay=TRUE)


