library(ggplot2)
library(tidyr)
library(broom)
library(dplyr)
library(car)
library(GGally)

pups_data = (read.csv("data/WOLFPUPS.csv"))
summary(pups_data)

## 2016 year is weird
pups_data = pups_data %>% filter(YEAR != 2016)

pairs(pups_data)
pairs(pups_data %>% select(-YEAR))
ggpairs(pups_data %>% select(-YEAR))

mod.1 = lm(PUPS~ADULTS+CALVES+WORMS+PERMITS, data=pups_data)
summary(mod.1)

par(mfrow=c(2,2))
plot(mod.1)
par(mfrow=c(1,1))

vif(mod.1)

mod.2<-update(mod.1,~. - ADULTS)
summary(mod.2)

vif(mod.2)

# Note that we did not use the anova() command to compare MOD.1 and MOD.2. Why not? The order of operations here is extremely important. First, we found a model that was not troubled by collinearity, then we used likelihood ratio tests to find the minimal adequate model. Never use anova to decide whether to retain highly collinear variables, as their collinearity is more than enough reason to get rid of them, and is expected to artificially and inappropriately decrease model deviance.
# anova(mod.1, mod.2)

mod.3<-update(mod.2,~. - PERMITS)
summary(mod.3)
vif(mod.3)
anova(mod.2, mod.3)

mod.4<-update(mod.3,~. - WORMS)
mod.5<-update(mod.3,~. - CALVES)

anova(mod.3, mod.4)
anova(mod.3, mod.5)
## RSS increase is too high and p-value<0.05 so we cannot reject the null hypothesis meaning that the term can be removed. 
# --> we keep the term

mod.3s<-lm(PUPS~scale(CALVES)+scale(WORMS),data=pups_data)
summary(mod.3s)

avPlots(mod.3)

confint(mod.3s)
summary(mod.3s)

