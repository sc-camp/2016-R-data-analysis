# Modelling growth in deer using polynomial regression

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

deers = data.table(read.csv("data/jaws.csv"))
print(summary(deers))
print(str(deers))

ggpairs(deers)

hist(deers$AGE)
hist(deers$BONE)
ggplot(data=deers, aes(x=AGE, y=BONE)) + geom_boxplot() + theme_bw()
ggplot(data=deers, aes(x=AGE, y=BONE)) + geom_point() + theme_bw()

ggplot(data=deers, aes(x=AGE, y=log(BONE))) + geom_point() + theme_bw()
## it is decidely not a logtransform :-)

par(mfrow=c(2,3))
lm.1 = lm(BONE~AGE, data=deers)
plot(lm.1)
hist(lm.1$residuals)
## crappy

mod.quad = lm(BONE~poly(AGE,2),data=deers)
plot(mod.quad)
hist(mod.quad$residuals)
summary(mod.quad)
## a bit better, residuals are more homogeneous but still skewed.

## let's compare the models
anova(lm.1, mod.quad)
AIC(lm.1)
AIC(mod.quad)
## better AIC and RSS for quadatric model


to_predict = seq(0, 55, .5)
predicted = predict(mod.quad, list(AGE=to_predict), int="c")
df_pred = cbind(data.frame(AGE=to_predict), predicted)

ggplot(data=deers) + geom_point(aes(y=BONE, x=AGE)) + theme_bw() + geom_line(data=df_pred, aes(x=AGE, y=fit), color="blue") + geom_ribbon(data=df_pred, aes(x=AGE, ymin=lwr, ymax=upr), alpha=.2)
## FU***K the fit decreases after age...


### we will do a Nonlinear least squares regression
# we might like to use a different function that better approximates how growth is known to work: the asymptotic exponential function:
# 𝑦 = 𝑎 − 𝑏 ∗ 𝑒!!∗!

## but we need to guess the starting points

# In our case, a represents the asymptotic value of the function, which we can guess is around 120 by consulting a scatterplot

# We can deduce a starting value for b by setting age to zero, in which case the value of b is derived by rearranging the equation b = a – intercept. If the intercept is around 10, then we can guess that b is around 110.

# To guess parameter c, we must inspect the plot at the steepest part of the curve (in these data, when AGE is around 5, and bone is about 40). We can guess the value for c by rearranging the equation again given the values of a and b specified: c = -log ((a – y)/b)/x. When AGE is 5, BONE is 40 or so, and therefore
# c = -log ((120 – 40)/110)/5 = 0.063. A bit painful, but we now have three starting values. Use them to build a NLS model.

starting_points = list(a=120,b=110,c=0.063)
MOD.ASYMP<-nls(BONE~a-b*exp(-c*AGE), data=deers, start=starting_points)

## or use the auto function but still need to check for init param sensitivity
MOD.ASYMP.SS<-nls(BONE~SSasymp(AGE,a,b,c),data=deers)

library(nlstools)
plot(MOD.ASYMP)
plot(nlsResiduals(MOD.ASYMP))

plot(MOD.ASYMP.SS)
plot(nlsResiduals(MOD.ASYMP.SS))

summary(MOD.ASYMP)
summary(MOD.ASYMP.SS)

anova(MOD.ASYMP, MOD.ASYMP.SS)
## same RSS but coeff are differents

### there is a problem with coefficients
MOD.ASYMP.2<-nls(BONE~a*(1-exp(-c*AGE)), data=deers,start=list(a=120,c=0.064))
plot(nlsResiduals(MOD.ASYMP.2))

anova(MOD.ASYMP, MOD.ASYMP.2)
## same but simpler

## let's plot
predicted = predict(MOD.ASYMP.2, list(AGE=to_predict)) # we cannot get confint
df_pred = cbind(data.frame(AGE=to_predict), predicted)

ggplot(data=deers) + geom_point(aes(y=BONE, x=AGE)) + theme_bw() + geom_line(data=df_pred, aes(x=AGE, y=predicted), color="blue") 

