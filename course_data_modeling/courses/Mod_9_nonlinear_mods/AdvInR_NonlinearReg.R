# Luc Bussière
# AdvInR Nonlinear regression
# Nov 4, 2012

# last modified Nov 28, 2015


# clear R of all objects
rm(list=ls())


# import the data & see columns & structure
DECAY<-read.csv("decay.csv")
names(DECAY)
str(DECAY)

# check data quality
hist(DECAY$TIME)
hist(DECAY$MASSLEFT)
# note distribution of response is skewed -- we'll try several approaches to modelling this and compare them

# plot the raw data
par(mfrow=c(1,1))
plot(MASSLEFT~TIME, data=DECAY)

# so a clear negative trend, but it looks curvilinear	 

# let's quickly look at the diagnostics for a linear model to see what they look like

MOD.LINEAR<-lm(MASSLEFT~TIME, data=DECAY)

par(mfrow=c(2,3))
plot(MOD.LINEAR)
hist(MOD.LINEAR$residuals)
par(mfrow=c(1,1))

# note the u-shaped pattern in the first plot, which strongly suggests we need to fit a curve


# let's first look at some transformations
par(mfrow=c(2,2))
hist(DECAY$MASSLEFT)
hist(log(DECAY$MASSLEFT))
hist(log10(DECAY$MASSLEFT))
hist(sqrt(DECAY$MASSLEFT))
par(mfrow=c(1,1))

# By far the best looking transformation is the log transform, which makes sense because decay is an inverse exponential function in theory

# create a model, with transformation
MOD.LOG<-lm(log(MASSLEFT)~TIME,data=DECAY)
# note that you can apply the transform in the function call

par(mfrow=c(2,3))
plot(MOD.LOG)
hist(MOD.LOG$residuals)
par(mfrow=c(1,1))

# this is already much better, although not yet perfect
summary(MOD.LOG)

# we can visualize the new fit to see how the model performs


summary(DECAY)
NEWTIME<-seq(0,30,0.25)

# to add lines I have two choices: I can either plot this on the log scale:
NEWML<-predict(MOD.LOG,newdata=list(TIME=NEWTIME),int="c")
plot(log(MASSLEFT)~TIME, data=DECAY)
matlines(NEWTIME,NEWML,lty=c(1,2,2),col="black")

# or I can back-transform the data and plot a curve, which is more fun -- note I need to exponentiate the predicted values
NEWML<-predict(MOD.LOG,newdata=list(TIME=NEWTIME),int="c")
plot(MASSLEFT~TIME, data=DECAY)
matlines(NEWTIME,exp(NEWML),lty=c(1,2,2),col="black")

# that's a pretty good fit, I'd say, and it's a rather simple model with only a single parameter, which is ideal

##
# Plot the original data with fitted slope and confidence interval
#  using broom and ggplot
##

library(broom)
library(ggplot2)
library(dplyr)

MOD.LOG %>%
  augment() %>%
  ggplot(., aes(x = TIME, y = exp(log.MASSLEFT.))) +
  geom_point(size = 3) +
  geom_line(aes(x = TIME, y = exp(.fitted))) +
  geom_ribbon(aes(ymin = exp(.fitted - (1.96*.se.fit)),
                  ymax = exp(.fitted + (1.96*.se.fit))),
              alpha = 0.5) +
  theme_bw() +
  labs(x = "Time",
       y = "Mass left (g)")

################
# can skip lines 80 to 109
# for fun, let's see how this would work using a polynomial

MOD.POLY<-lm(MASSLEFT~poly(TIME,2),data=DECAY) 
# one way to model poly, ensures orthogonal quadratic term

MOD.POLY2<-lm(MASSLEFT~TIME+I(TIME^2),data=DECAY) 
# another way to model poly but less preferred b/c quad corr with x
library(car)
vif(MOD.POLY2)
# note high vifs, which are not present for the first model
vif(MOD.POLY)



# check diagnostics 
par(mfrow=c(2,3))
plot(MOD.POLY)
hist(MOD.POLY$residuals)
par(mfrow=c(1,1))
# some diags better, others worse than transform

summary(MOD.POLY)
# note that there are now two parameters

NEWML.POLY<-predict(MOD.POLY,newdata=list(TIME=NEWTIME),int="c")
plot(MASSLEFT~TIME, data=DECAY)
matlines(NEWTIME,NEWML.POLY,lty=c(1,2,2),col="black")
# the fit here is also pretty good -- in fact the R-squared is higher than for the single parameter curve, but that's not surprising
# I think one should favour the single parameter model for two reasons: it fits theory about exponential decay (i.e., you don't get silly predictions where the material starts to increase over time towards the top the range of x), and it is more parsimonious


# pub quality plot
plot(MASSLEFT~TIME, data=DECAY,
     ylab=list("Biomass remaining (g)",cex=1.5),
     xlab=list("Time",cex=1.5),
     pch=16,
     bg="grey")
matlines(NEWTIME,exp(NEWML),lty=c(1,2,2),col="black")


# now let's study jaw bone length as a function of age in deer

# clear R of all objects
rm(list=ls())
DEER<-read.csv("jaws.csv")
str(DEER)
head(DEER)

# check data quality
hist(DEER$AGE)
hist(DEER$BONE)


par(mfrow=c(2,2))
hist(DEER$BONE)
hist(I(DEER$BONE)^2)
hist(sqrt(DEER$BONE))
hist(log(DEER$BONE))
par(mfrow=c(1,1))

# not readily transformable, which shouldn't surprise us

plot(DEER$AGE,DEER$BONE)
# function looks asymptotic

# for kicks, make a linear model to see how badly it fits
MOD.LINEAR.D<-lm(BONE~AGE, data=DEER)

par(mfrow=c(2,3))
plot(MOD.LINEAR.D)
hist(MOD.LINEAR.D$residuals)
par(mfrow=c(1,1))
# yuck!

MOD.POLY.D<-lm(BONE~poly(AGE,2),data=DEER) 


par(mfrow=c(2,3))
plot(MOD.POLY.D)
hist(MOD.POLY.D$residuals)
par(mfrow=c(1,1))
# better but not brilliant

# let's have a look anyway
summary(MOD.POLY.D)

summary(DEER)

NEWAGE<-seq(0,51,0.5)
NEWBONE.POLY<-predict(MOD.POLY.D,newdata=list(AGE=NEWAGE),int="c")
plot(BONE~AGE, data=DEER)
matlines(NEWAGE,NEWBONE.POLY,lty=c(1,2,2),col="black")
# the fit here is ok, but obviously there's a problem: do we really believe that the jaw starts to get smaller at very advanced age?

# this once again highlights the need to make a realistic model -- just because the model fits doesn't mean it is biologically realistic

## We can also plot in ggplot2 with a little trickery, 
##  so here's an example
library(broom)
library(ggplot2)

augment(MOD.POLY.D) %>%
  cbind(., RAW_AGE = DEER$AGE) %>%  # We need to bind our original AGE values and call them something useful
  ggplot(., aes(x = RAW_AGE, y = BONE)) +
  geom_point() +
  geom_line(aes(x = RAW_AGE, y = .fitted)) +
  geom_ribbon(aes(ymin = .fitted - (1.96*.se.fit),
                  ymax = .fitted + (1.96*.se.fit)),
              alpha = 0.6) +
  theme_bw()


# let's use nonlinear least squares to fit a more complex model formula that won't allow the line to dip at high ages
# a function we might like to use is an asymptotic exponential of the form y = a - b * exp(-c*x), where x is the predictor, y is the response, and a,b, and c are parameters (analagous to the slope, m, and intercept, c, in y = m*x + c)

# one different thing about using nonlinear least squares is that we need to provide initial guesses of the parameter values. you could use earlier studies or eyeball the graph, but for today let's use R's inbuilt "self-starting" exponential function SSasymp() to find candidate starting values

MOD.ASYMP.SS<-nls(BONE~SSasymp(AGE,a,b,c),data=DEER)

# the SSasymp part of this call tries to find good starting values

# in the absence of a starter function (they don't exist for all possible model forms), you need to specify starting values yourself. For example:
MOD.ASYMP<-nls(BONE~a-b*exp(-c*AGE),data=DEER,start=list(a=120,b=110,c=0.064))

# to examine diagnostics for nls regressions we need to (install if necessary) and load nlstools
library(nlstools)
plot(nlsResiduals(MOD.ASYMP))
# this gives a slightly different list of diagnostic models, but it suits us fine
plot(nlsResiduals(MOD.ASYMP.SS))

summary(MOD.ASYMP.SS)
summary(MOD.ASYMP)

# note how different the estimates for parameter b are!
# also note that not all starting values lead to a solution -- you can experiment with different values and see if you can generate an error where there is no solution

# we should be suspicious about parameter b; it is non-significant or indistinguishable from a in the two different models.

# let's try simplifying and build a two parameter model

MOD.ASYMP.2<-nls(BONE~a*(1-exp(-c*AGE)),data=DEER,start=list(a=120,c=0.064))

plot(nlsResiduals(MOD.ASYMP.2))
summary(MOD.ASYMP.2)

# let's plot the two models to see how they differ

NEWBONE.ASYMP<-predict(MOD.ASYMP,newdata=list(AGE=NEWAGE))
plot(BONE~AGE, data=DEER)
lines(NEWAGE,NEWBONE.ASYMP,col="black")
# note: matlines doesn't work with nls() I will try to find a substitute before the practical, but no guarantees....



NEWBONE.ASYMP.2<-predict(MOD.ASYMP.2,newdata=list(AGE=NEWAGE))
lines(NEWAGE,NEWBONE.ASYMP.2,col="red")

# the fit is virtually identical, so we should prefer the simpler model

plot(BONE~AGE, data=DEER,xlab="Age",ylab="Jawbone length (mm)")
lines(NEWAGE,NEWBONE.ASYMP.2,col="black")

# note that fitting confidence intervals around lines is tricky for curves that feature 2 or more coefficients


# Supplementary exercise

# import the data & see columns & structure
CG<-read.csv("CairngormNL.csv")
names(CG)
str(CG)

# check data quality
hist(CG$ELEVATION)
hist(CG$PLANT.HT)
# response var:
hist(CG$TRANSMISSION)
# not pretty! but looks like it won't transform easily
hist(I(CG$TRANSMISSION)^2)
# nope. leave alone for now

# visualize relationships
plot(TRANSMISSION~ELEVATION, data=CG)
plot(TRANSMISSION~PLANT.HT, data=CG)
# or
CGSUB<-CG[,c(3:5)]
pairs(CGSUB)
# apparent nonlinearity between elevation and both other vars; plant ht and transmission maybe linear?

# start with linear model?
MOD.LIN<-lm(TRANSMISSION~ELEVATION+PLANT.HT,data=CG)

par(mfrow=c(2,3))
plot(MOD.LIN)
hist(MOD.LIN$residuals)
par(mfrow=c(1,1))

# one suspect record 29
# can keep or remove as you see fit
# i decided to bin it
CG<-CG[-29,]
CG
# again
MOD.LIN<-lm(TRANSMISSION~ELEVATION+PLANT.HT,data=CG)

# check for variance inflation
library(car)
vif(MOD.LIN)
# fine!

par(mfrow=c(2,3))
plot(MOD.LIN)
hist(MOD.LIN$residuals)
par(mfrow=c(1,1))

summary(MOD.LIN)

# OK but maybe nonlinearity might help

MOD.POLYELEV<-lm(TRANSMISSION~poly(ELEVATION,2)*PLANT.HT,data=CG)
par(mfrow=c(2,3))
plot(MOD.POLYELEV)
hist(MOD.POLYELEV$residuals)
par(mfrow=c(1,1))
# not obvious that this has helped yet, but have a look

summary(MOD.POLYELEV)
# all three terms look significant, so there is nonlinearity
# one could also fit a polynomial of plant ht, but there's no compelling evidence from the plot that it's warranted

MOD.POLYELEV2<-update(MOD.POLYELEV,~.-poly(ELEVATION,2):PLANT.HT)
anova(MOD.POLYELEV,MOD.POLYELEV2)
# it's technically possible to take away just the interaction with the quadratic term (although it would require a bit of coding finesse), but I think this is philosophically strange, so I'm happy with removing both at once
summary(MOD.POLYELEV2)

# it's a bit redundant for terms with only a single coefficient, but let's make sure we have a min adequate model anyway
MOD.POLYELEV3<-update(MOD.POLYELEV2,~.-PLANT.HT)
anova(MOD.POLYELEV3,MOD.POLYELEV2)

MOD.POLYELEV4<-lm(TRANSMISSION~ELEVATION+PLANT.HT,data=CG)
# ok, so it doesn't take THAT much finesse
anova(MOD.POLYELEV4,MOD.POLYELEV2)

# remember you can't remove the first order of a polynomial if the second order is significant, so we stop here. MOD.POLYELEV2 is min adequate model
# check diagnostics
par(mfrow=c(2,3))
plot(MOD.POLYELEV2)
hist(MOD.POLYELEV2$residuals)
par(mfrow=c(1,1))
# good enough

# have a look again
summary(MOD.POLYELEV2)

# pub quality plots
# a couple of options
# could do avPlots(), but will only produce linear version of polynomial effect
avPlots(MOD.POLYELEV2)
# this is fine for the PLANT.HT term
avPlots(MOD.POLYELEV2,terms="PLANT.HT")
# but it's a bit trickier for the ELEVATION because it's not philosophically clear which parts of the polynomial we want to isolate from which others, and furthermore, the orthogonal nature of the quadratic term makes it hard to interpret anyway.
# since the patterns in the raw data are the same as those from the partial effects, I think it's fine to plot the raw data. these plots will not exactly illustrate the fitted coefficients, but they may illustrate the patterns well enough. if you use this approach, you should probably note (e.g., in the legend) for readers that the graphical representation is to visualize effects (rather than to accurately represent coefficients). 

# for PLANT.HT
plot(TRANSMISSION~PLANT.HT,data=CG,xlab="Plant height (cm)",ylab="Transmission")
# You could enter the actual coefficients, but because you're not plotting partial effects this may look like a poorly fitting line
abline(0.90845,-0.01274)

# alternatively, just plot the univariate best fit to illustrate the pattern
plot(TRANSMISSION~PLANT.HT,data=CG,xlab="Plant height (cm)",ylab="Transmission")
abline(lm(TRANSMISSION~PLANT.HT,data=CG))
# I wouldn't use matlines here because that would give a misleading impression of confidence -- the real confidence in the slope depends on the coefficient for ELEVATION as well


# get coefficients from "univariate" polynomial regression
UNIVAR<-lm(TRANSMISSION~poly(ELEVATION,2),data=CG)
NEWELEV<-seq(650,1113,2)
NEWTRANS<-predict(UNIVAR,newdata=list(ELEVATION=NEWELEV),int="c")
plot(TRANSMISSION~ELEVATION,data=CG,xlab="Altitude (m)",ylab="Transmission")
matlines(NEWELEV,NEWTRANS,lty=c(1,3,3),col="black")

# using {ggplot2}
library(ggplot2)
ggplot(CG, aes(x = ELEVATION, y = TRANSMISSION)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),se=FALSE)
# the last argument in the last line suppresses the shaded confidence interval, which is otherwise a bit misleading

# My minimal adequate model includes a significant negative effect of plant height on the index of productivity (plant height partial coefficient = -0.013 +/- 0.003; t = -3.737; P < 0.001; see figure 1) as well as a curvilinear effect of altitude that was strongly positive at low elevations but before reaching a plateau at around 900 m (linear elevation coefficient = 1.191 +/- 0.215; t = 5.543; P < 0.001; quadratic elevation coefficient = -0.606 +/- 0.221; t = -2.748; P = 0.008; see figure 2). The interaction between plant height and elevation did not significantly improve fit and was consequently removed from the final model (F(2,50) = 0.216; P = 0.807). 





