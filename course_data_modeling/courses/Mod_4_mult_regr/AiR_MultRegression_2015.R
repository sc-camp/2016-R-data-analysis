# Luc Bussière
# Stats Practical Exercise on Multiple regression
# Advancing in R
# Oct 12, 2012

# last modified Nov 29, 2015

# This is a practical exercise to conduct multiple linear regression 

# clear R of all objects
rm(list=ls())


# import the data & see columns & structure
# dataset from WOLFPUPS
WP<-read.csv("WOLFPUPS.csv")
head(WP)
# examine structure
str(WP)
# no recoding necessary


hist(WP$ADULTS)
hist(WP$CALVES)
hist(WP$WORMS)
hist(WP$PERMITS)
hist(WP$PUPS)

# note outlier in CALVES -- find it
WP[WP$CALVES>250,]
# YEAR for this entry is suspicious -- delete entire row
WP<-WP[-45,]

str(WP)
# no recoding necessary

# re-examine dists
hist(WP$ADULTS)
hist(WP$CALVES)
hist(WP$WORMS)
hist(WP$PERMITS)
hist(WP$PUPS)

# response var PUPS looks OK, but see if transform helps?

par(mfrow=c(2,2))
hist(WP$PUPS)
hist(sqrt(WP$PUPS))
hist(log(WP$PUPS))
hist(log10(WP$PUPS))
par(mfrow=c(1,1))

# none obviously better, leave as is for now and check later


# examine possible multicollinearity as well as pairwise relationships
pairs(WP)

# a few things to note
# very strong relationship between ADULTS and CALVES, suggests possible multicollinearity 
# intriguing relationships between WORMS and PUPS (negative), and ADULTS/CALVES and PUPS (positive) 
# use plot to get a sense of the predicted coefficients
plot(WP$ADULTS,WP$PUPS)
# possible violation of homogeneity of variance?
# guess slope of about 0.02 (rise/run = 10/500 = 0.02)

plot(WP$CALVES,WP$PUPS)
# possible violation of homogeneity of variance?
# guess slope of about 0.1 (rise/run = 10/100 = 0.1)

plot(WP$WORMS,WP$PUPS)
# guess slope of about - (rise/run = -14/1 = -14)

plot(WP$PERMITS,WP$PUPS)
# no pattern, so guess slope of 0



# build a maximal model but without interactions (we'll wait until after the midterm break for those)
MOD.1<-lm(PUPS~ADULTS+CALVES+WORMS+PERMITS,data=WP)

# examine diagnostics
par(mfrow=c(2,3))
plot(MOD.1)
hist(MOD.1$residuals)
par(mfrow=c(1,1))

# one point with high influence? check later
WP[19,]

# diagnostics look ok could try with transforms if you want for comparison

MOD.1sqrt<-lm(sqrt(PUPS)~ADULTS+CALVES+WORMS+PERMITS,data=WP)

par(mfrow=c(2,3))
plot(MOD.1sqrt)
hist(MOD.1sqrt$residuals)
par(mfrow=c(1,1))

# MOD.1 is certainly no worse

# so stick with raw data

# check for variance inflation using the vif() function from the {car} package
# {car} stands for "companion to applied regression" and has many very useful functions -- we'll use two today
# if the following command doesn't work, it's because you haven't installed the {car} package. refer to notes from the graphics practical if you have forgotten how to do this 
library(car)
vif(MOD.1)

# need to lose one of either ADULTS or CALVES
# the update command is very useful here again -- the tilde and fullstop minus CALVES mean: model is "as-is" except  without CALVES
MOD.2<-update(MOD.1,~. - CALVES)

# reexamine vifs
vif(MOD.2)

# now the vifs are all low, so model is OK
# let's see what mistake we might have made by comparing the parameters from the two models

# examine "incorrect" param ests
summary(MOD.1)

# compare to:
summary(MOD.2)
# so ADULTS param has wrong sign in MOD.1!

# check if removing ADULTS instead of CALVES matters?
MOD.2a<-update(MOD.1,~. - ADULTS)

vif(MOD.2a)
summary(MOD.2a)
# so estimate for CALVES from MOD.1 has correct sign, but param way off

# we can't really say which of the two models is better, but R-squared slightly higher with MOD.2a, so maybe take it
# remember that when interpreting CALVES coefficient below, effect could be caused by ADULTS or CALVES -- no way to tell with these data

# Model simplification
# model simplification should proceed by iteratively removing NS terms, and using F-tests or likelihood ratio tests to check that the RSS or deviance has not significantly increased

MOD.3<-update(MOD.2a, ~. - PERMITS)
anova(MOD.2a,MOD.3)
# in this case, the RSS went up only slightly
# so simpler model is preferred
summary(MOD.3)
# because all terms significant, no more model simplification is warranted
# NB: when we build more complex models with several factor levels, we'll need to check terms even if some of the parameters are significant

# check diagnostics again
par(mfrow=c(2,3))
plot(MOD.3)
hist(MOD.3$residuals)
par(mfrow=c(1,1))
# good enough, and no longer a high influence point!

# examine minimal adequate model again
summary(MOD.3)

# compare actual coefficients to predicted ones
# note that each coefficient has less than half as much effect as predicted based on simple univariate regression, because the two predictors left in the model are correlated (remember pairs() plot?)
# so mult regression gives a better estimate of real partial effect



# which of the two significant factors in MOD.3 is more important, CALVES or WORMS?
# to figure this out, must mean-centre and variance standardize predictors using scale() command
# rebuild the model with scaled predictors
MOD.3s<-lm(PUPS~scale(CALVES)+scale(WORMS),data=WP)

summary(MOD.3s)
# note that model R-squared is identical to model 3, but coefficients now scaled to express predicted change in PUPS for every standard deviation of the predictor
# this reveals that the two predictors are roughly equivalent in causing a change, although WORMS has perhaps a slightly stronger effect: 1.26 (+/- 0.44) more PUPS predicted for every SD of CALVES increased, or 1.63 (+/- 0.44) fewer PUPS for every SD of WORMS decreased

# let's plot the effects


# you could generate a confidence interval using matlines and predict if you wanted



# one could use low level plotting functions to add lines to a plot based on coefficients, but these will sometimes look odd because the partial effect is sometimes not well supported by the raw data 

# so for example
# using {graphics}

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

# plot the original data
plot(WP$WORMS,WP$PUPS,xlab="Infection score",ylab="Pups born",ylim=c(0,18))
matlines(NEWWORMS,NEWYUSINGWORMS,lty=c(1,2,2),col="black")
# NB this looks like a somewhat "shallow" fit, but this is because some of the trend is caused by the correlated variable
# The "advantage" of this approach is that the data are readily interpretable in raw units

# like vif(), the avPlots function is from the {car} package, so remember to load it if necessary
# generate avPlots to examine partial effects
avPlots(MOD.3)

# note that the pattern for scaled predictors is the same, but the x-axis units are different

# make publication-quality plots
avPlots(MOD.3,terms="WORMS",xlab="Residual infection score",ylab="Residual pups born",xlim=c(-0.6,0.6),ylim=c(-5,5))

avPlots(MOD.3,terms="CALVES",xlab="Residual calves born",ylab="Residual pups born",xlim=c(-60,60),ylim=c(-4,6))

# to do this by hand, would need resids from two different models
RESWORMSONOTHERPREDICTORS<-lm(WORMS~CALVES,data=WP)
RESPUPSNOWORMS<-lm(PUPS~CALVES,data=WP)
plot(RESWORMSONOTHERPREDICTORS$residuals,RESPUPSNOWORMS$residuals,xlab="Residual infection score",ylab="Residual pups born",xlim=c(-0.6,0.6),ylim=c(-5,5))
abline(lm(RESPUPSNOWORMS$residuals~RESWORMSONOTHERPREDICTORS$residuals))

# this plot should look just like the one generated with avPlots
# you could even add confidence intervals by using a predict command on the linear model nested within line 219 


# INTERPRETATION

# remember that because of strong correlation between CALVES and ADULTS, we can't say that the effect of CALVES in this model is actually due to CALVES or ADULTS. In the discussion, we would need to attend to the implications of both possibilities, even though we're only reporting a parameter estimate for one of them.

# Sample Results text:

# My minimal adequate model of wolf recruitment rates included a significant negative effect for infection status (Slope B = -6.77; Standardized regression coefficient Beta = -1.63; t = -3.68,P= 0.0007; see Figure 1A) and a positive effect for the number of moose calves (B = 0.044, Beta = 1.26; t = 2.83; P = 0.0071; see Figure 1B). Recall that moose calf numbers were collinear with adult population size, so I cannot distinguish between the effects of adults or calves on wolf recruitment.


# SuppEX


# clear R of all objects
rm(list=ls())

# read data in frame and examine it
CG<-read.csv("Cairngorm2012pt.csv")
str(CG)

# explore distributions of vars
# response
hist(CG$TRANSMISSION)
# def not normal, and doesn't look transformable
# explore possible transforms
par(mfrow=c(2,2))
hist(sqrt(CG$TRANSMISSION))
hist(log(CG$TRANSMISSION))
hist(CG$TRANSMISSION*CG$TRANSMISSION)
par(mfrow=c(1,1))

# leave alone for now
# examine predictors
hist(CG$ELEVATION)
hist(CG$SOIL.TEMP.CORR)
hist(CG$AVE.MOISTURE)
hist(CG$RICHNESS)
hist(CG$BARE.GRND)

# no obvious outliers/mistakes

# prepare panel plot

pairs(CG)
# some corrs between vars, but nothing super-strong between the vars of interest
# based on corrs, expect positive effect of ELEVATION and maybe (but probably not) BARE.GRND, and negative effects of SOIL.TEMP.CORR and AVE.MOISTURE?


# build model with all predictors
MOD.1<-lm(TRANSMISSION~ELEVATION+SOIL.TEMP.CORR+AVE.MOISTURE+RICHNESS+BARE.GRND,data=CG)

library(car)
vif(MOD.1)
# note marginally high number between ELEVATION and SOIL.TEMP. Can do a few things here, but since this is at the check stage, maybe it's best to postpone action: it may be that model simplification deals with this for me.

# check diagnostics
par(mfrow=c(2,3))
plot(MOD.1)
hist(MOD.1$residuals)
par(mfrow=c(1,1))
# resids vs fitted not great -- don't obsess yet'


summary(MOD.1)


# simplify model starting with least sig term
MOD.2<-update(MOD.1,~. - BARE.GRND)
vif(MOD.2)

# F test
anova(MOD.1,MOD.2)
# simpler model is better
summary(MOD.2)

# continue simplifying
MOD.3<-update(MOD.2,~. - SOIL.TEMP.CORR)
anova(MOD.3,MOD.2)
summary(MOD.3)
vif(MOD.3)
# now because simplification removed a collinear term, all is OK

MOD.4<-update(MOD.3,~. - RICHNESS)
anova(MOD.3,MOD.4)
summary(MOD.4)

MOD.5<-update(MOD.4,~. - AVE.MOISTURE)
anova(MOD.5,MOD.4)
# can't remove moisture

MOD.6<-update(MOD.4,~. - ELEVATION)
anova(MOD.6,MOD.4)
# can't remove altitude (note I have to compare mods 4 and 6, because 5 was no good)
# so min model is MOD.4

# examine diagnostics again
par(mfrow=c(2,3))
plot(MOD.4)
hist(MOD.4$residuals)
par(mfrow=c(1,1))
# still some concern about resids and fitted plot (due to nonlinearity) -- we'll deal with that in future weeks when we start to think of nonlinear fits
# this is what you will often find when you deal with your own data -- they don't play as nice as the datasets we select when teaching you

# meanwhile proceed as is
summary(MOD.4)

# to evaluate the relative importance of the predictors, use the scale() command in a new model


MOD.4std<-lm(TRANSMISSION~scale(ELEVATION)+scale(AVE.MOISTURE),data=CG)

summary(MOD.4std)
# According to this summary, ELEVATION has a slightly more important effect than moisture on transmission, but the absolute values of confidence intervals for the two coefficients overlap. Can use the confint() command to be sure
confint(MOD.4std)



# should make nice plots using either avPlots or graphics package
# here use avPlots

par(mfrow=c(1,2))
avPlots(MOD.4,terms="ELEVATION",xlab="Residual altitude",ylab="Residual productivity")

avPlots(MOD.4,terms="AVE.MOISTURE",xlab="Residual soil moisture",ylab="Residual productivity")
par(mfrow=c(1,1))


# Sample text:

# My minimal adequate model of factors predicting productivity included a positive partial effect of altitude (Slope +/- SE = 0.00101 +/- 0.00038; t = 2.69; P = 0.0096) and a negative partial effect of soil moisture (Slope +/- SE = -0.00425 +/- 0.00185; t = -2.30; P = 0.0257; see Figure 2). None of the other predictors (soil temperature, bare ground area and species richness) contributed significantly to the model (all P > 0.48), and consequently these terms were removed during the model simplification procedure.

# Another option is to cite the F ratios for the model simp when giving stats for ELEVATION and SOIL.MOISTURE
# It's also possible to cite standardized coefficients instead of raw ones, in which case you must explain what you're doing and the relative importance of the terms.


