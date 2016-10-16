# Luc Bussière
# Advancing in R
# Generalized linear models
# Oct 1, 2011

# last modified Nov 29, 2015

# clear R of all objects
rm(list=ls())

#load data


# data concern counts of species in plots as a function of BIOMASS and soil pH
# inspect data
SPECIES<-read.csv("species.csv")


names(SPECIES)
str(SPECIES)
head(SPECIES)
summary(SPECIES)

# examine dists and quality control
par(mfrow=c(1,2))
hist(SPECIES$BIOMASS)
hist(SPECIES$RICHNESS)
par(mfrow=c(1,1))

# check for collinearity
pairs(SPECIES)

# BIOMASS is a bit ugly, but remember it's a covariate, so it doesn't matter
# Species looks suspiciously skewed, but that's expected because we know it consists of counts. Note that it can't go lower than zero
levels(SPECIES$PH)
summary(SPECIES)
# nice that the pH is well balanced
# at this stage you might notice that the treatments are in the wrong order by default. To improve presentation later on, you may want to change this

SPECIES$PH = factor(SPECIES$PH,levels(SPECIES$PH)[c(1,3,2)])

# plot data, e.g., using ggplot2
library(ggplot2)
ggplot(SPECIES, aes(x = BIOMASS, y = RICHNESS,colour=PH)) + 
  geom_point()+
  stat_smooth(method = "lm")

# the lines are useful for illustrating trends, but also misleading -- we shouldn't predict negative counts for high biomass!

# build a model, try to simplify it, and examine diagnostics
MOD.1<-glm(RICHNESS~BIOMASS*PH,data=SPECIES,family=poisson)

# load boot package to get good diagnostics for glm
library(boot)
glm.diag.plots(MOD.1)
# can interact with the plots to identify points (for example those with high influence or leverage) by specifying iden=TRUE inside call for glm.diag.plots
# in this case, record 18 may have high influence
# otherwise this looks pretty good

summary(MOD.1)

MOD.2<-glm(RICHNESS~BIOMASS+PH,data=SPECIES,family=poisson)
glm.diag.plots(MOD.2)

# when doing model simp with glm, must specify that you want a chi-squared rather than an F-test or you won't get a p-value
anova(MOD.1,MOD.2,test="Chi")

# so must retain interaction

# examine model
summary(MOD.1)

# could check whether PHmid and high are really different by lumping them and doing a Chi-squared likelihood ratio test

NEWPH<-factor(SPECIES$PH)
# generates a new object to manipulate, so that we don't accidentally mess up the original data
# have a look at it
levels(NEWPH)

# now recode the new factor so that you combine similar treatment levels
levels(NEWPH)[c(1,2)]<-"mid&high"
levels(NEWPH)[c(3)]<-"low"
levels(NEWPH)

MOD.1.2LEVS<-glm(RICHNESS~BIOMASS*NEWPH,data=SPECIES,family=poisson)
anova(MOD.1,MOD.1.2LEVS,test="Chi")

# so must retain 3 levels

# generate publication quality plot
# what follows is an alternative to using the subset command in the usual way, but either way works well
plot(SPECIES$BIOMASS,SPECIES$RICHNESS,type="n",xlab="Biomass",ylab="Number of species")
# the type="n" in the line above suppresses the plotting of any points

spp<-split(SPECIES$RICHNESS,SPECIES$PH)
bio<-split(SPECIES$BIOMASS,SPECIES$PH)
points(bio[[1]],spp[[1]],pch=16)
points(bio[[2]],spp[[2]],pch=17)
points(bio[[3]],spp[[3]])

# make legend 
legend("topright",
	   title="pH",
	   legend=c("high","mid","low"),
	   pch=c(16,17,1),lty=c(1,2,4),bty="n")

# add lines for each treatment
XV<-seq(0,10,length=100)
levels(SPECIES$PH)

# the next line creates a newdata vector that simply repeats the same level of pH over and over again, and is designed to be the same length as XV
PHVH<-rep("high",100)
YVH<-predict(MOD.1,list(PH=factor(PHVH),BIOMASS=XV),type="response",se=TRUE)
# the type="response" argument backtransforms from the linear predictor; it's equivalent to exponentiating the predicted values for log-linked regression
# examine to make sure this worked
str(YVH)
# add line to existing plot
lines(XV,YVH$fit,lty=1)
lines(XV,YVH$fit+YVH$se.fit,lty=3)
lines(XV,YVH$fit-YVH$se.fit,lty=3)


PHVM<-rep("mid",100)
YVM<-predict(MOD.1,list(PH=factor(PHVM),BIOMASS=XV),type="response",se=TRUE)
lines(XV,YVM$fit,lty=2)
lines(XV,YVM$fit+YVM$se.fit,lty=3)
lines(XV,YVM$fit-YVM$se.fit,lty=3)


PHVL<-rep("low",100)
YVL<-predict(MOD.1,list(PH=factor(PHVL),BIOMASS=XV),type="response",se=TRUE)
lines(XV,YVL$fit,lty=4)
lines(XV,YVL$fit+YVL$se.fit,lty=3)
lines(XV,YVL$fit-YVL$se.fit,lty=3)


# note that I have shown the dashed lines here as ±1SE rather than in CIs like normal. The ability of predict to generate these SE intervals around the lines is recent. 

# Since I prefer CIs, I want to generate these using brute force and backtransforming, which is also a useful learning exrcise. The code below makes a very similar plot, but shows CIs instead of SEs. It exploits our ability to backtransform predictions in loglinear space (i.e., the fitted values are in link units and you need to backtransform them yourself, adding on the SEs as appropriate).

# Note that the reason we can't do a similar operation on SEs in the graph just above: 1.96 X the backtransformed SE is not the same as backtransforming 1.96 X the SE.

plot(SPECIES$BIOMASS,SPECIES$RICHNESS,type="n",xlab="Biomass",ylab="Number of species")
points(bio[[1]],spp[[1]],pch=16)
points(bio[[2]],spp[[2]],pch=17)
points(bio[[3]],spp[[3]])
# make legend 
legend("topright",
       title="pH",
       legend=c("high","mid","low"),
       pch=c(16,17,1),lty=c(1,2,4),bty="n")

# add lines for each treatment
XV<-seq(0,10,length=100)
levels(SPECIES$PH)
PHVH<-rep("high",100)
YVH.SE<-predict(MOD.1,list(PH=factor(PHVH),BIOMASS=XV),type="link",se=TRUE)
# the type="link" argument gives me predictions in log transformed units, and the se argument also provides standard errors around the prediction

# now I need 3 separate lines to call the fits and CIs
head(YVH.SE)
lines(XV,exp(YVH.SE$fit),lty=1)
# the exp() command exponentiates the fitted value, returning it from log space to raw units

# for the confidence limits, I will first add and subtract 1.96 SE before backtransforming
lines(XV,exp(YVH.SE$fit-1.96*YVH.SE$se.fit),lty=3)
lines(XV,exp(YVH.SE$fit+1.96*YVH.SE$se.fit),lty=3)

PHVM<-rep("mid",100)
YVM.SE<-predict(MOD.1,list(PH=factor(PHVM),BIOMASS=XV),type="link",se=TRUE)
head(YVM.SE)
lines(XV,exp(YVM.SE$fit),lty=2)

lines(XV,exp(YVM.SE$fit-1.96*YVM.SE$se.fit),lty=3)
lines(XV,exp(YVM.SE$fit+1.96*YVM.SE$se.fit),lty=3)


PHVL<-rep("low",100)
YVL.SE<-predict(MOD.1,list(PH=factor(PHVL),BIOMASS=XV),type="link",se=TRUE)
lines(XV,exp(YVL.SE$fit),lty=4)

lines(XV,exp(YVL.SE$fit-1.96*YVL.SE$se.fit),lty=3)
lines(XV,exp(YVL.SE$fit+1.96*YVL.SE$se.fit),lty=3)

# you may argue that this is getting a touch messy -- your call


# # # # # # # # # # # # # # #

# clear R of all objects
rm(list=ls())

# second dataset: binary outcome of INCIDENCE of breeding birds as a function of ISLAND AREA and ISOLATION
ISLAND<-read.csv("isolation.csv")
names(ISLAND)

# inspect data
str(ISLAND)
head(ISLAND)

par(mfrow=c(2,2))
hist(ISLAND$INCIDENCE)
hist(ISLAND$AREA)
hist(ISLAND$ISOLATION)
par(mfrow=c(1,1))
# note that INCIDENCE is numeric even though it has only two possible values; that's good for binomial regression

# check for collinearity
pairs(ISLAND)

# visualize effects
par(mfrow=c(1,2))
plot(ISLAND$INCIDENCE~ISLAND$AREA)
plot(ISLAND$INCIDENCE~ISLAND$ISOLATION)
par(mfrow=c(1,1))


# I like to spread the points around a bit so I can see them all, even if this isn't accurate. I do this with the jitter() command that adds small random numbers to vectors
par(mfrow=c(1,2))
plot(jitter(ISLAND$INCIDENCE,factor=0.25)~ISLAND$AREA)
plot(jitter(ISLAND$INCIDENCE,factor=0.25)~ISLAND$ISOLATION)
par(mfrow=c(1,1))

# build a model
MOD.1<-glm(INCIDENCE~AREA*ISOLATION,data=ISLAND,family=binomial)

# examine diagnostics
glm.diag.plots(MOD.1)
# why are the data spread in such a funny arrangement in the first panel?
# there's a record with high influence I need to identify
# use the iden argument, and then interact with the console, clicking your mouse on the point after you have selected the appropriate panel (panel 3 in this case). Then hit escape, and you should see the record number appear on the plot

glm.diag.plots(MOD.1,iden=TRUE)

0
# 
# 
# 
# 
# it's record 10. Record 19 may also be problematic; let's wait until after simplification to investigate
summary(MOD.1)
# check for overdispersion -- is this ok?


# try to simplify
MOD.2<-glm(INCIDENCE~AREA+ISOLATION,data=ISLAND,binomial)

anova(MOD.1,MOD.2,test="Chi")
# retain the simpler model

glm.diag.plots(MOD.2,iden=TRUE)

0


summary(MOD.2)

# can you do any further simplification?
MOD.3<-update(MOD.2,~.-AREA)
anova(MOD.3,MOD.2,test="Chi")
MOD.4<-update(MOD.2,~.-ISOLATION)
anova(MOD.4,MOD.2,test="Chi")
# nope


# check if high influence points are a problem for param estimation

# first look at them
ISLAND[10,]
ISLAND[19,]
# check data to see if these are outside norm
summary(ISLAND)

# check coefficients with and without
SUBISLAND<-ISLAND[c(-10,-19),]

MOD.2SUB<-update(MOD.2,data=SUBISLAND)
summary(MOD.2)
summary(MOD.2SUB)

# the inclusion of these points doesn't affect null hypothesis tests. Whether you omit or retain them is therefore a matter of opinion. Since I have no reason to think they are mistakes, I will keep them, and argue in my methods that their high influence doesn't qualitatively affect my statistical interpretation



# make plots

# can make added variable plots, but as above, the plots will produce straight lines
library(car)
avPlots(MOD.2)

# there's no interaction, so we're OK to plot the curves separately


par(mfrow=c(1,2))
XVAREA<-seq(0,9,length=100)
MNISOL<-rep(mean(ISLAND$ISOLATION),100)
YVAREA<-predict(MOD.2,list(AREA=XVAREA,ISOLATION=MNISOL),type="link",se=TRUE)
plot(ISLAND$AREA,ISLAND$INCIDENCE,xlab=expression("Area ( km"^{2}~")"),ylab="Incidence probability")
# the library faraway has a handy inverse logit function
library(faraway)
lines(XVAREA,ilogit(YVAREA$fit),lwd=2)
lines(XVAREA,ilogit(YVAREA$fit+1.96*YVAREA$se.fit),lty=3)
lines(XVAREA,ilogit(YVAREA$fit-1.96*YVAREA$se.fit),lty=3)


XVISOL<-seq(0,10,length=100)
MNAREA<-rep(mean(ISLAND$AREA),100)
YVISOL<-predict(MOD.2,list(ISOLATION=XVISOL,AREA=MNAREA),type="link",se=TRUE)

# the weird code in the line below allows superscript text for km^2
plot(ISLAND$ISOLATION,ISLAND$INCIDENCE,xlab="Isolation from mainland ( km )",ylab="Incidence probability")
lines(XVISOL,ilogit(YVISOL$fit),lwd=2)
lines(XVISOL,ilogit(YVISOL$fit+1.96*YVISOL$se.fit),lty=3)
lines(XVISOL,ilogit(YVISOL$fit-1.96*YVISOL$se.fit),lty=3)

par(mfrow=c(1,1))


# SuppEx

# clear R of all objects
rm(list=ls())

#load data
# does population DENSITY in wasps determine the sex ratio (p MALES)

SEXRATIO<-read.csv("sexratio.csv")

# inspect data and check dist of vars
head(SEXRATIO)
str(SEXRATIO)

hist(SEXRATIO$DENSITY)
hist(SEXRATIO$FEMALES)
hist(SEXRATIO$MALES)


# plot the data -- note that we'll generate proportions from two columns for the purpose of plotting
# but that we'll retain the individual columns later in the analysis

# examine relationships in raw space as well as loglinear space
par(mfrow=c(1,2))
PROPMALE<-SEXRATIO$MALES/(SEXRATIO$MALES+SEXRATIO$FEMALES)
plot(SEXRATIO$DENSITY,PROPMALE,ylab="Proportion male")
plot(log(SEXRATIO$DENSITY),PROPMALE,ylab="Proportion male") 
par(mfrow=c(1,1))

# build a model

# the first step is strange for data entered as they are here -- you need to generate a so-called
# "two vector response" which has both counts bound together
# we use the cbind command to bind columns together
TWOVECTRESP<-cbind(SEXRATIO$MALES,SEXRATIO$FEMALES)
MOD.1<-glm(TWOVECTRESP~DENSITY,data=SEXRATIO,binomial)

# you can also nest cbind within the model call, which is  preferred because it means all objects are referred to in the data call
MOD.1<-glm(cbind(SEXRATIO$MALES,SEXRATIO$FEMALES)~DENSITY,data=SEXRATIO,binomial)


library(boot)
glm.diag.plots(MOD.1)
# ugh! terrible
# for fun, let's examine the model
summary(MOD.1)
# density is sig, but there is overdispersion

# could fit a model with quasibinomial and see what happens, but this is not going to fix diagnostics
MOD.1qbin<-glm(cbind(SEXRATIO$MALES,SEXRATIO$FEMALES)~DENSITY,data=SEXRATIO,family=quasibinomial)
glm.diag.plots(MOD.1qbin)
# see? no difference. quasi models don't fix fit, only approach to hypothesis testing
# verify this by examining the model
summary(MOD.1qbin)
# density still sig, but pval much higher. Quasi models have lower power (are more conservative) models
# note the change in the assumption about the dispersion parameter in the summary above


# because diagnostics were so terrible, let's try with log transformed DENSITY
MOD.2<-glm(cbind(SEXRATIO$MALES,SEXRATIO$FEMALES)~log(DENSITY),data=SEXRATIO,binomial)

glm.diag.plots(MOD.2)
# much better 

# but maybe a high leverage observation?
glm.diag.plots(MOD.2,iden=TRUE)





par(mfrow=c(1,1))
# record 8 is a high leverage observation but this is not surprising because it is the highest value in the plot, and the record with the largest sample size. if you were to remove it, you would find that record 7 became high leverage, and so on. let's trust the data entry team and proceed as is.


summary(MOD.2)
# note the difference in residual deviance! and now the overdispersion is fixed. marvellous!



# create nice plot to illustrate pattern -- note we need to use the proportion even though this wasn't the thing modelled

# first generate newdata to use with predict() command
summary(SEXRATIO)
NEWXVAR<-seq(1,444,2)

# In the next line, I use the argument se.fit=TRUE to get standard errors around fit
NEWYVAR<-predict(MOD.2,list(DENSITY=NEWXVAR),se.fit=TRUE)

# plot raw data -- remember to log transform x variable
plot(log(SEXRATIO$DENSITY),PROPMALE,ylab="Proportion male",xlab="Ln(Density)")

# to draw lines as well as CIs, can exploit a handy invers logit function in the package {faraway}
# note you may need to install {faraway} before loading it
library(faraway)


# add fit, and remember to log transform x-coordinate and backtransform fit
lines(log(NEWXVAR),ilogit(NEWYVAR$fit))
# add CI's
# upperCI
lines(log(NEWXVAR),ilogit(NEWYVAR$fit+1.96*NEWYVAR$se.fit),lty=2)
# lowerCI
lines(log(NEWXVAR),ilogit(NEWYVAR$fit-1.96*NEWYVAR$se.fit),lty=2)


# alternatively, I prefer to plot the x axis in raw units but still on a log scale, as follows:

# plot raw data -- the log="x" argument puts the x-axis on a log scale even though the data are in raw units
plot(SEXRATIO$DENSITY,PROPMALE,ylab="Proportion male",xlab="Density",log="x")

# add fit, and remember to log transform x-coordinate and backtransform fit
lines(NEWXVAR,ilogit(NEWYVAR$fit))
# add CI's
lines(NEWXVAR,ilogit(NEWYVAR$fit+1.96*NEWYVAR$se.fit),lty=2)
lines(NEWXVAR,ilogit(NEWYVAR$fit-1.96*NEWYVAR$se.fit),lty=2)






