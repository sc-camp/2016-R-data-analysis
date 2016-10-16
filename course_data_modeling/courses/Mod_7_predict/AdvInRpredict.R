# Luc Bussière
# Stats Practical Exercise on predict()
# April 20, 2015

# last modified Nov 29, 2015

# This is a practical exercise for the AdvInR course
# featuring tips and tricks for making predictions from 
# linear model coefficients

# clear R of all objects
rm(list=ls())

# concept quiz: how well can you interpret coefficients?

# See the last page of the practical handout for a candidate answer to the challenge

# Using broom dplyr, and ggplot to get fitted lines
############################

library(broom)
library(dplyr)
library(ggplot2)

##
# First use the data for simple linear regression
##
# import tannin data from Module 3 (linear models)
TANDAT <- read.csv("tannin.csv")

tanfit <- lm(GROWTH ~ TANNIN,
             data = TANDAT)

summary(tanfit)

## Get a 'tidy' version of the model coefficients
tidy(tanfit)

## We can send the tidied model coefficients to a CSV file, so that the coefficients are easier to import into tables in MS Word or Excel
write.csv(tidy(tanfit), "TanninFits.csv", row.names = FALSE)

## Get a 'tidy' version of the model summary statistics
glance(tanfit)

## 'Augment' adds columns to the original data set
augment(tanfit)

## This includes predicted values etc, meaning we can 
#   work up a fancy figure very easily by chaining functions...
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

## You can even chain the whole thing, including linear model (although of course you should not be visualizing models before examining diagnostics)
TANDAT %>%
  lm(GROWTH ~ TANNIN, data = .) %>%
  augment() %>%
  ggplot(data = .,
         aes(x = TANNIN,
             y = GROWTH)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = .fitted - (2 * .se.fit),
                  ymax = .fitted + (2 * .se.fit)),
              alpha = 0.3) +
  theme_bw()


# alternatively, can use the package visreg
library(visreg)
# basic plot
visreg(tanfit)                          
# with points options
visreg(tanfit, points.par = list(cex = 1.2, col = "red"))
# with labels adjusted
visreg(tanfit,points.par = list(cex = 1.2, col = "red"),
       ylab="Caterpillar mass gain (g)",
       xlab="Dietary tannin dose (g/kg)") 

       

###### Broom with ANCOVA ######

COMP <- read.csv("compensationo.csv")

comp_mod <- lm(FRUIT ~ ROOT + GRAZING,
               data = COMP)

summary(comp_mod)

##
# How do the tidy versions change when there are multiple variables
##


## Get a 'tidy' version of the model coefficients
tidy(comp_mod)

## Get a 'tidy' version of the model summary statistics
glance(comp_mod)

## 'Augment' adds columns to the original data set
augment(comp_mod)

##
# You will notice that we get predicted values (.fitted) for 
#  FRUIT against ROOT for each level of GRAZING.
#
# We can use these to plot our predictions as before...
##
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


# Using visreg
# When you plot just one of the variables, visreg adjusts the data points for the other factors, conditioning on the most common factor level (for categorical factors) or the median value (for numeric variables). If you plot more than one variable, you can choose to overlay the fits for the different levels of the factor, or you can plot in separate panels.

# basic plots, will need to interact with console to see both
visreg(comp_mod)


# stripchart for GRAZING, holding ROOT constant (at mean)
visreg(comp_mod, xvar = "GRAZING", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))

# can change the conditioning values;
# notice how when we change the ROOT diameter we change the location of the expected means for each treatment
visreg(comp_mod, xvar = "GRAZING", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"),
       cond=list(ROOT=10))

# when using visreg to plot continuous variable effects in models with balanced designs, it's useful to check which treatment visreg by changing the condition statement

# First, plot only the effect of ROOT, but it's not clear which treatment for GRAZING is being used
visreg(comp_mod, xvar = "ROOT",
       points.par = list(cex = 1.1, col = "red"))

# Now systematically change conditioning argument
visreg(comp_mod, xvar = "ROOT",
       points.par = list(cex = 1.1, col = "red"),
       cond=list(GRAZING="Ungrazed"))
visreg(comp_mod, xvar = "ROOT",
       points.par = list(cex = 1.1, col = "red"),
       cond=list(GRAZING="Grazed"))
# The second option matches the default

# using panels (I think this is not so clear)
visreg(comp_mod, xvar = "ROOT", by = "GRAZING", 
       points.par = list(cex = 1.1, col = "red"))

# overlay
visreg(comp_mod, xvar = "ROOT", by = "GRAZING",
       overlay = TRUE, band = TRUE, points.par = list(cex = 1.1))



# now supp exercises from ANCOVA
# import data from suppex for ANCOVA on cattle weight gain
WEIGHTS<-read.csv("growth.csv")
head(WEIGHTS)

# relevel supplement so that control is reference
WEIGHTS$SUPPLEMENT<-relevel(WEIGHTS$SUPPLEMENT,ref="control")

# build a model
MOD.WEIGHTS<-lm(GAIN~DIET*SUPPLEMENT,data=WEIGHTS)

# stripcharts for ANOVA
# basic plots, will need to interact with console to see both
visreg(MOD.WEIGHTS)       

# only one variable visualized
visreg(MOD.WEIGHTS, xvar = "DIET", whitespace = 0.4, 
       points.par = list(cex = 1.1, col = "red"))

# three panels for diet
visreg(MOD.WEIGHTS, xvar = "SUPPLEMENT", by = "DIET", 
       whitespace = 0.4, points.par = list(cex = 1.1, col = "red"))

# overlay (but no 95%CI)
visreg(MOD.WEIGHTS, xvar = "SUPPLEMENT", by = "DIET",
       whitespace = 0.5, overlay = TRUE, band = FALSE, 
       points.par = list(cex = 1.1))


# generalized models?

ISLAND<-read.csv("isolation.csv")
names(ISLAND)
MOD.2<-glm(INCIDENCE~AREA+ISOLATION,data=ISLAND,binomial)

# visreg will visualize fit on the transformed scale
visreg(MOD.2, xvar = "ISOLATION")
visreg(MOD.2, xvar = "AREA")

# or on the original scale; note "rug" which shows the position of data
visreg(MOD.2, xvar = "ISOLATION",scale="response",rug=2,ylab="Incidence probability",xlab="Isolation from mainland (km)")
visreg(MOD.2, xvar = "AREA",scale="response",rug=2,ylab="Incidence probability",xlab="Island area (km^2)")


# OK, now plotting interactions between continuous variables
# import data on attractiveness of male dance fly silhouettes

DF <- read.csv("FLYSEX.csv")
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

library(boot)
glm.diag.plots(ORN.MOD)
# model fit obviously not great; homogeneity of variance not met
# let's ignore for the purpose of this exercise'

ORN.MOD.Q<-glm(ATTR~LEG*ABDOMEN,data=DF,family=quasipoisson)
summary(ORN.MOD.Q)

# simplify model?
ORN.MOD.Q2<-update(ORN.MOD.Q, ~. -LEG:ABDOMEN)
anova(ORN.MOD.Q,ORN.MOD.Q2,test="Chi")

# must retain interaction

# Using visreg, can plot interactions by specifying "breaks", which are levels of other predictor to use in plot

visreg(ORN.MOD.Q,xvar="LEG",by="ABDOMEN",breaks=c(-2,0,2),scale="response",xlab="Std. leg area",ylab="Male approaches")
visreg(ORN.MOD.Q,xvar="ABDOMEN",by="LEG",breaks=c(-2,0,2),scale="response",xlab="Std. abdomen area",ylab="Male approaches")

# with overlay
visreg(ORN.MOD.Q,xvar="LEG",by="ABDOMEN",breaks=c(-2,0,2),scale="response",overlay=TRUE,xlab="Std. leg area",ylab="Male approaches")
visreg(ORN.MOD.Q,xvar="ABDOMEN",by="LEG",breaks=c(-2,0,2),scale="response",overlay=TRUE,xlab="Std. abdomen area",ylab="Male approaches")

# interaction still clearer in loglinear space
visreg(ORN.MOD.Q,xvar="LEG",by="ABDOMEN",breaks=c(-2,0,2),overlay=TRUE)
visreg(ORN.MOD.Q,xvar="ABDOMEN",by="LEG",breaks=c(-2,0,2),overlay=TRUE)

# Or can go "old-school" and use predict if you want (?????)

NEWABD<-seq(-3,3,length=100)
LEGmin2<-rep(-2,100)
LEG2<-rep(2,100)

NEWLEG<-seq(-3,3,length=100)
ABDmin2<-rep(-2,100)
ABD2<-rep(2,100)


# predicted values on log-transformed scale
PREDSBYLEGABDmin2<-predict(ORN.MOD.Q,newdata=list(LEG=NEWLEG,ABDOMEN=ABDmin2),se.fit=TRUE)
str(PREDSBYLEGABDmin2)
PREDSBYLEGABD2<-predict(ORN.MOD.Q,newdata=list(LEG=NEWLEG,ABDOMEN=ABD2),se.fit=TRUE)

PREDSBYABDLEGmin2<-predict(ORN.MOD.Q,newdata=list(LEG=LEGmin2,ABDOMEN=NEWABD),se.fit=TRUE)
str(PREDSBYABDLEGmin2)
PREDSBYABDLEG2<-predict(ORN.MOD.Q,newdata=list(LEG=LEG2,ABDOMEN=NEWABD),se.fit=TRUE)

# now plot effect of legs
# plot data first
plot(ATTR~jitter(LEG),data=DF,col="gray")
# add fitted line for large abdomens; remember to backtransform fits!
lines(NEWLEG,exp(PREDSBYLEGABD2$fit))
# add 95% CIs -- very small so arguably can be omitted
# lines(NEWLEG,exp(PREDSBYLEGABD2$fit+1.96*PREDSBYLEGABD2$se.fit),lty=3)
# lines(NEWLEG,exp(PREDSBYLEGABD2$fit-1.96*PREDSBYLEGABD2$se.fit),lty=3)

# add fitted line for small abdomens; remember to backtransform fits!
lines(NEWLEG,exp(PREDSBYLEGABDmin2$fit),lty=2)
# add 95% CIs
# lines(NEWLEG,exp(PREDSBYLEGABDmin2$fit+1.96*PREDSBYLEGABDmin2$se.fit),lty=3)
# lines(NEWLEG,exp(PREDSBYLEGABDmin2$fit-1.96*PREDSBYLEGABDmin2$se.fit),lty=3)

# now plot effect of abdomens
plot(ATTR~ABDOMEN,data=DF,col="gray")
# add fitted line for large abdomes; remember to backtransform fits!
lines(NEWABD,exp(PREDSBYABDLEG2$fit))
# add 95% CIs
# lines(NEWABD,exp(PREDSBYABDLEG2$fit+1.96*PREDSBYABDLEG2$se.fit),lty=3)
# lines(NEWABD,exp(PREDSBYABDLEG2$fit-1.96*PREDSBYABDLEG2$se.fit),lty=3)

# add fitted line for small abdomens; remember to backtransform fits!
lines(NEWABD,exp(PREDSBYABDLEGmin2$fit),lty=2)
# add 95% CIs
# lines(NEWABD,exp(PREDSBYABDLEGmin2$fit+1.96*PREDSBYABDLEGmin2$se.fit),lty=3)
# lines(NEWABD,exp(PREDSBYABDLEGmin2$fit-1.96*PREDSBYABDLEGmin2$se.fit),lty=3)



# now plot effect of abdomens on log
plot(log(ATTR)~ABDOMEN,data=DF,col="gray",ylab="Log-transformed attractiveness")
# add fitted line for large abdomens
lines(NEWABD,PREDSBYABDLEG2$fit)
# add 95% CIs
# lines(NEWABD,exp(PREDSBYABDLEG2$fit+1.96*PREDSBYABDLEG2$se.fit),lty=3)
# lines(NEWABD,exp(PREDSBYABDLEG2$fit-1.96*PREDSBYABDLEG2$se.fit),lty=3)

# add fitted line for small abdomens
lines(NEWABD,PREDSBYABDLEGmin2$fit,lty=2)
# add 95% CIs
# lines(NEWABD,exp(PREDSBYABDLEGmin2$fit+1.96*PREDSBYABDLEGmin2$se.fit),lty=3)
# lines(NEWABD,exp(PREDSBYABDLEGmin2$fit-1.96*PREDSBYABDLEGmin2$se.fit),lty=3)

