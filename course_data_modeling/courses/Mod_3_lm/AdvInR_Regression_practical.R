# Luc Bussière
# Stats Practical Exercise on Regression
# Sep 27, 2011

# last modified Nov 29, 2015

# This is a practical exercise for the AdvInR course
# to conduct univariate linear regression 

# clear R of all objects
rm(list=ls())


# import the data & see columns & structure
# dataset of caterpillar growth fed different amounts of tannin in diet
TANDAT<-read.csv("tannin.csv")
str(TANDAT)
TANDAT
# originally I had called the dataset tannin, but after realizing that one of the columns was
# called tannin, I renamed it to avoid confusion

 
# inspect histograms
par(mfrow=c(1,2))
hist(TANDAT$GROWTH)
hist(TANDAT$TANNIN)
par(mfrow=c(1,1))
# not pretty, but also not unusual
# sample here is very small -- let's wait until we see diagnostics before we draw conclusions about this distribution



# plot data of interest: growth as a function of tannin
# the lwd argument changes the widths of all lines in the plots to twice the default
# the cex command changes the point size (cex is for character expansion)
plot(GROWTH~TANNIN,data=TANDAT,lwd=2,cex=1.5)
# suggests a solid negative relationship -- use this to anticipate parameters 
# could add a line here if you want, but not strictly necessary

# based on this plot I predict an intercept value of around 12, and a slope of around -1.25 (= rise/run = -10/8)

# build an initial model using lm
MOD.1<-lm(GROWTH~TANNIN,data=TANDAT)

# note that the new object has many components
str(MOD.1)

# before looking at the model, examine diagnostics!!!!

par(mfrow=c(2,3))
plot(MOD.1)
# note the preceding command will call four plots at once
# if you haven't changed the number of panels to plot using the par command, the R console will wait for you to hit enter between plotting each figure
hist(MOD.1$residuals)
par(mfrow=c(1,1))
# everything looks OK -- maybe slight concern at influence of datapts 4 and 7, which I could investigate(see below)

# examine the model 
summary(MOD.1)
# inspect residual quartiles, as well as summary stats



# check to see if datapoint 7 has undue influence: rerun model without it)

# OR perhaps more intuitively
MOD.2<-update(MOD.1,data=TANDAT[-7,])


summary(MOD.2)
# both parameters have changed, but in my opinion the change is not dramatic.
# in the absence of further justification for excluding (7), I would retain the first model

# MOD.3<-update(MOD.1,subset=(TANNIN!=9))
summary(MOD.3)
# as above, no dramatic change. One can conclude that neither of these points is unduly influencing the results


# examine the first model again 
summary(MOD.1)
# parameters conform well to expectations 

# interpretation on several levels

# (1) IMO, the most important conclusion is that for every increase of 1 unit of dose of tannin, there is
# a corresponding decrease of 1.22 units of growth
 
# (2) In the studied sample of caterpillars, the dietary tannin level explained 81.6% of variation in growth (the mult. R-squared value)
 
# (3) If you care about p-values, the whole model is significant, and both coefficients are significantly nonzero

#  This means that we can reject the null model (in regression, this is usually a model with only an intercept, which explains why the p-value for the slope term is the same as that for the whole model)
# We can also conclude that these data are unlikely if the "true" regression has a slope of zero
# If you were describing these results in a MS, you might produce a table of coefficients, and/or also cite the stats in the text as follows: Increased dietary tannin levels caused significant decreases in caterpillar growth  (regression slope (±SE) = -1.217 (±0.219),N=9,t=-5.565,P=0.0008; see Figure 1).

# Some people prefer to cite F-values, but note that this only works if the only term in the model is the one of interest; otherwise, the F-value won't be the same as that for the coefficient of interest. Also, because the F-value is associated with the whole model, and not the parameter, you can't really point out the parameter in the same parenthetical, which is why I prefer the example above. But for completeness, here's how you might cite the F:
# Increased dietary tannin levels caused significant decreases in caterpillar growth (F(1,7DF)=30.97,P=0.0008; see Figure 1)  

# if you want to see the ANOVA table, use one of the following:
summary.aov(MOD.1)
anova(MOD.1)
# note how much less information rich ANOVA tables are compared to tables of coefficients



# produce a publication-quality figure
# first with plot() from the base graphics package

# to generate a graphical representation of uncertainty about the parameter estimates, we use the following commands

# first generate a series of x values to plug into the regression equation
# they need to include the range of x and be sufficient in number (say, having 100 values) to create a smooth curve
NEWXVARS<-seq(0,8,length=100)


# the predicted y values come by using the predict() command and specifying which vector to substitute for the x variable in the original regression. The argument int="c" will tell a later command, matlines(), to draw 95% confidence intervals
NEWYVARS<-predict(MOD.1,newdata=list(TANNIN=NEWXVARS),int="c")
# examine the new variable to see what it looks like
head(NEWYVARS)
# can you guess what the int="c" argument did here? Try the command without this is you like


# first, generate a scatter plot of the data, and any arguments to change the look of the plot
plot(GROWTH~TANNIN,data=TANDAT,pch=16,cex=1.5,ylab="Caterpillar mass gain (g)",xlab="Dietary tannin dose (g/kg)",ylim=c(0,15))


# then matlines(), which is short for draw lines for each vector in this matrix, uses the new x and y variables to draw the fit and the 95% CIs 
matlines(NEWXVARS,NEWYVARS,lty=c(1,2,2),col="black")





# if you prefer, you can also do this with {ggplot2} 

# Load the libraries we'll need to use
library(dplyr)
library(ggplot2)

# Create a sequence of new x values
NEWXVARS <- seq(0,8,length=100)

# Create a sequence of predicted y values and confidence intervals
NEWYVARS <- predict(MOD.1,
                    newdata=list(TANNIN=NEWXVARS),
                    int="c")

# Check the structure of your predictions
str(NEWYVARS)

##
# Create a new data frame for your predictions:
#  newx: your range of new x values
#  newy: predicted y values for your range of x
#  newupr: the upper 95% confidence intervals
#  newlwr: the lower 95% confidence intervals
##
df.newvars <- data_frame(newx = NEWXVARS,
                         newy = as.numeric(NEWYVARS[,"fit"]),
                         newupr = as.numeric(NEWYVARS[,"upr"]),
                         newlwr = as.numeric(NEWYVARS[,"lwr"]))


## Plot just your predicted slope and shaded confidence regions
ggplot(df.newvars, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr,
                  alpha = 0.25))

## Plot predicted slope and confidence regions with better axis labels
ggplot(df.newvars, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr),
              alpha = 0.25) +
  labs(x = "Tannin",
       y = "Growth")

## Add points from the original TANDAT data set, and use theme_bw 
ggplot(df.newvars, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, 
                  ymax = newupr),
              alpha = 0.25) +
  geom_point(data = TANDAT, 
             aes(x = TANNIN,
                 y = GROWTH)) +
  labs(x = "Tannin",
       y = "Growth") +
  theme_bw()








# supplementary exercises
#####################

# read data in frame and examine it
CG<-read.csv("Cairngorm2012.csv")
str(CG)
head(CG)

# explore distributions of responses and predictors for today's exercises
hist(CG$ELEVATION)

# whoa! we're not in the himalayas
# find troublesome entry
CG[CG$ELEVATION>2000,]
# remove troublesome entry from new data.frame
CGFIX<-CG[-37,]
# check data
hist(CGFIX$ELEVATION)
# better

hist(CGFIX$AIR.TEMP.CORR)
hist(CGFIX$SOIL.TEMP.CORR)

# elevation has a broadly uniform distribution, but it is a predictor, so this doesn't matter
# temp vars don't look terrible

# exploratory plots of how elevation affects both soil temp and air temp
par(mfrow=c(1,2))
plot(CGFIX$AIR.TEMP.CORR~CGFIX$ELEVATION)
plot(CGFIX$SOIL.TEMP.CORR~CGFIX$ELEVATION)
par(mfrow=c(1,1))

# to compare more effectively, make sure the y range is the same across both plots
par(mfrow=c(1,2))
plot(CGFIX$AIR.TEMP.CORR~CGFIX$ELEVATION,ylim=c(-6,7))
plot(CGFIX$SOIL.TEMP.CORR~CGFIX$ELEVATION,ylim=c(-6,7))
par(mfrow=c(1,1))

# there is a quite noticeable negative effect in both cases, but the effect is much stronger for AIR than for SOIL. Use the formula rise/run to make a good guess at the slope parameter.

# the null hypotheses should sound something like the following:
# H0 for AIR: There is no effect of altitude on air temperature
# or this
# H0 for SOIL: The slope parameter for the regression of soil temperature on elevation is zero.


# build consecutive models for air and soil temp

AIR.MOD<-lm(AIR.TEMP.CORR~ELEVATION,data=CGFIX)

# examine diagnostics
par(mfrow=c(2,2))
plot(AIR.MOD)
par(mfrow=c(1,1))

# Q-Q plot is not perfect (as there's a hint of nonlinearity), but also not terrible -- looks OK

# view model summary
summary(AIR.MOD)



# For fun, let's see what would have happened if we had not cleaned the data

AIR.MOD.BAD<-lm(AIR.TEMP.CORR~ELEVATION,data=CG)

# examine diagnostics
par(mfrow=c(2,2))
plot(AIR.MOD.BAD)
par(mfrow=c(1,1))

# note the very odd behaviour of all four panels, and the clear high leverage and influence of record 37


# go back to correct model

# question 10 answers, briefly
# a negative
# b -0.019 degs change per m of altitude increase
# c conf limits around slope are 0.02129 and -0.01743 by hand
# you can also get these with the command below (although the numbers aren't exactly the same as the rough 1.96 SE)
confint(AIR.MOD)
# d I was right, of course
# e Yes, p<0.0001
# f 88.1%



# now soil
SOIL.MOD<-lm(SOIL.TEMP.CORR~ELEVATION,data=CGFIX)

# examine diagnostics
par(mfrow=c(2,2))
plot(SOIL.MOD)
par(mfrow=c(1,1))
# looks fine

# view model summary
summary(SOIL.MOD)

# question 10 answers, briefly
# a negative
# b -0.0068 degs change per m of altitude increase
confint(SOIL.MOD)
# c -0.0078 and -0.0057
# d I was right, of course, again
# e Yes, p<0.0001
# f 75.5%




# Sample Results text
# Both air temperature and soil temperature decreased at higher altitudes, although the effect in air was much stronger than it was in the soil (air slope = -0.0196 +- 0.0009, F(1,51)=443.2, P<0.0001; soil temperature slope = -0.0068 +- 0.0005, F(1,51)=156, P<0.0001; see Figure 1).




# prepare publication quality plots


# first, let's try making the plots using {graphics}
# generate new x vars over same range of x as data
summary(CGFIX)
NEWXVARS.AIR<-seq(650,1113,1)
# generate predictions of model
NEWYVARS.AIR<-predict(AIR.MOD,newdata=list(ELEVATION=NEWXVARS.AIR),int="c")

# same for SOIL
NEWXVARS.SOIL<-seq(650,1113,1)
# generate predictions of model
NEWYVARS.SOIL<-predict(SOIL.MOD,newdata=list(ELEVATION=NEWXVARS.SOIL),int="c")


# plot AIR
plot(CGFIX$AIR.TEMP.CORR~CGFIX$ELEVATION,xlab="Altitude (m)",ylab="Corrected Air Temperature (degs C)")
matlines(NEWXVARS.AIR,NEWYVARS.AIR,lty=c(1,2,2),col="black")

# plot soil
plot(CGFIX$SOIL.TEMP.CORR~CGFIX$ELEVATION,xlab="Altitude (m)",ylab="Corrected Soil Temperature (degs C)")
matlines(NEWXVARS.SOIL,NEWYVARS.SOIL,lty=c(1,2,2),col="black")

# if we want, we can position the two plots together rather easily
# note that you'll want to set the y-axis range to make sure they match across plots!

par(mfrow=c(1,2))
plot(CGFIX$AIR.TEMP.CORR~CGFIX$ELEVATION,xlab="Altitude (m)",ylab="Corrected Air Temperature (degs C)",main="Air Temperature",ylim=c(-6,6))
matlines(NEWXVARS.AIR,NEWYVARS.AIR,lty=c(1,2,2),col="black")
plot(CGFIX$SOIL.TEMP.CORR~CGFIX$ELEVATION,xlab="Altitude (m)",ylab="Corrected Soil Temperature (degs C)",main="Soil Temperature",ylim=c(-6,6))
matlines(NEWXVARS.SOIL,NEWYVARS.SOIL,lty=c(1,2,2),col="black")
par(mfrow=c(1,1))





