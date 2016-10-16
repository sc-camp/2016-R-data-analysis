# Luc Bussière
# Advancing in R practical session
# ANOVA, GLM
# Oct 1, 2011

# last modified Nov 29, 2015

# clear R of all objects
rm(list=ls())
getwd()
#load data: sample oneway anova table of ozone levels in two gardens
ONEWAY<-read.csv("oneway.csv")

names(ONEWAY)
head(ONEWAY)
str(ONEWAY)

# inspect data
hist(ONEWAY$OZONE)
# looks good -- note that the garden variable is categorical, so we can't make a histogram of it

# instead, we might want to know how many levels there are, and how many measures per level
levels(ONEWAY$GARDEN)
summary(ONEWAY$GARDEN)

# make a plot to illustrate patterns

# to make the plot, you need to think carefully about what are the independent and dependent vars
# in this case we have a single category, and a single continuous var, so a boxplot is a logical first step

boxplot(OZONE~GARDEN,data=ONEWAY)
# this suggests that garden B has higher ozone levels
 
# to check, build a model 
MOD.1<-lm(OZONE~GARDEN,data=ONEWAY)

# examine diagnostics
par(mfrow=c(2,3))
plot(MOD.1,lwd=2,cex=1.5)
hist(MOD.1$residuals)
par(mfrow=c(1,1))
# looks a bit weird, but don't panic! Because you are dealing with categories, the distribution of fitted and resids may sometimes look unusual
# There are only two fitted values because there are only two categories, and each one gets a fitted value at the estimated mean

summary(MOD.1)
# note here what the two estimates mean in the context of your initial exploratory plots

# you may have previously heard of a t-test to compare means -- how would that compare to our approach here?
?t.test
t.test(OZONE~GARDEN,data=ONEWAY)
# note that the t-value and p-value are identical
# so a t-test is really just a special case of a linear model!

# make a plot, for which you can use the boxplot above as a starting point, adding in axis titles
# cosmetic changes such as colours and line styles are up to you
boxplot(OZONE~GARDEN,data=ONEWAY,ylab="Ozone level",xlab="Garden",col=c("gray","white"))

# could equally easily do this using {ggplot2}


# sample results text
# Ozone levels in Garden B were significantly higher than those in Garden A (t=3.87, df=18, P = 0.001; see Figure 1).



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# new analysis -- new data on effect of grazing and root diameter on seed set
COMP<-read.csv("compensationo.csv")

str(COMP)

# inspect data
par(mfrow=c(2,1))
hist(COMP$FRUIT)
hist(COMP$ROOT)
par(mfrow=c(1,1))
# note that we don't often transform independent data (here, Root), but it can be 
# useful to know the distribution anyway

# examine the factor levels and sample size
levels(COMP$GRAZING)
summary(COMP$GRAZING)

 
# plot data
# again think carefully about what is the question
# do we want fruit as a function of root, or the converse? this is crucial!!!! always y~x
boxplot(FRUIT~GRAZING,data=COMP,ylab="Seed production")
plot(FRUIT~ROOT,data=COMP)

# this is a start, but you really want to simultaneously plot seed set as a function of both variables
# the long way to do this involves subdividing the data (see below where we make the figure)
# but for now we can do a quick job to inspect things with the qplot command from the ggplot2 library
library(ggplot2)
qplot(ROOT,FRUIT,data=COMP,geom="point",colour=GRAZING,size=4)

# although this isn't especially pretty, it quickly tells us that root predicts fruit, suggests an effect of treatment and prob no interaction

# the lack of interaction is important because ANCOVA assumes homogeneity of slopes. if slopes are not homogeneous, must use another approach than ANCOVA to analyze data

# take a moment to predict the slopes and intercepts


# build a model to see if you're right
MOD.G1<-lm(FRUIT~ROOT*GRAZING,data=COMP)
# remember that the '*' means include an interaction, while '+' says only put main effects in

# examine diagnostics
par(mfrow=c(2,3))
plot(MOD.G1)
hist(MOD.G1$residuals)
par(mfrow=c(1,1))
# these diagnostics suggest the model is not great, but perhaps simplifying the model will help
# note that you should always remove the interaction term at least -- see Engqvist's paper

summary(MOD.G1)

# first simplify by removing the interaction
MOD.G2<-update(MOD.G1,~. - ROOT:GRAZING)

# and do an F-test
anova(MOD.G1,MOD.G2)
# the simpler model is preferred, so let's examine it
summary(MOD.G2)
# it looks like this is the min model, but check anyway
MOD.G3<-update(MOD.G2,~. - GRAZING)
anova(MOD.G3,MOD.G2)
MOD.G4<-update(MOD.G2,~. - ROOT)
anova(MOD.G4,MOD.G2)

# minimum adequate model is MOD.G2

# examine diagnostics
par(mfrow=c(2,3))
plot(MOD.G2)
hist(MOD.G2$residuals)
par(mfrow=c(1,1))


# there are many ways to split the data and plot by colour

# can simply clean up code above by adding better x and y-axis labels
# alternatively, generate objects with the subset command and plot one treatment first
# another way, shown here, is to use the 'split' command
levels(COMP$GRAZING)
SF<-split(COMP$FRUIT,COMP$GRAZING)
# syntax here is create a new object that has the FRUIT scores split by GRAZING treatment
# have a look:
SF
str(SF)
# note that this is a list, not a data frame
# this has implications for how you call different parts of the list below

SR<-split(COMP$ROOT,COMP$GRAZING)
# similar for ROOT

plot(FRUIT~ROOT,data=COMP,type="n",ylab="Seed production",xlab="Initial root diameter",lwd=2)
# the type="n" switch above suppressed the plotting of points, so that you can add them later with modifications 
# in the next lines I will doubly nest square brackets to give the address of the vectors within the list
points(SR[[1]],SF[[1]],pch=16,lwd=2,cex=1.5)
# this plots only the first vector for the split objects created above, with point character (pch) = 16
points(SR[[2]],SF[[2]],cex=1.5,lwd=2)
# this plots only the second vector for the split objects created above, with point character (pch) = 1 , the default

# add a legend to make things clearer 
legend("topleft",
	   title="Grazing level",
	   legend=c("Grazed","Ungrazed"),
	   pch=c(16,1),lty=c(1,2),bty="n",cex=1.5,lwd=2)

# these commands add the fitted lines, if you want just the lines
# where do the numbers in the line commands below come from?
abline(-127.829,23.56,lwd=2)
abline(-127.829+36.103,23.56,lty=2,lwd=2)
	   
	   
# alternatively, you may (??) want to indicate confidence around lines
# some people find this a bit too busy -- it's your call

# I copy the code from the regression script, but note that there are two lines!
# so we need not just new values for root, but also for grazing level 
# use the expand.grid command, rather than just the seq command so that you can use predict appropriately
NEWGRAZED<-expand.grid(GRAZING="Grazed",ROOT=seq(4,11,0.1))
NEWUNGRAZED<-expand.grid(GRAZING="Ungrazed",ROOT=seq(4,11,0.1))
# have a look at what this produces!
head(NEWGRAZED)

# the predicted y values come by using the predict command and specifying which vector to substitute for 
# the x variables in the original regression. Note that here we need to tell the predict command where to find 
# the Grazing level as well as the Root measure
# The argument int="c" tells matlines to draw 95% confidence intervals
YVGRAZED<-predict(MOD.G1,list(GRAZING=NEWGRAZED$GRAZING,ROOT=NEWGRAZED$ROOT),int="c")
YVUNGRAZED<-predict(MOD.G1,list(GRAZING=NEWUNGRAZED$GRAZING,ROOT=NEWUNGRAZED$ROOT),int="c")

# plot the data agin
plot(FRUIT~ROOT,data=COMP,type="n",ylab="Seed production",xlab="Initial root diameter",lwd=2)
plot(FRUIT~ROOT,data=COMP,type="n",ylab="Seed production",xlab="Initial root diameter",lwd=2)
points(SR[[1]],SF[[1]],pch=16,lwd=2,cex=1.5)
points(SR[[2]],SF[[2]],cex=1.5,lwd=2)

# then matlines twice, using the appropriate new x and y variables to draw the fit and the 95% CIs 
matlines(NEWGRAZED$ROOT,YVGRAZED,lty=c(1,3,3),col="black",lwd=2)  
matlines(NEWUNGRAZED$ROOT,YVUNGRAZED,lty=c(2,3,3),col="black",lwd=2)  

# add a legend to make things clearer 
legend("topleft",
       title="Grazing level",
       legend=c("Grazed","Ungrazed"),
       pch=c(16,1),lty=c(1,2),bty="n",cex=1.5,lwd=2)

# check that the model parameters make sense. do they?

##########################

# if you really want to check (not usually necessary), expand the lines and the range of x and y to see the intercepts
NEWGRAZED<-expand.grid(GRAZING="Grazed",ROOT=seq(0,11,0.1))
NEWUNGRAZED<-expand.grid(GRAZING="Ungrazed",ROOT=seq(0,11,0.1))

YVGRAZED<-predict(MOD.G1,list(GRAZING=NEWGRAZED$GRAZING,ROOT=NEWGRAZED$ROOT),int="c")
YVUNGRAZED<-predict(MOD.G1,list(GRAZING=NEWUNGRAZED$GRAZING,ROOT=NEWUNGRAZED$ROOT),int="c")

plot(FRUIT~ROOT,data=COMP,type="n",ylab="Seed production",xlab="Initial root diameter",lwd=2,xlim=c(0,11),ylim=c(-160,140))
points(SR[[1]],SF[[1]],pch=16,lwd=2,cex=1.5)
points(SR[[2]],SF[[2]],cex=1.5,lwd=2)

# then matlines twice, using the appropriate new x and y variables to draw the fit and the 95% CIs 
matlines(NEWGRAZED$ROOT,YVGRAZED,lty=c(1,3,3),col="black",lwd=2)  
matlines(NEWUNGRAZED$ROOT,YVUNGRAZED,lty=c(2,3,3),col="black",lwd=2)  

# add a legend to make things clearer 
legend("topleft",
       title="Grazing level",
       legend=c("Grazed","Ungrazed"),
       pch=c(16,1),lty=c(1,2),bty="n",cex=1.5,lwd=2)

###########################
# sample results text 

# After controlling for the positive effect of initial root diameter (B = 23.56 +/- 1.15, t=20.51, P<0.001), plants from ungrazed plots set significantly more seeds than those from grazed plots (intercept deviation for ungrazed treatment = 36.10 +/- 3.36, t= 10.75, P<0.001; see Figure 2).


# SuppEx

# clear R of all objects
rm(list=ls())


# new analysis -- new data on effect of DIET and SUPPLEMENT on weight GAIN in livestock
WEIGHTS<-read.csv("growth.csv")

names(WEIGHTS)
head(WEIGHTS)

# inspect data, noting two cat vars and only one continuous one, the response
hist(WEIGHTS$GAIN)
levels(WEIGHTS$DIET)
levels(WEIGHTS$SUPPLEMENT)
summary(WEIGHTS)

# you may wish to compute means and SDs for each combination of DIET and SUPPLEMENT treatment
# this might be useful for your results text or in a table
# the tapply command arranges the products of a function according to group membership
tapply(WEIGHTS$GAIN,list(WEIGHTS$DIET, WEIGHTS$SUPPLEMENT),mean)
tapply(WEIGHTS$GAIN,list(WEIGHTS$DIET, WEIGHTS$SUPPLEMENT),sd)

# at this stage you may notice that there is a conspicuous control group in the SUPPLEMENT category, but 
# because c follows a for agrimore in the alphabet, the default comparison contrast will be with agrimore
# to change this, use the following 
WEIGHTS$SUPPLEMENT<-relevel(WEIGHTS$SUPPLEMENT,ref="control")



# plot the data
boxplot(WEIGHTS$GAIN~WEIGHTS$DIET+WEIGHTS$SUPPLEMENT,cex.axis=0.6)



# looks like both factors affect weight GAIN, but no real indication of interaction

# build a model
MOD.1<-lm(GAIN~DIET*SUPPLEMENT,data=WEIGHTS)
# recall that the * operator indicates an interaction, and the hypothesis addressed by the interaction term is something like: does the effect of DIET depend on SUPPLEMENT?


par(mfrow=c(2,3))
plot(MOD.1)
hist(MOD.1$residuals)
par(mfrow=c(1,1))
# diags not pretty, but maybe simp will help

summary(MOD.1)
# make sure you can interpret each of these parameter estimates! 

# as we suspected, none of interaction estimates seem important, so try simplification
MOD.2<-update(MOD.1,~. -DIET:SUPPLEMENT)
anova(MOD.1,MOD.2)
# we're OK to remove the interaction
# note that the P-value of the F test is the same as that provided by the anova table, below
summary.aov(MOD.1)


summary(MOD.2)
# notice how many parameters were being sucked up by the interaction!

# inspect the new model's diagnostics
par(mfrow=c(2,3))
plot(MOD.2)
hist(MOD.2$residuals)
par(mfrow=c(1,1))
# better!! this often happens as you approach a good model


# try to simplify further
MOD.3<-update(MOD.2,~. -SUPPLEMENT)
anova(MOD.2,MOD.3)

MOD.4<-update(MOD.2,~. -DIET)
anova(MOD.2,MOD.4)
# as expected, no further simplification is possible at the level of main effects

# however, you could try consolidating subgroups of treatments based on observing that the two best SUPPLEMENTs seem to outperform the two worst ones
# the code to do this is below

SUPP2<-factor(WEIGHTS$SUPPLEMENT)
# generates a new object to manipulate, so that we don't accidentally mess up the original data
# have a look at it
levels(SUPP2)

# now recode the new factor so that you combine similar treatment levels
levels(SUPP2)[c(2,4)]<-"best"
levels(SUPP2)[c(1,3)]<-"worst"
levels(SUPP2)
# the preceding commands reassign levels based on what we suspect is the grouping involved. note order must match levels, so check text and your data carefully


MOD.5<-lm(GAIN~DIET+SUPP2,data=WEIGHTS)
anova(MOD.2,MOD.5)
# this analysis suggests the recoding is worth doing, as the model deviance is not substantially increased by having four levels of SUPPLEMENT instead of two
# so the minimal adequate model is MOD.5
summary(MOD.5)

# check the diagnostics again
par(mfrow=c(2,3))
plot(MOD.5)
hist(MOD.5$residuals)
par(mfrow=c(1,1))
# and the diagnostics for this model are even better!



# illustrate the data
# note that even if the minimum model says that two treatments are indistinguishable, by convention we show all treatments on plots and simply explain where the differences are if it's not already obvious


# if you prefer, you can make a barplot 
# the barplot command can exploits the table style output of tapply

# first, save the means, sd's and sample sizes of all treatment combinations in named tapply objects (these objects are classed as matrices, and are treated differently from dataframes in recognition of their different structure, i.e., they contain the same "kind" of variable in both rows and columns)
# tapply needs as input the variable to summarize, then a list containing the factor variables, then the function to apply to the records belonging in each factor group
MN.GAIN<-tapply(WEIGHTS$GAIN,list(WEIGHTS$DIET,WEIGHTS$SUPPLEMENT),mean)
# have a look!
MN.GAIN
SD.GAIN<-tapply(WEIGHTS$GAIN,list(WEIGHTS$DIET,WEIGHTS$SUPPLEMENT),sd)
SD.GAIN
N.GAIN<-tapply(WEIGHTS$GAIN,list(WEIGHTS$DIET,WEIGHTS$SUPPLEMENT),length)
N.GAIN
# note the well-balanced experimental design, with equal replication in all groups!

# the barplot command can use as input a matrix of values such as those provided by tapply
# we need to store the barplot as an object (here called MIDS) so we can save the x-axis position of the bars for later when we add error bars and sample sizes
MIDS<-barplot(MN.GAIN,beside=TRUE,col=c("dark gray","light gray","white"),ylim=c(0,40),ylab="Mass gain (kg)")
# note that even though we stored the product in a named object, we also simultaneously produced a plot
# if you like, have a look at the new object called MIDS in the workspace -- you should see that it contains only the x-axis values for the midpoints of the bars in our bar plot

# to add error bars indicating SDs, we'll use the command arrow(), which draws an arrow between two coordinates on a plot. We'll start the arrow at the mean value - 1 SD (we need to provide two coordinates for this point: MIDS gives the x-axis position, and MN.GAIN-SD.GAIN the y), and then stretch it up to the mean value + 1 SD. The argument angle=90 tells R to make the arrowhead straight across, and the argument code=3 makes a straight line at both ends of the arrow.
arrows(MIDS,MN.GAIN-SD.GAIN,MIDS,MN.GAIN+SD.GAIN,angle=90,length=0.05,code=3) 

# let's be fancy and add a sample size designation to each bar
# again we need to give coordinates, so I use MIDS for x, and an arbitrary height of 1 for y 
text(MIDS,1,paste("N =",N.GAIN),cex=0.6)     

# finally, let's add a legend so anyone can work out which bar is which
# add legend
legend(6.7,41,title="Diet",legend=levels(WEIGHTS$DIET),fill=c("dark gray", "light gray","white"),bty="n")

# many of the arguments above I determined just by experimenting, to see what looks OK. 


# alternatively, make a boxplot instead
# I'll modify a few things to get the labels in groups like for the barplot above
# I use the argument xaxt="n" to suppress the labelling on the x, as I prefer to add my own labels using the low level plotting command axis()
boxplot(WEIGHTS$GAIN~WEIGHTS$DIET+WEIGHTS$SUPPLEMENT,xlab="Supplement",col=rep(c("dark gray","light gray","white"),4),xaxt="n")
axis(side=1,at=c(2,5,8,11),labels=levels(WEIGHTS$SUPPLEMENT))
legend(10.5,29,title="Diet",legend=levels(WEIGHTS$DIET),fill=c("dark gray","light gray","white"),bty="n")
# the first two numbers in the legend command give coordinates for where to place the legend
# experiment with changing these to see!

# sample results text and table
# Both diet and dietary supplement affected mass gain, but each factor acted independently of the other (diet by supplement interaction F(6,36)=0.33, P=0.917). Cattle gained the most mass on barley, followed by oats, and fared most poorly on wheat (see Table 1 and Figure 1). Both agrimore and supersupp supplements improved mass gain on all three diets, but were indistinguishable from one another, while the supergain supplement did not provide a significant mass gain relative to the control animals (see Table 1 and Figure 1).
# 
# Table 1. Parameter estimates from the minimal adequate model for the effect of diet and dietary supplement on cattle mass gain. The maximal model included an interaction between diet and supplement as well as separate intercept deviations for each of the supplement treatments, but these additional parameters did not significantly contribute to model fit and were consequently removed from the model.
# 
# Source         Estimate       SE       t        P    
# Intercept       23.0839     0.3674  62.825  < 0.001 
# Diet (oats)     -3.0928     0.4500  -6.873  < 0.001
# Diet (wheat)    -5.9903     0.4500 -13.311  < 0.001 
# Supplement      
# (agrimore        2.6754     0.3674   7.281  < 0.001
#  & supersupp)


