
##
# ggplot code for ANCOVA practical
#
# Also includes some dplyr code for streamlining parts of the exercise.
#
# Assumes data and models etc already in project workspace...
#
##

library(ggplot2)
library(dplyr)



#### Root:Fruit (Grazing) ####

##
# Assumes data and models already created 
##

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


df_graze_pred <- data_frame(GRAZING = factor(c(rep("Grazing", 
                                                   times = length(NEWGRAZED$GRAZING)), 
                                               rep("Ungrazed",
                                                   times = length(NEWUNGRAZED$GRAZING)))),
                            ROOT = c(NEWGRAZED$ROOT, NEWUNGRAZED$ROOT),
                            FRUIT_PRED = as.numeric(c(YVGRAZED[,"fit"], YVUNGRAZED[,"fit"])),
                            FRUIT_LWR = as.numeric(c(YVGRAZED[,"lwr"], YVUNGRAZED[,"lwr"])),
                            FRUIT_UPR = as.numeric(c(YVGRAZED[,"upr"], YVUNGRAZED[,"upr"])))

##
# Create ggplot from predicted data,
#  then plot points over the top of this
##
ggplot(df_graze_pred, aes(x = ROOT, y = FRUIT_PRED,
                          group = GRAZING)) +
  geom_line() +
  geom_ribbon(aes(ymin = FRUIT_LWR,
                  ymax = FRUIT_UPR),
              alpha = 0.1) +
  geom_point(data = COMP, aes(x = ROOT, y = FRUIT,
                              colour = GRAZING),
             size = 4, alpha = 0.7) +
  labs(x = "Initial root diameter",
       y = "Seed production") +
  theme_classic()


#### Supplementary exercise ####

WEIGHTS<-read.csv("growth.csv")

##
# Use dplyr for means and sds
##

WEIGHTS %>% 
  group_by(DIET, SUPPLEMENT) %>% 
  summarise(mean_gain = mean(GAIN),
            sd_gain = sd(GAIN))

WEIGHTS$SUPPLEMENT<-relevel(WEIGHTS$SUPPLEMENT,ref="control")


# plot the data
ggplot(WEIGHTS, aes(x = SUPPLEMENT, y = GAIN,
                    colour = DIET)) +
  geom_boxplot() +
  theme_classic()





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

##
# Use dplyr to explicitly code a new variable
##

WEIGHTS <- WEIGHTS %>% 
  mutate(SUPP2 = factor(ifelse(SUPPLEMENT %in% c("agrimore","supersupp"),
                               "best", "worst")))


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


# first, save the means, sd's and sample sizes of all treatment combinations in
# named tapply objects (these objects are classed as matrices, and are treated
# differently from dataframes in recognition of their different structure, i.e.,
# they contain the same "kind" of variable in both rows and columns) tapply
# needs as input the variable to summarize, then a list containing the factor
# variables, then the function to apply to the records belonging in each factor
# group
GAIN_INFO <- WEIGHTS %>% 
  group_by(SUPPLEMENT, DIET) %>% 
  summarise(mean_gain = mean(GAIN),
            sd_gain = sd(GAIN),
            n_gain = n())
  
# have a look!
GAIN_INFO
# note the well-balanced experimental design, with equal replication in all
# groups!


##
# Using ggplot to create bar plots, with supplement as main x-axis
#  but also separate bars for each diet within those groups.
#
# Within the call to geom_bar, I call 'identity' for the statistical
#  transformation to tell ggplot that I want the bar to just represent the 
#  value in the associated row (because we have already worked out the means).
#
# I also use 'position="dodge"' to describe how the bars should be positioned.
# Have a look at the documentation for geom_bar to find out more...
#
# geom_errorbar creates error bars; by default, the crossbars are the same width
#  as your main bars, but I prefer to shrink the width. This does mess with the 
#  positioning of the error bars though, so I have used 'position' to move these
#  slightly.
#
# 'scale_fill_grey' tells ggplot that, for the variable we have specified in
#   the aesthetics to give different 'fill' colours to, I want to use shades 
#   of grey (and ggplot will make as many as required)
#
# I have annotated the figure with the 'N=x' labels for every bar;
#  I use 'paste' to create a vector of labels, and also assign a value
#  to a variable I've called 'bump' to say how much I want to move the labels
#  from a position for each group of bars (play around with this to see what
#  I mean!)

annot_lab = paste("N=", GAIN_INFO$n_gain)
bump <- 0.3

ggplot(GAIN_INFO, aes(x = SUPPLEMENT, y = mean_gain,
                      fill = DIET)) +
  geom_bar(stat="identity",
           position="dodge") +
  geom_errorbar(aes(ymin = mean_gain - sd_gain,
                    ymax = mean_gain + sd_gain),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  scale_fill_grey() +
  labs(x = "Supplement",
       y = "Mass gain (kg)") +
  annotate("text", 
           x = c(1-bump,1,1+bump,
                 2-bump,2,2+bump,
                 3-bump,3,3+bump,
                 4-bump,4,4+bump), 
           y = -0.8, 
           label = annot_lab) +
  theme_classic()





