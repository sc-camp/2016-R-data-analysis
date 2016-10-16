library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(data.table)

data_growth = data.table(read.csv("data/growth.csv"))
print(summary(data_growth))

data_growth$SUPPLEMENT = relevel(data_growth$SUPPLEMENT, ref="control")

ggplot(data=data_growth, aes(x=SUPPLEMENT, y=GAIN, fill=DIET)) + theme_bw() + geom_boxplot() 

ggplot(data=data_growth, aes(x=SUPPLEMENT, y=GAIN, color=DIET)) + theme_bw() + geom_point() #+ geom_smooth(method="lm", fullrange = TRUE, se = FALSE) + expand_limits(x = 0, y = 0)

lm.1 = lm(GAIN~DIET+SUPPLEMENT, data=data_growth)
summary(lm.1)

lm.2 = lm(GAIN~DIET*SUPPLEMENT, data=data_growth)
summary(lm.2)

lm.diet = lm(GAIN~DIET, data=data_growth)
summary(lm.diet)
lm.supplement = lm(GAIN~SUPPLEMENT, data=data_growth)
summary(lm.diet)

anova(lm.2, lm.1)
anova(lm.1, lm.diet)
anova(lm.1, lm.supplement)

logLik(lm.1)
logLik(lm.2)
logLik(lm.diet)
logLik(lm.supplement)

AIC(lm.1)
AIC(lm.2)
AIC(lm.diet)
AIC(lm.supplement)

par(mfrow=c(2,2))
plot(lm.1)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(lm.2)
par(mfrow=c(1,1))


## lm.1 is the best 
summary(lm.1)

# Make a table of the weight gain predicted by the model for each of the 12 combinations of factor levels (i.e. diet = barley and supplement = control, diet = barley and supplement = agrimore, etc.) using the predict() function

to_predict = expand.grid(DIET=levels(data_growth$DIET), SUPPLEMENT=levels(data_growth$SUPPLEMENT))

predictions = predict(lm.1, to_predict)

d_pr = data.frame(GROWTH=predictions, DIET=to_predict$DIET, SUPPLEMENT=to_predict$SUPPLEMENT)




