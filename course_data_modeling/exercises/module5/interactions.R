library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(data.table)

data_ozone = read.csv("data/oneway.csv")
summary(data_ozone)

# do gardens differ in ozone levels?
# predictor is garden
# response is ozone



### Data Control
ggplot(data=data_ozone, aes(x=GARDEN, y=OZONE, color=GARDEN)) + theme_bw() + geom_point() + geom_jitter()
ggplot(data=data_ozone, aes(x=factor(GARDEN), y=OZONE)) + theme_bw() + geom_boxplot() + xlab("GARDEN")
# we see no outliers so we continue


### linear model
lm_ozone = lm(OZONE~factor(GARDEN), data=data_ozone)
summary(lm_ozone)
# vif(lm_ozone)
par(mfrow=c(2,2))
plot(lm_ozone)
par(mfrow=c(1,1))

## T-test will be used to reject the null hypothesis: the samples come from the same group.
data_ozone = data.table(data_ozone)
t.test(data_ozone[GARDEN=='A']$OZONE, data_ozone[GARDEN=='B']$OZONE)


