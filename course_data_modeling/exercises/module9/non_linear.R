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

decay = data.table(read.csv("data/decay.csv"))
print(summary(decay))
print(str(decay))

ggpairs(decay)

hist(decay$TIME)
hist(decay$MASSLEFT)
ggplot(data=decay, aes(x=TIME, y=MASSLEFT)) + geom_boxplot() + theme_bw()
ggplot(data=decay, aes(x=TIME, y=MASSLEFT)) + geom_point() + theme_bw()

ggplot(data=decay, aes(x=TIME, y=log(MASSLEFT))) + geom_point() + theme_bw()
ggplot(data=decay, aes(x=TIME, y=log(MASSLEFT))) + geom_point() + theme_bw() + geom_smooth(method="lm")

par(mfrow=c(2,2))

lm.1 = lm(MASSLEFT~TIME, data=decay)
plot(lm.1)
## crappy

lm.2 = lm(log(MASSLEFT)~TIME, data=decay)
plot(lm.2)
## better

par(mfrow=c(1,1))

summary(lm.2)
visreg(lm.2)

to_predict = seq(0, 30, .1)
pred_MASS = predict(lm.2, list(TIME=to_predict), int="c")

df_pred = cbind(data.frame(TIME=to_predict), exp(pred_MASS))


## now plot, do not forget to expo the predictions
plot(MASSLEFT~TIME, data=decay)
matlines(to_predict,exp(pred_MASS),lty=c(1,2,2),col="black")

## OR
ggplot(data=decay) + geom_point(aes(y=MASSLEFT, x=TIME)) + theme_bw() + geom_line(data=df_pred, aes(x=TIME, y=fit), color="blue") + geom_ribbon(data=df_pred, aes(x=TIME, ymin=lwr, ymax=upr), alpha=.2)



