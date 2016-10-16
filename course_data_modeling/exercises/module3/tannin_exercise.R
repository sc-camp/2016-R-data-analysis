library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

tannin_data = read.csv("data/tannin.csv")

p1 = ggplot(data=tannin_data, aes(x=TANNIN, y=GROWTH)) + theme_bw()
p1 = p1 + geom_point()
p1 = p1 + geom_smooth(method="lm")

lm_tannin = lm(GROWTH~TANNIN, data=tannin_data)

# The coefficients each have standard errors, a t-value which is a test statistic for the null hypothesis of a zero coefficient value, and a p-value associated with this null hypothesis.
print(summary(lm_tannin))
print(p1)

par(mfrow=c(2,3))
plot(lm_tannin)

# plot histogram of residuals, if shape is normal: sounds good.
hist(lm_tannin$residuals)

par(mfrow=c(1,1))

# summary.aov(lm_tannin)

# lm_null<-lm(GROWTH~1,data=tannin_data)

MOD.2<-update(lm_tannin,data=tannin_data[-7,])
print(summary(MOD.2))

MOD.3<-update(lm_tannin,data=tannin_data[-9,])
print(summary(MOD.3))


## Play with prediction
NEWXVARS<-seq(0,8,length=100)
NEWYVARS<-predict(lm_tannin,newdata=list(TANNIN=NEWXVARS),int="c")


plot(GROWTH~TANNIN, data=tannin_data)
matlines(NEWXVARS,NEWYVARS,lty=c(1,2,2),col="black")

