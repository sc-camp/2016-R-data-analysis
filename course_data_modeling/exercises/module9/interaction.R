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

cairn = data.table(read.csv("data/CairngormNL.csv"))
print(summary(cairn))
print(str(cairn))

ggpairs(cairn)

par(mfrow=c(2,2))
hist(cairn$REPID)
hist(cairn$ELEVATION)
hist(cairn$PLANT.HT)
hist(cairn$TRANSMISSION)
par(mfrow=c(1,1))

ggplot(data=cairn, aes(x=ELEVATION, y=TRANSMISSION, group=PLANT.HT)) + geom_boxplot() + theme_bw()
ggplot(data=cairn, aes(x=ELEVATION, y=TRANSMISSION, color=PLANT.HT)) + geom_point() + theme_bw()

par(mfrow=c(2,3))
lm.1 = lm(TRANSMISSION~ELEVATION*PLANT.HT, data=cairn)
plot(lm.1)
hist(lm.1$residuals)
summary(lm.1)
par(mfrow=c(1,2))
visreg(lm.1)
## crappy summary

## try to remove interaction
lm.1.simple = update(lm.1, ~. -ELEVATION:PLANT.HT)
visreg(lm.1.simple)
anova(lm.1, lm.1.simple)
### same AND simpler we keep


mod.quad = lm(TRANSMISSION~PLANT.HT + poly(ELEVATION,2),data=cairn)
par(mfrow=c(2,3))
plot(mod.quad)
hist(mod.quad$residuals)
summary(mod.quad)
anova(lm.1.simple, mod.quad)

avPlots(mod.quad)

par(mfrow=c(1,1))
visreg(mod.quad, xvar="ELEVATION", by="PLANT.HT", overlay=TRUE)
# cairn$plant_scaled = round(cairn$PLANT.HT/10) + 1
# mod.quad = lm(TRANSMISSION~plant_scaled + poly(ELEVATION,2),data=cairn)
# visreg(mod.quad, xvar="ELEVATION", by="plant_scaled", overlay=TRUE)


### predictions
to_predict_ELEV = seq(600, 1200, 10)
to_predict_PLANT = seq(0, 50, 1)

to_predict = expand.grid(ELEVATION=to_predict_ELEV, PLANT.HT=to_predict_PLANT)

predicted = predict(mod.quad, to_predict, int="c")
df_pred = data.table(cbind(to_predict, predicted))

# ggplot(data=cairn) + geom_point(aes(y=TRANSMISSION, x=ELEVATION, color=PLANT.HT)) + theme_bw() + geom_line(data=df_pred, aes(x=ELEVATION, y=fit, group=PLANT.HT), alpha=.15) + geom_line(data=df_pred[PLANT.HT == mean(df_pred$PLANT.HT),], aes(x=ELEVATION, y=fit, group=PLANT.HT), color="blue", size=1.1) + scale_color_gradient(low="green", high="purple")

ggplot(data=cairn, aes(y=TRANSMISSION, x=ELEVATION, color=PLANT.HT)) + geom_point() + theme_bw() + stat_smooth(method = "lm", formula = y ~ x + I(x^2),se=FALSE) + geom_line(data=df_pred[PLANT.HT == median(df_pred$PLANT.HT),], aes(x=ELEVATION, y=fit, group=PLANT.HT), color="red", size=1.1)
