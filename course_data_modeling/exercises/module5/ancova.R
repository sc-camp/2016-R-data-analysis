library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(data.table)

data_comp = data.table(read.csv("data/compensationo.csv"))
print(summary(data_comp))

ggplot(data=data_comp, aes(x=ROOT, y=FRUIT, color=GRAZING)) + theme_bw() + geom_point() 
ggplot(data=data_comp, aes(x=ROOT, y=FRUIT, color=GRAZING)) + theme_bw() + geom_boxplot() 

ggplot(data=data_comp, aes(x=ROOT, y=FRUIT, color=GRAZING)) + theme_bw() + geom_point() + geom_smooth(method="lm", fullrange = TRUE, se = FALSE) + expand_limits(x = 0, y = 0)

lm_plus = lm(FRUIT~ROOT+GRAZING, data=data_comp)
summary(lm_plus)
vif(lm_plus)
par(mfrow=c(2,2))
plot(lm_plus)
par(mfrow=c(1,1))

lm_interaction = lm(FRUIT~ROOT:GRAZING, data=data_comp)
summary(lm_interaction)
# vif(lm_interaction)
par(mfrow=c(2,2))
plot(lm_interaction)
par(mfrow=c(1,1))

lm_all = lm(FRUIT~ROOT*GRAZING, data=data_comp)
lm_all = lm(FRUIT~ROOT+GRAZING+ROOT:GRAZING, data=data_comp)


summary(lm_all)
vif(lm_all)
par(mfrow=c(2,2))
plot(lm_all)
par(mfrow=c(1,1))

## vif shows that lm_all has more than 10 for GRAZING and  ROOT:GRAZING 
# we redo the model without GRAZING
lm_simple = lm(FRUIT~ROOT, data=data_comp)
summary(lm_simple)
# vif(lm_simple)
par(mfrow=c(2,2))
plot(lm_simple)
par(mfrow=c(1,1))

anova(lm_plus, update(lm_plus, ~. - GRAZING))
anova(lm_plus, lm_simple)

logLik(lm_simple)
logLik(lm_plus) ## best logLik
logLik(lm_interaction)
logLik(lm_all) ## best logLik

AIC(lm_simple)
AIC(lm_plus) ## best AIC
AIC(lm_interaction)
AIC(lm_all)

coef(summary(lm_plus))


# > summary(lm_all)
# 
# Call:
#   lm(formula = FRUIT ~ ROOT * GRAZING, data = data_comp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -17.3177  -2.8320   0.1247   3.8511  17.1313 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -125.173     12.811  -9.771 1.15e-11 ***
#   ROOT                   23.240      1.531  15.182  < 2e-16 ***
#   GRAZINGUngrazed        30.806     16.842   1.829   0.0757 .  
# ROOT:GRAZINGUngrazed    0.756      2.354   0.321   0.7500    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.831 on 36 degrees of freedom
# Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9234 
# F-statistic: 157.6 on 3 and 36 DF,  p-value: < 2.2e-16
# 
# > summary(lm_plus)
# 
# Call:
#   lm(formula = FRUIT ~ ROOT + GRAZING, data = data_comp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -17.1920  -2.8224   0.3223   3.9144  17.3290 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -127.829      9.664  -13.23 1.35e-15 ***
#   ROOT              23.560      1.149   20.51  < 2e-16 ***
#   GRAZINGUngrazed   36.103      3.357   10.75 6.11e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.747 on 37 degrees of freedom
# Multiple R-squared:  0.9291,	Adjusted R-squared:  0.9252 
# F-statistic: 242.3 on 2 and 37 DF,  p-value: < 2.2e-16



### slope deviation is 0.756 with 2.35 std so they are the same....
## thus we get rid of interaction and keep lm_simple
## slope is 23.56, intercept difference for ungrazed is +36.103


data_to_predict = copy(data_comp)
data_to_predict[,FRUIT:=NULL]
d1 = data.frame(predicted=predict(lm_plus, data_to_predict))
d2 = data.frame(observed=data_comp$FRUIT)

d1$sample = as.numeric(rownames(d1))
d2$sample = as.numeric(rownames(d2))

dd = merge(d1, d2)
dd_g = gather(dd, type, value, predicted:observed)

ggplot(data=dd_g, aes(x=sample, y=value, color=type)) + theme_bw() + geom_bar(stat="identity", position="dodge")

t.test(d1$predict, d2$observed, paired = TRUE)

