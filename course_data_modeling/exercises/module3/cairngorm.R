library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

data = read.csv("data/Cairngorm2012.csv")
names(data)
summary(data)
ggplot(data=data) + theme_bw() + geom_histogram(aes(x=ELEVATION), binwidth=25)
ggplot(data=data) + theme_bw() + geom_histogram(aes(x=AIR.TEMP.CORR), binwidth=1)
ggplot(data=data) + theme_bw() + geom_histogram(aes(x=SOIL.TEMP.CORR), binwidth=1)


data_clean = data[-which(data$ELEVATION > 9000),]


data_new = gather(data_clean, Prov, TEMPERATURE, AIR.TEMP.CORR:SOIL.TEMP.CORR) %>% separate(Prov, into=c("V1","V2", "V3")) %>% select(-c(V2, V3)) %>% rename(temp = TEMPERATURE, elev = ELEVATION, prov = V1)

ggplot(data=data_new, aes(x=temp, y=elev)) + theme_bw() + geom_boxplot() + facet_wrap(~prov, scales="free")

ggplot(data=data_new, aes(x=temp, y=elev, color=prov)) + theme_bw() + geom_point() + geom_smooth(method="lm")


MOD.air = lm(temp~elev, data=data_new[which(data_new$prov == "AIR"),])
MOD.soil = lm(temp~elev, data=data_new[which(data_new$prov == "SOIL"),])

par(mfrow=c(2,2))
plot(MOD.air)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(MOD.soil)
par(mfrow=c(1,1))




