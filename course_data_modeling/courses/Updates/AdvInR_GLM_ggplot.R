
library(ggplot2)
library(dplyr)


#### Species ####

SPECIES<-read.csv("species.csv")

SPECIES$PH = factor(SPECIES$PH,levels(SPECIES$PH)[c("low","mid","high")])

MOD.1<-glm(RICHNESS~BIOMASS*PH,data=SPECIES,family=poisson)

summary(MOD.1)

df_sp_pred <- data_frame(BIOMASS = rep(seq(0,10,length=100), times = 3),
                         PH = rep(c("low", "mid", "high"), each = 100))

head(df_sp_pred)
tail(df_sp_pred)

YVH <- predict(MOD.1,list(PH=df_sp_pred$PH,
                          BIOMASS=df_sp_pred$BIOMASS),
               type="link",
               se.fit=TRUE)

df_sp_pred <- cbind(df_sp_pred,
                    YVH)

ggplot(df_sp_pred, aes(x = BIOMASS, y = exp(fit),
                       group = PH)) +
  geom_line(aes(colour = PH)) +
  geom_ribbon(aes(ymin = exp(fit - 1.96*se.fit),
                  ymax = exp(fit + 1.96*se.fit)),
              alpha = 0.2) +
  geom_point(data = SPECIES, aes(x = BIOMASS,
                                 y = RICHNESS,
                                 colour = PH)) +
  labs(x = "Biomass",
       y = "Species richness") +
  theme_classic()




