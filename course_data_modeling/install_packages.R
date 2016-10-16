#!/usr/local/bin/Rscript
packages = c("arm", "boot", "broom", "car", "dplyr", "ggplot2", "lme4", "MASS", "Matrix", "MuMIn", "nlme", "nlstools", "sjPlot", "tidyr", "visreg")
install.packages(packages, dependencies=TRUE)
print("Install Done.")

lapply(packages, require, character.only = TRUE)
print("Loading Done.")
