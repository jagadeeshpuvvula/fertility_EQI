library(epiDisplay)
library(dplyr)
library(MASS)
library(lme4)
library(tidyverse)
library(corrplot)

# 2000 - 2005 dataset
dat1 <- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/EQI_2000_2005/EQI_fertility.csv")

#2005 - 2010 dataset
dat2 <- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/EQI_2006_2010/EQI_fertility_wavetwo.csv")


str(dat)

#correlation plot
dat.cor1<-dat1[5:11]
M1<-cor(dat.cor1)

dat.cor2<-dat2[-1]
M2<-cor(dat.cor2)

corrplot(M2, method="color",  
         type="upper", order="original", addrect = 2,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.05, insig = "pch", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)


dat$cat_rucc<- as.factor(dat$cat_rucc)

x<- lm(LB~air_EQI_22July2013+cat_rucc, data=dat)
summary(x)
plot(x)
