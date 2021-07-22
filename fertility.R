library(epiDisplay)
library(dplyr)
library(MASS)
library(lme4)
library(tidyverse)
library(corrplot)

# full data 200-2005 & 2006-2010
dat <- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/eqi_fer_Jul142021_fin.csv")

dat[,c(1:3)]<- lapply(dat[,c(1:3)], factor)
str(dat)

#correlation plot
dat.cor1<-dat[4:12]
M1<-cor(dat.cor1)

corrplot(M1, method="color",  
         type="upper", order="original", addrect = 2,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.05, insig = "pch", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

#linear approach
dat1<- dat %>% filter(year == "2000_05")
fit<-lm(log(Avg_fer_rate_pct)~build, data = dat1)
summary(fit)
confint(fit, level = 0.95)
#[which(data_sens$case == 0),]

#Negative binomial approach
#2000-2005 data
fit <- glmer.nb(LB ~ build+soc_dem+air+water+land+(1|POP), 
                data=dat1)

IRR <- fixef(fit)
confnitfixed <- confint(fit, parm = "beta_", method = "Wald")
summary <- exp(cbind(IRR, confnitfixed))
summary

ggplot()+geom_pointrange(data=x, mapping=aes(x=x, 
                                             y=IRR,ymin=2.5, ymax=97.5), 
                         width=0.2, size=1, color="black", fill="black", shape=22)
