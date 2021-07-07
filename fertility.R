#library(epiDisplay)
#library(dplyr)
#library(MASS)
#library(lme4)
library(tidyverse)
library(corrplot)
#library(outliers) for dixon test
library(EnvStats)


#Fertility rate = ([average live births per year]/population)*100

# 2000 - 2005 dataset
dat1 <- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/EQI_2000_2005/EQI_fertility.csv")

#Rosner test for excluding outliers (dixon test for n less than 30)
#https://statsandr.com/blog/outliers-detection-in-r/

boxplot(dat1$Avg_fer_rate_pct)
sum(dat1$Avg_fer_rate_pct>=27) #count of observations greater than 27

#rosner test did not work as the outliers keep popping even after filtering the top ones
test<- rosnerTest(dat1$Avg_fer_rate_pct, k=10) #rosner test
test

#based on the percentile cutoff filtered observations between 1st and 3rd quantile
dat1.work<-dat1 %>% filter(between(Avg_fer_rate_pct, 8, 27))

#count of observations by variable and bin size
dat1.work %>%
  count(cut_width(Avg_fer_rate_pct, 2))
#bar graph of count of observations by variable and bin size
ggplot(data = dat1.work) +
  geom_histogram(mapping = aes(x = Avg_fer_rate_pct), binwidth = 3)



#2005 - 2010 dataset
dat2 <- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/EQI_2006_2010/EQI_fertility_wavetwo.csv")

#combined
dat3<- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/Merged_EQI_fertility.csv")

str(dat2)

#correlation plot
dat.cor1<-dat1[2:9]
M1<-cor(dat.cor1)

dat.cor2<-dat2[2:9]
M2<-cor(dat.cor2)

dat.cor3<- dat3[3:9]
M3<- cor(dat.cor3)

corrplot(M3, method="color",  
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
