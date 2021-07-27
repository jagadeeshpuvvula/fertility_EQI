library(tidyverse)
library(broom)
library(stringi)
library(lm.beta)
library(lme4)

# full data 200-2005 & 2006-2010
dat <- read_csv("C:/Users/jagad/Desktop/fertility_working/EQI_data/eqi_fer_Jul142021_fin.csv")
dat$lg_fer_rate<- log(dat$Avg_fer_rate_pct)

dat[,c(1:3)]<- lapply(dat[,c(1:3)], factor)
str(dat)

dat %>% ggplot(aes(x=Avg_fer_rate_pct, fill=year))+
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"))+
  xlab("Average fertility rate (%) per county")+
  #stat_bin(bins = 20)+
  theme(legend.position = "bottom")

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


dat1<- dat %>% filter(year == "2006_10")

#Univariate analysis - linear model - looping indipendent variables

# Creating pedictor and outcome vectors
ind_vec <- names(dat1)[c(4:9)]
dep_vec <- names(dat1)[13]

# Creating formulas and running the models
indVar <- paste0(" ~ ", ind_vec)
ind_dep <- unlist(map(indVar, ~paste0(dep_vec, .x))) #formula combinations
#model building
models <- map(setNames(ind_dep, ind_dep),
              ~ lm(formula = as.formula(.x),
                   data = dat1)) 
#extract model summary
model_summary <- map(models, ~ broom::tidy(.)) %>%
  map2_df(.,
          names(.),
          ~ mutate(.x, which_dependent = .y)) %>%
  select(which_dependent, everything()) %>%
  mutate(term = gsub("\\(Intercept\\)", "Intercept", term),
         which_dependent = stringi::stri_extract_first_words(which_dependent))

model_summary$std_estimate <-
  map_dfr(models, ~ coef(lm.beta::lm.beta(.)), .id = "which_dependent") %>%
  pivot_longer(.,
               cols = -which_dependent,
               names_to = "term",
               values_to = "std_estimate",
               values_drop_na = TRUE) %>%
  pull(std_estimate)

dat2_summ<- model_summary %>% filter(term != "Intercept")

#https://bbolker.github.io/morelia_2018/notes/mixedlab.html
#Mixed effect model
mod2<- lmer(Avg_fer_rate_pct~air+(1|County), data=dat1,
            control=lmerControl(check.nobs.vs.nlev = "ignore",
                                check.nobs.vs.rankZ = "ignore",
                                check.nobs.vs.nRE="ignore"))
summary(mod2)

