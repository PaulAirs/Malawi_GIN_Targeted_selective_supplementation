library(glmmTMB)
library(bbmle)
library(DHARMa)
library(emmeans)
library(carData)
library(car)
library(lubridate)
library(MASS)
library(multcomp)
library(ggplot2)
library(tibble)
library(data.table)
library(ggpubr)
library(rstatix)
library(writexl)
library(readr)
FEC21 <- read_csv("FEC21.csv", col_types = cols(Tag = col_character(), 
                                                Age = col_character(), Sex = col_character(), 
                                                Nasal = col_character(), Jaw = col_character(), 
                                                FAMACHA = col_character(), Condition = col_character(), 
                                                Dag = col_character(), Weight = col_number(), 
                                                Visit = col_character(), FEC = col_number()))
FEC21$Date <-dmy(FEC21$Date)
FEC21$year <- year(ymd(FEC21$Date))
FEC21$time2 <- paste(FEC21$month,FEC21$year)
FEC21$time2 <-factor(FEC21$time2, levels = c("January 2020", "February 2020", "March 2020", "April 2020","May 2020", "June 2020","July 2020","August 2020","September 2020","October 2020","November 2020","December 2020", "January 2021", "February 2021", "March 2021"))
FEC21$Status <-factor(FEC21$Status, levels = c("Healthy", "Borderline", "Sick"))
FEC21$farm <-factor(FEC21$farm, levels = c("D1", "D2", "D3", "D4", "D5", "D6", "D7","D8","D9","D10","D11", "D12","D13","D14","D15", "D16", "D17", "D18", "D25", "D26","D27", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8","P9", "P10", "P11", "P12", "P13", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24"))
### ###Main effects model AIC34306
p1<-glmmTMB(FEC~TST+Nasal+FAMACHA+Condition+Dag+Jaw+(1|Tag),FEC21,ziformula = ~Nasal+Dag+Jaw, family=poisson)
n1<-glmmTMB(FEC~TST+Nasal+FAMACHA+Condition+Dag+Jaw+(1|Tag),FEC21,ziformula = ~1, family=nbinom2)
nb1<-glmmTMB(FEC~TST+Nasal+FAMACHA+Condition+Dag+Jaw+(1|Tag),FEC21,ziformula = ~Nasal+Dag+Jaw, family=nbinom1)
AICtab(p1, n1, nb1)
Anova(n1)
summary(n1)

####Best Model
n6<-glmmTMB(FEC~FAMACHA + Condition + Dag + time2 + Status+ Lactation +  (1|Tag), data=FEC21,ziformula = ~1, family=nbinom2)
summary(n6)
Anova(n6)

###FAMACHA
e1<-emmeans(n6, pairwise~FAMACHA,type= "response",adjust="Tukey")
multcomp::cld(e1$emmeans, alpha = 0.05, Letters = LETTERS,reversed= F)
e1
###BCS
e2<-emmeans(n6, pairwise~Condition,type= "response",adjust="Tukey")
multcomp::cld(e2$emmeans, alpha = 0.05, Letters = LETTERS,reversed= F)
e2
##DAG
e3<-emmeans(n6, pairwise~Dag,type= "response",adjust="Tukey")
multcomp::cld(e3$emmeans, alpha = 0.05, Letters = LETTERS,reversed= F)
####TIME
e4<-emmeans(n6, pairwise~time2,type= "response",adjust="Tukey")
multcomp::cld(e4$emmeans, alpha = 0.05, Letters = LETTERS,reversed= T)
e4
#STATUS
e5<-emmeans(n6, pairwise~Status,type= "response",adjust="Tukey")
multcomp::cld(e5$emmeans, alpha = 0.05, Letters = LETTERS,reversed= F)
###LACTATION
e6<-emmeans(n6, pairwise~Lactation,type= "response",adjust="Tukey")
multcomp::cld(e6$emmeans, alpha = 0.05, Letters = LETTERS,reversed= F)
