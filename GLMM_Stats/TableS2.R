library(glmmTMB)
library(bbmle)
library(DHARMa)
library(emmeans)
library(carData)
library(car)
library(lubridate)
library(ggplot2)
library(writexl)
library(readr)
FEC2 <- read_csv("FEC2.csv", col_types = cols(Tag = col_character(), 
                                                Age = col_character(), Nasal = col_character(), 
                                                Jaw = col_character(), FAMACHA = col_character(), 
                                                Condition = col_character(), Dag = col_character(), 
                                                Visit = col_character()))
FEC2$Date <-dmy(FEC2$Date)
FEC2$year <- year(ymd(FEC2$Date))
FEC2$time2 <- paste(FEC2$month,FEC2$year)
FEC2$time2 <-factor(FEC2$time2, levels = c("January 2020", "February 2020", "March 2020", "April 2020","May 2020", "June 2020","July 2020","August 2020","September 2020","October 2020","November 2020","December 2020", "January 2021", "February 2021", "March 2021"))
FEC2$Status <-factor(FEC2$Status, levels = c("Healthy", "Borderline", "Sick"))
##testing normality
shapiro.test(FEC2$Weight)
ggqqplot(FEC2$Weight)
qqPlot(FEC2$Weight)
ks.test(FEC2$Weight)
ks.test(FEC2$Weight)
#####Weights###
##Poisson
wp0<-glmmTMB(Weight~Nasal+FAMACHA+Condition+Dag+Jaw+(1|Tag),FEC2,family=poisson)
## Zero-inflated negative binomial model
nb0<-glmmTMB(Weight~Nasal+FAMACHA+Condition+Dag+Jaw+(1|Tag),FEC2, family=nbinom2)
###Negative binomial distribution
n0<-glmmTMB(Weight~Nasal+FAMACHA+Condition+Dag+Jaw+(1|Tag),FEC2, family=nbinom1)
##Gaussian
g0<-glmmTMB(Weight~Nasal+FAMACHA+Condition++(1|Tag),FEC2, family = gaussian)
summary(g0)
Anova(g0)
##Best model
g17<-glmmTMB(Weight~Nasal + FAMACHA + Condition + Status + Lactation + time2 +Age + Location + (1|Tag), FEC2, family = gaussian)
summary(g17)
Anova(g17)
##plotting simple effects
###Plotting FAMACHA
j1<-emmeans(g17, pairwise~FAMACHA,type="response",adjust="Tukey")
summary(j1)
multcomp::cld(j1$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
##Plotting condition
j2<-emmeans(g17, pairwise~Condition,adjust="Tukey",type="response")
summary(j2)
multcomp::cld(j2$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
##Plotting Lactation
j3<-emmeans(g17, pairwise~Lactation,type="response",adjust="Tukey")
summary(j3)
multcomp::cld(j3$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
###Plotting time
j4<-emmeans(g17, pairwise~time2,type="response",adjust="Tukey")
summary(j4)
multcomp::cld(j4$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
###Plotting Age
j5<-emmeans(g17, pairwise~Age,type="response",adjust="Tukey")
summary(j5)
multcomp::cld(j5$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
###Plotting Location
j6<-emmeans(g17, pairwise~Location,type="response",adjust="Tukey")
summary(j6)
multcomp::cld(j6$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
###Plotting Status
j8<-emmeans(g17, pairwise~Status,type="response",adjust="Tukey")
summary(j8)
multcomp::cld(j8$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
###Plotting Nasal discharge
j9<-emmeans(g17, pairwise~Nasal,type="response",adjust="Tukey")
summary(j9)
multcomp::cld(j9$emmeans, alpha = 0.05, Letters = LETTERS,reversed=F)
