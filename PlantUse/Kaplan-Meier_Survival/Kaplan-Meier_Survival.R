################################################################
# Kaplan-Meier survival plots from supplementation - Figure S7 #
################################################################

library(survival)
library(ggplot2)
library(ggpubr)
install.packages('survminer')
library(survminer)
library(lubridate)
library(Rcpp)
library(writexl)
library(carData)
library(glmmTMB)
library(broom)
library(readr)
surv2021 <- read_csv("surv2021.csv")
fitp<- survfit(Surv(Time, Status)~ TST, data = surv2021)
ggsurvplot(fitp, data = surv2021,
           conf.int = TRUE,
           pval = TRUE,
           fun = "pct",
           risk.table = TRUE,
           size = 1,
           linetype = "strata",
           palette = c("#000000",
                       "#009771"),
           legend = "bottom",
           legend.title = "TST",
           legend.labs = c("Drug",
                           "Plant"))
res.sum <- surv_summary(fitp)
head(res.sum)

plantx5<- survfit(Surv(Time, Status)~TST +plantx3, data = surv2021)
ggsurv <- ggsurvplot(plantx5, fun = "pct", conf.int = TRUE, pval = TRUE,palette = c("#000000",
                                                                                    "#009771","#009771","#009771","#009771","#009771"),
                     ggtheme = theme_bw())
ggsurv$plot +theme_bw() + 
  theme (legend.position = "bottom")+
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
  facet_grid(~TST+plantx3)

plantx1<- survfit(Surv(Time, Status)~ TST + plant, data = surv2021)
ggsurv <- ggsurvplot(plantx1, fun = "pct", conf.int = TRUE, pval = TRUE,palette = c("#000000",
                                                                                    "#438C3E","#8AC53F"),
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() + 
  theme (legend.position = "bottom")+
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_continuous(breaks=c(0,50,100,150,200,250,300,350,400))+
  facet_grid(~ TST +plant)
