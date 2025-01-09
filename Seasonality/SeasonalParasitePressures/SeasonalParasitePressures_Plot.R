###########################################
# Seasonal parasite pressures - Figure S3 #
###########################################
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
ThemeEM_XY <- theme_bw() +
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size=14, face = "bold"),
        axis.title.y = element_text(size=14, face = "bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=14, face = "bold"),
        legend.text = element_text(size=12),
        panel.grid.major = element_line(colour = "NA", size = 0.2),
        panel.grid.minor = element_line(colour = "NA", size = 0.1),
        strip.background = element_rect(color="black", fill="NA", size=0.5, linetype="solid"),
        panel.border = element_rect(linetype = "blank", fill = NA),
        axis.line = element_line(size = 0.5, colour = "black"))

climalmean <- read_csv("climalmean.csv")
climalmean$Time <-factor(climalmean$Time, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
ggplot(data=climalmean, aes(x=Time, y=Mean, group=Climate, color = Climate)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1)+
  geom_line(aes(color = Climate))+
  geom_point(aes(colour = Climate)) +
  theme(legend.position="top")+
  scale_y_continuous("Temperature C", sec.axis = sec_axis(~ . , name = "Precipitation (mm)")) +
  ThemeEM_XY
d1 <- read_csv("d1.csv")
d1$time2 <-factor(d1$time2, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
ggplot(data=d1, aes(x=time2, y=FEC, fill = TST, color = TST)) +
  geom_bar(size = 1, stat="identity", position = "dodge")+
  labs(x = "Time", y = "FEC (EPG x 1000)")+
  facet_grid(~TST)+
  scale_y_continuous(breaks=c(0,250,500, 1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500))+
  ThemeEM_XY