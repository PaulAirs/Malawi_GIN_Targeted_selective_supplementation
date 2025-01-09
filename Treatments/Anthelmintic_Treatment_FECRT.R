# Anthelmintic treatment FEC reduction - shown in Figure S2

library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)
redjul <- read_csv("redjul.csv")
redjul$Intervention <-factor(redjul$Intervention, levels = c("Pre", "Post"))
ggpaired(redjul, x = "Intervention", y = "FECp",
         color = "TST", line.color = "gray", line.size = 0.4, ylab = "FEC (EPG x 1000)", xlab = "Anthelmintic Intervention",
         palette = c("#000000", "#06A64F"),name="TST:",
         breaks=c("Drug", "Plant"),
         labels=c("TST-Drug", "TST-Plant & Drug")) +
  facet_wrap(~TST) +
  scale_y_continuous(breaks=c(0,250,500, 1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000))+
  stat_compare_means(paired = TRUE, vjust = 1)+
  ThemeEM_XY 