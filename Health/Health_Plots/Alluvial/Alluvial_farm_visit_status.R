######################################################################
# Alluvial goat status over farm visit time / not season - Figure S5 #
######################################################################

library(readr)
library(ggplot2)
library(ggalluvial)
library(scales)
library(ggpubr)
library(lubridate)
library(readr)
allup <- read_csv("allup.csv", col_types = cols(Tag = col_integer()))
allup$Status <-factor(allup$Status, levels = c("Sick","Borderline", "Healthy", "Grazing","Sold","Stolen" ,"Died", "End"))
ggplot(allup,
       aes(x = Visit, stratum = Status, alluvium = Tag,
           fill = Status)) +
  geom_lode() + geom_flow() +
  geom_stratum(stat = "stratum", alpha = 2) +
  facet_grid(TST ~.) +
  labs("Proportion of individual goats assessed", x="Visit number (1 visit every 2 weeks")+
  scale_x_continuous(limits = c(0,30),breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))+
  geom_text(stat="stratum",size=3, aes(label = round(after_stat(prop), 2))) + ThemeEM_XY+
  scale_fill_manual(values = c(Sick = "#D61F26", Borderline = "#FFCB05",
                               Healthy = "#DBE8F7",Grazing = "#009771", Sold = "#DEC1A3", Stolen = "#D3226A", Died= "#642E91", End="#808080"))