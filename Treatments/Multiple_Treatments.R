library(tidyverse)
library(readr)
library(lubridate)
library(RColorBrewer)
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

# Total Treatments plots
totalah <- read_csv("Anthelmintic_treatments.csv", 
                    col_types = cols(Treatments = col_factor(levels = c("0","1", "2", "3", "4", "5", "6"))))
ggplot(data = totalah,aes(x=Treatments, y= Proportion, fill=Group))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values = c('control-TST' = "#000000", 'plant-TST' = "#06A64F"),name="Group:",
                    breaks=c("control-TST", "plant-TST"),
                    labels=c("control-TST", "plant-TST"))+
  labs(x = "Total Drug Interventions")+
  ThemeEM_XY

ggplot(data = totalah,aes(x=Group, y= Proportion, fill=Treatments))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_brewer(palette = "Oranges")+
  ThemeEM_XY

# Consecutive Treatments Plot
Consecu <- read_csv("Consecutive_treatments.csv",
                    col_types = cols(Consecutive = col_factor(levels = c("0","1", "2", "3"))))

ggplot(data = Consecu,aes(x=Consecutive, y= Proportion, fill=Group))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values = c('control-TST' = "#000000", 'plant-TST' = "#06A64F"),name="Group:",
                                                                  breaks=c("control-TST", "plant-TST"),
                                                                  labels=c("control-TST", "plant-TST"))+
  labs(x = "Consecutive Drug Interventions")+
  ThemeEM_XY

ggplot(data = Consecu,aes(x=Group, y= Proportion, fill=Consecutive))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_brewer(palette = "Oranges")+
  ThemeEM_XY
