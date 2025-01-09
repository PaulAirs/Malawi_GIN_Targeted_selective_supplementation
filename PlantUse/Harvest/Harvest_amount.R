##############################################################
# Comparison of supplementation events BETWEEN PLANT SPECIES #
##############################################################
library(tidyverse)
library(readr)

ThemeEM_Facet <- theme_bw() +
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size=14, face = "bold"),
        axis.title.y = element_text(size=14, face = "bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(size=14, face = "bold"),
        legend.text = element_text(size=12),
        panel.grid.major = element_line(colour = "light grey", size = 0.2),
        panel.grid.minor = element_line(colour = "NA", size = 0.1),
        strip.background = element_rect(color="black", fill="NA", size=0.5, linetype="solid"))

# Stat tests plant use
Supp <- as.table(rbind(
  c(286,	77),
  c(838,	89),
  c(367,	33)))
categories <- c("Healthy", "Supplemented")
plantnames <- c("C_africana", "F_ingens", "G_arborea")
dimnames(Supp) <- list(Plant_Names = plantnames, FPC_result = categories)
Supp

install.packages('chisq.posthoc.test')
library(chisq.posthoc.test)
chisq.test(Supp)
chisq.posthoc.test(Supp)

install.packages("RVAideMemoire")
library(RVAideMemoire)
chisq.multcomp(Supp, p.method = "bonferroni")

# Plotting plant use - Figure S6
install.packages('ggpubr')
library(ggpubr)

Total_Harvest <- read_csv("Total_Plant_Harvest.csv")
Harvest <- read_csv("Supplementation_Harvest_FullMonth.csv")
Harvest$Month_Year <-factor(Harvest$Month_Year, levels = c("January2020", "February2020", "March2020", "April2020","May2020", "June2020","July2020","August2020","September2020","October2020","November2020","December2020", "January2021", "February2021", "March2021"))

b <- ggplot(Total_Harvest, aes(x = Plant, y = Count, fill = Harvest_Status))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(legend.position = "none")

a <- ggplot(Total_Harvest, aes(x = Plant, y = Count, fill = Harvest_Status))+
  geom_bar(stat = "identity") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(legend.position = "above") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())

d <- 
  ggplot(Harvest, aes(x = Month_Year, y = Harvest_Weight, fill = Plant))+
  geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  facet_grid(cols = vars(Plant_Local))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank())

c <- 
  ggplot(Harvest, aes(x = Month_Year, y = Harvest_Weight, fill = Plant))+
  geom_bar(stat = "identity")+
  facet_grid(col = vars(Plant_Local))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(legend.position = "above")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())

ggarrange(a, c, b, d,
          labels = c("a", "c", "b", "d"),
          ncol = 2, nrow = 2, widths = c(1, 5))