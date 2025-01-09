# Seaonal FPC scores across group - shown in Figure S4

library(dplyr)
library(readr)
library(lubridate)
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

# FAMACHA
d2 <- read_csv("d2.csv")
d2$time2 <-factor(d2$time2, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
d2$FAMACHA <-factor(d2$FAMACHA, levels = c("1","2","3","4","5"))
fam<-ggplot(d2, aes(x = factor(time2), y = perc*100, fill = factor(FAMACHA))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Time", y = "FAMACHA (% score)", fill = "FAMACHA") + scale_fill_manual(values = c("#79CAF1", "#DBE8F7", "#FFCB05", "#D61F26", "#90191C")) +
  facet_grid(~TST)+ThemeEM_XY
fam

# BCS
d3 <- read_csv("d3.csv")
d3$time2 <-factor(d3$time2, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
d3$BCS <-factor(d3$BCS, levels = c("3.5","3","2.5","2","1.5","1"))
ggplot(d3, aes(x = factor(time2), y = perc*100, fill = factor(BCS))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Time", y = "BCS (% score)", fill = "BCS") +  scale_fill_manual(values = c("#0752A2", "#25ABE2", "#79CAF1", "#DBE8F7", "#FFCB05", "#D61F26","#90191C")) + facet_grid(~TST) + ThemeEM_XY

# Nasal score
d4 <- read_csv("d4.csv")
d4$time2 <-factor(d4$time2, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
d4$Nasal <-factor(d4$Nasal, levels = c("0","1"))
ggplot(d4, aes(fill=Nasal, y=count, x=time2)) +
  geom_bar(position="fill", stat="identity") + labs(x = "Time", y = "Nasal (% score)",fill = "Nasal")+ scale_fill_manual(values = c("#FFFFFF", "#FFCB05"))+ThemeEM_XY+facet_grid(~TST) 

# Dag / scour
d7 <- read_csv("d7.csv")
d7$time2 <-factor(d7$time2, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
d7$Dag <-factor(d7$Dag, levels = c("0", "1","2","3","4","5"))
dag<-ggplot(d7, aes(fill=Dag, y=count, x=time2)) +
  geom_bar(position="fill", stat="identity")+ labs(x = "Time", y = "Dag (% score)",fill = "Dag")+ scale_fill_manual(values = c("#FFFFFF","#79CAF1", "#DBE8F7", "#FFCB05", "#D61F26", "#90191C")) +ThemeEM_XY+facet_grid(~TST)
dag

# Bottle Jaw
d6 <- read_csv("d6.csv")
d6$time2 <-factor(d6$time2, levels = c("Jan-20", "Feb-20", "Mar-20", "Apr-20","May-20", "Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20", "Jan-21", "Feb-21", "Mar-21"))
d6$Jaw <-factor(d6$Jaw, levels = c("0", "1"))
ja<-ggplot(d6, aes(fill=Jaw, y=count, x=time2)) +
  geom_bar(position="fill", stat="identity")+ labs(x = "Time", y = "Percentage (%)",fill = "Jaw")+ scale_fill_manual(values = c("#FFFFFF", "#D61F26")) +ThemeEM_XY+facet_grid(~TST) 
ja