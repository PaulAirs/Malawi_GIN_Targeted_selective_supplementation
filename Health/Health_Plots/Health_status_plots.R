# GGvenn diagrams of goat health switching status
# Goats were identified by unique ear tag, null observations removed and goats <3 observations removed
# Shown in Figure S1 and Figure 3b - c

# Dependencies
install.packages("rlang")
install.packages('tidyverse')
install.packages("devtools")
library(devtools)
devtools::install_github("gaospecial/ggVennDiagram")
library(tidyverse)
library(ggVennDiagram)
library(readr)
library(lubridate)

# Figure 3C - Health switching
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
Switch <- read_csv("Status_Switch.csv")
Switch$Status <-factor(Switch$Status, levels = c("Healthy", "Borderline", "Sick"))
Switch$Treatment <-factor(Switch$Treatment, levels = c("Drug", "Plant+Drug", "Plant"))
ggplot(Switch, aes(x=Frequency, y=Treatment, fill=Status)) +
  geom_bar(stat="identity")+scale_fill_manual(values = c(Sick = "#D61F26", Borderline = "#FFCB05",
                                                         Healthy = "#DBE8F7"))+ThemeEM_XY+expand_limits(x = 60) 

# Venn diagrams data
ggvenn_goat_status2 <- read_csv("ggvenn_goat_status2.csv")
Plant_TST <- subset(ggvenn_goat_status2, Group == 'Plant-TST')
Control_TST <- subset(ggvenn_goat_status2, Group == 'Control-TST')

# Plot
ggVennDiagram(Plant_TST[2:4])  #+ scale_fill_gradient(low="white",high = "orange")
ggVennDiagram(Control_TST[2:4]) #+ scale_fill_gradient(low="white",high = "orange")

# Reason Sick
reason_sick <- read_csv("ggvenn_reason_sick.csv")
ggVennDiagram(reason_sick)+ scale_fill_gradient(low="white",high = "#D62027")

# Reason Borderline
reason_borderline <- read_csv("ggvenn_reason_borderline.csv")
ggVennDiagram(reason_borderline)+ scale_fill_gradient(low="white",high = "#FECA08")