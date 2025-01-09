#######################################################
# Availability of plants from prior survey - Figure 2 #
#######################################################
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

# Plant Seasonal Availability - shown in Figure 2
library(tidyverse)
library(readr)
Plants <- read_csv("Plant_Availability_Survey_Responses.csv")
ggplot(Plants, aes(Percent, Name, fill=Availability)) +
  geom_col() +
  ThemeEM_Facet +
  xlab("Percent of responses") +
  ylab("Plant name (local)") +
  facet_grid(. ~ Season) +
  coord_cartesian(xlim = c(0, 105)) +
  scale_fill_grey() #scale_fill_manual(values=c('#E0EC89','#AED75D','#31655A')) +
  theme(legend.position = "top") +
  guides(fill=guide_legend(title="Seasonal availability"))
