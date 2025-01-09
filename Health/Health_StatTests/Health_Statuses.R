# Tests comparing overall health statuses between groups and on-field decisions
# Health statuses
library(tidyverse)
library(readr)

# Time sick stat
library(tidyverse)
library(readr)
Time_Sick_Box <- read_csv("Time_Sick_Box.csv")
kruskal.test(Time_Healthy ~ Group, data = Time_Sick_Box)
#b <- ggplot(Time_Sick_Box, aes(x = Group, y = Time_Healthy)) +
#  geom_boxplot() + theme_minimal()
#b

# ChiSquare for all goats
Health_Status_All_ChiSq <- read_csv("Health_Status_All_ChiSq.csv")
Health_Status_All_ChiSq <- Health_Status_All_ChiSq %>% remove_rownames %>% column_to_rownames(var="Group")
chi_square_test <- chisq.test(Health_Status_All_ChiSq)
print(chi_square_test)

# ChiSquare for treated goats
Health_Status_Trt_ChiSq <- read_csv("Health_Status_Trt_ChiSq.csv")
Health_Status_Trt_ChiSq <- Health_Status_Trt_ChiSq[1:2,1:3] %>% remove_rownames %>% column_to_rownames(var="Group")
chi_square_test <- chisq.test(Health_Status_Trt_ChiSq)
print(chi_square_test)

# ChiSquare for BCS status goats labelled as sick
BSC_sick <- matrix(c(10, 33,1461, 1547), nrow = 2)
result <- chisq.test(BCS_sick)
print(result)

# Comparison of all health status between FPC and on Field/Farmer decisions
install.packages("vcd")
library("vcd")
# Table of all treatment decisions
Health <- as.table(rbind(
  c(2517, 68, 2), c(8, 358, 3),
  c(15, 9, 71)))
categories <- c("Healthy", "Borderline", "Sick")
dimnames(Health) <- list(Field_result = categories, FPC_result = categories)
Health

# Compute kappa
res.k <- Kappa(Health)
res.k
# Confidence intervals
confint(res.k, level = 0.95)
print(res.k, CI = TRUE)

# Checking result as Cramer V
install.packages('rcompanion')
library(rcompanion)
cramerV(Health, ci = TRUE)

