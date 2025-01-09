# Deaths Chi-square
library(tidyverse)
library(readr)
Multi_Chi_square_Death <- read_csv("Multi_Chi_square_Death.csv")
chisq.test(Multi_Chi_square_Death)

## From Agresti(2007) p.39
M <- as.table(rbind(c(365, 96, 101), c(105, 12, 11)))
dimnames(M) <- list(Outcome = c("Adults_Owned", "Died"),
                    Group = c("Pre-survey","Plant-TST", "Control-TST"))
M
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
