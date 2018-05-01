names(wildtype_101_at) <- c("Id", "x", "y", "z","a", "r", "t")
cyc_wild101at <- wildtype_101_at[,c(1, 5:7)]

max(cyc_wild101at$a)
73.47302
min(cyc_wild101at$a)
-73.79273
range(cyc_wild101at$a)
73.79273+73.47302
# 147.2658
147.2658/50

# r range
range(cyc_wild101at$r)

# Divide all the points into 50 groups 
library(Hmisc)
#split(cyc_wild101at, cut2(cyc_wild101at$a, g=2))
cyc_wild101at$group <- as.numeric(cut(cyc_wild101at$a, 50))
data("ZRF_landmarks_1-24-18")

library(ggplot2)
library(dplyr)
cyc_wild101at_1 <- cyc_wild101at %>%
  filter(group == 1 )
qplot(cyc_wild101at_1$r, 
      geom="histogram", 
      main = "Group One",
      xlab = "r",
      binwidth = 0.1,
      bins = 50, xlim=c(-3.2,3.2)) 

cyc_wild101at_2 <- cyc_wild101at %>%
  filter(group == 2)
qplot(cyc_wild101at_2$r, 
      geom="histogram", 
      main = "Group One",
      xlab = "r",
      binwidth = 0.1,
      bins = 50, xlim=c(-3.2,3.2)) 

cyc_wild101at_3 <- cyc_wild101at %>%
  filter(group == 3)
qplot(cyc_wild101at_3$r, 
      geom="histogram", 
      main = "Group One",
      xlab = "r",
      binwidth = 0.1,
      bins = 50, xlim=c(-3.2,3.2)) 

cyc_wild101at_49 <- cyc_wild101at %>%
  filter(group == 49)
qplot(cyc_wild101at_49$r, 
      geom="histogram", 
      main = "Group One",
      xlab = "r",
      binwidth = 0.1,
      bins = 50, xlim=c(-3.2,3.2)) 

cyc_wild101at_50 <- cyc_wild101at %>%
  filter(group == 50)
qplot(cyc_wild101at_50$r, 
      geom="histogram", 
      main = "Group One",
      xlab = "r",
      binwidth = 0.1,
      bins = 50, xlim=c(-3.2,3.2)) 

