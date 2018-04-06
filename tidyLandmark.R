library(readr)
library(dplyr)
library(tidyr)

AT_dataset <- read.csv("~/Desktop/SDS-Capstone-Zebrafish/data/AT_landmarks_3-28-18.csv", header=T, check.names = FALSE)
ZRF_dataset <- read.csv("~/Desktop/SDS-Capstone-Zebrafish/data/ZRF_landmarks_3-29-18.csv", header=T, check.names = FALSE)

gatherAT <- AT_dataset %>%
  gather("colname", "values", -Index)

separateAT <- gatherAT[1: 23712,] %>%
  separate(col = "colname", 
           into=c("min_alpha", "max_alpha", "min_theta", "max_theta", "num", "ptsOrR"),
           sep = "_"
  )

spreadAT <- separateAT %>%
  group_by(ptsOrR, Index) %>%
  mutate(ind = row_number()) %>%
  spread(ptsOrR, values)

gatherZRF <- ZRF_dataset %>%
  gather("colname", "values", -Index)

#separateZRF <- gatherZRF[1: 16720,] %>%
  #separate(col = "colname", 
    #       into=c("min_alpha", "max_alpha", "min_theta", "max_theta", "num", "ptsOrR"),
     #      sep = "_"
  #)

separateZRF <- gatherZRF[1: 23712,] %>%
  separate(col = "colname", 
           into=c("min_alpha", "max_alpha", "min_theta", "max_theta", "num", "ptsOrR"),
           sep = "_"
  )


spreadZRF <- separateZRF %>%
  group_by(ptsOrR, Index) %>%
  mutate(ind = row_number()) %>%
  spread(ptsOrR, values)

stypeData_ZRF <- gatherZRF[23713: 23790,]
stypeData_AT <- gatherAT[23713: 23790,]

resultAT <- left_join(spreadAT, stypeData_AT, by="Index")
resultAT$colname <- NULL
resultAT$ind <- NULL
colnames(resultAT)[colnames(resultAT) == 'values'] <- 'stype'

resultZRF <- left_join(spreadZRF, stypeData_ZRF, by="Index")
resultZRF$colname <- NULL
resultZRF$ind <- NULL
colnames(resultZRF)[colnames(resultZRF) == 'values'] <- 'stype'



res <- left_join(try3, stypeData, by="Index")
res$colname <- NULL
res$ind <- NULL
colnames(res)[colnames(res) == 'values'] <- 'stype'

write.csv(resultZRF, file="~/Desktop/tidyLandmarks_ZRF_new.csv")

write.csv(resultAT, file="~/Desktop/tidyLandmarks_AT_new.csv")
