#Calculate Median and 2*Median
landmark_AT <- fread("data/landmark_AT_w_index.csv")
landmark_ZRF <- fread("data/landmark_ZRF_w_index.csv")

AT_median <- landmark_AT %>%
  group_by(landmark_index, stype) %>%
  summarise(median = median(r, na.rm = TRUE), median_2 = 2*median(r, na.rm = TRUE))
fwrite(AT_median, "data/AT_median.csv")

# test <- AT_median %>%
#   filter(median != "NA")
# 
# test_2 <- landmark_AT %>%
#   filter(pts != 0) %>%
#   group_by(landmark_index, stype) %>%
#   summarise(N = n())


ZRF_median <- landmark_ZRF %>%
  group_by(landmark_index, stype) %>%
  summarise(median = median(r, na.rm = TRUE), median_2 = 2*median(r, na.rm = TRUE))
fwrite(ZRF_median, "data/ZRF_median.csv")


