#Calculate Median and 2*Median
landmark_AT <- fread("data/landmark_AT_w_index.csv")
landmark_ZRF <- fread("data/landmark_ZRF_w_index.csv")

#---------AT
AT_median <- landmark_AT %>%
  group_by(landmark_index, stype) %>%
  summarise(median = median(r, na.rm = TRUE), median_2 = 2*median(r, na.rm = TRUE)) %>%
  mutate(median = ifelse(is.na(median)==TRUE, 0, median),
         median_2 = ifelse(is.na(median_2)==TRUE, 0, median_2))
fwrite(AT_median, "data/AT_median.csv")

#---------ZRF

ZRF_median <- landmark_ZRF %>%
  group_by(landmark_index, stype) %>%
  summarise(median = median(r, na.rm = TRUE), median_2 = 2*median(r, na.rm = TRUE)) %>%
  mutate(median = ifelse(is.na(median)==TRUE, 0, median),
         median_2 = ifelse(is.na(median_2)==TRUE, 0, median_2))
fwrite(ZRF_median, "data/ZRF_median.csv")

#--------JOIN
landmark_AT_median <- landmark_AT %>%
  left_join(AT_median, by=c("landmark_index", "stype")) %>%
  mutate(r_new = ifelse(is.na(r)==TRUE, median, r),
         r2_new = ifelse(is.na(r)==TRUE, median_2, r)) %>%
  arrange(landmark_index, sample_index)
fwrite(landmark_AT_median, "data/landmark_AT_median.csv")

landmark_ZRF_median <- landmark_ZRF %>%
  left_join(ZRF_median, by=c("landmark_index", "stype")) %>%
  mutate(r_new = ifelse(is.na(r)==TRUE, median, r),
         r2_new = ifelse(is.na(r)==TRUE, median_2, r))
fwrite(landmark_ZRF_median, "data/landmark_ZRF_median.csv")

#----------------------------------------------------------
landmark_AT_filled_w_median <- fread("data/landmark_ZRF_w_index.csv")
landmark_AT_filled_w_median <- landmark_AT_filled_w_median %>%
  arrange(landmark_index, sample_index)

one <- landmark_AT_filled_w_median[, c(2, 12,10, 9,8)]
one$r_new <- landmark_AT_median$r_new
one <- one %>%
  mutate(equal = ifelse(r == r_new, 1, 0))
test <- as.data.frame(test)
