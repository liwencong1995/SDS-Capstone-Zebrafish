library(data.table)
library(dplyr)

#--------------------------------AT---------------------------------
#read in data to add indicies
landmark_AT_raw <- fread("data/tidyLandmarks_AT_updated.csv")
landmark_AT_raw <- landmark_AT_raw %>%
  select(-V1) 
str(landmark_AT_raw)
landmark_AT_raw_index <- landmark_AT_raw %>%
  mutate(unique_key = paste0(min_alpha, "/", min_theta)) %>%
  mutate(min_alpha = as.numeric(min_alpha),
         max_alpha = as.numeric(max_alpha),
         min_theta = as.numeric(min_theta),
         max_theta = as.numeric(max_theta)) %>%
  arrange(min_alpha, min_theta) %>%
  group_by(min_alpha, min_theta)

#assign indices  
str(landmark_AT_raw_index)
landmark_AT_w_index <- transform(landmark_AT_raw_index, id = match(unique_key, unique(unique_key)))
landmark_AT_w_index <- landmark_AT_w_index %>%
  rename(sample_index = Index,
         landmark_index = id)
fwrite(landmark_AT_w_index, "data/landmark_AT_w_index.csv")

#--------------------------------ZRF---------------------------------
#read in data to add indicies
landmark_ZRF_raw <- fread("data/tidyLandmarks_ZRF_updated.csv")
landmark_ZRF_raw <- landmark_ZRF_raw %>%
  select(-V1) 
#str(landmark_ZRF_raw)
landmark_ZRF_raw_index <- landmark_ZRF_raw %>%
  mutate(unique_key = paste0(min_alpha, "/", min_theta)) %>%
  mutate(min_alpha = as.numeric(min_alpha),
         max_alpha = as.numeric(max_alpha),
         min_theta = as.numeric(min_theta),
         max_theta = as.numeric(max_theta)) %>%
  arrange(min_alpha, min_theta) %>%
  group_by(min_alpha, min_theta)

#assign indices  
#str(landmark_ZRF_raw_index)
landmark_ZRF_w_index <- transform(landmark_ZRF_raw_index, id = match(unique_key, unique(unique_key)))
landmark_ZRF_w_index <- landmark_ZRF_w_index %>%
  rename(sample_index = Index,
         landmark_index = id)
fwrite(landmark_ZRF_w_index, "data/landmark_ZRF_w_index.csv")



#--------------------------------------------------------------------
#read in data needed to create vis
r1_AT <- fread("results/r1_AT.csv")
r1_ZRF <- fread("results/r1_ZRF.csv")
landmark_label_raw <- fread("data/landmark_AT_w_index_no_na.csv")

landmark_AT <- landmark_label_raw%>%
  arrange(sample_index) %>%
  select(-unique_key, -V1)

landmark_AT_test <- landmark_AT %>%
  group_by(sample_index, min_alpha, min_theta)%>%
  summarise(N=n())
#passed the test

test_num_sample <- landmark_AT%>%
  group_by(landmark_index, sample_index) %>%
  summarise(N=n()) %>%
  filter(N > 1)
landmark_AT_cleaned <- unique(landmark_AT[ , 3:10 ] )

#--------------------------------AT---------------------------------
#label each landmark with (x,y) format
# x = row number
#y = column number
landmark_label_AT <- landmark_AT %>%
  group_by(landmark_index) %>%
  summarise(min_alpha = mean(min_alpha),
            max_alpha = mean(max_alpha),
            min_theta = mean(min_theta),
            max_theta = mean(max_theta),
            num = mean(num))
# add x label
landmark_label_AT <- landmark_label_AT %>% 
  group_by(min_alpha) %>% mutate(x = row_number())
landmark_label_AT <- landmark_label_AT %>% 
  group_by(min_theta) %>% mutate(y = row_number())
# double check that there are 8 landmarks in each alpha slice
test_AT_x <- landmark_label_AT %>%
  group_by(y) %>%
  summarise(N = n())
max(test_AT_x$N)
min(test_AT_x$N)
# double check that there are 30 landmarks in each theta range
test_AT_y <- landmark_label_AT %>%
  group_by(x) %>%
  summarise(N = n())
max(test_AT_y$N)
min(test_AT_y$N)
# there are only 19 landmarks in each theta range

# Output landmark label
landmark_label_AT_output <- landmark_label_AT[, c(1,7:8)]
fwrite(landmark_label_AT_output, "analysis/landmark_xy.csv")  
