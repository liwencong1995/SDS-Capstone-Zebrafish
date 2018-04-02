library(data.table)
library(dplyr)
#read in data needed to create vis


r1_AT <- fread("results/r1_AT.csv")
r1_ZRF <- fread("results/r1_ZRF.csv")
landmark_AT <- fread("data/tidyLandmarks_AT_no_na.csv")
landmark_label_ZRF <- fread("data/tidyLandmarks_ZRF.csv")

landmark_AT <- landmark_AT%>%
  arrange(sample_index) %>%
  select(-landmark_index, -V1) %>%
  unique() 

landmark_AT_test <- landmark_AT %>%
  group_by(sample_index, min_alpha, min_theta)%>%
  summarise(N=n())
  

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

# Output landmark label for AT
landmark_label_AT <- landmark_label_AT %>%
  select(landmark_index, x, y)
landmark_label_AT <- landmark_label_AT[2:4]
fwrite(landmark_label_AT, "data/landmark_label_AT.csv")  

#--------------------------------ZRF---------------------------------
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

# Output landmark label for AT
landmark_label_AT <- landmark_label_AT %>%
  select(landmark_index, x, y)
landmark_label_AT <- landmark_label_AT[2:4]
fwrite(landmark_label_AT, "data/landmark_label_AT.csv")  
  
  