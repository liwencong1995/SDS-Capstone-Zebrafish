library(data.table)
library(dplyr)

#--------------------------------Adding new labels-------------------
landmark_AT <- fread("data/AT_landmarks_3-28-18.csv")
landmark_ZRF <- fread("data/ZRF_landmarks_3-29-18.csv")

landmark_AT_1 <- landmark_AT %>%
  mutate(Index = ifelse(Index == 112 & stype == "wt-at", "112_1", Index)) %>%
  mutate(Index = ifelse(Index == 112 & stype == "mt-at", "112_2", Index))

landmark_ZRF_1 <- landmark_ZRF %>%
  mutate(Index = ifelse(Index == 112 & stype == "wt-zrf", "112_1", Index)) %>%
  mutate(Index = ifelse(Index == 112 & stype == "mt-zrf", "112_2", Index))

fwrite(landmark_AT_1, "data/AT_landmarks.csv")
fwrite(landmark_ZRF_1, "data/ZRF_landmarks.csv")
#--------------------------------add indicies---------------------------
#--------------------------------AT---------------------------------
#read in data 
landmark_AT_raw <- fread("data/tidyLandmarks_AT_new.csv")
landmark_AT_raw <- landmark_AT_raw %>%
  select(-V1) 
str(landmark_AT_raw)
landmark_AT_raw_index <- landmark_AT_raw %>%
  mutate(unique_key = paste0(min_alpha, "/", min_theta)) %>%
  mutate(min_alpha = as.numeric(min_alpha),
         max_alpha = as.numeric(max_alpha),
         min_theta = as.numeric(min_theta),
         max_theta = as.numeric(max_theta)) %>%
  arrange(min_alpha, min_theta)

#assign indices  
#str(landmark_AT_raw_index)
landmark_AT_w_index <- transform(landmark_AT_raw_index, id = match(unique_key, unique(unique_key)))
landmark_AT_w_index <- landmark_AT_w_index %>%
  rename(sample_index = Index,
         landmark_index = id)
fwrite(landmark_AT_w_index, "data/landmark_AT_w_index.csv")

#--------------------------------ZRF---------------------------------
#read in data to add indicies
landmark_ZRF_raw <- fread("data/tidyLandmarks_ZRF_new.csv")
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

#----------------------------------landmark relative position----------------------------------
#--------------------------------AT---------------------------------
#read in data needed to create landmark labels
AT_2M <- fread("data/landmark_AT_filled_w_2median.csv")
AT_M <- fread("data/landmark_AT_filled_w_median.csv")
ZRF_2M <- fread("data/landmark_ZRF_filled_w_2median.csv")
ZRF_M <- fread("data/landmark_ZRF_filled_w_median.csv")

AT_2M <- unique(AT_2M[, 2:12])
AT_M <- fread("data/landmark_AT_filled_w_median.csv")
ZRF_2M <- fread("data/landmark_ZRF_filled_w_2median.csv")
ZRF_M <- fread("data/landmark_ZRF_filled_w_median.csv")

landmark_label_raw_AT <- fread("data/final/landmark_AT_filled_w_median.csv")

test <- landmark_label_raw_AT %>%
  group_by(sample_index, landmark_index) %>%
  summarise(N = n())

landmark_AT <- landmark_label_raw_AT%>%
  arrange(sample_index) %>%
  select(-unique_key, -V1)

# landmark_AT_test <- landmark_AT %>%
#   group_by(sample_index, min_alpha, min_theta)%>%
#   summarise(N=n())
# #passed the test
# 
# test_num_sample <- landmark_AT%>%
#   group_by(landmark_index, sample_index) %>%
#   summarise(N=n()) %>%
#   filter(N > 1)
landmark_AT_cleaned <- unique(landmark_AT[ , c(1,10)] )
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
# Output landmark label
landmark_label_AT_output <- landmark_label_AT[, c(1,2,4,7:8)]
fwrite(landmark_label_AT_output, "analysis/landmark_xy.csv") 

#--------------------------------ZRF---------------------------------
# # double check that there are 8 landmarks in each alpha slice
# test_AT_x <- landmark_label_AT %>%
#   group_by(y) %>%
#   summarise(N = n())
# max(test_AT_x$N)
# min(test_AT_x$N)
# # double check that there are 30 landmarks in each theta range
# test_AT_y <- landmark_label_AT %>%
#   group_by(x) %>%
#   summarise(N = n())
# max(test_AT_y$N)
# min(test_AT_y$N)
# # there are only 19 landmarks in each theta range



#----------------------------------add visualizations----------------------------------
#read in data to ccreate vis
landmark_xy <- fread("analysis/landmark_xy.csv")
AT_101 <- fread("analysis/r101_med_AT_result.csv")
AT_101_vis <- AT_101 %>%
  select(-V1) %>%
  left_join(landmark_xy, by="landmark_index")
#heatmap(AT_101_vis$w_precision, Rowv=AT_101_vis$x, Colv=AT_101_vis$y)
library(ggplot2)

#----------------Wildtype-----------------
p1 <- ggplot(data = AT_101_vis, aes(x = min_alpha, y = min_theta)) +
  geom_tile(aes(fill = w_precision)) +
  xlab("Alpha") +
  ylab("Theta") +
  scale_x_continuous(limits = c(-90.51, 90.51), breaks=c(-90.5, 0, 90.51), labels=c("-90.51", "0", "90.51")) +
  scale_y_continuous(limits = c(-3.14, 3.14), breaks=c(-3.14, 0, 3.14), labels=c("-3.14","0","3.14")) +
  scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.2)) 
p1


p2 <- ggplot(data = AT_101_vis, aes(x = y, y = x)) +
  geom_tile(aes(fill = w_recall)) 
p2 + scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25))

p3 <- ggplot(data = AT_101_vis, aes(x = y, y = x)) +
  geom_tile(aes(fill = w_f1))
p3 + scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25))
#----------------Mutant-----------------
ggplot(data = AT_101_vis, aes(x = y, y = x)) +
  geom_tile(aes(fill = m_precision))

ggplot(data = AT_101_vis, aes(x = y, y = x)) +
  geom_tile(aes(fill = m_recall))

ggplot(data = AT_101_vis, aes(x = y, y = x)) +
  geom_tile(aes(fill = m_f1))

#----------------------------------------------------------------------------
# landmark_xy <- fread("3.InputData/tidy/landmark_xy.csv")
# #assign position to each landmark
# test <- data %>%
#   left_join(landmark_xy, by="landmark_index")
# list_of_variables <- names(AT)
ZRF <- fread("/Users/priscilla/Desktop/SDS Capstone/Zebrafish/7.aggregatedResults/ZRF_2med.csv")

#add aggregated data
aggregated <- ZRF %>%
  group_by(landmark_index) %>%
  summarise(c0_f1 = mean(c0_f1),
            c0_precision = mean(c0_precision),
            c0_recall = mean(c0_recall),
            c0_support = mean(c0_support),
            c1_f1 = mean(c1_f1),
            c1_precision = mean(c1_precision),
            c1_recall = mean(c1_recall),
            c1_support = mean(c1_support),
            f1 = mean(f1),
            c1_c1 = mean(c1_c1),
            c1_c0 = mean(c1_c0),
            precision = mean(precision),
            pred = mean(pred),
            recall = mean(recall),
            sample_index = "aggregated",
            c0_c1 = mean(c0_c1),
            c0_c0 = mean(c0_c0)
            )
aggregated <- aggregated[,c(2: 10,1,11:18)]
AT_all <- bind_rows(AT_test, aggregated)
fwrite(AT_all, "7.aggregatedResults/AT_2med_all.csv")

#assign a pair of x-y coordinate to each landmark
# AT <- AT %>%
#   mutate(column =  floor((landmark_index/row_num) -0.1)+1)
# AT <- AT %>%
#   left_join(landmark_xy, by="landmark_index")

#getting the percision scores that will be shown as the output
grepl("precision", list_of_variables)
positions <- which(grepl("precision", list_of_variables) %in% TRUE)
# length(positions)
# list_of_variables[positions[1]]
# list_of_variables[positions[2]]
# list_of_variables[positions[3]]

#---------------------------------------------------------------------------
# Adding original type of the zebrafish brain
# type_label <- landmark_AT_w_index %>%
#   select(sample_index, stype)
# type_label <- unique(type_label)
#fwrite(type_label, "3.InputData/tidy/type_label.csv")
AT <- fread("7.aggregatedResults/AT_2med_all.csv")
type_label <- fread("3.InputData/tidy/type_label.csv")
AT <- AT %>%
  left_join(type_label, by="sample_index")

names(AT)
AT_rename <- AT %>%
  select(sample_index, landmark_index, stype, pred,
         c0_c0, c0_c1, c1_c0, c1_c1,
         c0_precision, c0_recall, c0_f1, c0_support,
         c1_precision, c1_recall, c1_f1, c1_support,
         precision, recall, f1)
names(AT_rename)
names(AT_rename) <- c("sample_index", "landmark_index", "type", "pred",
                      "type0_0", "type0_1", "type1_0", "type1_1",
                      "type0_precision", "type0_recall", "type0_f1", "type0_num",
                      "type1_precision", "type1_recall", "type1_f1", "type1_num",
                      "overall_precision", "overall_recall", "overall_f1")
fwrite(AT_rename, "7.aggregatedResults/AT_2med_renamed.csv")
data <- data %>%
  mutate(pred = ifelse(pred == 0, "wt-at", "mt-at"))
fwrite(data, "7.aggregatedResults/AT_2med_renamed_2.csv")
#---------------------------------------------------------------------------
#ZRF adding aggregated
ZRF <- ZRF %>%
  group_by(landmark_index) %>%
  rename(type1_num = type1_support,
         type0_num = type0_support,
         overall_recall = recall,
         overall_precision = precision,
         overall_f1 = f1)

aggregated <- ZRF %>%
  group_by(landmark_index) %>%
  rename(type1_num = type1_support,
         type0_num = type0_support,
         overall_recall = recall,
         overall_precision = precision,
         overall_f1 = f1) %>%
  summarise(type0_f1 = mean(type0_f1),
            type0_precision = mean(type0_precision),
            type0_recall = mean(type0_recall),
            type0_num = mean(type0_num),
            type1_f1 = mean(type1_f1),
            type1_precision = mean(type1_precision),
            type1_recall = mean(type1_recall),
            type1_num = mean(type1_num),
            overall_f1 = mean(overall_f1),
            type1_1 = mean(type1_1),
            type1_0 = mean(type1_0),
            overall_precision = mean(overall_precision),
            pred = mean(pred),
            overall_recall = mean(overall_recall),
            sample_index = "aggregated",
            type0_1 = mean(type0_1),
            type0_0 = mean(type0_0)
  )
aggregated <- aggregated[,c(16, 1, 14, 11:12, 18,17, 9, 5, 7, 8, 3,4,6,2,13,15,10)]
ZRF_all <- bind_rows(ZRF, aggregated)
fwrite(ZRF_all, "7.aggregatedResults/ZRF_2med_all.csv")

ZRF <- fread("7.aggregatedResults/ZRF_2med_all.csv")
type_label <- fread("3.InputData/tidy/type_label.csv")
ZRF <- ZRF %>%
  left_join(type_label, by="sample_index")
ZRF <- ZRF %>%
  rename(type = stype) %>%
  mutate(pred = ifelse(pred == 0, "wt-at", "mt-at"))
fwrite(ZRF, "7.aggregatedResults/ZRF_2med_renamed_2.csv")

#----------------------------------------------------------------------------
#test baselines
AT_sub <- AT %>%
  filter(type1_num > 35)
AT_sub_1 <- AT %>%
  filter(type1_num <36)

AT_wrong <- AT_sub %>%
  group_by(sample_index) %>%
  summarise(N=n())

