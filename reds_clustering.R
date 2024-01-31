################################################################################################
####################################  PULL DOWN FROM ORIGIN ####################################
################################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dplyr)
library(cluster)
library(factoextra)
library(caret)
source("../hawkeye_functions/handling_trackman_functions.R")
load("model_files/model_role_prob.RDS") 
# ^^ This is a role probability model. Training code is below but you can load it in to explore without retraining it

options(scipen = 999)

#Load Provided Data (2021, 2022, 2023)
pitch_data <- drop_read_csv("Datasets/Reds Hackathon/savant_pitch_level.csv")
season_data <- drop_read_csv("Datasets/Reds Hackathon/fangraphs_season_level.csv")

primary_roles <- pitch_data %>%
  group_by(player_name) %>%
  dplyr::summarize(
    rp = sum(role_key == 'RP', na.rm = T),
    sp = sum(role_key == 'SP', na.rm = T)
  ) %>%
  dplyr::mutate(
    primary_role = ifelse(sp > rp, 1, 0)
  )

q_pitchers <- season_data %>%
  filter(IP >= 20) %>%
  group_by(MLBAMID, Season) %>%
  select(NameASCII, MLBAMID, Season, Role, Location_plus, K_to_BB)

pitch_counts <- pitch_data %>%
  group_by(game_year,player_name,pitcher,pitch_type) %>%
  dplyr::summarize(count = n())

pitch_data <- left_join(pitch_data,pitch_counts,by = c("game_year","player_name","pitcher","pitch_type"))

variables <- pitch_data %>%
  group_by(game_year, player_name, pitcher) %>%
  dplyr::summarize(
    pitches = n(),
    arsenal = n_distinct(pitch_type[count >= 10]),
    mph_loss = cor(release_speed[pitch_type %in% c('FF', 'SI')], pitch_number_appearance[pitch_type %in% c('FF', 'SI')]),
    platoon = mean(estimated_woba_using_speedangle[stand == 'R'], na.rm = T) - mean(estimated_woba_using_speedangle[stand == 'L'], na.rm = T)
  ) %>%
  filter(pitches >= 200)




merged_data <- left_join(q_pitchers, variables, by = c("MLBAMID" = "pitcher", "Season" = "game_year")) %>%
  select(-c('NameASCII')) %>%
  na.omit()

# Create a binary outcome variable (1 for SP, 0 for RP)
merged_data$role_binary <- as.factor(ifelse(merged_data$Role == "SP", 1, 0))

df_train <- merged_data

#######################################
######### Pitcher Role Model ##########
#######################################
#Cross Validation
addTaskCallback(function(...) {set.seed(123); TRUE})
strata <- paste(df_train$Role, sep = "_")
dt <- createDataPartition(y = strata, p = 0.7, list = FALSE)
train <- df_train[dt,]
test <- df_train[-dt,]

# Build a logistic regression model
model <- glm(role_binary ~ K_to_BB + Location_plus + mph_loss + arsenal + platoon, data = merged_data, family = "binomial")
# save(model, file = "model_files/model_role_prob.RDS")

# Make predictions for role
test$role_prob <- predict(model, newdata = test, type = "response")

#Find largest misfits
rp_subset <- test[test$Role == "RP", ]
sp_subset <- test[test$Role == "SP", ]

#RP Misfits
ten_largest_rp <- head(rp_subset[order(-rp_subset$role_prob), ], 10)
#SP Misfits
ten_smallest_sp <- head(sp_subset[order(sp_subset$role_prob), ], 10)


#########################
###Apply Model to 2023###
#########################
df_23 <- merged_data %>%
  filter(Season == 2023)

df_23$role_prob <- with(df_23, predict(model_role_prob, df_23, type = "response"))

#Find largest misfits
rp_subset <- df_23[df_23$Role == "RP", ]
sp_subset <- df_23[df_23$Role == "SP", ]

#RP Misfits
sp_candidates <- head(rp_subset[order(-rp_subset$role_prob), ], 10)
#SP Misfits
rp_candidates <- head(sp_subset[order(sp_subset$role_prob), ], 10)












#########################################
########### K-means Cluster #############
#########################################
# cluster_df <- merged_data %>%
#   select(Location_plus, K_to_BB, arsenal, mph_loss, platoon)
# 
# #Scale Variables
# cluster_df <- scale(cluster_df)
# head(cluster_df)
# 
# # distance <- get_dist(cluster_df)
# # fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
# 
# #Find optimal number of clusters
# #Elbow Method
# set.seed(123)
# fviz_nbclust(cluster_df, kmeans, method = "wss") #About 5 clusters
# 
# #Silhouette Method
# set.seed(123)
# fviz_nbclust(cluster_df, kmeans, method = "silhouette") #2 followed by 5
# 
# #Gap Stat Method
# set.seed(123)
# gap_stat <- clusGap(cluster_df, FUN = kmeans, nstart = 25,
#                     K.max = 10, B = 50)
# fviz_gap_stat(gap_stat)
# 
# 
# ###################
# # Create Clusters #
# ###################
# set.seed(123)
# final <- kmeans(cluster_df, 5, nstart = 25)
# 
# fviz_cluster(final, data = cluster_df)
# 
# merged_data$cluster <- final$cluster
