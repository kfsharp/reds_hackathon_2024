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

q_pitchers <- season_data %>%
  filter(IP >= 20) %>%
  group_by(MLBAMID, Season) %>%
  select(NameASCII, MLBAMID, Season, Location_plus, K_minus_BB_pct, Stuff_plus)

pitch_data <- left_join(q_pitchers, primary_roles,by = c('NameASCII' = 'player_name'))

pitch_counts <- pitch_data %>%
  filter(!pitch_type %in% c('PO', 'FA', 'FO')) %>%
  group_by(game_year,player_name,pitcher,pitch_type) %>%
  dplyr::summarize(count = n())

pitch_data <- left_join(pitch_data,pitch_counts,by = c("game_year","player_name","pitcher","pitch_type"))

variables <- pitch_data %>%
  group_by(game_year, player_name, pitcher) %>%
  dplyr::summarize(
    pitches = n(),
    rp = sum(role_key == 'RP', na.rm = T),
    sp = sum(role_key == 'SP', na.rm = T),
    arsenal = n_distinct(pitch_type[count >= 10]),
    mph_loss = cor(release_speed[pitch_type %in% c('FF', 'SI')], pitch_number_appearance[pitch_type %in% c('FF', 'SI')]),
    primary_role = ifelse(sp > rp, 'SP', 'RP')
  ) %>%
  filter(pitches >= 200) %>%
  select(-c(rp, sp))

merged_data <- left_join(q_pitchers, variables, by = c("MLBAMID" = "pitcher", "Season" = "game_year")) %>%
  select(-c('NameASCII')) %>%
  na.omit()

#Create a binary outcome variable (1 for SP, 0 for RP)
merged_data$role_binary <- as.factor(ifelse(merged_data$primary_role == "SP", 1, 0))

df_train <- merged_data

#######################################
######### Pitcher Role Model ##########
#######################################
#Cross Validation
addTaskCallback(function(...) {set.seed(123); TRUE})
strata <- paste(df_train$role_binary, sep = "_")
dt <- createDataPartition(y = strata, p = 0.7, list = FALSE)
train <- df_train[dt,]
test <- df_train[-dt,]

#Build a logistic regression model
model <- glm(role_binary ~ Stuff_plus + Location_plus + mph_loss + arsenal, data = merged_data, family = "binomial")
#save(model, file = "model_files/model_role_prob.RDS")

#Visualize Feature Importance
feature_importance = caret::varImp(model)

ggplot2::ggplot(feature_importance, aes(x=reorder(rownames(feature_importance),Overall), y=Overall)) +
  geom_point( color="#FFCD00", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(feature_importance), xend=rownames(feature_importance), y=0, yend=Overall), 
                color='black') +
  xlab('Variable')+
  ylab('Overall Importance') +
  theme_light() +
  coord_flip() 

#Make predictions for role
test$role_prob <- predict(model, newdata = test, type = "response")

#Find largest misfits
rp_subset <- test[test$primary_role == "RP", ]
sp_subset <- test[test$primary_role == "SP", ]

#RP Misfits
ten_largest_rp <- head(rp_subset[order(-rp_subset$role_prob), ], 10)
#SP Misfits
ten_smallest_sp <- head(sp_subset[order(sp_subset$role_prob), ], 10)


#########################
###Apply Model to 2023###
#########################
df_23 <- merged_data %>%
  filter(Season == 2023)

df_23$role_prob <- with(df_23, predict(model, df_23, type = "response"))

#Find largest misfits
rp_subset <- df_23[df_23$primary_role == "RP", ]
sp_subset <- df_23[df_23$primary_role == "SP", ]

#RP Misfits
sp_candidates <- head(rp_subset[order(-rp_subset$role_prob), ], 10)
#SP Misfits
rp_candidates <- head(sp_subset[order(sp_subset$role_prob), ], 10)


#Batters per game appears to be bimodal distribution
pitcher_role_types <- season_data %>%
  group_by(NameASCII, Season) %>%
  dplyr::summarize(
    batters_faced = TBF/G
  ) %>%
  filter(Season == 2023)

hist(pitcher_role_types$batters_faced) #Proof that there are only two roles for pitchers?

#################################
## Pitch Movement Plots
pitcher_specific <- pitch_data %>%
  filter(player_name == 'Phillips, Connor', game_year == 2023) %>%
  tail(200)

PitchTypeColors <- c("FF"="red", "SI"="deeppink", "FC"="blue", "CU"="orange", "KC"="orange", "SV"="orange", "SL"="deepskyblue", "ST"="deepskyblue", 
                               "CH"="palegreen4", "FS"="palegreen3", "KN"="grey55")

ggplot(pitcher_specific, aes(x = pfx_x*12, y = pfx_z*12)) + 
  labs(title = paste0("Connor Phillips Pitch Movement (Last 200)")) + 
  scale_x_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) + 
  scale_y_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
  geom_segment(aes(x = 0, y = -30, xend = 0, yend = 30), size = 1, color = "grey55") + 
  geom_segment(aes(x = -30, y = 0, xend = 30, yend = 0), size = 1, color = "grey55") +
  geom_point(aes(fill = pitch_type), color = "black", pch = 21, alpha = 0.8, size = 4, na.rm = TRUE) +
  scale_fill_manual(values = PitchTypeColors) +
  theme_bw() + theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12), axis.title = element_blank()) + 
  theme(legend.position = "none") +
  coord_equal()

## Pitch Locations
get_background_info()

ggplot2::ggplot(pitcher_specific, aes(x = plate_x, y = plate_z)) + 
  scale_x_continuous(limits = c(-2.5,2.5)) + scale_y_continuous(limits = c(0,5)) +
  geom_path(data = homePlateBatter, aes(x = x, y = y), size = 1) + 
  geom_rect(aes(xmin = negZone - fullBall, xmax = posZone + fullBall, ymin = botZone - fullBall, ymax = topZone + fullBall), alpha = 0, size = 1, color = "black") +
  geom_rect(aes(xmin = negZone , xmax = posZone, ymin = horzBottom, ymax = horzTop), alpha = 0, size = 1, color = "grey55") +
  geom_rect(aes(xmin = vertLeft, xmax = vertRight, ymin = botZone, ymax = topZone), alpha = 0, size = 1, color = "grey55") +
  geom_rect(aes(xmin = negZone, xmax = posZone, ymin = botZone, ymax = topZone), alpha = 0, size = 1, color = "black") +
  geom_point(aes(fill = pitch_type), color = "black", pch = 21, alpha = 0.8, size = 4, na.rm = TRUE) +
  scale_fill_manual(values = PitchTypeColors) +
  theme_bw() + guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 12), panel.grid = element_blank()) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12))


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
