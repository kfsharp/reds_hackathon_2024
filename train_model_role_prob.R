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
options(scipen = 999)
load("model_files/model_role_prob.RDS") 
# ^^ This is a role probability model. Training code is below but you can load it in to explore without retraining it

#Load Provided Data (2021, 2022, 2023)
pitch_data <- drop_read_csv("Datasets/Reds Hackathon/savant_pitch_level.csv")
season_data <- drop_read_csv("Datasets/Reds Hackathon/fangraphs_season_level.csv")

q_pitchers <- season_data %>%
  filter(IP >= 20) %>%
  group_by(MLBAMID, Season) %>%
  select(NameASCII, MLBAMID, Season, Location_plus, K_minus_BB_pct, Stuff_plus)

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



