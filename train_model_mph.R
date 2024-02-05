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

#Load Provided Data (2021, 2022, 2023)
pitch_data <- drop_read_csv("Datasets/Reds Hackathon/savant_pitch_level.csv")
season_data <- drop_read_csv("Datasets/Reds Hackathon/fangraphs_season_level.csv")



q_pitchers <- season_data %>%
  dplyr::group_by(NameASCII,MLBAMID, Season) %>%
  dplyr::summarize(IP = sum(IP),
                   G = sum(G)) %>%
  filter(IP >= 20) 

pitch_counts <- pitch_data %>%
  filter(!pitch_type %in% c('PO', 'FA', 'FO')) %>%
  group_by(game_year, player_name, pitcher,pitch_type) %>%
  dplyr::summarize(count = n())

pitch_data <- left_join(pitch_data, pitch_counts, by = c("game_year","player_name","pitcher","pitch_type"))

variables <- pitch_data %>%
  group_by(game_year, player_name, pitcher) %>%
  dplyr::summarize(
    pitches = n(),
    mph_loss = cor(release_speed[pitch_type %in% c('FF', 'SI')], pitch_number_appearance[pitch_type %in% c('FF', 'SI')]),
    fbs = sum(pitch_type %in% c('FF', 'SI'), na.rm = T)
  ) %>%
  filter(pitches >= 200)


merged_data <- left_join(q_pitchers, variables, by = c("MLBAMID" = "pitcher", "Season" = "game_year")) %>%
  select(-c('NameASCII')) %>%
  na.omit() %>%
  mutate(fbs_per_app = fbs/G)
  

df_train <- merged_data

#######################################
######### Pitcher Role Model ##########
#######################################
#Cross Validation
addTaskCallback(function(...) {set.seed(123); TRUE})
strata <- paste(df_train$mph_loss, sep = "_")
dt <- createDataPartition(y = strata, p = 0.7, list = FALSE)
train <- df_train[dt,]
test <- df_train[-dt,]

#Build a logistic regression model
model <- lm(mph_loss ~ fbs_per_app, data = merged_data)
summary(model)
#save(model, file = "model_files/model_role_prob.RDS")

merged_data$xmph_loss <- predict(model,merged_data)

merged_data <- merged_data %>%
  mutate(residual = mph_loss - xmph_loss)
