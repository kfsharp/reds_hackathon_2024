################################################################################################
####################################  PULL DOWN FROM ORIGIN ####################################
################################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dplyr)
library(cluster)
library(factoextra)
options(scipen = 999)

#Load Provided Data (2021, 2022, 2023)
pitch_data <- drop_read_csv("Datasets/Reds Hackathon/savant_pitch_level.csv")
season_data <- drop_read_csv("Datasets/Reds Hackathon/fangraphs_season_level.csv")

q_pitchers <- season_data %>%
  group_by(MLBAMID, Season) %>%
  select(NameASCII, MLBAMID, Season, Location_plus, Stuff_plus) %>%
  filter(IP >= 20)

pitch_counts <- pitch_data %>%
  filter(!pitch_type %in% c('PO', 'FA', 'FO')) %>%
  group_by(game_year, player_name, pitcher, pitch_type) %>%
  dplyr::summarize(count = n())

pitch_data <- left_join(pitch_data, pitch_counts, by = c("game_year", "player_name", "pitcher", "pitch_type"))

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
  filter(Season == 2023) %>%
  na.omit()

pitcher_role_types <- season_data %>%
  group_by(NameASCII, Season, MLBAMID) %>%
  dplyr::summarize(
    batters_faced = TBF/G
  ) %>%
  filter(Season == 2023)

hist(pitcher_role_types$batters_faced) #Proof that there are only two roles for pitchers?

#########################################
########### K-means Cluster #############
#########################################
cluster_df <- merged_data %>%
  left_join(pitcher_role_types, by = "MLBAMID") %>%
  select(arsenal, mph_loss, batters_faced)

#Scale Variables
cluster_df <- scale(cluster_df)
head(cluster_df)

#Find optimal number of clusters
#Gap Stat Method
set.seed(123)
gap_stat <- clusGap(cluster_df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat) +
  ggtitle('Optimal Number of Clusters') +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(axis.text = element_text(size = 10, face = 'bold'), axis.title = element_text(face = 'bold'))
  

###################
# Create Clusters #
###################

#Perform k-means clustering with the chosen number of clusters
set.seed(123)
role_cluster <- kmeans(cluster_df, centers = 2, nstart = 25)

#Visualize the clusters
fviz_cluster(role_cluster, data = cluster_df, 
             paletter = 'Set2', ggtheme = theme_bw()) +
  ggtitle('Pitcher Role Clusters') +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(axis.text = element_text(size = 10, face = 'bold'), axis.title = element_text(face = 'bold'))
  

#Append back to initial DF
unscaled_data <- t(apply(cluster_df, 1, 
                       function(r) r * attr(cluster_df, 'scaled:scale') + 
                         attr(cluster_df, 'scaled:center')))

pitcher_groups <- as.data.frame(cbind(unscaled_data, cluster = role_cluster$cluster)) %>%
  select('MLBAMID', 'cluster')

final_data <- left_join(q_pitchers, pitcher_groups, by = 'MLBAMID') %>%
  filter(Season == 2023) %>%
  na.omit()



