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

#Load Provided Data (2021, 2022, 2023)
pitch_data <- drop_read_csv("Datasets/Reds Hackathon/savant_pitch_level.csv")


## Pitch Movement Plots
pitcher_specific <- pitch_data %>%
  filter(player_name == 'Kopech, Michael', game_year == 2023) %>%
  tail(200)

PitchTypeColors <- c("FF"="red", "SI"="deeppink", "FC"="blue", "CU"="orange", "KC"="orange", "SV"="orange", "SL"="deepskyblue", "ST"="deepskyblue", 
                     "CH"="palegreen4", "FS"="palegreen3", "KN"="grey55")

ggplot(pitcher_specific, aes(x = -pfx_x*12, y = pfx_z*12)) + 
  labs(title = paste0("Connor Phillips Pitch Movement (Last 200)")) + 
  scale_x_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) + 
  scale_y_continuous(limits = c(-30, 30),  breaks = c(-30, -20, -10, 0, 10, 20, 30)) +
  geom_segment(aes(x = 0, y = -30, xend = 0, yend = 30), size = 1, color = "grey55") + 
  geom_segment(aes(x = -30, y = 0, xend = 30, yend = 0), size = 1, color = "grey55") +
  geom_point(aes(fill = pitch_type), color = "black", pch = 21, alpha = 0.8, size = 4, na.rm = TRUE) +
  scale_fill_manual(values = PitchTypeColors) +
  theme_bw() + theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12), axis.title = element_blank()) + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Pitch Type")) +
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
