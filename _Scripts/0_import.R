###################################
### PAE2 import script ############
###################################

# Author: Juliet Rowe

#### Import required packages ####
library(data.table)
library(dplyr)
library(tidytable)
library(afex)
library(emmeans)
library(car)
library(performance)

#### Import trial data ####
df <- list.files(path = "./_Data/reach_and_point/",
                 pattern="*.csv",
                 all.files = TRUE,
                 full.names = TRUE) %>%
  map_df(~fread(., colClasses = "character"))

# Remove participant P107 and 110, experiment crashed
df <- subset(df, !(id %in% c("P34", "P107", "P110", "P153", "P154", "P163", "P165", "P168", "P186")))

# Subset relevant groups
df_PAE2 <- subset(df, group %in% c("PP", "PP-None", "PP-CTRL", "PP-MI"))

# Subset Pre- and Post-Test Blocks
df_PAE2 <- subset(df_PAE2, block %in% c("Baseline", "PostTest"))

# Converting mm to visual angle
df_PAE2$distance_x <- as.numeric(df_PAE2$distance_x)
df_PAE2$visual_angle <- atan(df_PAE2$distance_x/450)* (180/pi) #converting radians to degrees #47 cm #54cm

# Subsetting only appropriate columns
df_PAE2 <- df_PAE2[, !c("created",
                        "age",
                        "handedness",
                        "response_time",
                        "reaction_time",
                        "points_x",
                        "points_y",
                        "location_x",
                        "location_y",
                        "distance_x",
                        "distance_y",
                        "run_time",
                        "MIRating")]

# Subset with only 10 post test trials
df_PAE2$trial_num <- as.numeric(df_PAE2$trial_num)
df_PAE2<- subset(df_PAE2, block %in% c("Baseline", "PostTest") & trial_num < 11 )

# Average visual angle
df_PAE2$id <- as.factor(df_PAE2$id)
df_PAE2$block <- as.factor(df_PAE2$block)
df_aftereffects_E2 <- df_PAE2 %>%
  group_by(id, block, group) %>%
  summarize(visual_angle_avg = mean(visual_angle))

# Summarize data
df_summary <- df_PAE2 %>%
  group_by(block,group) %>%
  summarize(visual_angle_avg = mean(visual_angle),
            visual_angle_sd = sd(visual_angle))

#### Movement time DataFrame ####
# Subsetting Exposure Trials
df_exposure <- subset(df, block %in% c("Exposure", "MIExposure"))

# Subsetting only appropriate columns
df_exposure <- df_exposure[, !c("created",
                                "age",
                                "handedness",
                                "reaction_time",
                                "points_x",
                                "points_y",
                                "location_x",
                                "location_y",
                                "distance_x",
                                "distance_y",
                                "run_time")]

# Subset relevant groups
df_CTRL_MI <- subset(df_exposure, group %in% c("PP-CTRL", "PP-MI"))
df_PP <- subset(df_exposure, group == "PP")

# 10 groups of 23 trials for each participant in CTRL and MI
df_grouped_CTRL_MI <- df_CTRL_MI %>%
  group_by(id) %>%
  mutate(subblock = rep(1:10, each = 23)) %>%
  ungroup()

# 10 groups of 25 trials for each participant in PP
df_grouped_PP <- df_PP %>%
  group_by(id) %>%
  mutate(subblock = rep(1:10, each = 25)) %>%
  ungroup()

# Binding df_grouped_CTRL_MI with df_grouped_PP
df_grouped_trials <- rbind(df_grouped_CTRL_MI, df_grouped_PP)
df_grouped_trials$subblock<- as.numeric(df_grouped_trials$subblock)
df_grouped_trials$group <- as.character(df_grouped_trials$group)
df_grouped_trials$response_time <- as.numeric(df_grouped_trials$response_time)
df_movement_time <- df_grouped_trials %>%
  group_by(group, subblock) %>%
  summarise(response_time_avg = mean(response_time),
            response_time_sd = sd(response_time))

#### Participant Info ####

# Import Demographic Data
demo_dat <- read.csv("./_Data/Participant_info_E2.csv")

# Remove unwanted participants
demo_dat <- subset(demo_dat, !(id %in% c("P34",
                                         "P107",
                                         "P110",
                                         "P153",
                                         "P154",
                                         "P163",
                                         "P165",
                                         "P168",
                                         "P186")))

# Subset relevant groups
demo_dat <- subset(demo_dat, group %in% c("PP", "PP-None", "PP-CTRL", "PP-MI"))
View(demo_dat)
