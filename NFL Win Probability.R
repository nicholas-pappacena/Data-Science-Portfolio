install.packages('nflfastR')

# Nicholas Pappacena - MSDS 456 Assignment 1: Win Probability
library(nflfastR)
library(dplyr)
library(ggplot2)
library(caTools)

# Load in the Play-by-Play snaps and add in the winner and poswins columns 
pbp_21 <- load_pbp(2021)
pbp_21 <- pbp_21 %>% mutate(winner = ifelse(home_score > away_score, home_team, 
                                            away_team))
pbp_21 <- pbp_21 %>% mutate(poswins = ifelse(winner == posteam, "Yes", "No"))
pbp_21$poswins <- as.factor(pbp_21$poswins)

# Filter only the columns that want to include in the model 
pbp_21_filter <- pbp_21 %>% 
  filter(qtr <= 4 & poswins != "NA" & play_type != "no_play" & 
           play_type != "NA") %>%
  select(game_id, home_team, away_team, yardline_100,game_seconds_remaining/60, 
         posteam, poswins, down, ydstogo, score_differential, home_wp, away_wp, 
         wp, desc)
View(pbp_21_filter)

# Split the data into a training and testing set 
set.seed(954)
split = sample.split(pbp_21_filter$poswins, SplitRatio = 0.8)
train = pbp_21_filter %>% filter(split == TRUE)
test = pbp_21_filter %>% filter(split == FALSE)

# Create the play-by-play model
np_model <- glm(poswins ~ down + yardline_100 + game_seconds_remaining + 
                  ydstogo + score_differential, train, family = "binomial")
summary(np_model)

pred_home = predict(np_model, train, type = "response")

train <- cbind(train, pred_home)
train <- mutate(train, pred_home = ifelse(posteam == home_team, 
                                          pred_home, 1 - pred_home))

# Test the model against 2021 NFL Super Bowl
game_id_21 <- unique(pbp_21$game_id)
View(pbp_21_filter %>% filter(game_id == "2021_22_LA_CIN"))

ggplot(filter(train, game_id == "2021_22_LA_CIN", !is.na(down)),aes(x=game_seconds_remaining/60, y = pred_home)) +
  geom_line(size = 2, color = "orange") + scale_x_reverse() +
  ylim(c(0,1)) + theme_minimal() +
  xlab("Time Remaining") + ylab("Win Probability") + 
  geom_line(aes(game_seconds_remaining/60, 1 - pred_home), col = "blue", size = 2) +
  geom_line(aes(game_seconds_remaining/60, home_wp), col = "darkgray") +
  geom_line(aes(game_seconds_remaining/60, away_wp), col = "black") +
  annotate("text", x = 15, y = 0.9, label = "Pappacena WP Model: Rams", col = "blue") +
  annotate("text", x = 15, y = 0.85, label = "nflfastR WP: Rams", col = "black") +
  annotate("text", x = 15, y = 0.15, label = "Pappacena WP Model: Bengals", col = "orange") +
  annotate("text", x = 15, y = .1, label = "nflfastR WP: Bengals", col = "darkgrey") +
  geom_vline(aes(xintercept = 30), lty = "dashed") +
  geom_vline(aes(xintercept = 45), lty = "dashed") +
  geom_vline(aes(xintercept = 15), lty = "dashed")
