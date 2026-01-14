library(dplyr)
library(readr)
library(stats)
library(mgcv)
library(rjson)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(R.basic)

options(digits = 15)

exampleMatch <- read.csv("matches/all_matches.csv")

innings1 <- exampleMatch[exampleMatch$innings == 1, ]

first_balls <- innings1 %>%
  filter(ball == 0.1) %>%
  select(match_id, first_ball_runs = runs_off_bat, first_ball_extras = extras, wickets = wicket_type)

innings1Totals <- innings1 %>%
  group_by(match_id) %>%
  summarise(total_score = sum(runs_off_bat + extras))

combined <- first_balls %>%
  left_join(innings1Totals, by = "match_id")

conditional_averages <- combined %>%
  group_by(first_ball_runs) %>%
  summarise(
    count = n(),
    mean_score = mean(total_score),
    sd_score = sd(total_score)
  ) %>%
  arrange(first_ball_runs)

print(conditional_averages)

conditional_extras <- combined %>%
  group_by(first_ball_extras) %>%
  summarise(
    count = n(),
    mean_score = mean(total_score),
    sd_score = sd(total_score)
  ) %>%
  arrange(first_ball_extras)

print(conditional_extras)

conditional_wickets <- combined %>%
  group_by(wickets) %>%
  summarise(
    count = n(),
    mean_score = mean(total_score),
    sd_score = sd(total_score)
  ) %>%
  arrange(wickets)

print(conditional_wickets)

# Overall mean for comparison
cat("\nOverall mean:", mean(innings1Totals$total_score), "\n")

innings1Totals <- aggregate(runs_off_bat+extras~match_id+innings, exampleMatch[exampleMatch$innings==1,], sum)
scores1 <- innings1Totals$`runs_off_bat + extras`
hist(scores1, breaks=20, freq=FALSE)
length(scores1)
mean(scores1)
