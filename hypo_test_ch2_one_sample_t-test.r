# One sample t-test

library(tidyverse)
library(janitor)
library(hablar)
library(skimr)
library(infer)

setwd("C:/users/ryan/desktop/stats")

df <- read_csv("C:/users/ryan/desktop/stats/AssessmentScores.csv")
df <- df %>% clean_names()

average <- specify(df, response = score) %>%
  calculate(stat = "mean")

null <- specify(df, response = score) %>%
  hypothesize(null = "point", mu = 60) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

null %>% get_p_value(average, direction = "both")

visualize(null) +
  shade_p_value(average, direction = "two-sided")

# t test
t <- t_test(df, response = score, mu = 60)

# plotting confidence interval
ci <- null %>% get_ci(average, level = .95, type = "se")

null %>% visualize() +
  shade_ci(ci)





