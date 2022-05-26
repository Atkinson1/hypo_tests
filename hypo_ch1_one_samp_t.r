library(tidyverse)
library(janitor)
library(hablar)
library(skimr)
library(infer)
library(here)

x <- read_csv(here("FuelsCosts.csv")) %>% clean_names()

skim(x)

null_dist <- specify(x, response = fuel_cost) %>%
  hypothesize(null = "point", mu = 260) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

x_bar <- x %>%
  specify(., response = fuel_cost) %>%
  calculate(stat = "mean")

visualize(null_dist) +
  shade_p_value(obs_stat = x_bar, direction = "two-sided")

null_dist %>%
  get_p_value(x_bar, direction = "two-sided")

# t_test wrapper around original tibble

x %>% t_test(response = fuel_cost, mu = 260)
