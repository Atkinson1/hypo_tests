# Paired t-test

library(tidyverse)
library(janitor)
library(hablar)
library(skimr)
library(infer)

setwd("C:/users/ryan/desktop/stats")

# to compare sd, use skim() or sd()
skim(two_samp)

# 2-sample t-test
two_samp <- read_csv("C:/users/ryan/desktop/stats/t-TestExamples.csv")
two_samp <- two_samp[-3] # removes third column
two_samp <- clean_names(two_samp)

# directions:
    # create a new column that is the difference between the two: difference = after - before, 
    # and then examine this distribution to see how each individuals measurements changed over time.

# create column of diff b/t two samples
two_samp_ind <- two_samp %>% mutate(diff = (pretest - posttest))

    # Once we've mutate()d that new difference column, we can run a 1-sample t-test on it, 
    # where our null hypothesis is that mu = 0 (i.e. the difference between these measurements 
    # before and after treatment is, on average, 0)..

stat <- specify(two_samp_ind, response = diff) %>%
  calculate(stat = "mean")

null <- specify(two_samp_ind, response = diff) %>%
  hypothesize(null = "point", mu = 0) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

# get p-value
null %>% get_p_value(stat, direction = "both")

# visualize p value
null %>% visualize() +
  shade_p_value(stat, direction = "both")

# get ci
ci <- null %>% get_ci(stat, level = .95, type = "se")

# visualize ci
null %>% visualize() +
  shade_ci(ci)

# t_test wrapper
t_test(two_samp_ind, response = diff, mu = 0)

# Negative value reflect fact that pretest has lower mean than posttest.