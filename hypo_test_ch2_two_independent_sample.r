# 2 Sample t-test

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
# more cleaning
two_samp_ind <- two_samp %>% pivot_longer(., cols = c(method_a, method_b), names_to = "method", values_to = "values") %>%
  select(c(method, values)) %>% convert(fct(method))

# independent sample t-test

  # 1.) calculating sample statistic
two_samp_stat <- two_samp_ind %>%
  specify(response = values, explanatory = method) %>%
  calculate(stat = "diff in means", order = c("method_a", "method_b"))

  # creating sampling distribution
two_samp_null <- two_samp_ind %>%
  specify(values ~ method) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("method_a", "method_b"))

  # 2.) visualize
two_samp_null %>%
    visualize() +
    shade_p_value(two_samp_stat, direction = "two-sided")
  
two_samp_null %>% get_p_value(two_samp_stat, direction = "two-sided")

# wrapper function, t_test, to carry out 2-sample t-tests on tidy data
t_test(x = two_samp_ind,
       formula = values ~ method,
       order = c("method_a", "method_b"),
       alternative = "two-sided")

# base r t-test
  # t.test(two_samp$method_a, two_samp$method_b, var.equal=TRUE)