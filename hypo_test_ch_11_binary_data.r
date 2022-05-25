library(tidyverse)
library(infer)
library(inferr)
library(hablar)

ones <- tibble(a = rep(1, 80)) 
zeroes <- tibble(a = rep(0, 20))
ones_and_zeroes <- ones %>% bind_rows(zeroes) %>%
convert(fct(a))  

        # One-sample Proportions Test

# ci stat
p_hat <- ones_and_zeroes %>%
  specify(response = a, success = "1") %>%
  calculate(stat = "prop")

# ci null
boot_null <- ones_and_zeroes %>%
  specify(response = a, success = "1") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")

# get confidence interval
ci <- get_ci(boot_null)

# visualize ci
visualize(boot_null) +
  shade_ci(endpoints = ci)


# Alternatively, use the bootstrap distribution to 
# find a confidence interval using the standard error:
standard_error_ci <- boot_null %>%
  get_ci(type = "se", point_estimate = p_hat)

visualize(boot_null) +
  shade_confidence_interval(endpoints = standard_error_ci)

# p-value null
null_dist <- ones_and_zeroes %>%
  specify(response = a, success = "1") %>%
  hypothesize(null = "point", p = .7) %>%
  generate(reps = 1000) %>%
  calculate(stat = "prop")

visualize(null_dist) +
  shade_p_value(obs_stat = p_hat, direction = "two-sided")

# p-value of .04
null_dist %>%
  get_p_value(obs_stat = p_hat, direction = "two-sided")

        # Two-sample Proportions Test
  # use inferr to get p-value; use infer to get/plot confidence intervals

two_samp <- tibble(samp_one = c(rep(1, 8), rep(0, 122)), 
                   samp_two = c(rep(1, 19), rep(0, 111)))


# inferr
    # p-value that tests difference
infer_ts_prop_test(two_samp, samp_one, samp_two,
                   alternative = "both")

two_samp_infer <- two_samp %>% pivot_longer(., 
                  cols = c("samp_one", "samp_two"),
                  names_to = "samples", values_to = "values") %>%
  convert(fct(values, samples))

# infer
# https://infer.netlify.app/articles/observed_stat_examples.html#two-categorical-variables-diff-in-proportions-

two_samp_infer_stat <- two_samp_infer %>% 
 specify(values ~ samples, success = "1") %>%
 calculate(stat = "diff in props", order = c("samp_one", "samp_two"))

two_samp_infer_ci <- two_samp_infer %>% 
  specify(values ~ samples, success = "1") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("samp_one", "samp_two"))

percentile_ci <- get_ci(two_samp_infer_ci)
percentile_ci # the estimated difference, lower/upper bounds

visualize(two_samp_infer_ci) +
 shade_confidence_interval(endpoints = percentile_ci)

# CI would contain 0 if null hypothesis couldn't be rejected;
  # Since it doesn't, we can reject the null & conclude 
  # that this difference exists in the population

# however, since lower bound of CI is close to 0, 
# may not have practical difference.

# Normal approximation of the binomial distribution
plot(dbinom(x = 1:20, size = 20, prob = 0.5))




# Fisher's exact test for small sample size
https://www.statology.org/fishers-exact-test-in-r/
# fisher.test(data)  

    
# random number generator (remember to set.seed())
round(runif(100, 0, 1))


# rstats - binomial distribution
https://www.tutorialspoint.com/r/r_binomial_distribution.htm














