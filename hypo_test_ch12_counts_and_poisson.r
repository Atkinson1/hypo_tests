library(here)
library(tidyverse)
library(janitor)
library(infer)
library(hablar)

# https://bookdown.org/mpfoley1973/statistics/one-sample-poisson-test.html

count_data <- read_csv(here("CountDataExamples.csv")) %>%
  clean_names() %>% select(-x2)
count_data <- count_data %>% mutate(time = row_number())

      # One-Sample Poisson Rate Test

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/poisson.test
# poisson.test(x, alternative = "two.sided, conf.level =.95)

ggplot(count_data, aes(x = falls)) +
  geom_bar()

poisson.test(x = sum(count_data$falls * count_data$time), 
             T = sum(count_data$time), 
             r = 1.75,
             alternative = "two.sided")

# T is the total frequency count;
# r is the reference value
# x is the total number of occurrences (falls) 
    # * the number of days (time)

      # Two-Sample Poisson Rate Test
two_samp <- count_data[1:30, 2:4]
      
poisson.test(x = c(sum(two_samp$store_1 * two_samp$time), sum(two_samp$store_2 * two_samp$time)), 
                                   T = c(sum(two_samp$time), sum(two_samp$time)))

      # rateratio.test() package also returns average rates for each counted group       

# https://rdrr.io/cran/rateratio.test/
      # install.packages("rateratio.test")
      # library(rateratio.test)

rateratio.test(x = c(sum(two_samp$store_1 * two_samp$time), sum(two_samp$store_2 * two_samp$time)),
               c(sum(two_samp$time), sum(two_samp$time)))

    # For the output below:

# p-value < 2.2e-16
# alternative hypothesis: true rate ratio is not equal to 1
# 95 percent confidence interval:
   # 0.5925221 0.6708385

# this means that the rate ratio describe what proportion is between rate 1 and rate 2;
# confidence interval describes what the population rate ratio is likely to be
# the p-value means that the null cannot be accepted, and therefore the alt hyp (true rate ratio not equal to 1) is true.
  # simplified: there is a statistically significant difference.


      # Chi-square goodness of fit test for Poisson distribution 

# Incomplete, but links below can be followed to get output.

# https://rdrr.io/github/reyzaguirre/rhep/man/chisq.pois.html
# https://www.r-bloggers.com/2013/04/checking-the-goodness-of-fit-of-the-poisson-distribution-in-r-for-alpha-decay-by-americium-241/

    # discrete_gof <- read_csv(here("DiscreteGOF.csv")) %>% clean_names() %>%
    #   convert(fct(color))
    # discrete_gof <- discrete_gof %>% mutate(row = row_number())
    # 
    # library(rhep)