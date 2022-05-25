library(hablar) # quick data type conversion
library(tidyverse)
library(janitor) # cleans names
library(here)
library(infer) # hypothesis testing
library(Hmisc) # allows rcorr(as.matrix(df))
library(corrplot) # allows corrplot(cor(df), method = "number")
                                           # method = "circle"
                                           # method = "color"

# https://cran.r-project.org/web/packages/inferr/vignettes/intro.html

# install.packages("inferr")
library(inferr)
set.seed(1)

one_sample_var <- read_csv("C:/users/ryan/desktop/stats/statistics/ProductStrength.csv")


        # one_sample_variance test w inferr
# uses chi_sq, gives 95% conf interval, and statistical significance
infer_os_var_test(one_sample_var, Strength, 5, alternative = "greater")


        # one_sample_variance test w infer
stat <- one_sample_var %>% specify(response = Strength) %>%
  calculate(stat = "sd")

null <- one_sample_var %>%
  specify(response = Strength) %>%
  hypothesize(null = "point", sigma = 5) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "sd")

# ci bounds don't contain 5, meaning statistically significant result
conf_int_val <- null %>% get_ci(stat, level = .95, type = "se") 
null %>% visualize() +
  shade_ci(conf_int_val)


      # two_sample_variances 

two_var_test <- read_csv("C:/users/ryan/desktop/stats/Statistics/VariancesTest.csv") %>%
  clean_names()
infer_ts_var_test(two_var_test, male, female, alternative = "all")

    # look at Pr(F > f), which shows a p-value < .0031


        # Pearson's Correlation

ht_wt <- read_csv(here("Continuous_Data_Examples.csv")) %>% clean_names()

rcorr(as.matrix(ht_wt)) # will return correlation and p values
rcorr(ht_wt$method_1, ht_wt$method_2) # same way of getting same result

cor(ht_wt) # returns correlation
corrplot(cor(ht_wt), method = c("number")) # plot correlation


  # distribution

body_fat <- read_csv(here("body_fat.csv")) %>% clean_names()

# plotting distribution
ggplot(body_fat) +
  geom_histogram(aes(x = percent_fat), binwidth = 2.5, 
                 color = "black", fill = "blue")

cor_bf <- cor(body_fat)

# for goodness-of-fit tests & creating probability plots:

        # fitdistr() from MASS package

# https://www.statmethods.net/advgraphs/probability.html
# The fitdistr( ) function in the MASS package provides 
    # maximum-likelihood fitting of univariate distributions. 
    # The format is fitdistr(x, densityfunction) where x is the 
    # sample data and densityfunction is one of the following: 
    #   "beta", "cauchy", "chi-squared", "exponential", "f", 
    # "gamma", "geometric", "log-normal", "lognormal", 
    # "logistic", "negative binomial", "normal", "Poisson", 
    # "t" or "weibull". 

outliers <- read_csv(here("Outliers.csv")) %>% clean_names()

    # Testing for outliers with grubbs.test()

# https://universeofdatascience.com/how-to-test-for-identifying-outliers-in-r/
grubbs.test()


