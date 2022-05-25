library(hablar)
library(tidyverse)
library(janitor)
library(infer)

set.seed(1)

# Anova


    # One-way ANOVA in base R - aov()

setwd("C:/users/ryan/desktop/stats")
anova_one_way <- read_csv("OneWayExample.csv") %>% clean_names() %>% convert(fct(sample))

# summarize information about groups
anova_one_way %>% group_by(sample) %>% summarize(mean = mean(strength),
                                                 sd = sd(strength))

# graph groups
ggplot(anova_one_way, aes(x = sample, y = strength)) +
  geom_boxplot()

# Example scenario: comparing strength of raw material from four suppliers

one_way <- aov(strength ~ sample, anova_one_way)
summary(one_way)


# How to read:
  # # https://www.scribbr.com/statistics/anova-in-r/

  # The F-value column is the test statistic from the F test. This is the mean square of each 
  # independent variable divided by the mean square of the residuals. The larger the F value, 
  # the more likely it is that the variation caused by the independent variable is real and 
  # not due to chance.
  
  # The Pr(>F) column is the p-value of the F-statistic. This shows how likely it is that 
  # the F-value calculated from the test would have occurred if the null hypothesis of no 
  # difference among group means were true.


    # ANOVA with infer

# https://cran.r-project.org/web/packages/infer/vignettes/anova.html

# calculate observed stat, F

obs_f_stat <- anova_one_way %>%
  specify(strength ~ sample) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F")

null <- anova_one_way %>%
  specify(strength ~ sample) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

null %>% visualize() +
  shade_p_value(obs_f_stat, direction = "right")

# calculate the p value from the observed statistic and null distribution
p_value <- null %>%
  get_p_value(obs_stat = obs_f_stat,
              direction = "greater")

p_value


### Post-Hoc Tests with One-Way ANOVA

anova_one_way_ph <- read_csv("PostHocTests.csv") %>% clean_names() 

# summary stats
anova_one_way_ph %>% group_by(material) %>%
  summarize(mean = mean(strength),
            sd = sd(strength),
            var = var(strength))
# graph
ggplot(anova_one_way_ph, aes(x = material, y = strength)) +
  geom_boxplot()

# convert to factors
anova_one_way_ph <- anova_one_way_ph %>% convert(fct(material))

###

# base r post-hoc Tukey test w/ One-Way ANOVA

aov_ph_sum <- aov(strength ~ material, anova_one_way_ph)
summary(aov_ph_sum)

# Base R Tukey's test:

# Tukey's Test
TukeyHSD(aov_ph_sum, conf.levels = .95) # defaults to .95

# Cleaning Tukey's Test to plot confidence intervals
tukey_df <- Tukey_ph$material %>% data.frame() 
tukey_df_clean <- rownames_to_column(tukey_df, var = "groups")

# plotting Confidence Intervals
  # https://newbedev.com/r-ggplot2-pointrange-example
ggplot(tukey_df_clean, aes(x = groups, 
                           ymin = lwr, 
                           ymax = upr, 
                           y = diff)) +
  geom_pointrange() +
  labs(x = "Group",
       y = "Confidence Interval") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip()


  # Compare to original boxplot
ggplot(anova_one_way, aes(x = sample, y = strength)) +
  geom_boxplot()


# infer ANOVA

one_way_ph <- anova_one_way_ph %>%
  specify(strength ~ material) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F")

one_way_ph_null <- anova_one_way_ph %>%
  specify(strength ~ material) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

# graphing p-value -- direction can be "greater" or "right"
one_way_ph_null %>% visualize() +
  shade_p_value(one_way_ph, direction = "right")

one_way_ph_null %>% get_p_value(one_way_ph, direction = "right")


    # Dunnett's test/method

# https://www.statology.org/dunnetts-test-r/

install.packages("DescTools")
library(DescTools)

# Treats A as control
DunnettTest(anova_one_way_ph$strength, anova_one_way_ph$material)

# Output shows that pval for A-B < .0355, making it stat. sig.


two_way_ANOVA <- read_csv("C:/users/ryan/desktop/stats/Two-WayANOVAExamples.csv") %>% clean_names()

two_way_ANOVA <- two_way_ANOVA %>% select(-x4) %>% 
  convert(fct(c(gender, major, food, condiment)))

glimpse(two_way_ANOVA)

# https://www.statology.org/q-q-plot-r/

two_way <- aov(income ~ major * gender, two_way_ANOVA)

par(mfrow = c(2,2))
plot(two_way)

summary(two_way)

two_way$coefficients


# There are six bands b/c, in the aov above, there are two IVs (one IV has 3 levels, one IV has 2 levels; 3x2 = 6 total bands)


# qqnorm(two_way$residuals) # creates qqnorm plot
# qqline(two_way$residuals) # places line on plot

# In general, if the data points fall along a straight diagonal line 
# in a Q-Q plot, then the dataset likely follows a normal distribution.

# understanding plot() output -
  # https://www.contrib.andrew.cmu.edu/~achoulde/94842/homework/regression_diagnostics.html
  # https://data.library.virginia.edu/diagnostic-plots/
  

    # two way ANOVA with interaction

# creating interaction plots in r
https://www.geeksforgeeks.org/how-to-create-interaction-plot-in-r/
  
  









