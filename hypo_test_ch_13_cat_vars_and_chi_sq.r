library(here)
library(tidyverse)
library(janitor)
library(infer)
library(hablar)

# resources:
  # https://www.tutorialspoint.com/r/r_chi_square_tests.htm
  # https://cran.r-project.org/web/packages/infer/vignettes/chi_squared.html
  # http://sthda.com/english/wiki/chi-square-test-of-independence-in-r

star_trek <- read_csv(here("StarTrekFatalities.csv")) %>% clean_names() %>%
  convert(fct(c(color, status)))

    # Base R contingency table and chisq.test()

# creating table without frequency
table_names <- table(star_trek$status, star_trek$color)
# creating matrix without column/row names
matrix_values <- star_trek$frequency %>% matrix(nrow = 2, ncol = 3)
# assigning colnames to values
colnames(matrix_values) <- colnames(table_names) 
# assigning rownames to values
rownames(matrix_values) <- rownames(table_names) 
# converting matrix to table for chisq test
star_trek_cont_table <- matrix_values %>% as.table()
# chisq.test()
chisq.test(star_trek_cont_table) # gives p-value of .04531
chi_test <- chisq.test(star_trek_cont_table)
# Extracting observed counts & expected counts
chi_test$observed
round(chi_test$expected, 2)

# to get Chi-square statistic for each cell 
# (giving a total Chi-square score), take residual and square it:
round(chi_test$residuals^2, 2)
# to get total Chi-square statistic, sum the residuals 
  # (sum of squared errors, like regression):
sum(round(chi_test$residuals^2, 2))

# Access to the values returned by chisq.test() function, with df$var
  # statistic: the value the chi-squared test statistic (chi_test$p.value)
  # parameter: the degrees of freedom
  # p.value: the p-value of the test
  # observed: the observed count
  # expected: the expected count

    ---

# infer chi-squared

  # Creating two columns w/ frequencies from frequency column:

set.seed(1)
st_infer <- star_trek %>% uncount(frequency)

ind <- st_infer %>%
  specify(status ~ color) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "Chisq")

null <- st_infer %>%
  specify(status ~ color) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq")

#  visualizing ci

ci <- get_confidence_interval(null)
ci # lower bound: .0162; upper bound: 6.97
null %>% visualize() +
  shade_ci(ci)

p_value <- get_pvalue(null, obs_stat = ind, direction = "right")
p_value # .04

null %>% visualize() +
  shade_p_value(p_value, direction = "right")

# infer chisq_test() -- gives same p-value as chisq.test()
chisq_test(st_infer, status ~ color)

    # Note: both chisq_test() and chisq.test() give Pearson Chi-Square
          # and not likelihood ratio chi-square()
