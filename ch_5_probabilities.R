# Binomial distribution

# taken from stats package
library(tidyverse)

# x = vector of quantiles (some event happening); in the case below, an event occuring 0 times, 1 times ... up to 10 times;
# size = number of trials (0 or more)
# prob = probability of success on each trial

?dbinom
# density dbinom -- the density distribution of an event occurring (likelihood x event occurs)

bin <-dbinom(c(0:10), size = 10, prob = 1/6) %>% tibble() %>% rename(., col = .)
bin <- bin %>% mutate(num = row_number())

ggplot() +
  geom_col(bin, mapping = aes(x = num, y = col))

# to find probability of rolling a die (let's say a 6) some number of times,
# filter for the column meeting some specification
# in this case, our column is called num, so let's say we want num > 4;
  # remember that one of our numbers is a 0, so will either want 
  # filter(num >= 5) or filter(num > 4), which both do same filter

bin %>% filter(num >= 5) %>% summarize(sum = sum(col))

# cumulative pbinom (sums to 1) -- the probability for some event occurring
quant <- pbinom(c(0:10), size = 10, prob = 1/6) %>% tibble() %>% rename(., col = .)
quant <- quant %>% mutate(num = row_number())

ggplot() +
  geom_line(quant, mapping = aes(x = num, y = col))


### 

# Geometric distribution - dgeom(vector of quantiles, probability of success)
  # Probability some event occurs at, before,
  # or after some number of rolls

geom <- dgeom(c(0:50), prob = 1/6) %>% tibble() %>% rename(., col = .)
geom <- geom %>% mutate(num = row_number())

ggplot(geom) +
  geom_col(aes(x = num, y = col))

# to find probability until some event occurs, filter num for some range
# ex: want to find sum probability a die rolls a 6 after 7 rolls

geom %>% filter(num >= 7) %>% summarize(sum = sum(col))

### 

# Negative binomial distribution -dnbinom()
  # Used to calculate number of trials required to observe
  # an event a specific number of times

# Ex: how many times to roll a 6 five times; 
  # size = 5 refers to number of times for event to occur
  # x refers to number of trials

dbinom <- dnbinom(x = c(0:100), size = 5, p = 1/6) %>% tibble() %>% rename(., col = .)
dbinom <- dbinom %>% mutate(num = row_number())

ggplot(dbinom) +
  geom_col(aes(x = num, y = col))

max(dbinom$col)
dbinom %>% filter(col > .0356441)

### --- ????

# Hypergeometric distribution - dhyper()
  # Used to calculate prob of x events over n trials,
  # but assumes probability changes b/c drawing
  # occurs from small population w/o replacement

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Hypergeometric.html




