
##############################
##############################
#### Programming Task 2.1 ####
####  Alejandro C Parra   ####
##############################
##############################


#Import some packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("janitor")
library("dplyr")
library("ggplot2")
library("janitor")



## Problem 2.18

# Define possible percentage of population
intolerance <- data.frame(pi = c(0.4, 0.5, 0.6, 0.7))

# Define the prior model
prior <- c(0.10, 0.2, 0.44, 0.26)

# Simulate 10,000 values of pi from the prior
set.seed(64) # set Seed
intolerance_sim <- sample_n(intolerance, size = 10000, weight = prior, replace = TRUE)

# Simulate 10000 random samples, of 80 people
intolerance_sim <- intolerance_sim %>% 
  mutate(y = rbinom(10000, size = 80, prob = pi))

# Check it out
intolerance_sim %>% 
  head(10)

# Summarize the prior
intolerance_sim %>% 
  tabyl(pi) %>% 
  adorn_totals("row")

# Plot y by pi
ggplot(intolerance_sim, aes(x = y)) + 
  stat_count(aes(y = ..prop..)) + 
  facet_wrap(~ pi)

# Focus on simulations with y = 47
intolerance_47 <- intolerance_sim %>% 
  filter(y == 47)

# Summarize the posterior approximation
intolerance_47 %>% 
  tabyl(pi) %>% 
  adorn_totals("row")

# Plot the posterior approximation
ggplot(intolerance_47, aes(x = pi)) + 
  geom_bar()

# Plot the prior approximation
ggplot(intolerance_sim, aes(x = pi)) + 
  geom_bar()




##############################
##############################
##############################


## Problem 2.19

# Define possible percentage of survival
survival <- data.frame(pi = c(0.6, 0.65, 0.7, 0.75))

# Define the prior model
prior <- c(0.3, 0.4, 0.2, 0.1)

# Simulate 10,000 values of pi from the prior
set.seed(64) # set Seed
survival_sim <- sample_n(survival, size = 10000, weight = prior, replace = TRUE)

# Simulate 10000 random samples, of 15 birds
survival_sim <- survival_sim %>% 
  mutate(y = rbinom(10000, size = 15, prob = pi))

# Check it out
survival_sim %>% 
  head(10)

# Summarize the prior
survival_sim %>% 
  tabyl(pi) %>% 
  adorn_totals("row")

# Plot y by pi
ggplot(survival_sim, aes(x = y)) + 
  stat_count(aes(y = ..prop..)) + 
  facet_wrap(~ pi)

# Focus on simulations with y = 10
survival_10 <- survival_sim %>% 
  filter(y == 10)

# Summarize the posterior approximation
survival_10 %>% 
  tabyl(pi) %>% 
  adorn_totals("row")

# Plot the posterior approximation
ggplot(survival_10, aes(x = pi)) + 
  geom_bar()

# Plot the prior approximation
ggplot(survival_sim, aes(x = pi)) + 
  geom_bar()
