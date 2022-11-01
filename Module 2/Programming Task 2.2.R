

##############################
##############################
#### Programming Task 2.2 ####
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


## Problem 2.20

# Define possible cat images
cat <- data.frame(type = c("cat", "notCat"))


# Define the prior model
prior <- c(0.08, 0.92)

# Simulate 10,000 articles
set.seed(64)
cat_sim <- sample_n(cat, size = 10000, weight = prior, replace = TRUE)

# Print the distribution of Cat NotCat images
ggplot(cat_sim, aes(x = type)) + 
  geom_bar()

# Summarize the prior
cat_sim %>% 
  tabyl(type) %>% 
  adorn_totals("row")

# Add the likelyhood of having cat to the dataframe depending on the type of image
cat_sim <- cat_sim %>% 
  mutate(data_model = case_when(type == "notCat" ~ 0.5,
                                type == "cat" ~ 0.8))

# Define whether the predictions thinks there are cats in the image
data <- c("no", "yes")

# Simulate cat detection on the image
set.seed(1)
cat_sim <- cat_sim %>%
  group_by(1:n()) %>% 
  mutate(predicted = sample(data, size = 1, 
                        prob = c(1-data_model, data_model)))

# Print the predicted values and the original.
cat_sim %>% 
  tabyl(predicted, type) %>% 
  adorn_totals(c("col","row"))

# Plot the predictions by type of image.
ggplot(cat_sim, aes(x = type, fill = predicted)) + 
  geom_bar(position = "fill")

# Print the posterior approximation
cat_sim %>% 
  filter(predicted == "yes") %>% 
  tabyl(type) %>% 
  adorn_totals("row")




##############################
##############################
##############################



## Problem 2.21

# Define possible condition
disease <- data.frame(type = c("disease", "notDisease"))


# Define the prior model
prior <- c(0.03, 0.97)

# Simulate 10,000 persons
set.seed(64)
disease_sim <- sample_n(disease, size = 10000, weight = prior, replace = TRUE)

# Print the distribution of people with and without disease
ggplot(disease_sim, aes(x = type)) + 
  geom_bar()

# Summarize the prior
disease_sim %>% 
  tabyl(type) %>% 
  adorn_totals("row")

# Add the likelyhood of having the disease to the dataframe
disease_sim <- disease_sim %>% 
  mutate(data_model = case_when(type == "notDisease" ~ 0.07,
                                type == "disease" ~ 0.93))

# Define whether the test predict the disease or not
data <- c("no", "yes")

# Simulate the test
set.seed(1)
disease_sim <- disease_sim %>%
  group_by(1:n()) %>% 
  mutate(predicted = sample(data, size = 1, 
                            prob = c(1-data_model, data_model)))

# Print the predicted values and the original.
disease_sim %>% 
  tabyl(predicted, type) %>% 
  adorn_totals(c("col","row"))

# Plot the predictions by disease or not disease
ggplot(disease_sim, aes(x = type, fill = predicted)) + 
  geom_bar(position = "fill")

# Print the posterior approximation
disease_sim %>% 
  filter(predicted == "yes") %>% 
  tabyl(type) %>% 
  adorn_totals("row")

