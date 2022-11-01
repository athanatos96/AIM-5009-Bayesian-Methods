library(bayesrules)
library(tidyverse)
library(janitor)

# Import data
data(bechdel, package = "bayesrules")

# Get 1980 films
bechel_1980 = bechdel[bechdel$year==1980,]

# Get 1990 films
bechel_1990 = bechdel[bechdel$year==1990,]

# Get 2000 films
bechel_2000 = bechdel[bechdel$year==2000,]

# Get 1980, 1990 and 2000 films
bechel_1980_1990_2000 = bechdel[bechdel$year==1980 | bechdel$year==1990 | bechdel$year==2000,]

# Get % of 1980 films
bechel_1980 %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

# Get % of 1990 films
bechel_1990 %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

# Get % of 2000 films
bechel_2000 %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

# Get % of 1980, 1990 and 2000 films
bechel_1980_1990_2000 %>% 
  tabyl(binary) %>% 
  adorn_totals("row")
