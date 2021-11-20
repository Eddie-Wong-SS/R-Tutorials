library(tidyverse)
set.seed(1)
load("kenscars.RData")

#For dataframes, '=' is the correct format, not '<-'
costs <- data.frame(
  carnames = kenscars$carnames,
  cost = sample(20000:60000, nrow(kenscars), TRUE),
  stringsAsFactors = TRUE #Peserves the format of string-like data
) %>% tibble::as_tibble()

head(costs)

#Simulate 20 sales over 3 months
sales <- data.frame(
  carnames = sample(kenscars$carnames,20, TRUE),
  month = sample(c("September", "OCtober", "November"), 20, TRUE),
  stringsAsFactors = TRUE
) %>% tibble::as_tibble()

head(sales)

#table() function is good for freq count of categorial values that appear 
#in a column
table(sales$carnames)

table(sales$month)

#How much revenue in September
sept <-
  sales %>%
  filter(month == "September")

head(sept)

septcash <-
  sept %>%
  left_join(costs, by = "carnames")

sum(septcash$cost)



#inner join will match pairs with equal keys, but no duplicates
septmoney <-
  sept %>%
  inner_join(costs, by = "carnames")

sum(septmoney$cost)