library(dplyr) #Included as part of tidyverse, I just put in for fun
library(tidyverse)

#Simulating data to use dplyr functions
#10 in all 3 rows are to generate 10 samples
set.seed(1)
#Set.seed may return different results based on R update version
#Version 3.5 and lower are the same, 3.6 and 4.0 are the same

my_df <-
  data.frame(
    sex = sample(c("Male", "Female"), 10, TRUE),
    age = sample(20:60, 10, TRUE),
    IQ = rnorm(10, 100, 15) #100 is centered, 15 is standard deviation
  ) %>% tibble::as_tibble()

my_df #Prints out the full tibble(up to 10 and must be >5)

print(my_df, table_df, n = 5) #To print a specific number or for >10 rows

#(filter is case sensitive)
#Select only males
my_df %>%
  filter(sex == "Male")

#Get the youngest(may return duplicates if >1 rows have same age)
my_df %>%
  filter(age == min(age))

#Select males older than 30
my_df %>%
  filter(sex == "Male", age > 30)

#Select 1 column
my_df %>%
  select(IQ)

#Average age and standard deviation
my_df %>%
  group_by(sex) %>%
  summarise(meanAge = mean(age), stan_dev = sd(age))

#Count the number of people with the same age
my_df %>%
  group_by(age) %>%
  summarise(Amount = n())

#Simulate 3 tables
#Use join to restructure and join 3 tables
set.seed(1)
ulog <- data.frame(
  user_id = sample(c(1,2,3), 10, TRUE),
  item_id = sample(c(1,2,3), 10, TRUE),
  correct = sample(c(0,1), 10, TRUE)
) %>% tibble :: as_tibble()

users <- data.frame(
  user_id = c(1,2,4),
  age = c(20,20,30)
) %>% tibble :: as_tibble()

items <- data.frame(
  item_id = 1:3,
  item = c("1+1", "2*2", "3/3")
) %>% tibble :: as_tibble()

#left_join
ulog %>%
  left_join(users, "user_id") %>%
  left_join(items, "item_id")

#inner_join (to avoid "NA" data value)
ulog %>%
  inner_join(users, "user_id")