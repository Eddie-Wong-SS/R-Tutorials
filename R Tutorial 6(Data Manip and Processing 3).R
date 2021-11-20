library(tidyverse)

load("kenscars.RData")
data("mtcars")

kenscars

mtcars

head(kenscars)
head(mtcars)

#Filter for mercedes with >15 miles per gallon capacity
my_df <- filter(kenscars, mpg > 15, str_detect(carnames, "Merc"))
my_df

#Get total mpg of my_df
my_df %>%
  summarise(Frequency = sum(mpg))

#Sumarise mean weight, max miles per gallon and min cylinders of all cars
summarise(kenscars, mean_weight = mean(wt),
          max_mpg = max(mpg),
          min_cylinder = min(cyl))

#Get same results as lines 13 - 15 but using only kenscars dataset
filter(kenscars, mpg > 15, str_detect(carnames, "Merc")) %>%
  summarise(Frequency = sum(mpg))

#using group_by, more piping and filtering using weight(wt), cylinders(cyl)
#Automatic/manual(am)
kenscars %>%
  filter(wt>3.5) %>%
  group_by(cyl, am) %>%
  summarise(mean = mean(mpg))

#Using mtcars dataset
#Select cars > 3 tonnes and calculate mean horsepower
mtcars %>%
  filter(wt>3) %>%
  summarise(mean_hp = mean(hp))

#How many cars of each number of gears are present?
#Using summarise()
mtcars %>%
  group_by(gear) %>%
  summarise(cars = n())

#USing tally()
mtcars %>%
  group_by(gear) %>%
  tally( sort = FALSE, name = "Number")

#Back to kenscars dataset
#Using mpg and hp to find specific cars
#Find cars with > 75 horsepower and >25 miles per gallon
filter(kenscars, mpg > 25, hp >75)

#Sorting the order of data frames
#desc() means descending
#my_df only has Mercedes cars in it
my_df %>%
  arrange(desc(cyl), desc(mpg))

#Using select and distinct
#Works the same way as SQL
#carb means carburettor
distinct(select(kenscars, carb))

#Count the number of cars for each cylinder amount and sort in descending order
kenscars %>%
  group_by(cyl) %>%
  summarise(cylinder_count = n()) %>%
  arrange(desc(cylinder_count))