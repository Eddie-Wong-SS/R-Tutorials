library(tidyverse)
library(dplyr)

mydata1 <- read.csv("states.csv")

head(mydata1)

sample_n(mydata1, 3) #Obtain 3 random rows of data

set.seed(42) #Sets a certain seed to always obtain the same result
#Set.seed may return different results based on R update version
#Version 3.5 and lower are the same, 3.6 and 4.0 are the same

sample_n(mydata1, 3)

#Select data from 2002 to 2008
mydata2 <- select(mydata1, State:Y2008) #Selects columns from State to 2008
head(mydata2)

mydata2 <- select(mydata2, -Y2008) #remove 2008 column
head(mydata2)

#Using pipe operator to select states starting with "A"
mydata2 %>%
  filter(str_detect(State, "A")) 

#Using pipe operator to select states starting with "Ala"
mydata2 %>%
  filter(str_detect(State, "Ala")) 

#Get average income of states 2002-2007
group_by(mydata2, State) %>%
  summarize(average = mean(Y2002:Y2007))