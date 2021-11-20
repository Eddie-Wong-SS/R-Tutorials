# CETM 50 Technology Management Assignment 2
#Data simulation, transformation and analysis using dplyr
#Simulating Facebook ads in several dataframes
#Ran on R version 4.0.2("Taking Off Again") as of 22/01/2021
library(tidyverse)

#Generate number of ad campaigns 
campaign <- 1:5

#Seed used for assignment is seed 255
set.seed(255)
#Generate number of ads per campaign(max of 50)
ad <- rbinom(n = 5, size = 10, prob = 0.7)

#Generate ids for each ad and campaign
campid <- seq(campaign)
adid <- unlist(sapply(ad, seq))
#Associates each adid with their respective campaigns
campid <- rep(campid, ad)
#Stores the number of total ads(Used to define number of rows to be simulated) 
amount <- length(campid)

#-----------Data Simulation-------------

#dataframe showing average ages the ad is mainly shown
df_adage <-
  data.frame(
    campid = factor(campid),
    adid = factor(adid),
    age = replicate(n = amount, expr = min(sample(15:60, size=2, replace=TRUE)))
  ) %>% tibble::as_tibble()

print(df_adage, tbl_df, n = nrow(df_adage))

#dataframe showing the gender to whom the ad is mainly shown
df_adgen <-
  data.frame(
    campid = factor(campid),
    adid = factor(adid),
    gender = sample(c("M", "F"), amount, replace=TRUE, prob = c(0.6, 0.4))
  ) %>% tibble::as_tibble()

print(df_adgen, tbl_df, n = nrow(df_adgen))

#dataframe that shows the interest of people for each ad
df_interest <-
  data.frame(
    campid = factor(campid),
    adid = factor(adid),
    #impressions: number of times ad is shown to people
    impressions = replicate(n = amount, 
                            expr = max(sample(500:5000, size=1, replace=TRUE)))
  ) %>% tibble::as_tibble()

#Sets argument lengths to be same as dataframe to run all rows
length(impressions > 3500) == amount
length(impressions > 2000) == amount
length(impressions > 1000) == amount

#Sets number of clicks per ad based on impressions
df_interest$clicks <-
  ifelse(df_interest$impressions > 3500,
         sample(15:25, nrow(df_interest), replace = TRUE),
         ifelse(df_interest$impressions > 2000,
                sample(17:14, nrow(df_interest), replace = TRUE),
                ifelse(df_interest$impressions > 1000,
                       sample(3:6, nrow(df_interest), replace = TRUE),
                       sample(0:2, nrow(df_interest), replace = TRUE))))

#Click-through-rate: shows rate that impressions turn into clicks
#High Click-through-rate implies the ad is engaging to the target audience
df_interest$CTR <-
  ((df_interest$clicks / df_interest$impressions) * 100)

print(df_interest, tbl_df, n = nrow(df_interest))

#dataframe that shows the number of product buyers during the ad's lifetime
df_conv <-
  data.frame(
    campid = factor(campid),
    adid = factor(adid),
    #conversion: Number who bought the product during the ad's lifetime
    conversion = replicate(n = amount, 
                           expr = max(sample(0:4, size=1, replace=TRUE)))
  ) %>% tibble::as_tibble()

print(df_conv, tbl_df, n = nrow(df_conv))

#dataframe that shows how much is paid per ad to Facebook(USD)
df_paid <-
  data.frame(
    campid = factor(campid),
    adid = factor(adid),
    #spent: Amount paid per ad to Facebook
    spent = replicate(n = amount, 
                           expr = min(sample(1000:3000, size=1, replace=TRUE)))
  ) %>% tibble::as_tibble()

#Cost Per Click: Amount charged by Facebook for every click on the ad
df_paid$CPC <-
  (df_interest$clicks / df_paid$spent )
#rounds Cost Per Click to 4 decimal points for readability
df_paid$CPC <- 
  round(df_paid$CPC, 4)

print(df_paid, tbl_df, n = nrow(df_paid))

#-----------Data Transformation---------------

#shows the ratio of interest versus amount spent
df_interest %>%
  inner_join(df_paid, by = c("campid", "adid")) %>%
  print(n = nrow(df_interest))

#shows the coversion ratio vs average age and gender to whom the ads are shown
df_adage %>%
  left_join(df_adgen, by = c("campid", "adid")) %>%
  left_join(df_conv, by = c("campid", "adid")) %>%
  print(n = nrow(df_adage))

#shows the ratio of interest vs conversion rate per ad
df_interest %>%
  left_join(df_conv, by = c("campid", "adid")) %>%
  print(n = nrow(df_interest))

#Shows the relationship between age, gender and interest per ad
df_adage %>%
  left_join(df_adgen, by = c("campid", "adid")) %>%
  left_join(df_interest, by = c("campid", "adid")) %>%
  print(n = nrow(df_adage))

#Shows the relationship between money spent on ads to conversions
df_paid %>%
  left_join(df_conv, by = c("campid", "adid")) %>%
  print(n = nrow(df_paid))

#-----------Data Analysis-------------------

#Get the total amount of money spent on Facebook ads(USD)
df_paid %>%
  summarise(Total = sum(df_paid$spent))

#Get the total amount of conversions
df_conv %>%
  summarise(Totalconv = sum(df_conv$conversion))

#New dataframe to get conversions by average age/gender
df_agegenconv <-
  df_adage %>%
  left_join(df_adgen, by = c("campid", "adid")) %>%
  left_join(df_conv, by = c("campid", "adid")) 

#New dataframe to get interest by average age/gender
df_agegenint <-
  df_adage %>%
  left_join(df_adgen, by = c("campid", "adid")) %>%
  left_join(df_interest, by = c("campid", "adid"))

#Total conversions for males
filter(df_agegenconv, str_detect(gender, "M")) %>%
  summarise(Conversion = sum(conversion))
#Total conversions for females
filter(df_agegenconv, str_detect(gender, "F")) %>%
  summarise(Conversion = sum(conversion))

#Total number of males and female audiences
df_adgen %>%
  group_by(gender) %>%
  summarise(gender_count=n())

#Total conversions for ads shown to average ages 40 and above
filter(df_agegenconv, age >= 40) %>%
  summarise(Conversion = sum(conversion))
#Total conversions for ads shown to average ages below 40
filter(df_agegenconv, age < 40) %>%
  summarise(Conversion = sum(conversion))

#sorts the interest shown by average age and gender in ascending order
df_agegenint %>%
  group_by(age, gender) %>%
  arrange(age, gender) %>%
  print(n = nrow(df_agegenint))

#Customise the Box plot
options(repr.plot.width=4, repr.plot.height=3)

#Box plot showing conversion amount per ad campaign
ggplot(df_adage, aes(campid, age)) + geom_boxplot() +
  labs(x = "Campaign", y = "Conversions")

#Box plot showing amount spent on advertising per ad campaign
ggplot(df_paid, aes(campid, spent)) + geom_boxplot() +
  labs(x = "Campaign", y = "Advertising Spend")

#Box plot showing Cost Per Click of ads for each ad campaign
ggplot(df_paid, aes(campid, CPC)) + geom_boxplot() +
  labs(x = "Campaign", y = "Cost Per Click") 

#Box plot showing number of impressions for ads in each ad campaign
ggplot(df_interest, aes(campid, impressions)) + geom_boxplot() + 
  labs(x = "Campaign", y = "Impressions")

#Box plot showing number of clicks for ads in each ad campaign
ggplot(df_interest, aes(as.factor(campid), clicks)) + geom_boxplot() +
  labs(x = "Campaign", y = "Number of clicks") 

#Box plot showing Click-Through-Rate for ads in each ad campaign
ggplot(df_interest, aes(campid, CTR)) + geom_boxplot() + 
  labs(x = "Campaign", y = "Click-Through-Rate") 