# logfile.R

#Packages for data viewing
library(dplyr)
library(ggplot2)
library(lubridate)

solar <- read.table('solardata.csv' ,header=TRUE, sep=',', stringsAsFactors = TRUE)

str(solar)

#How many days worth of data do we have?
#Date_time has to be converted from a string to a timestamp
solar$date_time <- ymd_hms(solar$date_time)

min(solar$date_time)
max(solar$date_time)

#Difference of 3.5 months or 103.9 days
difftime(ymd_hms(max(solar$date_time)), ymd_hms((min(solar$date_time))))

cols <- c("solarpower", "solarvoltage", "solarenergy", "solarcurrent")
#Plot voltage, current, power and energy for 1 day
day <- "2015-12-12"
oneday <- solar[date(solar$date_time) == day, ]
summary(oneday[,cols])

#Plot voltage, current, power and energy for all days
summary(solar[,cols])

#Plot 4 line charts
ggplot(oneday, aes(x=date_time, y=solarvoltage)) + geom_line()
ggplot(oneday, aes(x=date_time, y=solarcurrent)) + geom_line()
ggplot(oneday, aes(x=date_time, y=solarpower)) + geom_line()
ggplot(oneday, aes(x=date_time, y=solarenergy)) + geom_line()

#Plot a Pareto diagram using the culmulative sum(cumsum)
oneday$solarenergy_cumsum <- cumsum(oneday$solarenergy)
ggplot(oneday, aes(x=date_time, y=solarenergy_cumsum)) + geom_line()

#Calculate total solar energy generated in 1 day
solar$date <- as.Date(solar$date_time)
# %>% is called a "pipe" and is from dplyr. Used to make code logical and easy to follow
groupdata <- solar %>% group_by(location, date) %>%
  summarise(total_energy=sum(solarenergy))

#Get energy units per day
#9-10 units of energy are generated per day on average
summary(groupdata$total_energy)

#Plot the energy unit data
ggplot(groupdata, aes(x=date, y=total_energy)) + 
  geom_line(colour="blue", size=1) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15, face="bold")) +
  geom_hline(yintercept = 11, colour = "red") +
  geom_hline(yintercept = 8, colour = "red")
#Red lines using yintercept are used to show when energy is generated
#Solar energy is generated between 8 and 11
#There are surges and sags with no obvious trends
#There is a small dip between Feb to March