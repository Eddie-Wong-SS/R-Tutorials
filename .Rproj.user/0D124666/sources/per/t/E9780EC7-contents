library(dplyr) #Used for data science
library(ggplot2) #For better looking graphs

iot <- read.table('IoTData.csv', header=TRUE, sep=',', stringsAsFactors = TRUE)

str(iot) #Examine column names and data types

unique(iot$Product_Qty_Unit) #KG indicates kilogram is the basic unit of quantity
unique(iot$Product_Name) #Indicates only one type of product in data
unique(iot$Site_location) #Confirms the data is from the Pune factory

summary(iot$Total_Manufacturing_Time_mins) #Shows long long a product takes to be completed
#Summary shows some outliers
#Min is 0.0 which is unrealistic
#Max is 2880 which is far higher than the mean and 3rd quadrant

#Using histograms to determine how well data is distributed
ggplot(data = iot, aes(Order_Quantity)) + geom_histogram(binwidth=500)
ggplot(data = iot, aes(Produced_Quantity)) + geom_histogram(binwidth=500)

#Create a frequency polygon plot to compare the two histograms
ggplot(data = iot) + geom_freqpoly(binwidth=500, aes(Order_Quantity), 
                                   color="red", size = 1) +
  geom_freqpoly(binwidth=500, aes(Produced_Quantity), 
                color="blue", size = 1)

#Check for deviations by subtracting Produced Quantity from Order quantity
ggplot(data = iot) + geom_freqpoly(binwidth=10, 
                                   aes(abs(Order_Quantity - Produced_Quantity)))
#There are mutiple counts of 0-500 unit deviations from the order
#This shows problems with the manufacturing process