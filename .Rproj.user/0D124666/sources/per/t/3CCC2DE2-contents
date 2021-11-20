library(ggplot2) #Used for better looking graphs
library(car) #Salaries dataset is included in this library

data("mtcars") #Dataset included in R(Motor Trend Car Road Tests)
data(Salaries) #Dataset of US professors salaries from 008-009(9 months total)
?mtcars #Explains the mtcars dataset
?Salaries #Explains the salaries dataset

#Basic scatter plot
ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point() +
  labs(title = "Automobile Data", x = "Weight", y = "Miles per Gallon")

#Scatter plot with line of best fit
ggplot(data=mtcars, aes(x=wt, y=mpg)) + 
  geom_point(pch=17, color="blue", size=2) + 
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title = "Automobile Data", x = "Weight", y = "Miles per Gallon")

#Scatter plot with faceting and grouping, by Engine Type and No. of Cylinders
mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, levels=c(0,1), labels=c("V-Engine", "Straight Engine"))
mtcars$cyl <- factor(mtcars$cyl)

ggplot(data=mtcars, aes(x=hp, y=mpg, shape=cyl, color=cyl)) +
  geom_point(size=3) + facet_grid(am~vs) +
  labs(title="Automobile Data By Engine Type", x="Horsepower", y="Miles per Gallon")

#Bubble chart - Good for showing relative magnitude of data points
ggplot(data=mtcars, aes(x=wt, y=mpg, size=disp)) +
  geom_point(shape=21, color="black", fill="cornsilk") +
  labs(title="Bubble chart", x="Weight", y="Miles per Gallon", size="Engine\nDisplacement")

#Using salaries data for checking US professors salaries
#Boxplot of salaries vs staff type
ggplot(Salaries, aes(x=rank, y=salary)) +
  geom_point(position="jitter", color="blue", alpha=.5) +
  geom_boxplot(fill="cornflowerblue", color="black", notch=TRUE) +
  geom_rug(sides="1", color="black")

#Scatter plot of salary controlled by years after obtaining PhD
#Includes staff type and gender as well
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape=sex)) +
  geom_point() + 
  labs(title = "Salary by Years, sex and rank", x="Years since PhD", y="Salary")

#Smooth out the relationship between experience(years, PhD) and salary
#To determine relation between PhD and salary
ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) +
  geom_smooth() + geom_point() +
  labs(title="Relation between PhD and Salary", x="Years since PhD", y="Salary")
#Results are not linear, especially for staff that have graduated a long time ago

ggplot(Salaries, aes(x=yrs.service, y=salary)) +
  geom_smooth() + geom_point() +
  labs(title="Relation between PhD and Salary", x="Years since PhD", y="Salary")
