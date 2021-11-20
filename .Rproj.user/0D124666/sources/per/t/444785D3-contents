#Fuzzy rules for this example were made by the author and is subjective
library(sets)
sets_options("universe", seq(1, 100, 0.5))

#Build a fuzzy set about weather
#Specify attributes for each variable and give them values 
variables <- set(
  temperature = fuzzy_partition(varnames = c(cold = 30, good = 70, hot = 90),
                                sd = 5.0), #using Fahrenheit
  humidity = fuzzy_partition(varnames = c(dry = 30, good = 60, wet = 80), 
                             sd = 3.0),
  precipitation = fuzzy_partition(varnames = c(no.rain = 30, little.rain = 60,
                                               rain = 90), sd = 7.5),
  weather = fuzzy_partition(varnames = c(bad = 40, ok = 65, perfect = 80),
                            FUN = fuzzy_cone, radius = 10)
)

# Fuzzy rules
rules <- set(
  fuzzy_rule(temperature == good && humidity == dry &&
               precipitation == no.rain, weather == perfect),
  fuzzy_rule(temperature == hot && humidity == wet &&
               precipitation == rain, weather == bad),
  fuzzy_rule(temperature == cold, weather == bad),
  fuzzy_rule(temperature == good || humidity == good ||
               precipitation == little.rain, weather == ok),
  fuzzy_rule(temperature == hot && precipitation == little.rain,
             weather == ok),
  fuzzy_rule(temperature == hot && humidity == dry &&
               precipitation == little.rain, weather == ok)
)

model <- fuzzy_system(variables, rules)
print(model)

plot(model)

#Testing the system(tempt = 75, humid = 0, precipitation = 70)
example.1 <- fuzzy_inference(model, list(temperature = 75, humidity = 0,
                                         precipitation = 70))
#Defuzzify the example to get actual numbers
gset_defuzzify(example.1, "centroid")

plot(example.1)
#Conclusion: weather is .60 ok

#Testing the system(Temperature = 30, humidity = 0 and precipitation = 70)
example.2 <- fuzzy_inference(model, list(temperature = 30, humidity = 0,
                                         precipitation = 70))
gset_defuzzify(example.2, "centroid")

plot(example.2)
#"Ok" weather is 0.4
#New smooth peak with maximum of 1 is also made
#This means probability of weather being bad is 1

sets_options("universe", NULL)  # Reset the universe