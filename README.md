## Citibikes ##

#Load the library

library('dplyr')
library('ggplot2')
library('lubridate')

citibike <- read.csv(file = "citibike.csv")

#Explore the dataset

dim(citibike)
str(citibike)
colnames(citibike)
head(citibike)
tail(citibike)

citibike$DayOfWeek <- recode(citibike$DayOfWeek,
                             
"1"="Sunday",
"2"="Monday",
"3"="Tuesday",
"4"="Wednesday",
"5"="Thursday",
"6"="Friday",
"7"="Saturday")

#Summary Statistics

summary(citibike$Age)
summary(citibike$TripDurationMinutes)
summary(citibike$StartPerCapitaIncome)
summary(citibike$EndPerCapitaIncome)
summary(citibike$StartPctHouseholdsNoVehicle)
summary(citibike$EndPctHouseholdsNoVehicle)

#Compute frequency

table(citibike$UserType)
table(citibike$Gender)

#Plot the number of trips

ggplot(citibike, aes(x = DayOfWeek,)) + 
  geom_bar() +
  geom_bar(colour = "white", fill = "dodgerblue") +
  ggtitle("Number of Trips by Day of the Week") +
  ylab("Trips") + xlab("Day of Week") 

ggplot(citibike, aes(x = UserType, , fill = UserType)) + 
  geom_bar() +
  geom_bar(colour = "white") +
  ggtitle("Number of Trips by User Type") +
  ylab("Trips") + xlab("User Type") 
  
ggplot(citibike, aes(x = Age)) + 
  geom_bar() +
  geom_bar( colour = "dodgerblue", fill = "dodgerblue") +
  ggtitle("Number of Trips by Age") +
  ylab("Trips") + xlab("Age") 

ggplot(citibike, aes(x = Gender, fill = Gender)) + 
  geom_bar() +
  ggtitle("Number of Trips by Gender") +
  ylab("Trips") + xlab("Gender") 

ggplot(citibike, aes(x = Temperature)) + 
  geom_bar()+
  geom_bar( colour = "dodgerblue", fill = "dodgerblue") +
  ggtitle("Number of Trips by Temperature") +
  ylab("Trips") + xlab("Temperature") 

#Missing data

sum(is.na(citibike))
sum(is.na(citibike$Age))
colSums(is.na(citibike)) 

#The total amount of missing values was 10,681. The Age column had 2,789 missing value. However, we concluded that the amount of missing values was not significant because the data set has a total of 859,294 values. It is 1.22% of the overall data.

#Convert the startdatatime to date/time

citibike$StartDateTime <- as.POSIXct(citibike$StartDateTime,"%m/%d/%Y %H:%M",tz="America/New_York")

#Add daytime vs evening column as well as DemandDate

citibike <- citibike %>% mutate(DemandTime = ifelse(hour(StartDateTime)<=12, "daytime", "evening"), DemandDate = as.Date(citibike$StartDateTime))

#Estimate the demand for each DemandDate, StartStationId, EndStationId, DemandTime

citibikeDemand <- citibike %>% group_by(DemandDate, UserType, StartStationId, StartNeighborhood,EndStationId, EndNeighborhood,DemandTime, Temperature, DailySnowFall, DailyPrecipitation, StartPerCapitaIncome, EndPerCapitaIncome, StartPctHouseholdsNoVehicle, EndPctHouseholdsNoVehicle, TripDurationMinutes, DistanceMiles) %>% summarise(Demand = n())

#Scale the demand by the frequency of the StartStation and time

#Create a dataset that contains the demand for each StartStationId and DemandTime

citibikeDemandStations <- citibikeDemand %>% group_by(StartStationId, DemandTime) %>% summarise(DemandStations = n())

#Join the citibikeDemandStations dataset to the citibikeDemand dataset

citibikeDemand <- left_join(citibikeDemand, citibikeDemandStations, by = c('StartStationId','DemandTime'))

#Caculated the Scaled Demand

citibikeDemand$Demand <- citibikeDemand$Demand * citibikeDemand$DemandStations

summary(citibikeDemand$Demand)

#Calculating mean

summary(citibike$Temperature) 
summary(citibike$DailySnowFall) 
summary(citibike$DailyPrecipitation) 
summary(citibike$TripDurationMinutes) 
summary(citibike$DistanceMiles) 

#Mean for Temperature is 62.72 degrees
#Mean for DailySnowFall is 0.0903 inches
#Mean for DailyPrecipitation is 0.0832 inches
#Mean for TripDurationMinutes is 12.97 minutes
#Mean for DistanceMiles is 1.12 miles

dim(citibikeDemand)
str(citibikeDemand)
colnames(citibikeDemand)
head(citibikeDemand)
tail(citibikeDemand)

sum(is.na(citibikeDemand))
sum(is.na(citibikeDemand$Demand))

#Identify Demand Patterns

mean(citibikeDemand$Demand)
sd(citibikeDemand$Demand)

cor(citibikeDemand$Demand, citibikeDemand$TripDurationMinutes)
cor(citibikeDemand$Demand, citibikeDemand$DistanceMiles)

#Mean and SD for predictive variables

mean(citibikeDemand$Demand)
sd(citibikeDemand$Demand)
mean(citibikeDemand$TripDurationMinutes)
sd(citibikeDemand$TripDurationMinutes)
mean(citibikeDemand$DistanceMiles)
sd(citibikeDemand$DistanceMiles)

#Propability Table
#2-Way Frequency Table
#First argument will be rows, second argument will be columns

frequencyTable <- table(citibikeDemand$Demand,citibikeDemand$UserType) 

#2-Way Probability Table

probabilityTable <- prop.table(frequencyTable) 

probabilityTable <- rbind(probabilityTable,c( sum(probabilityTable[,"Subscriber"]),sum(probabilityTable[,"Customer"])))

probabilityTable

#Histogram

bin_width <- 2 * IQR(citibikeDemand$Demand) / length(citibikeDemand$Demand)^(1/3)
ggplot(citibikeDemand, aes(x = Demand)) + 
geom_histogram(binwidth = bin_width) +
geom_histogram( colour = "white", fill = "dodgerblue") +
ggtitle("Distribution of Demand")

#Bar Plot

ggplot(citibikeDemand, aes(x = Demand, y = TripDurationMinutes)) + 
geom_bar(stat = "identity", fill="dodgerblue", color="dodgerblue") +
ggtitle("Trip Duration per Demand Levels")

#Scatterplots

ggplot(citibikeDemand, mapping = aes(x = Demand, y = StartPerCapitaIncome )) + 
geom_point() +
geom_point( colour = "dodgerblue") +
ggtitle("Neighborhood Income per Demand Levels") +
ylab("StartPerCapitaIncome") + xlab("Demand") 

ggplot(citibikeDemand, mapping = aes(x = Demand, y = DistanceMiles)) + 
geom_point() +
geom_point( colour = "dodgerblue") +
ggtitle("Distance Traveled per Demand Levels") +
ylab("Distance in Miles") + xlab("Demand") 

#Linear Regression

citireg <- lm(Demand ~ StartStationId + EndStationId + DemandTime + StartPerCapitaIncome + StartPctHouseholdsNoVehicle + TripDurationMinutes + DistanceMiles, data = citibikeDemand)

summary(citireg)

#Temperature P-value is more than 0.05, meaning no significant impact on predicting demand

#Predict DayTime and Evening for 5 stations

### Murray Hill Predictions ###

murray_predict1 <- data.frame(DemandTime = "daytime", StartStationId = 519, EndStationId = 519, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 90,000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict1)

murray_predict2 <- data.frame(DemandTime = "daytime", StartStationId = 519, EndStationId = 3164, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 90,000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict2)

murray_predict3 <- data.frame(DemandTime = "daytime", StartStationId = 519, EndStationId = 3423, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 65000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict3)

murray_predict4 <- data.frame(DemandTime = "daytime", StartStationId = 519, EndStationId = 326, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 85000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict4)

murray_predict5 <- data.frame(DemandTime = "daytime", StartStationId = 519, EndStationId = 473, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 30000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict5)

murray_predict6 <- data.frame(DemandTime = "evening", StartStationId = 519, EndStationId = 519, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 90,000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict6)

murray_predict7 <- data.frame(DemandTime = "evening", StartStationId = 519, EndStationId = 3164, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 90,000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict7)

murray_predict8 <- data.frame(DemandTime = "evening", StartStationId = 519, EndStationId = 3423, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 65000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict8)

murray_predict9 <- data.frame(DemandTime = "evening", StartStationId = 519, EndStationId = 326, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 85000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict9)

murray_predict10 <- data.frame(DemandTime = "evening", StartStationId = 519, EndStationId = 473, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 30000, StartPctHouseholdsNoVehicle = .65, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = murray_predict10)

### Lincoln Square Predictions ###

lincoln_predict1 <- data.frame(DemandTime = "daytime", StartStationId = 3164, EndStationId = 519, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict1)

lincoln_predict2 <- data.frame(DemandTime = "daytime", StartStationId = 3164, EndStationId = 3164, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 90,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict2)

lincoln_predict3 <- data.frame(DemandTime = "daytime", StartStationId = 3164, EndStationId = 3423, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict3)

lincoln_predict4 <- data.frame(DemandTime = "daytime", StartStationId = 3164, EndStationId = 326, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict4)

lincoln_predict5 <- data.frame(DemandTime = "daytime", StartStationId = 3164, EndStationId = 473, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict5)

lincoln_predict6 <- data.frame(DemandTime = "evening", StartStationId = 3164, EndStationId = 519, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict6)

lincoln_predict7 <- data.frame(DemandTime = "evening", StartStationId = 3164, EndStationId = 3164, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 90,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict7)

lincoln_predict8 <- data.frame(DemandTime = "evening", StartStationId = 3164, EndStationId = 3423, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict8)

lincoln_predict9 <- data.frame(DemandTime = "evening", StartStationId = 3164, EndStationId = 326, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict9)

lincoln_predict10 <- data.frame(DemandTime = "evening", StartStationId = 3164, EndStationId = 473, StartPerCapitaIncome = 100,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .737, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = lincoln_predict10)

### Brooklyn Predictions ###

brook_predict1 <- data.frame(DemandTime = "daytime", StartStationId = 3423, EndStationId = 519, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict1)

brook_predict2 <- data.frame(DemandTime = "daytime", StartStationId = 3423, EndStationId = 3164, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict2)

brook_predict3 <- data.frame(DemandTime = "daytime", StartStationId = 3423, EndStationId = 3423, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict3)

brook_predict4 <- data.frame(DemandTime = "daytime", StartStationId = 3423, EndStationId = 326, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict4)

brook_predict5 <- data.frame(DemandTime = "daytime", StartStationId = 3423, EndStationId = 473, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict5)

brook_predict6 <- data.frame(DemandTime = "evening", StartStationId = 3423, EndStationId = 519, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict6)

brook_predict7 <- data.frame(DemandTime = "evening", StartStationId = 3423, EndStationId = 3164, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict7)

brook_predict8 <- data.frame(DemandTime = "evening", StartStationId = 3423, EndStationId = 3423, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict8)

brook_predict9 <- data.frame(DemandTime = "evening", StartStationId = 3423, EndStationId = 326, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict9)

brook_predict10 <- data.frame(DemandTime = "evening", StartStationId = 3423, EndStationId = 473, StartPerCapitaIncome = 65,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .544, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = brook_predict10)

### East Village Predictions ###
ev_predict1 <- data.frame(DemandTime = "daytime", StartStationId = 326, EndStationId = 519, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict1)

ev_predict2 <- data.frame(DemandTime = "daytime", StartStationId = 326, EndStationId = 3164, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict2)

ev_predict3 <- data.frame(DemandTime = "daytime", StartStationId = 326, EndStationId = 3423 , StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict3)

ev_predict4 <- data.frame(DemandTime = "daytime", StartStationId = 326, EndStationId = 326, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict4)

ev_predict5 <- data.frame(DemandTime = "daytime", StartStationId = 326, EndStationId = 473, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict5)

ev_predict6 <- data.frame(DemandTime = "evening", StartStationId = 326, EndStationId = 519, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict6)

ev_predict7 <- data.frame(DemandTime = "evening", StartStationId = 326, EndStationId = 3164, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict7)

ev_predict8 <- data.frame(DemandTime = "evening", StartStationId = 326, EndStationId = 3423 , StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict8)

ev_predict9 <- data.frame(DemandTime = "evening", StartStationId = 326, EndStationId = 326, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict9)

ev_predict10 <- data.frame(DemandTime = "evening", StartStationId = 326, EndStationId = 473, StartPerCapitaIncome = 85,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .79, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = ev_predict10)

### Lower East Side Predictions ###
les_predict1 <- data.frame(DemandTime = "daytime", StartStationId = 473, EndStationId = 519, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict1)

les_predict2 <- data.frame(DemandTime = "daytime", StartStationId = 473, EndStationId = 3164, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict2)

les_predict3 <- data.frame(DemandTime = "daytime", StartStationId = 473, EndStationId = 3423, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict3)

les_predict4 <- data.frame(DemandTime = "daytime", StartStationId = 473, EndStationId = 326, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict4)

les_predict5 <- data.frame(DemandTime = "daytime", StartStationId = 473, EndStationId = 473, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict5)

les_predict6 <- data.frame(DemandTime = "evening", StartStationId = 473, EndStationId = 519, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict6)

les_predict7 <- data.frame(DemandTime = "evening", StartStationId = 473, EndStationId = 3164, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 100,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict7)

les_predict8 <- data.frame(DemandTime = "evening", StartStationId = 473, EndStationId = 3423, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 65,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict8)

les_predict9 <- data.frame(DemandTime = "evening", StartStationId = 473, EndStationId = 326, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 85,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict9)

les_predict10 <- data.frame(DemandTime = "evening", StartStationId = 473, EndStationId = 473, StartPerCapitaIncome = 30,000, EndPerCapitaIncome = 30,000, StartPctHouseholdsNoVehicle = .817, TripDurationMinutes = 10, DistanceMiles = .865)

predict(citireg, newdata = les_predict10)
