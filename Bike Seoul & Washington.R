library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)
install.packages("sjPlot")
library(sjPlot)

# Read in the file for Seoul 
BikeSeoul <- read_csv("BikeSeoul.csv")
View(BikeSeoul)


# Remove columns 
BikeSeoul <- BikeSeoul[-c(7:11)]

#Filter out missing values
BikeSeoul <- BikeSeoul %>% filter(!is.na(BikeSeoul$`Rented Bike Count`))

# Remove Functioning day column 
BikeSeoul <- BikeSeoul[-c(9)]

# Change column headers 

colnames(BikeSeoul) [1] <- "Date (Day / Month / Year)"
colnames(BikeSeoul) [2] <- "Count (Number of bikes rented in that hour)"
colnames(BikeSeoul) [3] <- "Hour (Hour of the day)"
colnames(BikeSeoul) [4] <- "Temperature (Air temperature in degree Celsius)"
colnames(BikeSeoul) [5] <- "Humidity (As a %)"
colnames(BikeSeoul) [6] <- "WindSpeed (In m/s)"
colnames(BikeSeoul) [7] <- "Season (Winter, Spring, Summer, Autumn)"
colnames(BikeSeoul) [8] <- "Holiday (Holiday / No holiday)"

# Change dates from characters to objects 
BikeSeoul$'Date (Day / Month / Year)' <- as.Date(as.character(BikeSeoul$'Date (Day / Month / Year)' ), "%d/%m/%Y")


# New variable FullDate
date_time <- BikeSeoul$'Date (Day / Month / Year)' + hours(BikeSeoul$`Hour (Hour of the day)`) + minutes(0) + seconds(0)
BikeSeoul$FullDate <- as.POSIXct(date_time, format = "%m/%d/%Y %H:%M:%S")

#Change factor levels in Holidays to yes/no
BikeSeoul$`Holiday (Holiday / No holiday)` <- fct_recode(BikeSeoul$`Holiday (Holiday / No holiday)`, "Yes" = "Holiday", "No" = "No Holiday")

# Change factor levels order in Season 
BikeSeoul$"Season (Winter, Spring, Summer, Autumn)" <- factor(BikeSeoul$`Season (Winter, Spring, Summer, Autumn)`, levels = c("Spring", "Summer", "Autumn", "Winter"))
levels(BikeSeoul$"Season (Winter, Spring, Summer, Autumn)")

#Load in BikeWashingtonDC dataset 
library(readr)
BikeWashingtonDC <- read_csv("BikeWashingtonDC.csv")
View(BikeWashingtonDC)


# Remove columns 
BikeWashingtonDC <- BikeWashingtonDC[-c(1,4,5,8,9,10,12,15,16)]

# Change colomun headers 

colnames(BikeWashingtonDC) [1] <- "Date (Day / Month / Year)"
colnames(BikeWashingtonDC) [2] <- "Season (Winter, Spring, Summer, Autumn)" 
colnames(BikeWashingtonDC) [3] <- "Hour (Hour of the day)"
colnames(BikeWashingtonDC) [4] <- "Holiday (Holiday / No holiday)" 
colnames(BikeWashingtonDC) [5] <- "Temperature (Air temperature in degree Celsius)" 
colnames(BikeWashingtonDC) [6] <- "Humidity (As a %)"
colnames(BikeWashingtonDC) [7] <- "WindSpeed (In m/s)"
colnames(BikeWashingtonDC) [8] <- "Count (Number of bikes rented in that hour)"

#Converting humidity to percentages 
BikeWashingtonDC$`Temperature (Air temperature in degree Celsius)` = BikeWashingtonDC$`Temperature (Air temperature in degree Celsius)`*(39-(-8))+(-8)

#Coverting km/h to m/s and rounded it up to 1 decimal place, similar to the Bike Seoul dataset
BikeWashingtonDC$"WindSpeed (In m/s)" <- (BikeWashingtonDC$"WindSpeed (In m/s)"*(67))*(0.277778)
BikeWashingtonDC$"WindSpeed (In m/s)" <- round(BikeWashingtonDC$"WindSpeed (In m/s)", digits = 1)

# Change factor levels order in Season 

BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)" <- as.character(BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)") 
BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)" <-  recode(BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)", "1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Autumn") 
BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)" <- factor(BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)", levels = c("Spring", "Summer", "Autumn", "Winter")) 

levels(BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)")
factor(BikeWashingtonDC$"Season (Winter, Spring, Summer, Autumn)")

#Change factor levels in Holidays to yes/no
BikeWashingtonDC$"Holiday (Holiday / No holiday)"<- as.character(BikeWashingtonDC$"Holiday (Holiday / No holiday)")
BikeWashingtonDC$"Holiday (Holiday / No holiday)"<- fct_recode(BikeWashingtonDC$"Holiday (Holiday / No holiday)", "Yes" = "1", "No" = "0")
BikeWashingtonDC$"Holiday (Holiday / No holiday)"<- factor(BikeWashingtonDC$"Holiday (Holiday / No holiday)", levels = c("Yes", "No")) 

levels(BikeWashingtonDC$"Holiday (Holiday / No holiday)")
factor(BikeWashingtonDC$"Holiday (Holiday / No holiday)")


# new variable FullDate
date_time_DC <- BikeWashingtonDC$"Date (Day / Month / Year)" + hours(BikeWashingtonDC$"Hour (Hour of the day)") + minutes(0) + seconds(0)
BikeWashingtonDC$FullDate <- as.POSIXct(date_time_DC, format = "%m/%d/%Y %H:%M:%S")

# Plotting

#Q1
# Creating a line plot for Seoul and Washington, DC with tempeature and date as values
ggplot() +
  geom_line(data = BikeSeoul, aes(x = FullDate, y = `Temperature (Air temperature in degree Celsius)`), linetype = "solid", color= "#FF6666", size=0.3)+
  labs(x = "Date (December 2017 to November 2018)", y = "Temperature (Celcius)", title = "Air Temperature in Seoul" )

ggplot() +
  geom_line(data = BikeWashingtonDC, aes(x = FullDate, y = `Temperature (Air temperature in degree Celsius)`), linetype = "solid", color= "#669900", size=0.3)+
  labs(x = "Date (January 2011 to December 2012)", y = "Temperature (Celcius)", title = "Air Temperature in Washington, DC" )

#Q2
# Creating a bar graph for season and average number of bike rented for Seoul and Washington, DC

## Seoul
# Filtering each season values into seperate variables and calculating the mean of bikes rented per season
Seoulwinter <- filter(BikeSeoul, `Season (Winter, Spring, Summer, Autumn)` == "Winter")
mw <- mean(Seoulwinter$`Count (Number of bikes rented in that hour)`)

Seoulspring <- filter(BikeSeoul, `Season (Winter, Spring, Summer, Autumn)` == "Spring")
ms <- mean(Seoulspring$`Count (Number of bikes rented in that hour)`)

Seoulsummer <- filter(BikeSeoul, `Season (Winter, Spring, Summer, Autumn)` == "Summer")
msu <- mean(Seoulsummer$`Count (Number of bikes rented in that hour)`)

Seoulautumn <- filter(BikeSeoul, `Season (Winter, Spring, Summer, Autumn)` == "Autumn")
ma <- mean(Seoulautumn$`Count (Number of bikes rented in that hour)`)

# Creating a new data.fram will all the values above, called dataq2
season <- c("Winter", "Spring", "Summer", "Autumn")
mean_bike <- c(mw, ms, msu, ma) 
dataq2 <- data.frame(season, mean_bike)

# Plotting the values of the data frame into a bar graph using ggplot
ggplot(data = dataq2, aes(x = season, y = mean_bike)) +
  geom_bar(stat='identity', color="tomato4", fill =  "lightgoldenrod1" )+
  labs(x = "Season (Winter, Spring, Summer, Autumn)", y = "Average number of Bike Rented", title = "Average number of Bikes Rented per Season in Seoul" )

## Washington, DC
# The same method is done on Washington, DC
DCwinter <- filter(BikeWashingtonDC, `Season (Winter, Spring, Summer, Autumn)` == "Winter")
dw <- mean(DCwinter$`Count (Number of bikes rented in that hour)`)

DCspring <- filter(BikeWashingtonDC, `Season (Winter, Spring, Summer, Autumn)` == "Spring")
ds <- mean(DCspring$`Count (Number of bikes rented in that hour)`)

DCsummer <- filter(BikeWashingtonDC, `Season (Winter, Spring, Summer, Autumn)` == "Summer")
dsu <- mean(DCsummer$`Count (Number of bikes rented in that hour)`)

DCautumn <- filter(BikeWashingtonDC, `Season (Winter, Spring, Summer, Autumn)` == "Autumn")
da <- mean(DCautumn$`Count (Number of bikes rented in that hour)`)

seasondc <- c("Winter", "Spring", "Summer", "Autumn")
mean_bikedc <- c(dw, ds, dsu, da) 
dataq2_dc <- data.frame(seasondc, mean_bikedc)

ggplot(data = dataq2_dc, aes(x = seasondc, y = mean_bikedc)) +
  geom_bar(stat='identity', color="palevioletred4", fill = "plum2" )+
  labs(x = "Season (Winter, Spring, Summer, Autumn)", y = "Average number of Bike Rented", title = "Average number of Bikes Rented per Season in Washington, DC" )




#Q3
# Creating a boxplot of holiday and number of bikes rented in Seoul and Washington, DC
ggplot(data = BikeSeoul, mapping = aes(x = `Holiday (Holiday / No holiday)`, y = `Count (Number of bikes rented in that hour)`))+
  geom_boxplot(alpha=0.3, colour = c("tomato4", "turquoise4"), fill = c("tomato1", "turquoise1"))+
  labs(x = "Holiday (Holiday / No Holiday)", y = "Number of Bike Rented", title = "Number of Bikes Rented in Seoul")

ggplot(data = BikeWashingtonDC, mapping = aes(x = `Holiday (Holiday / No holiday)`, y = `Count (Number of bikes rented in that hour)`))+ 
  geom_boxplot(alpha=0.3, colour = c("#0073C2FF", "#FC4E07"), fill = c("#00AFBB", "#E7B800")) +
  labs(x = "Holiday (Holiday / No Holiday)", y = "Number of Bike Rented", title = "Number of Bikes Rented in Washington, DC")


#Q4
# Creating a line graph of average number of bikes rented over time 

#Create a new data.frame with date and counts as the variables
new_Seoul <- data.frame(bydate = BikeSeoul$FullDate,
                        count = BikeSeoul$`Count (Number of bikes rented in that hour)`)

# Uses pipes to extract hour from the date column in the new data frame created
s1 <- new_Seoul  %>%
  mutate(hr = hour(ymd_hms(bydate))) %>%
  group_by(hr) %>%                                              # Group counts by hours
  summarise(count = mean(count, na.rm = TRUE)) %>%              # Then average the number of bikes rented based on each hour
  ggplot(mapping = aes(x = hr, y = count))+                     # Map the values onto ggplot
  geom_line(linetype = "solid", color = "#E7B800", size=1.2)+   # Creating a line graph
  geom_point(color="#D55E00", size=2.0, shape = 8)+             # Creating the plots of average number of bikes rented per hour
  labs(x = "Hour of the day", y = "Number of Bike Rented", title = "Number of Bikes Rented in Seoul" )

# Print the graph
s1 

# The same is done to Washington, DC
new_DC <- data.frame(bydate = BikeWashingtonDC$FullDate,
                     count = BikeWashingtonDC$`Count (Number of bikes rented in that hour)`)

dc1 <- new_DC  %>%
  mutate(hr = hour(ymd_hms(bydate))) %>%
  group_by(hr) %>%
  summarise(count = mean(count, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = hr, y = count))+
  geom_line(linetype = "solid", color = "#00AFBB", size=1.2)+
  geom_point(color="#FC4E07", size=2.0, shape = 8)+
  labs(x = "Hour of the day", y = "Number of Bike Rented", title = "Number of Bikes Rented in Washington, DC" )

dc1

#Q5
# Multiple plots are made for the three meteorological variables listed
# Plots for Seoul

ggplot() +
  geom_line(data = BikeSeoul, aes(x = `Temperature (Air temperature in degree Celsius)`, y = `Count (Number of bikes rented in that hour)`), linetype = "dotted", color= "#00AFBB", size=1.0)+
  labs(x = "Temperature (Celcius)", y = "Number of Bikes Rented", title = "Air Temperature in Seoul" )

ggplot() + 
  geom_line(data = BikeSeoul, aes(x = `Humidity (As a %)`, y = `Count (Number of bikes rented in that hour)`), linetype = "dotted", color= "#E7B800", size=1.0)+
  labs(x = "Humidity (As a %)", y = "Number of Bikes Rented", title = "Humidity in Seoul" )

ggplot() + 
  geom_line(data = BikeSeoul, aes(x = `WindSpeed (In m/s)`, y = `Count (Number of bikes rented in that hour)`), linetype = "dotted", color= "#FC4E07", size=1.0)+
  labs(x = "WindSpeed (In m/s)", y = "Number of Bikes Rented", title = "Wind Speed in Seoul" )

# Plots for Washington, DC

ggplot() +
  geom_line(data = BikeWashingtonDC, aes(x = `Temperature (Air temperature in degree Celsius)`, y = `Count (Number of bikes rented in that hour)`), linetype = "dotted", color= "#B2182B", size=1.0)+
  labs(x = "Temperature (Celcius)", y = "Number of Bikes Rented", title = "Air Temperature in Washington, DC" )

ggplot() + 
  geom_line(data = BikeWashingtonDC, aes(x = `Humidity (As a %)`, y = `Count (Number of bikes rented in that hour)`), linetype = "dotted", color= "#D6604D", size=1.0)+
  labs(x = "Humidity (As a %)", y = "Number of Bikes Rented", title = "Humidity in Washington, DC" )

ggplot() + 
  geom_line(data = BikeWashingtonDC, aes(x = `WindSpeed (In m/s)`, y = `Count (Number of bikes rented in that hour)`), linetype = "dotted", color= "#4393C3", size=1.0)+
  labs(x = "WindSpeed (In m/s)", y = "Number of Bikes Rented", title = "Wind Speed in Washington, DC" )

# Statistical Modelling 

# Multiple Linear Regression 
## Seoul
# Using pipes to create a linear model
# Selects the needed variables from the BikeSeoul dataset
# Renames the column for easier coding
# Mutates "count" to become log by multiplying "count" values with 100.
lm_count <- BikeSeoul %>% 
  select("Count (Number of bikes rented in that hour)", "Season (Winter, Spring, Summer, Autumn)", "Temperature (Air temperature in degree Celsius)", "Humidity (As a %)", "WindSpeed (In m/s)") %>%
  rename(count = "Count (Number of bikes rented in that hour)",
         Temperature = "Temperature (Air temperature in degree Celsius)",
         Humidity = "Humidity (As a %)",
         Windspeed = "WindSpeed (In m/s)",
         Season = "Season (Winter, Spring, Summer, Autumn)") %>%
  mutate(Log.count100 = count*100)

# Print the new dataset
lm_count

# Create the linear model 
lm_Seoul <- lm(Log.count100 ~ Temperature + Humidity + Windspeed + Season, data = lm_count)

# Summarises the linear model 
summary(lm_Seoul)

# Prints the 97% confidence interval for the linear model
tab_model(lm_Seoul, show.ci = 0.97)

## Washington, DC
# The same is done to Washington, DC
lm_count_dc <- BikeWashingtonDC %>% 
  select("Count (Number of bikes rented in that hour)", "Season (Winter, Spring, Summer, Autumn)", "Temperature (Air temperature in degree Celsius)", "Humidity (As a %)", "WindSpeed (In m/s)") %>%
  rename(count = "Count (Number of bikes rented in that hour)",
         Temperature = "Temperature (Air temperature in degree Celsius)",
         Humidity = "Humidity (As a %)",
         Windspeed = "WindSpeed (In m/s)",
         Season = "Season (Winter, Spring, Summer, Autumn)") %>%
  mutate(Log.count100 = count*100)

lm_count_dc

lm_DC <- lm(Log.count100 ~ Temperature + Humidity + Windspeed + Season, data = lm_count_dc)

summary(lm_DC)

tab_model(lm_DC, show.ci = 0.97)


# Prediction 
#Seoul
# Adds in the values of each variables to a data frame
predict_Seoul <- data.frame(Temperature = c(0), 
                            Humidity = c(20), 
                            Windspeed = c(0.5), 
                            Season = "Winter")

# Uses the data frame and linear model for Seoul to calculate the prediction interval
predict(lm_Seoul, newdata = predict_Seoul, interval = "predict", level = 0.90)

#Washington, DC
# The Same is done for Washington, DC
predict_DC <- data.frame(Temperature = c(0), 
                         Humidity = c(20), 
                         Windspeed = c(0.5), 
                         Season = "Winter")

predict(lm_DC, newdata = predict_DC, interval = "predict", level = 0.90)

