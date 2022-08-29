#read rain and temperature data
rain_data<- read.csv("C:/Users/Darko/OneDrive/Desktop/R/rain_data/bci_cl_ra_elect.csv")
temperature_data <- read.csv("C:/Users/Darko/OneDrive/Desktop/R/Temperature_Data/bci_cl_at_elect.csv")

# Convert datetime to posixct
rain_data$datetime <- as.POSIXct( rain_data$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz='America/Panama' )

##check the range and  get rid of the negative values
range(rain_data$raw)
rain_data<-rain_data[which(rain_data$raw >= 0),]

#trim the data range for our vedba data range 
rain_data = rain_data[which((as.character(lubridate::date(rain_data$datetime))>= range(lubridate::date(acc_dat$local_timestamp))[1]) 
                            & (as.character(lubridate::date(rain_data$datetime)) <= range(lubridate::date(acc_dat$local_timestamp))[2])),]


#create columns for date and year
rain_data$date <- lubridate::date(rain_data$datetime)
rain_data$year <- as.character(lubridate::year(rain_data$datetime))

#in the column year2; 1 = 2015 and 2016, 2 = 2017 and 2018 
rain_data$year2 = NA

#combine the years
for(i in 1:nrow(rain_data)){
  if(rain_data$year[i] == "2015" | rain_data$year[i] == "2016" ){
    rain_data$year2[i] = 1
    }
  else{
    rain_data$year2[i] = 2
    }

}
rain_data$year2 <- as.factor(rain_data$year2)

#visualizing the data
library(ggplot2)
ggplot(rain_data, aes(x = as.factor(as.character(date)), y = raw)) + geom_boxplot()+facet_wrap(~year2)

# see how many data points we have in a day
library(dplyr)

rain_data2 = rain_data %>%
  group_by(date) %>%
  summarise(count = length(raw))
rain_data2= data.frame(rain_data2)

ggplot(rain_data2, aes(x = date, y = count )) + geom_point()
rain_data$hour = as.factor (as.character(lubridate::hour(rain_data$datetime)))

#creates means for the rainfall per hor
rain_data2 = rain_data %>% 
  group_by(hour) %>% 
  summarise(mean = mean(raw))

#plot rain against hours of the day
rain_data2$hour <- as.numeric(as.character(rain_data2$hour))
ggplot(rain_data2, aes (x = hour, y = mean)) + geom_line() + theme_classic()

#if we want to split by year or month, we use facet

rain_data$month = lubridate::month(rain_data$datetime)
rain_data$month <- as.factor(rain_data$month)


rain_data3 = rain_data %>% 
  group_by(month, hour) %>% 
  summarise(mean = mean(raw))
rain_data3 <- data.frame(rain_data3)
rain_data3$hour <- as.numeric(as.character(rain_data3$hour))
ggplot(rain_data3, aes (x = hour, y = mean)) + geom_line() + facet_wrap(~month) +theme_classic()

hist(rain_data$raw, 
     xlim = c(0,4), 
     breaks = 50)
#we can use two variables in a model, only if they are not highly correlated

rain_data$hour <- as.factor (rain_data$hour)
rain_data$date <- as.factor(as.character(rain_data$date))


##Temperature data analysis, same as with the rain data
temperature_data = Temperature_Data
temperature_data$datetime <- as.POSIXct( temperature_data$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz='America/Panama' )

range(lubridate::date(acc_dat$local_timestamp))

temperature_data = temperature_data[which((as.character(lubridate::date(temperature_data$datetime))>= range(lubridate::date(acc_dat$local_timestamp))[1]) 
                            & (as.character(lubridate::date(temperature_data$datetime)) <= range(lubridate::date(acc_dat$local_timestamp))[2])),]

temperature_data$date = lubridate::date(temperature_data$datetime)
temperature_data$month = lubridate::month(temperature_data$datetime)
temperature_data$hour = lubridate::hour(temperature_data$datetime)
temperature_data$rain = NA

#see the temp range
range(temperature_data$raw)

#we need to get rid of the negative temperatures and "negative" rain
temperature_data <-temperature_data[which(temperature_data$raw > 0),]
range(temperature_data$raw)




#create a column with the sum of rain every 15 min in temperature_data

for( i in 2:nrow(temperature_data)){
  if(length(which(rain_data$datetime <= temperature_data$datetime[i] & rain_data$datetime >=temperature_data$datetime[i-1])) == 0){
    next
  }
  rain=rain_data$raw[which(rain_data$datetime <= temperature_data$datetime[i] & rain_data$datetime>=temperature_data$datetime[i-1])]
  rain=sum(rain)
  temperature_data$rain[i]=rain
}
save(rain_data, file = "rain_data.RData")
save(temperature_data, file = "temperature_data.RData")

# plot the data #
ggplot(temperature_data, aes(x = rain, y = raw)) + geom_point() + theme_classic()



