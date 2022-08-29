####Calculating sleep sites####
library(rstan)
library(brms)
library(cmdstanr)
library(sf)

#read in GPSdata
GPSdata$individual.local.identifier= gsub(" ", "_", GPSdata$individual.local.identifier) ###Change space to underscore in tag names

##create new column names to store after loop
sleep_per_nona$rain=NA
sleep_per_nona$temperature=NA
sleep_per_nona$Sleep_centroid=NA
sleep_per_nona$Sleep_centroid_spread=NA
sleep_per_nona$overnight_GPS_error=NA
sleep_per_nona$Number_GPS_Points=NA

temp_data = temperature_data


##Loop to merge weather and GPS data to sleep data
for(i in 1:nrow(sleep_per_nona)){
  
  #Isolate weather data within the sleep bout times
  temp=temp_data[which(temp_data$datetime>=sleep_per_nona$onset[i] & temp_data$datetime<sleep_per_nona$waking[i] ),]
  sleep_per_nona$rain[i]=sum(temp$rain, na.rm = TRUE) #Add total rain to sleep data for this sleep bout
  sleep_per_nona$temp[i]=mean(temp$raw, na.rm = TRUE) #Add mean temp to sleep data for this sleep bout
  
  #If statement checks if there is GPS data during the night
  if(length(which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i]& GPSdata$individual.local.identifier==sleep_per_nona$tag[i]))==0){
    
    ##If no GPS was recorded during the night, take the last point of the day the first point of the next day 
    tempgps=GPSdata[which(lubridate::date(GPSdata$localtime)==sleep_per_nona$night[i] & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]),] #last point of day
    
    tempgps2 = GPSdata[which(GPSdata$day==(unique(tempgps$day)+1) & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]),] #first point of next day
    
    start=tempgps[which(tempgps$localtime==max(tempgps$localtime, na.rm = TRUE)),]
    end=tempgps2[which(tempgps2$localtime==min(tempgps2$localtime, na.rm = TRUE)),]
    
    coords=rbind(start, end)
    coords$Z=coords$location.long.2+1i*coords$location.lat.2
    
    sleep_per_nona$Sleep_centroid[i]=mean(coords$Z) ##mean GPS location
    sleep_per_nona$Sleep_centroid_spread[i]=sqrt(var(coords$location.long.2)+var(coords$location.lat.2)) #GPS drift accross all GPS points 
    sleep_per_nona$overnight_GPS_error[i]=Mod(diff(coords$Z)) ##Distance between start and end if no GPS overnight
    sleep_per_nona$Number_GPS_Points[i]=length(which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i]& GPSdata$individual.local.identifier==sleep_per_nona$tag[i])) #How many GPS points were there if any
    
    #inside the if, Number_GPS_Points should be 0, in else should be above 0
    #IF only one overnight GPS point, Sleep_centroid_spread will be NA
    
    
  } else{
    #overnight_GPS_error will be NA if there are overnight GPS points
    tempgps=GPSdata[which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i] & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]),]
    
    tempgps$Z=tempgps$location.long.2+1i*tempgps$location.lat.2
    
    sleep_per_nona$Sleep_centroid[i]=mean(tempgps$Z)
    sleep_per_nona$Sleep_centroid_spread[i]=sqrt(var(tempgps$location.long.2)+var(tempgps$location.lat.2))
    sleep_per_nona$Number_GPS_Points[i]=length(which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i] & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]))
    
    
  }
  
  
}

#exploratory plots

hist(sleep_per_nona$Sleep_centroid_spread, breaks = 100)
hist(sleep_per_nona$rain, breaks = 100)
hist(sleep_per_nona$temperatureerature, breaks = 100)

## make a model with these rain and temperature values


#How many points have error above 50 if there were no overnight GPS points (should probably be excluded)

length(which(sleep_per_nona$overnight_GPS_error>50 & sleep_per_nona$Number_GPS_Points==0))
nrow(sleep_per_nona) #Total number of data points

# make column for sleep sites 
sleep_per_nona$Sleep_site_y_n=NA

# define sleep sites and save it into the column 
sleep_per_nona$Sleep_site_y_n[which(sleep_per_nona$overnight_GPS_error<=50 & sleep_per_nona$Sleep_centroid_spread<=70)]="Yes"
sleep_per_nona$Sleep_site_y_n[-which(sleep_per_nona$overnight_GPS_error<=50 & sleep_per_nona$Sleep_centroid_spread<=70)]="No"

# new data frame, split by individuals
sleep_per_nona2=sleep_per_nona[with(sleep_per_nona, order(tag,night)),]
sleep_per_nona2=split(sleep_per_nona2,as.factor(sleep_per_nona2$tag))

# data frame, that will be used for the model; defining sleep sites and if they were recorded on consecutive nights
first_night_effect_data=c()
for(i in 1:length(sleep_per_nona2)){
  temp=sleep_per_nona2[[i]][which(sleep_per_nona2[[i]]$Sleep_site_y_n=="Yes"),]
  temp$timelags=c(NA,diff(temp$night))
  temp$Z=temp$Sleep_centroid_X+1i*temp$Sleep_centroid_Y
  
  temp$consecutive_nights="No"

  #print(temp$timelags)
  if(length(which(temp$timelags==1))==0){
    
    temp$New_sleep_site="No"
    first_night_effect_data[i]=list(temp)
  } else{
    temp$consecutive_nights[which(temp$timelags==1)]="Yes"
    
    First_night=temp$night[which(temp$timelags==1)]
    night_before=temp$night[(which(temp$timelags==1)-1)]
    
    First_night_location=temp$Z[which(temp$night%in%First_night==TRUE)]
    night_before_location=temp$Z[which(temp$night%in%night_before==TRUE)]
    distances=Mod(First_night_location-night_before_location)
    print(distances)
    First_night[which(distances>50)]
    temp$New_sleep_site="No"
    temp$New_sleep_site[which(temp$night%in%First_night[which(distances>50)]==TRUE)]="Yes"
    temp$New_sleep_site[which(temp$night%in%First_night[which(distances<=50)]==TRUE)]="No"
    first_night_effect_data[i]=list(temp)
  }
 
  

}

#### Fist night effect #### 

first_night_effect_data = do.call(rbind,first_night_effect_data)
first_night_effect_data$New_sleep_site=as.factor(first_night_effect_data$New_sleep_site)

FNE = brm (sleep_eff ~ New_sleep_site + (1 | tag), 
        data = first_night_effect_data[complete.cases(first_night_effect_data[,c("New_sleep_site", "sleep_eff")]),],
        save_pars = save_pars(all = TRUE),
        iter = 10000,
        init = 0,
        prior = c(
        prior(normal(0, 1.5), class = Intercept),
        prior(exponential(.8), class = sd ),
        prior(normal(0, 1.5), class = b )
        ),
        family = Beta(link = "logit"),
        backend = "cmdstanr",
        control = list(max_treedepth = 10, adapt_delta = .999))

summary(FNE)
pp_check(FNE)
loo_R2(FNE, probs = c(0.05, 0.95),)

# check the interval 
hK = hypothesis(FNE, c("b_New_sleep_site > 0 ","b_New_sleep_site < 0 "), class="")
print(hK,digits=3)

# plot the model
conditional_effects(FNE, spaghetti = TRUE)
 plot(conditional_effects(FNE, spaghetti = TRUE), points = TRUE)

# customized plot
FNE_Figure_11 = plot(conditional_effects(FNE, spaghetti = TRUE), points = F)[[1]]
FNE_Figure_11 + labs(x = "New sleep site", y = "Sleep efficiency (TST/SPT)") + 
  theme_classic()