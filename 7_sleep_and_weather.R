#First, make a sleep_data data frame
install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)

#data frame with weather data: rain and temperature, and changing the name of the column raw into temperature
weather_data <- temperature_data %>%
  select (datetime, date, raw, rain) %>%
  rename(temperature = raw)

#data frame with sleep data
sleep_data <- sleep_per_nona %>%
  select (night, tag, onset, waking, sleep_eff, TST, SPT) %>%
  rename(date = night)

#we make two columns in the data frame for sleep, in order to fill them with the compatible weather data  
sleep_data$rain = NA
sleep_data$temperature = NA

for(i in 1:nrow(sleep_data)){
  sleep_data$rain[i] = sum(na.omit(weather_data$rain [which(weather_data$datetime >= sleep_data$onset[i] 
                                                           & weather_data$datetime <= sleep_data$waking[i] )]))
  sleep_data$temperature[i] = mean(weather_data$temperature [which(weather_data$datetime >= sleep_data$onset[i] 
                                                                  & weather_data$datetime <=  sleep_data$waking[i])], na.rm = TRUE)

}


### Sleep efficiency and weather ### 

# Sleep efficiency and rain #
sleep_rain_model <- brm(bf(sleep_eff ~ rain + (rain | tag)), 
                        
                        data = sleep_data[complete.cases(sleep_data[, c("sleep_eff" , "rain")]),],
                        
                        save_pars = save_pars(all = TRUE),
                        
                        iter = 2000,
                        
                        init = 0, #because log function needs to start sampling above 0
                        
                        prior = c (prior(normal(0, 1.5), class = Intercept),
                                   
                                   prior(normal(0, 1.5), class = b),
                                   
                                   prior(exponential(2), class = sd)),
                        
                        family = Beta (link = "logit"), 
                        
                        backend = "cmdstanr",
                        
                        control = list(max_treedepth = 10, adapt_delta = .999))

print(summary(sleep_rain_model), digits = 10) #because the values were very small
pp_check(sleep_rain_model)

# adding criterion for comparison later
sleep_rain_model = add_criterion(sleep_rain_model, c("loo", "loo_R2"), moment_match = TRUE,
                                 backend = "cmdstanr",
                                 control = list(max_treedepth = 10, adapt_delta = .999))

# plot the model
conditional_effects(sleep_rain_model, spaghetti = TRUE)
plot(conditional_effects(sleep_rain_model, spaghetti = TRUE), points = F, plot = FALSE) #with all datapoints


# Sleep efficiency, rain and temperature decorrelated #
sleep_weather_model <- brm(bf(sleep_eff ~ rain + temperature + (rain + temperature | tag),decomp = "QR"), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                     data = sleep_data  [complete.cases(sleep_data[,c("rain","temperature")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     init = 0,
                     prior = c(
                       prior(normal(0, .5), class = Intercept),
                       prior(exponential(2), class = sd ),
                       prior(normal(0, .5), class = b )
                     ),
                     family = Beta(link = "logit"), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .999999999))

summary(sleep_weather_model)
pp_check(sleep_weather_model)

# adding criterion for comparison later
sleep_weather_model =  add_criterion(sleep_weather_model, c("loo", "loo_R2"), moment_match = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999999999))

loo(sleep_weather_model)

# plot sleep weather model
conditional_effects(sleep_weather_model, spaghetti = TRUE)
plot(conditional_effects(sleep_weather_model, spaghetti = TRUE),points = TRUE) #with all datapoints


# test to see how much of the interval is above or below 0
hD = hypothesis(sleep_weather_model,c("b_rain>0","b_rain<0",
                                "b_temperature>0","b_temperature<0"),class="")
print(hD, digits=3)


# Sleep efficiency and temperature #
sleep_temperature_model <- brm(bf(sleep_eff ~ temperature + (temperature | tag)), 
                     data = sleep_data[complete.cases(sleep_data[,c("temperature")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     init = 0, #because log it function needs to start sampling above 0
                     prior = c(
                       prior(normal(0, .5), class = Intercept),
                       prior(exponential(2), class = sd ),
                       prior(normal(0, .5), class = b )
                     ),
                     family = Beta (link = "logit"), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .9999999999))

summary(sleep_temperature_model)
pp_check(sleep_temperature_model)

# adding criterion for comparison later 
sleep_temperature_model= add_criterion(sleep_temperature_model, c("loo", "loo_R2"), moment_match = TRUE,
                              backend = "cmdstanr", 
                              control = list(max_treedepth = 10, adapt_delta = .99999999))

# plot the model
conditional_effects(sleep_temperature_model, spaghetti = TRUE)
plot(conditional_effects(sleep_temperature_model, spaghetti = TRUE), points = F)  #with all datapoints

# test the interval of temperature 
hC = hypothesis(sleep_temperature_model,c("b_temperature>0","b_temperature<0"),class="")
print(hC, digits=3)


# Sleep efficiency, rain and temperature #
sleep_model_all <- brm(bf(sleep_eff ~ rain + temperature + rain:temperature + (rain + temperature | tag), decomp = "QR"),
                       data = sleep_data[complete.cases(sleep_data[,c("rain","temperature")]),],
                       save_pars = save_pars(all = TRUE),
                       iter = 3000,
                       init = 0,
                       prior = c(
                         prior(normal(0, .5), class = Intercept),
                         prior(exponential(2), class = sd ),
                         prior(normal(0, .5), class = b )
                       ),
                       family = Beta(link = "logit"), #because of the distribution of the rain and temp data
                       backend = "cmdstanr",
                       control = list(max_treedepth = 10, adapt_delta = .99999999))
summary(sleep_model_all)
pp_check(sleep_model_all)  

# adding criterion for comparison later 
sleep_model_all = add_criterion(sleep_model_all, c("loo", "loo_R2"), reloo = TRUE,
                                backend = "cmdstanr", 
                                control = list(max_treedepth = 10, adapt_delta = .99999999))

# plot the model
conditional_effects(sleep_model_all, spaghetti = TRUE)
plot(conditional_effects(sleep_model_all, spaghetti = TRUE),points = TRUE) #with all datapoints

### Comparing the models ###
# rain models
loo_compare(sleep_rain_model ,sleep_weather_model)
loo_compare(sleep_model_all,sleep_rain_model, sleep_weather_model)
# temp models
loo_compare(sleep_temperature_model ,sleep_weather_model)
loo_compare(sleep_model_all,sleep_temperature_model , sleep_weather_model)
# comparative models 
loo_compare(sleep_model_all,sleep_weather_model)

loo_R2(sleep_rain_model)
loo_R2(sleep_weather_model)
loo_R2(sleep_temperature_model )
loo_R2(sleep_model_all)  


############################################################################################################################################################

### Total sleep time (TST) and weather ### 

# TST and rain # 
TST_rain_model <- brm(bf(TST ~ rain + (rain | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_data[complete.cases(sleep_data[,c("rain")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      prior = c(
                        prior(student_t(3, 426, 50), class = Intercept),
                        prior(student_t(3,0,70), class = sd ),
                        prior(normal(0, 10), class = b )
                      ),
                      family = skew_normal, #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .999))
summary(TST_rain_model)
pp_check(TST_rain_model)

# test the interval
hE = hypothesis(TST_rain_model,c("b_rain>0","b_rain<0"),class="")
print(hE,digits=3)

# plot the model 
conditional_effects(TST_rain_model, spaghetti = TRUE)
plot(conditional_effects(TST_rain_model, spaghetti = TRUE),points = TRUE)


#adding criterion for comparison later 
TST_rain_model = add_criterion(TST_rain_model, c("loo", "loo_R2"), reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999))


# TST, rain and tempertaure decorrelated #
TST_model_2 <- brm(bf(TST ~ rain + temperature + (rain + temperature | tag),decomp = "QR"), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                   data = sleep_data[complete.cases(sleep_data[,c("rain","temperature")]),],
                   save_pars = save_pars(all = TRUE),
                   iter = 2000,
                   init = 0,
                   prior = c(
                     prior(student_t(3, 426, 50), class = Intercept),
                     prior(student_t(3,0,70), class = sd ),
                     prior(normal(0, 10), class = b )),
                   family = skew_normal(), #because of the distribution of the rain and temp data
                   backend = "cmdstanr",
                   control = list(max_treedepth = 10, adapt_delta = .999999))
prior_summary(TST_model_2)
summary(TST_model_2)
pp_check(TST_model_2)

# test the interval
hG = hypothesis(TST_model_2,c("b_rain>0","b_rain<0",
                              "b_temperature>0","b_temperature<0"),class="")
print(hG,digits=3)

#plot the model 
conditional_effects(TST_model_2, spaghetti = TRUE)
plot(conditional_effects(TST_model_2, spaghetti = TRUE),points = TRUE) 


#adding criterion for comparison later 
TST_model_2 = add_criterion(TST_model_2, c("loo", "loo_R2"), reloo = TRUE,
                            backend = "cmdstanr", 
                            control = list(max_treedepth = 10, adapt_delta = .99999))


# TST and temperature #
TST_temp_model <- brm(bf(TST ~ temperature + (temperature | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_data[complete.cases(sleep_data[,c("temperature")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      init = 0,
                      prior = c(
                        prior(student_t(3, 426, 50), class = Intercept),
                        prior(student_t(3,0,20), class = sd ),
                        prior(student_t(3, 0, 15), class = b )
                      ),
                      family = skew_normal, #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .999999999999999))
summary(TST_temp_model)
pp_check(TST_temp_model)

#plot the model 
conditional_effects(TST_temp_model, spaghetti = TRUE)
plot(conditional_effects(TST_temp_model, spaghetti = TRUE),points = TRUE)


#adding criterion for comparison later 
TST_temp_model = add_criterion(TST_temp_model, c("loo", "loo_R2"),reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999999999999999))


# TST, rain and temperature #
TST_model_all <- brm(bf(TST ~ rain + temperature + rain:temperature + (rain + temperature | tag ), decomp = "QR"),
                     data = sleep_data[complete.cases(sleep_data[,c("rain","temperature")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     prior = c(
                     prior(student_t(3, 426, 50), class = Intercept),
                     prior(student_t(3,0,70), class = sd ),
                     prior(normal(0, 10), class = b )),
                     family = skew_normal(), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .999))
summary(TST_model_all)
pp_check(TST_model_all)

#plot the model 
conditional_effects(TST_model_all, spaghetti = TRUE)
plot(conditional_effects(TST_model_all, spaghetti = TRUE),points = TRUE) 

#adding criterion for comparison later 
TST_model_all = add_criterion(TST_model_all, c("loo", "loo_R2"),reloo = TRUE,
                              backend = "cmdstanr", 
                              control = list(max_treedepth = 10, adapt_delta = .999))


### Comparing the TST models ###
#rain models 
loo_compare(TST_rain_model,TST_model_2)
loo_compare(TST_model_all,TST_rain_model, TST_model_2)
#temp models 
loo_compare(TST_model_all,TST_temp_model, TST_model_2)
loo_compare(TST_temp_model, TST_model_2)
#comparative models 
loo_compare(TST_model_all,TST_model_2)

loo_R2(TST_rain_model)
loo_R2(TST_model_2)
loo_R2(TST_temp_model)
loo_R2(TST_model_all)



############################################################################################################################################################

### Sleep period time (SPT) and weather ### 

# SPT and rain #
SPT_rain_model <- brm(bf(SPT ~ rain + (rain | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_data[complete.cases(sleep_data[,c("rain")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      prior = c(
                      prior(student_t(3, 571, 70), class = Intercept),
                      prior(student_t (3,0,70), class = sd ),
                      prior(student_t(3, 0, 20), class = b )
                      ),
                      family = skew_normal(), #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .9999))
summary(SPT_rain_model)
pp_check(SPT_rain_model)

#plot the model
conditional_effects(SPT_rain_model, spaghetti = TRUE)
plot(conditional_effects(SPT_rain_model, spaghetti = TRUE),points = TRUE)


#add criterion for later comparison 
SPT_rain_model = add_criterion(SPT_rain_model, c("loo", "loo_R2"), reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .9999))


# SPT, rain and temperature decorrelated # 
SPT_model_2 <- brm(bf(SPT ~ rain + temperature + (rain + temperature | tag),decomp = "QR"), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                   data = sleep_data[complete.cases(sleep_data[,c("rain","temperature")]),],
                   save_pars = save_pars(all = TRUE),
                   iter = 2000,
                   prior = c(
                   prior(student_t(3, 571, 70), class = Intercept),
                   prior(student_t(3, 0 , 30), class = sd),
                   prior(student_t(3, 0 , 15), class = b)
                   #prior(normal(0, 20), class = b )
                   ),
                   family = skew_normal(), #because of the distribution of the rain and temp data
                   backend = "cmdstanr",
                   control = list(max_treedepth = 10, adapt_delta = .99999999999999))
summary(SPT_model_2)
pp_check(SPT_model_2)

#test the intervals
hJ = hypothesis(SPT_model_2,c("b_rain>0","b_rain<0",
                              "b_temperature>0","b_temperature<0"),class="")
print(hJ,digits=3)

#plot the model
conditional_effects(SPT_model_2, spaghetti = TRUE)
plot(conditional_effects(SPT_model_2, spaghetti = TRUE),points = TRUE) 

#add criterion for later comparison 
SPT_model_2 = add_criterion(SPT_model_2, c("loo", "loo_R2"), reloo = TRUE,
                            backend = "cmdstanr", 
                            control = list(max_treedepth = 10, adapt_delta = .99999999999999))



# SPT and temperature #
SPT_temp_model <- brm(bf(SPT ~ temperature + (temperature | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_data[complete.cases(sleep_data[,c("temperature")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 3000,
                      init = 0,
                      prior = c(
                      prior(student_t(3, 571, 50), class = Intercept),
                      prior(student_t(3, 0 , 15), class = sd),
                      prior(normal(0, 15), class = b )
                      ),
                      family = skew_normal(), #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .99999999999))
summary(SPT_temp_model)
pp_check(SPT_temp_model)

#plot the model
conditional_effects(SPT_temp_model, spaghetti = TRUE)
plot(conditional_effects(SPT_temp_model, spaghetti = TRUE),points = TRUE) 


#add criterion for later comparison 
SPT_temp_model = add_criterion(SPT_temp_model, c("loo", "loo_R2"), reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999999))


# SPT, rain and temperature #
SPT_model_all <- brm(bf(SPT ~ rain + temperature + rain:temperature + (rain + temperature | tag ), decomp = "QR"),
                     data = sleep_data[complete.cases(sleep_data[,c("rain","temperature")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     prior = c(
                     prior(student_t(3, 571, 70), class = Intercept),
                     prior(student_t(3, 0 , 15), class = sd),
                     prior(student_t(3, 0 , 20), class = b)
                     #prior(exponential(2), class = sd ),
                     #prior(normal(0, 20), class = b )
                     ),
                     family = skew_normal(), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .9999999999))
summary(SPT_model_all)
pp_check(SPT_model_all)

#plot the model
conditional_effects(SPT_model_all, spaghetti = TRUE)
plot(conditional_effects(SPT_model_all, spaghetti = TRUE),points = TRUE) 

#add criterion for later comparison 
SPT_model_all = add_criterion(SPT_model_all, c("loo", "loo_R2"), reloo = TRUE,
                              backend = "cmdstanr", 
                              control = list(max_treedepth = 10, adapt_delta = .9999999999))

### Comparing the SPT models ###
#rain models
loo_compare(SPT_model_all,SPT_rain_model, SPT_model_2)
loo_compare(SPT_rain_model,SPT_model_2)
#temp models
loo_compare(SPT_model_all,SPT_temp_model, SPT_model_2)
loo_compare(SPT_temp_model,SPT_model_2)
#comparative models 
loo_compare(SPT_model_all,SPT_model_2)

loo_R2(SPT_rain_model)
loo_R2(SPT_temp_model)
loo_R2(SPT_model_2)
loo_R2(SPT_model_all)



### Configuring plots ###

# Sleep weather model #
# sleep efficiency and rain #
sleep_weather_Figure_2_rain = plot(conditional_effects(sleep_weather_model, spaghetti = TRUE), points = F)[[1]] #with all datapoints
sleep_weather_Figure_2_rain = sleep_weather_Figure_2_rain + geom_point(data = sleep_data, aes( x = rain , y = sleep_eff)) + 
  labs(x = "Rain (mm)", y = "Sleep efficiency (TST/SPT)") + 
  theme_classic()

# sleep efficiency and temperature
sleep_weather_Figure_3_temp = plot(conditional_effects(sleep_weather_model, spaghetti = TRUE),points = F)[[2]] #with all datapoints
sleep_weather_Figure_3_temp = sleep_weather_Figure_3_temp  + geom_point(data = sleep_data, aes( x =  temperature, y = sleep_eff)) + 
  labs(x = "Temperature (°C)", y = "Sleep efficiency (TST/SPT)") + 
  theme_classic()

# arrange both plots to the same page #
ggpubr::ggarrange(sleep_weather_Figure_2_rain,sleep_weather_Figure_3_temp)


# TST model 2 #
# TST and rain #
TST_Figure_4_rain = plot(conditional_effects(TST_model_2, spaghetti = TRUE), points = F)[[1]] #with all datapoints
TST_Figure_4_rain = TST_Figure_4_rain  + geom_point(data = sleep_data, aes( x = rain , y = TST)) + 
  labs(x = "Rain (mm)", y = "Total sleep time (min)") + 
  theme_classic()

# TST and temperature #
TST_Figure_5_temp = plot(conditional_effects(TST_model_2, spaghetti = TRUE),points = F)[[2]] #with all datapoints
TST_Figure_5_temp = TST_Figure_5_temp + geom_point(data = sleep_data, aes( x =  temperature, y = TST)) + 
  labs(x = "Temperature (°C)", y = "Total sleep time (min)") + 
  theme_classic()

# arrange both plots to the same page #
ggpubr::ggarrange(TST_Figure_4_rain,TST_Figure_5_temp)


# SPT model 2 #
# SPT and rain# 
SPT_Figure_6_rain = plot(conditional_effects(SPT_model_2, spaghetti = TRUE), points = F)[[1]] #with all datapoints
SPT_Figure_6_rain = SPT_Figure_6_rain  + geom_point(data = sleep_data, aes( x = rain , y = SPT)) + 
  labs(x = "Rain (mm)", y = "Sleep period time (min)") + 
  theme_classic()

# SPT and temperature #
SPT_Figure_7_temp = plot(conditional_effects(SPT_model_2, spaghetti = TRUE),points = F)[[2]] #with all datapoints
SPT_Figure_7_temp = SPT_Figure_7_temp + geom_point(data = sleep_data, aes( x =  temperature, y = SPT)) + 
  labs(x = "Temperature (°C)", y = "Sleep period time (min)") + 
  theme_classic()

# arrange both plots to the same page #
ggpubr::ggarrange(SPT_Figure_6_rain,SPT_Figure_7_temp)

