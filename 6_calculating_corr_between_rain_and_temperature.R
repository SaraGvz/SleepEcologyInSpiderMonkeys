### Calculating the correlation between rain and temperature
library(rstan)
library(brms)
library(cmdstanr)

#new column with rain data raised by .001
temperature_data$rain2 = temperature_data$rain +.001

#we run two different models, compare them and see which one fits better

options(mc.cores = parallel::detectCores()) ##parallelize the chains

#first model
weather_model <- brm(rain2 ~ raw,
                     data = temperature_data[complete.cases(temperature_data[,c("rain" , "raw")]),],
                     
                     save_pars = save_pars(all = TRUE),
                     
                     prior = c(prior(gamma(2, .1), class = shape),
                     
                     prior(normal(0, 10), class = Intercept),
                     
                     prior(normal(0, 10),class = b)),

                     family = Gamma (link ="log"), 
                
                     backend = "cmdstanr", 

                     control = list(max_treedepth = 10,  adapt_delta = .999))
                                
#look at the model summary
summary(weather_model)

#plot the correlation 
conditional_effects(weather_model, spaghetti = TRUE)
plot(conditional_effects(weather_model, spaghetti = TRUE), points = TRUE) #plot with all data points

#configuring the plot
weather_model_Figure_1 = plot(conditional_effects(weather_model, spaghetti = TRUE), points = F)[[1]]
weather_model_Figure_1 = weather_model_Figure_1 + labs(x = "Temperature (°C)", y = "Rain (mm)") + 
  theme_classic()

#to later compare the model 
weather_model = add_criterion(weather_model, c("loo", "loo_R2"), moment_match = TRUE, 
              
              backend = "cmdstanr",
              
              control = list(max_treedepth = 10,
                             
                             adapt_delta = .999))



# second model 
 weather_model2 <- brm(rain ~ raw, 
                      data = temperature_data[complete.cases(temperature_data[,c("rain" , "raw")]),],
                      
                      save_pars = save_pars (all = TRUE),
                      
                      prior = c(prior(gamma(2, .1), class = shape),
                      
                      prior(normal(0, 10), class = Intercept),
                      
                      prior(normal(0, 10), class = b)),
                      
                      family = hurdle_gamma(),
                      
                      backend = "cmdstanr", 
                      
                      control = list(max_treedepth = 10, adapt_delta = .999))

summary(weather_model2)

weather_model2 = add_criterion(weather_model2, c("loo", "loo_R2"), moment_match = TRUE,
                             
                             backend = "cmdstanr", 
                             
                             control = list(max_treedepth = 10, adapt_delta = .999))

conditional_effects(weather_model2, spaghetti = TRUE)

# comparing the models 
loo_compare(weather_model, weather_model2)
loo_R2(weather_model)
loo_R2 (weather_model2)

#Gamma regression fits better (weather_model)


