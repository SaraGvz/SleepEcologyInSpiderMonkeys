library(brms)
library(cmdstanr)
library(rstan)
library(sf)
library(ggplot2)

options(mc.cores = parallel::detectCores()) #paralelizes the chains

# Create a data frame with naps on the following day #

# split the data by tags to assign naps to each individual
splitdata = split(sleep_per_nona,as.factor(sleep_per_nona$tag))

# prev_day_sleep_lim = minutes spent sleeping during the day (from 7.30 a.m to 5.30 p.m)
# shift prev_day_sleep_lim by one day, in order to look at the naps of the consecutive day and not the previous day
napdata = c()
counts=1
for(i in 1:length(splitdata)){
  naps=c()
  temp = splitdata[[i]]
  for(j in 1:nrow(temp)){
    tempnaps=temp$night[j]+lubridate::days(1)
    if(length(which(temp$night==tempnaps))==0){
      futurenaps=NA
    }else{
      futurenaps=temp$prev_day_sleep_lim[which(temp$night==tempnaps)]
    }
    naps[j]=list(futurenaps)
  }
  naps=unlist(naps)
  temp$futurenaps=naps
  napdata[i]=list(temp)
}

# put all individuals into one data frame
napdata = do.call(rbind,napdata)


### we're trying to see if the future naps are impacted by sleep efficiency of the previous night ### 

# correlation between sleep efficiency and future naps # 
napdata$futurenaps = napdata$futurenaps+0.00001 # shift all data by 0.00001 to eliminate the 0s
mean(napdata$futurenaps, na.rm = TRUE) #histogram of future naps

nap_model <- brm(futurenaps ~ sleep_eff + (sleep_eff | tag),
                 data = napdata[complete.cases(napdata[,c("futurenaps","sleep_eff")]),], 
                 save_pars = save_pars(all = TRUE),
                 prior = c(prior(gamma(2, .1), class = shape),
                           prior(student_t(3, 88, 1.5), class = Intercept),
                           prior(student_t(3, 0, 2), class = sd),
                           prior(student_t(3, 0, 1.5), class = b)),
                 family = Gamma(link = "log"),
                 backend = "cmdstanr",
                 control = list(max_treedepth = 10, adapt_delta = .99999))


pp_check(nap_model)
summary(nap_model)
loo_R2(nap_model, probs = c(0.05, 0.95),) # 90% uncertainty interval

# check the interval 
hL = hypothesis(nap_model, c("b_sleep_eff>0","b_sleep_eff<0"),class="")
print(hL,digits=3)


# plot the model
conditional_effects(nap_model, spaghetti = TRUE)
plot(conditional_effects(nap_model, spaghetti = TRUE),points = TRUE)



nap_TST_model <- brm(futurenaps ~ TST + (TST | tag),
                     data = napdata[complete.cases(napdata[,c("futurenaps","TST")]),], 
                     save_pars = save_pars(all = TRUE),
                     iter = 10000, 
                     init = 0,
                     prior = c(prior(gamma(2, .1), class = shape),
                               prior(student_t(3, 4, 1.5), class = Intercept),
                               prior(student_t(3, 0, 1.5), class = sd),
                               prior(student_t(3, 0, 1.5), class = b)),
                     family = Gamma(link = "log"),
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .9999999))
summary(nap_TST_model)
pp_check(nap_model)
print(summary(nap_TST_model), digits = 10)
loo_R2(nap_TST_model, probs = c(0.05, 0.95),) # 90% uncertainty interval

# plot the model
conditional_effects(nap_TST_model, spaghetti = TRUE)
plot(conditional_effects(nap_TST_model, spaghetti = TRUE),points = TRUE)

# check the interval 
hM = hypothesis(nap_TST_model,c("b_TST>0","b_TST<0"),class="")
print(hM,digits=3)

loo_R2(nap_TST_model, probs = c(0.05, 0.95),)







### Configuring plots ###

# nap model# 
# sleep efficiency and future naps #
nap_Figure_9 = plot(conditional_effects(nap_model, spaghetti = TRUE),points = F)[[1]] 
nap_Figure_9 = nap_Figure_9  + geom_point(data = napdata, aes( x = sleep_eff, y = futurenaps)) + 
  labs(x = "Sleep efficiency (TST/SPT)", y = "Time spent napping the next day (min)") + 
  theme_classic()

# TST and future naps # 
nap_TST_Figure_10 = plot(conditional_effects(nap_TST_model, spaghetti = TRUE),points = F)[[1]]
nap_TST_Figure_10 = nap_TST_Figure_10 + geom_point(data = napdata, aes( x = TST, y = futurenaps)) + 
  labs(x = "Total sleep time (min)", y = "Time spent napping the next day (min)") + 
  theme_classic()


# arrange both plots to the same page #
ggpubr::ggarrange(nap_Figure_9,nap_TST_Figure_10)