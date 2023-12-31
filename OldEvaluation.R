################################################################################
#IN THIS DO-FILE
#ESTIMATION OF A POPULATION OF SPEAKERS IN A BASELINE YEAR (HERE: 2001)
#BASED ON DATA COLLECTED AT SUBSEQUENT POINTS (HERE: 2001, 2006, 2011, 2016, 2021)
#WE THEN PROJECT THIS POPULATION'S SIZE AND STRUCTURE BY AGE IN THE YEAR 2101

#CONTENTS
#1. PACKAGES & DATA
#2. THE REAL (UNOBSERVED) POPULATION, 1951-2101
#3. THE SYNTHETIC (OBSERVED) POPULATION, 2001-2021

################################################################################
##1. PACKAGES & DATA############################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)
library(betareg)

#set theme
theme_set(theme_bw())

#life fertility tables (World Population prospects 2022)
lt <- readRDS("WPP2022_Life_Table_Abridged_Medium_Both_ExcludingSingleCountries") 
ft <- readRDS("WPP2022_Fertility_by_Age5_Medium_ExcludingSingleCountries") 

#(the mortality and fertility of the WPP lower-middle-income group corresponds roughly to that of the Canadian indigenous population)

#mortality probability matrix 
m <- lt %>% 
  filter(Location=="Lower-middle-income countries", Time %in% seq(1951, 2096, 5), AgeGrpStart!=0) %>%
  mutate(qx = ifelse(AgeGrpStart==1, 1-lx/100000, qx)) %>%
  pull(qx) %>%
  matrix(nrow=21)

#fertility rates
f <- ft %>% 
  filter(Location=="Lower-middle-income countries", Time %in% seq(1951, 2096, 5), AgeGrpStart<=45) %>%
  pull(ASFR) %>%
  matrix(nrow=8)

#fill the rest of the fertility matrix with zeros
f <- rbind(matrix(nrow=2, ncol=30, 0), f, matrix(nrow=11, ncol=30, 0))/400

################################################################################
#2. THE REAL (UNOBSERVED) POPULATION, 1951-2101#################################
#Suppose a population of speakers in the year 1951 who gradually stop transmitting their language to their children 
#(No children learn it by ~2050.)
################################################################################
eval_fct <- function(r){
  
  #counts in 1951
  N <- list(rpois(1:21, (21:1)^2))
  
  #intergenerational transmission ratio, whole period
  itr <- 1 / (1+exp(1)^(runif(1, 0, .5)*(1:30-sample(5:25, 1))))
  
  #microsimulation function
  #Loop through all 5-year periods between 1951 and 2100
  for (t in 1:30){
    
    #remove the dead
    N[[t+1]] <- N[[t]] - rbinom(21, N[[t]], m[, t])
    
    #add the newborn 
    N[[t+1]] <- c(sum(rbinom(21, N[[t+1]], f[, t]*itr[t])), N[[t+1]][-21])
    
  }
  
  #data frame, counts of speakers by year and age
  N_df <- data.frame(year=rep(seq(1951, 2101, 5), each=21), age=seq(0, 100, 5), N=unlist(N))
  
  ################################################################################
  ##3. THE SYNTHETIC (OBSERVED) POPULATION, 2001-2021#############################
  #Consider the population above to be the 'true', underlying population
  #We dispose of five censuses (2001, 2006, 2011, 2016, 2021) to estimate its size and structure by age
  #However, speaker detection in the census is prone to error 
  #We measure the extent of the detection error between censuses and include it in our calculations
  #But first, we need to adjust the speaker numbers by age to account for the effect of mortality over time
  #For this, we estimate for the year 1996 five populations based on each census counts 
  ################################################################################
  #Draw 5 random samples from a log-normal distribution with mean 0
  #These are the year-specific deviations from the true (unobserved) population
  census_error <- rlnorm(5, 0, .2)
  
  #The observed counts of speakers in the censuses of 2001...2021 are a result of poisson process * age-specific census error
  n <- lapply(1:5, function(i) rpois(21, census_error[[i]] * N[[10+i]]))
  
  #data frame with observed and underlying counts
  N_n_df <- N_df %>% filter(year>=2001, year<=2021) %>% left_join(
    data.frame(n = unlist(n), age = seq(0, 100, 5), year = rep(seq(2001, 2021, 5), each = 21))
  ) %>% rename(real = N, synthetic = n) %>%
    pivot_longer(c(real, synthetic), values_to = "Value", names_to = "Counts")
  
  #Estimation##################################################################### 
  #We use the 5 census snap-shots to estimate a single "real" count in the year 2001
  #This requires backcasting,
  #i.e. applying mortality probabilities retrospectively to the counts in the years 2001...2021 
  #to obtain counts that are free of the influence of mortality 
  #(e.g. populations of older speakers will be smaller in later years, over and beyond the influence of census error, 
  # because some of their speakers will have died in the intervening time)
  
  #Backcast function
  n01_fct <- function(i){
    
    p <- list(n[[i+1]])
    
    #backcast loop
    for (t in 1:i){
      
      p[[t+1]] <- c((p[[t]] + p[[t]] * m[,c((10+i):11)[t]])[-1], 0)
      
    }
    
    data.frame(census = seq(2006, 2021, 5)[i],
               year = rep(seq(seq(2006, 2021, 5)[i], 2001, -5), each=21),
               age = seq(0, 100, 5),
               n = unlist(p))
  }
  
  #run function, add year 2001
  n01 <- bind_rows(lapply(1:4, function(x) n01_fct(x))) %>%
    filter(year==2001) %>%
    select(-year) %>%
    bind_rows(data.frame(n = n[[1]], census = 2001, age = seq(0, 100, 5))) %>%
    arrange(census, age)
  
  #Error across population totals
  observed_error <- n01 %>% group_by(census) %>% summarise(sum_n = sum(n)) %>% 
    mutate(mean_sum_n = mean(sum_n), ratio_mean_sum = mean_sum_n / sum_n) %>% pull(ratio_mean_sum)
  
  #means by age
  lambda <- n01 %>% 
    group_by(age) %>% 
    summarise(mean = mean(n)) %>%
    pull(mean)
  
  #standard deviation of the errors 
  sd_obs_error <- sd(observed_error) 
  
  #Function to forecast the population from in each census year t-5 until year t
  #Allows to estimate the probability of intergenerational transmission in the years 2001 to 2021
  #The probability is obtained by dividing the number of children ages 0-4 in the observed data
  #by the number of children that would be observed assuming that all children would learn the language
  it_01_21 <- function(r){
    
    #population in year t
    p <- lapply(1:5, function(x) rpois(21, n[[x]]))
    
    #pop aged 0-4 in t
    b <- lapply(1:5, function(x) p[[x]][1])
    
    #population in year t + deaths t-5, t
    p <- lapply(1:5, function(x) p[[x]] + p[[x]]*m[,9+x])
    
    #births counter t-5, t
    c <- lapply(1:5, function(x) sum(rbinom(21, round(p[[x]]), f[,9+x])))
    
    #put result in data frame
    data.frame(run=r, census = seq(2001, 2021, 5), births=unlist(b), counter = unlist(c)) %>% 
      mutate(proportion = births / counter,
             itr = itr[10:14])
    
  }
  
  #run function to see trend in it
  it_trend <- bind_rows(lapply(1:300, function(x) it_01_21(x)))
  
  #for the beta model, values should not equal 0 or 1
  it_trend <- it_trend %>% mutate(proportion = ifelse(proportion>=1, .999, proportion),
                                  proportion = ifelse(proportion<=0, .001, proportion))
  
  #for each run, estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
  beta_model <- it_trend %>% group_by(run) %>% do(model = betareg(proportion ~ census, data = .))
  
  #extract the predictions until year 2101
  it_predictions <- sapply(1:300, function(x) 
    predict(beta_model$model[[x]], newdata = data.frame(census=seq(2001, 2101, 5))))
  
  #full forecast##################################################################
  #The full forecast is based on the estimated population in 2001 
  #and the estimated intergenerational transmission ratio
  #full forecast function
  fullforecast_fct <- function(r){
    
    #draw random ratio value
    ratio <- rlnorm(1, 0, sd_obs_error)
    
    #draw random number for it trajectory 
    it_traject <- sample(1:300, 1)
    
    #Population in 2001
    p <- list(rpois(1:21, lambda))
    
    #Loop through 2101
    for (i in 1:20){
      
      #specify the value
      it_value <- it_predictions[1+i, it_traject]
      it_value <- ifelse(it_value>1, 1, it_value)
      
      #remove the dead
      p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[, 10+i])
      
      #find the total number of newborns who acquire the language
      newborns <- sum(rbinom(21, p[[i+1]], f[, 10+i]*it_value))
      
      #add the newborns to the population next year
      p[[i+1]] <- c(newborns, p[[i+1]])[-22]
      
    }
    
    #put result in data frame
    data.frame(run=r, year = rep(seq(2001, 2101, 5), each = 21), age = rep(seq(0, 100, 5), 21), n = unlist(p))
    
  }
  
  #run function
  fullforecast <- bind_rows(lapply(1:300, function(x) fullforecast_fct(x)))
  
  #total population by year (median and 90% bounds)
  ff_sum <- fullforecast %>% group_by(run, year) %>% summarise(sum_n = sum(n)) %>%
    group_by(year) %>% summarise(q2 = quantile(sum_n, .5), lower = quantile(sum_n, .05), upper = quantile(sum_n, .95))
  
  #add real trend 
  ff_sum <- ff_sum %>% 
    left_join(bind_rows(lapply(11:31, function(x) data.frame(year = seq(1951, 2101, 5)[x], real = sum(N[[x]])))))
  
  #save results
  saveRDS(ff_sum, paste("C:/Users/micbo262/Documents/FutureofCanadasIndigenousLanguages/Results/run", r, sep = ''))
  
  }

#run function
lapply(1011:1020, function(x) eval_fct(x))

#download results
ff_sum <- bind_rows(lapply(1000:1019, function(x) 
  readRDS(paste("C:/Users/micbo262/Documents/FutureofCanadasIndigenousLanguages/Results/run", x, sep = ''))))

#summarise accurace of the predictions
ff_sum <- ff_sum %>% mutate(within = ifelse(real >lower & real < upper, 1, 0))
accuracy_by_year <- ff_sum %>% group_by(year) %>% summarise(correct = sum(within) / 300 * 100)

table(ff_sum$within)

#regression model
glm(within ~ year + real, data = ff_sum, family = "binomial") %>% summary()

#viz
ggplot(ff_sum)+
  geom_density(aes(real, group=within, fill=within), alpha = .2)

filter(ff_sum, real<50)

#run numbers
ff_sum <- ff_sum %>%
  mutate(run = rep(1:20, each=21))

#show result
ggplot(ff_sum)+
  geom_line(aes(year, q2))+
  geom_line(aes(year, real), color = 'red')+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha=.2)+
  facet_wrap(~run, scales = 'free_y')

#further visualizations
#population pyramids, each 5 year period
ggplot(N_df)+
  geom_col(aes(age, N))+
  coord_flip()+
  facet_wrap(~year)

#population size over time
ggplot(N_df %>% group_by(year) %>% summarise(sum_N = sum(N)))+
  geom_line(aes(year, sum_N))+
  expand_limits(y=0)

#real vs. synthetic numbers by age and year
ggplot(N_n_df)+
  geom_line(aes(age, Value, group= Counts, color = Counts))+
  facet_wrap(~year)+
  coord_flip()

#population numbers by age and census
ggplot(n01)+
  geom_line(aes(age, n, color=as.character(census), group=as.character(census)))+
  coord_flip()

#observed IT vs. real IT, 2001-2021
ggplot(it_trend)+
  geom_line(aes(census, itr), linewidth = 2, color = 'red')+
  geom_line(aes(census, proportion, group = run), alpha = .05)

#IT predictions (beta model)
ggplot(data.frame(it_predictions) %>% 
         mutate(year = seq(2001, 2101, 5)) %>% pivot_longer(X1:X100, values_to = 'prediction', names_to = "run") %>% 
         mutate(observed = rep(itr[10:30], each = 100)))+
  geom_line(aes(year, prediction, group = run), alpha = .1)+
  geom_line(aes(year, observed), linewidth = 1.5, color = 'red')

#Relative range of predictions over time
ff_sum %>% mutate(diff = upper - lower,
                  reldiff = diff / q2) %>% pull(reldiff) %>% plot
