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
#counts in 1951
N <- list(c(100, 70, rep(50, 10), seq(40, 0, -5)))
          
#intergenerational transmission ratio, whole period
itr <- 1 / (1+exp(1)^(.25*(1:30-10)))
plot(seq(1951, 2096, 5), itr)

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

#population pyramids, each 5 year period
ggplot(N_df)+
  geom_col(aes(age, N))+
  coord_flip()+
  facet_wrap(~year)

#population size over time
ggplot(N_df %>% group_by(year) %>% summarise(sum_N = sum(N)))+
  geom_line(aes(year, sum_N))+
  expand_limits(y=0)

################################################################################
##3. THE SYNTHETIC (OBSERVED) POPULATION, 2001-2021#############################
#Consider the population above to be the 'true', underlying population
#We dispose of five censuses (2001, 2006, 2011, 2016, 2021) to estimate its size and structure by age
#However, speaker detection in the census is prone to error 
################################################################################
#Draw 5 random samples from a log-normal distribution with mean 0
#These are the year-specific deviations from the true (unobserved) population
census_error <- rlnorm(5, 0, .2)

#The observed counts of speakers in the censuses of 2001...2021 are a result of poisson process * age-specific census error
n <- lapply(1:5, function(i) rpois(21, census_error[[i]] * N[[10+i]]))
 
#Estimation##################################################################### 
#We use the 5 census snap-shots to estimate a single "real" count in the year 2001
#This requires backcasting,
#i.e. applying mortality probabilities retrospectively to the counts in the years 2006...2021 
#to obtain counts that are free of the influence of mortality 
#(e.g. populations of older speakers will be smaller in later years, over and beyond the influence of census error, 
# because some of their speakers will have died in the intervening time)

#Backcast function
n01_fct <- function(i, r){
  
  #estimate counts from poisson process
  p <- list(rpois(21, n[[i+1]]))
  
  #backcast loop
  for (t in 1:i){
    
    p[[t+1]] <- c((p[[t]] + rbinom(1:length(p[[t]]), p[[t]], m[,c((10+i):11)[t]]))[-1], 0)
    
  }
  
  data.frame(run = r, 
             census = seq(2006, 2021, 5)[i],
             year = rep(seq(seq(2006, 2021, 5)[i], 2001, -5), each=21),
             age = seq(0, 100, 5),
             n = unlist(p))
  }

#run function
n01 <- bind_rows(lapply(1:4, function(x) lapply(1:100, function(y) n01_fct(x, y))))

#find the median for the population in 2001 estimated from the censuses of 2006...2021
n01_median <- n01 %>% filter(year==2001) %>% group_by(census, age) %>% summarise(n = quantile(n, .5))

#add year 2001
n01_median <- n01_median %>% bind_rows(data.frame(census = 2001, age = seq(0, 100, 5), n = n[[1]])) %>% arrange(census, age)

#plot population numbers by age and census
ggplot(n01_median)+
  geom_line(aes(age, n, color=as.character(census), group=as.character(census)))

#Error across population totals
observed_error <- n01_median %>% group_by(census) %>% summarise(sum_n = sum(n)) %>% 
  mutate(mean_sum_n = mean(sum_n), ratio_mean_sum = mean_sum_n / sum_n) %>% pull(ratio_mean_sum)

#means by age
lambda <- n01 %>% 
  filter(year==2001) %>%
  group_by(age) %>% 
  summarise(mean = mean(n)) %>%
  pull(mean)
  
#standard deviation of the different totals
sd_obs_ratio <- sd(observed_error / mean(observed_error))

#Population in 2001
pop2001 <- lapply(1:1000, function(x) rpois(1:21, lambda*rnorm(1, 1, sd_obs_ratio)))

#result in data frame
pop2001_df <- data.frame(run=rep(1:1000, each=21), age=rep(seq(0, 100, 5), 1000), n=unlist(pop2001)) %>% 
  filter(age<=75)

#sums
pop2001_sum <- pop2001_df %>% 
  group_by(run) %>% 
  summarise(sumpop = sum(n))

#graph
ggplot(pop2001_sum)+
  geom_density(aes(sumpop))+
  geom_point(aes(y = 0, x = sum(N[[11]][1:16])))

#this population does not include people aged above 80 (although they are few)
#the point is the real population
#repeat several times to see if the real population falls within given boundaries the corresponding proportion of times

#Intergenerational transmission#################################################
#Other backcast but until 1996 to estimate the total number of births 
n96_fct <- function(i, r){
  
  #estimate counts from poisson process
  p <- list(rpois(21, n[[i]]))
  
  #backcast loop
  for (t in 1:i){
    
    p[[t+1]] <- c((p[[t]] + rbinom(1:length(p[[t]]), p[[t]], m[,c((9+i):10)[t]]))[-1], 0)
    
  }
  
  data.frame(run = r, 
             census = seq(2001, 2021, 5)[i],
             year = rep(seq(seq(2001, 2021, 5)[i], 1996, -5), each=21),
             age = seq(0, 100, 5),
             n = unlist(p))
  
}

#run function, put in df
n96 <- bind_rows(lapply(1:5, function(i) lapply(1:1000, function(r) n96_fct(i, r))))

#totals' dispersion around the mean
obs_ratio <- n96 %>% 
  filter(year==2001, age<=70) %>%
  group_by(census) %>% 
  summarise(tot = sum(n)/1000) %>%
  pull(tot)

#standard deviation of totals 
sd_obs_ratio <- sd(obs_ratio / mean(obs_ratio))

#mean values by age in 1996
lambda <- n96 %>% 
  filter(year==1996, age<=70) %>% 
  group_by(age) %>% 
  summarise(n = mean(n)) %>% 
  pull(n)

#Forecast to estimate intergenerational transmission
# (counterfactual number of births assuming that all children learn the language)
itforecast_fct <- function(r){
  
  #draw random ratio value
  ratio <- rnorm(1, 1, sd_obs_ratio)
  
  #estimate population
  p <- rpois(1:16, lambda*ratio)
  
  #remove the dead
  p <- p - rbinom(16, p, m[, 10])
    
  #find the total number of newborns 
  counter <- sum(rbinom(16, p, f[, 10]))
  
  #find the number of newborns who acquire the language
  births <- rpois(1, lambda[1]*ratio)
    
  #put result in data frame
  data.frame(run=r, births=births, counter = counter, proportion = births / counter)
  
  }

#run function, pivot wider
int_trans <- bind_rows(lapply(1:1000, function(r) itforecast_fct(r))) %>% 
  pivot_longer(c(births, counter), values_to = 'value', names_to = 'scenario')

#plot
ggplot(int_trans)+
  geom_density(aes(value, group=scenario, fill=scenario), alpha = .3)

#table with counts
int_trans %>% 
  group_by(scenario) %>% 
  summarise(total = quantile(value, .5), lower = quantile(value, .05), upper = quantile(value, .95))

#table with proportions
data.frame(estimate = c(int_trans %>% 
  filter(scenario == 'counter') %>%
  pull(proportion) %>% 
  quantile(., c(0.05, 0.5, .95))), quantile = c("0.05", "0.50", "0.95"), actual = itr[10]) 

#Intergenerational transmission between 2001 and 2021###########################
it_01_21 <- function(r){
  
  #population in year t
  p <- lapply(1:5, function(x) rpois(21, n[[x]]))
  
  #pop aged 0-4 in t
  b <- lapply(1:5, function(x) p[[x]][1])
  
  #deaths t-5, t
  p <- lapply(1:5, function(x) p[[x]] + p[[x]]*m[,9+x])
  
  #births counter t-5, t
  c <- lapply(1:5, function(x) sum(rbinom(21, round(p[[x]]), f[,9+x])))

  #put result in data frame
  data.frame(run=r, census = seq(2001, 2021, 5), births=unlist(b), counter = unlist(c)) %>% 
    mutate(proportion = births / counter,
           itr = itr[10:14])
  
  }

#run function to see trend in it
it_trend <- bind_rows(lapply(1:100, function(x) it_01_21(x)))

#show result
ggplot(it_trend)+
  geom_line(aes(census, itr), linewidth = 1.5)+
  geom_line(aes(census, proportion, group = run), alpha = .3)

#for each run, estimate log-linear model
it_trend_linear <- it_trend %>% group_by(run) %>% do(model = lm(log(proportion) ~ census, data =.))

#extract the predictions until year 2101
it_predictions <- sapply(1:100, function(x) exp(predict(it_trend_linear$model[[x]], newdata = data.frame(census=seq(2001, 2101, 5)))))

#show predictions
ggplot(data.frame(it_predictions) %>% 
         mutate(year = seq(2001, 2101, 5)) %>% pivot_longer(X1:X100, values_to = 'prediction', names_to = "run") %>% 
         mutate(observed = rep(itr[10:30], each = 100)))+
  geom_line(aes(year, prediction, group = run), alpha = .2)+
  geom_line(aes(year, observed), linewidth = 1.5)

#full forecast##################################################################
#The full forecast is based on the estimated population in 2001 
#and the estimated intergenerational transmission ratio

#specify again the parameters for the estimated population
#Error across population totals
observed_error <- n01_median %>% group_by(census) %>% summarise(sum_n = sum(n)) %>% 
  mutate(mean_sum_n = mean(sum_n), ratio_mean_sum = mean_sum_n / sum_n) %>% pull(ratio_mean_sum)

#means by age
lambda <- n01 %>% 
  filter(year==2001) %>%
  group_by(age) %>% 
  summarise(mean = mean(n)) %>%
  pull(mean)

#standard deviation of the different totals
sd_obs_ratio <- sd(observed_error / mean(observed_error))

#full forecast function
fullforecast_fct <- function(r){

  #draw random ratio value
  ratio <- rnorm(1, 1, sd_obs_ratio)
  
  #draw random it trajectory value
  it_traject <- sample(1:100, 1)
  
  #Population in 2001
  p <- list(rpois(1:21, lambda*ratio))

  #Loop through 2101
  for (i in 1:20){
    
    #remove the dead
    p[[i]] <- p[[i]] - rbinom(21, p[[i]], m[, 10+i])

    #find the total number of newborns who acquire the language
    newborns <- sum(rbinom(21, p[[i]], f[, 10+i]*it_predictions[1+i, it_traject]))
    
    #add the newborns to the population next year
    p[[i+1]] <- c(newborns, p[[i]])[-22]
    
    }

  #put result in data frame
  data.frame(run=r, year = rep(seq(2001, 2101, 5), each = 21), age = rep(seq(0, 100, 5), 21), n = unlist(p))
  
  }

#run function
fullforecast <- bind_rows(lapply(1:100, function(x) fullforecast_fct(x)))
           
#compare result with 'real' population
#year 2101
forecast_yr2101 <- fullforecast %>% 
  filter(year==2101) %>% 
  group_by(age) %>% 
  summarise(q2 = quantile(n, .5),
            lower = quantile(n, .05),
            upper = quantile(n, .95)) %>% 
  mutate(observed = N[[31]]) 

forecast_yr2101 %>% 
  pivot_longer(c(q2, lower, upper, observed), values_to = "value", names_to = "valuetype") %>% 
  group_by(valuetype) %>% 
  summarise(sum = sum(value)) %>% 
  pivot_wider(values_from = sum, names_from = valuetype) %>% 
  select(lower, q2, upper, observed)

forecast_yr2101 %>% 
  filter(q2>0) %>% 
  pull(age) %>% 
  min

forecast_yr2101 %>% 
  filter(observed>0) %>% 
  pull(age) %>% 
  min
