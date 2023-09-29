################################################################################
#IN THIS DO-FILE
#ESTIMATION OF A POPULATION OF SPEAKERS IN A BASELINE YEAR (2001)
#BASED ON DATA COLLECTED AT SUBSEQUENT POINTS (2001, 2006, 2011, 2016, 2021)
#THEN PROJECTION OF THIS POPULATION'S SIZE AND STRUCTURE BY AGE IN THE YEAR 2101

#CONTENTS
#1. PACKAGES & DATA
#2. THE REAL (UNOBSERVED) POPULATION, 1951-2101
#3. THE SYNTHETIC (OBSERVED) POPULATION, 2001-2021
#4. BACKCAST
#5. INTERGENERATIONAL TRANSMISSION
#6. PROJECTION
################################################################################
##1. PACKAGES & DATA############################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)
library(betareg)

#set theme
theme_set(theme_bw())

#mortality (WPP No income group available; remove years 1951 through 1996)
m <- readRDS('DemographicParameters')[[1]][[6]][[1]]

#mortality between 0 and 5
m0 <- readRDS('DemographicParameters')[[1]][[6]][[2]]

#fertility (Lower-middle-income countries)
f <- readRDS('DemographicParameters')[[2]][[5]]

#the mortality and fertility matrices contains probabilities taken from the UN's World Population Prospects 2022
#which correspond to the estimates and projections for the lower-middle-income group of countries
#the fertility and mortality regimes of these countries correspond roughly to that of the Canadian indigenous population

################################################################################
#2. THE REAL (UNOBSERVED) POPULATION, 1951-2101#################################
#Suppose a population of speakers in the year 1951 who gradually stop transmitting their language to their children 
#(No children learn it by ~2050.)
################################################################################
#counts in 1951
N <- list(rpois(1:21, (21:1)^2/4))

#intergenerational transmission rates, whole period
itr <- 1 / (1+exp(1)^(.25*(1:30-10)))
plot(seq(1951, 2096, 5), itr)

#Microsimulation to let the population change over time given mortality, fertility and intergenerational transmission rates
#The microsimulation covers the period 1951-2101 using five-year jumps
for (t in 1:30){
  
  #remove the dead
  N[[t+1]] <- N[[t]] - rbinom(21, N[[t]], m[, t])
  
  #find the number of newborns
  nb <- sum(rbinom(21, N[[t+1]], f[, t]*itr[t]))
  
  #subtract the number of newborns that die between 0 and 5
  nb <- nb - rbinom(1, nb, m0[t])
  
  #Add the newborns to the population
  N[[t+1]] <- c(nb, N[[t+1]][-21])
  
}

#Data frame, counts of speakers by year and age
N_df <- data.frame(year=rep(seq(1951, 2101, 5), each=21), age=seq(0, 100, 5), N=unlist(N))

#Show the population's structure by age over time
ggplot(N_df)+
  geom_col(aes(age, N))+
  coord_flip()+
  facet_wrap(~year)

#Show the change over in the number of speakers
ggplot(N_df %>% group_by(year) %>% summarise(sum_N = sum(N)))+
  geom_line(aes(year, sum_N))+
  expand_limits(y=0)

################################################################################
##3. THE SYNTHETIC (OBSERVED) POPULATION, 2001-2021#############################
#Consider the population above to be the 'true', underlying population
#We dispose of five censuses (2001, 2006, 2011, 2016, 2021) to estimate its size and structure by age
#However, speaker detection in the census is prone to error 
#We measure the extent of this error and include it in our calculations
#But first, we need to adjust the speaker numbers by age to account for the effect of mortality on speaker counts over time
#For this, we estimate for the year 2001 five populations based on each census counts 
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

#plot comparing the true and synthetic counts
ggplot(N_n_df)+
  geom_line(aes(age, Value, group= Counts, color = Counts))+
  facet_wrap(~year)+
  coord_flip()

################################################################################
#4 BACKCAST#####################################################################
################################################################################
#We apply mortality probabilities retrospectively to the counts in the years 2001...2021 back until the year 1976
#This allows to obtain counts that are free of the influence of mortality 
#What remains is only the census error and the effect of poisson randomness
#We use the counts in 1976 to estimate the change over time in intergenerational transmission
#and the counts in 2001 to obtain an estimate of the census error

#Backcast function
backcast_fct <- function(x){
  
  #identify the population from given census
  p <- list(n[[x]])
  
  #specify the mortality schedule
  m_rev <- m[ , which(seq(1956, 2101, 5)==seq(2001, 2021, 5)[x]):6]
  
  for (i in 1:ncol(m_rev)){
    
    p[[i+1]] <- c(p[[i]] + p[[i]]*m_rev[ , i], 0)[-1]
    
  }
  
  data.frame(age = rep(seq(0, 100, 5), ncol(m_rev)+1), 
             census = seq(2001, 2021, 5)[x], 
             year = rep(seq(seq(2001, 2021, 5)[x], 1976, -5), each=21), 
             n = unlist(p))
  
}

#run function
n76 <- bind_rows(lapply(1:5, function(x) backcast_fct(x))) 

#average counts by age, across all years, for each language
n76_mean_df <- n76 %>% 
  filter(year==1976) %>%
  group_by(age) %>% 
  summarise(mean_n = mean(n))

#compare with real counts
n76_N <- n76 %>% left_join(data.frame(age = seq(0, 100, 5), year = 1991, N = N[[9]]))

ggplot(n76_N %>% filter(year==1991) %>% mutate(census = factor(census)))+
  geom_line(aes(age, n, group=census, color=census))+
  geom_smooth(aes(age, n))+
  geom_line(aes(age, N), linewidth = 2)+
  ggtitle("Black line is real pop, colored ones are the model applied to the data in each year, blue is their average")

################################################################################
#5. INTERGENERATIONAL TRANSMISSION###############################################
################################################################################
#Function to forecast populations between 1976 and 2021 supposing full intergenerational transmission
#We then divide the observed counts by those obtained supposing full inttergenerational transmission
#in order to obtain estimates of the true intergenerational transmission between 1981 and 2021

#specify number of runs
runs <- 100

#put speaker counts age 0 in 1981-2001 in vector
age0_81_01 <- n76 %>% 
  filter(age==0, year>=1981, year<=2001) %>% 
  group_by(year) %>% 
  summarise(mean_n = mean(n)) %>% 
  pull(mean_n) %>%
  rpois(5, .)

#put speaker counts in 1976 into vector
n76_mean <- n76_mean_df %>% pull(mean_n)

#define function
it_fct <- function(){
  
  #create list to store predictions below
  beta_pred <- list()
  
  for (j in (1+length(beta_pred)):runs){
    
    tryCatch({
      
      #establish population in 1976
      p <- n76_mean %>% rpois(1:10, .) %>% list()
      
      #loop until 2001
      for (i in 1:5){
        
        #remove the dead
        p[[i+1]] <- p[[i]] - rbinom(length(p[[i]]), p[[i]], m[, 5+i])
        
        #find the total number of newborns 
        newborns <- sum(rbinom(length(p[[i+1]]), p[[i+1]], f[, 5+i]))
        
        #remove newborns who die between 0 and 2.5 years old
        newborns <- round(newborns - newborns*m0[5+i])
        
        #add the newborns to the population next year
        p[[i+1]] <- c(newborns, p[[i+1]])
        
      }
      
      #extract total children
      counter <- sapply(2:6, function(x) p[[x]][1])
      
      #estimate actual number of children speakers
      actual <- age0_81_01 %>% rpois(5, .)
      
      #find it values
      it_trend <- actual / counter 
      
      #transform if values are not within ]0;1[
      it_trend <- ifelse(it_trend>=1, .9999, ifelse(it_trend<=0, 0.0001, it_trend))
      
      #estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
      beta_model <- betareg(it ~ year, data = data.frame(it = it_trend, year = seq(1981, 2001, 5)))
      
      #put the predictions of the beta model into the list
      beta_pred[[j]] <- predict(beta_model, newdata = data.frame(year= seq(2001, 2101, 5)))
      
    }, error = function(e){})
    
  }
  
  #put in matrix, save
  matrix(unlist(beta_pred), ncol=runs)
  
}

#run function, put result in matrix
it_m <- it_fct()

#put in df, add real it values
it_df_real <- data.frame(run = rep(1:runs, each = 21),
                         year = seq(2001, 2101, 5),
                         synthetic_it = c(it_m), 
                         real_it = itr[-c(1:9)])

#show results
ggplot(it_df_real)+
  geom_line(aes(year, synthetic_it, group = run), alpha = .05)+
  geom_smooth(aes(year, synthetic_it))+
  geom_line(aes(year, real_it), color = 'red')+
  ggtitle("Red line is real it, grey ones are the estimates, blue their average")

################################################################################
#6. PROJECTION##################################################################
################################################################################
#The projection is based on the estimated population in 2001 
#and the estimated intergenerational transmission rates

#vector of population means by age in 2001
n01 <- n76 %>% 
  filter(year==2001, age<=80) %>%
  group_by(age) %>%
  summarise(n = mean(n)) %>%
  pull(n)

#vector of population totals by census in 2001
totalbycensus01 <- n76 %>% 
  filter(year==2001) %>%
  group_by(census) %>% 
  summarise(sum = sum(n)) %>%
  pull(sum)

#standard error of the ratio total / mean of totals  
se <- sd(totalbycensus01 / mean(totalbycensus01))  / sqrt(5)
         
#specify number of runs
runs <- 100

#function to run the projection model
proj_fct <- function(){
  
  #list to store results
  results <- list()
  
  for (r in 1:runs){
    
    #estimate starting population
    p <- c(rpois(17, n01*rlnorm(1, 0, se)), rep(0, 4)) %>% list()
    
    #vector of intergenerational transmission
    it <- it_m[,r]
    
    #Loop through 2101
    for (i in 1:20){
      
      #remove the dead
      p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[ , 10+i])
      
      #find the total number of newborns who acquire the language
      newborns <- sum(rbinom(21, p[[i+1]], f[ , 10+i]*it[i]))
      
      #remove the newborns that die between 0 and 2.5
      newborns <- round(newborns - newborns*m0[10+i])
      
      #add the newborns to the population next year
      p[[i+1]] <- c(newborns, p[[i+1]])[-22]
      
    }
    
    results[[r]] <- unlist(p)
    
  }
  
  #put result in data frame
  bind_rows(
    lapply(1:runs, function(x) 
      data.frame(run = x,
                 year = rep(seq(2001, 2101, 5), each=21),
                 age = rep(seq(0, 100, 5), 21),
                 n = results[[x]]))) 
  
}

#run function
proj_results <- proj_fct()

#compare the predicted and actual populations
comparison <- proj_results %>% 
  group_by(run, year) %>%
  summarise(sum_n = sum(n)) %>%
  group_by(year) %>%
  summarise(lower = quantile(sum_n, .05), upper = quantile(sum_n, .95)) %>% 
  left_join(N_df %>% group_by(year) %>% summarise(sum_N = sum(N)))

#show trend in actual population and 80% CI in the predicted population
ggplot(comparison)+
  geom_line(aes(year, sum_N))+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha = .1)+
  scale_x_continuous(breaks=seq(2001, 2101, 20))+
  ggtitle("Solid line is real pop size, shaded area the model's 90% CI")

#populations by age in selected years
pop_byage <- proj_results %>% 
  filter(year %in% seq(2001, 2101, 20)) %>% 
  group_by(age, year) %>% 
  summarise(lower = quantile(n, .05), upper = quantile(n, .95)) %>%
  left_join(N_df %>% filter(year %in% seq(2001, 2101, 20)))

#compare population pyramids in selected years
ggplot(pop_byage)+
  geom_point(aes(age, N))+
  geom_segment(aes(x = age, xend = age, y = lower, yend = upper), linewidth = 3, alpha = .2)+
  facet_wrap(~year)+
  coord_flip()+
  ggtitle("Dots are real pop values, shaded area the model's 90% CI")
