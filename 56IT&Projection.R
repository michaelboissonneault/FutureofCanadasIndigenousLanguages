################################################################################
#1. GET PACKAGES AND LOAD DATA##################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)
library(betareg)

#set theme
theme_set(theme_bw())

#mortality (WPP No income group available) 
m <- readRDS('DemographicParameters')[[1]][[6]][[1]][ , -c(1:10)]     #2001-2096
m_rev <- readRDS('DemographicParameters')[[1]][[6]][[1]][ , c(10:6)]  #years 1976-1996, reversed, ages 0-70
m0 <- readRDS('DemographicParameters')[[1]][[6]][[2]][-c(1:10)]       #between 0 and 5, 2001-2096
m0_rev <- readRDS('DemographicParameters')[[1]][[6]][[2]][c(10:6)]    #1976-1996 reversed

#fertility (Lower-middle-income countries)
f <- readRDS('DemographicParameters')[[2]][[5]][ , -c(1:10)]          #2001-2096
f_rev <- readRDS('DemographicParameters')[[2]][[5]][ , c(10:6)]       #1976-1996, reversed

#vector of language names
names <- readRDS("backcast") %>% pull(name) %>% unique()

#matrix of average counts by age, across all censuses, in year 2001, for each language
baseline <- sapply(names, function(l) 
  readRDS("backcast") %>% 
    mutate(name_census = paste(name, census)) %>%
    filter(year==2001, name == l, age<=80, !name_census %in% c(readRDS('excluded') %>% pull(name_census))) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n) %>%
    round())

#population totals by census (in 2001)
census_totals <- sapply(names, function(l)
  readRDS("backcast") %>% 
    mutate(name_census = paste(name, census)) %>%
    filter(year==2001, name == l, age<=80, !name_census %in% c(readRDS('excluded') %>% pull(name_census))) %>%
    group_by(census) %>% 
    summarise(sum = sum(round(n))) %>%
    pull(sum))

#mean of the totals, for each language 
total_means <- sapply(1:length(names), function(x) mean(census_totals[[x]]))

#standard error of the totals, for each language 
total_ses <- sapply(1:length(names), function(x) sd(census_totals[[x]]) / sqrt(length(census_totals[[x]])))

#standardized counts by age  
baseline_st <- sapply(1:length(names), function(x) baseline[ , x] / total_means[x]) 

#create vectors to store actual and counterfactual number of children speakers
actual <- c()
counter <- c()

################################################################################
#2. ESTIMATE INTERGENERATIONAL TRNAMISSION, 1981-2001###########################
################################################################################
#specify number of runs
runs <- 1000

#create folder to store results
dir.create("Results")

#function to run the projection model
fct <- function(l){
  
  #create lists to store estimates and predictions below
  it_list <- list()
  actual_list <- list()
  counter_list <- list()
  result_list <- list()
  
  while(length(it_list)<runs){
    
    #establish population in 2001
    p <- list(c(rpois(17, rnorm(1, total_means[l], total_ses[l])*baseline_st[ , l]), rep(0, 4)))
    
    #BACKCAST
    #loop until 1977
    for (i in 1:5){
      
      #Extract actual number of children speakers
      actual[i] <- p[[i]][1]
      
      #Remove the actual children from the population vector, add to next pop list
      p[[i+1]] <- c(p[[i]][-1], 0)
      
      #Add to the actual children those who died between 0 and 2.5 years old
      actual[i] <- actual[i] + rbinom(1, actual[i], m0_rev[i])
      
      #Compute the counterfactual number of children speakers
      counter[i] <- sum(rbinom(21, p[[i+1]], f_rev[, i]))
      
      #Add the dead to the population
      p[[i+1]] <- p[[i+1]] + rbinom(21, p[[i+1]], m_rev[ , i])
      
    }
    
    #save backcast population
    backcast <- unlist(p[c(6:2)])
    
    #find it values
    it_trend <- rev(actual) / rev(counter) 
    
    #transform if values are too close to 0 or 1
    it_trend <- ifelse(it_trend>.999, .999, ifelse(it_trend<0.001, 0.001, it_trend))
    
    #estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
    #if unable to converge, loops continue at the same index number
    it <- tryCatch(
      
      predict(betareg(it ~ year, data = data.frame(it = it_trend, year = seq(1976, 1996, 5))), 
              newdata = data.frame(year= seq(1976, 2101, 5))), 
      
      error = function(e){})
    
    #FORECAST
    #Loop through 2101
    for (i in 1:20){
      
      #remove the dead
      p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[, i])
      
      #find the total number of newborns who acquire the language
      newborns <- sum(rbinom(21, p[[i+1]], f[ , i] * it[(5+i)]))
      
      #remove the newborns that die between 0 and 2.5
      newborns <- newborns - rbinom(1, newborns, m0[i])
      
      #add the newborns to the population next year
      p[[i+1]] <- c(newborns, p[[i+1]])[-22]
      
    }
    
    #Put results in to the lists
    it_list[[length(it_list)+1]] <- it
    actual_list[[length(it_list)+1]] <- c(rev(actual), rep(NA, 21))
    counter_list[[length(it_list)+1]] <- c(rev(counter), rep(NA, 21))
    result_list[[length(it_list)+1]] <- c(backcast, unlist(p))
    
  }#end of while loop
  
  #save results to df
  data.frame(run = rep(1:runs, each=26*21),
             name = names[l],
             year = rep(rep(seq(1976, 2101, 5), each = 21), runs),
             age = rep(seq(0, 100, 5), 26*runs),
             n = unlist(result_list),
             actual = rep(unlist(actual_list), each = 21),
             counter = rep(unlist(counter_list), each = 21),
             it = rep(unlist(it_list), each = 21)) %>%
    saveRDS(paste("Results/", names[l], sep = ''))
  
  }

lapply(1:length(names), function(x) fct(x))

lapply(8, function(x) fct(x))
