################################################################################
#IN THIS DO FILE: ESTIMATION OF INTERGERATIONAL TRANSMISSION
#BASED ON THE ESTIMATED POPULATIONS IN 1976 (SEE 3BACKCAST) 
#FOR THE SELECTION OF THE SPECIFIC LANGUAGES AND YEAR, SEE DATA EVALUATION
#1. GET PACKAGES AND LOAD DATA
#2. ESTIMATE INTERGENERATIONAL TRNAMISSION, 1981-2001

################################################################################
#1. GET PACKAGES AND LOAD DATA##################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)
library(betareg)

#set theme
theme_set(theme_bw())

#mortality (WPP No income group available), years 1976-1996, reversed
m <- readRDS('DemographicParameters')[[1]][[6]][[1]][1:15, c(10:6)]
  
#mortality between 0 and 5
m0 <- readRDS('DemographicParameters')[[1]][[6]][[2]][c(10:6)]
  
#fertility (Lower-middle-income countries)
f <- readRDS('DemographicParameters')[[2]][[5]][1:15, c(10:6)]

#load speakers data
backcast_df <- readRDS("backcast") %>% 
  mutate(name_census = paste(name, census)) %>%
  filter(year==2001, !name_census %in% c(readRDS('excluded') %>% pull(name_census))) %>%
  select(-name_census)

#vector of language names
names <- backcast_df %>% pull(name) %>% unique()

#matrix of average counts by age, across all censuses, in year 2001, for each language
backcast_m <- sapply(names, function(l) 
  backcast_df %>% 
    filter(name == l, age<=70) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n) %>%
    round())

################################################################################
#2. ESTIMATE INTERGENERATIONAL TRNAMISSION, 1981-2001###########################
################################################################################
#Function to backcast populations from 2001 until 1976

#specify number of runs
runs <- 1000
  
#create folder to store results
dir.create("IntergenerationalTransmission")
  
#define function
it_fct <- function(l){
    
  #create lists to store estimates and predictions below
  beta_pred <- list()
  actual_list <- list()
  counter_list <- list()
  
  while(length(beta_pred)<runs){
    
      #establish population in 1981
      p <- list(rpois(1:15, backcast_m[ , which(names==l)]))
      
      #create vectors to store actual and counterfactual number of children speakers
      actual <- c()
      counter <- c()
      
      #loop until 1981
      for (i in 1:5){
          
        #Extract actual number of children speakers
        actual[i] <- p[[i]][1]
        
        #Remove the actual children from the population vector
        p[[i]] <- c(p[[i]][-1], 0)
        
        #Add to the actual children those who died between 0 and 2.5 years old
        actual[i] <- actual[i] + rbinom(1, actual[i], m0[i])
        
        #Compute the counterfactual children speakers
        counter[i] <- sum(rbinom(15, p[[i]], f[, i]))
        
        #Add the dead to the population, assign it to the next list
        p[[i+1]] <- p[[i]] + rbinom(1:15, p[[i]], m[ , i])
          
      }
        
      #put actual and counter numbers into lists
      actual_list[[length(beta_pred)+1]] <- c(rev(actual), rep(NA, 21))
      counter_list[[length(beta_pred)+1]] <- c(rev(counter), rep(NA, 21))
      
      #find it values
      it_trend <- rev(actual) / rev(counter) 
        
      #transform if values are too close to 0 or 1
      it_trend <- ifelse(it_trend>.9999, .9999, ifelse(it_trend<0.0001, 0.0001, it_trend))
        
      #estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
      #if unable to converge, loops continue at the same index number
      beta_pred[[length(beta_pred)+1]] <- tryCatch(
        
        predict(betareg(it ~ year, data = data.frame(it = it_trend, year = seq(1976, 1996, 5))), 
                newdata = data.frame(year= seq(1976, 2101, 5))), 
        
        error = function(e){})
        
      }
      
    #put in df, save
    data.frame(run = rep(1:runs, each=26),
               name = l,
               year = rep(seq(1976, 2101, 5), runs),
               actual = unlist(actual_list),
               counter = unlist(counter_list),
               it = unlist(beta_pred)) %>%
      saveRDS(paste("IntergenerationalTransmission/", l, sep = ''))
      
    }

#run function (1,000 runs take about 1.5 hours)
lapply(names, function(x) it_fct(x))

################################################################################