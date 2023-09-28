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

#mortality (WPP No income group available), remove years 1951 through 1971
m <- readRDS('DemographicParameters')[[1]][[6]][[1]][, -c(1:5)]
  
#mortality between 0 and 5
m0 <- readRDS('DemographicParameters')[[1]][[6]][[2]][-c(1:5)]
  
#fertility (Lower-middle-income countries)
f <- readRDS('DemographicParameters')[[2]][[5]][, -c(1:5)]

#load speakers data
backcast_df <- readRDS("backcast") %>% 
  mutate(name_census = paste(name, census)) %>%
  filter(!name_census %in% c(readRDS('excluded') %>% pull(name_census))) %>%
  select(-name_census)
  
#vector of language names
names <- backcast_df %>% pull(name) %>% unique()
  
################################################################################
#2. ESTIMATE INTERGENERATIONAL TRNAMISSION, 1981-2001###########################
################################################################################
#matrix of average counts by age, across all censuses, in year 1976, for each language
mean76_m <- sapply(names, function(l) 
  backcast_df %>% 
    filter(year==1976, name == l, age<=45) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n))
  
#matrix, number of children 0-4, years 1981, 1986, 1991, 1996, 2001
age0_4 <- sapply(names, function(l) 
  backcast_df %>% 
    filter(name == l, age==0, year>=1981, year<=2001) %>% 
    group_by(year) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n))
  
#Function to forecast populations between 1976 and 2001 supposing full intergenerational transmission
#From this obtain predicted probabilities of intergenerational transmission during the period 2001-2101
  
#specify number of runs
runs <- 10
  
#create folder to store results
dir.create("IntergenerationalTransmission")
  
#define function
it_fct <- function(l){
    
  #create lists to store estimates and predictions below
  beta_pred <- list()
  actual_list <- list()
  counter_list <- list()
    
  while (length(beta_pred)<runs){
    
      #establish population in 1976
      p <- mean76_m[ , which(names==l)] %>% rpois(1:10, .) %>% list()
        
      #loop until 2001
      for (i in 1:5){
          
        #remove the dead
        p[[i+1]] <- p[[i]] - rbinom(length(p[[i]]), p[[i]], m[, i])
        
        #find the total number of newborns 
        newborns <- sum(rbinom(length(p[[i+1]]), p[[i+1]], f[, i]))
          
        #remove newborns who die between 0 and 2.5 years old
        newborns <- round(newborns - newborns * m0[i])
          
        #add the newborns to the population next year
        p[[i+1]] <- c(newborns, p[[i+1]])
          
      }
        
      #extract total children
      counter <- sapply(2:6, function(x) p[[x]][1])
        
      #estimate actual number of children speakers
      actual <- age0_4[ , which(names==l)] %>% rpois(5, .)
      
      #put actual and counter numbers into lists
      actual_list[[length(beta_pred)+1]] <- c(actual, rep(NA, 20))
      counter_list[[length(beta_pred)+1]] <- c(counter, rep(NA, 20))
                                                       
      #find it values
      it_trend <- actual / counter 
        
      #transform if values are too close to 0 or 1
      it_trend <- ifelse(it_trend>.9999, .9999, ifelse(it_trend<0.0001, 0.0001, it_trend))
        
      #estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
      #if unable to converge, loops continue at the same index number
      beta_pred[[length(beta_pred)+1]] <- tryCatch(
        
        predict(
          betareg(it ~ year, data = data.frame(it = it_trend, year = seq(1981, 2001, 5))),
          newdata = data.frame(year= seq(1981, 2101, 5))), 
        
        error = function(e){})
        
      }
      
    #put in df, save
    data.frame(run = rep(1:runs, each=25),
               name = l,
               year = rep(seq(1981, 2101, 5), runs),
               actual = unlist(actual_list),
               counter = unlist(counter_list),
               it = unlist(beta_pred)) %>%
      saveRDS(paste("IntergenerationalTransmission/", l, sep = ''))
      
    }

#run function
lapply(names, function(x) it_fct(x))

################################################################################