################################################################################
#IN THIS DO FILE: ESTIMATION OF POPULATION COUNTS BY AGE IN THE YEAR 1976
  #COUNTS ARE BASED ON THE FIVE CENSUS ROUNDS 2001-2021 
  #1. GET PACKAGES AND LOAD DATA


################################################################################
#1. GET PACKAGES AND LOAD DATA##################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)

#set theme
theme_set(theme_bw())

#mortality matrix, WPP "No income group available", period 1981-2021, reversed
m_rev <- readRDS('DemographicParameters')[[1]][[6]][[1]][, c(14:6)]
  
#Statcan data (script 1DataPreparation_Linguistic)
sc <- readRDS("scdata") 
  
################################################################################
#2. ESTIMATE SPEAKER NUMBERS IN THE YEARS 1976, 1981, 1986, 1991, 1996, 2001####
################################################################################
#vector of names
names <- sc %>% pull(name) %>% unique()
  
#list of matrices, raw data by age
raw <- lapply(seq(2001, 2021, 5), function(y) 
  lapply(names, function(x) sc %>% filter(year==y, name==x) %>% pull(pop)))
  
#Backcast function
backcast_fct <- function(y, x){
    
  #identify population
  p <- list(raw[[y]][[x]])
    
    for (i in 1:(y+4)){
        
      p[[i+1]] <- c(p[[i]] + p[[i]]*m_rev[ , (5-y+i)], 0)[-1]
        
    }
    
    #speaker numbers in each year, at each age
    data.frame(name = names[x],
               year = rep(seq(seq(2001, 2021, 5)[y], 1976, -5), each = 21),
               census = seq(2001, 2021, 5)[y],
               age = seq(0, 100, 5), 
               n = unlist(p))
    
    }
  
#run function
backcast_df <- bind_rows(lapply(1:5, function(y) lapply(1:length(names), function(x) backcast_fct(y, x))))
  
#save datasets
saveRDS(backcast_df, "backcast")
  