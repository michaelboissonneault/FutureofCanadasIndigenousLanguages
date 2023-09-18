################################################################################
#IN THIS DO FILE: PROJECTION 2001-2101
################################################################################
#1. GET PACKAGES AND LOAD DATA##################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)

#set theme
theme_set(theme_bw())

#read in life and fertility matrices
m1 <- readRDS('DemographicParameters')[[1]][, -c(1:5)]
m1_0 <- readRDS('DemographicParameters')[[2]][-c(1:5)]
m2 <- readRDS('DemographicParameters')[[3]][, -c(1:5)]
m2_0 <- readRDS('DemographicParameters')[[4]][-c(1:5)]
f <- readRDS('DemographicParameters')[[5]][, -c(1:5)]

#load speakers data
n76 <- readRDS("backcast") %>% 
  mutate(name_census = paste(name, census)) %>%
  filter(!name_census %in% c(readRDS('excluded') %>% mutate(name_census = paste(name, census)) %>% pull(name_census))) %>%
  select(-name_census)

#vector of names
names <- n76 %>% pull(name) %>% unique()

#load it trajectory data and assign to matrix
it_m <- lapply(names, function(x) 
  matrix(readRDS(paste('IntergenerationalTransmission/', x, sep = '')) %>% filter(year>=2001) %>% pull(it), nrow = 21))

################################################################################
#PROJECTION 2001-2101###########################################################
################################################################################
#population means by age in 2001
n01 <- n76 %>% 
  filter(year==2001) %>%
  group_by(name, age) %>%
  summarise(n = mean(n))

#population totals by census in 2001
totalbycensus01 <- n76 %>% 
  filter(year==2001) %>%
  group_by(name, census) %>% 
  summarise(sum = sum(n)) 

#standard deviation of the ratio total / mean of totals  
se_census_ratio <- totalbycensus01 %>%
  group_by(name) %>%
  mutate(mean_sum = mean(sum),
         ratio = sum / mean_sum) %>%
  group_by(name) %>%
  summarise(sd_ratio = sd(ratio)) %>%
  mutate(n = data.frame(name = names, n = 5) %>%
           left_join(readRDS('excluded') %>% group_by(name) %>% summarise(excl. = n())) %>%
           mutate(excl. = ifelse(is.na(excl.), 0, excl.), n = n - excl.) %>%
           pull(n)) %>%
  mutate(se = sd_ratio / sqrt(n))
           
#specify number of runs
runs <- 500

#create folder to store results
dir.create("Results")

#function to run the projection model
proj_fct <- function(l){
  
  #list to store results
  results <- list()
  
  for (r in 1:runs){
    
    #select standard devition of census ratio
    se <- se_census_ratio %>% filter(name == l) %>% pull(se) 
    
    #estimate starting population
    p <- c(n01 %>% filter(name==l, age<=80) %>% mutate(n = round(n*rlnorm(1, 0, se))) %>% pull(n) %>% rpois(17, .), 0,0,0,0) %>% list()
    
    #select it trajectory
    it <- it_m[[which(names==l)]][ , r] 
    
    #specify the mortality schedules
    m <- if(l %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2}else{m1}
    m0 <- if(l %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2_0}else{m1_0}
    
    #Loop through 2101
    for (i in 1:20){
      
      #remove the dead
      p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[ , 5+i])
      
      #find the total number of newborns who acquire the language
      newborns <- sum(rbinom(21, p[[i+1]], f[ , 5+i]*it[i]))
      
      #remove the newborns that die between 0 and 2.5
      newborns <- round(newborns - newborns*m0[5+i])
      
      #add the newborns to the population next year
      p[[i+1]] <- c(newborns, p[[i+1]])[-22]
      
    }
  
    results[[r]] <- unlist(p)
      
  }
  
  #put result in data frame
  bind_rows(
    lapply(1:runs, function(x) 
      data.frame(run = x,
                 name = l,
                 year = rep(seq(2001, 2101, 5), each=21),
                 age = rep(seq(0, 100, 5), 21),
                 n = results[[x]]))) %>%
    saveRDS(paste("Results/", l, sep = ''))
  
  }

#run function
lapply(names, function(x) proj_fct(x))
