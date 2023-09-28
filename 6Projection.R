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

#mortality (WPP No income group available; remove years 1951 through 1996)
m <- readRDS('DemographicParameters')[[1]][[6]][[1]][, -c(1:10)]

#mortality between 0 and 5
m0 <- readRDS('DemographicParameters')[[1]][[6]][[2]][-c(1:10)]

#fertility (Lower-middle-income countries)
f <- readRDS('DemographicParameters')[[2]][[5]][, -c(1:10)]

#load speakers data
backcast_df <- readRDS("backcast") %>% 
  mutate(name_census = paste(name, census)) %>%
  filter(!name_census %in% c(readRDS('excluded') %>% pull(name_census))) %>%
  select(-name_census)

#vector of names
names <- backcast_df %>% pull(name) %>% unique()

#load it trajectory data and assign to matrix
it_m <- lapply(names, function(x) 
  matrix(readRDS(paste('IntergenerationalTransmission/', x, sep = '')) %>% filter(year>=2001) %>% pull(it), nrow = 21))

################################################################################
#PROJECTION 2001-2101###########################################################
################################################################################
#matrix of average counts by age, across all censuses, in year 2001, for each language
mean01_m <- sapply(names, function(l) 
  backcast_df %>% 
    filter(year==2001, name==l, age<=80) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n))

#population totals by census in 2001
totalbycensus01 <- backcast_df %>% 
  filter(year==2001) %>%
  group_by(name, census) %>% 
  summarise(sum = sum(n)) 

#standard error of the ratio total / mean of totals  
se_v <- totalbycensus01 %>%
  group_by(name) %>%
  mutate(mean_sum = mean(sum), ratio = sum / mean_sum) %>%
  group_by(name) %>%
  summarise(sd_ratio = sd(ratio)) %>%
  mutate(n = data.frame(name = names, n = 5) %>%
           left_join(readRDS('excluded') %>% group_by(name) %>% summarise(excl. = n())) %>%
           mutate(excl. = ifelse(is.na(excl.), 0, excl.), n = n - excl.) %>%
           pull(n), 
         se = sd_ratio / sqrt(n)) %>%
  pull(se)

#specify number of runs
runs <- 10

#create folder to store results
dir.create("Results")

#function to run the projection model
proj_fct <- function(l){
  
  #list to store results
  results <- list()
  
  for (r in 1:runs){
    
    #select standard error of census ratio
    se <- se_v[l]
    
    #estimate starting population
    p <- round(mean01_m[, l]*rlnorm(1, 0, se)) %>% rpois(17, .) %>%  c(., rep(0, 4)) %>% list()
    
    #select it trajectory
    it <- it_m[[l]][ , r] 
    
    #Loop through 2101
    for (i in 1:20){
      
      #remove the dead
      p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[, i])
      
      #find the total number of newborns who acquire the language
      newborns <- sum(rbinom(21, p[[i+1]], f[, i]*it[i]))
      
      #remove the newborns that die between 0 and 2.5
      newborns <- round(newborns - newborns * m0[i])
      
      #add the newborns to the population next year
      p[[i+1]] <- c(newborns, p[[i+1]])[-22]
      
    }
  
    results[[r]] <- unlist(p)
      
  }
  
  #put result in data frame
  bind_rows(
    lapply(1:runs, function(x) 
      data.frame(run = x,
                 name = names[l],
                 year = rep(seq(2001, 2101, 5), each=21),
                 age = rep(seq(0, 100, 5), 21),
                 n = results[[x]]))) %>%
    saveRDS(paste("Results/", names[l], sep = ''))
  
  }

#run function
lapply(1:length(names), function(x) proj_fct(x))
