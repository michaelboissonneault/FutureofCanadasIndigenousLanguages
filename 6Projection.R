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

#load it trajectory data, put in matrices, assign to list
it_l <- lapply(names, function(x) 
  matrix(readRDS(paste('IntergenerationalTransmission/', x, sep = '')) %>% filter(year>=2001) %>% pull(it), nrow = 21))

################################################################################
#PROJECTION 2001-2101###########################################################
################################################################################
#matrix of average counts by age, across all censuses, in year 2001, for each language
counts_by_age <- sapply(names, function(l) 
  backcast_df %>% 
    filter(year==2001, name==l, age<=80) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n))

#population totals by census (in 2001)
totals_by_census <- sapply(names, function(l)
  backcast_df %>% 
    filter(year==2001, name==l) %>%
    group_by(census) %>% 
    summarise(sum = sum(n)) %>%
    pull(sum))

#mean of the totals, for each language 
total_means <- sapply(1:length(names), function(x) mean(totals_by_census[[x]]))

#standard error of the totals, for each language 
total_ses <- sapply(1:length(names), function(x) sd(totals_by_census[[x]]) / sqrt(length(totals_by_census[[x]])))

#counts by age, standardized for each language
counts_by_age_st <- sapply(1:length(names), function(x) counts_by_age[ , x] / total_means[x]) 

#specify number of runs
runs <- 1000

#create folder to store results
dir.create("Results")

#function to run the projection model
proj_fct <- function(l){
  
  #list to store results
  results <- list()
  
  for (r in 1:runs){
    
    #estimate starting population, put in list
    p <- list(c(rpois(17, rnorm(1, total_means[l], total_ses[l])*counts_by_age_st[ , l]), rep(0, 4)))
    
    #Loop through 2101
    for (i in 1:20){
      
      #remove the dead
      p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[, i])
      
      #find the total number of newborns who acquire the language
      newborns <- sum(rbinom(21, p[[i+1]], f[, i]*it_l[[l]][i, r]))
      
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

#run function (1,000 runs take about 2-3 minutes)
lapply(1:length(names), function(x) proj_fct(x))
