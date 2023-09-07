################################################################################
#IN THIS DO FILE: ESTIMATION OF INTERGERATIONAL TRANSMISSION
#BASED ON THE ESTIMATED POPULATIONS IN 1976 (SEE BACKCAST) 
#FOR THE SELECTION OF THE SPECIFIC LANGUAGES AND YEAR, SEE DATA EVALUATION

################################################################################
#1. GET PACKAGES AND LOAD DATA##################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)
library(betareg)

#set theme
theme_set(theme_bw())

#read in life and fertility matrices
m1 <- readRDS('m1')[, -c(1:5)]
m1_0 <- readRDS('m1_0')[-c(1:5)]
m2 <- readRDS('m2')[, -c(1:5)]
m2_0 <- readRDS('m2_0')[-c(1:5)]
f <- readRDS('f')[, -c(1:5)]

#load speakers data
backcast <- readRDS("backcast") %>% 
  mutate(name_census = paste(name, census)) %>%
  filter(!name_census %in% c(readRDS('excluded') %>% mutate(name_census = paste(name, census)) %>% pull(name_census))) %>%
  select(-name_census)

#vector of names
names <- backcast %>% pull(name) %>% unique()

################################################################################
#3. ESTIMATE INTERGENERATIONAL TRNAMISSION, 1981-2001###########################
################################################################################
#average counts by age, across all years, for each language
n76_mean <- sapply(names, function(l) 
  backcast %>% 
    filter(year==1976, name == l, age<=45) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(pred)) %>% 
    pull(mean_n))

#number of children 0-4 in years 1981 to 2001
age0_4 <- sapply(names, function(l) 
  backcast %>% 
    filter(name == l, age==0, year>=1981, year<=2001) %>% 
    group_by(year) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n))

#Function to forecast populations between 1976 and 2001 supposing full intergenerational transmission
#and obtain predicted probabilities of intergenerational transmission during the period 2001-2101

#specify number of runs
runs <- 500

#define function
it_fct <- function(l){
  
  #create list to store predictions below
  beta_pred <- list()
  counts0_4 <- list()
  
  while (length(beta_pred)<runs){
  
      #estbalish population in 1976
      p <- n76_mean[ , which(names==l)] %>% rpois(1:10, .) %>% list()
      
      #specify the mortality schedules
      m <- if(l == "Inuktitut"){m2}else{m1}
      m0 <- if(l == "Inuktitut"){m2_0}else{m1_0}
      
      #loop until 2001
      for (i in 1:5){
        
        #remove the dead
        p[[i+1]] <- p[[i]] - rbinom(length(p[[i]]), p[[i]], m[, i])
        
        #find the total number of newborns 
        newborns <- sum(rbinom(length(p[[i+1]]), p[[i+1]], f[, i]))
        
        #remove newborns who die between 0 and 2.5 years old
        newborns <- round(newborns - newborns*m0[i])
        
        #add the newborns to the population next year
        p[[i+1]] <- c(newborns, p[[i+1]])
        
      }
      
      #extract total children
      counter <- sapply(2:6, function(x) p[[x]][1])
      
      #estimate actual number of children speakers
      actual <- age0_4[ , which(names==l)] %>% rpois(5, .)
      
      #put counter and actual numbers in df
      counts0_4[[length(beta_pred)+1]] <- data.frame(run = length(beta_pred)+1,
                                                     name = l,
                                                     year = seq(1981, 2001, 5),
                                                     actual = actual,
                                                     counter = counter)
                                                     
      #find it values
      it_trend <- actual / counter 
      
      #transform if values are too close to 0 or 1
      it_trend <- ifelse(it_trend>.9999, .9999, ifelse(it_trend<0.0001, 0.0001, it_trend))
      
      #estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
      #if error, loops continue at the same index number
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
               it = unlist(beta_pred)) %>%
      saveRDS(paste("IntergenerationalTransmission/", l, sep = ''))
    
    saveRDS(bind_rows(counts0_4), paste("IntergenerationalTransmission/", l, '_counts0_4', sep = ''))
    
    }

#run function
lapply(names, function(x) it_fct(x))

################################################################################