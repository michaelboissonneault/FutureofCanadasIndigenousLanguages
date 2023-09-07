################################################################################
#IN THIS DO FILE: ESTIMATION OF POPULATION COUNTS BY AGE IN THE YEAR 1976
  #COUNTS ARE BASED ON THE FIVE CENSUS ROUNDS 2001-2021 (EXCEPT ikt WHICH IS FOUR 2006-2021)
 
################################################################################
#1. GET PACKAGES AND LOAD DATA##################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)

#set theme
theme_set(theme_bw())

#read in life and fertility matrices
m1 <- readRDS('m1')[, -c(1:5)]
m2 <- readRDS('m2')[, -c(1:5)]
m1_0 <- readRDS('m1_0')[-c(1:5)]
m2_0 <- readRDS('m2_0')[-c(1:5)]
f <- readRDS('f')[, -c(1:5)]

#reverse mortality matrices, period 1976-2021
m1_rev <- m1[,c(9:1)]
m2_rev <- m2[,c(9:1)]

#Statcan data
sc <- readRDS("scdata") 

################################################################################
#2. ESTIMATE SPEAKER NUMBERS IN THE YEAR 1976 ##################################
################################################################################
#name and year data frame
names_years <- sc %>% select(name, year) %>% unique()

#Function to apply mortality probabilities retrospectively to the observed counts in 2006, 2011, 2016, 2021
backcast_fct <- function(l){
  
  #specify name, year, and period covered
  na <- names_years$name[l] 
  yr <- names_years$year[l]  
  lg <- length(seq(1981, yr, 5))
  
  #identify the population from given census
  p <- sc %>% filter(name == na, year == yr) %>% pull(pop) %>% list()
  
  #specify the mortality schedule
  m_rev <- if(na %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2_rev}else{m1_rev}
  
  for (i in 1:lg){
    
    p[[i+1]] <- c(p[[i]] + p[[i]]*m_rev[ , (9-lg+i)], 0)[-1]
    
  }
  
  data.frame(name=na, 
             age=rep(seq(0, 100, 5), lg+1), 
             census = yr, 
             year = rep(seq(yr, 1976, -5), each=21), 
             n=unlist(p))
  
}

#run function
n76 <- bind_rows(lapply(1:length(names_years$name), function(x) backcast_fct(x)))

#show pyramids
ggplot(n76 %>% filter(year==1976) %>% mutate(census = as.character(census)))+
  geom_line(aes(age, n, group=census, color=census))+
  facet_wrap(~name, scales = 'free_y')

#estimate loess model
n76_loess <- n76 %>% group_by(name, census, year) %>% do(model = loess(n ~ age, data =., span = .4))

#extract loess predictions
n76_loess <- bind_rows((lapply(1:length(n76_loess$name), function(x) 
  data.frame(name = n76_loess$name[x],
             year = n76_loess$year[x],
             census = n76_loess$census[x],
             age = seq(0, 100, 5),
             pred=predict(n76_loess$model[[x]])))))

#merge the loess predictions with the main dataset
n76 <- n76 %>% 
  left_join(n76_loess) %>%
  mutate(pred = ifelse(pred<0, 0, pred),
         pred = ifelse(age==0 & n==0 & lag(n)==0, 0, pred))

#show pyramids again
ggplot(n76 %>% filter(year==1976) %>% mutate(census = as.character(census)))+
  geom_line(aes(age, pred, group=census, color=census))+
  facet_wrap(~name, scales = 'free_y')

#show how much it matters that we correct for mortality
comparison <- n76 %>% 
  filter(year==2001) %>%
  group_by(name, census) %>%
  summarise(sum_n = sum(n)) %>% 
  left_join(sc %>% 
              group_by(name, year) %>%
              summarise(sum_pop = sum(pop)) %>%
              rename(census = year))

#plot
ggplot(comparison)+
  geom_ribbon(aes(census, ymin = sum_n, ymax = sum_pop), alpha = .3)+
  geom_line(aes(census, sum_n), color = 'red')+
  geom_line(aes(census, sum_pop), color = 'blue')+
  facet_wrap(~name, scales = 'free_y')

#save datasets
saveRDS(n76, "backcast")
