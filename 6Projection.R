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

#read in life and fertility tables
lt <- readRDS("C:/Users/micbo262/Documents/WPPData/WPP2022_Life_Table_Abridged_Medium_Both_ExcludingSingleCountries")
ft <- readRDS("C:/Users/micbo262/Documents/WPPData/WPP2022_Fertility_by_Age5_Medium_ExcludingSingleCountries")

#choice of schedules
m_sched <- "Upper-middle-income countries"
f_sched <- "Lower-middle-income countries"

#mortality probabilities: registered indians
m1 <- lt %>% 
  filter(Location==m_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart!=0) %>%
  mutate(qx = ifelse(AgeGrpStart==1, 1-lx/100000, qx)) %>%
  pull(qx) %>%
  matrix(nrow=21)

#correction to have mortality halfway
m1 <- (m1[-21, -26] + m1[-1, -1])/2
m1 <- rbind(m1, matrix(rep(1, 25), ncol=25))

#mortality between 0 and 2.5 (take mortality 0-1 and that between 1-4 x (2/3))
m1_0 <- c(
  lt %>% 
    filter(Location==m_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart==0) %>%
    pull(qx)+ 
    lt %>% 
    filter(Location==m_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart==1) %>%
    pull(qx)*(2/3))[-26]

#mortality between 0 and 5 is actuall between 2.5 and 5
m1[1,] <- c(lt %>% 
              filter(Location==m_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart==1) %>%
              pull(qx)*(1/3))[-26]

#mortality probabilities: Inuit
m2 <- lt %>% 
  filter(Location==f_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart!=0) %>%
  mutate(qx = ifelse(AgeGrpStart==1, 1-lx/100000, qx)) %>%
  pull(qx) %>%
  matrix(nrow=21)

#correction to have mortality halfway
m2 <- (m2[-21, -26] + m2[-1, -1])/2
m2 <- rbind(m2, matrix(rep(1, 25), ncol=25))

#mortality between 0 and 2.5 (take mortality 0-1 and that between 1-4 x (2/3))
m2_0 <- c(
  lt %>% 
    filter(Location==f_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart==0) %>%
    pull(qx)+ 
    lt %>% 
    filter(Location==f_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart==1) %>%
    pull(qx)*(2/3))[-26]

#mortality between 0 and 5 is actuall between 2.5 and 5
m2[1,] <- c(lt %>% 
              filter(Location==f_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart==1) %>%
              pull(qx)*(1/3))[-26]

#fertility rates
f <- ft %>% 
  filter(Location==f_sched, Time %in% c(seq(1976, 2096, 5), 2100), AgeGrpStart<=45) %>%
  pull(ASFR) %>%
  matrix(nrow=8)

#fill the rest of the fertility matrix with zeros
f <- rbind(matrix(nrow=2, ncol=26, 0), f, matrix(nrow=11, ncol=26, 0))/400

#load speakers data
n76 <- readRDS("backcast_reduced")

#load intergenerational transmission data
it_pred <- readRDS("predicted_it")

################################################################################
#PROJECTION 2001-2101###########################################################
################################################################################
#vector of names
names <- n76 %>% pull(name) %>% unique()

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
sd01 <- totalbycensus01 %>%
  group_by(name) %>%
  mutate(mean_sum = mean(sum),
         ratio = sum / mean_sum) %>%
  group_by(name) %>%
  summarise(sd_ratio = sd(ratio))

#function to run the projection model
proj_fct <- function(l, r){
  
  #select standard devition of census ratio
  sd_cens_ratio <- sd01 %>% filter(name == l) %>% pull(sd_ratio) 
  
  #estimate starting population
  p <- c(n01 %>% filter(name==l, age<=80) %>% mutate(n = round(n*rlnorm(1, 0, sd_cens_ratio))) %>% pull(n) %>% rpois(17, .), 0,0,0,0) %>% list()
  
  #select it trajectory
  it <- it_pred %>% filter(name==l, run == r) %>% pull(it)
  
  #specify the mortality schedules
  m <- if(l %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2}else{m1}
  m0 <- if(l %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2_0}else{m1_0}
  
  #Loop through 2101
  for (i in 1:20){
    
    #remove the dead
    p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[, 5+i])
    
    #find the total number of newborns who acquire the language
    newborns <- sum(rbinom(21, p[[i+1]], f[, 5+i]*it[i]))
    
    #remove the newborns that die between 0 and 2.5
    newborns <- round(newborns - newborns*m0[5+i])
    
    #add the newborns to the population next year
    p[[i+1]] <- c(newborns, p[[i+1]])[-22]
    
  }
  
  #put result in data frame
  data.frame(run=r, 
             name = l, 
             year = rep(seq(2001, 2101, 5), each = 21), 
             age = rep(seq(0, 100, 5), 21), 
             n = unlist(p))
  
}

#run function
proj_results <- bind_rows(lapply(names, function(x) 
  lapply(1:100, function(y) proj_fct(x, y))))

################################################################################
#ANALYSIS#######################################################################
################################################################################
#total speaker median and 80% CI by year
proj_results_sum <- proj_results %>% 
  group_by(run, name, year) %>% 
  summarise(sum_n = sum(n)) %>% 
  group_by(name, year) %>%
  summarise(q2 = quantile(sum_n, .5), lower = quantile(sum_n, .1), upper = quantile(sum_n, .9))

proj_results_sum %>% filter(year==2101) %>% select(-year) %>% arrange(q2)

#show trends
ggplot(proj_results_sum)+
  geom_line(aes(year, q2))+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha = .3)+
  facet_wrap(~name, scales = 'free_y')

#speakers in 2100 by age group 
minage_2101 <- proj_results %>% 
  filter(year==2101, n>0) %>% 
  group_by(name, run) %>%
  summarise(min_age = min(age)) %>% 
  group_by(name) %>% 
  summarise(q2 = quantile(min_age, .5), lower = quantile(min_age, .1), upper = quantile(min_age, .9))  

#extinction risk
proj_results %>% 
  group_by(run, name, year) %>% 
  summarise(sum_n = sum(n)) %>%
  filter(year==2101, sum_n==0) %>% 
  group_by(name) %>%
  summarise(n = n())

