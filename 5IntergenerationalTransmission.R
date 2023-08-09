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
#n76 <- readRDS("backcast_full")
n76 <- readRDS("backcast_reduced")

################################################################################
#3. ESTIMATE INTERGENERATIONAL TRNAMISSION, 1981-2001###########################
################################################################################
#average counts by age, across all years, for each language
n76_mean <- n76 %>% 
  filter(year==1976) %>%
  group_by(name, age) %>% 
  summarise(mean_n = mean(pred))

#name vector
names <- unique(n76$name)

#Function to forecast populations between 1976 and 2021 supposing full intergenerational transmission
#and obtain predicted probabilities of intergenerational transmission during the period 2001-2101
it_fct <- function(l, r){
  
  #estbalish population in 1976
  p <- n76_mean %>% filter(name == l) %>% pull(mean_n) %>% rpois(1:10, .) %>% list()
  
  #specify the mortality schedules
  m <- if(l %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2}else{m1}
  m0 <- if(l %in% c('ike', "ikt", "Inuinnaqtun (continuum)")){m2_0}else{m1_0}
  
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
  actual <- n76 %>% 
    filter(name == l, age==0, year>=1981, year<=2001) %>% 
    group_by(year) %>% 
    summarise(mean_n = mean(n)) %>% 
    pull(mean_n) %>%
    rpois(5, .)
  
  #find it values
  it_trend <- actual / counter 
  
  #transform if values equal or exceed 0 or 1
  it_trend <- ifelse(it_trend>=1, .9999, ifelse(it_trend<=0, 0.0001, it_trend))
  
  #estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
  beta_model <- betareg(it ~ year, data = data.frame(it = it_trend, year = seq(2001, 2021, 5)))
  
  #extract predictions until 2100, put in df
  data.frame(run = r,
             name = l,
             year = seq(2001, 2101, 5),
             it = predict(beta_model, newdata = data.frame(year=seq(2001, 2101, 5))))

  }

#run function
it_df <- bind_rows(lapply(names, function(x) lapply(1:100, function(y) it_fct(x, y))))

#show results
ggplot(it_df)+
  geom_line(aes(year, it, group = run), alpha = .05)+
  geom_smooth(aes(year, it))+
  facet_wrap(~name)

#save results
saveRDS(it_df, "predicted_it")

################################################################################


#find actual numbre of children
actual <- lapply(1:runs, function(x) 
  lapply(names, function(y)
    n76 %>% 
      filter(name == y, age==0, year>=1981, year<=2001) %>% 
      group_by(year) %>% 
      summarise(mean_n = mean(n)) %>% 
      pull(mean_n) %>%
      rpois(5, .)))

#find it trend
it_trend_mat <- lapply(1:runs, function(x) lapply(1:length(names), function(y) actual[[x]][[y]] / counter[[x]][[y]]))

#put it trend in data frame
it_trend_df <- bind_rows(lapply(1:runs, function(x) lapply(1:length(names), function(y) 
  data.frame(run = x, name = names[y], year = seq(1981, 2001, 5), it = it_trend_mat[[x]][[y]]))))

#show trends in graph, one line per run, one graph per language
ggplot(it_trend_df)+
  geom_line(aes(year, it, group = run), alpha = .05)+
  geom_smooth(aes(year, it))+
  facet_wrap(~name)

#transorm values equal to or beyond 0 and 1 for the beta model below
it_trend_df <- it_trend_df %>% 
  mutate(it = ifelse(it>=1, .9999, it),
         it = ifelse(it<=0, .0001, it))

#for each run, estimate beta model (Cribari-Neto Zeileis 2023 J. Statist. Software)
beta_model <- it_trend_df %>% group_by(run, name) %>% do(model = betareg(it ~ year, data = .))

#extract the predictions until year 2101
it_predictions <- bind_rows(lapply(1:length(beta_model$run), function(x) 
  data.frame(run = beta_model$run[x], name = beta_model$name[x], year = seq(2001, 2101, 5), pred = predict(beta_model$model[[x]], newdata = data.frame(year=seq(2001, 2101, 5))))))

#save predictions
saveRDS(it_predictions, "it_predictions")

#show predictions
ggplot(it_predictions)+
  geom_line(aes(year, pred, group = run), alpha = .1)+
  geom_smooth(aes(year, pred))+
  facet_wrap(~name)
