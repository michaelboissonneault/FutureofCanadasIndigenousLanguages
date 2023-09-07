################################################################################
#IN THIS DO FILE
  #1. GET PACKAGES AND LOAD DATA
  #2. T-TEST FOR THE EQUALITY OF THE TOTAL NUMBER OF SPEAKERS BETWEEN CENSUSES
  #3. INTERACTION TESTS FOR THE PARALLEL DISTRIBUTIONS OF SPEAKER NUMBERS BY AGE
  #4. SUMMARIZING AND SAVING RESULTS

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
f <- readRDS('f')[, -c(1:5)]

#Backcast data year 2001
n01 <-  readRDS("backcast") %>% filter(year==2001)

################################################################################
#2.T-TEST FOR THE EQUALITY OF THE TOTAL NUMBER OF SPEAKERS BETWEEN CENSUSES#####
################################################################################
#aggregate counts by language and census
n01_sum <- n01 %>% group_by(name, census) %>% summarise(n = sum(n)) 

#names vector
names <- n01 %>% pull(name) %>% unique()

#function to determine whether values for an entire population in a given year
#are significantly different from the values in the other years
census_error_test_fct <- function(l){
  
  #years in which language l appears
  yrs <- n01_sum %>% filter(name == names[l]) %>% pull(census)

  #identify all possible combinations of years involving at least three years
  combi <- lapply(3:length(yrs), function(x) combn(yrs, x, simplify = F))

  #rearrange the combinations so that each fits in one list
  combi_list <- if(length(yrs)==5){append(append(combi[[1]], combi[[2]]), combi[[3]])}else if(length(yrs)==4){append(combi[[1]], combi[[2]])}else{combi[[1]]}
                     
  #use t-test to find the 95% population size confidence interval for a given combination of years
  t_test_interval <- lapply(1:length(combi_list), function(x) 
    n01_sum %>% filter(name == names[l], census %in% combi_list[[x]]) %>% pull(n) %>% t.test(., mu = 100))

  #extract from the t-test result the 95% confidence interval
  intervals <- sapply(1:length(combi_list), function(x) t_test_interval[[x]]$conf.int[2] - t_test_interval[[x]]$conf.int[1])

  #vector of years to which the narrowest interval corresponds
  narrowest_interval <- combi_list[[which(intervals==min(intervals))]]
  
  #vector of population sizes in each year
  all_values <- n01_sum %>% filter(name == names[l]) %>% pull(n)
  
  #vector of values that correspond to the narrowest interval 
  narrowest_interval_values <- n01_sum %>% filter(name == names[l], census %in% narrowest_interval) %>% pull(n)
  
  #perform t-test to determine whether each value belongs to the population defined by the narrowest interval
  t_test_pvalue <- sapply(1:length(yrs), function(x) t.test(narrowest_interval_values, mu = all_values[x])$p.value)
  
  #data frame with results
  data.frame(name = names[l], census = yrs, pvalue = t_test_pvalue)
  
  }

#run function
census_error_test <- bind_rows(lapply(1:length(names), function(x) census_error_test_fct(x)))

#merge results to main df
n01_sum <- n01_sum %>% left_join(census_error_test)

#make discrete variable with teh pvalue
n01_sum <- n01_sum %>%
  mutate(pvalue_discrete = case_when(
    pvalue<0.01 ~ "<0.01",
    pvalue>=0.01 & pvalue <0.05 ~ "<0.05",
    pvalue>=0.05 & pvalue<0.1 ~ "<0.1",
    pvalue>=0.1 ~ ">=0.1"
  ))

#visualization
ggplot(n01_sum %>% filter(grepl('continuum', name)))+
  geom_tile(aes(census, name, fill=pvalue_discrete, color=pvalue_discrete))+
  scale_x_continuous(breaks=seq(2001, 2021, 5))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral')+
  scale_color_brewer(palette = 'Spectral')

ggplot(n01_sum %>% filter(!grepl('continuum', name), !name %in% c('Ojibway', 'Cree', 'ikt')))+
  geom_tile(aes(census, name, fill=pvalue_discrete, color=pvalue_discrete))+
  scale_x_continuous(breaks=seq(2001, 2021, 5))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral')+
  scale_color_brewer(palette = 'Spectral')

n01_sum %>% filter(pvalue<0.01, !name %in% c('Ojibway', 'Cree', 'ikt')) %>% select(-pvalue_discrete) %>% pivot_wider(names_from = census, values_from = pvalue) %>% print(n=23)

################################################################################
#3.INTERACTION TESTS FOR THE PARALLEL DISTRIBUTIONS OF SPEAKER NUMBERS BY AGE###
################################################################################
#get all the 2X2 census combinations
census_combn <- combn(seq(2001, 2021, 5), 2)

#vector of names
names <- n01 %>% filter(name!='Inuinnaqtun (continuum)', name!='ikt') %>% pull(name) %>% unique()

#function to estimate the interaction between census and age
interactions <- bind_rows(
  lapply(names, function(x)
    data.frame(name = x, 
               year1 = census_combn[1, ], 
               year2 = census_combn[2, ], 
               pvalue = sapply(1:10, function(y) summary(lm(pred ~ age + census + age*census, 
                                                            data = n01 %>% 
                                                              filter(age<=50, name==x, census %in% census_combn[ , y])))$coefficients[4, 4]))))

#discrete pvalue categories
interactions <- interactions %>% 
  mutate(sig_level = case_when(
    pvalue<0.001 ~ "0.001",
    pvalue>=0.001 & pvalue<0.01 ~ "0.01",
    pvalue>=0.01 & pvalue<0.05 ~ "0.05",
    pvalue>=0.05 & pvalue<0.1 ~ "0.1",
    pvalue>=0.1 ~ ">=0.1"
  )) %>% 
  mutate(sig_level = factor(sig_level, levels = rev(c('0.001', '0.01', '0.05', '0.1', '>=0.1'))))

#heat maps
ggplot(interactions)+
  geom_tile(aes(year1, year2, fill=sig_level))+
  scale_fill_brewer(palette = 'Reds')+
  facet_wrap(~name)+
  scale_x_continuous(breaks = seq(2001, 2021, 5))+
  scale_y_continuous(breaks = seq(2001, 2021, 5))

#sum pvalues by language and census year
sum_pvalues <- interactions %>% 
  pivot_longer(c(year1, year2), values_to = 'year', names_to = 'fuhgetaboutit') %>% 
  select(-fuhgetaboutit) %>%
  group_by(name, year) %>% 
  summarise(sum_pvalue = sum(pvalue)) %>%
  mutate(sum_pv_dis = case_when(
    sum_pvalue>=1 ~ ">=1",
    sum_pvalue<1 & sum_pvalue>=0.1 ~ "[0.1; 1[",
    sum_pvalue<0.1 & sum_pvalue>=0.01 ~ "[0.01; 0.1[",
    sum_pvalue<0.01 ~ "<0.01"
  )) %>%
  mutate(sum_pv_dis = factor(sum_pv_dis, levels = rev(c("<0.01", "[0.01; 0.1[", "[0.1; 1[", ">=1"))))

sum_pvalues %>% group_by(name) %>% summarise(sum_sum_pvalue = sum(sum_pvalue)) %>% arrange(sum_sum_pvalue)
sum_pvalues %>% group_by(year) %>% summarise(sum_sum_pvalue = sum(sum_pvalue)) %>% arrange(sum_sum_pvalue)

#what year perform better in each language? 
ggplot(sum_pvalues)+
  geom_tile(aes(year, name, fill = sum_pv_dis))+
  scale_fill_brewer(palette = 'Reds')

#what to drop?
sum_pvalues %>% 
  filter(sum_pvalue<.1) %>% pull(name) %>% unique() -> drop #adjust level here

ggplot(n01 %>% filter(name %in% drop) %>% mutate(census = as.factor(census)))+
  geom_line(aes(age, n, group=census, color=census))+
  facet_wrap(~name, scales = 'free_y')+
  ggtitle("Dakotan 2006+2021; Dene, chp, ike 2021")

################################################################################
#4. SUMMARIZING AND SAVING RESULTS
################################################################################
#combination of data frames
combined <- n01_sum %>% 
  mutate(valid_pop = 
           case_when(pvalue<0.001 ~ 2,
                     pvalue>=0.001 & pvalue<0.01 ~ 1,
                     pvalue>=0.01 ~ 0)) %>% 
  select(name, census, valid_pop) %>% 
  left_join(sum_pvalues %>%
              mutate(valid_age = 
                       case_when(sum_pvalue<0.01 ~ 2,
                                 sum_pvalue>=0.01 & sum_pvalue<0.1 ~ 1, 
                                 sum_pvalue >=.1 ~ 0)) %>% 
              select(name, year, valid_age) %>% 
              rename(census = year)) %>%
  filter(!name %in% c('Ojibway', 'Cree', 'ikt')) %>%
  mutate(eval = case_when(
    valid_pop == 0 & valid_age==0 ~ "Good",
    valid_pop == 1 & valid_age==0 ~ "Caution pop",
    valid_pop == 0 & valid_age==1 ~ "Caution age",
    valid_pop == 1 & valid_age==1 ~ "Caution pop & age",
    valid_pop == 2  & valid_age==0 ~ "Prob. pop",
    valid_pop == 0 & valid_age==2 ~ "Prob. age",
    valid_pop == 2 & valid_age==2 ~ "Prob. pop & age",
    valid_pop == 2 & valid_age==1 ~ "Prob. pop, caution age",
    valid_pop == 1 & valid_age==2 ~ "Caution pop, prob. age")) %>%
  mutate(eval = factor(eval, 
                       levels = rev(c("Good", "Caution pop", "Caution age", "Caution pop & age", 
                                  "Prob. pop", "Prob. age", "Prob. pop, caution age",
                                   "Caution pop, prob. age", "Prob. pop & age" ))))

#heat map
ggplot(combined)+
  geom_tile(aes(census, name, fill=eval, color=eval))+
  scale_x_continuous(breaks=seq(2001, 2021, 5))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral')+
  scale_color_brewer(palette = 'Spectral')

#exclude t-test p value < 0.001, sum of interactions' pvalues <0.01
excluded <- combined %>% 
  filter(valid_pop==2 | valid_age==2) %>% 
  mutate(reason = ifelse(valid_pop==2 & valid_age==2, "Both",
                         ifelse(valid_pop==2 & valid_age!=2, "Pop", "Age"))) %>%
  select(name, census, reason)

#save
saveRDS(excluded, "excluded")
