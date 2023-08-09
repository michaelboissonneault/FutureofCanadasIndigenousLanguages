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

#reverse mortality matrices, period 1976-2021
m1_rev <- m1[,c(9:1)]
m2_rev <- m2[,c(9:1)]

#Statcan data
sc <- readRDS("C:/Users/micbo262/Documents/DataSpeakerNumbers/Canada/Analyses/scdata") 

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

#combinations of census and languag that shouldn't be included
n76_red <- n76 %>% 
  filter(name!="Cree", name!="ikt", name!="Ojibway",
    census!=2021 | name!="Cree (continuum)",
    census!=2021 | name!="atj",
    census!=2021 | name!='crx',
    census!=2021 | name!='Dakotan (continuum)',
    census!=2021 | name!="Innu-Naskapi (continuum)",
    census!=2021 | name!="kut",
    census!=2021 | name!='mic',
    census!=2016 | name!='moh',
    census!=2021 | name!="Ojibway (continuum)",
    census!=2011 | census!=2021 | name!="scs",
    census!=2021 | name!='thp',
    census!=2011 | census!=2021 | name!='xsl')

#save datasets
saveRDS(n76, "backcast_full")
saveRDS(n76_red, "backcast_reduced")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#order names by it level at the beginning and at the end
names_by_level <- it_predictions %>% 
  filter(year==2001 | year==2101) %>%
  group_by(name, year) %>%
  summarise(mean_pred = mean(pred)) %>%
  pivot_wider(names_from = year, values_from = mean_pred) %>%
  arrange(`2001`, `2101`) %>%
  pull(name)

#arrange predictions by levels in the beginning and at the end
it_predictions <- it_predictions %>%
  mutate(name = factor(name, levels = names_by_level))

#year 2001 (average)
n01_mean <- n76 %>% 
  filter(year==2001) %>%
  group_by(name, age) %>% 
  summarise(mean_n = mean(pred))

#age pyramid
ggplot(n01_mean)+
  geom_col(aes(age, mean_n))+
  facet_wrap(~name, scales = 'free_x')+
  coord_flip()

################################################################################
#EVALUATION OF VALIDITY OF IT###################################################
################################################################################
#The estimated levels of it will only be valid if the level of fertility are accurate
#We test this by projecting the Atikamekw population until 2016
test_fct <- function(r){
    
  #establish population in 1976
  p <- n76_mean %>% filter(name=='atj', age<=45) %>% pull(mean_n) %>% rpois(10, .) %>% list()
  
  #create empty df list
  df <- list()
  
  for (i in 1:9){
    
    #remove the dead
    p[[i+1]] <- p[[i]] - rbinom(length(p[[i]]), p[[i]], m[ , i])
    
    #find the total number of newborns
    newborns <- sum(rbinom(21, p[[i+1]], f[, i]))
    
    #remove newborns who die between 0 and 2.5
    newborns <- round(newborns - newborns*m0[i])
    
    #add the newborns to the population next year
    p[[i+1]] <- c(newborns, p[[i+1]])[-22]
    
    #put result in df
    df[[i]] <- data.frame(year=seq(1981, 2021, 5)[i], age = seq(0, 100, 5)[1:length(p[[i+1]])], n = p[[i+1]])
    
  }
  
  bind_rows(df) %>% mutate(run = r)

}

#run function
test_result <- bind_rows(lapply(1:100, function(x) test_fct(x)))

#estimate 90% CI
test_result_CI <- test_result %>% group_by(year, age) %>% summarise(lower = quantile(n, 0.05), upper = quantile(n, .95))

#add the real figures
test_comp <- test_result_CI %>% filter(year>=2001) %>% left_join(sc %>% filter(name=='atj') %>% select(-continuum))

#visualize result
ggplot(test_comp)+
  geom_point(aes(age, pop))+
  geom_ribbon(aes(age, ymin = lower, ymax = upper), alpha = .2)+
  facet_wrap(~year)

################################################################################
#CHECK IF THERE IS ATTRITION####################################################
################################################################################
ggplot(n76 %>% 
         filter(year==2001) %>% 
         mutate(age = case_when(
           age<15 ~ "0",
           age>=15 & age<30 ~ "15",
           age>=30 & age<45 ~ "30",
           age>=45 & age<60 ~ "45",
           age>=60 ~ "60")) %>%
         group_by(name, census, age) %>% 
         summarise(pred = sum(pred)))+
  geom_line(aes(census, pred, color = age, group = age))+
  facet_wrap(~name, scale = 'free_y')+
  scale_color_brewer(palette = "Spectral")

################################################################################
#PROJECTION###################################################################
################################################################################
#Loop through 2101
for (i in 1:20){
  
  #specify the value
  it_value <- it_predictions[1+i, it_traject]
  it_value <- ifelse(it_value>1, 1, it_value)
  
  #remove the dead
  p[[i+1]] <- p[[i]] - rbinom(21, p[[i]], m[, 10+i])
  
  #find the total number of newborns who acquire the language
  newborns <- sum(rbinom(21, p[[i+1]], f[, 10+i]*it_value))
  
  #add the newborns to the population next year
  p[[i+1]] <- c(newborns, p[[i+1]])[-22]
  
}

#put result in data frame
data.frame(run=r, year = rep(seq(2001, 2101, 5), each = 21), age = rep(seq(0, 100, 5), 21), n = unlist(p))

}


plot(n81 %>% filter(name=='gwi', census =='2001') %>% pull(n))
lines(n81 %>% filter(name=='gwi', census =='2021') %>% pull(n))


#add the 2001 numbers
n01 <- bind_rows(
  n01, 
  sc %>% filter(year==2001, age<=80) %>% select(pop, age, year, name) %>% rename(n = pop, census = year)
) %>% arrange(name, census, age)

#aggregate counts by language and census
n01_sum <- n01 %>% group_by(name, census) %>% summarise(n = sum(n)) 

################################################################################
#3. TEST WHETHER COUNTS ARE LIKELY TO COME FROM THE SAME POPULATION#############
################################################################################
#names vector
names <- sc %>% pull(name) %>% unique()

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

#add continuum information
n01_sum <- left_join(n01_sum, sc %>% select(name, continuum) %>% unique() %>% filter(!is.na(continuum)))

#visualization
ggplot(n01_sum %>% filter(!is.na(continuum)))+
  geom_tile(aes(census, name, fill=pvalue_discrete, color=pvalue_discrete))+
  scale_x_continuous(breaks=seq(2001, 2021, 5))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral')+
  scale_color_brewer(palette = 'Spectral')+
  facet_wrap(~continuum, scales = 'free_y')

ggplot(n01_sum %>% filter(is.na(continuum)))+
  geom_tile(aes(census, name, fill=pvalue_discrete, color=pvalue_discrete))+
  scale_x_continuous(breaks=seq(2001, 2021, 5))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral')+
  scale_color_brewer(palette = 'Spectral')+
  facet_wrap(~continuum)

################################################################################
#4. TEST FOR THE CONSISTENCY OF THE PROBABILITY OF INTERGENERATIONAL TRANSMISSION
################################################################################
#function for calculating intergenerational transmission
it_fct <- function(r){
  
  #years in which language l appears
  yrs <- n01 %>% filter(name == l) %>% pull(census) %>% unique()
  
  #population in year t
  p <- lapply(yrs, function(x) n01 %>% filter(name==l, census==x) %>% pull(n))
  
  #pop aged 0-4 in t
  b <- lapply(1:length(yrs), function(x) p[[x]][1])
  
  #population in year t + deaths t-5, t
  p <- lapply(1:length(yrs), function(x) p[[x]] + p[[x]]*m[ , x])
  
  #births counter t-5, t
  c <- lapply(1:length(yrs), function(x) sum(rbinom(21, round(p[[x]]), f[ , x])))
  
  unlist(b) / unlist(c)
  
  #put result in data frame
  data.frame(run=r, census = seq(2001, 2021, 5), births=unlist(b), counter = unlist(c)) %>% 
    mutate(proportion = births / counter,
           itr = itr[10:14])
  
}

#run function to see trend in it
it_trend <- bind_rows(lapply(1:300, function(x) it_01_21(x)))



#age groups of 20 (or 25) years
n01_agegroups <- n01 %>%
  mutate(age = case_when(
    age <=15 ~ 0,
    age >=20 & age<=35 ~ 20,
    age >=40 & age<=55 ~ 40,
    age>=60 ~ 60
  )) %>% 
  group_by(name, age, census) %>%
  summarise(n = sum(n))

#Center speaker numbers around their sum over all censuses, for each language
age_profile <- n01_agegroups %>% 
  left_join(n01_agegroups %>% group_by(name, age) %>% summarise(mean_n = mean(n))) %>% 
  mutate(ratio = ifelse(mean_n==0, 1, n / mean_n),
         logratio = log(ratio+1))

#visualize
ggplot(age_profile)+
  geom_line(aes(age, logratio, group=as.factor(census), color=as.factor(census)))+
  facet_wrap(~name)

#regression model with interaction age*census
intmodel <- age_profile %>% 
  group_by(name) %>% 
  filter(census<=2006) %>% 
  do(model = summary(lm(logratio ~ age + census + age*census, .)))

#extract p values for interaction term
p_int <- sapply(1:length(names), function(x) intmodel$model[[x]]$coefficients[4,4])

#put in data frame
viz_int_agecensus <- data.frame(name=names, p_int = round(p_int, 3)) %>% 
  mutate(sig_level = case_when(
    p_int  >=.1 ~ "0.1 or above",
    p_int  <.1 & p_int>=0.05 ~ "Greater or equal to 0.05, below 0.1",
    p_int  < 0.05 & p_int>=0.01 ~ "Greater or equal to 0.01, below 0.05",
    p_int  <0.01 ~ "Below 0.01"), specification = "Full set")

#specify levels for name
viz_int_agecensus$name <- factor(viz_int_agecensus$name, levels=rev(names))

#specify levels for sig level
viz_int_agecensus$sig_level <- factor(viz_int_agecensus$sig_level, 
                                      levels= rev(c("Below 0.01", "Greater or equal to 0.01, below 0.05", "Greater or equal to 0.05, below 0.1", "0.1 or above")))

#visualization
ggplot(viz_int_agecensus)+
  geom_tile(aes(specification, name, fill=sig_level, color=sig_level))+
  scale_fill_brewer(palette = 'YlOrRd')+
  scale_color_brewer(palette = 'YlOrRd')+
  ylab("")

#age pyramids
ggplot(n01 %>% filter(name=="clc" | name=="Innu-Naskapi (continuum)"))+
  geom_col(aes(age, n))+
  facet_grid(census ~ name, scales = "free_x")+
  coord_flip()

ggplot(n01 %>% filter(name=="kut") %>% mutate(census = as.character(census)))+
  geom_line(aes(age, n, color = census, group = census))+
  geom_smooth(aes(age, n))
  facet_grid(census ~ name, scales = "free_x")+
  coord_flip()

  n01 %>% filter(name=="kut") %>% mutate(census = as.character(census))

loess_kut <- loess(n ~ age + census, data = n01 %>% filter(name=='kut'))  

kut <- n01 %>% 
  filter(name=='kut') %>% 
  mutate(pred = rep(predict(loess_kut, newdata = data.frame(age = seq(0, 80, 5), census = 2011)), 5))

n01 %>% filter(name == 'kut', age<=10, census==2006) %>% pull(n) %>% sum()

n01 %>% 
  filter(age<=45) %>%
  mutate(young = ifelse(age<=10, "yes", "no")) %>% 
  group_by(name, census, young) %>% 
  summarise(sum_n = sum(n)) %>% 
  pivot_wider(names_from = young, values_from = sum_n) %>% 
  mutate(prop_young = yes / no) -> a

ggplot(a)+
  geom_line(aes(census, prop_young))+
  facet_wrap(~name)

#same exercise, this time excluding the bad years (99% CI)######################
#age groups of 15 (or 20) years
n01_agegroups_reduced <- n01_agegroups %>% left_join(test_df_viz %>% select(name, census, valid)) %>%
  filter(valid == "Within 95% CI") %>% 
  mutate(age = case_when(
    age <=10 ~ 0,
    age >=15 & age<=25 ~ 15,
    age >=30 & age<=40 ~ 30,
    age>=45 ~ 45
  )) %>% 
  group_by(name, age, census) %>%
  summarise(n = sum(n))

#Center speaker numbers around their sum over all censuses, for each language
age_profile_reduced <- n01_agegroups_reduced %>% 
  left_join(n01_agegroups %>% group_by(name, age) %>% summarise(mean_n = mean(n))) %>% 
  mutate(ratio = ifelse(mean_n==0, 1, n / mean_n),
         logratio = log(ratio+1))

#visualize
ggplot(age_profile_reduced)+
  geom_line(aes(age, logratio, group=as.factor(census), color=as.factor(census)))+
  facet_wrap(~name)

#regression model with interaction age*census
intmodel_reduced <- age_profile_reduced %>% 
  group_by(name) %>% 
  do(model = summary(lm(logratio ~ age + census + age*census, .)))

#extract p values for interaction term
p_int_reduced <- sapply(1:length(fiveobs), function(x) intmodel_reduced$model[[x]]$coefficients[4,4])

#put in data frame
viz_int_agecensus_reduced <- data.frame(name=fiveobs, p_int = round(p_int_reduced, 3)) %>% 
  mutate(sig_level = case_when(p_int  >=.1 ~ "0.1 or above",
                               p_int  <.1 & p_int>=0.05 ~ "Greater or equal to 0.05, below 0.1",
                               p_int  < 0.05 & p_int>=0.01 ~ "Greater or equal to 0.01, below 0.05",
                               p_int  <0.01 ~ "Below 0.01"),
         specification = "Reduced set")

#specify levels for name
viz_int_agecensus_reduced$name <- factor(viz_int_agecensus_reduced$name, levels=rev(fiveobs))

#specify levels for sig level
viz_int_agecensus_reduced$sig_level <- factor(viz_int_agecensus_reduced$sig_level, 
                                              levels= rev(c("Below 0.01", "Greater or equal to 0.01, below 0.05", "Greater or equal to 0.05, below 0.1", "0.1 or above")))

#put with the full set
viz_int_agecensus_full <- bind_rows(viz_int_agecensus, viz_int_agecensus_reduced)

#visualization
ggplot(viz_int_agecensus_full)+
  geom_tile(aes(specification, name, fill=sig_level, color=sig_level))+
  scale_fill_brewer(palette = 'YlOrRd')+
  scale_color_brewer(palette = 'YlOrRd')+
  ylab("")+
  xlab("")

#save
ggsave("C:/Users/micbo262/Documents/RPlots/WGProject1_fig2.tiff", width = 7, height = 9)

################################################################################
#5. TEST FOR LANGUAGE ATTRITION
################################################################################
#Data frame, regression with speaker number as dependent, year as independent, each language and age group
attrition_test <- n01 %>% left_join(test_df_viz %>% select(name, census, valid)) %>%
  filter(valid == "Within 95% CI") %>% 
  mutate(census = census-2001) %>% group_by(name, age) %>% do(model = lm(n ~ census, data =.))

#add direction of slope
attrition_test$slope <- unlist(lapply(1:length(attrition_test$name), function(x) summary(attrition_test$model[[x]])$coefficient[2, 1]))

#add p value
attrition_test$pvalue <- unlist(lapply(1:length(attrition_test$name), function(x) summary(attrition_test$model[[x]])$coefficient[2, 4]))

#discrete variable
attrition_test <- attrition_test %>% 
  mutate(category = case_when(
    pvalue >= 0.1 ~ "P-value 0.1 or above",
    pvalue < 0.1 & pvalue>=0.05  & slope>=0 ~ "P-value between 0.05 and 0.1, positive slope",
    pvalue < 0.05 & slope>=0 ~ "P-value below 0.05, positive slope",
    pvalue < 0.1 & pvalue>=0.05  & slope<0 ~ "P-value between 0.05 and 0.1, negative slope",
    pvalue < 0.05 & slope<0 ~ "P-value below 0.05, negative slope",
  ))

table(attrition_test$category)

#specify levels for the test category
attrition_test$category <- factor(attrition_test$category, levels = c("P-value between 0.05 and 0.1, positive slope",
                                                                      "P-value below 0.05, positive slope",
                                                                      "P-value 0.1 or above",
                                                                      "P-value between 0.05 and 0.1, negative slope",
                                                                      "P-value below 0.05, negative slope"))

#visualization
ggplot(attrition_test)+
  geom_tile(aes(age, name, fill = category))+
  scale_fill_brewer(palette = 'Spectral')+
  scale_color_brewer(palette = 'Spectral')
  
#save
ggsave("C:/Users/micbo262/Documents/RPlots/WGProject1_fig3.tiff", width = 9, height = 9)

################################################################################
#6. ESTIMATE INTERGENERATIONAL TRANSMISSION#####################################
################################################################################

####################################################################
#NOTE: Check if it wouldn't be better with a random intercept model#
#      for the estimation of the census and age-specific errors    #
#      (language name as the fixed group       
#MORE IMPORTANT NOTE: I think that age-specific errors fluctuate in fact much more 
#as a result of the fluctuation in the age-specific number of speakers, rather than 
# representing any underlying, 'real' error (small numbers, e.g. at lower ages, results in large errors)
####################################################################

#make data set with the within 95% pop size only
n01_reduced <- n01 %>% left_join(test_df_viz %>% select(name, census, valid)) %>%
  filter(valid == "Within 95% CI")

#save the number of age categories we work with
agecat <- n01_reduced %>% pull(age) %>% unique %>% length()

#language and age specific means
#absolute standardized error
reg_df <- n01_reduced %>% 
  left_join(n01_reduced %>% 
              group_by(name, age) %>%
              summarise(mean_n = mean(n))) %>%
  mutate(abs.er = abs(n/mean_n-1))

#visualize errors
ggplot(reg_df)+
  geom_histogram(aes(abs.er))+
  facet_wrap(~age)

#regression model with the log of the absolute errors as variable of interest
model <- lm(log(abs.er) ~ age + name, reg_df)
summary(model)

#how much do speaker numbers deviate from their mean
#supposing that deviations follow a log-linear trend across years of age

#df for predictions
pred_df <- data.frame(name = rep(fiveobs, each=agecat), age = rep(seq(0, 100, 5)[1:agecat], 27))

#fetch predictions
pred <- predict(model, newdata = pred_df) 

#put in df (deviations are transformed back to absolute values)
pred_df <- pred_df %>% mutate(pred = exp(pred))

#visualize spread 
hist(pred_df$pred)

#add the observed mean speaker numbers across censuses, by language and age
pred_df <- pred_df %>% 
  left_join(n01_reduced %>% group_by(name, age) %>% summarise(n = mean(n))) 

#simulate age and language specific counts based on poisson * log norm dist
pred_list <- bind_rows(lapply(1:1000, function(x) 
  pred_df %>% mutate(observed = rpois(length(pred_df$name), rlnorm(length(pred_df$name), 0, pred)*n), run = x)))

#evaluate the accuracy of the predictions
evaluation <- n01_reduced %>% select(name, age, census, n) %>%
  left_join(
    pred_list %>% 
      group_by(name, age) %>% 
      summarise(q50 = round(quantile(observed, .5)), q05 = round(quantile(observed, .05)), q95 = round(quantile(observed, .95)))) %>%
  mutate(within = ifelse(n>=q05 & n<=q95, 1, 0))

#show results
evaluation %>% group_by(age, census) %>% summarise(within = mean(within)) %>%
  pivot_wider(names_from = census, values_from = within)

evaluation %>% group_by(census) %>% summarise(within = mean(within)) %>%
  pivot_wider(names_from = census, values_from = within)

evaluation %>% group_by(age) %>% summarise(within = mean(within)) 

evaluation %>% group_by(name) %>% summarise(within = mean(within)) %>% arrange(within) %>% print(n=27)

mean(evaluation$within)

#estimate past number of children
#vectors of speaker counts
pop <- lapply(fiveobs, function(x) pred_df %>% filter(name==x) %>% pull(n) %>% round())

#vectors of predicted errors
pred <- lapply(fiveobs, function(x) pred_df %>% filter(name==x) %>% pull(pred))

#function for estimating the total number of children born
fct_totchild <- function(index, r){
  
  #define population
  p <- rpois(length(pop[[index]]), rlnorm(length(pop[[index]]), 0, pred[[index]])*pop[[index]])
  
  #save the actual number of children speakers
  n0obs <- p[1]
  
  for (i in 1:5){
    
    #subtract the dead
    p <- c(p + rbinom(length(p), p, m[ , 10]))[-1]
    
    #find the number of newborns 
    n0tot <- sum(rbinom(length(p), p, f[ , 10]))
    
    #subtract newborns who die between 0 and 2.5 years
    n0tot <- n0tot - rbinom(1, n0tot, m0[10]) 
    
  }
  
  #put info in df
  data.frame(name = fiveobs[index], run = r, observed = n0obs, total = n0tot) %>% mutate(prop = round(observed / total * 100, 1))
  
}

#run function to produce estimates
estimateN0_w_uncer <- bind_rows(lapply(1:length(fiveobs), function(x) 
  lapply(1:1000, function(y) fct_totchild(x, y))))

#visualize results
ggplot(estimateN0_w_uncer %>% pivot_longer(c(observed, total), names_to = 'estimate', values_to = 'value'))+
  geom_density(aes(value, group = estimate, fill = estimate), alpha = .3)+
  facet_wrap(~name, scales = 'free')+
  expand_limits(x=0)

#save
ggsave("C:/Users/micbo262/Documents/RPlots/WGProject1_fig4.tiff", width = 12, height = 7)

#check the proportions
N0_w_uncer_prop <- estimateN0_w_uncer %>% 
  group_by(name) %>% 
  summarise(q50 = quantile(prop, .5), q05 = quantile(prop, .05), q95 = quantile(prop, .95)) %>%
  print(n = 27)

#plot variation across languages
ggplot(N0_w_uncer_prop %>% arrange(-q50) %>% mutate(number = row_number()))+
  geom_point(aes(number, q50))+
  scale_y_continuous(limits=c(0, 100))+
  geom_smooth(aes(number, q50))

################################################################################
################################################################################
################################################################################















################################################################################
#A1. TEST WHETHER THERE IS ATTRITION
################################################################################
#visualize relationship
ggplot(lmndf %>% filter(name!='moh' | census!=2016))+
  geom_point(aes(census, coef))+
  geom_smooth(aes(census, coef))

#linear model
summary(lm(coef ~ as.factor(census), lmndf %>% filter(name!='moh' | census!=2016)))

#better way of doing this would be in a random intercept model with name as id
#outcome is then the raw data, not the deviations from the year 2001

#add variable for population size (log)
lmndf <- lmndf %>% left_join(n96_tot %>% group_by(name) %>% summarise(tot=sum(n)) %>% mutate(tot = log(tot)))

#linear model, controlling for pop size
summary(lm(coef ~ as.factor(census) + tot + as.factor(census)*tot, lmndf %>% filter(name!='moh' | census!=2016)))

#The effect of census year on the deviation in speaker number in years 2006-2021 vs. 2001 
#does not depend on a language's total population size

#check whether the effect persists when we exclude Dene, Innu, atj, ike
#these languages lost speakers in 2021 but in reality they most probably 
#hardly lose any speakers over time as they are very vigorous
summary(lm(coef ~ as.factor(census), 
           lmndf %>% filter((name!='moh' | census!=2016),
                             name!="Dene (continuum)",
                             name!="Innu-Naskapi (continuum)",
                             name!="Dakotan (continuum)",
                             name!='atj',
                             name!='ike')))

################################################################################
#A2 TEST WHETHER THE AGE PROFILES SPECIFIC TO DIFFERENT YEARS ARE PARALLEL TO EACH OTHER
################################################################################
#Center speaker numbers around their sum over all censuses, for each language
#set ratio to 1 in cases where tot = 0 (one case: hax-hdn age 20)
age_pro <- n01 %>% 
  left_join(n01 %>% group_by(name, age) %>% summarise(tot = sum(n)/5)) %>% 
  mutate(ratio = ifelse(tot==0, 1, n / tot),
         logratio = log(ratio+1))

#visualize
ggplot(age_pro)+
  geom_line(aes(age, logratio, group=as.factor(census), color=as.factor(census)))+
  facet_wrap(~name)

#regression model with interaction age*census
intmodel <- age_pro %>% 
  group_by(name) %>% 
  do(model = summary(lm(logratio ~ age + census + age*census, .)))

#extract p values for interaction term
p_int <- sapply(1:length(fiveobs), function(x) intmodel$model[[x]]$coefficients[4,4])

#put in data frame
data.frame(name=fiveobs, p_int = round(p_int, 3)) %>% 
  mutate(signif95 = ifelse(p_int<.05,1,0),
         signif99 = ifelse(p_int<.01,1,0))

#same exercise, this time excluding the bad years (95% CI)
n01_reduced <- n01 %>% 
  left_join(test_df_viz %>% select(name, census, valid)) %>%
  filter(valid!='-2', valid!="2") %>%
  arrange(name, census, age)

#prepared data frame
age_pro_reduced <- n01_reduced %>% 
  left_join(n01_reduced %>% group_by(name, age) %>% summarise(tot = mean(n))) %>% 
  mutate(ratio = ifelse(tot==0, 1, n / tot),
         logratio = log(ratio+1))

#visualize
ggplot(age_pro_reduced)+
  geom_line(aes(age, logratio, group=as.factor(census), color=as.factor(census)))+
  facet_wrap(~name)

#regression model with interaction age*census
intmodel <- age_pro_reduced %>% 
  group_by(name) %>% 
  do(model = summary(lm(logratio ~ age + census + age*census, .)))

#extract p values for interaction term
p_int <- sapply(1:length(fiveobs), function(x) intmodel$model[[x]]$coefficients[4,4])

#put in data frame
data.frame(name=fiveobs, p_int = round(p_int, 3)) %>% 
  mutate(signif95 = ifelse(p_int<.05,1,0),
         signif99 = ifelse(p_int<.01,1,0))

#age groups of 15 (or 20) years
n01_red_age <- n01_reduced %>%
  mutate(age = case_when(
    age <=10 ~ 0,
    age >=15 & age<=25 ~ 15,
    age >=30 & age<=40 ~ 30,
    age>=45 ~ 45
  )) %>% 
  group_by(name, age, census) %>%
  summarise(n = sum(n))

#prepare data frame
age_pro3 <- n01_red_age %>% 
  left_join(n01_red_age %>% group_by(name, age) %>% summarise(tot = mean(n))) %>% 
  mutate(ratio = ifelse(tot==0, 1, n / tot),
         logratio = log(ratio+1))

#visualize
ggplot(age_pro3)+
  geom_line(aes(age, logratio, group=as.factor(census), color=as.factor(census)))+
  facet_wrap(~name)

#regression model with interaction age*census
intmodel <- age_pro3 %>% 
  group_by(name) %>% 
  do(model = summary(lm(logratio ~ age + census + age*census, .)))

#extract p values for interaction term
p_int <- sapply(1:length(fiveobs), function(x) intmodel$model[[x]]$coefficients[4,4])

#put in data frame
data.frame(name=fiveobs, p_int = round(p_int, 3)) %>% 
  mutate(signif95 = ifelse(p_int<.05,1,0),
         signif99 = ifelse(p_int<.01,1,0))

#proportion of children learning the language###################################
#proportion based on single censuses
#vector of census years
years <- n01_reduced %>% select(name, census) %>% unique() %>% pull(census)

#vector of names
names <- n01_reduced %>% select(name, census) %>% unique() %>% pull(name)

#list of population counts
pop <- lapply(1:length(years), function(x) n01_reduced %>% filter(census==years[x], name==names[x]) %>% pull(n) %>% round())
                
#function for estimating the total number of children born
fct_totchild <- function(index, r){
  
  #define population
  p <- pop[[index]]
  
  #subtract the dead
  p <- c(p + rbinom(length(p), p, m[ , 10]))[-1]
   
  #find the number of newborns 
  nb <- sum(rbinom(length(p), p, f[ , 10]))
  
  #subtract newborns who die between 0 and 2.5 years
  nb <- nb - rbinom(1, nb, m0[10]) 
  
  #put info in df
  data.frame(name = names[index], census = years[index], run = r, N0 = nb)
  
  }

#run function
singcensus_res <- bind_rows(lapply(1:length(names), function(x) 
  lapply(1:100, function(y) fct_totchild(x, y))))

#summarise info
#add observed number of speakers 0-4
singcensus_summary <- singcensus_res %>% 
  group_by(name, census) %>% 
  summarise(q2 = quantile(N0, .5), lower = quantile(N0, .025), upper = quantile(N0, .975)) %>%
  left_join(n01_reduced %>% filter(age==0) %>% select(name, census, n)) %>%
  mutate(disrupted = ifelse(n<lower, 1, 0),
         proportion = round(n/q2*100, 1),
         above = ifelse(n>upper, 1, 0))

#visualization
ggplot(singcensus_summary)+
  geom_segment(aes(y = census, yend = census, x = lower, xend = upper))+
  geom_point(aes(y = census, x = n))+
  facet_wrap(~name, scales = 'free_x')+
  scale_y_continuous(breaks=seq(2001, 2021, 5))+
  expand_limits(x = 0)+
  ggtitle("Points are the observed number of speakers,\nlines the stochastic estimates of the total number of children")

#visualization: 1 line per language concentrating on proportion
singcens_prop <- singcensus_summary %>% 
  select(name, census, proportion) %>% 
  summarise(min = min(proportion), max = max(proportion)) %>%
  mutate(range = case_when(
    max - min >= 100 ~ ">=100",
    max - min <100 & max - min>=50 ~ "<100 & >=50",
    max - min <50 & max - min>=25 ~ "<50 & >=25",
    max - min <25 & max - min >=10 ~ "<25 & >=10",
    max - min <10 ~ "<10"))
    
ggplot(singcens_prop)+
  geom_segment(aes(y = name, yend = name, x = min, xend = max, color = range, group = range))+
  geom_segment(aes(y = -Inf, yend=Inf, x=0, xend = 0 ))+
  geom_segment(aes(y = -Inf, yend=Inf, x=100, xend = 100 ))+
  theme_minimal()+
  ylab("")+
  xlab("Range")

#may be add something to show whether include 0 (and eventually 1)

################################################################################
#compare proportions with those obtained from the point estimates
singcens_prop2 <- singcens_prop %>% select(-range) %>% mutate(model = 'without_uncer') %>%
  bind_rows(N0_w_uncer_prop %>% rename(min = q05, max = q95) %>% select(-q50) %>% mutate(model = 'with_uncer'))

#viz
ggplot(singcens_prop2)+
  geom_linerange(aes(x = name, ymin = min, ymax = max, color = model, group = model),position = position_dodge(.5))+
  coord_flip()+
  theme_minimal()+
  xlab("")+
  ylab("Range")

singcens_prop2 %>% 
  filter(name=='tsi')

#how to estimate the current population size?###################################
#df, regression model
rm_df <- n01_reduced %>% 
  filter(name == 'crx') %>% 
  select(-name, -valid)

summary(lm(n ~ factor(name) + factor(age) + census, n01_reduced))
