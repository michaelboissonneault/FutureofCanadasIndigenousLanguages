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

#Backcast data year 2001
backcast01_df <-  readRDS("backcast") %>% filter(year==2001)

#vector of names 
names <- backcast01_df %>% pull(name) %>% unique()

#backcast data intro matrices
backcast01_m <- lapply(names, function(x) backcast01_df %>% filter(name==x, age<=65) %>% pull(n) %>% matrix(ncol=5))

#find total for each year and language, put into matrix
sum01_m <- sapply(1:length(names), function(x) backcast01_m[[x]] %>% apply(2, sum))

#total for each year and language, standardized
backcast01_st <- lapply(1:length(names), function(x) sapply(1:5, function(y) backcast01_m[[x]][ , y] / sum01_m[y , x])) 

#names ordered by tot pop size
names_ordered <- names[order(apply(sum01_m, 2, sum))]

################################################################################
#2.TEST FOR THE EQUALITY OF THE TOTAL NUMBER OF SPEAKERS BETWEEN CENSUSES#######
################################################################################
#standardize counts around each language's mean, put in vector
stsum01 <- c(sapply(1:length(names), function(x) sum01_m[,x] / apply(sum01_m, 2, mean)[x]) - 1)

#Shapiro test
shapiro.test(stsum01)

#normal distribution with mean and sd taken from the standardized counts
#extract the pvalue for each observation
pvalues <- sapply(1:length(stsum01), function(x) pnorm(stsum01[x], mean(stsum01), sd(stsum01)))

#discretize pvalues
pvalues_dis <- ifelse(pvalues<=0.01, "<=0.01",
                      ifelse(pvalues>0.01 & pvalues<=0.1, ">0.01; <=0.1",
                             ifelse(pvalues>0.1 & pvalues<=0.2, ">0.1; <=0.2",
                                    ifelse(pvalues>=0.8 & pvalues<0.9, ">=0.8; <0.9",
                                           ifelse(pvalues>=0.9 & pvalues<.99, ">=0.9; <0.99",
                                                  ifelse(pvalues>=0.99, ">=0.99", ">0.2; <0.8"))))))

#specify levels for the discrete values
pvalues_dis <- factor(pvalues_dis, levels = c("<=0.01", ">0.01; <=0.1", ">0.1; <=0.2", ">0.2; <0.8", ">=0.8; <0.9", ">=0.9; <0.99", ">=0.99"))

#put in data frame
pvalue_df <- data.frame(name = factor(rep(names, each = 5), levels = names_ordered), 
                        census = factor(seq(2001, 2021, 5)),
                        n = c(sum01_m),
                        pvalue = pvalues,
                        pvalue_dis = pvalues_dis)

#visualization of discrete pvalues
ggplot(pvalue_df)+
  geom_tile(aes(census, name, fill = pvalue_dis, color = pvalue_dis))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral', name = "P-value")+
  scale_color_brewer(palette = 'Spectral', name = "P-value")+
  xlab("Census")

ggsave("Figures/pop_size_heatmap.tiff", width = 4.5, height = 8, dpi = 1000)

#Shapiro test
shapiro.test(sort(stsum01)[c(-1, -2, -134, -135)])

hist(stsum01)
hist(sort(stsum01)[c(-1, -2, -134, -135)])

################################################################################
#3. TEST FOR WHETHER COUNTS BY AGE ARE POISSON
################################################################################
#poisson probabilities: each age x census combination vs. mean, for each language
pv_vs_mean <- lapply(1:length(names), function(x)
  sapply(1:5, function(y) 
    ppois(round(backcast01_m[[x]][, y]),
          round(ifelse(apply(backcast01_st[[x]], 1, mean)==0, NA, apply(backcast01_st[[x]], 1, mean)) * sum01_m[y, x]))))

#distribution should be flat
hist(unlist(pv_vs_mean))

#too many values at the extremeties
#let's see if we can fix that

#get all the 2X2 census combinations
combi <- combn(1:5, 2)

#poisson probabilities, each census combination, for each language 
#(zero values appearing in the lambda position are changed to one)
pv_1x1 <- lapply(1:length(names), function(x) 
  sapply(1:10, function(y) 
    ppois(round(backcast01_m[[x]][ , combi[1, y]]), 
          round(ifelse(backcast01_st[[x]][, combi[2, y]] * sum01_m[combi[1, y], x]==0, 1, 
                       backcast01_st[[x]][, combi[2, y]] * sum01_m[combi[1, y], x])))))

#which combi cols. have a given census ?
census_list <- lapply(1:5, function(x) which(sapply(1:10, function(y) x %in% combi[ , y])==TRUE))

#mean of pvalues pertaining to each census (x is language, y is census)
pv_cens <- lapply(1:length(names), function(x) 
  sapply(1:5, function(y) 
    0.5-abs(pv_1x1[[x]][ , c(census_list[[y]])]-.5) %>% apply(., 1, mean)))

#mean of pvalues by language
pv_lang <- sapply(1:length(names), function(x)
  apply(pv_cens[[x]][1:14, ], 2, mean))

#combinations of language and census to be excluded 
exclude <- sapply(1:length(names), function(x) which(pv_lang[ , x]<0.05))

#exclude from backcast list those combinations to be excluded
for (x in 1:length(names)){
  
  backcast01_m[[x]][, exclude[[x]]] <- NA
  
  }

#total for each year and language, standardized, excluding problematic combinations
backcast01_st <- lapply(1:length(names), function(x) sapply(1:5, function(y) backcast01_m[[x]][ , y] / sum01_m[y , x])) 

#poisson probabilities using means again, this time excluding combinations 
#of language and census with pvalue <0.05
pv_vs_mean2 <- lapply(1:length(names), function(x)
  sapply(1:5, function(y) 
    ppois(round(backcast01_m[[x]][, y]),
          round(ifelse(apply(backcast01_st[[x]], 1, mean, na.rm = T)==0, NA, apply(backcast01_st[[x]], 1, mean, na.rm = T)) * sum01_m[y, x]))))

#check histogram
hist(unlist(pv_vs_mean2))

#not much, but a bit better
#at least we exclude those combinatinos languages x census that 
#had systematic discrepancies, across most years of age

################################################################################
#4 SAVE
################################################################################
bind_rows(pvalue_df %>%
  filter(pvalue<=0.01 | pvalue>=0.99) %>%
  select(name, census) %>% 
  mutate(reason = "pop"),
  bind_rows(lapply(1:length(names), function(x) 
    data.frame(name = names[x],
               census1 = factor(seq(2001, 2021, 5)[exclude[[x]][1]]),
               census2 = factor(seq(2001, 2021, 5)[exclude[[x]][2]])))) %>% 
    pivot_longer(c(census1, census2), names_to= "index",values_to = 'census') %>%
    select(-index) %>%
    filter(!is.na(census)) %>%
    mutate(reason = "age")) %>%
  print() %>%
  mutate(name_census = paste(name, census)) -> excluded
  
#save
saveRDS(excluded, "excluded")

################################################################################


