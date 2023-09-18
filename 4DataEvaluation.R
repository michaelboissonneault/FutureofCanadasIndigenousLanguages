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
m1 <- readRDS('DemographicParameters')[[1]][, -c(1:5)]
m2 <- readRDS('DemographicParameters')[[3]][, -c(1:5)]
f <- readRDS('DemographicParameters')[[5]][, -c(1:5)]

#Backcast data year 2001
n01 <-  readRDS("backcast") %>% filter(year==2001)

#vector of names 
names <- n01 %>% pull(name) %>% unique()

#backcast data intro matrices
backcast01 <- lapply(names, function(x) n01 %>% filter(name==x) %>% pull(n) %>% matrix(ncol=5))

#find total for each year and language, put in matrix
totbyyr <- sapply(1:length(names), function(x) backcast01[[x]] %>% apply(2, sum))

#names ordered by tot pop size
names_ordered <- names[order(apply(totbyyr, 2, sum))]

################################################################################
#2.TEST FOR THE EQUALITY OF THE TOTAL NUMBER OF SPEAKERS BETWEEN CENSUSES#######
################################################################################
#standardize counts around each language's mean, put in vector
totbyyr_stand <- c(sapply(1:length(names), function(x) totbyyr[,x]/apply(totbyyr, 2, mean)[x])-1)

#normal distribution with mean and sd taken from the standardized counts
#extract the pvalue for each observation
pvalues <- sapply(1:length(totbyyr_stand), function(x) pnorm(totbyyr_stand[x], mean(totbyyr_stand), sd(totbyyr_stand)))

#discretize pvalues
pvalues_dis <- ifelse(pvalues<=0.01, "<=0.01",
                      ifelse(pvalues>0.01 & pvalues<=0.1, ">0.01; <=0.1",
                             ifelse(pvalues>0.1 & pvalues<=0.2, ">0.1; <=0.2",
                                    ifelse(pvalues>=0.8 & pvalues<0.9, ">=0.8; <0.9",
                                           ifelse(pvalues>=0.9 & pvalues<.99, ">=0.9; <0.99",
                                                  ifelse(pvalues>=0.99, ">=0.99", ">0.2; <0.8"))))))

pvalues_dis <- factor(pvalues_dis, levels = c("<=0.01", ">0.01; <=0.1", ">0.1; <=0.2", ">0.2; <0.8", ">=0.8; <0.9", ">=0.9; <0.99", ">=0.99"))

#put in data frame
pvaluedf <- data.frame(name = factor(rep(names, each = 5), levels = names_ordered), 
                       census = factor(seq(2001, 2021, 5)),
                       n = c(totbyyr),
                       pvalue = pvalues,
                       pvalue_dis = pvalues_dis)

#visualization of discrete pvalues
ggplot(pvaluedf)+
  geom_tile(aes(census, name, fill = pvalue_dis, color = pvalue_dis))+
  ylab("")+
  scale_fill_brewer(palette = 'Spectral', name = "P-value")+
  scale_color_brewer(palette = 'Spectral', name = "P-value")+
  xlab("Census")

ggsave("Figures/pop_size_heatmap.tiff", width = 5, height = 8, dpi = 1000)

################################################################################
#3.INTERACTION TESTS FOR THE PARALLEL DISTRIBUTIONS OF SPEAKER NUMBERS BY AGE###
################################################################################
#get all the 2X2 census combinations
combi <- combn(1:5, 2)

#estimate pvalues for the interaction census*age, for all 2x2 census combinations, for all languages
interaction_pvalues <- lapply(1:length(names), function(x) 
  sapply(1:10, function(i) 
    c(lm(c(backcast01[[x]][,combi[1,i]][1:11], backcast01[[x]][,combi[2,i]][1:11]) ~ rep(1:11, 2) + rep(c(0, 1), each = 11) + rep(1:11, 2) * rep(c(0, 1), each = 11)) %>%
        summary() %>%
        coefficients())[16]))

#put in data frame
interaction_df <- bind_rows(lapply(1:length(names), function(x) 
  data.frame(name = factor(rep(names[x], 10), levels = names_ordered),
             census1 = factor(combn(seq(2001, 2021, 5), 2)[1,]),
             census2 = factor(combn(seq(2001, 2021, 5), 2)[2,]),
             pvalue = interaction_pvalues[[x]]))) 

#mean of pvalues across years
interaction_sum <- interaction_df %>% 
  pivot_longer(c(census1, census2), names_to = "idontcare", values_to = 'census') %>% 
  group_by(name, census) %>%
  summarise(mean_pvalue = mean(pvalue)) %>%
  mutate(pvalue_dis = factor(abs(ceiling(log10(mean_pvalue)))),
         pvalue_dis = ifelse(mean_pvalue>.2, ">0.2",
                             ifelse(pvalue_dis==1, ">0.01; <=0.1",
                                    ifelse(pvalue_dis==2, '<0.01', ">0.1; <=0.2"))),
         pvalue_dis = factor(pvalue_dis, levels = c(">0.2", ">0.1; <=0.2", ">0.01; <=0.1", '<0.01')))

#heatmap
ggplot(interaction_sum)+
  geom_tile(aes(census, name, fill = pvalue_dis))+
  scale_fill_brewer(palette = 'Reds', name = "P-value")+
  xlab("Census")+
  ylab("")

################################################################################
#4. SUMMARIZING AND SAVING RESULTS
################################################################################
#identify the extreme combinations of year * language
bind_rows(
  pvaluedf %>%
    filter(pvalue<=0.01 | pvalue>=0.99) %>%
    select(name, census) %>% 
    mutate(reason = "pop"),
  interaction_sum %>% 
    filter(mean_pvalue<=.01) %>%
    select(name, census) %>% 
    mutate(reason = "age")) %>%
  arrange(name, census) %>%
  print() %>%
  mutate(name_census = paste(name, census)) -> excluded

#save
saveRDS(excluded, "excluded")
