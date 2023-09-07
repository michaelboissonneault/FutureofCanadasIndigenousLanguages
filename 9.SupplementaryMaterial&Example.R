################################################################################
#IN THIS DO-FILE 
#SUPPLEMENTARY FIGURES

################################################################################
##1. PACKAGES & DATA############################################################
################################################################################
rm(list=ls())

#get packages
library(tidyverse)
library(ggpubr)

#set theme
theme_set(theme_bw())

#read in mortality and fertility matrices
m <- readRDS('m1')
m_0 <- readRDS('m1_0')
f <- readRDS('f')

#raw counts, censuses 2001-2021
sc <- readRDS("scdata") %>% 
  mutate(name_census = paste(name, year)) %>%
  filter(!name_census %in% c(readRDS('excluded') %>% mutate(name_census = paste(name, census)) %>% pull(name_census))) %>%
  select(-name_census)

#counts adjusted for mortality (including backcast until 76)
n76 <- readRDS("backcast") %>% 
  mutate(name_census = paste(name, census)) %>%
  filter(!name_census %in% c(readRDS('excluded') %>% mutate(name_census = paste(name, census)) %>% pull(name_census))) %>%
  select(-name_census)

#vector of names (ordered by pop size in 2001)
names <- n76 %>%
  filter(year==2001) %>%
  group_by(name, census) %>%
  summarise(sum_n = sum(n)) %>%
  group_by(name) %>%
  summarise(pop = mean(sum_n)) %>%
  arrange(-pop) %>%
  pull(name)

#name levels in pop dfs is vector of names
sc <- sc %>% mutate(name = factor(name, levels = names))
n76 <- n76 %>% mutate(name = factor(name, levels = names))

#load it trajectory data and assign to matrix
it_m <- lapply(names, function(x) 
  matrix(readRDS(paste('IntergenerationalTransmission/', x, sep = '')) %>% pull(it), nrow = 25))

#average counts by age, across all years, for each language
n76_mean <- sapply(names, function(l) 
  n76 %>% 
    filter(year==1976, name == l, age<=45) %>%
    group_by(age) %>% 
    summarise(mean_n = mean(pred)) %>% 
    pull(mean_n))

################################################################################
#2. THE DATA FRAMES
################################################################################
#data frames
sc_prop <- sc %>% 
  left_join(sc %>% group_by(name, year) %>% summarise(tot = sum(pop))) %>%
  mutate(prop = pop / tot)

n76_01_mean <- n76 %>% 
  filter(year>=1981, year<=2001) %>%
  group_by(name, year, age) %>% 
  summarise(mean_n = mean(pred))

n76_01_prop <- n76_01_mean %>%
  left_join(n76_01_mean %>% group_by(name, year) %>% summarise(tot = sum(mean_n))) %>%
  mutate(prop = mean_n / tot)

################################################################################
#2. THE FIGURES
################################################################################
#raw counts 2001-2021 (split into three)########################################
for (x in c(1, 10, 19)){ 
  
  ggplot(sc_prop %>% filter(name %in% names[x:(x+8)]))+
    geom_col(aes(age, prop))+
    facet_grid(name ~ year)+
    coord_flip() 
  
  ggsave(paste("Figures/SM1_", x, 'to', x+8, '.tiff', sep = ''), width = 7, height = 9)

  #pyramids, backcast results (raw counts corrected for mortality), years 1976 and 2001
  ggplot(n76_01_prop %>% filter(name %in% names[x:(x+8)]))+
    geom_col(aes(age, prop))+
    facet_grid(name ~ year)+
    coord_flip() 

  ggsave(paste("Figures/SM2_", x, 'to', x+8, '.tiff', sep = ''), width = 7, height = 9)
  
  }

#Intergenerational transmission#################################################
#Actual and counterfactual number of children
actual_counter <- bind_rows(lapply(names, function(x) 
  readRDS(paste('IntergenerationalTransmission/', x, '_counts0_4', sep= ''))))

#summarise
act_count_sum <- actual_counter %>%
  pivot_longer(c(actual, counter), names_to = "scenario", values_to = "value") %>%
  group_by(name, year, scenario) %>%
  summarise(mean_value = mean(value)) %>%
  pivot_wider(values_from = mean_value, names_from = scenario) %>%
  mutate(prop = actual / counter) %>%
  pivot_longer(c(actual, counter), names_to = "scenario", values_to = "value")

#plot
ggplot(act_count_sum)+
  geom_point(aes(year, value, color = scenario, group = scenario))+
  facet_wrap(~name, scales = 'free_y', ncol = 4)+
  expand_limits(min = 0)

#fetch it predictions
it_df <- bind_rows(lapply(names, function(x) readRDS(paste("IntergenerationalTransmission/", x, sep = ''))))

#find the 80% CI
it_df <- it_df %>% 
  group_by(year, name) %>% 
  summarise(q2 = quantile(it, .5), lower = quantile(it, .1), upper = quantile(it, .9))

#arrange names by degree of increase in it
it_df <- it_df %>%
  mutate(name = factor(name, levels = it_df %>% 
                         filter(year==2001 | year==2101) %>%
                         pivot_wider(names_from = year, values_from = c(q2, lower, upper)) %>%
                         mutate(ratio = q2_2101 / q2_2001) %>% 
                         arrange(-ratio) %>% pull(name)))

#add estimated proportions 1981-2001
it_df <- left_join(it_df, act_count_sum %>% select(name, year, prop) %>% unique())

#show point estimates for the proportions and projected values with 80% CI
ggplot(it_df)+
  geom_line(aes(year, q2))+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha = .2)+
  geom_point(aes(year, prop))+
  facet_wrap(~name, ncol = 4)+
  scale_x_continuous(breaks = seq(1981, 2101, 20))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
ggsave("Figures/SM3.tiff", width = 8.3, height = 11.7)

#Projection results#############################################################
#speaker numbers by age and language, years 2001, 2026, 2051, 2076, 2101
projection_df <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year %in% seq(2001, 2101, 25)) %>% 
    group_by(year, name, age) %>% 
    summarise(q2 = quantile(n, .5)))) %>%
  mutate(year = factor(year))

#calculate proportions
projection_df_prop <- left_join(
  projection_df,
  projection_df %>% group_by(name) %>% summarise(sum_q2 = sum(q2))) %>%
  mutate(prop = q2 / sum_q2)

#pyramids
for (x in c(1, 10, 19)){ 
  
  ggplot(projection_df_prop %>% filter(name %in% names[x:(x+8)]))+
    geom_col(aes(age, prop))+
    facet_grid(name ~ year)+
    coord_flip() 
  
  ggsave(paste("Figures/SM4_", x, 'to', x+8, '.tiff', sep = ''), width = 8.3, height = 11.7)
  
  }
  
#trends over time, total number of speakers
trend_df <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    group_by(year, name, run) %>%
    summarise(sum_n = sum(n)) %>%
    group_by(year, name) %>%
    summarise(q2 = quantile(sum_n, .5), lower = quantile(sum_n, .1), upper = quantile(sum_n, .9))))

#arrange names in order of size
trend_df <- trend_df %>%
  mutate(name = factor(name, levels = 
                         trend_df %>% filter(year==2001) %>% arrange(-q2) %>% pull(name)))

#figure with trends 2001-2101
ggplot(trend_df)+
  geom_line(aes(year, q2))+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha = .2)+
  facet_wrap(~name, ncol = 4, scale = 'free_y')+
  scale_x_continuous(breaks = seq(1981, 2101, 20))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Figures/SM5.tiff", width = 8.3, height = 11.7)

################################################################################
#4 EXAMPLE######################################################################
################################################################################
#language
l <- "Tlicho"

#Raw counts by age 2001-2021
rawcounts01_21 <- readRDS("scdata") %>% filter(name==l)

#pyramids
a <- ggplot(rawcounts01_21)+
  geom_col(aes(age, pop))+
  facet_wrap( ~ year, ncol = 5)+
  coord_flip()+
  ylab("Number of speakers (raw)")+
  xlab("Age")+
  theme(axis.text=element_text(size = 10))

#standardized counts in 2001
standardizedcounts76 <- n76 %>% filter(name==l, year==2001)

#pyramids
ggplot(standardizedcounts76)+
  geom_col(aes(age, n))+
  facet_wrap( ~ census, ncol = 5)+
  coord_flip()

#mean of standardized counts in 2001
meanstandardizedcounts76 <- standardizedcounts76 %>% group_by(age) %>% summarise(mean_n = mean(n))

#pyramid
b <- ggplot(meanstandardizedcounts76 %>% mutate(group = ifelse(age<25, "C", "A"), year = 2001))+
  geom_col(aes(age, mean_n, fill = group, group = group))+
  coord_flip()+
  ylab("Number of speakers\n(mean standardized counts, 2001-2021)")+
  annotate('text', x = 0, y = 120, label = '2001', size = 2.8)+
  annotate('text', x = 5, y = 125, label = '1996', size = 2.8)+ 
  annotate('text', x = 10, y = 145, label = '1991', size = 2.8)+
  annotate('text', x = 15, y = 150, label = '1986', size = 2.8)+
  annotate('text', x = 20, y = 195, label = '1981', size = 2.8)+
  annotate('text', x = 10, y = 185, label = 'Cohort', size = 3.2)+
  theme(legend.position = "none",
        axis.text=element_text(size = 10))+
  scale_fill_brewer(palette = "Set2")+
  facet_wrap(~ year)+
  xlab("Age")

#actual and coutnerfactual births 1981-2001
c1 <- ggplot(act_count_sum %>% 
         filter(name == l) %>% 
         pivot_wider(names_from = scenario, values_from = value) %>%
         mutate(counter = counter - actual) %>% 
         pivot_longer(c(counter, actual), names_to = 'scenario', values_to = 'value') %>%
         mutate(scenario = factor(scenario, levels = c('counter', 'actual'))))+
  geom_col(aes(year, value, group = scenario, fill = scenario))+
  expand_limits(min = 0)+
  scale_x_continuous(breaks = seq(1981, 2001, 5))+
  theme(legend.position = "none",
        axis.text=element_text(size = 10))+
  scale_fill_brewer(palette = "Set2")+
  xlab("Cohort")+
  ylab("Number of children aged 0-4")

#proportions and fit
ggplot(it_df %>% filter(name == l, year<=2001))+
  geom_point(aes(year, prop))+
  geom_line(aes(year, q2))+
  expand_limits(min = 0, max = 1)+
  theme(axis.text=element_text(size = 10))

#it projection
c2 <- ggplot(it_df %>% filter(name == l))+
  geom_point(aes(year, prop))+
  geom_line(aes(year, q2))+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha = .2)+
  expand_limits(min = 0, max = 1)+
  scale_x_continuous(breaks = seq(1981, 2101, 20))+
  ylab("Proportion raised in Tlicho")+
  xlab("Year")+
  theme(axis.text=element_text(size = 10))

#projection results, counts by age at 25 year intervals
d <- ggplot(projection_df %>% filter(name == l, year!=2001))+
  geom_col(aes(age, q2))+
  facet_wrap(~ year, ncol = 5)+
  coord_flip()+
  ylab("Number of speakers (projected)")+
  xlab("")+
  theme(axis.text.y=element_blank(),
        axis.text=element_text(size = 10))

e <- ggplot()+theme_minimal()

#put plots together
ggarrange(a, 
          ggarrange(b, d, widths = c(2, 7), labels = c("B", "D")),
          ggarrange(c1, c2, e, widths = c(2, 3, 2), nrow = 1), 
          nrow = 3, 
          labels = c("A", "", "C"))

ggsave("Figures/Example.tiff", width = 15, height= 9, dpi = 1000)