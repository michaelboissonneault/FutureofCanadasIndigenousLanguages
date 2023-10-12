################################################################################
#IN THIS DO FILE: ANALYSIS OF THE PROJECTION RESULTS (INCLUDING ALL FIGURES AND TABLES)
#1. GET PACKAGES
#2. FIGURE 1: MAP
#3. FIGURE 2: SPEAKER NUMBERS IN 2001 AND 2101
#4. FIGURE 3: AGE OF YOUNGEST SPEAKER 2001 - 2101
#5. TABLE 1: DORMANCY RISKS 

################################################################################
#1. GET PACKAGES ###############################################################
################################################################################
rm(list=ls())

#packages
library(openxlsx)
library(rnaturalearth)
library(sf)
library(ggrepel)
library(raster)
library(tidyverse)
library(ggpubr)

#set theme
theme_set(theme_bw())

#vector of names 
names <- readRDS("scdata") %>% pull(name) %>% unique()

#create folder to store figures
dir.create("Figures")

################################################################################
#2. FIGURE 1: MAP
################################################################################
#Population in 2001 and coordinates
fig1_df <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year==2001) %>%
    group_by(name, year, run) %>%
    summarise(n = sum(n)) %>% 
    group_by(name, year) %>% 
    summarise(q2 = quantile(n, .5)))) %>%
  mutate(year = factor(year)) %>%
  left_join(read.xlsx("coordinates.xlsx")) %>%
  mutate(latitude = ifelse(name == "Cree langs.", 54, latitude),
         longitude = ifelse(name == "Cree langs.", -77, longitude), 
         latitude = ifelse(name == "Gwich'in.", 67, latitude),
         longitude = ifelse(name == "Gwich'in", -139, longitude),
         latitude = ifelse(name == "Mohawk", 45, latitude),
         longitude = ifelse(name == "Mohawk", -74, longitude),
         latitude = ifelse(name == "Ktunaxa", 49, latitude),
         longitude = ifelse(name == "Ktunaxa", -115, longitude))

#map data from natural earth
world <- ne_countries(scale = "small", country=c("Canada", "United States"), returnclass = "sf")

# filter to bbox
bbox <- st_bbox(world)

#set limits
ylimits <- c(min(fig1_df$latitude)-1, max(fig1_df$latitude)+8)
xlimits <- c(min(fig1_df$longitude)-2, max(fig1_df$longitude)+8)

#Not sure what this does cause I copied it from Simon's
world.rst <- ne_load(type="MSR_50M", category='raster', destdir=".", returnclass="sf")
world.rst.df <- raster::as.data.frame(world.rst, xy=TRUE)
world.rst.df <- world.rst.df[dplyr::between(world.rst.df$x, bbox[['xmin']], bbox[['xmax']]), ]
world.rst.df <- world.rst.df[dplyr::between(world.rst.df$y, xlimits[[1]], xlimits[[2]]), ]

#Produce map
ggplot(world)+
  geom_sf(fill = "mintcream") +
  geom_point(data=fig1_df,#[fig1_df$year == "2001",], # filter only for combined plot
             aes(x=longitude, y=latitude, size=q2, color=family),
             alpha=0.5) +
  theme(
    panel.background = element_rect(fill = "slategray1"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.key.height = unit(1, 'cm'),
    legend.title = element_text(size=12), 
    legend.text = element_text(size=12)
  ) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_color_brewer(palette = "Set1") +
  geom_text_repel(data=fig1_df,#[fig1_df$year == "2001",], # filter only for combined plot
                  aes(x=longitude, y=latitude, label=name), max.overlaps = 27) +
  scale_y_continuous(breaks = seq(45, 75, 10), limits = ylimits)+
  xlim(xlimits)+
  scale_size_continuous(breaks=c(100, 1000, 10000),
                        range=c(2,20),
                        name="Population size",
                        limit=c(100, 100000)) +
  theme_bw(base_size=14)

ggsave('Figures/Fig1.tiff', width=9, height=6.5, dpi=1000)

#In text comments
fig1_df %>% 
  ungroup %>%
  summarise(min=round(min(q2)), 
            max=round(max(q2)),
            mean=mean(q2),
            median=quantile(q2, .5))

fig1_df %>% filter(q2<=110)
fig1_df %>% filter(q2>=50000)

################################################################################
#FIGURE 2: EXAMPLES OF PYRAMIDS IN 2001 AND 2101 AND ASSOCIATED IT RATIO
################################################################################
#vector of names for which we present examples
examples <- c("Atikamekw", "Mi'kmaq", "Oji-Cree", "Ojibwa langs.")

#df including projection results by age 2001-2101
fig2_df_age <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter((year==2001 | year==2101), name %in% examples))) %>%
  group_by(name, year, age) %>%
  summarise(q2 = quantile(n, .5, na.rm = T),
            lower = quantile(n, .1, na.rm = T),
            upper = quantile(n, .9, na.rm = T)) %>%
  mutate(name = factor(name, levels = examples))

#df with it whole period
fig2_df_it <- bind_rows(lapply(names, function(x) 
  readRDS(paste('Results/', x, sep = '')) %>% 
    filter(name %in% examples))) %>%
  mutate(prop = actual / counter) %>%
  group_by(year, name) %>%
  summarise(prop = quantile(prop, .5, na.rm = T),
            q2 = quantile(it, .5, na.rm = T),
            lower = quantile(it, .1, na.rm = T),
            upper = quantile(it, .9, na.rm = T)) %>%
  mutate(name = factor(name, levels = examples))

#add to the previous df the children that did not learn the language, 0-20 years old
fig2_df_age <- fig2_df_age %>% 
  left_join(fig2_df_it %>% 
              mutate(age = 1996-year, year = 2001, no_learn = 1-q2) %>%
              select(name, age, year, no_learn)) %>%
  mutate(no_learn = floor(q2 * no_learn)) 
  
#df with populatoin sizes whole period
fig2_df_sum <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(name %in% examples))) %>%
  group_by(run, name, year) %>%
  summarise(sum_n = sum(n, na.rm = T)) %>%
  group_by(name, year) %>%
  summarise(q2 = quantile(sum_n, .5, na.rm = T), 
            lower = quantile(sum_n, .1, na.rm = T), 
            upper = quantile(sum_n, .9, na.rm = T)) %>%
  mutate(name = factor(name, levels = examples))

#figure 2a: population pyramids 2001
fig2a <- ggplot(fig2_df_age %>% filter(year==2001))+
  geom_segment(aes(x = age+2.5, xend = age+2.5, y = 0, yend = q2), linewidth = 1.5, alpha = .8)+
  geom_segment(aes(x = age+2.5, xend = age+2.5, y = q2, yend = q2+no_learn), linewidth = 1.5, alpha = .4)+
  scale_x_continuous(breaks = c(0, 50, 100))+
  scale_y_continuous(breaks = c(0, 1000, 2000))+
  coord_flip()+
  facet_wrap(~name, ncol = 1)+
  xlab("Age")+
  ylab("Speakers")+
  theme(strip.text = element_blank(), strip.background = element_blank())+
  ggtitle("A")+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#figure 2b: it in 1981-2001 and projection until 2101
fig2b <- ggplot(fig2_df_it)+
  geom_line(aes(year, q2))+
  geom_ribbon(aes(year, ymin = lower, ymax = upper), alpha = .2)+
  geom_point(aes(year, prop))+
  facet_wrap(~name, ncol = 1)+
  scale_y_continuous(breaks = c(0, .5, 1))+
  scale_x_continuous(breaks = c(1976, 2021, 2066, 2101))+
  ylab("Proportion")+
  xlab("Year")+
  theme(strip.text = element_blank(), strip.background = element_blank())+
  ggtitle("B")+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#figure 2c: population size 2001-2101
fig2c <- ggplot(fig2_df_sum %>% filter(year>=2001))+
  geom_line(aes(year, q2))+
  geom_ribbon(aes(year, min = lower, max = upper), alpha = .3)+
  facet_wrap(~name, ncol = 1)+
  xlab("Year")+
  ylab("Speakers")+
  scale_y_continuous(breaks = seq(0, 20000, 10000))+
  expand_limits(ymin = 0)+
  scale_x_continuous(breaks = seq(2001, 2101, 50))+
  theme(strip.text = element_blank(), strip.background = element_blank())+
  ggtitle("C")+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#figure 2d: population pyramids 2101
fig2d <- ggplot(fig2_df_age %>% filter(year==2101))+
  geom_segment(aes(x = age+2.5, xend = age+2.5, y = 0, yend = q2), linewidth = 1.5, alpha = .5)+
  geom_segment(aes(x = age+2.5, xend = age+2.5, y = lower, yend = q2), linewidth = .3, alpha = .5)+
  geom_segment(aes(x = age+2.5, xend = age+2.5, y = q2, yend = upper), linewidth = .3, alpha = .8)+
  scale_x_continuous(breaks = c(0, 50, 100))+
  scale_y_continuous(breaks = c(0, 300, 600))+
  coord_flip()+
  facet_wrap(~name, ncol = 1, strip.position = "right")+
  xlab("Age")+
  ylab("Speakers")+
  ggtitle("D")+
  theme(plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#arrange plots together
ggarrange(fig2a, fig2b, fig2c, fig2d, ncol = 4)

ggsave("Figures/NewFig2.tiff", height = 5, width= 8, dpi = 1000)

################################################################################
#FIGURE 3: POP EVOLUTION, PERIOD 2001-2101 (BY SIZE IN 2001)
################################################################################
#df with populatoin sizes whole period
fig3_df <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
  filter(year>=2001) %>%
  group_by(run, name, year) %>%
  summarise(sum_n = sum(n, na.rm = T)) %>%
  group_by(name, year) %>%
  summarise(q2 = quantile(sum_n, .5, na.rm = T), 
            lower = quantile(sum_n, .1, na.rm = T), 
            upper = quantile(sum_n, .9, na.rm = T)))) 

#languages by initial and final size
groups <- fig3_df %>% 
  filter(year==2001 | year==2101) %>% 
  dplyr::select(name, year, q2) %>%
  pivot_wider(names_from = year, values_from = q2) %>%
  arrange(`2001`) %>%
  ungroup() %>%
  mutate(initial = factor(rep(1:9, each = 3))) %>%
  group_by(initial) %>%
  arrange(`2101`, .by_group = T) %>%
  ungroup() %>%
  mutate(final = factor(rep(1:3, 9))) %>%
  dplyr::select(name, initial, final)
  
#add groups to the main df
fig3_df <- fig3_df %>% left_join(groups) %>% ungroup()

#function for the breaks on the plot's y axis
breaks_fct <- function(q2) {
 
  x <- floor(log10(max(q2)))-1
  
  c <- floor(max(q2) / 10^x)*10^x
  
  c(0, c/2, c)
  
}

#name labels: change to na except year where should appear
fig3_df <- fig3_df %>% 
  mutate(name = ifelse(initial!=6 & year==2051, name, 
                       ifelse(initial==6 & year==2021, name, NA)))

#plot
ggplot(fig3_df, aes(year, q2))+
  geom_line(aes(group = final, color = final), alpha = .8, linetype = 'dashed')+
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper, group = final, fill = final), alpha = .2)+
  geom_text_repel(aes(label = name, group = initial), nudge_x = 2)+
  facet_wrap(~ initial, scale = 'free_y')+
  scale_y_continuous(breaks = breaks_fct, limits = c(0, NA))+
  scale_x_continuous(breaks = c(2001, 2051, 2101))+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme(strip.text.x = element_blank())+
  ylab("Number of speakers")+
  xlab("Year")+
  theme(legend.position = "none")

ggsave("Figures/Figure3.tiff", width = 7.5, height = 5)

################################################################################
#TABLE 1 POPULATION SIZES, AGE OF YOUNGEST SPEAKER, AND IT IN 2001 AND 2021
################################################################################
#df with populatoin sizes in 2001 and 2101, for each language
tbl1_df_sum <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year==2001 | year==2101))) %>%
  group_by(run, name, year) %>%
  summarise(sum_n = sum(n, na.rm = T)) %>%
  group_by(name, year) %>%
  summarise(q2 = round(quantile(sum_n, .5, na.rm = T)), 
            lower = round(quantile(sum_n, .1, na.rm = T)), 
            upper = round(quantile(sum_n, .9, na.rm = T))) %>%
  mutate(metric = "speakers")

#df with it values in 2001 and 2101, for each language
tbl1_df_it <- bind_rows(lapply(names, function(x) 
  readRDS(paste('IntergenerationalTransmission/', x, sep = '')) %>% 
    filter(year==2001 | year==2101))) %>%
  group_by(year, name) %>%
  summarise(q2 = round(quantile(it, .5, na.rm = T), 2),
            lower = round(quantile(it, .1, na.rm = T), 2),
            upper = round(quantile(it, .9, na.rm = T), 2)) %>%
  mutate(metric = "it")

#df with age of the youngest speaker in 2001, 2101, each language
tbl1_df_age <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year==2001 | year==2101))) %>%
  group_by(name, year, run) %>%
  filter(n>0) %>%
  summarise(youngest = min(age, na.rm = T)) %>%
  group_by(name, year) %>%
  summarise(q2 = round(quantile(youngest, .5, na.rm = T)),
            lower = round(quantile(youngest, .1, na.rm = T)),
            upper = round(quantile(youngest, .9, na.rm = T))) %>%
  mutate(metric = 'youngest')

#vector of names ordered by population size in 2001
names_ordered <- tbl1_df_sum %>% filter(year==2001) %>% arrange(q2) %>% pull(name)

#put dfs together
tbl1_df <- bind_rows(tbl1_df_sum, tbl1_df_age) %>%
  mutate(value = paste(q2, " (", lower,"-", upper, ")", sep = ""),
         name = factor(name, levels=names_ordered)) %>%
  dplyr::select(name, year, value, metric) %>% 
  pivot_wider(names_from = metric, values_from = value) %>%
  left_join(groups) %>%
  arrange(initial, -as.numeric(final)) %>%
  dplyr::select(-initial, -final) %>%
  pivot_wider(names_from = year, values_from = c(speakers, youngest)) %>%
  dplyr::select(name, speakers_2001, youngest_2001, speakers_2101, youngest_2101)
  
write.xlsx(tbl1_df, "Figures/table1.xlsx")

#Total
tbl1_df_sum %>% group_by(year) %>% summarise(q2 = sum(q2), lower = sum(lower), upper = sum(upper))

#proportions
tbl1_df_sum %>%
  pivot_wider(names_from = year, values_from = c(q2, lower, upper)) %>%
  mutate(prop = q2_2101 / q2_2001) %>% 
  dplyr::select(name, q2_2001, lower_2001, upper_2001, q2_2101, lower_2101, upper_2101, prop) %>%
  arrange(prop) %>%
  print(n = 27)

################################################################################
#TABLE 2 EXTINCTION RISKS
################################################################################
#table 2: extinction risks, all ages, all years
tbl2_allages <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')))) %>%
  group_by(run, name, year) %>%
  summarise(sum_n = sum(n)) %>%
  filter(sum_n==0) %>%
  group_by(name, year) %>%
  summarise(extinct = n()/10) %>% 
  mutate(group = 'all')

#extinction risks: no speakers below age 50
tbl2_below50 <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')))) %>%
  filter(age<50) %>%
  group_by(run, name, year) %>%
  summarise(sum_n = sum(n)) %>%
  filter(sum_n==0) %>%
  group_by(name, year) %>%
  summarise(extinct = n()/10) %>% 
  mutate(group = 'below50')

#extinction risks: no speakers below age 15
tbl2_below15 <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')))) %>%
  filter(age<15) %>%
  group_by(run, name, year) %>%
  summarise(sum_n = sum(n)) %>%
  filter(sum_n==0) %>%
  group_by(name, year) %>%
  summarise(extinct = n()/10) %>% 
  mutate(group = 'below15')

#merge everything
tbl2 <- bind_rows(tbl2_allages, tbl2_below50, tbl2_below15) 

#add year when risk reaches specific threshold
tbl2 <- tbl2 %>% 
  left_join(tbl2 %>%  
              filter(extinct>=10) %>%
              group_by(name, group) %>%  
              summarise(risk10 = min(year))) %>%
  left_join(tbl2 %>%  
              filter(extinct>=50) %>%
              group_by(name, group) %>%  
              summarise(risk50 = min(year))) %>%
  filter(year==2101) %>%
  arrange(group, -extinct) %>%
  mutate(value = paste(extinct, " (", ifelse(is.na(risk10), " ", risk10), "-", ifelse(is.na(risk50), " ", risk50), ")", sep="")) %>%
  dplyr::select(name, group, value) %>%
  pivot_wider(values_from = value, names_from = group) %>%
  dplyr::select(name, all, below50, below15) %>%
  filter(name!="Atikamekw", name!= "Secwepemctsin")

tbl2  

write.xlsx(tbl2, "Figures/table2.xlsx")

################################################################################
#FIG 3 EXTINCTION RISKS
################################################################################



################################################################################
# FIGURE (OR TABLE?) NUMBER OF SPEAKERS IN 2101 VS 2001
################################################################################
#numbers in all years
fig_x <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    group_by(name, year, run) %>%
    summarise(n = sum(n)) %>% 
    group_by(name, year) %>% 
    summarise(q2 = quantile(n, .5), lower = quantile(n, .1), upper = quantile(n, .9)))) %>%
  mutate(year = factor(year))

#numbers proportional to their values in 2001
fig_x2 <- fig_x %>%
  left_join(fig_x %>% 
              filter(year==2001) %>%
              select(name, q2) %>% 
              rename(q2_2001 = q2)) %>% 
  mutate(q2 = q2 / q2_2001,
         lower = lower / q2_2001,
         upper = upper / q2_2001)  %>%
  group_by(year) %>%
  mutate(min = min(q2), max = max(q2))
  

#categories for the number of speakers in the year 2101
fig_x2 <- fig_x2 %>% 
  left_join(fig_x2 %>% 
              filter(year==2101) %>% 
              mutate(
                category = case_when(
                q2 <0.1 & year==2101 ~ "<0.1",
                q2 >=0.1 & q2<.5 & year==2101 ~ ">=0.1; <0.5",
                q2 >=.5 & year==2101 ~ ">=0.5"
              )) %>% 
              select(name, category))
                      
#plot
ggplot(fig_x2)+
  geom_line(aes(year, q2, group = name))+
  facet_wrap(~category)

table(fig_x2 %>% filter(year==2101) %>% pull(category))
fig_x %>% filter(year==2101, q2==0) %>% pull(name) 

filter(fig_x %>% filter(year==2101, q2>2))
################################################################################
#3.FIGURE 2: SPEAKER NUMBERS IN 2001 AND 2101
################################################################################
#adjust language levels according to size
fig2_df <- fig2_df %>%
  mutate(name = factor(name, levels = fig2_df %>% filter(year==2001) %>% arrange(q2) %>% pull(name)))

#Figure 2
ggplot(fig2_df)+
  geom_point(aes(x = q2, y = name, color = year, group = year), position = position_dodge(-.4))+
  geom_errorbar(aes(xmin=lower, xmax=upper, y=name, color = year, group = year), linewidth=.8, position = position_dodge(-.4), width = 0)+
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10),
                     breaks = c(0, 10^1, 10^2, 10^3, 10^4, 10^5),
                     labels=scales::comma,
                     lim=c(0,105000))+
  annotation_logticks(sides="b")+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position = c(.85, .3), legend.title = element_blank())+
  ylab("")+
  xlab("Number of speakers")

ggsave("Figures/Fig2.tiff", height=7, width=6, dpi=1000)
            
################################################################################
#4. FIGURE 3: AGE OF YOUNGEST SPEAKER 2001 - 2101
################################################################################
#youngest speaker age group in F2001 and 2100  
fig3_df <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year==2001 | year==2101, n>0) %>% 
    group_by(year, name, run) %>%
    summarise(min_age = min(age)) %>% 
    group_by(year, name) %>% 
    summarise(q2 = quantile(min_age, .5), lower = quantile(min_age, .1), upper = quantile(min_age, .9)))) %>%
  mutate(year = factor(year))

#adjust language levels according to size
fig3_df <- fig3_df %>%
  mutate(name = factor(name, levels = fig2_df %>% filter(year==2001) %>% arrange(q2) %>% pull(name)))

#plot
ggplot(fig3_df)+
  geom_point(aes(x = q2, y = name, color = year, group = year), position = position_dodge(-.4))+
  geom_errorbar(aes(xmin=lower, xmax=upper, y=name, color = year, group = year), linewidth=.8, position = position_dodge(-.4), width = 0)+
  scale_color_brewer(palette="Dark2")+
  theme(legend.position = c(.85, .85), legend.title = element_blank())+
  ylab("")+
  xlab("Age of the youngest speakers")

ggsave("Figures/Fig3.tiff", height=7, width=6, dpi=1000)

################################################################################
#5. TABLE 1: DORMANCY RISKS 
################################################################################
bind_rows(lapply(names, function(x) 
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year==2101) %>%
    group_by(run) %>%
    summarise(n = sum(n)) %>% 
    mutate(name = x))) %>% 
  filter(n == 0) %>% 
  group_by(name) %>%
  summarise(n_extinct = n()) %>%
  mutate(xrisk = n_extinct) %>%
  select(-n_extinct) %>%
  right_join(
    bind_rows(lapply(names, function(x) 
      readRDS(paste("Results/", x, sep = '')) %>% 
        filter(year==2101, age<15) %>%
        group_by(run) %>%
        summarise(n = sum(n)) %>% 
        mutate(name = x))) %>% 
      filter(n == 0) %>% 
      group_by(name) %>%
      summarise(n_extinct = n()) %>%
      mutate(risk_interrupt_it = n_extinct) %>%
      select(-n_extinct)) %>% 
  mutate(xrisk = ifelse(is.na(xrisk), 0, xrisk)) %>%
  arrange(-xrisk, -risk_interrupt_it) %>% 
  left_join(fig_x %>% filter(year==2101) %>% select(name, q2, lower, upper)) %>% 
  mutate(speaker_number = paste(q2, " (", lower, "-", upper, ")", sep = "")) %>% 
  select(-q2, -lower, -upper)
