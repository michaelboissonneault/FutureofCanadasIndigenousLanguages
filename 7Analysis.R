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
library(tidyverse)
library(openxlsx)
library(rnaturalearth)
library(sf)

#set theme
theme_set(theme_bw())

#vector of names 
names <- readRDS("scdata") %>% pull(name) %>% unique()

#create folder to store figures
dir.create("Figures")

#population size in 2001 and 2101 
fig2_df <- bind_rows(lapply(names, function(x)
  readRDS(paste("Results/", x, sep = '')) %>% 
    filter(year==2001 | year==2101) %>% 
    group_by(name, year, run) %>%
    summarise(n = sum(n)) %>% 
    group_by(name, year) %>% 
    summarise(q2 = quantile(n, .5), lower = quantile(n, .1), upper = quantile(n, .9)))) %>%
  mutate(year = factor(year))

################################################################################
#2. FIGURE 1: MAP
################################################################################
#Population in 2001 and coordinates
fig1_df <- fig2_df %>%
  filter(year==2001) %>%
  select(q2) %>%
  left_join(read.xlsx("coordinates.xlsx")) 

#map data from natural earth
world <- ne_countries(scale = "small", country=c("Canada", "United States of America"), returnclass = "sf")

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
  geom_point(data=fig1_df, aes(x=longitude, y=latitude, size=q2, color=family))+
  ylim(ylimits)+
  xlim(xlimits)+
  theme_bw(base_size=14)+
  theme(
    panel.background = element_rect(fill = "slategray1"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    legend.key.height = unit(.3, 'cm'),
    legend.title = element_text(size=8), 
    legend.text = element_text(size=7))+
  scale_color_brewer(palette = "Set1")+
  scale_size_continuous(breaks=c(100, 1000, 10000), name="Population size")

ggsave('Figures/Fig1.tiff', width=6, height=4, dpi=1000)

#In text comments
fig1_df %>% 
  ungroup %>%
  summarise(min=round(min(q2)), 
            max=round(max(q2)),
            mean=mean(q2),
            median=quantile(q2, .5))

fig1_df %>% filter(q2<=110)

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
  mutate(xrisk = n_extinct / 500 * 100) %>%
  select(-n_extinct) %>%
  right_join(
    bind_rows(lapply(names, function(x) 
      readRDS(paste("Results/", x, sep = '')) %>% 
        filter(year==2101, age<45) %>%
        group_by(run) %>%
        summarise(n = sum(n)) %>% 
        mutate(name = x))) %>% 
      filter(n == 0) %>% 
      group_by(name) %>%
      summarise(n_extinct = n()) %>%
      mutate(risk_interrupt_it = n_extinct / 500 * 100) %>%
      select(-n_extinct)) %>% 
  mutate(xrisk = ifelse(is.na(xrisk), 0, xrisk)) %>%
  arrange(-xrisk, -risk_interrupt_it)
