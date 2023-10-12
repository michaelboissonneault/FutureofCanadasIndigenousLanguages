################################################################################
#IN THIS DOFILE
  #1. DOWNLOAD AND UNZIP FILES FROM WPP 2022
  #2. SELECT APPROPRIATE SCHEDULES AND ARRANGE IT INTO MATRICES
  #3. PUT SCHEDULES IN LIST AND SAVE 
################################################################################
rm(list=ls())

#Packages
library(tidyverse)

################################################################################
#1. DOWNLOAD AND UNZIP FILES FROM WPP 2022######################################
################################################################################
#set timeout to 200 sec
options(timeout=200)

#Create temporary file
temp <- tempfile()

#Download and unzip fertility data
download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Fertility_by_Age5.zip", temp)
ft <- read.csv(unz(temp, "WPP2022_Fertility_by_Age5.csv"))

#Download and unzip mortality data, years 1950-2021
download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Abridged_Medium_1950-2021.zip", temp)
lt1 <- read.csv(unz(temp, "WPP2022_Life_Table_Abridged_Medium_1950-2021.csv"))

#Download and unzip mortality data, years 2022-2100
download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Abridged_Medium_2022-2100.zip", temp)
lt2 <- read.csv(unz(temp, "WPP2022_Life_Table_Abridged_Medium_2022-2100.csv"))

#Remove the remporary file
unlink(temp)

#combine lt1 and lt2, remove
lt <- bind_rows(lt1, lt2)
remove(lt1, lt2)

################################################################################
#2. SELECT APPROPRIATE SCHEDULES AND ARRANGE IT INTO MATRICES###################
################################################################################
#vector of candidate schedules
candidates <- c("Less developed regions", "World",
                "Middle-income countries", 
                "Upper-middle-income countries", "Lower-middle-income countries", "No income group available")

#visualize possibilities for fertility schedules
ggplot(ft %>% 
         filter(Time %in% seq(1976, 2006, 5), Location %in% candidates, Variant == "Medium") %>% 
         group_by(Location, Time) %>%
         summarise(tfr = sum(ASFR)/200))+
  geom_line(aes(Time, tfr, color = Location, group = Location), linewidth = 2)+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(breaks = seq(1976, 2006, 10))+
  theme_bw()

ft %>% 
  filter((Time==2011 | Time==2096), Location %in% candidates, Variant == "Medium") %>% 
  group_by(Location, Time) %>%
  summarise(tfr = sum(ASFR)/200)

#visualize possibilities for mortality schedules
ggplot(lt %>% 
         filter(Time %in% seq(1971, 2101, 5), Location %in% candidates, AgeGrpStart==0, Variant =="Medium", Sex =="Total"))+
  geom_line(aes(Time, ex, color = Location, group = Location), linewidth = 2)+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(breaks = seq(1971, 2101, 20))+
  theme_bw()

lt %>% 
  filter((Time==2006 | Time==2096), Location %in% candidates, AgeGrpStart==0, Variant =="Medium", Sex =="Total") %>%
  select(Location, ex)

#visualize possibilities for mortality under 1
ggplot(lt %>% 
         filter(Time %in% seq(1976, 2001, 5), Location %in% candidates, AgeGrpStart==0, Variant =="Medium", Sex =="Total"))+
  geom_line(aes(Time, qx, color = Location, group = Location), linewidth = 2)+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(breaks = seq(1971, 2101, 20))+
  theme_bw()

#list to store the mortality schedules
m_list <- list()

#loop for the mortality probabilities
for (c in candidates){
  
  m1 <- lt %>% 
    filter(Location==c, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart!=0) %>%
    mutate(qx = ifelse(AgeGrpStart==1, 1-lx/100000, qx)) %>%
    pull(qx) %>%
    matrix(nrow=21)
  
  #correction to have mortality halfway
  m1 <- (m1[-21, -31] + m1[-1, -1])/2
  m1 <- rbind(m1, matrix(rep(1, 30), ncol=30))
  
  #mortality between 0 and 2.5 (take mortality 0-1 and that between 1-4 x (2/3))
  m1_0 <- c(
    lt %>% 
      filter(Location==c, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==0) %>%
      pull(qx)+ 
      lt %>% 
      filter(Location==c, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==1) %>%
      pull(qx)*(2/3))[-31]
  
  #mortality between 0 and 5 is actuall between 2.5 and 5
  m1[1,] <- c(lt %>% 
                filter(Location==c, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==1) %>%
                pull(qx)*(1/3))[-31]

  m_list[[which(candidates==c)]] <- list(m1, m1_0)
  
  }
  
#list to store the fertility schedules
f_list <- list()
  
#loop for the mortality probabilities
for (c in candidates){
    
  #fertility rates
  f1 <- ft %>% 
    filter(Location==c, Time %in% seq(1951, 2096, 5), AgeGrpStart<=45, Variant=="Medium") %>%
    pull(ASFR) %>%
    matrix(nrow=8)
  
  #fill the rest of the fertility matrix with zeros
  f1 <- rbind(matrix(nrow=2, ncol=30, 0), f1, matrix(nrow=11, ncol=30, 0))/400
  
  f_list[[which(candidates==c)]] <- f1

  }
  
################################################################################
#3. PUT SCHEDULES IN LIST AND SAVE #############################################
################################################################################
#save
saveRDS(list(m_list, f_list), "DemographicParameters")
