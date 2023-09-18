#IN THIS DOFILE
  #READ THE WPP DATA 2022 AND KEEP ONLY THE INFO THAT WILL BE USED IN OUR STUDY

rm(list=ls())

#Packages
library(tidyverse)



################################################################################
#2. DOWNLOAD AND UNZIP FILES FROM WPP 2022######################################
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
#3. SELECT APPROPRIATE DATA AND ARRANGE IT INTO MATRICES########################
################################################################################
#choice of schedules
m_sched <- "Upper-middle-income countries"
f_sched <- "Lower-middle-income countries"

#mortality probabilities: registered indians
m1 <- lt %>% 
  filter(Location==m_sched, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart!=0) %>%
  mutate(qx = ifelse(AgeGrpStart==1, 1-lx/100000, qx)) %>%
  pull(qx) %>%
  matrix(nrow=21)

#correction to have mortality halfway
m1 <- (m1[-21, -31] + m1[-1, -1])/2
m1 <- rbind(m1, matrix(rep(1, 30), ncol=30))

#mortality between 0 and 2.5 (take mortality 0-1 and that between 1-4 x (2/3))
m1_0 <- c(
  lt %>% 
    filter(Location==m_sched, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==0) %>%
    pull(qx)+ 
    lt %>% 
    filter(Location==m_sched, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==1) %>%
    pull(qx)*(2/3))[-31]

#mortality between 0 and 5 is actuall between 2.5 and 5
m1[1,] <- c(lt %>% 
              filter(Location==m_sched, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==1) %>%
              pull(qx)*(1/3))[-31]

#mortality probabilities: Inuit
m2 <- lt %>% 
  filter(Location==f_sched, Sex=="Total", Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart!=0) %>%
  mutate(qx = ifelse(AgeGrpStart==1, 1-lx/100000, qx)) %>%
  pull(qx) %>%
  matrix(nrow=21)

#correction to have mortality halfway
m2 <- (m2[-21, -31] + m2[-1, -1])/2
m2 <- rbind(m2, matrix(rep(1, 30), ncol=30))

#mortality between 0 and 2.5 (take mortality 0-1 and that between 1-4 x (2/3))
m2_0 <- c(
  lt %>% 
    filter(Location==f_sched, Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==0, Sex=="Total") %>%
    pull(qx)+ 
    lt %>% 
    filter(Location==f_sched, Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==1, Sex=="Total") %>%
    pull(qx)*(2/3))[-31]

#mortality between 0 and 5 is actuall between 2.5 and 5
m2[1,] <- c(lt %>% 
              filter(Location==f_sched, Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart==1, Sex=="Total") %>%
              pull(qx)*(1/3))[-31]

#fertility rates
f <- ft %>% 
  filter(Location==f_sched, Time %in% c(seq(1951, 2096, 5), 2100), AgeGrpStart<=45, Variant=="Medium") %>%
  pull(ASFR) %>%
  matrix(nrow=8)

#fill the rest of the fertility matrix with zeros
f <- rbind(matrix(nrow=2, ncol=31, 0), f, matrix(nrow=11, ncol=31, 0))/400

################################################################################
#4. PUT IN LIST AND SAVE #######################################################
################################################################################
#save
saveRDS(list(m1, m1_0, m2, m2_0, f), "DemographicParameters")
