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
