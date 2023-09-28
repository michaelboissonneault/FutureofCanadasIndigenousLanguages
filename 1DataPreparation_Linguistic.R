################################################################################
#In this do file
#Clean and put together StatCan data on speaker numbers, Indigenous languages, by groups of five years of age, years 2001-2021
#1. YEAR 2001
#2. YEAR 2006
#3. YEAR 2011
#4. YEAR 2016
#5. YEAR 2021
#6. YEARS 2001-2021

rm(list=ls())

library(tidyverse)

################################################################################
#1.YEAR 2001####################################################################
################################################################################
#load data, select appropriate information, add age vector
sc01 <- read.csv("indigenousmothertongue2001.csv")[5:39, -2] %>% 
  pivot_longer(X0.4:X100., names_to="age", values_to="pop") %>%
  mutate(age = str_extract(age, "[0-9]+")) %>%
  rename(name = 1) %>%
  filter(!str_detect(name, "languages") | (str_detect(name, "n.i.e.") | str_detect(name, "n.o.s."))) %>%
  mutate(year = 2001, name = trimws(str_remove(name, " languages")))

#assign ISO codes
sc01 <- sc01 %>% 
  mutate(iso = case_when(
    name == "Algonquin" ~ "alq", 
    name == "Attikamekw" ~ "atj",
    name == "Blackfoot" ~ "bla",
    name == "Cree" ~ "Cree",
    name == "Malecite" ~ "pqm",
    name == "Micmac" ~ "mic",
    name == "Montagnais-Naskapi" ~ "Montagnais-Naskapi",
    name == "Ojibway" ~ "Ojibway",
    name == "Oji-Cree" ~ "ojs",
    name == "Algonquian, n.i.e." ~ "Algonquian languages",
    
    name == "Carrier" ~ "crx",
    name == "Chilcotin" ~ "clc",
    name == "Chipewyan" ~ "Chipewyan",
    name == "Dene" ~ "chp",
    name == "Dogrib" ~ "dgr",
    name == "Kutchin-Gwich'in (Loucheux)" ~ "gwi",
    name == "North Slave (Hare)" ~ "scs",
    name == "South Slave" ~ "xsl",
    name == "Athapaskan, n.i.e." ~ "Athabaskan languages",
    
    name == "Tlingit" ~ "tli",
    name == "Haida" ~ "hax-hdn",
    
    name == "Mohawk" ~ "moh",
    name == "Iroquoian, n.i.e." ~ "Iroquoian languages",
    
    name == "Kutenai" ~ "kut",
    
    name == "Shuswap" ~ "shs",
    name == "Thompson (Ntlakapamux)" ~ "thp",
    name == "Salish, n.i.e." ~ "Salish languages",
    
    name == "Dakota/Sioux" ~ "Siouan",
    
    name == "Gitksan" ~ "git",
    name == "Nishga" ~ "ncg",
    name == "Tsimshian" ~ "tsi",
    
    name == "Nootka" ~ "nuk",
    name == "Wakashan, n.i.e." ~ "Wakashan languages",
    
    name == "Inuktitut (Eskimo)" ~ "ike",
    
    name == "Aboriginal, n.i.e." ~ "Indigenous, n.i.e."))

#languages to which we assign NA speakers because they are not in this census round
sc01 <- bind_rows(sc01, 
                  data.frame(
                    iso = c("crj", "crk", "crl", "crm", "csw", "cwd", #Cree languages
                            "moe","nsk",                              #Innu-Naskapi
                            "ciw", "ojw", "otw",                      #Ojibway
                            "bcr", "bea","sek", "kkz","srs", "ttm",
                            "tce","tht", "Slavey", "Tutchone",        #Athabaskan
                            "cay", "one",                             #Iroquoian
                            "hur", "lil", "squ","str", "oka", "coo",  #Salish
                            "dak", "sto", "asb", "Siouan languages",  #Siouan
                            "has", "hei", "kwk",                      #Wakashan
                            "ikt", "Inuvialuktun", "Inuit languages", #Inuit languages
                            "crg", "Indigenous, n.o.s."),             #Other
                    pop = NA,
                    year = 2001))

sc01 %>% pull(iso) %>% unique()

################################################################################
#2. YEAR 2006###################################################################
################################################################################
#load data, select appropriate information, add age vector
sc06 <- read.csv("indigenousmothertongue2006.csv")[5:40, -2] %>% 
  pivot_longer(X0.to.4.years:X100.years.and.over, names_to="age", values_to="pop") %>%
  mutate(age = str_extract(age, "[0-9]+")) %>%
  rename(name = 1) %>%
  filter(!str_detect(name, "languages") | (str_detect(name, "n.i.e.") | str_detect(name, "n.o.s."))) %>%
  mutate(year = 2006, name = trimws(str_remove(name, " languages")))

#add iso codes
sc06 <- left_join(sc06, sc01 %>% select(name, iso) %>% unique()) %>%
  bind_rows(data.frame(name = NA, pop = NA, age = NA, year = 2006, iso = sc01 %>% filter(is.na(name)) %>% pull(iso)))

#check missing iso's
sc06 %>% filter(is.na(iso)) %>% pull(name) %>% unique() %>% sort()

#add iso's manually
sc06 <- sc06 %>% 
  mutate(iso = case_when(
    name == "Inuinnaqtun" ~ "ikt",
    name == "Inuktitut, n.i.e." ~ "ike",
    name == "Atikamekw" ~ "atj",
    name == "Mi'kmaq" ~ "mic",
    name == "Nisga'a" ~ "ncg",
    TRUE ~ as.character(iso))) 

#check name changes, missings
sc06 %>% pull(iso) %>% unique()

################################################################################
#3. YEAR 2011###################################################################
################################################################################
sc11 <- read.csv("indigenousmothertongue2011.csv")[7:80, -2] %>% 
  pivot_longer(X0.to.4.years:X100.years.and.over, names_to="age", values_to="pop") %>%
  mutate(age = str_extract(age, "[0-9]+")) %>%
  rename(name = 1) %>%
  filter(!str_detect(name, "languages") | (str_detect(name, "n.i.e.") | str_detect(name, "n.o.s."))) %>%
  mutate(year = 2011, name = trimws(str_remove(name, " languages")))

#extract names between parentheses
sc11 <- sc11 %>% 
  mutate(name1 = str_remove(name," \\s*\\([^\\)]+\\)"),
         name2 = str_extract(name,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"))

#add iso codes: by full name
sc11 <- sc11 %>% 
  left_join(sc06 %>% filter(!is.na(name)) %>% select(name, iso) %>% unique())

#add iso codes: by name before parenthesis
sc11 <- sc11 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc06 %>% filter(!is.na(name)) %>% select(name, iso) %>% unique(), by=c("name1" = "name")) %>%
  bind_rows(sc11 %>% filter(!is.na(iso)))

#add iso codes: by name between parenthesis
sc11 <- sc11 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc06 %>% filter(!is.na(name)) %>% select(name, iso) %>% unique(), by=c("name2" = "name")) %>%
  bind_rows(sc11 %>% filter(!is.na(iso))) %>%
  filter(name!="Cree, n.i.e.") 

#check missing iso's
sc11 %>% filter(is.na(iso)) %>% pull(name) %>% unique() %>% sort()

#add iso's manually
sc11 <- sc11 %>% 
  mutate(iso = case_when(
    name == "Aboriginal, n.i.e." ~ "Indigenous, n.i.e.",
    name == "Beaver" ~ "bea", 
    name == "Cayuga" ~ "cay",
    name == "Cree, n.o.s." ~ "Cree", 
    name == "Cree, n.i.e." ~ "Cree",
    name == "Dakota" ~ "dak",
    name == "Gwich'in" ~ "gwi",
    name == "Haisla" ~ "has",
    name == "Halkomelem" ~ "hur",
    name == "Heiltsuk" ~ "hei",
    name == "Innu/Montagnais" ~ "moe",
    name == "Inuvialuktun" ~ "Inuvialuktun",
    name == "Inuit, n.i.e." ~ "Inuit languages",
    name == "Inuktitut" ~ "ike",
    name == "Kaska (Nahani)" ~ "kkz",
    name == "Kwakiutl (Kwak'wala)" ~ "kwk",
    name == "Lillooet" ~ "lil",
    name == "Michif" ~ "crg",
    name == "Naskapi" ~ "nsk",
    name == "North Slavey (Hare)" ~ "scs",  
    name == "Northern Tutchone" ~ "ttm",
    name == "Okanagan" ~ "oka",
    name == "Oneida" ~ "one",
    name == "Plains Cree" ~ "crk",
    name == "Sarcee" ~ "srs",
    name == "Sekani" ~ "sek",
    name == "Siouan, n.i.e." ~ "Siouan languages",
    name == "Slavey, n.o.s." ~ "Slavey",
    name == "South Slavey" ~ "xsl",
    name == "Southern Tutchone" ~ "tce",
    name == "Squamish" ~ "squ",
    name == "Stoney" ~ "sto",
    name == "Straits" ~ "str",
    name == "Swampy Cree" ~ "csw",
    name == "Tahltan" ~ "tht",
    name == "Tutchone, n.o.s." ~ "Tutchone",
    name == "Wetsuweten" ~ "bcr",
    name == "Woods Cree" ~ "cwd",
    TRUE ~ as.character(iso))) 

#reorganize data because Cree appears twice
sc11 <- sc11 %>% group_by(name, age, year, name1, name2, iso) %>% summarise(pop = sum(pop))

#add iso for languages that did not appear in this census data release
sc11 <- sc11 %>% 
  select(name, pop, age, year, iso, name1, name2) %>%
  bind_rows(data.frame(name = NA, 
                       pop = NA, 
                       age = NA, 
                       year = 2011,
                       iso = sc06 %>% pull(iso) %>% unique() %>%
                         setdiff(sc11 %>% pull(iso) %>% unique()))) %>% ungroup()

sc11 %>% arrange(iso) %>% pull(iso) %>% unique() %>% 
  setdiff(sc06 %>% arrange(iso) %>% pull(iso) %>% unique())

sc06 %>% arrange(iso) %>% pull(iso) %>% unique() %>% 
  setdiff(sc11 %>% arrange(iso) %>% pull(iso) %>% unique())

sc11 %>% arrange(iso) %>% pull(iso) %>% unique()

################################################################################
#4. YEAR 2016###################################################################
################################################################################
sc16 <- read.csv("indigenousmothertongue2016.csv")[8:89, -2] %>% 
  pivot_longer(X0.to.4.years:X100.years.and.over, names_to="age", values_to="pop") %>%
  mutate(age = str_extract(age, "[0-9]+")) %>%
  rename(name = 1) %>%
  filter(!str_detect(name, "languages") | (str_detect(name, "n.i.e.") | str_detect(name, "n.o.s."))) %>%
  mutate(year = 2016, name = trimws(str_remove(name, " languages")))

#extract names between parentheses
sc16 <- sc16 %>% 
  mutate(name1 = str_remove(name," \\s*\\([^\\)]+\\)"),
         name2 = str_extract(name,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"))

#add iso codes: by full name
sc16 <- sc16 %>% left_join(sc11 %>% select(name, iso) %>% unique())

#add iso codes: by name before parenthesis
sc16 <- sc16 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc11 %>% select(name1, iso) %>% unique()) %>%
  bind_rows(sc16 %>% filter(!is.na(iso)))

sc16 <- sc16 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc11 %>% select(name2, iso) %>% filter(!is.na(name2)) %>% unique(), by=c("name1" = "name2")) %>%
  bind_rows(sc16 %>% filter(!is.na(iso)))

#add iso codes: by name between parenthesis
sc16 <- sc16 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc11 %>% filter(!is.na(name)) %>% select(name1, iso) %>% unique(), by=c("name2" = "name1")) %>%
  bind_rows(sc16 %>% filter(!is.na(iso)))

sc16 <- sc16 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc11 %>% filter(!is.na(name)) %>% select(name2, iso) %>% filter(!is.na(name2)) %>% unique()) %>%
  bind_rows(sc16 %>% filter(!is.na(iso)))

#check name changes, missings
sc16 %>% filter(is.na(iso)) %>% pull(name) %>% unique() %>% sort()

#change iso's manually
sc16 <- sc16 %>% 
  mutate(iso = case_when(
    name == "Aboriginal, n.o.s." ~ "Indigenous, n.o.s.",
    name == "Athabaskan, n.i.e." ~ "Athabaskan languages",
    name == "Babine (Wetsuwet'en)" ~ "bcr", 
    name == "Comox" ~ "coo",
    name == "Montagnais (Innu)" ~ "moe",
    name == "Moose Cree" ~ "crm",
    name == "Northern East Cree" ~ "crl",
    name == "Southern East Cree" ~ "crj",
    name == "Ottawa (Odawa)" ~ "otw",
    TRUE ~ as.character(iso)))

#add iso for languages that did not appear in this census data release
sc16 <- sc16 %>% 
  select(name, pop, age, year, iso, name1, name2) %>%
  bind_rows(data.frame(name = NA, 
                       pop = NA, 
                       age = NA, 
                       year = 2016,
                       iso = sc11 %>% pull(iso) %>% unique() %>%
                         setdiff(sc16 %>% pull(iso) %>% unique())))

sc16 %>% pull(iso) %>% unique()

################################################################################
#5. YEAR 2021###################################################################
################################################################################
#load data
sc21 <- read.csv("indigenousmothertongue2021.csv")[5:76, -2] %>% 
  pivot_longer(X0.to.4.years:X100.years.and.over, names_to="age", values_to="pop") %>%
  mutate(age = str_extract(age, "[0-9]+")) %>%
  rename(name = 1) %>%
  filter(!str_detect(name, "languages") | (str_detect(name, "n.i.e.") | str_detect(name, "n.o.s."))) %>%
  mutate(year = 2021, name = trimws(str_remove(name, " languages")))

#extract names between parentheses
sc21 <- sc21 %>% 
  mutate(name1 = str_remove(name," \\s*\\([^\\)]+\\)"),
         name2 = str_extract(name,"(?<=\\()([^()]*?)(?=\\)[^()]*$)"))

#add iso codes: by full name
sc21 <- sc21 %>% 
  left_join(sc16 %>% filter(!is.na(name)) %>% select(name, iso) %>% unique())

#add iso codes: by name before parenthesis
sc21 <- sc21 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc16 %>% filter(!is.na(name)) %>% select(name1, iso) %>% unique()) %>%
  bind_rows(sc21 %>% filter(!is.na(iso)))

sc21 <- sc21 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc16 %>% filter(!is.na(name), !is.na(name2)) %>% select(name2, iso) %>% unique(), by=c("name1" = "name2")) %>%
  bind_rows(sc21 %>% filter(!is.na(iso)))

#add iso codes: by name between parenthesis
sc21 <- sc21 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc16 %>% filter(!is.na(name)) %>%select(name1, iso) %>% unique(), by=c("name2" = "name1")) %>%
  bind_rows(sc21 %>% filter(!is.na(iso)))

sc21 <- sc21 %>% 
  filter(is.na(iso)) %>% 
  select(name, age, pop, year, name1, name2) %>% 
  left_join(sc16 %>% filter(!is.na(name), !is.na(name2)) %>% select(name2, iso) %>% unique()) %>%
  bind_rows(sc21 %>% filter(!is.na(iso)))

#check name changes, missings
sc21 %>% filter(is.na(iso)) %>% pull(name) %>% unique() %>% sort()

#change iso's manually
sc21 <- sc21 %>% 
  mutate(iso = case_when(
    name == "Anishinaabemowin (Chippewa)" ~ "ciw", 
    name == "Assiniboine" ~ "asb",
    name == "Dene, n.o.s." ~ "chp",
    name == "Inuktut (Inuit), n.i.e." ~ "Inuit languages",
    name == "Ojibway, n.o.s." ~ "Ojibway",
    name == "Saulteau (Western Ojibway)" ~ "ojw",
    name == "Tutchone, n.o.s." ~ "Tutchone",
    name == "Wetsuwet'en-Babine" ~ "bcr",
    name == "Inuvialuktun" ~ "Inuvialuktun",
    name == "Indigenous, n.i.e." ~ "Indigenous, n.i.e.",
    name == "Indigenous, n.o.s." ~ "Indigenous, n.o.s.",
    TRUE ~ as.character(iso)))

#add iso for languages that did not appear in this census data release
sc21 <- sc21 %>% 
  select(name, pop, age, year, iso, name1, name2) %>%
  bind_rows(data.frame(name = NA, 
                       pop = NA, 
                       age = NA, 
                       year = 2021,
                       iso = sc16 %>% pull(iso) %>% unique() %>%
                         setdiff(sc21 %>% pull(iso) %>% unique())))

sc21 %>% pull(iso) %>% unique()

################################################################################
#6. ALL YEARS###################################################################
################################################################################
#combine in single data frame
sc <- bind_rows(sc01, sc06, sc11, sc16, sc21) %>%
  select(-name, -name1, -name2) %>%
  rename(name = iso) %>%
  mutate(age = as.numeric(age))

#specify continuum 
sc <- sc %>% 
  mutate(continuum = case_when(
    name %in% c("Cree", "csw", "crk", "cwd", "crl", "crj", "crm") ~ "Cree",
    name %in% c("Montagnais-Naskapi", "moe", "nsk") ~ "Innu-Naskapi",
    name %in% c("Ojibway", "otw", "ciw", "ojw") ~ "Ojibway",
    name %in% c('Chipewyan', "chp") ~ "Dene",
    name %in% c("scs", "xsl", "Slavey") ~ "Slavey",
    name %in% c("ttm", "tce", "Tutchone") ~ "Tutchone",
    name %in% c("dak", "sto", "asb", "Siouan", "Siouan languages") ~ "Dakotan",
    name %in% c("ikt", "Inuvialuktun") ~ "Inuinnaqtun"))

#find the speaker numbers for the different continua
sc <- sc %>% 
  bind_rows(sc %>% 
              filter(!is.na(continuum)) %>%
              group_by(age, year, continuum) %>% 
              summarise(pop = sum(pop, na.rm = T)) %>%
              rename(name = continuum) %>%
              mutate(name = case_when(
                name == "Cree" ~ "Cree langs.",
                name == "Ojibway" ~ "Ojibwa langs.",
                name == "Slavey" ~ "Slavey (N & S)",
                name == "Tutchone" ~ "Tutchone (N & S)",
                name == "Dakotan" ~ "Dakotan langs.",
                .default = as.character(name)),
                group = "aggregate"))

#find the speaker numbers for the category "Indigenous, n.o.s." (2016 and 2021)
indigenous_nos <- sc %>% 
  filter(name=="Indigenous, n.o.s.", !is.na(pop)) %>%
  select(age, year, pop) %>%
  rename(indig_nos = pop)

#add the nos speakers to the languages in 2016 and 2021, number equal proportion in entire indigenous speaker community
sc <- sc %>% 
  left_join(sc %>% filter(is.na(continuum), year>=2016, !is.na(pop)) %>% group_by(age, year) %>% summarise(total = sum(pop))) %>%
  mutate(prop = pop / total) %>% 
  left_join(indigenous_nos) %>% 
  mutate(pop = ifelse(is.na(prop), pop, pop + prop*indig_nos)) %>%
  select(-total, -prop, -indig_nos) %>%
  arrange(year, name, age)

#identify languages or continua that appear 5 times or more
fivetimes <- sc %>% 
  filter(!is.na(pop), !is.na(year), !is.na(age), is.na(continuum), !grepl('languages', name)) %>% 
  distinct(name, year) %>% 
  group_by(name) %>%
  summarise(n = n()) %>%
  filter(n ==5) %>% 
  pull(name)

#keep observations that appear 5 times
sc <- sc %>% 
  filter(name %in% fivetimes, !is.na(age)) %>%
  select(-continuum) %>%
  arrange(year, name, age) 
  
#add the sc names in 2021 and use them where possible
sc <- left_join(sc, sc21 %>% select(name, iso) %>% unique() %>% rename(sc_name = 1, name = 2)) %>%
  mutate(iso = ifelse(!is.na(sc_name), name, NA),
         name = ifelse(is.na(sc_name), name, str_remove(sc_name, " \\s*\\([^\\)]+\\)"))) %>%
  select(-sc_name)

#save
saveRDS(sc, "scdata")

################################################################################