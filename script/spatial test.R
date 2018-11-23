#---- load library
library(data.table)
library(sf)
library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)
library(ggplot2)

#---- 
#---- import data
# CWBF data
#separate observation date to YYYY/MM/DD
#select column needed and set type of column 
CWBF <- 
  fread("data/raw/eBird格式鳥種紀錄_補充資訊_0115修正.csv", 
        sep = "|", encoding = "UTF-8") %>%
  separate(Date, c("Month", "Date", "Year"), "/") %>%
  .[, list(C.CommonName = `Common Name`,
           Count = as.numeric(Number),
           Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           Year = as.numeric(Year),
           Month = as.numeric(Month),
           Duration = Duration,
           All.Reported = `All observations reported?`,
           EventID = as.character(f_id),
           Species.Code = e_code_ident)]
eBird.Taxon <- 
  read_xlsx("data/raw/eBird_Taxonomy_v2017_10Aug2017.xlsx") %>% 
  setDT %>% 
  .[, list(Category = CATEGORY,
           Species.Code = SPECIES_CODE,
           E.CommonName = PRIMARY_COM_NAME,
           ScientificName = SCI_NAME,
           C.CommonName.e = `Chinese, Mandarin (trad'l)`
  )]

CWBF.1 <- 
  eBird.Taxon[CWBF, on = "Species.Code"] %>% 
  .[Duration == "NULL", Duration := NA] %>% 
  .[, Duration := as.numeric(Duration)] %>% 
  .[, C.CommonName := NULL]

#---- eBird dataset
eBird <- 
  fread("data/raw/ebd_TW_200001_201712_prv_relMay-2017.txt",
        encoding = "UTF-8", sep = "\t") %>%
  separate("OBSERVATION DATE", c("Year", "Month", "Date"), "-") %>%
  .[, list(Category = CATEGORY,
           E.CommonName = `COMMON NAME`,
           ScientificName = `SCIENTIFIC NAME`,
           Count = `OBSERVATION COUNT`,
           Latitude = `LATITUDE`,
           Longitude = `LONGITUDE`,
           Year = as.numeric(Year),
           Month = as.numeric(Month),
           EventID = as.character(`SAMPLING EVENT IDENTIFIER`),
           Duration = `DURATION MINUTES`,
           All.Reported = `ALL SPECIES REPORTED`,
           Group.ID = `GROUP IDENTIFIER`)] 

# removed shared bird lists
list.shared <-
  eBird[Group.ID != "",
        list(Group.ID,
             EventID)] %>%
  unique %>%
  .[!duplicated(Group.ID), EventID]

#---- merge two dataset
dat.all <- 
  bind_rows(CWBF.1, eBird.unique)

#---- Taiwan Barbet
Barbet <- 
  dat.all[E.CommonName == "Taiwan Barbet"] %>% 
  .[Year != 2101] %>% 
  .[, Year.c := cut(Year, 
                    c(1970, 1980, 1990, 2000, 2010, 2020),
                    c("1970s", "1980s", "1990s", "2000s", "2010s"),
                    right = FALSE)] %>% 
  .[, Season := cut(Month,
                    c(1, 3, 6, 9, 12, 13),
                    c("Winter", "Spring", "Summer", "Fall", "Winter"),
                    right = FALSE)] %>% 
  .[Longitude >= 120]

ggplot() +
  geom_point(data = Barbet,
             aes(x = Longitude, y = Latitude)) +
  facet_grid(Season ~ Year.c) +
  coord_fixed()

#---- Gray-capped Woodpecker
G.Woodpecker <- 
  dat.all[E.CommonName == "Gray-capped Woodpecker"] %>% 
  .[Year != 2101] %>% 
  .[, Year.c := cut(Year, 
                    c(1970, 1980, 1990, 2000, 2010, 2020),
                    c("1970s", "1980s", "1990s", "2000s", "2010s"),
                    right = FALSE)] %>% 
  .[, Season := cut(Month,
                    c(1, 3, 6, 9, 12, 13),
                    c("Winter", "Spring", "Summer", "Fall", "Winter"),
                    right = FALSE)] %>% 
  .[Longitude >= 120]

ggplot() +
  geom_point(data = G.Woodpecker,
             aes(x = Longitude, y = Latitude)) +
  facet_grid(Year.c ~ Month) +
  coord_fixed() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
