#---- load library
library(data.table)
library(sf)
library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(writexl)

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

list.unique <-
  eBird[Group.ID == "", EventID] %>%
  unique

eBird.unique <-
  eBird[EventID %in% c(list.shared, list.unique)] %>% 
  .[Count == "X", Count := 0] %>% 
  .[, Count := as.numeric(Count)] %>% 
  .[, All.Reported := as.character(All.Reported)] %>% 
  .[, Group.ID := NULL]

#---- merge two dataset
dat.all <- 
  bind_rows(CWBF.1, eBird.unique) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326) %>% 
  st_transform(3826)

#---- island shp
Island <-
  st_read("data/raw/Island.shp") %>%
  st_transform(3826)

#----
#---- Spatial join with island
dat.island <-
  st_intersection(dat.all, Island)

saveRDS(dat.island, "Results/island_data_3826.rds")


#####
dat.island <- 
  readRDS("Results/island_data_3826.rds")


checklist.dat <- 
  dat.island %>% 
  setDT %>% 
  .[, list(E.CommonName, 
           ScientificName,
           C_Desc)] %>% 
  unique %>% 
  .[!is.na(E.CommonName)]

island.checklist <- 
  lapply(unique(checklist.dat$C_Desc), 
         function(x)
           checklist.dat[C_Desc == x, -"C_Desc"] %>% 
           unique)
names(island.checklist) <- unique(checklist.dat$C_Desc)

write_xlsx(island.checklist, "Results/Island_checklist.xlsx")
