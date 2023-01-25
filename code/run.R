## User definied entries  -----------------------------------------------------------
maxyr <- 2022


## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", 
         "RODBC", 
         "here", 
         "janitor", 
         "getPass", 
         "dbscan")
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)


# loading check outlier function
source("code/check_outlier.R")




# RACEBASE tables ----------------------------------------------------
# source("C:/Users/sarah.friedman/Work/Rfunctions/ConnectToOracle_STF.R")

# downloading relevant data from Oracle
source("code/connect_to_oracle.R")
source("code/download_data.R")


# get catch and taxonomy info
catch <- read_csv("data/oracle/racebase-catch.csv") %>%
  janitor::clean_names() %>%
  mutate(year = as.numeric(stringr::str_extract(cruise, "^[:digit:]{4}")))

cruise <- read_csv("data/oracle/racebase-cruise.csv") %>%
  janitor::clean_names()

haul <- read_csv("data/oracle/racebase-haul.csv") %>%
  clean_names()


species_codes <- read_csv("data/oracle/race_data-race_species_codes.csv") %>%
  janitor::clean_names() %>%
  dplyr::select("species_code", "species_name", "common_name")




# Getting species from year ----------------------------------------------------


# non species indicator strings
rm_bits <- paste0(
  c(" egg", "egg case", "larva", "larvae", " tubes", "sp\\.$"), 
  collapse = "|"
)



# filtering to just specimens IDed to species and caught this survey year
species_maxyr <- catch %>%
  left_join(species_codes) %>%
  mutate(species_name = trimws(species_name),
         level = case_when(
           str_detect(species_name, rm_bits) | 
             !str_detect(species_name, " ") |
             is.na(species_name) ~ "",
           TRUE ~ "species"
         )) %>% 
  filter(level == "species") %>%
  dplyr::select(-level) %>%
  select(species_code) %>% 
  unique()





# Finding outliers ----------------------------------------------------

# checking for species that were caught this year that are suspicious/need manual checking using DBSCAN/past confirmed records


#all catch/haul data to check against
all_records <- catch %>%
  filter(species_code %in% species_maxyr$species_code & year > 2000) %>%
  left_join(haul, by = c("cruisejoin", "hauljoin")) %>%
  left_join(cruise) %>%
  filter(grepl("bottom trawl survey", survey_name, ignore.case = TRUE)) %>%
  filter(abundance_haul == "Y") %>%
  
  left_join(species_codes, by ="species_code") %>%
  dplyr::select(species_code, species_name, start_longitude, 
                start_latitude, gear_depth, voucher, year) 


# outlier species from this year
outlier_spp <- species_maxyr %>%
  mutate(outlier = purrr::map(species_code, ~check_outlier(.x, maxyr, all_records))) %>%
  unnest(cols = outlier) %>%
  filter(!is.na(species_name)) %>%
  left_join(species_codes) 




# plots outliers to pdf document
pdf(paste0("output/species_outliers_", maxyr, ".pdf"))
purrr::map(unique(outlier_spp$species_code),
           ~check_outlier(.x, maxyr, catch_haul, plot = T))
dev.off()
