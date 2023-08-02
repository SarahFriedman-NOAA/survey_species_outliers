## User definied entries  -----------------------------------------------------------
maxyr <- 2023
not_finalized <- TRUE #are not-yet-finalized data being checked?


## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", 
         "RODBC", 
         "here", 
         "janitor", 
         "getPass", 
         "dbscan",
         "ggrepel",
         "gapindex")

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)



# loading bespoke functions
source("code/functions.R")




# RACEBASE tables ----------------------------------------------------
#source("C:/Users/sarah.friedman/Work/Rfunctions/ConnectToOracle_STF.R")

# downloading relevant racebase data from Oracle
source("code/connect_to_oracle.R")
source("code/download_data.R")


# cleaning and formatting racebase data
source("code/clean_data.R")




# Getting species from year ----------------------------------------------------


# non species indicator strings
rm_bits <- paste0(
  c("egg", "egg case", "larva", "larvae", "tubes", "sp\\.$", "empty ", 
    "\\(juvenile\\)", "unid\\."), 
  collapse = "| "
)



# filtering to just specimens IDed to species and caught this survey year
if(not_finalized){
  catch_maxyr <- new_catch %>%
    mutate(year = maxyr)
} else {
  catch_maxyr <- old_catch %>%
    filter(year == maxyr)
}


species_maxyr <- catch_maxyr %>%
  left_join(species_codes) %>%
  mutate(species_name = trimws(species_name),
         level = case_when(
           str_detect(species_name, rm_bits) | 
             str_detect(common_name, "morphotype") |
             !str_detect(species_name, " ") |
             is.na(species_name) ~ "",
           TRUE ~ "species"
         )) %>% 
  filter(level == "species") %>%
  dplyr::select(-level)





# Finding outliers ----------------------------------------------------

# checking for species that were caught this year that are suspicious/need manual checking using DBSCAN/past confirmed records


#all catch/haul data to check against
racebase_records <- old_catch %>%
  filter(species_code %in% species_maxyr$species_code) %>%
  left_join(old_haul) %>%
  full_join(species_codes, by ="species_code") %>%
  dplyr::select(species_code, species_name, common_name, start_longitude, 
                start_latitude, voucher, year) 


# outlier species from this year
outlier_spp <- species_maxyr %>%
  group_by(species_name) %>%
  nest() %>%
  mutate(outlier = purrr::map(data, ~check_outlier(.x, maxyr, racebase_records))) %>%
  unnest(cols = outlier) %>%
  filter(!is.na(species_name)) %>%
  left_join(species_codes) 




# plots outliers to pdf document
pdf(paste0("output/species_outliers_", maxyr, ".pdf"))
species_maxyr %>%
  filter(species_name %in% outlier_spp$species_name) %>%
  group_by(species_name) %>%
  nest() %>%
  mutate(outlier = purrr::map(data, ~check_outlier(.x, maxyr, racebase_records, plot = T))) 
dev.off()


