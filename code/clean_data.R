# get all catch and taxonomy info together and filtering to just groundfish surveys after 2000
survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                    "BSS" = 78, "NBS" = 143)

old_cruise <- read_csv("data/oracle/race_data-v_cruises.csv") %>%
  janitor::clean_names() %>%
  filter(year >= 2000 & survey_definition_id %in% survey_def_ids) %>%
  select(year, survey_definition_id, cruisejoin, region, cruise, cruise_id, vessel_id)

old_haul <- read_csv("data/oracle/racebase-haul.csv") %>%
  clean_names() %>%
  filter(abundance_haul == "Y") %>%
  select(cruisejoin:haul, start_latitude, start_longitude)


old_catch <- read_csv("data/oracle/racebase-catch.csv") %>%
  janitor::clean_names() %>%
  select(-c(weight, number_fish, subsample_code)) %>%
  right_join(old_cruise) %>%
  right_join(old_haul)


species_codes <- read_csv("data/oracle/race_data-race_species_codes.csv") %>%
  janitor::clean_names() %>%
  dplyr::select("species_code", "species_name", "common_name")





# download un-finalized data if needed
if(not_finalized){
  new_cruise <- old_cruise %>% filter(year == maxyr)
  
  # compiling haul data and lat/lon
  new_haul <- read_csv("data/oracle/race_data-edit_events.csv") %>%
    janitor::clean_names() %>%
    filter(event_type_id == 4) %>% # taking lat/lon at EQ for each haul
    mutate(start_latitude = ddm_to_dd(edit_latitude, "lat"), # converting ddm to dd
           start_longitude = ddm_to_dd(edit_longitude, "long")) %>%
    select(haul_id, contains("start")) %>%
    right_join(read_csv("data/oracle/race_data-edit_hauls.csv") %>%
                 janitor::clean_names() %>%
                 right_join(new_cruise) %>%
                 select(haul_id, cruise_id, haul, region, vessel_id)
               )
  
  new_catch <- read_csv("data/oracle/race_data-edit_catch_species.csv") %>%
    select(CATCH_SPECIES_ID, CATCH_SAMPLE_ID, SPECIES_CODE, 
           EDIT_SPECIES_NAME, VOUCHER_NUMBER) %>%
    left_join(read_csv("data/oracle/race_data-edit_catch_samples.csv")) %>%
    janitor::clean_names() %>%
    select(haul_id, species_code, voucher = voucher_number) %>%
    right_join(new_haul)
    
  }
