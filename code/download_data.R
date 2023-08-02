## Download data sets to local machine -------------------------------------------------------

# RACEBASE tables to query
locations <- c(
  ## General Tables of data (racebase)
  "RACEBASE.HAUL",
  "RACEBASE.SPECIMEN",
  "RACEBASE.CRUISE",
  "RACEBASE.CATCH",
  
  ## Race Data tables
  "RACE_DATA.RACE_SPECIES_CODES",
  #"RACE_DATA.CRUISES",
  "RACE_DATA.V_CRUISES"
)


# adding edit tables for non-finalized data
if(not_finalized){
  locations <- c(locations, 
 
   ## Race Data edit tables
  "RACE_DATA.EDIT_CATCH_SPECIES",
  "RACE_DATA.EDIT_CATCH_SAMPLES",
  "RACE_DATA.EDIT_HAULS",
  "RACE_DATA.EDIT_EVENTS")
}


if (!file.exists("data/oracle")) dir.create("data/oracle", recursive = TRUE)



  if(!exists("channel")){
    cat("Tables required from racebase. Connect to Oracle and re-run this script.\n")
    source("code/ConnectToOracle.R")
  }
  for (i in 1:length(locations)) {
    print(locations[i])
    filename <- tolower(gsub("\\.", "-", locations[i]))
    a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
      write_csv(
        x = a,
        here("data", "oracle", paste0(filename, ".csv"))
      )
      remove(a)
    
  }

