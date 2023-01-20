## Download data sets to local machine -------------------------------------------------------

# RACEBASE tables to query
locations <- c(
  ## General Tables of data (racebase)
  "RACEBASE.HAUL",
  "RACEBASE.SPECIMEN",
  "RACEBASE.CRUISE",
  "RACEBASE.CATCH",
  #"RACEBASE.SPECIES_CLASSIFICATION",
  
  ## Race Data tables
  "RACE_DATA.RACE_SPECIES_CODES"
)




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

