# Load Libraries
library(pacman)
p_load(dplyr, nnet, ggplot2, effects, broom, stringr, purrr, tidyr, fst,
  car, fastDummies, data.table, tidycensus, ggplot2, scales, tigris, sf)

# Set working directory
setwd("~/data/projects/climate_displacement/")

# florida_counties <- counties(year = 2013, state = "FL", cb = TRUE)

severely_affected_counties <- c("12087", "12086", "12021")
monroe <- c("12087")

# Counties affected by the hurriacne
read_hh_func <- function(counties, start_year, end_year, region) {

# Set working directory
setwd("~/data/raw/dataaxle/")

# Load IDs of households in study areas
for(year in start_year:end_year){
  cat("Processing", year, "FAMILYIDs...\n")
  ig <- read_fst(paste0("US_Consumer_5_File_", year, ".fst"),
                 columns = c("FAMILYID", "GE_CENSUS_STATE_2010", "GE_CENSUS_COUNTY")) %>%
    ## Filter to specific counties
    filter(paste0(str_pad(GE_CENSUS_STATE_2010, 2, "left", "0"),
                  str_pad(GE_CENSUS_COUNTY, 3, "left", "0")) %in% counties)
  setattr(ig$FAMILYID, "class", "integer64")
  ## Create list of FAMILYIDs
  if(exists("familyids")){
    familyids <<- c(familyids, ig %>% pull(FAMILYID))
  } else {
    familyids <<- ig %>% pull(FAMILYID)
  }
}

print("read!")

# Initialize an empty panel object
panel <- NULL

for(year in start_year:end_year){
  cat("Processing", year, "infogroup data...\n")
  ig <- read_fst(paste0("US_Consumer_5_File_", year, ".fst"),
                 ## Add additional fields as desired
                 columns = c("FAMILYID", "GE_CENSUS_STATE_2010", "GE_CENSUS_COUNTY", "GE_ALS_CENSUS_TRACT_2010",
                             "STREET_NAME", "CITY",
                             "GE_LONGITUDE_2010", "GE_LATITUDE_2010",
                             "first_name_1", "last_name_1", "gender_1", "Ethnicity_Code_1", "HEAD_HH_AGE_CODE",
                             "FIND_DIV_1000", "OWNER_RENTER_STATUS", "LOCATION_TYPE",
                             "MARITAL_STATUS", "CHILDRENHHCOUNT", "CHILDREN_IND", "LENGTH_OF_RESIDENCE"
                             )) %>%
    filter(STREET_NAME != "" & FAMILYID %in% familyids) %>%
    mutate(YEAR = year)
  setattr(ig$FAMILYID, "class", "integer64")
  print(head(ig))
    # Append ig object to the panel using rbind
  if (is.null(panel)) {
    panel <- ig
  } else {
    panel <- rbind(panel, ig)
  }
  print(table(panel$YEAR))
  rm(ig)
}

print("complete!")

fwrite(panel, paste0("~/data/projects/climate_displacement/raw/hh_raw_", region, ".csv.gz"))

print("exported!")

}
#* Section 2. Run the functions -----------------------

# Define counties
# read_hh_func(florida_affected_counties, "florida")
read_hh_func(monroe, 2012, 2019, "monroe")
