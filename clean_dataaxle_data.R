library(pacman)
p_load(
  dtplyr, tidyverse, fst, data.table, bit64, sf, tidycensus, tigris, beepr, ## Data Processing
  sjPlot, visdat, ggpubr, cowplot, ## Visualization
  survey, estimatr, lm.beta, robustbase, rsample, caret, glmnet, recipes, vip, Metrics, MLmetrics,## Modeling
  modelsummary, stargazer, sjmisc # Export
)

# Clarify functions
summarize <- dplyr::summarize
lag <- dplyr::lag
lead <- dplyr::lead

# Set working directory
if(dir.exists("/accounts/projects/timthomas/udp")) {
  setwd("~/data/projects/climate_displacement")
} else if(dir.exists("/Users/taesoo/Documents")) {
  setwd("/Users/taesoo/Documents/Projects/post-hurricane-migration")
}

# Define hurricane-affected counties
affected <- c("Charlotte", "Collier", "Hillsborough", "Lee", "Manatee", "Miami-Dade", "Monroe", "Pinellas", "Sarasota")

florida_counties <- counties(year = 2016, state = "FL", cb = TRUE)

florida_affected_counties <- florida_counties %>%
  st_drop_geometry() %>%
  filter(grepl(paste0(affected, collapse="|"), NAME)) %>%
  select(GEOID) %>%
  pull()

florida_counties <- florida_counties %>%
  st_drop_geometry() %>%
  select(GEOID) %>%
  pull()

hh <- fread("raw/hh_raw_florida_all.csv.gz")

# Create migration variables
hh <- hh %>% 
    # Reorder variables and arrange by ID and YEAR
    select(FAMILYID, YEAR, everything()) %>%
    mutate(COUNTY_FIPS = paste0(GE_CENSUS_STATE_2010, str_pad(GE_CENSUS_COUNTY, 3, "left", "0"))) %>%
    arrange(FAMILYID, YEAR) %>%
    lazy_dt() %>%
    group_by(FAMILYID) %>%
    #group_by(FAMILYID) %>% 
    mutate(MOVE_OUT = ifelse(lead(GE_LONGITUDE_2010) != GE_LONGITUDE_2010, 1, 0),
           MOVE_IN = ifelse(lag(GE_LONGITUDE_2010) != GE_LONGITUDE_2010, 1, 0),
           ## Move within county
           MOVE_OUT_County = ifelse(MOVE_OUT == 1 & lead(COUNTY_FIPS) == COUNTY_FIPS, 1, 0),
           MOVE_IN_County = ifelse(MOVE_IN == 1 & lag(COUNTY_FIPS) == COUNTY_FIPS, 1, 0),
           ## Move outside county, within hurricane-affected-counties
           MOVE_OUT_NineCounty = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & lead(GE_CENSUS_STATE_2010) == GE_CENSUS_STATE_2010, 1, 0) & lead(COUNTY_FIPS) %in% florida_affected_counties,
           MOVE_IN_NineCounty = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & lag(GE_CENSUS_STATE_2010) == GE_CENSUS_STATE_2010, 1, 0) & lag(COUNTY_FIPS) %in% florida_affected_counties,
          ## Move outside county, within the state
          MOVE_OUT_State = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & lead(GE_CENSUS_STATE_2010) == GE_CENSUS_STATE_2010, 1, 0),
           MOVE_IN_State = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & lag(GE_CENSUS_STATE_2010) == GE_CENSUS_STATE_2010, 1, 0),
           ## Move outside state
           MOVE_OUT_National = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & MOVE_OUT_State == 0, 1, 0),
           MOVE_IN_National = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & MOVE_IN_State == 0, 1, 0)
    ) %>%
    ungroup() %>%
    data.frame()

# Load Infogroup race/ethnicity codes
# For UDP server:
if(dir.exists("/accounts/projects/timthomas/udp")) {
  ## For UDP server:
  ethnicity <- read_csv("~/data/projects/climate_displacement/raw/ig_ethnicity.csv") %>%
    left_join(read_csv("~/data/projects/climate_displacement/raw/ig_race.csv"))
}

hh <- hh %>% left_join(ethnicity %>% select(Ethnicity_Code_1 = Ethnicity, Race))

# write_fst(panel, "King.fst")

# Recode demographic variables
  hh <- hh %>%
  lazy_dt() %>%
  mutate(
    AGE = case_when(HEAD_HH_AGE_CODE == "A" ~ "<25",
                    HEAD_HH_AGE_CODE == "B" ~ "25-29",
                    HEAD_HH_AGE_CODE == "C" ~ "30-34",
                    HEAD_HH_AGE_CODE == "D" ~ "35-39",
                    HEAD_HH_AGE_CODE == "E" ~ "40-44",
                    HEAD_HH_AGE_CODE == "F" ~ "45-49",
                    HEAD_HH_AGE_CODE == "G" ~ "50-54",
                    HEAD_HH_AGE_CODE == "H" ~ "55-59",
                    HEAD_HH_AGE_CODE == "I" ~ "60-64",
                    HEAD_HH_AGE_CODE == "J" ~ "65+",
                    HEAD_HH_AGE_CODE == "K" ~ "65-69",
                    HEAD_HH_AGE_CODE == "L" ~ "70-74",
                    HEAD_HH_AGE_CODE == "M" ~ "75+"),
    AGE1 = case_when(AGE %in% c("25-29", "30-34") ~ "25-34",
                     AGE %in% c("35-39", "40-44") ~ "35-44",
                     AGE %in% c("45-49", "50-54") ~ "45-54",
                     AGE %in% c("55-59", "60-64") ~ "55-64",
                     AGE %in% c("65+", "65-69", "70-74", "75+") ~ "65+",
                     TRUE ~ AGE),
    AGE2 = case_when(
      AGE1 %in% c("<25") ~ "<25",
      AGE1 %in% c("25-34", "35-44") ~ "25-44",
      AGE1 %in% c("45-54", "55-64") ~ "45-64",
      AGE1 %in% c("65+") ~ "65+",
      TRUE ~ NA_character_),
    CHILDREN = ifelse(CHILDRENHHCOUNT > 0, "Has children", "No children"),
    TENURE = ifelse(OWNER_RENTER_STATUS >= 7, "Own", "Rent"),
    # Race = ifelse(Race == "latinx", "hispanic", Race)
    ) %>%
    rename(SEX = gender_1) %>%
    data.frame()

head(hh$Ethnicity_Code_1)

fwrite(hh, "output/hh_cleaned.csv.gz")
