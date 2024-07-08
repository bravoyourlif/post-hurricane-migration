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


hh <- fread("raw/hh_raw_florida.csv.gz")

nrow(hh) # 21,215,272

hh <- hh %>%
  filter(first_name!="")

# Reorder variables
hh <- hh %>% 
    # Reorder variables and arrange by ID and YEAR
    select(FAMILYID, PID, YEAR, everything()) %>%
    arrange(FAMILYID, PID, YEAR) %>%
    mutate(COUNTY_ID = str_pad(COUNTY_ID, 5, "left", "0"),
            TRACT_ID = str_pad(as.character(TRACT_ID), 11, "left", "0"))

# Some households, despite having the same lat/long,
# have length of residence values of 1 in two consecutive years
# E.g.) FAMILYID "47567965"
# Those households have not moved in the first instance

hh %>%
  filter(PID==1 & LENGTH_OF_RESIDENCE==1) %>%
  select(FAMILYID, YEAR, GE_LONGITUDE_2010, GE_LATITUDE_2010, LENGTH_OF_RESIDENCE) %>%
  head(10)

hh <- hh %>%
    lazy_dt() %>%
    mutate(location = paste(GE_LONGITUDE_2010, GE_LATITUDE_2010, sep = ", ")) %>%
    group_by(FAMILYID, PID) %>%
    mutate(
        # Determine if moved out by comparing next location
        MOVE_OUT = ifelse(lead(location, default = last(location)) != location, 1, 0),
        # Determine if moved in by comparing previous location
        MOVE_IN = ifelse(lag(location, default = first(location)) != location, 1, 0),

        # Move within county
        MOVE_OUT_County = ifelse(MOVE_OUT == 1 & lead(COUNTY_ID, default = last(COUNTY_ID)) == COUNTY_ID, 1, 0),
        MOVE_IN_County = ifelse(MOVE_IN == 1 & lag(COUNTY_ID, default = first(COUNTY_ID)) == COUNTY_ID, 1, 0),

        # Move outside county, within the state
        MOVE_OUT_State = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & substr(lead(COUNTY_ID, default = last(COUNTY_ID)), 1, 2) == substr(COUNTY_ID, 1, 2), 1, 0),
        MOVE_IN_State = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & substr(lag(COUNTY_ID, default = first(COUNTY_ID)), 1, 2) == substr(COUNTY_ID, 1, 2), 1, 0),

        # Move outside state
        MOVE_OUT_National = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & MOVE_OUT_State == 0, 1, 0),
        MOVE_IN_National = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & MOVE_IN_State == 0, 1, 0)
    ) %>%
    as_tibble()

# Recode demographic variables
hh <- hh %>%
  lazy_dt() %>%
  mutate(
    HH_AGE = case_when(HEAD_HH_AGE_CODE == "A" ~ "<25",
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
    HH_AGE1 = case_when(HH_AGE %in% c("25-29", "30-34") ~ "25-34",
                     HH_AGE %in% c("35-39", "40-44") ~ "35-44",
                     HH_AGE %in% c("45-49", "50-54") ~ "45-54",
                     HH_AGE %in% c("55-59", "60-64") ~ "55-64",
                     HH_AGE %in% c("65+", "65-69", "70-74", "75+") ~ "65+",
                     TRUE ~ HH_AGE),
    HH_AGE2 = case_when(
      HH_AGE1 %in% c("<25") ~ "<25",
      HH_AGE1 %in% c("25-34", "35-44") ~ "25-44",
      HH_AGE1 %in% c("45-54", "55-64") ~ "45-64",
      HH_AGE1 %in% c("65+") ~ "65+",
      TRUE ~ NA_character_),
    CHILDREN = ifelse(CHILDRENHHCOUNT > 0, "Has children", "No children"),
    TENURE = ifelse(OWNER_RENTER_STATUS >= 7, "Own", "Rent"),
    # Race = ifelse(Race == "latinx", "hispanic", Race)
    ) %>%
    rename(SEX = gender) %>%
    data.frame()

hh <- hh %>%
  left_join(fips_codes %>%
    mutate(GEOID = paste0(state_code, county_code),
          county_name = paste0(county, ", ", state)) %>%
    select(GEOID, county_name),
            by = c("COUNTY_ID" = "GEOID")) %>%
  group_by(FAMILYID) %>%
  # Arrange by YEAR within each FAMILYID group
  arrange(FAMILYID, YEAR) %>%
  # Identify the first row where COUNTY_FIPS changes within each FAMILYID group
  mutate(AFTER_COUNTY_ID = lag(COUNTY_ID, default = first(COUNTY_ID))) %>%
  ungroup() %>%
left_join(fips_codes %>%
    mutate(GEOID = paste0(state_code, county_code),
          after_county_name = paste0(county, ", ", state)) %>%
    select(GEOID, after_county_name, after_state_name = state_name),
            by = c("AFTER_COUNTY_ID" = "GEOID"))

hh <- hh %>%
   mutate(across(where(is.character), ~ na_if(., "")))

nrow(hh)
data.frame(head(hh))

fwrite(hh, "output/hh_monroe_cleaned.csv.gz")
