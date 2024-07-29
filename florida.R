# Load Libraries
library(pacman)
p_load(dplyr, nnet, ggplot2, effects, broom, stringr, purrr, tidyr, fst,
      rethnicity, dtplyr,
       car, fastDummies, data.table, tidycensus, ggplot2, scales, tigris, sf, reshape2)

# Set working directory
if(dir.exists("/accounts/projects/timthomas/udp")) {
  setwd("~/data/projects/climate_displacement")
} else if(dir.exists("/Users/taesoo/Documents")) {
  setwd("/Users/taesoo/Documents/Projects/post-hurricane-migration")
} else {# Add Chaeyeon's directory here
}

# load migration data
florida <- fread("hh_raw_florida.csv.gz") # Changed from `.csv` file for faster access
# temp <- florida
# florida <- temp # Unnecessary steps?

################################
# Add predicted race variables
################################

# Load Data Axle's ethnicity-race code combinations
ethnicity <- read.csv("~/data/projects/climate_displacement/raw/ig_ethnicity.csv") %>%
    left_join(read.csv("~/data/projects/climate_displacement/raw/ig_race.csv"))

florida <- florida %>%
  left_join(ethnicity %>% select(Ethnicity_Code_1 = Ethnicity, raw_race = Race))

unique_names <- florida %>%
  distinct(first_name_1, last_name_1)

nrow(unique_names)/nrow(florida) # Obsservations reduced to 20%

# Extract unique name combinations
unique_names <- unique_names %>%
  # Create new variables
  mutate(pred_race = predict_ethnicity(
    firstnames = first_name_1,
    lastnames = last_name_1,
    method = "fullname",
    threads = 0,
    na.rm = FALSE)) %>%
  unnest(cols = c(pred_race))

# Calculate prob_race using apply to avoid `rowwise` operations for shorter processing
prob_columns <- grep("^prob_", colnames(unique_names))

unique_names <- unique_names %>%
  mutate(pred_race_probability = apply(unique_names[, prob_columns], 1, max, na.rm = TRUE)) %>%
  select(-starts_with("prob_")) %>%
  rename(pred_race = race)

florida <- florida %>%
  left_join(unique_names %>%
    select(first_name_1, last_name_1, pred_race, pred_race_probability),
    by = c("first_name_1", "last_name_1"))

nrow(florida) # 21,215,272

# Check distribution of the race variables
florida %>%
  count(raw_race, pred_race) %>%
  arrange(desc(n)) %>%
  data.frame()

################################
# Migration Patterns - Chaeyeon ver.
################################

MOVEYEAR = 2019
florida <- florida[florida$YEAR > MOVEYEAR-2, ]
florida <- florida[florida$YEAR < MOVEYEAR+1, ]

# write.csv(florida, file = "hh_florida_2016-2018.csv", row.names = FALSE)

# extract families that moved in 2018.
moved <- florida[(florida$LENGTH_OF_RESIDENCE == 1 & florida$YEAR == MOVEYEAR), ]
moved_familyID <- unique(moved$FAMILYID)  # 724,998 families that moved in 2018, 786,191 families combining 2017 & 2018
# move_2018 <- florida[florida$FAMILYID %in% moved_2018_familyID, ] # 1,131,163 records of the moved families, 1,053,592 records

# how many of them have records in 2017?
moved <- florida[florida$YEAR == MOVEYEAR-1 & florida$FAMILYID %in% moved_familyID, ] # 281,536 families have records
moved2 <- moved[moved$GE_CENSUS_STATE_2010 == 12, ] 
  # 238,215 families have lived in florida in 2016 or 2017 and moved in 2018.

# how many of them lived in the highly affected counties?: 
# Charlotte, Collier, Hillsborough, Lee, Manatee, Miami-Dade, Monroe, Pinellas and Sarasota
affected <- c(15, 21, 57, 71, 81, 86, 87, 103, 115)
moved2 <- moved2[moved2$GE_CENSUS_COUNTY %in% affected, ]
moved2_familyID <- unique(moved2$FAMILYID) # 205,941 families

# make one row for each family, generate 'moved_to' 
moved2_FIPS <- florida[florida$FAMILYID %in% moved2_familyID, ] %>%
  mutate(
    FIPS = sprintf("%02d%03d", GE_CENSUS_STATE_2010, GE_CENSUS_COUNTY)
  ) %>% dplyr::select(FAMILYID, FIPS, YEAR)

moved2_wide <- moved2_FIPS %>%
  mutate(YEAR = as.character(YEAR)) %>%  # Ensure YEAR is a character string, if not already
  pivot_wider(
    names_from = YEAR,
    values_from = FIPS,
    names_prefix = "FIPS_",
    values_fill = list(FIPS = NA)  # Fill missing values with NA
  ) 

# generate an O-D (county-level) 
# Creating a new dataframe for migration tracking
migration <- moved2_wide %>%
  mutate(
    # Destination = if_else(is.na(FIPS_2018), FIPS_2019, FIPS_2018)
    Destination = FIPS_2019
  ) %>%
  group_by(FIPS_2018, Destination) %>%
  summarize(Migrants = n(), .groups = "drop") %>%
  filter(!is.na(Destination)) %>%
  rename(Origin = FIPS_2018, Destination = Destination)

migration_1516 <- migration
migration_1617 <- migration
migration_1718 <- migration
migration_1819 <- migration

################################
# Migration Patterns - Taesoo ver.
################################

florida_move <- florida %>% #mutate(FAMILYID, YEAR) %>%
  dplyr::select(FAMILYID, YEAR, everything()) %>%
  lazy_dt() %>%
  arrange(FAMILYID, YEAR) %>%
  group_by(FAMILYID) %>%
  mutate(MOVE_OUT = ifelse(lead(GE_LONGITUDE_2010) != GE_LONGITUDE_2010, 1, 0),
         MOVE_IN = ifelse(lag(GE_LONGITUDE_2010) != GE_LONGITUDE_2010, 1, 0),
         ################################
         ## Move within county
         # MOVE_OUT_County = ifelse(MOVE_OUT == 1 & lead(substr(GEOID, 1, 5)) == substr(GEOID, 1, 5), 1, 0),
         # MOVE_IN_County = ifelse(MOVE_IN == 1 & lag(substr(GEOID, 1, 5)) == substr(GEOID, 1, 5), 1, 0),
         ## Move outside county, within state
         # MOVE_OUT_State = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & lead(substr(GEOID, 1, 2)) == substr(GEOID, 1, 2), 1, 0),
         # MOVE_IN_State = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & lag(substr(GEOID, 1, 2)) == substr(GEOID, 1, 2), 1, 0),
         ## Move outside state
         # MOVE_OUT_National = ifelse(MOVE_OUT == 1 & MOVE_OUT_County == 0 & MOVE_OUT_State == 0, 1, 0),
         # MOVE_IN_National = ifelse(MOVE_IN == 1 & MOVE_IN_County == 0 & MOVE_IN_State == 0, 1, 0)
         ################################
  ) %>% ungroup() %>%
  arrange(FAMILYID) %>% 
  mutate(FIPS = sprintf("%02d%03d", GE_CENSUS_STATE_2010, GE_CENSUS_COUNTY)) %>%
  as_tibble()

# temp <- florida_move[0:20, ]

# Save the data for faster access
# fwrite(florida_move, "hh_florida_move.csv.gz")

########################
# Start from here for faster access
########################

florida_move <- fread("hh_florida_move.csv.gz")

########################
# Data Validation
########################

share_da <- florida_move %>%
  lazy_dt() %>%
  mutate(FIPS = str_pad(FIPS, 5, "left", "0")) %>%
  filter(substr(FIPS, 1, 2)=="12") %>%
  group_by(YEAR, FIPS) %>%
  summarise(total_hh = n(),
            renter_occupied = sum(OWNER_RENTER_STATUS < 7, na.rm=TRUE),
            hh_white = sum(pred_race == "white", na.rm=TRUE),
            hh_black = sum(pred_race == "black", na.rm=TRUE),
            hh_asian = sum(pred_race == "asian", na.rm=TRUE),
            hh_hispanic = sum(pred_race == "latinx", na.rm=TRUE),
            hh_other = sum(pred_race == "other", na.rm=TRUE)) %>%
  ungroup() %>%
  as_tibble()

share_da %>%
  filter(FIPS == "12087")

acs_vars <- get_acs(geography = "county",
  variables = c("total_hh" = "B25003_001E", # Excluding group quarters
                # "total_hh" = "B09019_001E", # Including group quarters
                "renter_hh" = "B25003_003E",
                "hh_total" = "B25006_001E",
                "hh_black" = "B25006_003E",
                "hh_asian" = "B25006_005E"
                ),
                             state = "FL",
                             county = c("Charlotte", "Collier", "Hillsborough", "Lee",
                                      "Manatee", "Miami-Dade", "Monroe", "Pinellas", "Sarasota"),
                             year = 2019,
                             survey = "acs5",
                             output = "wide",
                             geometry = FALSE) %>%
  mutate(share_renter = renter_hh /total_hh,
         share_black = hh_black/hh_total,
         share_asian = hh_asian/hh_total
    ) %>%
  select(-matches("M$", ignore.case = FALSE))

share_comb <- share_da %>%
  filter(YEAR >= 2015 & YEAR <= 2019) %>%
  group_by(FIPS) %>%
  summarise(renter_occupied = mean(renter_occupied, na.rm=TRUE),
            mean_hh = mean(total_hh, na.rm=TRUE),
            total_hh = mean(total_hh, na.rm=TRUE),
            hh_black = mean(hh_black, na.rm=TRUE),
            hh_asian = mean(hh_asian, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(share_renter = renter_occupied/total_hh,
         share_black = hh_black/total_hh,
         share_asian = hh_asian/total_hh
    ) %>%
  right_join(acs_vars,
    by = c("FIPS" = "GEOID"),
    suffix = c("_da", "_acs"))

# Total number of households
plot <- share_comb %>%
  ggplot(aes(x = total_hh_acs, y = mean_hh)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") + # Identity line
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Regression line
  labs(title = "Total number of households by census tract (2006-2010)",
       x = "Total Households ACS",
       y = "Total Households DA")

# Calculate R-squared value
model <- lm(mean_hh ~ total_hh_acs, data = share_comb)
r_squared <- summary(model)$r.squared

# Add R-squared value to the plot
plot <- plot +
  annotate("text", x = Inf, y = -Inf, label = paste("R² =", round(r_squared, 2)),
           hjust = 1.1, vjust = -0.5, size = 5, color = "black")

ggsave("plot/validation_total_hh_2006_2010.png", plot)

# Renter householder (%)
plot <- share_comb %>%
  ggplot(aes(x = share_renter_acs, y = share_renter_da)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") + # Identity line
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Regression line
  labs(title = "Share of renter households (2006-2010)",
       x = "Share Renter ACS",
       y = "Share Renter DA")

# Calculate R-squared value
model <- lm(share_renter_da ~ share_renter_acs, data = share_comb)
r_squared <- summary(model)$r.squared

# Add R-squared value to the plot
plot <- plot +
  annotate("text", x = Inf, y = -Inf, label = paste("R² =", round(r_squared, 2)),
           hjust = 1.1, vjust = -0.5, size = 5, color = "black")

ggsave("plot/validation_pct_renter_2006_2010.png", plot)

# Black householder (%)
plot <- share_comb %>%
  ggplot(aes(x = share_black_acs, y = share_black_da)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") + # Identity line
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Regression line
  labs(title = "Share of Black households (2006-2010)",
       x = "Share Black ACS",
       y = "Share Black DA")

# Calculate R-squared value
model <- lm(share_black_da ~ share_black_acs, data = share_comb)
r_squared <- summary(model)$r.squared

# Add R-squared value to the plot
plot <- plot +
  annotate("text", x = Inf, y = -Inf, label = paste("R² =", round(r_squared, 2)),
           hjust = 1.1, vjust = -0.5, size = 5, color = "black")

ggsave("plot/validation_pct_black_2006_2010.png", plot)


############
# Calculate migration
############

# load FEMA affected counties data
ia <- read.csv("fema-ia.csv")
ia <- ia %>% mutate(FIPS2 = sprintf("12%03d", FIPS))

# Choice of Highly Affected Counties
affected9 <- c("12015", "12021", "12057", "12071", "12081", "12086", "12087", "12103", "12115")
affected6 <- c("12021", "12086", "12009", "12051", "12043", "12087", "12097")
affected48 <- ia$FIPS2
affected2 <- c("12021", "12086")
monroe <- c("12087")

affected <- affected6

# how many of them lived in the highly affected counties in MOVEYEAR and moved out?
get_migration <- function(MOVEYEAR, FIPS_IN, FIPS_OUT) {

  # Subset the data to:
    # (1) Affected counties
    # (2) A certain year
    # (3) Households that have been coded to moved out
  mg <- florida_move[florida_move$FIPS %in% affected & florida_move$YEAR == MOVEYEAR & florida_move$MOVE_OUT == 1, ]
  # Get the FAMILYID of those households
  mg_familyID <- unique(mg$FAMILYID) # 180687 families
  
  # temp <- mg[1:20, ]
  # make one row for each family, generate 'moved_to'
  moved2_FIPS <- florida_move[florida_move$FAMILYID %in% mg_familyID, ] %>%
    mutate(
      FIPS = sprintf("%02d%03d", GE_CENSUS_STATE_2010, GE_CENSUS_COUNTY)
    ) %>% dplyr::select(FAMILYID, FIPS, YEAR) 
  
  moved2_wide <- moved2_FIPS %>%
    mutate(YEAR = as.character(YEAR)) %>%  # Ensure YEAR is a character string, if not already
    pivot_wider(
      names_from = YEAR,
      values_from = FIPS,
      names_prefix = "FIPS_",
      values_fill = list(FIPS = NA)  # Fill missing values with NA
    ) 
  print(names(moved2_wide))
  # generate an O-D (county-level) 
  # Creating a new dataframe for migration tracking
  migration <- moved2_wide %>%
    mutate(Destination = !!as.name(FIPS_OUT)) %>%
    group_by(!!as.name(FIPS_IN), Destination) %>%
    summarize(Migrants = n(), .groups = "drop") %>%
    filter(!is.na(Destination)) %>%
    dplyr::rename(Origin = !!as.name(FIPS_IN), Destination = Destination)
  
  return(migration)
}

migration_1516 <- get_migration(2015, "FIPS_2015", "FIPS_2016")
migration_1617 <- get_migration(2016, "FIPS_2016", "FIPS_2017")
migration_1718 <- get_migration(2017, "FIPS_2017", "FIPS_2018")
migration_1819 <- get_migration(2018, "FIPS_2018", "FIPS_2019")

################################
# Family Ties
################################

# Set working directory
if(dir.exists("/accounts/projects/timthomas/udp")) {
  ties <- read.csv("raw/2019_total.csv")
} else if(dir.exists("/Users/taesoo/Documents")) {
  ties <- read.csv("raw/2019_total.csv")
} else {# Add Chaeyeon's directory here
ties <- read.csv("raw/2019_total.csv")
}

# load social ties data
ties <- ties %>% mutate(poi_bg = sprintf("%05d", poi_cbg))

# select only related areas
ties <- ties[(ties$home_bg %in% affected) | (ties$poi_bg %in% affected), ]

# create keys "county-county" pair
ties_df <- ties %>%
  mutate(Key = pmap_chr(list(home_bg, poi_bg), ~ paste0(sort(c(...)), collapse = "-"))) %>% 
  dplyr::select(Key, ratio) %>%
  group_by(Key) %>%
  summarise(ratio = sum(ratio)) %>%
  ungroup()

get_result <- function(migration) { 
  migration_df <- migration %>%
    mutate(Key = pmap_chr(list(Origin, Destination), ~ paste0(sort(c(...)), collapse = "-")))
  
  # Join the dataframes based on the Key (migration_df <- ties_df)
  result_df <- migration_df %>%
    left_join(ties_df %>% dplyr::select(Key, ratio), by = "Key") %>%
    dplyr::select(-Key)  # Optionally remove the Key column if no longer needed
  
  print(dim(result_df)[1])
  print(sum(is.na(result_df$ratio)))
  
  result_df <- result_df[!is.na(result_df$ratio), ]
  return(result_df)
}

result_1516 <- get_result(migration_1516)
result_1617 <- get_result(migration_1617)
result_1718 <- get_result(migration_1718)
result_1819 <- get_result(migration_1819)


################################
# Regression
################################

# delete NAs
final_df <- result_1819

final_df <- final_df %>% 
  mutate(
    Q1 = quantile(Migrants, 0.25),
    Q3 = quantile(Migrants, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Migrants >= lower_bound & Migrants <= upper_bound) %>%
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) # Remove additional columns

final_df <- final_df %>% 
  mutate(
    Q1 = quantile(ratio, 0.25),
    Q3 = quantile(ratio, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(ratio >= lower_bound & ratio <= upper_bound) %>%
  dplyr::select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) # Remove additional columns

dim(final_df)

# Linear regression
mod <- lm(log(Migrants) ~ log(ratio), final_df)
summary(mod)

# Create a data frame of fitted values and residuals
residuals_df <- data.frame(Fitted = fitted(mod), Residuals = residuals(mod))

# Plot using ggplot2
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point() + # Adds points
  geom_hline(yintercept = 0, color = "red") + # Adds a horizontal line at y = 0_sf
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Plot") +
  theme_minimal() # A cleaner theme for the plot

# Delete Residual Outliers 
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(mod, las = 1) 

# save plot
ggsave("plot/mod.png")

par(opar)
#final_df[c(172, 187, 91), ]
outlier_indices <- c(32, 23, 4, 74, 169) # 2015-2016
outlier_indices <- c(32, 135, 30, 47) # 2018-2019
outlier_indices <- c(43, 35, 40, 33) # 2017-2018
outlier_indices <- c(31, 141, 37, 53) # 2018-2019

# Remove Outliers
clean_df <- final_df[-outlier_indices, ]

# Linear regression
mod <- lm(log(Migrants) ~ log(ratio), clean_df)
summary(mod)

p_load(hrbrthemes)
# linear trend + confidence interval
ggplot(clean_df, aes(x=Migrants, y=ratio)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
ggsave("plot/linear_trend_confidence_interval.png")

migration_1718 <- result_df # result_df does not exist

##############################
# Generate CSV for mapping
##############################

# Set working directory
if(dir.exists("/accounts/projects/timthomas/udp")) {
  county.sf <- read.csv('raw/county_latlon.csv')
} else if(dir.exists("/Users/taesoo/Documents")) {
  county.sf <- read.csv('raw/county_latlon.csv')
} else {# Add Chaeyeon's directory here
county.sf <- read.csv('../geojson/county_latlon.csv')
}

county.sf <- county.sf %>% 
  mutate(geoid =  sprintf("%05d", poi_cbg)) %>% dplyr::select(geoid, poi.lon, poi.lat)

mapcsv <- left_join(clean_df, county.sf, by = join_by("Origin" == "geoid")) 
mapcsv <- left_join(mapcsv, county.sf, by = join_by("Destination" == "geoid") )

cols <- c("Origin", "Destination", "Migrants", "Ratio", "O_lon", "O_lat", "D_lon", "D_lat")
names(mapcsv) <- cols

# Set working directory
if(dir.exists("/accounts/projects/timthomas/udp")) {
  write.csv(mapcsv, file = 'florida2019.csv', row.names = F)
} else if(dir.exists("/Users/taesoo/Documents")) {
  write.csv(mapcsv, file = 'florida2019.csv', row.names = F)
} else {
  write.csv(mapcsv, file = './florida2019.csv', row.names = F)
}

##############################
# Data Comparison with ACS 2015-2019
##############################

# Set working directory
if(dir.exists("/accounts/projects/timthomas/udp")) {
  acs <- read.csv('raw/acs2015-2019.csv')
} else if(dir.exists("/Users/taesoo/Documents")) {
  acs <- read.csv('acs2015-2019.csv')
} else {
  acs <- read.csv('acs2015-2019.csv')
}

# drop NA values 
acs <- acs[complete.cases(acs[,c("current.state", "current.county", "origin.state", "origin.county")]),]

acs$current.state <- as.numeric(acs$current.state)

# acs <- acs %>% mutate(
#   Destination = sprintf("%02d%03d", current.state, current.county),
#   Origin = sprintf("%02d%03d", origin.state, origin.county)
# ) %>% dplyr::select(-current.state, -current.county, -origin.state, -origin.county)

acs <- acs %>%
mutate(
  Destination = paste0(
    str_pad(current.state, 2, "left", "0"),
    str_pad(current.county, 3, "left", "0")),
  Origin = paste0(
    str_pad(origin.state, 2, "left", "0"),
    str_pad(origin.county, 3, "left", "0"))
) %>% dplyr::select(-current.state, -current.county, -origin.state, -origin.county)

affected <- c("12015", "12021", "12057", "12071", "12081", "12086", "12087", "12103", "12115")
acs <- acs[acs$Origin %in% affected, ]

# Adding a Year column to each dataframe before combining
migration_1516$Year <- 2015
migration_1617$Year <- 2016
migration_1718$Year <- 2017
migration_1819$Year <- 2018

# Combine the dataframes
combined_df <- rbind(migration_1516, migration_1617, migration_1718, migration_1819)

# Calculate the average number of migrants for each Origin-Destination pair
average_migrants <- combined_df %>%
  group_by(Origin, Destination) %>%
  summarise(Average_Migrants = mean(Migrants, na.rm = TRUE))  # na.rm = TRUE to handle any NA values safely

# Monroe county is not present in the acs dataset
acs %>%
  count(Origin)

# Merging ACS and Axle
comparison_df <- merge(average_migrants, acs, by = c("Origin", "Destination")) # Why not joined?
comparison_df$Average_Migrants <- as.numeric(comparison_df$Average_Migrants)
comparison_df$Estimate <- as.numeric(comparison_df$Estimate)

comparison_df <- comparison_df[complete.cases(comparison_df[,c("Average_Migrants", "Estimate")]),]

# Linear regression
mod <- lm(Estimate ~ Average_Migrants, data = comparison_df)
summary(mod)

# Calculate residuals
residuals <- resid(mod)

# Identify outlier indices using the 2 or 3 standard deviations rule
outliers <- which(abs(residuals) > 3* sd(residuals))  # Change the multiplier as necessary

# Get row names of outliers
outlier_rows <- rownames(comparison_df)[outliers]

# Print row names
print(outlier_rows)

# Remove outliers
comparison_df_clean <- comparison_df[-outliers, ]
dim(comparison_df_clean)

# Optionally, refit the model with the cleaned data
model_clean <- lm(Estimate ~ Average_Migrants, data = comparison_df_clean)

# Create a data frame of fitted values and residuals
residuals_df <- data.frame(Fitted = fitted(model_clean), Residuals = residuals(model_clean))

# Plot using ggplot2
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point() + # Adds points
  geom_hline(yintercept = 0, color = "red") + # Adds a horizontal line at y = 0
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Plot") +
  theme_minimal() # A cleaner theme for the plot

ggsave("plot/residuals.png")

library(hrbrthemes)

# linear trend + confidence interval
comparison_df_charlotte <- comparison_df_clean[comparison_df_clean$Origin == "12015", ]

ggplot(comparison_df_clean, aes(x=Average_Migrants, y=Estimate)) +
  geom_point() +
  facet_wrap(~Origin, scales = "free") + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Data Axle vs. ACS Migrant Estimates",
       x = "Data Axle 2015-2019 Average Migrants",
       y = "ACS Migration Estimate") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  #xlim(0, 300) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

  ggsave("plot/linear_trend_confidence_interval2.png")

# Add predicted values to the dataframe
comparison_df_clean$Predicted_Estimate <- predict(mod, newdata = comparison_df_clean)

library(ggpmisc)
# Create the scatterplot of predicted values vs actual estimates
ggplot(comparison_df_clean, aes(x = Predicted_Estimate, y = Estimate)) +
  geom_point(alpha = 0.6, color = "black") +
  facet_wrap(~Origin, scales = "free") + 
  geom_abline(color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. ACS Migrant Estimates",
       x = "Predicted Migrant Estimate",
       y = "ACS Migration Estimate") +
  # xlim(0, 1500) + 
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plot/scatter_predicted_values_actual_estimates.png")

##############################
# Descriptive Stats for Migration 
##############################

get_mgtype <- function(data, year) {
  # Determine if migration is in-state or out-state
  data <- data %>%
    mutate(Migration_Type = case_when(
      Destination == Origin ~ 'within-county',
      # substr(Destination, 1, 2) == '12' & Destination %in% affected ~ 'within-affected', # Seems unnecessary
      substr(Destination, 1, 2) == '12' & !(Destination %in% affected) ~ 'In-State',
      substr(Destination, 1, 2) != '12' ~ 'Out-State'
    ))

  library(dplyr)

# Summary of migrants by type
migration_summary <- data %>%
  group_by(Origin, Migration_Type) %>%
  summarise(Total_Migrants = sum(Migrants), .groups = 'drop')

# Calculate total migrants for each Origin
total_migrants_by_origin <- migration_summary %>%
  group_by(Origin) %>%
  summarise(Total_Migrants_Origin = sum(Total_Migrants), .groups = 'drop')

# Join the total migrants by origin back to the migration summary
migration_summary <- migration_summary %>%
  left_join(total_migrants_by_origin, by = "Origin")

# Calculate proportions within each Origin
migration_summary <- migration_summary %>%
  mutate(Proportion = Total_Migrants / Total_Migrants_Origin * 100)

  # Bar plot for in-state vs out-state migration with proportions
  ggplot(migration_summary, aes(x = Migration_Type, y = Total_Migrants, fill = Migration_Type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Origin, scales = "free") +
    geom_text(aes(label = paste(Total_Migrants, " (", sprintf("%.2f%%", Proportion), ")", sep = "")), vjust = -0.1) +
    labs(title = "In-State vs. Out-State Migration", x = "Type of Migration", y = "Number of Migrants") +
    theme_minimal()
  
  migration_summary$Year <- year
  return(migration_summary)
}

mgtype_1516 <- get_mgtype(migration_1516, "2015-2016")
mgtype_1617 <- get_mgtype(migration_1617, "2016-2017")
mgtype_1718 <- get_mgtype(migration_1718, "2017-2018")
mgtype_1819 <- get_mgtype(migration_1819, "2018-2019")

# Combine the dataframes
combined_df <- rbind(mgtype_1516, mgtype_1617, mgtype_1718, mgtype_1819)

# Convert Migration_Type to a factor with desired order
combined_df$Migration_Type <- factor(combined_df$Migration_Type, levels = c("Out-State", "In-State", "within-affected", "within-county"))

combined_df <- combined_df %>%
  left_join(tigris::fips_codes %>%
    mutate(county_fips = paste0(state_code, county_code),
          origin_county = paste0(county, ", ", state)) %>%
    select(county_fips, origin_county),
    by = c("Origin" = "county_fips"))

# Create the stacked bar chart
ggplot(combined_df, aes(x = Year, y = Proportion, fill = Migration_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", Proportion)), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "Proportions by Migration Type",
       x = "Year",
       y = "Proportion") +
  theme_minimal() +
  facet_wrap(~origin_county, scales = "free") +
   theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_fill_manual(values = c("within-county" = "#addc91", 
                               "Out-State" = "#89b6b5",
                               "within-affected" = "#7eb7e8",
                               "In-State" = "#0072ce"))  # You can customize the colors as needed

ggsave("plot/stacked_bar_chart.png")

# Chaeyeon ver.
# 1516 87.88 12.12
# 1617 92.87 7.13
# 1718 85.62, 14.38
# 1819 92.33 7.67

# migration_data <- data.frame(
#   Year = 2015:2018,
#   InState_Proportion = c(87.88, 92.87, 85.62, 92.33),
#   OutState_Proportion = c(12.12, 7.13, 14.38, 7.67)
# )

combined_plot <- combined_df[combined_df$Migration_Type %in% c("In-State","Out-State"), ]
combined_plot <- combined_plot %>% group_by(Origin, Year) %>%
  summarise(out_affected = sum(Proportion)) %>%
  ungroup() %>%
  left_join(tigris::fips_codes %>%
    mutate(county_fips = paste0(state_code, county_code),
          origin_county = paste0(county, ", ", state)) %>%
    select(county_fips, origin_county),
    by = c("Origin" = "county_fips"))
  
# Creating the plot
ggplot(combined_plot, aes(x = Year, y = out_affected, group = origin_county, color = origin_county)) +
  geom_line(size = 1) +  # Draw a blue line
  geom_point(size = 2) +  # Add blue points at each data point
  geom_text(aes(label = sprintf("%.2f", out_affected)), vjust =-0.4) +  # Add labels above each point
  # scale_x_continuous(breaks = c("2015-2016", "2016-2017", "2017-2018", "2018-2019")) +  # Define x-axis breaks to show all years
  labs(title = "Yearly Proportion of Migration outside the affected counties",
       x = "Year",
       y = "Proportion (%)") +
  # facet_wrap(~origin_county, scales = "free") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12)  # Setting text size for better readability
  )
ggsave("plot/proportion_linegraph.png")

####################
# 2) Table of # of Migrants from each county. in-state and out-state
####################

# Combine the dataframes
# Adding a Year column to each dataframe before combining
migration_1516$Year <- 2015
migration_1617$Year <- 2016
migration_1718$Year <- 2017
migration_1819$Year <- 2018
combined_df <- rbind(migration_1516, migration_1617, migration_1718, migration_1819)

# Determine if migration is in-state or out-state
combined_df <- combined_df %>%
  mutate(Migration_Type = case_when(
    Destination == Origin ~ 'within-county',
    substr(Destination, 1, 2) == '12' & Destination %in% affected ~ 'within-affected', # Originally affected48
    substr(Destination, 1, 2) == '12' & !(Destination %in% affected) ~ 'In-State', # Originally affected48
    substr(Destination, 1, 2) != '12' ~ 'Out-State'
  )) %>%
  left_join(tigris::fips_codes %>%
    mutate(county_fips = paste0(state_code, county_code),
          origin_county = paste0(county, ", ", state)) %>%
    select(county_fips, origin_county),
    by = c("Origin" = "county_fips"))

# combined_df$County[combined_df$Origin == '12015'] <- "Charlotte"
# combined_df$County[combined_df$Origin == '12021'] <- "Collier"
# combined_df$County[combined_df$Origin == '12057'] <- "Hillsborough"
# combined_df$County[combined_df$Origin == '12071'] <- "Lee"
# combined_df$County[combined_df$Origin == '12081'] <- "Manatee"
# combined_df$County[combined_df$Origin == '12086'] <- "Miami-Dade"
# combined_df$County[combined_df$Origin == '12087'] <- "Monroe"
# combined_df$County[combined_df$Origin == '12103'] <- "Pinellas"
# combined_df$County[combined_df$Origin == '12115'] <- "Sarasota"

# combined_df$County[combined_df$Origin == '12009'] <- "Brevard"
# combined_df$County[combined_df$Origin == '12051'] <- "Hendry"
# combined_df$County[combined_df$Origin == '12043'] <- "Glades"
# combined_df$County[combined_df$Origin == '12097'] <- "Osceola"

county_stat <- combined_df %>%
  group_by(origin_county, Migration_Type, Year) %>%
  summarise(Total_Migrants = sum(Migrants))

# Calculate total migrations per origin and year
total_migrations <- county_stat %>%
  group_by(origin_county, Year) %>%
  summarise(Total = sum(Total_Migrants))

total_migrations2 <- county_stat %>%
  group_by(origin_county, Year) %>%
  summarise(Total = sum(Total_Migrants))

# Plotting the data
ggplot(total_migrations, aes(x = Year, y = Total, group = origin_county, color = origin_county)) +
  geom_line() +  # Line for each origin
  geom_point() +  # Points on each data point
  labs(title = "Total Migration by Origin and Year",
       x = "Year",
       y = "Number of Migrants",
       color = "Origin") +
  theme_minimal() +
  scale_y_continuous()  # Format y-axis as percentage

  ggsave("plot/total_migration1.png")

# Plotting the data
ggplot(total_migrations2, aes(x = Year, y = Total, group = origin_county, color = origin_county)) +
  geom_line() +  # Line for each origin
  geom_point() +  # Points on each data point
  geom_text(aes(label = Total),  vjust = -0.4, color = "black") +
  labs(title = "Total Migration by Origin and Year",
       x = "Year",
       y = "Number of Migrants") +
  theme_minimal() +
  scale_y_continuous() 

  ggsave("plot/total_migration2.png")

# Calculate in-state migrations
out_affected <- county_stat %>%
  filter(Migration_Type %in% c("In-State","Out-State")) %>%
  group_by(origin_county, Year) %>%
  summarise(out_affected_total = sum(Total_Migrants))

# Join to get full data with totals
migration_data <- merge(out_affected, total_migrations, by = c("origin_county", "Year"))

# Calculate the proportion of in-state migrations
migration_data$out_affected_prop <- 100 *(migration_data$out_affected_total / migration_data$Total)

# Plotting the data
ggplot(migration_data, aes(x = Year, y = out_affected_prop, group = origin_county, color = origin_county)) +
  geom_line() +  # Line for each origin
  geom_point() +  # Points on each data point
  geom_text(aes(label = sprintf("%.2f", out_affected_prop)), vjust =-0.4) +
  labs(title = "Migration Proportion to outside the affected counties by Origin and Year",
       x = "Year",
       y = "Proportion of Migrations",
       color = "Origin") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Format y-axis as percentage


ggsave("plot/in_state_migration.png")
