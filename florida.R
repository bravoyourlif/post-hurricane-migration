# Load Libraries
library(pacman)
p_load(dplyr, nnet, ggplot2, effects, broom, stringr, purrr, tidyr, fst,
       car, fastDummies, data.table, tidycensus, ggplot2, scales, tigris, sf)

################################
# Migration Patterns
################################
# load migration data
florida <- read.csv("hh_raw_florida.csv")
temp <- florida
florida <- temp

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
# Family Ties
################################

# load social ties data
ties <- read.csv("../csv/florida2019.csv")

ties <- read.csv("../csv/2019_total.csv")

# Charlotte, Collier, Hillsborough, Lee, Manatee, Miami-Dade, Monroe, Pinellas and Sarasota
affected <- c("12015", "12021", "12057", "12071", "12081", "12086", "12087", "12103", "12115")
ties <- ties[(ties$county1 %in% affected) | (ties$county2 %in% affected), ]
ties <- ties[(ties$home_bg %in% affected) | (ties$poi_cbg %in% affected), ]

# Create a standardized key that ignores order
migration <- migration_1617
migration_df <- migration %>%
  mutate(Key = pmap_chr(list(Origin, Destination), ~ paste0(c(...), collapse = "-")))

ties_df <- ties %>%
  mutate(Key = pmap_chr(list(home_bg, poi_cbg), ~ paste0(sort(c(...)), collapse = "-")))
# mutate(Key = pmap_chr(list(county1, county2), ~ paste0(sort(c(...)), collapse = "-")))

# Join the dataframes based on the Key
result_df <- migration_df %>%
  left_join(ties_df %>% select(Key, ratio), by = "Key") %>%
  select(-Key)  # Optionally remove the Key column if no longer needed

sum(is.na(result_df$ratio))

################################
# Regression
################################

# delete NAs
result_df <- result_df[!is.na(result_df$ratio), ]
final_df <- result_df
# final_df <- result_df[result_df$Migrants > 2, ]
# final_df <- final_df[final_df$Destination > "12999" | final_df$Destination < "12000",]

#final_df <- final_df[final_df$Origin != final_df$Destination, ]

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

par(opar)
#final_df[c(172, 187, 91), ]
outlier_indices <- c(414, 4236, 3352) # 2015-2016
outlier_indices <- c(1179, 1149, 1156) # 2018-2019
outlier_indices <- c(2279, 1208, 462, 4508) # 2017-2018
outlier_indices <- c(4010, 1203, 927) # 2018-2019

# Remove Outliers
clean_df <- final_df[-outlier_indices, ]

# Linear regression
mod <- lm(log(Migrants) ~ log(ratio), clean_Df)
summary(mod)

library(hrbrthemes)
# linear trend + confidence interval
ggplot(clean_df, aes(x=Migrants, y=ratio)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()


migration_1718 <- result_df
migration_1617 

##############################
# Generate CSV for mapping
##############################
county.sf <- read.csv('../geojson/county_latlon.csv')
county.sf <- county.sf %>% 
  mutate(geoid =  sprintf("%05d", poi_cbg)) %>% dplyr::select(geoid, poi.lon, poi.lat)

mapcsv <- left_join(clean_df, county.sf, by = join_by("Origin" == "geoid")) 
mapcsv <- left_join(mapcsv, county.sf, by = join_by("Destination" == "geoid") )

cols <- c("Origin", "Destination", "Migrants", "Ratio", "O_lon", "O_lat", "D_lon", "D_lat")
names(mapcsv) <- cols

write.csv(mapcsv, file = './florida2019.csv', row.names = F)

##############################
# Data Comparison with ACS 2015-2019
##############################

acs <- read.csv('acs2015-2019.csv')
# drop NA values 
acs <- acs[complete.cases(acs[,c("current.state", "current.county", "origin.state", "origin.county")]),]

acs$current.state <- as.numeric(acs$current.state)

acs <- acs %>% mutate(
  Destination = sprintf("%02d%03d", current.state, current.county),
  Origin = sprintf("%02d%03d", origin.state, origin.county)
) %>% select(-current.state, -current.county, -origin.state, -origin.county)

affected <- c(12015, 12021, 12057, 12071, 12081, 12086, 12087, 12103, 12115)
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

# Merging ACS and Axle
comparison_df <- merge(average_migrants, acs, by = c("Origin", "Destination"))
comparison_df$Average_Migrants <- as.numeric(comparison_df$Average_Migrants)
comparison_df$Estimate <- as.numeric(comparison_df$Estimate)

comparison_df <- comparison_df[complete.cases(comparison_df[,c("Average_Migrants", "Estimate")]),]

# Linear regression
mod <- lm(Estimate ~ Average_Migrants, data = comparison_df)
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

par(opar)
#final_df[c(172, 187, 91), ]
outlier_indices <- c(2968, 2425, 1823, 1552, 1567) # 2017-2018

# Remove Outliers
comparison_df <- comparison_df[-outlier_indices, ]

library(hrbrthemes)
# linear trend + confidence interval
comparison_df_charlotte <- comparison_df[comparison_df$Origin == "12015", ]

ggplot(comparison_df, aes(x=Average_Migrants, y=Estimate)) +
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

# Add predicted values to the dataframe
comparison_df$Predicted_Estimate <- predict(mod, newdata = comparison_df)

library(ggpmisc)
# Create the scatterplot of predicted values vs actual estimates
ggplot(comparison_df, aes(x = Predicted_Estimate, y = Estimate)) +
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
##############################
# Descriptive Stats for Migration 
##############################

data <- migration_1819

# Load required libraries
library(ggplot2)
library(dplyr)
library(reshape2)

# Determine if migration is in-state or out-state
data$Migration_Type <- ifelse(substr(as.character(data$Origin), 1, 2) == substr(as.character(data$Destination), 1, 2), "In-State", "Out-State")

# Summary of migrants by type
migration_summary <- data %>%
  group_by(Migration_Type) %>%
  summarise(Total_Migrants = sum(Migrants))

# Calculate proportions
total_migrants <- sum(migration_summary$Total_Migrants)
migration_summary$Proportion <- migration_summary$Total_Migrants / total_migrants * 100


# 1) Bar plot for in-state vs out-state migration with proportions
####################
ggplot(migration_summary, aes(x = Migration_Type, y = Total_Migrants, fill = Migration_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(Total_Migrants, " (", sprintf("%.2f%%", Proportion), ")", sep = "")), vjust = -0.1) +
  labs(title = "In-State vs. Out-State Migration", x = "Type of Migration", y = "Number of Migrants") +
  theme_minimal()

# 1516 87.88 12.12
# 1617 92.87 7.13
# 1718 85.62, 14.38
# 1819 92.33 7.67

migration_data <- data.frame(
  Year = 2015:2018,
  InState_Proportion = c(87.88, 92.87, 85.62, 92.33),
  OutState_Proportion = c(12.12, 7.13, 14.38, 7.67)
)

# Creating the plot
ggplot(migration_data, aes(x = Year, y = InState_Proportion)) +
  geom_line(color = "hotpink", size = 1, linetype = "dashed") +  # Draw a blue line
  geom_point(color = "hotpink", size = 2) +  # Add blue points at each data point
  geom_text(aes(label = InState_Proportion), vjust =-0.4, color = "hotpink") +  # Add labels above each point
  scale_x_continuous(breaks = 2015:2018) +  # Define x-axis breaks to show all years
  labs(title = "Yearly Proportion of In-State Migration",
       x = "Year",
       y = "Proportion (%)") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12)  # Setting text size for better readability
  )
####################

# 2) Table of # of Migrants from each county. in-state and out-state
####################

# Determine if migration is in-state or out-state
combined_df$Migration_Type <- ifelse(substr(as.character(combined_df$Origin), 1, 2) == substr(as.character(combined_df$Destination), 1, 2), "In-State", "Out-State")
combined_df$County[combined_df$Origin == '12015'] <- "Charlotte"
combined_df$County[combined_df$Origin == '12021'] <- "Collier"
combined_df$County[combined_df$Origin == '12057'] <- "Hillsborough"
combined_df$County[combined_df$Origin == '12071'] <- "Lee"
combined_df$County[combined_df$Origin == '12081'] <- "Manatee"
combined_df$County[combined_df$Origin == '12086'] <- "Miami-Dade"
combined_df$County[combined_df$Origin == '12087'] <- "Monroe"
combined_df$County[combined_df$Origin == '12103'] <- "Pinellas"
combined_df$County[combined_df$Origin == '12115'] <- "Sarasota"

county_stat <- combined_df %>%
  group_by(County, Migration_Type, Year) %>%
  summarise(Total_Migrants = sum(Migrants))

# Calculate total migrations per origin and year
total_migrations <- county_stat %>%
  group_by(County, Year) %>%
  summarise(Total = sum(Total_Migrants))

total_migrations2 <- county_stat %>%
  group_by(Year) %>%
  summarise(Total = sum(Total_Migrants))

# Plotting the data
ggplot(total_migrations, aes(x = Year, y = Total, group = County, color = County)) +
  geom_line() +  # Line for each origin
  geom_point() +  # Points on each data point
  labs(title = "Total Migration by Origin and Year",
       x = "Year",
       y = "Number of Migrants",
       color = "Origin") +
  theme_minimal() +
  scale_y_continuous()  # Format y-axis as percentage

# Plotting the data
ggplot(total_migrations2, aes(x = Year, y = Total)) +
  geom_line() +  # Line for each origin
  geom_point() +  # Points on each data point
  labs(title = "Total Migration by Origin and Year",
       x = "Year",
       y = "Number of Migrants") +
  theme_minimal() +
  scale_y_continuous() 


# Calculate in-state migrations
in_state_migrations <- county_stat %>%
  filter(Migration_Type == "In-State") %>%
  group_by(County, Year) %>%
  summarise(InState_Total = sum(Total_Migrants))

# Join to get full data with totals
migration_data <- merge(in_state_migrations, total_migrations, by = c("County", "Year"))

# Calculate the proportion of in-state migrations
migration_data$InState_Prop <- migration_data$InState_Total / migration_data$Total

# Plotting the data
ggplot(migration_data, aes(x = Year, y = InState_Prop, group = County, color = County)) +
  geom_line() +  # Line for each origin
  geom_point() +  # Points on each data point
  labs(title = "In-State Migration Proportion by Origin and Year",
       x = "Year",
       y = "Proportion of In-State Migrations",
       color = "Origin") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Format y-axis as percentage


