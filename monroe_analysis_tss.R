library(pacman)
p_load(
  dtplyr, tidyverse, fst, rlang, data.table, bit64, sf, tidycensus, tigris, beepr, rethnicity, ## Data Processing
  sjPlot, visdat, ggpubr, cowplot, scales, leaflet, htmlwidgets, scales, viridis, ## Visualization
  survey, estimatr, lm.beta, robustbase, rsample, caret, glmnet, recipes, vip, Metrics, MLmetrics,## Modeling
  modelsummary, stargazer, sjmisc # Export
)

####################################
# 1. Load Data
####################################

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

# Load household-level data for severely affected Florida counties
hh <- fread("output/hh_severe_counties_cleaned.csv.gz")
hh_miami <- fread("output/hh_severe_counties_cleaned.csv.gz")

####################################
# 2. Create predicted race variables
####################################

ethnicity <- read.csv("~/data/projects/climate_displacement/raw/ig_ethnicity.csv") %>%
    left_join(read.csv("~/data/projects/climate_displacement/raw/ig_race.csv"))

hh <- hh %>%
left_join(ethnicity %>% select(Ethnicity_Code_1 = Ethnicity, Race))

# Predict Race using `predictrace` package
# hh <- hh %>%
  # mutate(pred_race = predict_race(last_name_1)) %>%
  # unnest(pred_race) %>%
  # select(-c(name, match_name))

# Use `rethnicity` package instead
hh <- hh %>%
  rename(raw_race = Race) %>%
  # Filter to households that belonged to Monroe at least once during the study period
  group_by(FAMILYID) %>%
  filter(any(county_name == "Monroe County, FL")) %>%
  ungroup() %>%
  # Create new variables
  mutate(pred_race = predict_ethnicity(
    firstnames = first_name_1,
    lastnames = last_name_1,
    method = "fullname",
    threads = 0,
    na.rm = FALSE
))

hh <- hh %>%
  unnest(cols = c(pred_race))

# Calculate prob_race using apply to avoid rowwise()
prob_columns <- grep("^prob_", colnames(hh))

hh <- hh %>%
  mutate(prob_race = apply(hh[, prob_columns], 1, max, na.rm = TRUE)) %>%
  select(-starts_with("prob_")) %>%
  rename(pred_race = race)

hh %>%
  count(raw_race, pred_race) %>%
  arrange(desc(n)) %>%
  data.frame()

# 13.1% missing; 6.6% missing if including Data Axle's race imputation variable
sum(is.na(hh$raw_race))/nrow(hh) # 6.6% NA
sum(is.na(hh$pred_race))/nrow(hh) # 12.1% NA

############################################
# 3. Define migration using Chaeyeon's approach
############################################

# extract families that moved in 2018.
MOVEYEAR <- 2019
moved <- hh[(hh$LENGTH_OF_RESIDENCE == 1 & hh$YEAR == MOVEYEAR), ]
moved_familyID <- unique(moved$FAMILYID)  # 722,762 families that moved in 2018, 786,191 families combining 2017 & 2018

# how many of them have records in 2017?
moved <- hh[hh$YEAR == MOVEYEAR-1 & hh$FAMILYID %in% moved_familyID, ] # 281,536 families have records
moved2 <- hh[hh$GE_CENSUS_STATE_2010 == 12, ]

# how many of them lived in the highly affected counties?:
# Charlotte, Collier, Hillsborough, Lee, Manatee, Miami-Dade, Monroe, Pinellas and Sarasota
affected <- c(15, 21, 57, 71, 81, 86, 87, 103, 115)
moved2 <- moved2[moved2$GE_CENSUS_COUNTY %in% affected, ]
moved2_familyID <- unique(moved2$FAMILYID) # 205,941 families

# make one row for each family, generate 'moved_to'
moved2_FIPS <- hh[hh$FAMILYID %in% moved2_familyID, ] %>%
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

# Create a standardized key that ignores order
migration <- migration_1617
migration_df <- migration %>%
  mutate(Key = pmap_chr(list(Origin, Destination), ~ paste0(c(...), collapse = "-")))

data <- migration_1819

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
ggsave("plot/migration_summary.png")


#########################
# 4. Check sample size
########################

hh_miami_count <- hh_miami %>%
  # Filter to households that belonged to Monroe at least once during the study period
  group_by(FAMILYID) %>%
  filter(county_name == "Miami-Dade County, FL") %>%
  ungroup() %>%
  count(YEAR)

hh_count <- hh %>%
  filter(county_name == 'Monroe County, FL') %>%
  count(YEAR)

# Household number averages at 50-60k;
acs_hh_count <- get_acs(
  state = "FL",
  county = "Monroe",
  geography = "county",
  variable = "B11001_001E",
  geometry = FALSE,
  year = 2019,
  survey = "acs5")

# Household number averages at 50-60k;
acs_hh_count <- get_acs(
  state = "FL",
  county = "Miami",
  geography = "county",
  variable = "B11001_001E",
  geometry = FALSE,
  year = 2019,
  survey = "acs5")

# Check household distribution by cities
hh %>%
  count(CITY) %>%
  arrange(desc(n))

# Visualize location against a map
hh_monroe <- hh %>%
  filter(county_name == 'Monroe County, FL') %>%
  st_as_sf(coords = c("GE_LONGITUDE_2010", "GE_LATITUDE_2010"),
            crs = 4326)

# Import census places in Monroe
monroe_places <- places(year = 2019, state = "FL", cb = TRUE)
florida_counties <- counties(year = 2019, state = "FL", cb = TRUE)
monroe_places <- st_intersection(monroe_places, florida_counties %>% filter(NAME=="Monroe"))

# Create a color palette function based on the NAME column
pal <- colorFactor(topo.colors(length(unique(monroe_places$NAME))), monroe_places$NAME)

monroe_map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add base tiles
  addCircleMarkers(data = hh_monroe,
                   lng = ~st_coordinates(hh_monroe)[,1],
                   lat = ~st_coordinates(hh_monroe)[,2],
                   color = "blue", radius = 0.01, fillOpacity = 0.5, popup = ~paste("Lat:", st_coordinates(hh_monroe)[,2], "<br>Lon:", st_coordinates(hh_monroe)[,1])) %>%
  addPolygons(data = monroe_places %>%
    st_transform(crs = 4326),
              fillColor = ~pal(NAME),   # Use the color palette
              color = "black",          # Border color
              weight = 1,               # Border weight
              opacity = 1,              # Border opacity
              fillOpacity = 0.5,        # Fill opacity
              label = ~NAME)            # Add NAME as a label

# Save the interactive map as an HTML file
saveWidget(monroe_map, "plot/monroe_map.html")

# Check distribution by census tracts and cities
hh_monroe_tract <- hh_monroe %>%
  st_drop_geometry() %>%
  mutate(TRACT_FIPS = paste0(
    str_pad(GE_CENSUS_STATE_2010, 2, "left", "0"),
    str_pad(GE_CENSUS_COUNTY, 3, "left", "0"),
    str_pad(GE_ALS_CENSUS_TRACT_2010, 6, "left", "0")
    )) %>%
  group_by(YEAR, TRACT_FIPS) %>%
  summarize(raw_hh = n()) %>%
  ungroup() %>%
  group_by(TRACT_FIPS) %>%
  summarize(raw_hh = mean(raw_hh, na.rm=TRUE)) %>%
  ungroup()

acs_hh_count_tract <- get_acs(
  state = "FL",
  county = "Monroe",
  geography = "tract",
  variable = "B11001_001E",
  geometry = FALSE,
  year = 2019,
  survey = "acs5")

hh_monroe_tract_comb <- full_join(hh_monroe_tract,
  acs_hh_count_tract %>% select(GEOID, NAME, estimate),
  by = c("TRACT_FIPS" = "GEOID"))

tract_hh_plot <- hh_monroe_tract_comb %>%
  ggplot(aes(x = raw_hh, y = estimate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add y = x line
  stat_cor(method = "spearman", label.x = 1, label.y = 1) +
  labs(x = "Data Axle Count (2015-2019, mean average)",
       y = "ACS estimate (2015-2019)",
       title = "Monroe County Household Counts by Census Tracts (2015-2019)")

ggsave("plot/tract_hh_plot_monroe.png", tract_hh_plot)

# Check racial/ethnic distribution
acs_hh_race_tract <- get_acs(
  state = "FL",
  county = "Monroe",
  geography = "tract",
  variables = c(
    "Total" = "B03002_001",
    "White" = "B03002_003",
    "Black" = "B03002_004",
    "Asian" = "B03002_006",
    "Hispanic" = "B03002_012"),
  geometry = TRUE,
  year = 2019,
  survey = "acs5") %>%
select(GEOID, variable, estimate) %>%
pivot_wider(names_from = "variable", values_from = "estimate")

# Create tract-level maps of racial populations
map_race <- function(data, race_var, total_var, filename, plot_type = c("share", "absolute")) {
  plot_type <- match.arg(plot_type)

  data <- data %>%
    mutate(pct_race = !!sym(race_var) / !!sym(total_var),
           abs_race = !!sym(race_var))

  if (plot_type == "share") {
    plot <- data %>%
      ggplot() +
      geom_sf(aes(fill = pct_race)) +
      scale_fill_viridis(option = "viridis", labels = scales::percent_format()) +
      labs(fill = paste("Percentage", race_var)) +
      theme_minimal()
  } else {
    plot <- data %>%
      ggplot() +
      geom_sf(aes(fill = abs_race)) +
      scale_fill_viridis(option = "viridis") +
      labs(fill = paste("Absolute Count of", race_var)) +
      theme_minimal()
  }

  ggsave(filename, plot)
}

# Example data (replace with your actual data)
acs_hh_race_tract <- st_read("path_to_your_acs_hh_race_tract_data.geojson")

# Apply the function to create and save the plots for different race combinations and plot types
map_race(acs_hh_race_tract, "Hispanic", "Total", "plot/tract_pct_Hispanic_monroe.png", plot_type = "share")
map_race(acs_hh_race_tract, "Hispanic", "Total", "plot/tract_abs_Hispanic_monroe.png", plot_type = "absolute")

map_race(acs_hh_race_tract, "White", "Total", "plot/tract_abs_White_monroe.png", plot_type = "absolute")
map_race(acs_hh_race_tract, "Black", "Total", "plot/tract_pct_Black_monroe.png", plot_type = "share")
map_race(acs_hh_race_tract, "Asian", "Total", "plot/tract_abs_Asian_monroe.png", plot_type = "absolute")


####################################
# 5. Visualize post-migration destination
####################################

# Defining out-migration might need to be revised.

# Counties
hh <- hh %>%
mutate(COUNTY_FIPS = str_pad(COUNTY_FIPS, 5, "left", "0"),
  AFTER_COUNTY_FIPS = str_pad(AFTER_COUNTY_FIPS, 5, "left", "0")
  ) %>%
  mutate(after_category = case_when(
  MOVE_OUT==0 ~ NA_character_,
  MOVE_OUT==1 & COUNTY_FIPS == AFTER_COUNTY_FIPS ~ "A. Within-County",
  MOVE_OUT==1 & substr(COUNTY_FIPS, 1, 2) == substr(AFTER_COUNTY_FIPS, 1, 2) ~ "B. Within-State",
  MOVE_OUT==1 &  substr(COUNTY_FIPS, 1, 2) != substr(AFTER_COUNTY_FIPS, 1, 2) ~ "C. Out-State",
  TRUE ~ NA_character_
  ))

# 5-1. Grouped Destination #######

hh %>%
  filter(MOVE_IN==1) %>%
  count(LENGTH_OF_RESIDENCE) %>%
  data.frame()

hh %>%
  filter(LENGTH_OF_RESIDENCE >= 3 & MOVE_IN==1) %>%
  data.frame() %>%
  select(FAMILYID, YEAR, location, LENGTH_OF_RESIDENCE, MOVE_IN) %>%
  head()

hh %>%
  filter(FAMILYID %in% c("594284", "594551")) %>%
  select(FAMILYID, YEAR, location, LENGTH_OF_RESIDENCE, MOVE_IN)

# Define the function
destination_plot1 <- function(data, filter_expr, count_vars, aes_vars, labs_vars, filename_prefix, position = "identity", width = 8, height = 4, facet_var = NULL) {
  plot_data <- data %>%
    filter(!!parse_expr(filter_expr)) %>%
    count(!!!syms(count_vars)) %>%
    group_by(!!!syms(count_vars[1])) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  if (!is.null(facet_var)) {
    plot_data <- plot_data %>%
      group_by(!!!syms(c(facet_var, count_vars[1]))) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()
  }

  plot <- plot_data %>%
    ggplot(aes(!!!syms(aes_vars))) +
    geom_bar(stat = "identity", position = position) +
    labs(!!!labs_vars) +
    theme_bw()

  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(as.formula(paste(facet_var, "~ .")))
  }

  # Save plot
  ggsave(paste0("plot/", filename_prefix, ".png"), plot, width = width, height = height)
}

# Plot out-migration destination (counts)
destination_plot1(hh, "MOVE_OUT == 1", c("YEAR", "after_category"),
  c(x = "YEAR", y = "n", fill = "after_category"),
  list(x = "Year", y = "Count", fill = "Destination"),
  "bar_destination")

destination_plot1(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'", c("YEAR", "after_category"),
  c(x = "YEAR", y = "n", fill = "after_category"),
  list(x = "Year", y = "Count", fill = "Destination"),
  "bar_destination_monroe")

# Plot out-migration destination (shares)
destination_plot1(hh, "MOVE_OUT == 1", c("YEAR", "after_category"),
  c(x = "YEAR", y = "share", fill = "after_category"),
  list(x = "Year", y = "Share", fill = "Destination"),
  "bar_destination_shares", position = "fill")

destination_plot1(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'", c("YEAR", "after_category"),
  c(x = "YEAR", y = "share", fill = "after_category"),
  list(x = "Year", y = "Share", fill = "Destination"),
  "bar_destination_monroe_shares", position = "fill")

# Plot out-migration destinations by predicted racial group
destination_plot1(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'", c("YEAR", "after_category", "pred_race"),
  c(x = "YEAR", y = "n", fill = "after_category"),
  list(x = "Year", y = "Count", fill = "Destination"),
  "bar_destination_monroe_race", facet_var = "pred_race")

destination_plot1(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'", c("YEAR", "after_category", "pred_race"),
  c(x = "YEAR", y = "share", fill = "after_category"),
  list(x = "Year", y = "Share", fill = "Destination"),
  "bar_destination_monroe_shares_race", position = "fill", facet_var = "pred_race")

# Plot out-migration destinations by Data Axle racial group
destination_plot1(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'", c("YEAR", "after_category", "raw_race"),
  c(x = "YEAR", y = "n", fill = "after_category"),
  list(x = "Year", y = "Count", fill = "Destination"),
  "bar_destination_monroe_raw_race", facet_var = "raw_race")

destination_plot1(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'", c("YEAR", "after_category", "raw_race"),
  c(x = "YEAR", y = "share", fill = "after_category"),
  list(x = "Year", y = "Share", fill = "Destination"),
  "bar_destination_monroe_shares_raw_race", position = "fill", facet_var = "raw_race")


# 5-2. Destination patterns (high-foreign born vs. low foreign-born?) #######

# Check nativity distribution
acs_hh_fb_tract <- get_acs(
  state = "FL",
  county = "Monroe",
  geography = "tract",
  variables = c(
    "Total" = "B05012_001",
    "Foreign" = "B05012_003"),
  geometry = TRUE,
  year = 2019,
  survey = "acs5") %>%
select(GEOID, variable, estimate) %>%
pivot_wider(names_from = "variable", values_from = "estimate") %>%
mutate(`Foreign Born (%)` = Foreign/Total)

tract_fb_plot <- acs_hh_fb_tract %>%
  ggplot() +
  geom_sf(aes(fill = `Foreign Born (%)`)) +
      scale_fill_viridis(option = "viridis", labels = scales::percent_format()) +
      theme_minimal()

ggsave("plot/tract_pct_fb_monroe.png", tract_fb_plot)

# Join tract-level race and foreign born data
acs_tract <- acs_hh_race_tract %>%
  full_join(acs_hh_fb_tract %>% st_drop_geometry() %>% select(GEOID, Foreign, `Foreign Born (%)`),
    by = "GEOID")

hh <- hh %>%
  st_as_sf(coords = c("GE_LONGITUDE_2010", "GE_LATITUDE_2010"),
            crs = 4326) %>%
  st_join(st_transform(acs_tract, crs=4326))

hh %>%
  st_drop_geometry() %>%
  filter(county_name == 'Monroe County, FL') %>% # 274,647
  filter(!is.na(Total)) %>%
  count(YEAR)

# Define the function
destination_plot1 <- function(data, filter_expr, count_vars, aes_vars, labs_vars, filename_prefix, position = "identity", width = 8, height = 4, facet_var = NULL) {
  plot_data <- data %>%
    filter(!!parse_expr(filter_expr)) %>%
    count(!!!syms(count_vars)) %>%
    group_by(!!!syms(count_vars[1])) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  if (!is.null(facet_var)) {
    plot_data <- plot_data %>%
      group_by(!!!syms(c(facet_var, count_vars[1]))) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()
  }

  plot <- plot_data %>%
    ggplot(aes(!!!syms(aes_vars))) +
    geom_bar(stat = "identity", position = position) +
    labs(!!!labs_vars) +
    theme_bw()

  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(as.formula(paste(facet_var, "~ .")))
  }

  # Save plot
  ggsave(paste0("plot/", filename_prefix, ".png"), plot, width = width, height = height)
}

# Plot out-migration destination (counts)
destination_plot1(hh, "MOVE_OUT == 1 & `Foreign Born (%)` >= 0.3", c("YEAR", "after_category"),
  c(x = "YEAR", y = "share", fill = "after_category"),
  list(x = "Year", y = "Share", fill = "Destination"),
  "bar_destination_share_fb_30plus")

destination_plot1(hh %>% mutate(pct_Hispanic = Hispanic/Total),
  "MOVE_OUT == 1 & pct_Hispanic >= 0.3", c("YEAR", "after_category"),
  c(x = "YEAR", y = "share", fill = "after_category"),
  list(x = "Year", y = "Share", fill = "Destination"),
  "bar_destination_share_hispanic_30plus")

# Define migratino in an alternative way (Chayeon's approach)



# 5-3. Top States #######

# States plot function
create_and_save_state_plot <- function(data, filter_expr, filename, width = 8, height = 4) {
  plot <- data %>%
    filter(!!rlang::parse_expr(filter_expr)) %>%
    filter(!is.na(after_state_name)) %>%
    count(YEAR, after_state_name) %>%
    arrange(desc(n)) %>%
    mutate(share = n / sum(n, na.rm = TRUE)) %>%
    filter(after_state_name != "Florida") %>%
    head(30) %>%
    ggplot(aes(x = reorder(after_state_name, -share), y = share, fill = after_state_name)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_bw() +
    labs(x = "Moved-out State", y = "Households") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  ggsave(filename, plot, width = width, height = height)
}

# Apply the function to create and save the state plot
create_and_save_state_plot(hh, "MOVE_OUT == 1",
                          "plot/bar_destination_state.png")

create_and_save_state_plot(hh, "MOVE_OUT == 1 & county_name == 'Monroe County, FL'",
                     "plot/bar_destination_state_monroe.png")

# 5-3. Top Counties #######

plot <- hh_monroe %>%
  filter(MOVE_OUT==1) %>%
  filter(!is.na(after_county_name)) %>%
  count(after_state_name, AFTER_COUNTY_FIPS, after_county_name) %>%
  arrange(desc(n)) %>%
  filter(!AFTER_COUNTY_FIPS %in% c("12087", "12086", "12021")) %>%
  head(30) %>%
  ggplot(aes(x = reorder(county_name, -n), y = n, fill = state_name)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x="Moved-out county", y="Households") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("plot/bar_destination_monroe.png", plot2017, width=8, height=4)



############
# 4. Monroe neighborhoods
############

monroe_places <- places(year = 2016, state = "FL", cb = TRUE)
monroe_places <- st_intersection(monroe_places, florida_counties %>% filter(NAME=="Monroe"))

# Ensure the CRS is WGS84
monroe_places <- st_transform(monroe_places, crs = 4326)
florida_counties <- st_transform(florida_counties, crs = 4326)

# Clean Monroe county damage data
monroe_damage <- monroe_damage %>%
  mutate(NAME_cleaned = str_replace(Name, "\n", " "))

# Create a color palette function based on the NAME column
pal <- colorFactor(topo.colors(length(unique(monroe_places$NAME))), monroe_places$NAME)

# Create a base map with Leaflet
monroe_map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add base tiles
  addPolygons(data = florida_counties %>% filter(county_name == "Monroe"),
              color = "blue", weight = 2, fillOpacity = 0.1) %>%
  addPolygons(data = monroe_places,
              fillColor = ~pal(NAME),   # Use the color palette
              color = "black",          # Border color
              weight = 1,               # Border weight
              opacity = 1,              # Border opacity
              fillOpacity = 0.5,        # Fill opacity
              label = ~NAME)            # Add NAME as a label

# Save the interactive map as an HTML file
saveWidget(monroe_map, "plot/monroe_map.html")

# Which areas were impacted the most?
# Key Largo Kampground
# Calusa Campground

hh_monroe %>%
  filter(NAME=="Monroe") %>%
  count(CITY) %>%
  arrange(desc(n)) %>%
  data.frame()

severe_cities <- c("KEY WEST", "STOCK ISLAND", "BIG PINE KEY", "SUMMERLND KEY",
  "CUEJOE KEY", "KEY LARGO", "MARATHON", "ISLAMORADA", "TAVERNIER")

destination_2017 <- hh_monroe %>%
  filter(YEAR==2017 & MOVE_OUT==1 & CITY %in% severe_cities) %>%
  filter(!is.na(county_name)) %>%
  count(state_name, AFTER_COUNTY_FIPS, county_name) %>%
  arrange(desc(n))

hh_monroe %>%
  filter(MOVE_OUT==1 & CITY %in% severe_cities) %>%
  count(YEAR)

hh_monroe %>%
count(last_name_1) %>%
arrange(desc(n))

destination_2016 <- hh_monroe %>%
  filter(YEAR==2016 & MOVE_OUT==1 & CITY %in% severe_cities) %>%
  filter(!is.na(county_name)) %>%
  count(state_name, AFTER_COUNTY_FIPS, county_name) %>%
  arrange(desc(n))

plot2017 <- destination_2017 %>%
  head(30) %>%
  ggplot(aes(x = reorder(county_name, -n), y = n, fill = state_name)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x="Moved-out county", y="Households") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("plot/bar_destination_monroe_severe_2017.png", plot2017, width=8, height=4)

plot2016 <- destination_2016 %>%
  head(30) %>%
  ggplot(aes(x = reorder(county_name, -n), y = n, fill = state_name)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x="Moved-out county", y="Households") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("plot/bar_destination_monroe_severe_2016.png", plot2016, width=8, height=4)

# States

destination_state_2017 <- hh_monroe %>%
  filter(YEAR==2017 & MOVE_OUT==1 & CITY %in% severe_cities) %>%
  filter(!is.na(state_name)) %>%
  count(state_name) %>%
  arrange(desc(n)) %>%
  mutate(share = n/sum(n, na.rm=TRUE))

destination_state_2016 <- hh_monroe %>%
  filter(YEAR==2016 & MOVE_OUT==1 & CITY %in% severe_cities) %>%
  filter(!is.na(state_name)) %>%
  count(state_name) %>%
  arrange(desc(n)) %>%
  mutate(share = n/sum(n, na.rm=TRUE))

plot2017 <- destination_state_2017 %>%
  filter(state_name!="Florida") %>%
  head(30) %>%
  ggplot(aes(x = reorder(state_name, -share), y = share, fill = state_name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label=scales::percent_format()) +
  theme_bw() +
  labs(x="Moved-out county", y="Households") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("plot/bar_destination_monroe_severe_state_2017.png", plot2017, width=8, height=4)

plot2016 <- destination_state_2016 %>%
  filter(state_name!="Florida") %>%
  head(30) %>%
  ggplot(aes(x = reorder(state_name, -share), y = share, fill = state_name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label=scales::percent_format()) +
  theme_bw() +
  labs(x="Moved-out county", y="Households") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("plot/bar_destination_monroe_severe_state_2016.png", plot2016, width=8, height=4)

