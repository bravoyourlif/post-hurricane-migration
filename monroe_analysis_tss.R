library(pacman)
p_load(
  dtplyr, tidyverse, fst, data.table, bit64, sf, tidycensus, tigris, beepr, predictrace, ## Data Processing
  sjPlot, visdat, ggpubr, cowplot, scales, leaflet, htmlwidgets, ## Visualization
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

# Load data
hh <- fread("output/hh_cleaned.csv.gz")

# Define hurricane-affected counties
affected <- c("Charlotte", "Collier", "Hillsborough", "Lee", "Manatee", "Miami-Dade", "Monroe", "Pinellas", "Sarasota" "Monroe")
severely_affected <- c("Monroe", "Collier", "Miami-Dade")

florida_counties <- counties(year = 2016, state = "FL", cb = TRUE)

####################################
# 2. Filter to severely affected counties
####################################

# Filter to severely affected counties only
hh <- hh %>%
  left_join(florida_counties %>%
    st_drop_geometry() %>%
    select(GEOID, NAME),
    by = c("COUNTY_FIPS" = "GEOID")) %>%
  filter(any(NAME %in% severely_affected))

florida_affected_counties_fips <- florida_counties %>%
  st_drop_geometry() %>%
  filter(grepl(paste0(severely_affected, collapse="|"), NAME)) %>%
  select(GEOID) %>%
  pull()

florida_counties_fips <- florida_counties %>%
  st_drop_geometry() %>%
  select(GEOID) %>%
  pull()

# Subset to severely affected areas
hh <- hh %>%
  group_by(FAMILYID) %>%
  filter(any(NAME %in% severely_affected)) %>%
  ungroup()

nrow(hh) # 11,234,459 (Florida total) -> 16,195,361

####################################
# 3. Identify out-migration patterns
####################################

# Create post-migration destination variables
hh <- hh %>%
mutate(COUNTY_FIPS = paste0(str_pad(GE_CENSUS_STATE_2010, 2, "left", "0"),
                         str_pad(GE_CENSUS_COUNTY, 3, "left", "0"))) %>%
  # Group by FAMILYID
  group_by(FAMILYID) %>%
  # Arrange by YEAR within each FAMILYID group
  arrange(FAMILYID, YEAR) %>%
  # Identify the first row where COUNTY_FIPS changes within each FAMILYID group
  mutate(AFTER_COUNTY_FIPS = lag(COUNTY_FIPS, default = first(COUNTY_FIPS))) %>%
  ungroup() %>%
left_join(fips_codes %>%
    mutate(GEOID = paste0(state_code, county_code),
          after_county_name = paste0(county, ", ", state)) %>%
    select(GEOID, after_county_name, after_state_name = state_name),
            by = c("AFTER_COUNTY_FIPS" = "GEOID"))


####################################
# 4. Create predicted race variables
####################################

# fwrite(hh, "output/hh_cleaned_severely_affected.csv.gz")

# Start from here
hh <- fread("output/hh_cleaned_severely_affected.csv.gz")

# Predict Race using `predictrace` package
hh <- hh %>%
  mutate(pred_race = predict_race(last_name_1)) %>%
  unnest(pred_race) %>%
  select(-c(name, match_name))

hh %>%
  count(likely_race)

# 13.1% missing; 5.3% missing if including Data Axle's race imputation variable
sum(is.na(hh$likely_race[hh$Race==""]))/nrow(hh)

####################################
# 5. Visualize post-migration destination
####################################

# Defining out-migration might need to be revised.

# Counties
hh <- hh %>%
mutate(COUNTY_FIPS = paste0(str_pad(GE_CENSUS_STATE_2010, 2, "left", "0"),
                         str_pad(GE_CENSUS_COUNTY, 3, "left", "0"))) %>%
  mutate(after_category = case_when(
  MOVE_OUT==0 ~ NA_character_,
  MOVE_OUT==1 & COUNTY_FIPS == AFTER_COUNTY_FIPS ~ "A. Within-County",
  MOVE_OUT==1 & substr(COUNTY_FIPS, 1, 2) == substr(AFTER_COUNTY_FIPS, 1, 2) ~ "B. Within-State",
  MOVE_OUT==1 &  substr(COUNTY_FIPS, 1, 2) != substr(AFTER_COUNTY_FIPS, 1, 2) ~ "C. Out-State",
  TRUE ~ NA_character_
  ))

# 5-1. Grouped Destination #######

# Define the function
create_and_save_plot <- function(data, filter_expr, count_vars, aes_vars, labs_vars, filename, position = "identity", width = 8, height = 4) {
  plot <- data %>%
    filter(!!rlang::parse_expr(filter_expr)) %>%
    count(!!!syms(count_vars)) %>%
    group_by(!!!syms(count_vars[1])) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(!!!rlang::syms(aes_vars))) +
    geom_bar(stat = "identity", position = position) +
    labs(!!!labs_vars) +
    theme_bw()

  ggsave(filename, plot, width = width, height = height)
}

# Apply the function to create and save the plots
create_and_save_plot(hh, "MOVE_OUT == 1", c("YEAR", "after_category"),
                     c(x = "YEAR", y = "n", fill = "after_category"),
                     list(x = "Year", y = "Count", fill = "Destination"),
                     "plot/bar_destination.png")

create_and_save_plot(hh, "MOVE_OUT == 1 & NAME == 'Monroe'", c("YEAR", "after_category"),
                     c(x = "YEAR", y = "n", fill = "after_category"),
                     list(x = "Year", y = "Count", fill = "Destination"),
                     "plot/bar_destination_monroe.png")

create_and_save_plot(hh, "MOVE_OUT == 1", c("YEAR", "after_category"),
                     c(x = "YEAR", y = "share", fill = "after_category"),
                     list(x = "Year", y = "Share", fill = "Destination"),
                     "plot/bar_destination_shares.png", position = "fill")

create_and_save_plot(hh, "MOVE_OUT == 1 & NAME == 'Monroe'", c("YEAR", "after_category"),
                     c(x = "YEAR", y = "share", fill = "after_category"),
                     list(x = "Year", y = "Share", fill = "Destination"),
                     "plot/bar_destination_monroe_shares.png", position = "fill")

# 5-2. Top States #######

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

create_and_save_state_plot(hh, "MOVE_OUT == 1 & NAME == 'Monroe'",
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
  addPolygons(data = florida_counties %>% filter(NAME == "Monroe"),
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

