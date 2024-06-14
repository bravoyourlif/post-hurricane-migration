library(pacman)
p_load(
  dtplyr, tidyverse, fst, data.table, bit64, sf, tidycensus, tigris, beepr, ## Data Processing
  sjPlot, visdat, ggpubr, cowplot, scales, ## Visualization
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

# Load data
hh <- fread("output/hh_cleaned.csv.gz")

# Define hurricane-affected counties
affected <- c("Charlotte", "Collier", "Hillsborough", "Lee", "Manatee", "Miami-Dade", "Monroe", "Pinellas", "Sarasota" "Monroe")
severely_affected <- c("Monroe", "Collier", "Miami-Dade")
# affected <- "Monroe"

florida_counties <- counties(year = 2016, state = "FL", cb = TRUE)

hh <- hh %>%
  left_join(florida_counties %>%
    st_drop_geometry() %>%
    select(GEOID, NAME),
    by = c("COUNTY_FIPS" = "GEOID"))

florida_affected_counties <- florida_counties %>%
  st_drop_geometry() %>%
  filter(grepl(paste0(severely_affected, collapse="|"), NAME)) %>%
  select(GEOID) %>%
  pull()

florida_counties <- florida_counties %>%
  st_drop_geometry() %>%
  select(GEOID) %>%
  pull()

# Approach 1. Use `MOVE_IN_RATE`, `MOVE_OUT_RATE` variables

hh %>%
  filter(YEAR==2019) %>%
  head()

hh_sum1 <- hh %>%
  lazy_dt() %>%
  filter(COUNTY_FIPS %in% florida_counties) %>%
  group_by(COUNTY_FIPS, NAME, YEAR) %>%
  # Calculate county-level migration rates
  summarise(
    HH_OUT = sum(!is.na(MOVE_OUT)),
    HH_IN = sum(!is.na(MOVE_IN)),
    MOVE_OUT = sum(MOVE_OUT, na.rm = TRUE),
    MOVE_IN = sum(MOVE_IN, na.rm = TRUE),
    .groups = "drop"  # This ensures that the data is not grouped after summarise
  ) %>%
  mutate(
    MOVE_OUT_RATE = MOVE_OUT / HH_OUT,
    MOVE_IN_RATE = MOVE_IN / HH_IN
  ) %>%
   # mutate(
    # MOVE_OUT_RATE = tidyr::replace_na(MOVE_OUT_RATE, 0),
    # MOVE_IN_RATE = tidyr::replace_na(MOVE_IN_RATE, 0)
  # ) %>%
   mutate(treat = ifelse(COUNTY_FIPS %in% florida_affected_counties, "Affected", "Unaffected")) %>%
   data.frame()

# Create descriptive plots
plot1 <- hh_sum1 %>%
  pivot_longer(cols = c("MOVE_OUT_RATE", "MOVE_IN_RATE"),
              names_to = "Move",
              values_to = "Rate"
            ) %>%
  mutate(MOVE = ifelse(grepl("IN", Move), "Move-In", "Move-Out")) %>%
  ggplot() + aes(x=YEAR, y=Rate, group = Move, color=Move) +
  geom_point() + geom_line() +
  geom_vline(xintercept=2017, color = "blue", linetype="dotted") +
  geom_vline(xintercept=2018, color = "red", linetype="dotted") +
  scale_x_continuous(breaks = c(2010:2019)) +
  scale_y_continuous(label=scales::percent_format()) +
  facet_wrap(NAME~.) +
  theme_bw() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y="Move-in Rate")

ggsave(paste0("plot/descriptive_move_rate.png"), plot1, width = 10, height=8)

plot2 <- hh_sum1 %>%
  pivot_longer(cols = c("MOVE_OUT_RATE", "MOVE_IN_RATE"),
              names_to = "Move",
              values_to = "Rate"
            ) %>%
  mutate(MOVE = ifelse(grepl("IN", Move), "Move-In", "Move-Out")) %>%
  filter(NAME %in% affected) %>%
  ggplot() + aes(x=YEAR, y=Rate, group = Move, color=Move) +
  geom_point() + geom_line() +
  geom_vline(xintercept=2017, color = "blue", linetype="dotted") +
  geom_vline(xintercept=2018, color = "red", linetype="dotted") +
  scale_x_continuous(breaks = c(2010:2019)) +
  scale_y_continuous(label=scales::percent_format()) +
  facet_wrap(NAME~.) +
  theme_bw() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y="Move-in Rate")

ggsave(paste0("plot/descriptive_move_rate_affected.png"), plot2)


# Conduct a diff-in-diff analysis
hh_sum1 <- hh_sum1 %>%
    filter(YEAR!=2010) %>%
    mutate(pre_post_treatment = ifelse(YEAR < 2018, "Pre-Irma", "Post-Irma")) %>%
    # Recenter the year variable based on the treatment year (2017)
    mutate(YEAR = relevel(as.factor(YEAR), ref = as.character(2018-1)))


# Generate models  
model_in <- lm_robust(MOVE_IN_RATE ~ factor(YEAR) * as.factor(treat) + as.factor(COUNTY_FIPS),
                     data = hh_sum1, clusters = COUNTY_FIPS)

model_out <- lm_robust(MOVE_OUT_RATE ~ factor(YEAR) * as.factor(treat) + as.factor(COUNTY_FIPS),
                     data = hh_sum1, clusters = COUNTY_FIPS)
  

# Plot diff-in-diff results using a function
diff_plot_func <- function(model_df, type, title){

# Rearrange results
  matching_df <- tidy(model_df) %>%
    filter(str_detect(term, "treat") & str_detect(term, "YEAR")) %>%
    mutate(treat = str_extract(term, "Affected|Unaffected"),
    YEAR = as.character(str_extract(term, "-?\\d+"))) %>%
    mutate(legend = factor(ifelse(as.numeric(YEAR) >= as.numeric(2018-1), "Post-Irma", "Pre-Irma"),
                       levels = c("Pre-Irma", "Post-Irma")))

  # Dummy data for base year treatment effect
  base_year_df <- data.frame(
    YEAR = as.character(2018 - 1),
    estimate = 0,
    legend = "Pre-Irma"  # Assuming this is the right classification
  )

# Plotting
plot <- ggplot(matching_df, aes(x = YEAR, y = estimate)) +
    geom_point(aes(color = legend), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
        width = 0.1, position = position_dodge(0.1), alpha = 0.2) +
    geom_point(data = base_year_df, aes(color = legend), size = 3) +
    geom_vline(aes(xintercept = 2018 - 1), linetype = "dotted", color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    theme_minimal() +
    scale_x_discrete() +
    labs(title = title, y = "Group-Time Average Treatment Effect", x = "Year", color = "Period") +
    # scale_color_grey(start = 0.8, end = 0.6) +
    scale_color_manual(values = c("blue", "red")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold")) +
    scale_y_continuous(label = scales::percent_format())
  

ggsave(paste0("plot/diff_in_diff_", type, ".png"), plot)


}

# Run the plotting functions
diff_plot_func(model_in, "IN", "Diff-in-diff results for county-level move-in rates")
diff_plot_func(model_out, "OUT", "Diff-in-diff results for county-level move-out rates")


# Approach 2. Use `MOVE_IN`, `MOVE_OUT` variables (Counts)

hh_sum2 <- hh %>%
  lazy_dt() %>%
  filter(COUNTY_FIPS %in% florida_counties) %>%
  group_by(COUNTY_FIPS, NAME, YEAR) %>%
  # Calculate county-level migration rates
  summarise(
    MOVE_OUT = sum(MOVE_OUT, na.rm = TRUE),
    MOVE_IN = sum(MOVE_IN, na.rm = TRUE),
    .groups = "drop"  # This ensures that the data is not grouped after summarise
  ) %>%
  ungroup() %>%
  mutate(MOVE_IN = ifelse(YEAR==2010, NA_real_, MOVE_IN),
        MOVE_OUT = ifelse(YEAR==2019, NA_real_, MOVE_OUT),
        treat = ifelse(COUNTY_FIPS %in% florida_affected_counties, "Affected", "Unaffected")) %>%
   data.frame()

# Create descriptive plots
plot2 <- hh_sum2 %>%
  pivot_longer(cols = c("MOVE_OUT", "MOVE_IN"),
              names_to = "Move",
              values_to = "Count"
            ) %>%
  mutate(MOVE = ifelse(grepl("IN", Move), "Move-In", "Move-Out")) %>%
  ggplot() + aes(x=YEAR, y=Count, group = Move, color=Move) +
  geom_point() + geom_line() +
  geom_vline(xintercept=2017, color = "blue", linetype="dotted") +
  geom_vline(xintercept=2018, color = "red", linetype="dotted") +
  scale_x_continuous(breaks = c(2010:2019)) +
  scale_y_continuous(label= comma) +
  facet_wrap(NAME~., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y="Move Count")

ggsave(paste0("plot/descriptive_move_count.png"), plot2, width = 12, height=8)

plot2 <- hh_sum2 %>%
  pivot_longer(cols = c("MOVE_OUT", "MOVE_IN"),
              names_to = "Move",
              values_to = "Count"
            ) %>%
  mutate(MOVE = ifelse(grepl("IN", Move), "Move-In", "Move-Out")) %>%
  filter(NAME %in% affected) %>%
  ggplot() + aes(x=YEAR, y=Count, group = Move, color=Move) +
  geom_point() + geom_line() +
  geom_vline(xintercept=2017, color = "blue", linetype="dotted") +
  geom_vline(xintercept=2018, color = "red", linetype="dotted") +
  scale_x_continuous(breaks = c(2010:2019)) +
  scale_y_continuous(label= comma) +
  facet_wrap(NAME~., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y="Move Count")

ggsave(paste0("plot/descriptive_move_count_affected.png"), plot2)



# Conduct a diff-in-diff analysis
hh_sum2 <- hh_sum1 %>%
    filter(YEAR!=2010 & YEAR!=2019) %>%
    mutate(pre_post_treatment = ifelse(YEAR < 2018, "Pre-Irma", "Post-Irma")) %>%
    # Recenter the year variable based on the treatment year (2017)
    mutate(YEAR = relevel(as.factor(YEAR), ref = as.character(2018-1)))

# Generate models  
model_in2 <- lm_robust(MOVE_IN ~ factor(YEAR) * as.factor(treat) + as.factor(COUNTY_FIPS),
                     data = hh_sum1, clusters = COUNTY_FIPS)

model_out2 <- lm_robust(MOVE_OUT ~ factor(YEAR) * as.factor(treat) + as.factor(COUNTY_FIPS),
                     data = hh_sum1, clusters = COUNTY_FIPS)
  

# Plot diff-in-diff results using a function
diff_plot_func2 <- function(model_df, type, title){

# Rearrange results
  matching_df <- tidy(model_df) %>%
    filter(str_detect(term, "treat") & str_detect(term, "YEAR")) %>%
    mutate(treat = str_extract(term, "Affected|Unaffected"),
    YEAR = as.character(str_extract(term, "-?\\d+"))) %>%
    mutate(legend = factor(ifelse(as.numeric(YEAR) >= as.numeric(2018-1), "Post-Irma", "Pre-Irma"),
                       levels = c("Pre-Irma", "Post-Irma")))

  # Dummy data for base year treatment effect
  base_year_df <- data.frame(
    YEAR = as.character(2018 - 1),
    estimate = 0,
    legend = "Pre-Irma"  # Assuming this is the right classification
  )

# Plotting
plot <- ggplot(matching_df, aes(x = YEAR, y = estimate)) +
    geom_point(aes(color = legend), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
        width = 0.1, position = position_dodge(0.1), alpha = 0.2) +
    geom_point(data = base_year_df, aes(color = legend), size = 3) +
    geom_vline(aes(xintercept = 2018 - 1), linetype = "dotted", color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    theme_minimal() +
    scale_x_discrete() +
    labs(title = title, y = "Group-Time Average Treatment Effect", x = "Year", color = "Period") +
    # scale_color_grey(start = 0.8, end = 0.6) +
    scale_color_manual(values = c("blue", "red")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold")) +
    scale_y_continuous(label = scales::comma)
  

ggsave(paste0("plot/diff_in_diff2_", type, ".png"), plot)


}

# Run the plotting functions
diff_plot_func2(model_in2, "IN", "Diff-in-diff results for county-level move-in counts")
diff_plot_func2(model_out2, "OUT", "Diff-in-diff results for county-level move-out counts")
