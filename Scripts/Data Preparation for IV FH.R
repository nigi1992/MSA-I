### **Data Preparation for IV FH** ###

# 1. Data Import -------------------------------------------------------------

# Cleaning environment
#rm(list = ls())

#install.packages("geosphere")
library(geosphere)
library(splines)
library(tidyr)
#install.packages("reshape2")
library(reshape2)
#install.packages("dplyr")
library(dplyr)

# Importing the aggregated data set
install.packages("readxl")
library(readxl)

df1 <- read_excel("/Users/nicolaswaser/New-project-GitHub-first/R/MSA I/Input Data/Aggregate_Dataset_simple.xlsx")
head(df1)


# 2. Data Cleaning -------------------------------------------------------------

# Checking Data Types for all Variables
str(df1)

## Deleting whole rows of countries/territories with multiple missing values ('OWID_GZA', 'OWID_WBA', 'OWID_SML', 'OWID_ZAN', 'PSE')
install.packages("dplyr")
library(dplyr)

# Countries to remove
remove_countries <- c("OWID_GZA", "OWID_WBA", "OWID_SML", "OWID_ZAN", "PSE")

# Filtering out the rows
df1_clean <- df1 %>% filter(!iso3 %in% remove_countries)
head(df1_clean)

# Look for double entries in 'iso3', 'iso2', 'country_name', 'city_en'
df1_clean %>% 
  group_by(iso3, iso2, country_name, city_en) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Decided to keep FL, Mon, HK and Taiwan


# 3. Imputation of missing values --------------------------------------------

# Imputing missing values for Eritrea GDPpc 2020-2023
library(splines)
library(tidyr)
# Filtering for Eritrea
eritrea_data <- df1_clean %>% filter(iso3 == "ERI")

eritrea_data <- data.frame(
  year = 2015:2019,
  gdp  = c(1624.28, 1726.77, 1537.45, 1717.68, 1761.13)
)
all_years <- 2015:2023

# Performing spline interpolation/extrapolation for years 2020–2023
spline_interp <- spline(eritrea_data$year, eritrea_data$gdp, xout = all_years, method = "natural")

# Creating data frame from the interpolated values
eritrea_filled <- data.frame(
  year = spline_interp$x,
  gdp  = spline_interp$y
)
eritrea_filled
df1_impute <- df1_clean # Copying the data frame to keep the original data

install.packages("reshape2")
library(reshape2)
# Reshaping eritrea_filled to wide format
eritrea_filled_wide <- dcast(eritrea_filled, value.var = "gdp", formula = .~ year)
colnames(eritrea_filled_wide) <- c("delete", "GDPpc2015", "GDPpc2016", "GDPpc2017", "GDPpc2018", "GDPpc2019", "GDPpc2020", "GDPpc2021", "GDPpc2022", "GDPpc2023")
eritrea_filled_wide <- eritrea_filled_wide[, -1] # deleting unnecessary column "delete"
eritrea_filled_wide$iso3 <- "ERI" # Adding a column to identify Eritrea

# Updating df with the interpolated values
df1_impute[df1_impute$iso3 == "ERI", paste0("GDPpc", 2020:2023)] <- eritrea_filled_wide[eritrea_filled_wide$iso3 == "ERI", paste0("GDPpc", 2020:2023)]

# Finding duplicates & fixing dataset accordingly
dublicate <- duplicated(df1_impute$city_en)
df1_impute$city_en[dublicate]

dublicate <- duplicated(df1_impute$lat)
df1_impute$lat[dublicate]

dublicate <- duplicated(df1_impute$lon)
df1_impute$lon[dublicate]

dublicate <- duplicated(df1_impute$area)
range(df1_impute$area)
df1_impute$area[dublicate]


# 4. Data Frame for FH -------------------------------------------------------

df_fh <- df1_impute
unique(df_fh$FH_Scores)
table(df_fh$country_name)
str(df_fh)

# Removing columns 43 - 52 with v-dem scores
df_fh <- df_fh[, -c(43:52)]
table(!is.na(df_fh$FH_Scores))


# 5. CV: Diffusion Variable for FH--------------------------------------------------

install.packages("geosphere")
library(geosphere) # For calculating geographic distances
library(tidyverse)
# Extracting coordinates and country identifiers
coords_fh <- df_fh %>%
  select(country_name, lon, lat) %>%
  distinct()  # Ensuring unique country entries

# Creating a distance matrix (in kilometers)
distance_matrix_fh <- distm(
  x = coords_fh[, c("lon", "lat")], 
  fun = distHaversine  # Uses Haversine formula for Earth distances
) / 1000  # Converting meters to kilometers

# Computing inverse distance weights (excluding self-distance)
inverse_distance_fh <- 1 / (distance_matrix_fh + 1)  # Adding 1 to avoid division by zero
diag(inverse_distance_fh) <- 0  # Setting diagonal (self-distance) to 0

# Defining time frame for diffusion variable
years_fh <- 2015:2023

# Reshape df_fh from wide to long format
df_fh_long <- df_fh %>%
  pivot_longer(cols = matches("\\d{4}Total"), 
               names_to = "year", 
               values_to = "total_fh") %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}"))) # Extract year as a number

# Create a list to store yearly diffusion scores
diffusion_list_fh <- list()

# Compute the weighted democracy diffusion score for each year
for (year in years_fh) {
  # Extract democracy scores for the given year
  total_fh <- df_fh_long %>%
    filter(year == !!year) %>%
    select(country_name, total_fh) %>%
    distinct()
  
  # Merge with country coordinates
  total_fh <- left_join(coords_fh, total_fh, by = "country_name")
  
  # Convert democracy scores to matrix form
  score_matrix_fh <- matrix(total_fh$total_fh, 
                         nrow = nrow(coords_fh), 
                         ncol = 1)
  
  # Compute the weighted sum of democracy scores
  weighted_sum_fh <- inverse_distance_fh %*% score_matrix_fh
  # Compute the total sum of weights (to normalize)
  total_weights_fh <- rowSums(inverse_distance_fh)
  # Compute the final diffusion variable as a weighted average
  diffusion_value_fh <- weighted_sum_fh / total_weights_fh
  #diffusion_value_fh <- inverse_distance_fh %*% score_matrix
  # Store the results in a dataframe
  diffusion_df_fh <- data.frame(
    country_name = coords_fh$country_name,
    year = year,
    diffusion_fh = as.numeric(diffusion_value_fh)
  )
  
  # Append to the list
  diffusion_list_fh[[as.character(year)]] <- diffusion_df_fh
}


# Combine all years into a single dataframe
diffusion_scores_fh <- bind_rows(diffusion_list_fh)

# Merge diffusion scores back into df_fh_long
df_fh_long <- left_join(df_fh_long, diffusion_scores_fh, by = c("country_name", "year"))

# Convert back to wide format
df_fh <- df_fh_long %>%
  pivot_wider(names_from = "year", values_from = c("total_fh", "diffusion_fh"))

# View the updated dataset
head(df_fh)

head(df_fh$diffusion_fh_2015)
summary(df_fh$diffusion_fh_2015)

head(df_vdem$diffusion_2015)
summary(df_vdem$diffusion_2015)

# create ranking of countries and their diffusion scores (v-dem and fh) to  compare plausability
# empty dataframe for ranking
df_ranking_vdem <- df_vdem
df_ranking_fh <- df_fh

df_ranking_vdem$rank_vdem <- rank(-df_vdem$diffusion_2015, na.last =TRUE)
df_ranking_fh$rank_fh <- rank(-df_fh$diffusion_fh_2015, na.last =TRUE)

library(dplyr)
df_ranking_vdem <- df_ranking_vdem %>%
  select(c(country_name, rank_vdem, diffusion_2015))

df_ranking_fh <- df_ranking_fh %>% 
  select(c(country_name, rank_fh, diffusion_fh_2015))

df_ranking <- left_join(df_ranking_vdem, df_ranking_fh, by = "country_name")


# 6. Dealing with missing values for FH -----------------------------------------------------------------------

colSums(is.na(df_fh)) # Showing NAs of all columns
colnames(df_fh)[colSums(is.na(df_fh)) > 0] # Showing columns with NAs
sum(is.na(df_fh$GDPpc2023)) # There is 7 NA in GDPpc2023, which is the only column with NAs.

# show rows with NAs
df_fh[is.na(df_fh$GDPpc2023),]
# **I could impute or research them, but instead, I will choose to pick the year 2022 for my analysis instead.**


# 7. Variable Transformation for FH ---------------------------------------

str(df_fh) # all relevant variables are numeric, no transformation needed
# all relevant variables are numeric, no transformation needed
# except the Status Variables, which I'll need to transform

# Transforming Status Variables to ordered factors
class(df_fh$`2015Status`) # character
df_fh$`2015Status` <- factor(df_fh$`2015Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
class(df_fh$`2015Status`) # ordered factor
df_fh$`2016Status` <- factor(df_fh$`2016Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2017Status` <- factor(df_fh$`2017Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2018Status` <- factor(df_fh$`2018Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2019Status` <- factor(df_fh$`2019Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2020Status` <- factor(df_fh$`2020Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2021Status` <- factor(df_fh$`2021Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2022Status` <- factor(df_fh$`2022Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
df_fh$`2023Status` <- factor(df_fh$`2023Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
str(df_fh)

# Population Variable on the other hand has very large range, need to apply log transformation
df_fh$Pop_log_2015 <- log(df_fh$Pop_2015)
summary(df_fh$Pop_2015)
summary(df_fh$Pop_log_2015)
df_fh$Pop_log_2016 <- log(df_fh$Pop_2016)
df_fh$Pop_log_2017 <- log(df_fh$Pop_2017)
df_fh$Pop_log_2018 <- log(df_fh$Pop_2018)
df_fh$Pop_log_2019 <- log(df_fh$Pop_2019)
df_fh$Pop_log_2020 <- log(df_fh$Pop_2020)
df_fh$Pop_log_2021 <- log(df_fh$Pop_2021)
df_fh$Pop_log_2022 <- log(df_fh$Pop_2022)
df_fh$Pop_log_2023 <- log(df_fh$Pop_2023)

# I also want to create a categorical variable for population
#	Micro (<1'000'000), Small (between 1'000'000 - 10'000'000), Large (between 10'000'000 and 100'000'000), and Huge (>100'000'000)
df_fh$Pop_cat_2015 <- cut(df_fh$Pop_2015, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
summary(df_fh$Pop_cat_2015)
df_fh$Pop_cat_2016 <- cut(df_fh$Pop_2016, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_fh$Pop_cat_2017 <- cut(df_fh$Pop_2017, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_fh$Pop_cat_2018 <- cut(df_fh$Pop_2018, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_fh$Pop_cat_2019 <- cut(df_fh$Pop_2019, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_fh$Pop_cat_2020 <- cut(df_fh$Pop_2020, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_fh$Pop_cat_2021 <- cut(df_fh$Pop_2021, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_fh$Pop_cat_2022 <- cut(df_fh$Pop_2022, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
summary(df_fh$Pop_cat_2022)
df_fh$Pop_cat_2023 <- cut(df_fh$Pop_2023, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))

# Log Transformation of GDPpc for FH
range(df_fh$GDPpc2015) # GDPpc Variable also has very large range, need to apply log transformation
df_fh$GDPpc_log_2015 <- log(df_fh$GDPpc2015)
summary(df_fh$GDPpc_log_2015)
df_fh$GDPpc_log_2016 <- log(df_fh$GDPpc2016)
df_fh$GDPpc_log_2017 <- log(df_fh$GDPpc2017)
df_fh$GDPpc_log_2018 <- log(df_fh$GDPpc2018)
df_fh$GDPpc_log_2019 <- log(df_fh$GDPpc2019)
df_fh$GDPpc_log_2020 <- log(df_fh$GDPpc2020)
df_fh$GDPpc_log_2021 <- log(df_fh$GDPpc2021)
df_fh$GDPpc_log_2022 <- log(df_fh$GDPpc2022)
summary(df_fh$GDPpc_log_2022)
df_fh$GDPpc_log_2023 <- log(df_fh$GDPpc2023)


# CV: area(ln), lat(ln), Pop_Density (persons per km2)---------------------

# Area (ln)
range(df_fh$area)
df_fh$area_log <- log(df_fh$area)
range(df_fh$area_log)

# Latitude (ln)
range(df_fh$lat)
df_fh$lat_log <- log(df_fh$lat - min(df_fh$lat) + 1)
range(df_vdem$lat_log)

# Population Density (persons per km2)
range(df_fh$Pop_2022)
df_fh$Pop_Density_2015 <- df_fh$Pop_2015 / df_fh$area
df_fh$Pop_Density_2016 <- df_fh$Pop_2016 / df_fh$area
df_fh$Pop_Density_2017 <- df_fh$Pop_2017 / df_fh$area
df_fh$Pop_Density_2018 <- df_fh$Pop_2018 / df_fh$area
df_fh$Pop_Density_2019 <- df_fh$Pop_2019 / df_fh$area
df_fh$Pop_Density_2020 <- df_fh$Pop_2020 / df_fh$area
df_fh$Pop_Density_2021 <- df_fh$Pop_2021 / df_fh$area
df_fh$Pop_Density_2022 <- df_fh$Pop_2022 / df_fh$area
summary(df_fh$Pop_Density_2022)
df_fh$Pop_Density_2023 <- df_fh$Pop_2023 / df_fh$area

# Log Transformation of Population Density for FH
df_fh$Pop_Density_log_2015 <- log(df_fh$Pop_Density_2015)
df_fh$Pop_Density_log_2016 <- log(df_fh$Pop_Density_2016)
df_fh$Pop_Density_log_2017 <- log(df_fh$Pop_Density_2017)
df_fh$Pop_Density_log_2018 <- log(df_fh$Pop_Density_2018)
df_fh$Pop_Density_log_2019 <- log(df_fh$Pop_Density_2019)
df_fh$Pop_Density_log_2020 <- log(df_fh$Pop_Density_2020)
df_fh$Pop_Density_log_2021 <- log(df_fh$Pop_Density_2021)
df_fh$Pop_Density_log_2022 <- log(df_fh$Pop_Density_2022)
summary(df_fh$Pop_Density_log_2022)
df_fh$Pop_Density_log_2023 <- log(df_fh$Pop_Density_2023)

# CV: Dummy Variable for culture, region and sub-region--------------------

# Creating dummy variables for culture
unique(df_fh$culture)
table(df_fh$culture)
df_fh <- df_fh %>%
  mutate(
    central_asia = ifelse(culture == "Central Asia", 1, 0),
    east_asia = ifelse(culture == "East Asia", 1, 0),
    southeast_asia = ifelse(culture == "Southeast Asia", 1, 0),
    south_asia = ifelse(culture == "South Asia", 1, 0),
    eastern_europe = ifelse(culture == "Eastern European", 1, 0),
    carribbean = ifelse(culture == "Carribbean", 1, 0),
    north_america = ifelse(culture == "North American", 1, 0),
    pacific_island = ifelse(culture == "Pacific Island", 1, 0),
    not_defined = ifelse(culture == "Not Defined", 1, 0)
  )

# Creating dummy variables for regions
df_fh <- df_fh %>%
  mutate(
    Asia = ifelse(region == "Asia", 1, 0),
    Americas = ifelse(region == "Americas", 1, 0),
    Africa = ifelse(region == "Africa", 1, 0),
    Europe = ifelse(region == "Europe", 1, 0),
    Oceania = ifelse(region == "Oceania", 1, 0)
  )

# Creating dummy variables for sub-regions
unique(df_fh$sub_region)
table(df_fh$sub_region)
df_fh <- df_fh %>%
  mutate(
    srNorth_Africa = ifelse(sub_region == "North Africa", 1, 0),
    srWest_Africa = ifelse(sub_region == "West Africa", 1, 0),
    srCentral_Africa = ifelse(sub_region == "Central Africa", 1, 0),
    srEast_Africa = ifelse(sub_region == "East Africa", 1, 0),
    srSouthern_Africa = ifelse(sub_region == "Southern Africa", 1, 0),
    srCaucasus = ifelse(sub_region == "Caucasus", 1, 0),
    srCentral_Asia = ifelse(sub_region == "Central Asia", 1, 0),
    srMiddle_East = ifelse(sub_region == "Middle East", 1, 0),
    srEast_Asia = ifelse(sub_region == "East Asia", 1, 0),
    srSoutheast_Asia = ifelse(sub_region == "Southeast Asia", 1, 0),
    srSouth_Asia = ifelse(sub_region == "South Asia", 1, 0),
    srNordic = ifelse(sub_region == "Nordic", 1, 0),
    srWestern_Europe = ifelse(sub_region == "Western Europe", 1, 0),
    srEast_Central_Europe = ifelse(sub_region == "East-Central Europe", 1, 0),
    srBalkans = ifelse(sub_region == "Balkans", 1, 0),
    srSouthern_Europe = ifelse(sub_region == "Southern Europe", 1, 0),
    srNorth_America = ifelse(sub_region == "North America", 1, 0),
    srCentral_America = ifelse(sub_region == "Central America", 1, 0),
    srCarribbean = ifelse(sub_region == "Carribbean", 1, 0),
    srSouth_America = ifelse(sub_region == "South America", 1, 0),
    srOceania = ifelse(sub_region == "Oceania", 1, 0)
  )



# 8. Filtering final fh df for Regression ---------------------------------------------

install.packages("dplyr")
library(dplyr)
df_fh_filtered <- df_fh %>%
  select(-c(iso2, dis_int, city_en, lat, lon, `former/current_communist_regime`, V_Dem, FH, `C/T`, Pop, `GDP_pc_PPP_const_2021$`, FH_Scores))

colSums(is.na(df_fh_filtered)) # Showing NAs of all columns
colnames(df_fh_filtered)[colSums(is.na(df_fh_filtered)) > 0]
str(df_fh_filtered)

# Saving df as csv file
file_path_fh <- '/Users/nicolaswaser/New-project-GitHub-first/R/MSA I/Input Data/df_fh_filtered.csv'
write.csv(df_fh_filtered, file_path_fh, row.names = FALSE)

