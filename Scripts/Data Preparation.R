### **Data Preparation** ###

# 1. Data Import -------------------------------------------------------------
# Cleaning environment
rm(list = ls())

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
#View(df1)

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
df1_impute$area[dublicate]

# 4. Data Frame for V-Dem ----------------------------------------------------
# Removing rows of countries with missing values for V-Dem

# Printing countries names from column V-Dem_Scores
unique(df1_impute$V_Dem_Scores)

df_vdem <- df1_impute

# Removing rows of countries from column V-Dem_Scores, apart from NA
df_vdem <- df_vdem %>% filter(is.na(V_Dem_Scores))

# Removing all columns from column 53 onwards
df_vdem <- df_vdem[, 1:52]

# Showing Data Type of Variables "lon", "lat" and "2015V-Dem" for diffusion variable
str(df_vdem$lon)
str(df_vdem$lat)
str(df_vdem$`2015V_Dem`)

# CV: Diffusion Variable for V-Dem--------------------------------------------------
install.packages("geosphere")
library(geosphere) # For calculating geographic distances
library(tidyverse)
# Extracting coordinates and country identifiers
coords_vdem <- df_vdem %>%
  select(country_name, lon, lat) %>%
  distinct()  # Ensuring unique country entries

# Creating a distance matrix (in kilometers)
distance_matrix_vdem <- distm(
  x = coords_vdem[, c("lon", "lat")], 
  fun = distHaversine  # Uses Haversine formula for Earth distances
) / 1000  # Converting meters to kilometers

# Computing inverse distance weights (excluding self-distance)
inverse_distance_vdem <- 1 / (distance_matrix_vdem + 1)  # Adding 1 to avoid division by zero
diag(inverse_distance_vdem) <- 0  # Setting diagonal (self-distance) to 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem <- inverse_distance_vdem %*% v_dem_scores
  total_weights_vdem <- rowSums(inverse_distance_vdem)
  
  # Adding diffusion variable with proper naming 
  df_vdem <- df_vdem %>%
    mutate(
      !!paste0("diffusion_", year) := as.numeric(weighted_sum_vdem / total_weights_vdem)
    )
}
head(df_vdem$diffusion_2015)
summary(df_vdem$diffusion_2015)
# Compute the weighted democracy diffusion score

# try diffusion variable with distance threshold. which distance? for threshold?
summary(distance_matrix_vdem)
# average of average
mean(distance_matrix_vdem)
median(distance_matrix_vdem)
min(distance_matrix_vdem)
max(distance_matrix_vdem)
# The distance matrix shows that the average distance is 5,000 km.

# Diffusion Variable for V-Dem with Distance Threshold and Combo with exponential decay----------------------

## trying with distance threshold
# Computing inverse distance weights (excluding self-distance)
inverse_distance_vdem2 <- 1 / (distance_matrix_vdem + 1)  # Adding 1 to avoid division by zero
diag(inverse_distance_vdem2) <- 0  # Setting diagonal (self-distance) to 0

# Setting a distance threshold of 5000 km
inverse_distance_vdem2[distance_matrix_vdem > 5000] <- 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

df_vdem2 <- df_vdem

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem2 <- inverse_distance_vdem2 %*% v_dem_scores
  total_weights_vdem2 <- rowSums(inverse_distance_vdem2)
  
  # Adding diffusion variable with proper naming 
  df_vdem2 <- df_vdem2 %>%
    mutate(
      !!paste0("diffusion2_", year) := as.numeric(weighted_sum_vdem2 / total_weights_vdem2)
    )
}
head(df_vdem2$diffusion_2015)
head(df_vdem2$diffusion2_2015)
summary(df_vdem2$diffusion_2015)
summary(df_vdem2$diffusion2_2015)

# The diffusion variable with a distance threshold of 4500 km seems to have a lower mean and median compared to the original diffusion variable.

## With distance threshold and exponential decay

# Distance threshold for very remote islands
distance_threshold <- 5000

# Exponential decay parameter
lambda <- 0.001

# Option 1: Computing inverse distance weights with distance threshold and exponential decay
inverse_distance_vdem3 <- ifelse(
  distance_matrix_vdem > distance_threshold, 
  0,  # Setting weight to 0 if distance exceeds threshold
  exp(-lambda * distance_matrix_vdem)  # Applying exponential decay
)

# Option 2: Computing inverse distance weights with only exponential decay
lambda <- 0.001
inverse_distance_vdem3 <- exp(-lambda * distance_matrix_vdem)  # Applying exponential decay


# Excluding self-distance
diag(inverse_distance_vdem3) <- 0  # Setting diagonal (self-distance) to 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

df_vdem3 <- df_vdem2

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem3 <- inverse_distance_vdem3 %*% v_dem_scores
  total_weights_vdem3 <- rowSums(inverse_distance_vdem3)
  
  # Adding diffusion variable with proper naming 
  df_vdem3 <- df_vdem3 %>%
    mutate(
      !!paste0("diffusion3_", year) := as.numeric(weighted_sum_vdem3 / total_weights_vdem3)
    )
}

head(df_vdem3$diffusion_2015)
head(df_vdem3$diffusion2_2015)
head(df_vdem3$diffusion3_2015)
summary(df_vdem3$diffusion_2015)
summary(df_vdem3$diffusion2_2015)
summary(df_vdem3$diffusion3_2015)

# Dealing with missing values for V-Dem -----------------------------------------------------------------------

colSums(is.na(df_vdem)) # Showing NAs of all columns
colnames(df_vdem)[colSums(is.na(df_vdem)) > 0] # Showing columns with NAs
sum(is.na(df_vdem$GDPpc2023))
# There is 3 NA in GDPpc2023, which is the only column with NAs.
# **I could impute or research them, but instead, I will choose to pick the year 2022 for my analysis instead.**


# Variable Transformation for V-Dem ---------------------------------------

# **Reminder:**
#numeric - (10.5, 55, 787)
#integer - (1L, 55L, 100L, where the letter "L" declares this as an integer)
#complex - (9 + 3i, where "i" is the imaginary part)
#character (a.k.a. string) - ("k", "R is exciting", "FALSE", "11.5")
#logical (a.k.a. boolean) - (TRUE or FALSE)

class(df_vdem$`2015V_Dem`)
class(df_vdem$diffusion_2015)
class(df_vdem$Pop_2015)
class(df_vdem$GDPpc2015)
class(df_vdem$island_state)
class(df_vdem$landlocked)
class(df_vdem$MENA)
# all relevant variables are numeric, no transformation needed

# Population Variable on the other hand has very large range, need to apply log transformation
df_vdem$Pop_log_2015 <- log(df_vdem$Pop_2015)
summary(df_vdem$Pop_2015)
summary(df_vdem$Pop_log_2015)
df_vdem$Pop_log_2016 <- log(df_vdem$Pop_2016)
df_vdem$Pop_log_2017 <- log(df_vdem$Pop_2017)
df_vdem$Pop_log_2018 <- log(df_vdem$Pop_2018)
df_vdem$Pop_log_2019 <- log(df_vdem$Pop_2019)
df_vdem$Pop_log_2020 <- log(df_vdem$Pop_2020)
df_vdem$Pop_log_2021 <- log(df_vdem$Pop_2021)
df_vdem$Pop_log_2022 <- log(df_vdem$Pop_2022)
df_vdem$Pop_log_2023 <- log(df_vdem$Pop_2023)

# I also want to create a categorical variable for population
#	Micro (<1'000'000), Small (between 1'000'000 - 10'000'000), Large (between 10'000'000 and 100'000'000), and Huge (>100'000'000)
df_vdem$Pop_cat_2015 <- cut(df_vdem$Pop_2015, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
summary(df_vdem$Pop_cat_2015)
df_vdem$Pop_cat_2016 <- cut(df_vdem$Pop_2016, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2017 <- cut(df_vdem$Pop_2017, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2018 <- cut(df_vdem$Pop_2018, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2019 <- cut(df_vdem$Pop_2019, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2020 <- cut(df_vdem$Pop_2020, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2021 <- cut(df_vdem$Pop_2021, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2022 <- cut(df_vdem$Pop_2022, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
summary(df_vdem$Pop_cat_2022)
df_vdem$Pop_cat_2023 <- cut(df_vdem$Pop_2023, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))

# GDPpc Variable also has very large range, need to apply log transformation
df_vdem$GDPpc_log_2015 <- log(df_vdem$GDPpc2015)
summary(df_vdem$GDPpc_log_2015)
df_vdem$GDPpc_log_2016 <- log(df_vdem$GDPpc2016)
df_vdem$GDPpc_log_2017 <- log(df_vdem$GDPpc2017)
df_vdem$GDPpc_log_2018 <- log(df_vdem$GDPpc2018)
df_vdem$GDPpc_log_2019 <- log(df_vdem$GDPpc2019)
df_vdem$GDPpc_log_2020 <- log(df_vdem$GDPpc2020)
df_vdem$GDPpc_log_2021 <- log(df_vdem$GDPpc2021)
df_vdem$GDPpc_log_2022 <- log(df_vdem$GDPpc2022)
summary(df_vdem$GDPpc_log_2022)
df_vdem$GDPpc_log_2023 <- log(df_vdem$GDPpc2023)

# CV: area(ln), lat(ln), Pop_Density (persons per km2)
range(df_vdem$area)
df_vdem$area_log <- log(df_vdem$area)
range(df_vdem$area_log)

range(df_vdem$lat)
#df_vdem$lat_pos <- (df_vdem$lat - min(df_vdem$lat) + 1)
#df_vdem$lat_mag_pos <- (abs(df_vdem$lat) + 1)
#range(df_vdem$lat_pos)
#range(df_vdem$lat_mag_pos)
#df_vdem$lat_log <- log(abs(df_vdem$lat) + 1)
df_vdem$lat_log <- log(df_vdem$lat - min(df_vdem$lat) + 1)
range(df_vdem$lat_log)

range(df_vdem$Pop_2022)
df_vdem$Pop_Density_2015 <- df_vdem$Pop_2015 / df_vdem$area
df_vdem$Pop_Density_2016 <- df_vdem$Pop_2016 / df_vdem$area
df_vdem$Pop_Density_2017 <- df_vdem$Pop_2017 / df_vdem$area
df_vdem$Pop_Density_2018 <- df_vdem$Pop_2018 / df_vdem$area
df_vdem$Pop_Density_2019 <- df_vdem$Pop_2019 / df_vdem$area
df_vdem$Pop_Density_2020 <- df_vdem$Pop_2020 / df_vdem$area
df_vdem$Pop_Density_2021 <- df_vdem$Pop_2021 / df_vdem$area
df_vdem$Pop_Density_2022 <- df_vdem$Pop_2022 / df_vdem$area
df_vdem$Pop_Density_2023 <- df_vdem$Pop_2023 / df_vdem$area
range(df_vdem$Pop_Density_2022)

df_vdem$Pop_Density_log_2015 <- log(df_vdem$Pop_Density_2015)
df_vdem$Pop_Density_log_2016 <- log(df_vdem$Pop_Density_2016)
df_vdem$Pop_Density_log_2017 <- log(df_vdem$Pop_Density_2017)
df_vdem$Pop_Density_log_2018 <- log(df_vdem$Pop_Density_2018)
df_vdem$Pop_Density_log_2019 <- log(df_vdem$Pop_Density_2019)
df_vdem$Pop_Density_log_2020 <- log(df_vdem$Pop_Density_2020)
df_vdem$Pop_Density_log_2021 <- log(df_vdem$Pop_Density_2021)
df_vdem$Pop_Density_log_2022 <- log(df_vdem$Pop_Density_2022)
df_vdem$Pop_Density_log_2023 <- log(df_vdem$Pop_Density_2023)
range(df_vdem$Pop_Density_log_2022)

# Creating dummy variables for regions
df_vdem <- df_vdem %>%
  mutate(
    Asia = ifelse(region == "Asia", 1, 0),
    Americas = ifelse(region == "Americas", 1, 0),
    Africa = ifelse(region == "Africa", 1, 0),
    Europe = ifelse(region == "Europe", 1, 0),
    Oceania = ifelse(region == "Oceania", 1, 0)
  )

# Creating dummy variables for culture
unique(df_vdem$culture)
table(df_vdem$culture)

df_vdem <- df_vdem %>%
  mutate(
    central_asia = ifelse(culture == "Central Asia", 1, 0),
    east_asia = ifelse(culture == "East Asia", 1, 0),
    southeast_asia = ifelse(culture == "Southeast Asia", 1, 0),
    south_asia = ifelse(culture == "South Asia", 1, 0),
    nordic = ifelse(culture == "Nordic", 1, 0),
    eastern_europe = ifelse(culture == "Eastern European", 1, 0),
    mediterranean = ifelse(culture == "Mediterranean", 1, 0),
    carribbean = ifelse(culture == "Carribbean", 1, 0),
    north_america = ifelse(culture == "North American", 1, 0),
    pacific_island = ifelse(culture == "Pacific Island", 1, 0),
    not_defined = ifelse(culture == "Not Defined", 1, 0)
  )

# Creating dummy variables for sub-regions
# Unique values for sub_region
unique(df_vdem$sub_region)
table(df_vdem$sub_region)
df_vdem <- df_vdem %>%
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


# Filtering final v-dem df for Regression ---------------------------------------------
install.packages("dplyr")
library(dplyr)
df_vdem_filtered <- df_vdem %>%
  select(-c(iso2, dis_int, city_en, lat, lon, V_Dem, FH, `C/T`, Pop, `GDP_pc_PPP_const_2021$`, V_Dem_Scores))

colSums(is.na(df_vdem_filtered)) # Showing NAs of all columns
colnames(df_vdem_filtered)[colSums(is.na(df_vdem_filtered)) > 0]
str(df_vdem_filtered)

# Saving df as csv file
file_path_vdem <- '/Users/nicolaswaser/New-project-GitHub-first/R/MSA I/Input Data/df_vdem_filtered.csv'
write.csv(df_vdem_filtered, file_path_vdem, row.names = FALSE)


# 4. Data Frame for FH -------------------------------------------------------
# **Next Steps:**
# **1. Remove uneccessary columns from df_fh**
# **2. Remove rows with missing values for FH**
# **3. Calculate diffusion variable for FH**
# **4. Decide about HK, Taiwan, FL and Mon**
# **5. Remove NAs from all chosen IV, DV and CVs
# **6. Transformation of all the chosen variables? -> no string/characters, only numeric variables
# **7. Log transformation of Pop and GDPpc 

# **Reminder:** 
#numeric - (10.5, 55, 787)
#integer - (1L, 55L, 100L, where the letter "L" declares this as an integer)
#complex - (9 + 3i, where "i" is the imaginary part)
#character (a.k.a. string) - ("k", "R is exciting", "FALSE", "11.5")
#logical (a.k.a. boolean) - (TRUE or FALSE)

df_fh <- df1_impute
unique(df_fh$FH_Scores)
table(df_fh$country_name)
str(df_fh)

# Removing columns 43 - 52 with v-dem scores
df_fh <- df_fh[, -c(43:52)]

table(!is.na(df_fh$FH_Scores))

# CV: Diffusion Variable for FH--------------------------------------------------
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
               values_to = "democracy_score") %>%
  mutate(year = as.numeric(str_extract(year, "\\d{4}"))) # Extract year as a number

# Create a list to store yearly diffusion scores
diffusion_list <- list()

# Compute the weighted democracy diffusion score for each year
for (year in years_fh) {
  # Extract democracy scores for the given year
  democracy_scores <- df_fh_long %>%
    filter(year == !!year) %>%
    select(country_name, democracy_score) %>%
    distinct()
  
  # Merge with country coordinates
  democracy_scores <- left_join(coords_fh, democracy_scores, by = "country_name")
  
  # Convert democracy scores to matrix form
  score_matrix <- matrix(democracy_scores$democracy_score, 
                         nrow = nrow(coords_fh), 
                         ncol = 1)
  
  # Compute the weighted sum of democracy scores
  weighted_sum_fh <- inverse_distance_fh %*% score_matrix
  # Compute the total sum of weights (to normalize)
  total_weights_fh <- rowSums(inverse_distance_fh)
  # Compute the final diffusion variable as a weighted average
  diffusion_value <- weighted_sum_fh / total_weights_fh
  
  # Store the results in a dataframe
  diffusion_df <- data.frame(
    country_name = coords_fh$country_name,
    year = year,
    diffusion_score = as.numeric(diffusion_value)
  )
  
  # Append to the list
  diffusion_list[[as.character(year)]] <- diffusion_df
}

# Combine all years into a single dataframe
diffusion_scores <- bind_rows(diffusion_list)

# Merge diffusion scores back into df_fh_long
df_fh_long <- left_join(df_fh_long, diffusion_scores, by = c("country_name", "year"))

# Convert back to wide format
df_fh <- df_fh_long %>%
  pivot_wider(names_from = "year", values_from = c("democracy_score", "diffusion_score"))

# View the updated dataset
head(df_fh)

head(df_fh$diffusion_score_2015)
summary(df_fh$diffusion_score_2015)

head(df_vdem$diffusion_2015)
summary(df_vdem$diffusion_2015)


# try diffusion variable with distance threshold. which distance? for threshold?
summary(distance_matrix_fh)
# average of average
mean(distance_matrix_fh)
median(distance_matrix_fh)
min(distance_matrix_fh)
max(distance_matrix_fh)

# Diffusion Variable for V-Dem with Distance Threshold and Combo with exponential decay----------------------

## trying with distance threshold
# Computing inverse distance weights (excluding self-distance)
inverse_distance_vdem2 <- 1 / (distance_matrix_vdem + 1)  # Adding 1 to avoid division by zero
diag(inverse_distance_vdem2) <- 0  # Setting diagonal (self-distance) to 0

# Setting a distance threshold of 5000 km
inverse_distance_vdem2[distance_matrix_vdem > 5000] <- 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

df_vdem2 <- df_vdem

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem2 <- inverse_distance_vdem2 %*% v_dem_scores
  total_weights_vdem2 <- rowSums(inverse_distance_vdem2)
  
  # Adding diffusion variable with proper naming 
  df_vdem2 <- df_vdem2 %>%
    mutate(
      !!paste0("diffusion2_", year) := as.numeric(weighted_sum_vdem2 / total_weights_vdem2)
    )
}
head(df_vdem2$diffusion_2015)
head(df_vdem2$diffusion2_2015)
summary(df_vdem2$diffusion_2015)
summary(df_vdem2$diffusion2_2015)

# The diffusion variable with a distance threshold of 4500 km seems to have a lower mean and median compared to the original diffusion variable.

## With distance threshold and exponential decay

# Distance threshold for very remote islands
distance_threshold <- 5000

# Exponential decay parameter
lambda <- 0.001

# Option 1: Computing inverse distance weights with distance threshold and exponential decay
inverse_distance_vdem3 <- ifelse(
  distance_matrix_vdem > distance_threshold, 
  0,  # Setting weight to 0 if distance exceeds threshold
  exp(-lambda * distance_matrix_vdem)  # Applying exponential decay
)

# Option 2: Computing inverse distance weights with only exponential decay
lambda <- 0.001
inverse_distance_vdem3 <- exp(-lambda * distance_matrix_vdem)  # Applying exponential decay


# Excluding self-distance
diag(inverse_distance_vdem3) <- 0  # Setting diagonal (self-distance) to 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

df_vdem3 <- df_vdem2

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem3 <- inverse_distance_vdem3 %*% v_dem_scores
  total_weights_vdem3 <- rowSums(inverse_distance_vdem3)
  
  # Adding diffusion variable with proper naming 
  df_vdem3 <- df_vdem3 %>%
    mutate(
      !!paste0("diffusion3_", year) := as.numeric(weighted_sum_vdem3 / total_weights_vdem3)
    )
}

head(df_vdem3$diffusion_2015)
head(df_vdem3$diffusion2_2015)
head(df_vdem3$diffusion3_2015)
summary(df_vdem3$diffusion_2015)
summary(df_vdem3$diffusion2_2015)
summary(df_vdem3$diffusion3_2015)

# Dealing with missing values for V-Dem -----------------------------------------------------------------------

colSums(is.na(df_vdem)) # Showing NAs of all columns
colnames(df_vdem)[colSums(is.na(df_vdem)) > 0] # Showing columns with NAs
sum(is.na(df_vdem$GDPpc2023))
# There is 3 NA in GDPpc2023, which is the only column with NAs.
# **I could impute or research them, but instead, I will choose to pick the year 2022 for my analysis instead.**


# Variable Transformation for V-Dem ---------------------------------------

# **Reminder:**
#numeric - (10.5, 55, 787)
#integer - (1L, 55L, 100L, where the letter "L" declares this as an integer)
#complex - (9 + 3i, where "i" is the imaginary part)
#character (a.k.a. string) - ("k", "R is exciting", "FALSE", "11.5")
#logical (a.k.a. boolean) - (TRUE or FALSE)

class(df_vdem$`2015V_Dem`)
class(df_vdem$diffusion_2015)
class(df_vdem$Pop_2015)
class(df_vdem$GDPpc2015)
class(df_vdem$island_state)
class(df_vdem$landlocked)
class(df_vdem$MENA)
# all relevant variables are numeric, no transformation needed

# Population Variable on the other hand has very large range, need to apply log transformation
df_vdem$Pop_log_2015 <- log(df_vdem$Pop_2015)
summary(df_vdem$Pop_2015)
summary(df_vdem$Pop_log_2015)
df_vdem$Pop_log_2016 <- log(df_vdem$Pop_2016)
df_vdem$Pop_log_2017 <- log(df_vdem$Pop_2017)
df_vdem$Pop_log_2018 <- log(df_vdem$Pop_2018)
df_vdem$Pop_log_2019 <- log(df_vdem$Pop_2019)
df_vdem$Pop_log_2020 <- log(df_vdem$Pop_2020)
df_vdem$Pop_log_2021 <- log(df_vdem$Pop_2021)
df_vdem$Pop_log_2022 <- log(df_vdem$Pop_2022)
df_vdem$Pop_log_2023 <- log(df_vdem$Pop_2023)

# I also want to create a categorical variable for population
#	Micro (<1'000'000), Small (between 1'000'000 - 10'000'000), Large (between 10'000'000 and 100'000'000), and Huge (>100'000'000)
df_vdem$Pop_cat_2015 <- cut(df_vdem$Pop_2015, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
summary(df_vdem$Pop_cat_2015)
df_vdem$Pop_cat_2016 <- cut(df_vdem$Pop_2016, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2017 <- cut(df_vdem$Pop_2017, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2018 <- cut(df_vdem$Pop_2018, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2019 <- cut(df_vdem$Pop_2019, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2020 <- cut(df_vdem$Pop_2020, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2021 <- cut(df_vdem$Pop_2021, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
df_vdem$Pop_cat_2022 <- cut(df_vdem$Pop_2022, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))
summary(df_vdem$Pop_cat_2022)
df_vdem$Pop_cat_2023 <- cut(df_vdem$Pop_2023, breaks = c(0, 1e6, 1e7, 1e8, Inf), labels = c("Micro", "Small", "Large", "Huge"))

# GDPpc Variable also has very large range, need to apply log transformation
df_vdem$GDPpc_log_2015 <- log(df_vdem$GDPpc2015)
summary(df_vdem$GDPpc_log_2015)
df_vdem$GDPpc_log_2016 <- log(df_vdem$GDPpc2016)
df_vdem$GDPpc_log_2017 <- log(df_vdem$GDPpc2017)
df_vdem$GDPpc_log_2018 <- log(df_vdem$GDPpc2018)
df_vdem$GDPpc_log_2019 <- log(df_vdem$GDPpc2019)
df_vdem$GDPpc_log_2020 <- log(df_vdem$GDPpc2020)
df_vdem$GDPpc_log_2021 <- log(df_vdem$GDPpc2021)
df_vdem$GDPpc_log_2022 <- log(df_vdem$GDPpc2022)
summary(df_vdem$GDPpc_log_2022)
df_vdem$GDPpc_log_2023 <- log(df_vdem$GDPpc2023)

# CV: area(ln), lat(ln), Pop_Density (persons per km2)
range(df_vdem$area)
df_vdem$area_log <- log(df_vdem$area)
range(df_vdem$area_log)

range(df_vdem$lat)
#df_vdem$lat_pos <- (df_vdem$lat - min(df_vdem$lat) + 1)
#df_vdem$lat_mag_pos <- (abs(df_vdem$lat) + 1)
#range(df_vdem$lat_pos)
#range(df_vdem$lat_mag_pos)
#df_vdem$lat_log <- log(abs(df_vdem$lat) + 1)
df_vdem$lat_log <- log(df_vdem$lat - min(df_vdem$lat) + 1)
range(df_vdem$lat_log)

range(df_vdem$Pop_2022)
df_vdem$Pop_Density_2015 <- df_vdem$Pop_2015 / df_vdem$area
df_vdem$Pop_Density_2016 <- df_vdem$Pop_2016 / df_vdem$area
df_vdem$Pop_Density_2017 <- df_vdem$Pop_2017 / df_vdem$area
df_vdem$Pop_Density_2018 <- df_vdem$Pop_2018 / df_vdem$area
df_vdem$Pop_Density_2019 <- df_vdem$Pop_2019 / df_vdem$area
df_vdem$Pop_Density_2020 <- df_vdem$Pop_2020 / df_vdem$area
df_vdem$Pop_Density_2021 <- df_vdem$Pop_2021 / df_vdem$area
df_vdem$Pop_Density_2022 <- df_vdem$Pop_2022 / df_vdem$area
df_vdem$Pop_Density_2023 <- df_vdem$Pop_2023 / df_vdem$area
range(df_vdem$Pop_Density_2022)

df_vdem$Pop_Density_log_2015 <- log(df_vdem$Pop_Density_2015)
df_vdem$Pop_Density_log_2016 <- log(df_vdem$Pop_Density_2016)
df_vdem$Pop_Density_log_2017 <- log(df_vdem$Pop_Density_2017)
df_vdem$Pop_Density_log_2018 <- log(df_vdem$Pop_Density_2018)
df_vdem$Pop_Density_log_2019 <- log(df_vdem$Pop_Density_2019)
df_vdem$Pop_Density_log_2020 <- log(df_vdem$Pop_Density_2020)
df_vdem$Pop_Density_log_2021 <- log(df_vdem$Pop_Density_2021)
df_vdem$Pop_Density_log_2022 <- log(df_vdem$Pop_Density_2022)
df_vdem$Pop_Density_log_2023 <- log(df_vdem$Pop_Density_2023)
range(df_vdem$Pop_Density_log_2022)

# Creating dummy variables for regions
df_vdem <- df_vdem %>%
  mutate(
    Asia = ifelse(region == "Asia", 1, 0),
    Americas = ifelse(region == "Americas", 1, 0),
    Africa = ifelse(region == "Africa", 1, 0),
    Europe = ifelse(region == "Europe", 1, 0),
    Oceania = ifelse(region == "Oceania", 1, 0)
  )

# Creating dummy variables for culture
unique(df_vdem$culture)
table(df_vdem$culture)

df_vdem <- df_vdem %>%
  mutate(
    central_asia = ifelse(culture == "Central Asia", 1, 0),
    east_asia = ifelse(culture == "East Asia", 1, 0),
    southeast_asia = ifelse(culture == "Southeast Asia", 1, 0),
    south_asia = ifelse(culture == "South Asia", 1, 0),
    nordic = ifelse(culture == "Nordic", 1, 0),
    eastern_europe = ifelse(culture == "Eastern European", 1, 0),
    mediterranean = ifelse(culture == "Mediterranean", 1, 0),
    carribbean = ifelse(culture == "Carribbean", 1, 0),
    north_america = ifelse(culture == "North American", 1, 0),
    pacific_island = ifelse(culture == "Pacific Island", 1, 0),
    not_defined = ifelse(culture == "Not Defined", 1, 0)
  )

# Creating dummy variables for sub-regions
# Unique values for sub_region
unique(df_vdem$sub_region)
table(df_vdem$sub_region)
df_vdem <- df_vdem %>%
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


# Filtering final v-dem df for Regression ---------------------------------------------
install.packages("dplyr")
library(dplyr)
df_vdem_filtered <- df_vdem %>%
  select(-c(iso2, dis_int, city_en, lat, lon, V_Dem, FH, `C/T`, Pop, `GDP_pc_PPP_const_2021$`, V_Dem_Scores))

colSums(is.na(df_vdem_filtered)) # Showing NAs of all columns
colnames(df_vdem_filtered)[colSums(is.na(df_vdem_filtered)) > 0]
str(df_vdem_filtered)

# Saving df as csv file
file_path_vdem <- '/Users/nicolaswaser/New-project-GitHub-first/R/MSA I/Input Data/df_vdem_filtered.csv'
write.csv(df_vdem_filtered, file_path_vdem, row.names = FALSE)
