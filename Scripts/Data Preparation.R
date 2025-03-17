#### Data Preparation ####
# 1. Data Import -------------------------------------------------------------
# Cleaning environment
rm(list = ls())

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
library(dplyr)

# Countries to remove
remove_countries <- c("OWID_GZA", "OWID_WBA", "OWID_SML", "OWID_ZAN", "PSE")

# Filtering out the rows
df1_clean <- df1 %>% filter(!iso3 %in% remove_countries)
head(df1_clean)

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

# 5. CV: Diffusion Variable for V-Dem--------------------------------------------------
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

# try diffusion variable with distance threshold. which distance? for threshold?

# 6. Dealing with missing values for V-Dem -----------------------------------------------------------------------

colSums(is.na(df_vdem)) # Showing NAs of all columns
colnames(df_vdem)[colSums(is.na(df_vdem)) > 0] # Showing columns with NAs
sum(is.na(df_vdem$GDPpc2023))
# There is 3 NA in GDPpc2023, which is the only column with NAs.
# **I could impute or research them, but instead, I will choose to pick the year 2022 for my analysis instead.**


# **Next Steps:**
# **1. Finally choose Countries, Year(s), the IVs (Pop_log or Pop_cat) and the DVs (V-Dem u/o FH) for the analysis!!!**
#   (What about HK and Taiwan?) -> leave for now, maybe remove later!
# **2. Remove NAs from all chosen IV, DV and CVs**
# **3. Transformation of all the chosen variables?** -> no string/characters, only numeric variables
# **4. Log transformation of Pop and GDPpc** 

# 7. Variable Transformation for V-Dem ---------------------------------------

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

# GDPpc Variable also has very large range, need to apply log transformation
df_vdem$GDPpc_log_2015 <- log(df_vdem$GDPpc2015)
df_vdem$GDPpc_log_2016 <- log(df_vdem$GDPpc2016)
df_vdem$GDPpc_log_2017 <- log(df_vdem$GDPpc2017)
df_vdem$GDPpc_log_2018 <- log(df_vdem$GDPpc2018)
df_vdem$GDPpc_log_2019 <- log(df_vdem$GDPpc2019)
df_vdem$GDPpc_log_2020 <- log(df_vdem$GDPpc2020)
df_vdem$GDPpc_log_2021 <- log(df_vdem$GDPpc2021)
df_vdem$GDPpc_log_2022 <- log(df_vdem$GDPpc2022)
df_vdem$GDPpc_log_2023 <- log(df_vdem$GDPpc2023)

# Data Frame for FH -------------------------------------------------------

df_fh <- df1_impute

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
