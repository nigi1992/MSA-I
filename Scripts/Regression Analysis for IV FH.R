### **Regression Analysis for IV FH** ###

# Importing the data
library(readr)
file_path_fh
fh <- read_csv(file_path_fh)

# Loading Libraries
library(stargazer)
library(tidyverse)
#install.packages("betareg") 
library(betareg)
library(broom)
library(knitr)
#install.packages("MASS")
library(MASS)

# Transformation of categorical variables ---------------------------------
is.factor(fh$Pop_cat_2015)
# Transform into ordered factors for Ordered Logistic Regression
fh$Pop_cat_2015 <- factor(fh$Pop_cat_2015, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
levels(fh$Pop_cat_2015)
fh$Pop_cat_2016 <- factor(fh$Pop_cat_2016, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
fh$Pop_cat_2017 <- factor(fh$Pop_cat_2017, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
fh$Pop_cat_2018 <- factor(fh$Pop_cat_2018, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
fh$Pop_cat_2019 <- factor(fh$Pop_cat_2019, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
fh$Pop_cat_2020 <- factor(fh$Pop_cat_2020, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
fh$Pop_cat_2021 <- factor(fh$Pop_cat_2021, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
fh$Pop_cat_2022 <- factor(fh$Pop_cat_2022, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
levels(fh$Pop_cat_2022)
fh$Pop_cat_2023 <- factor(fh$Pop_cat_2023, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
is.factor(fh$Pop_cat_2015)

range(fh$Pop_log_2022)
range(fh$total_fh_2022)
range(fh$Pop_cat_2022)
range(fh$`2022Status`)
range(fh$GDPpc_log_2022)
range(fh$island_state)
range(fh$diffusion_fh_2022)
range(fh$area_log)
range(fh$lat_log)
range(fh$Pop_Density_2022)
range(fh$Pop_Density_log_2022)

# 1. Simple OLS Regression: DV (FH Total Score) & IV (Log Pop) --------------

# Model 1: DV (FH_Total_Score) & IV (Log Pop)
range(fh$Pop_log_2022)
range(fh$total_fh_2022)

library(tidyverse)
model1_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022, data = fh)
summary(model1_fh)

# Checking for heteroscedasticity
plot(model1_fh, which = 1)
# Normal Q-Q plot to check normality of residuals
plot(model1_fh, which = 2)


# 2. Ordinal Logistic Regression polr(): DV (FH Status) & IV (Log Pop) -------------

# Model 2: DV (FH Status) & IV (Log Pop)
class(fh$`2022Status`) # character -> transformation necessary!
fh$`2015Status` <- factor(fh$`2015Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
class(fh$`2015Status`) # ordered factor
fh$`2016Status` <- factor(fh$`2016Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2017Status` <- factor(fh$`2017Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2018Status` <- factor(fh$`2018Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2019Status` <- factor(fh$`2019Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2020Status` <- factor(fh$`2020Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2021Status` <- factor(fh$`2021Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2022Status` <- factor(fh$`2022Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2023Status` <- factor(fh$`2023Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
str(fh)

library(MASS)
model2_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022, data = fh, Hess = TRUE)
summary(model2_fh)
#This is highly significant (t-value = -5.478)
#The negative coefficient indicates that as log population increases, the probability of being in a higher freedom category (F vs PF or PF vs NF) decreases
#NF|PF: -7.3963 This is the threshold between Not Free and Partly Free categories
#PF|F: -5.9925 This is the threshold between Partly Free and Free categories


# 3. OLS Regression: DV (FH Total Score) & IV (Log Pop) with CVs --------------

# Model 3: DV (FH Total Score) & IV (Log Pop) with CVs (Log GDP per Capita, island_state, diffusion variable)
model3a_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 , data = fh)

model3b_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh)

model3c_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

stargazer(model3a_fh, model3b_fh, model3c_fh, type = "text")


# 4. Ordinal Logistic Regression polr(): DV (FH Status) & IV (Log Pop) -------------

# Model 4: DV (FH Status) & IV (Log Pop) with CVs (Log GDP per Capita, island_state, diffusion variable)
model4a_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022, data = fh, Hess = TRUE)
summary(model4a_fh)
model4b_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh, Hess = TRUE)
summary(model4b_fh)
model4c_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh, Hess = TRUE)
summary(model4c_fh)
stargazer(model4a_fh, model4b_fh, model4c_fh, type = "text")


# 5. OLS Regression: DV (FH Total Score) & IV (Log Pop) with more regional CVs --------------

# Model 5: DV (FH Total Score) & IV (Log Pop) with more regional CVs (MENA, sub_saharan_africa, latin_america, west_europe, former_commu, landlocked)

model5a_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe, data = fh)

model5b_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu, data = fh)

model5c_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)

stargazer(model5a_fh, model5b_fh, model5c_fh, type = "text")

# Adding more Control Variables (area(ln), Pop_Density, lat(ln))
model5d_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)

model5e_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log, data = fh)

model5f_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022, data = fh)
# Adding more Cultures (central_asia, southeast_asia)
model5g_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$central_asia + fh$southeast_asia, data = fh)
summary(model5g_fh)

stargazer(model5d_fh, model5e_fh, model5f_fh, model5g_fh, type = "text")

stargazer(model5b_fh, model5c_fh, model5d_fh, model5e_fh, model5f_fh, type = "text")


# adding all cultures, regions, sub-regions------------------------------------------

model5h_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$culture, data = fh)

model5i_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$former_commu + fh$landlocked +
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$region, data = fh)

model5j_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$former_commu + fh$landlocked +
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$sub_region, data = fh)

stargazer(model5h_fh, model5i_fh, model5j_fh, type = "text")


# 6. Ordinal Logistic Regression polr(): DV (FH Status) & IV (Log Pop) with more regional CVs -------------

# Model 6: DV (FH Total Score) & IV (Log Pop) with more regional CVs (MENA, sub_saharan_africa, latin_america, west_europe, former_commu, landlocked)

model6a_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe, data = fh)
summary(model6a_fh)
model6b_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu, data = fh)
summary(model6b_fh)
model6c_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)
summary(model6c_fh)
stargazer(model6a_fh, model6b_fh, model6c_fh, type = "text")

# Adding more Control Variables (area(ln), Pop_Density, lat(ln))
model6d_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)

model6e_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log, data = fh)

model6f_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022, data = fh)

stargazer(model6d_fh, model6e_fh, model6f_fh, type = "text")

# Adding more Cultures (central_asia, southeast_asia)
model6g_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$central_asia + fh$southeast_asia, data = fh)
summary(model6g_fh)

stargazer(model6d_fh, model6e_fh, model6f_fh, model6g_fh, type = "text")

# adding all cultures, regions, sub-regions-------------------------------------------------------------------------

model6h_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + 
                     fh$former_commu + fh$landlocked + fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$culture, data = fh)

model6i_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 +
                     fh$former_commu + fh$landlocked + fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$region, data = fh)

model6j_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 +
                     fh$former_commu + fh$landlocked + fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$sub_region, data = fh)

stargazer(model6h_fh, model6i_fh, model6j_fh, type = "text")



# The best models so far-------------------------------------------------------------------------

model3c_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)


model5c_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)

model5d_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)


model6a_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                     fh$sub_saharan_africa + fh$latin_america + fh$west_europe, data = fh)
summary(model6a_fh)
model6b_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                     fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu, data = fh)
summary(model6b_fh)
model6c_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                     fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)
summary(model6c_fh)


# 7. OLS Regression: DV (Pol Rights) & IV (Log Pop) with CVs --------------

model7a_fh <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

model7b_fh <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)

model7c_fh <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)
stargazer(model7a_fh, model7b_fh, model7c_fh, type = "text")


# 8. Ordinal Logistic Regression polr(): DV (`2022PR rating`) & IV (Log Pop) with more regional CVs-----------------

fh$`2022PR rating` <- factor(fh$`2022PR rating`, levels = c(7, 6, 5, 4, 3, 2, 1), ordered = TRUE)

model8a_fh <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

model8b_fh <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)

model8c_fh <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)

stargazer(model8a_fh, model8b_fh, model8c_fh, type = "text")


# 9. OLS Regression: DV (Civ Rights) & IV (Log Pop) with CVs ----------------------------------

model9a_fh <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

model9b_fh <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)

model9c_fh <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)

stargazer(model9a_fh, model9b_fh, model9c_fh, type = "text")


# 10. Ordinal Logistic Regression polr(): DV (`2022CL rating`) & IV (Log Pop) with more regional CVs-----------------

fh$`2022CL rating` <- factor(fh$`2022CL rating`, levels = c(7, 6, 5, 4, 3, 2, 1), ordered = TRUE)

model10a_fh <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

model10b_fh <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)

model10c_fh <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)

stargazer(model10a_fh, model10b_fh, model10c_fh, type = "text")



# 11. OLS Regression: DV (FH Total Score) & IV (Pop Cat) with CVs-------------------------------------------------------------------------
contrasts(fh$Pop_cat_2022) <- contr.treatment(4)

model11a_fh <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)
coef_table11 <- tidy(model11a_fh)
kable(coef_table11, digits = 3)
model11b_fh <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked, data = fh)
coef_table12 <- tidy(model11b_fh) 
kable(coef_table12, digits = 3)
model11c_fh <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$former_commu + fh$landlocked + 
                   fh$area_log, data = fh)
coef_table13 <- tidy(model11c_fh)
kable(coef_table13, digits = 3)
#stargazer(model11a_fh, model11b_fh, model11c_fh, type = "text")
stargazer(model11a_fh, model11b_fh, type = "text",
          covariate.labels = c("Population (Small)", "Population (Large)", 
                               "Population (Huge)", "GDPpc log 2022", "Island State", 
                               "Democratic Diffusion", "MENA", "Sub-saharan Africa", "Latin America", "Western Europe", "former Commies", "Landlocked", "Pop (Micro)/Intercept"),
          title = "Pop_cat (IV) - FH Total Score (DV) with CVs 2022")

