### **Regression Analysis for IV V-Dem** ###

# Importing the data
library(readr)
file_path_vdem
vdem <- read_csv(file_path_vdem)

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
is.factor(vdem$Pop_cat_2015)
# Transform into ordered factors for Ordered Logistic Regression
vdem$Pop_cat_2015 <- factor(vdem$Pop_cat_2015, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
levels(vdem$Pop_cat_2015)
vdem$Pop_cat_2016 <- factor(vdem$Pop_cat_2016, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
vdem$Pop_cat_2017 <- factor(vdem$Pop_cat_2017, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
vdem$Pop_cat_2018 <- factor(vdem$Pop_cat_2018, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
vdem$Pop_cat_2019 <- factor(vdem$Pop_cat_2019, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
vdem$Pop_cat_2020 <- factor(vdem$Pop_cat_2020, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
vdem$Pop_cat_2021 <- factor(vdem$Pop_cat_2021, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
vdem$Pop_cat_2022 <- factor(vdem$Pop_cat_2022, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
levels(vdem$Pop_cat_2022)
vdem$Pop_cat_2023 <- factor(vdem$Pop_cat_2023, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered = TRUE)
is.factor(vdem$Pop_cat_2015)

# 1. Tests: Simple Regression for 2022: Pop (IV) - V_Dem (DV) -----------

# IV Pop_2022 no log
model1_no_log <- lm(vdem$`2022V_Dem` ~ vdem$Pop_2022, data = vdem)
summary(model1_no_log)

range(vdem$Pop_log_2022)
range(vdem$`2022V_Dem`)

# IV Pop_2022 with log
model1_log <- lm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022, data = vdem)
summary(model1_log)

# Betareg for DV bounded between 0 and 1, but not equal to 0 or 1 (0 < y < 1)
model1_betareg <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022, data = vdem)
summary(model1_betareg)

# Trying Fractional Logit Regression
model1_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022, 
                         family = quasibinomial(link = "logit"), data = vdem)
summary(model1_fractional)

stargazer(model1_log, model1_betareg, model1_fractional, type = "text", 
          title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)") 
# model1_betareg is best, fractional logit second best
# but low R^2 and Adj. R^2 value, though to be expected since there are no CVs


# Adding fixed effects ----------------------------------------------------

install.packages("lfe")
library(lfe)

model1_felm <- felm(vdem$`2022V_Dem` ~ vdem$Pop_2022, data = vdem)
summary(model1_felm)

model1_felm_log <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 | 0 |0 | iso3, data = vdem)
summary(model1_felm)

model1_felm_scale <- felm(vdem$`2022V_Dem` ~ scale(vdem$Pop_2022) | 0 |0 | iso3, data = vdem)
summary(model1_felm_scale)

model1_felm_scale_fixed <- felm(vdem$`2022V_Dem` ~ scale(vdem$Pop_2022) | vdem$sub_region |0 | iso3, data = vdem)
summary(model1_felm_scale_fixed)

model1_felm_log_fixed <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 | vdem$sub_region |0 | iso3, data = vdem)
summary(model1_felm_log_fixed)

stargazer(model1_betareg, model1_felm_scale, model1_felm_scale_fixed, model1_felm_scale_fixed2, type = "text", 
          title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)")

# Alternative approach using lapply to work with a list of models
models_list <- list(model1_betareg, model1_felm_log, model1_felm, model1_felm_scale, model1_felm_scale_fixed, model1_felm_log_fixed)
valid_models <- models_list[sapply(models_list, function(x) !inherits(tryCatch(summary(x), error = function(e) e), "error"))]

# Using only valid models in stargazer
stargazer(valid_models, 
          type = "text", 
          title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)")

# 2. Simple Regression 2022: Categorical_Pop (IV) - V_Dem (DV) -------------
table(vdem$Pop_cat_2022)

#vdem$Pop_cat_2022 <- factor(vdem$Pop_cat_2022, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered=TRUE)
is.factor(vdem$Pop_cat_2022)
contrasts(vdem$Pop_cat_2022) <- contr.treatment(4)

model1_cat <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022, data = vdem)
summary_model1_cat <- summary(model1_cat)

# Displaying the results in a cleaner format
coef_table <- tidy(model1_cat) # coefficients table
kable(coef_table, digits = 3)

stargazer(model1_cat, type = "text", 
          covariate.labels = c("Population (Small)", "Population (Large)", 
                               "Population (Huge)", "Population (Micro)"))
# Only stat. significant for large and less for hug
# again low R^2 value

# Tests of log_pop and log_cat -----------
library(car)       # For regression diagnostics
library(lmtest)    # For heteroskedasticity tests
library(sandwich)  # For robust standard errors

summary_model1_betareg <- summary(model1_betareg)
summary_model1_log <- summary(model1_log)
summary_model1_cat <- summary(model1_cat)

# Testing for heteroskedasticity
bp_test1 <- bptest(model1_betareg)
bp_test2 <- bptest(model1_log)
bp_test3 <- bptest(model1_cat)

# Printing heteroskedasticity test results
print(bp_test1) # p-value is well above the conventional threshold of 0.05
# This indicates no significant evidence of heteroskedasticity
print(bp_test2) # same
print(bp_test3) # p-value is close to but still above the 0.05 threshold
# This suggests some potential heteroskedasticity, though not statistically significant at α=0.05

# Model comparison using AIC and BIC
aic_values <- c(AIC(model1_betareg), AIC(model1_log), AIC(model1_cat))
bic_values <- c(BIC(model1_betareg), BIC(model1_log), BIC(model1_cat))
model_comparison <- data.frame(
  Model = c("Log Pop betareg", "Log Pop fractional logit", "Pop Cat"),
  AIC = aic_values,
  BIC = bic_values
)
print(model_comparison)
# The Log Pop betareg model has the lowest AIC (-34.93243), making it the best model according to AIC criterion. 
# It also has the lowest BIC (-25.43807), suggesting it provides the best balance between fit and complexity.


# 3. Introducing CVs (GDPpc_log_2022, Island State, diffusion_2022) --------

# Renaming models for ease of use
model1 <- model1_betareg
model2 <- model1_cat

# Starting with GDPpc_log_2022
model3a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022, data = vdem)
summary(model3a)

# Adding Island State
model3b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state, data = vdem)
summary(model3b)

# Adding diffusion_2022
model3c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
summary(model3c)

stargazer(model3a, model3b, model3c, type = "text", 
          title = "Pop_log (IV) - V_Dem (DV) with CVs 2022")
model3 <- model3c
# The coefficient for Pop_log_2022 is negative and statistically significant in all models. 
# But statistical significance decreases from Model 1 (p<0.05) to Models 2 and 3 (p<0.1)
# Coefficient magnitude decreases from -0.099 to -0.078 as control variables are added

#GDPpc_log_2022 (logged GDP per capita) has a strong positive effect on democracy levels.

#Island states tend to have higher democracy scores, but the effect is only significant in the full model.

#Democratic diffusion (diffusion_2022) has the strongest effect, suggesting that regional democratic influence plays a crucial role.

#R^2 increases as more controls are added, improving model fit. 
#Model 3 provides the best fit with an R² of 0.406, explaining about 41% of variance in democracy scores
#The log likelihood increases substantially from 45.859 (Model 1) to 62.148 (Model 3)
#All variables in Model 3 are statistically significant

#The analysis confirms that population size is indeed a significant predictor of democracy scores, with larger countries tending to have lower democracy levels. 
#However, this effect is relatively modest compared to economic development and regional diffusion effects. 
#The relationship remains significant even when controlling for these important variables, suggesting population size has an independent effect on democratic governance.

# Interaction Effects ----------------
model3c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

# Interaction between Pop_log_2022 and GDPpc_log_2022
model3_inter1 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 * vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
# Interaction between Pop_log_2022 and diffusion_2022
model3_inter2 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 * vdem$diffusion_2022 + vdem$GDPpc_log_2022 + vdem$island_state, data = vdem)

summary(model3_inter1) 
# The interaction between population and GDP per capita is not significant, meaning the effect of population size on democracy is not conditional on GDP per capita
summary(model3_inter2)
# The interaction between population and democratic diffusion is not significant, meaning the effect of population size on democracy does not depend on the regional democratic environment.

# Robustness Checks ----------------------------------------------------
library(car)

# Checking multicollinearity
vif(lm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem))
# VIF < 5 for all variables → No serious multicollinearity issues. Generally, VIF > 5 indicates high collinearity, and VIF > 10 is a strong concern.
#	Since all values are close to 1, this suggests that none of the independent variables are strongly correlated with each other.
# Multicollinearity is not a concern in this model.

# 4. Introducing CVs for Pop_cat (GDPpc_log_2022, Island State, diffusion_2022) -------
contrasts(vdem$Pop_cat_2022) <- contr.treatment(4)

model4a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022, data = vdem)
summary(model4a)
coef_table2 <- tidy(model4a)
kable(coef_table2, digits = 3)
#stargazer(model4a, type = "text", covariate.labels = c("Population (Small)", "Population (Large)", 
#                              "Population (Huge)", "GDPpc log 2022", "Pop (Micro)/Intercept"))

model4b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state, data = vdem)
summary(model4b)
coef_table3 <- tidy(model4b) 
kable(coef_table3, digits = 3)
#stargazer(model4b, type = "text", covariate.labels = c("Population (Small)", "Population (Large)", 
#                              "Population (Huge)", "GDPpc log 2022", "Island State", "Pop (Micro)/Intercept"))

model4c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
summary(model4c)
coef_table4 <- tidy(model4c)
kable(coef_table4, digits = 3)
#stargazer(model4c, type = "text", covariate.labels = c("Population (Small)", "Population (Large)", 
#                              "Population (Huge)", "GDPpc log 2022", "Island State", "Democratic Diffusion", "Pop (Micro)/Intercept"))

stargazer(model4a, model4b, model4c, type = "text",
          covariate.labels = c("Population (Small)", "Population (Large)", 
                               "Population (Huge)", "GDPpc log 2022", "Island State", 
                               "Democratic Diffusion", "Pop (Micro)/Intercept"),
          title = "Pop_cat (IV) - V_Dem (DV) with CVs 2022")
model4 <- model4c
# Interaction Effects & Robustness Checks ----------------
model4 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

# Interaction between Pop_cat_2022 and GDPpc_log_2022
model4_inter1 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 * vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
summary(model4_inter1)

# Interaction between Pop_cat_2022 and diffusion_2022
model4_inter2 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 * vdem$diffusion_2022 + vdem$GDPpc_log_2022 + vdem$island_state, data = vdem)
summary(model4_inter2)
# No sig. interaction effects between Pop_cat_2022 and GDPpc_log_2022 or diffusion_2022

library(car)
# Checking multicollinearity
vif(lm(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem))
# VIF < 5 for all variables → No serious multicollinearity issues. Generally, VIF > 5 indicates high collinearity, and VIF > 10 is a strong concern.

# 5. Adding more CVs to IV Pop_log (MENA, sub_saharan_africa, latin_america, west_europe, former_commu, landlocked -----------------------------------------------------------------------
model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

model5a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe, data = vdem)

model5b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked, data = vdem)

model5c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu, data = vdem)

stargazer(model3, model5a, model5b, model5c, type = "text", title = "Pop_log (IV) - V_Dem (DV) with more CVs 2022")

# The results suggest that economic development (GDP per capita) and democratic diffusion are the strongest positive predictors of democracy, while being in the MENA region or a former communist country has negative associations. 
# Population size maintains a small but significant negative relationship with democracy scores, suggesting larger countries tend to be slightly less democratic, all else equal.
# Island state: Loses significance when more controls are added. By model 4, the effect is positive but no longer statistically significant
# Regional effects: MENA (Middle East and North Africa): Strong negative effect (-0.925 in model 4, p<0.01)
# Sub-Saharan Africa: Initially positive and significant, becomes non-significant in model 4. Latin America and West Europe: No significant effects
#Additional CV: Landlocked status: No significant effect, Former communist countries: Negative effect (-0.368, p<0.05) in model 4
# R² increases from 0.406 to 0.493 across models


# adding even more cvs (area(ln), Pop_Density, lat(ln))
install.packages("lfe")
library(lfe)

model5d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log , data = vdem)

model5e <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$lat_log, data = vdem)

model5f <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log +  vdem$lat_log + vdem$Pop_Density_2022, data = vdem)

model5g <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log, data = vdem)

stargazer(model3, model5c, model5d, model5e, model5f, model5g, type = "text", title = "Pop_log (IV) - V_Dem (DV) with more CVs 2022")

# Model 5d with area_log, but not lat or Pop_Density, is the best model is best

## Testing the felm() function to deal with fixed effects
#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

model3_felm_log_base <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 | 0 |0 | iso3, data = vdem)

model5_felm_log_max <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log | 0 |0 | iso3, data = vdem)

model5_felm_log_fixed_base <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 | vdem$sub_region |0 | iso3, data = vdem)

model5_felm_log_fixed_max <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log | vdem$sub_region |0 | iso3, data = vdem)

model5_felm_log_fixed_max_max <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log | vdem$sub_region |0 | iso3, data = vdem)

# Alternative approach using lapply to work with a list of models
models_list <- list(model5d, model5f, model5g, model5_felm_log_fixed_base, model5_felm_log_fixed_max, model5_felm_log_fixed_max_max)
valid_models <- models_list[sapply(models_list, function(x) !inherits(tryCatch(summary(x), error = function(e) e), "error"))]

# Using only valid models in stargazer
stargazer(valid_models, 
          type = "text", 
          title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)")
# The results are worse in many ways, going back to betareg() models, maybe try glm()
model5 <- model5d

#### Next Steps: 
# See how it is possible to include fixed effects in the betareg model, as the felm model is not working
# maybe try fractional glm() model as well
# Experiment a bit with the culture, the region and sub-region variables, see how they affect the model
# when you find the best model, do some checks from underneath and then get to interpretation
# add some graphs to the interpretation

# Proposals:
# 1. Check for multicollinearity
# 2. Check for heteroskedasticity
# 3. Check for normality of residuals
# 4. Consider alternative specifications or transformations of variables
# 5. Consider interaction effects or non-linear relationships
# 6. Consider robustness checks or sensitivity analysis
# 7. Consider additional control variables or model specifications
# 8. Interpret the results and consider implications for theory



#In R, the scale() function is used to standardise or centre the data. It can be used to scale the columns of a matrix or data frame so that they have zero mean and unit variance, often useful in data preprocessing for statistical analysis or machine learning. By default, it centres the data but you can specify scale = FALSE to only centre or center = FALSE to only scale.
# Interaction Effects & Robustness Checks ---------------------------------



# Adding even more CVs ----------------------------------------------------

#model3, model5, model5d, model5e
#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
#model5d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log , data = vdem)
#model5e <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$lat_log, data = vdem)
#model5f <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log +  vdem$lat_log + vdem$Pop_Density_2022, data = vdem)

# Adding more cultural variables
unique(df_vdem$culture)
table(df_vdem$culture)
model6a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$former_commu + vdem$area_log, data = vdem) 

model6b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$culture, data = vdem)

stargazer(model3, model5d, model6a, model6b, type = "text", title = "Pop_log (IV) - V_Dem (DV) with more CVs 2022")

# Adding regional variables
table(df_vdem$region)
table(df_vdem$Asia)
table(df_vdem$Europe)
table(df_vdem$Africa)
table(df_vdem$Americas)
table(df_vdem$Oceania)

model6c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + area_log + vdem$region, data = vdem)

# Adding sub region variables
unique(df_vdem$sub_region)
table(df_vdem$sub_region)
names(df_vdem)[(ncol(df_vdem)-50):ncol(df_vdem)] # names of last 50 columns

#model6d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$srEast_Central_Europe + 
#                    vdem$srWestern_Europe +vdem$srNordic + vdem$srSouthern_Europe + vdem$srBalkans + vdem$srNorth_Africa + vdem$srCentral_Asia + vdem$srEast_Asia + vdem$srMiddle_East + 
#                   vdem$srSouth_Asia + vdem$srSoutheast_Asia + vdem$srWest_Africa + vdem$srEast_Africa + vdem$srCentral_Africa + vdem$srSouthern_Africa + vdem$srCarribbean + 
#                  vdem$srCentral_America + vdem$srSouth_America + vdem$srOceania, data = vdem)

model6d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + area_log + vdem$sub_region, data = vdem)

stargazer(model5, model6a, model6b, model6c, model6d, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

model6 <- model6a


# Best Results of this round
stargazer(model3, model5, model6, model6b, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

#model5 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log , data = vdem)

#model6a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$former_commu + vdem$area_log, data = vdem) 

#model6b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$culture, data = vdem)

# Interaction Effects & Robustness Checks ---------------------------------


# Try with glm() function
stargazer(model3, model5, model6, model6b, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

#model5 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log , data = vdem)

#model6a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$former_commu + vdem$area_log, data = vdem) 

#model6b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$culture, data = vdem)

model3_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, family = quasibinomial(link = "logit"), data = vdem)

model5_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$former_commu + vdem$area_log , family = quasibinomial(link = "logit"), data = vdem)

model6_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$former_commu + vdem$area_log, family = quasibinomial(link = "logit"), data = vdem)

model6b_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$former_commu + vdem$area_log + vdem$culture, family = quasibinomial(link = "logit"), data = vdem)

stargazer(model3_fractional, model5_fractional, model6_fractional, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

summary(model5_fractional)

summary(model5)
stargazer(model5, model5_fractional, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

# Log-Likelihood
logLik(model5_fractional)  # For quasibinomial
logLik(model5)             # For betareg

# AIC (only for betareg, as quasibinomial doesn't have it)
AIC(model5)

# Pseudo R² for Beta regression
1 - (model5$deviance / model5$null.deviance)  # McFadden's R²

# Overdispersion check for quasibinomial
summary(model5_fractional)$dispersion  # Should be close to 1 for a good fit

# Precision parameter for Beta regression
summary(model5)$phi  # Higher values = more precise estimates# Log-Likelihood
logLik(model5_fractional)  # For quasibinomial
logLik(model5)             # For betareg

# AIC (only for betareg, as quasibinomial doesn't have it)
AIC(model5)

# Pseudo R² for Beta regression
#1 - (model5$deviance / model5$null.deviance)  # McFadden's R²
pseudo_r2 <- 1 - (summary(model5)$deviance / summary(model5)$null.deviance)
print(pseudo_r2)
# Overdispersion check for quasibinomial
summary(model5_fractional)$dispersion  # Should be close to 1 for a good fit

# Precision parameter for Beta regression
summary(model5)$phi  # Higher values = more precise estimates

#Beta regression (model5) seems to be the better model because:
#•	It has a valid log-likelihood (76.75).
#•	The AIC is very low (-127.5), suggesting a good fit.
#•	The quasibinomial dispersion is too low (0.149), meaning it may not properly capture variability in the data.
#•	Beta regression explicitly models dispersion through phi, making it more reliable for fractional outcomes.


## next? - check for multicollinearity, check for outliers, check for heteroscedasticity, check for normality of residuals, check for zero inflation, check for overdispersion

## robustness checks? -  check for different time periods, check for different DVs (FH!), check for different controls, 

# The felm() function in R, provided by the "lfe" package, is used to fit linear models with large fixed effects. It is particularly useful for handling panel data or models with multiple group fixed effects, allowing for efficient estimation and inference in high-dimensional fixed effects regression settings.

# The plm() function in R, provided by the "plm" package, is used to fit linear models with panel data. It is particularly useful for handling panel data or models with multiple group fixed effects, allowing for efficient estimation and inference in high-dimensional fixed effects regression settings.