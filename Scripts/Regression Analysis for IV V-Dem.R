### **Regression Analysis for IV V-Dem** ###

# Cleaning environment
#rm(list = ls())

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

# Transforming dummy variables into factors
vdem$landlocked <- factor(vdem$landlocked, levels = c(0, 1))
vdem$island_state <- factor(vdem$island_state, levels = c(0, 1))
vdem$communist <- factor(vdem$communist, levels = c(0, 1))
is.factor(vdem$communist)
is.factor(vdem$landlocked)
is.factor(vdem$island_state)

# Same for culture variables
vdem$MENA <- factor(vdem$MENA, levels = c(0, 1))
vdem$sub_saharan_africa <- factor(vdem$sub_saharan_africa, levels = c(0, 1))
vdem$latin_america <- factor(vdem$latin_america, levels = c(0, 1))
vdem$west_europe <- factor(vdem$west_europe, levels = c(0, 1))
vdem$central_asia <- factor(vdem$central_asia, levels = c(0, 1))
vdem$southeast_asia <- factor(vdem$southeast_asia, levels = c(0, 1))
is.factor(vdem$MENA)
is.factor(vdem$sub_saharan_africa)
is.factor(vdem$latin_america)
is.factor(vdem$west_europe)
is.factor(vdem$central_asia)
is.factor(vdem$southeast_asia)


# 1. Pop_log_2022 (IV) - V_Dem (DV) - Simple - lm(), betareg(), glm() -----------

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
          title = "Pop_log_2022 (IV) - V_Dem (DV) - Simple - lm(), betareg(), glm()") 
# model1_betareg is best, fractional logit second best
# but low R^2 and Adj. R^2 value, though to be expected since there are no CVs


# Adding fixed effects - felm() ----------------------------------------------------

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

#stargazer(model1_betareg, model1_felm_scale, model1_felm_scale_fixed, model1_felm_scale_fixed2, type = "text", 
  #        title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)")

# Alternative approach using lapply to work with a list of models
models_list <- list(model1_betareg, model1_felm_log, model1_felm, model1_felm_scale, model1_felm_scale_fixed, model1_felm_log_fixed)
valid_models <- models_list[sapply(models_list, function(x) !inherits(tryCatch(summary(x), error = function(e) e), "error"))]

# Using only valid models in stargazer
stargazer(valid_models, 
          type = "text", 
          title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)")

# 2. Cat_pop_2022 (IV) - V_Dem (DV) - Simple - polr() -------------
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
                               "Population (Huge)", "Population (Micro)"),
          title = "Cat_pop_2022 (IV) - V_Dem (DV) - Simple - polr()")
# Only stat. significant for large and less for hug
# again low R^2 value

# Tests of log_pop_2022 and log_cat_2022 -----------
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


# 3. Pop_log_2022 (IV) - V_Dem (DV) - betareg() - CVs Benchmark (GDPpc_log_2022, island_state, diffusion_2022, communist) --------

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

# Adding communist
model3d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + 
                     vdem$communist, data = vdem)
summary(model3d)

stargazer(model1_betareg, model3a, model3b, model3c, model3d, type = "text", 
          title = "Pop_log_2022 (IV) - V_Dem (DV) - betareg() - CVs Benchmark")
model3 <- model3d
# The coefficient for Pop_log_2022 is negative and statistically significant in all models. apart from 3.
# But statistical significance decreases from Model 1 (p<0.05) to Models 2 + 4 (p<0.1) and disappears in Model 3.
# Coefficient magnitude decreases from -0.099 to -0.064 as control variables are added

#GDPpc_log_2022 (logged GDP per capita) has a strong positive effect on democracy levels.

#Island states tend to have higher democracy scores, but the effect is only significant in the model 3.

#Democratic diffusion (diffusion_2022) has the strongest effect, suggesting that regional democratic influence plays a crucial role.

#communist also has sig. neg. effect.

#R^2 increases as more controls are added, improving model fit. 
#Model 4 provides the best fit with an R² of 0.455, explaining about 45% of variance in democracy scores
#The log likelihood increases substantially from 45.859 (Model 1) to 69.324 (Model 3)
#All variables in Model 4 are statistically significant but island.

#The analysis confirms that population size is indeed a significant predictor of democracy scores, with larger countries tending to have lower democracy levels. 
#However, this effect is relatively modest compared to economic development and regional diffusion effects. 
#The relationship remains significant even when controlling for these important variables, suggesting population size has an independent effect on democratic governance.

# Trying Fractional Logit Regression
model3_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + 
                           vdem$communist, 
                         family = quasibinomial(link = "logit"), data = vdem)
summary(model3_fractional) # Not much better...


# Interaction Effects & Robustness Checks ----------------
model3d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + 
                     vdem$communist, data = vdem)

# Interaction between Pop_log_2022 and GDPpc_log_2022
model3_inter1 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 * vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + 
                           vdem$communist, data = vdem)
# Interaction between Pop_log_2022 and diffusion_2022
model3_inter2 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 * vdem$diffusion_2022 + vdem$GDPpc_log_2022 + vdem$island_state + 
                           vdem$communist, data = vdem)
# Interaction between Pop_log_2022 and communist
model3_inter3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 * vdem$communist + vdem$GDPpc_log_2022 + vdem$island_state + 
                           vdem$diffusion_2022, data = vdem)

summary(model3_inter1) 
# The interaction between population and GDP per capita is not significant, meaning the effect of population size on democracy is not conditional on GDP per capita
summary(model3_inter2)
# The interaction between population and democratic diffusion is not significant, meaning the effect of population size on democracy does not depend on the regional democratic environment.
summary(model3_inter3)
# The interaction between population and communist regime form is significant, meaning the effect of population size on democracy is at least partially conditional on the type of regime.
# Though it is very small .008
cor(vdem$Pop_log_2022, vdem$communist) # No sig. cor.


library(car)
# Checking multicollinearity
vif(lm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist, data = vdem))
# VIF < 5 for all variables → No serious multicollinearity issues. Generally, VIF > 5 indicates high collinearity, and VIF > 10 is a strong concern.
#	Since all values are close to 1, this suggests that none of the independent variables are strongly correlated with each other.
# Multicollinearity is not a concern in this model.


# 4. Cat_pop_2022 (IV) - V_Dem (DV) - polr() - CVs Benchmark (GDPpc_log_2022, island_state, diffusion_2022, communist)  -------

contrasts(vdem$Pop_cat_2022) <- contr.treatment(4)

model4a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022, data = vdem)
coef_table2 <- tidy(model4a)
kable(coef_table2, digits = 3)

model4b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state, data = vdem)
coef_table3 <- tidy(model4b) 
kable(coef_table3, digits = 3)

model4c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
coef_table4 <- tidy(model4c)
kable(coef_table4, digits = 3)

model4d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + 
                     vdem$communist, data = vdem)
coef_table4b <- tidy(model4d)
kable(coef_table4b, digits = 3)
stargazer(model4a, model4b, model4c, model4d, type = "text",
          covariate.labels = c("Population (Small)", "Population (Large)", 
                               "Population (Huge)", "GDPpc log 2022", "Island State", 
                               "Diffusion", "Communist", "Pop (Micro)/Intercept"),
          title = "Cat_pop_2022 (IV) - V_Dem (DV) - betareg() - CVs Benchmark")
model4 <- model4d


# Interaction Effects & Robustness Checks ----------------
model4 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist, data = vdem)

# Interaction between Pop_cat_2022 and GDPpc_log_2022
model4_inter1 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 * vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist, data = vdem)
summary(model4_inter1)

# Interaction between Pop_cat_2022 and diffusion_2022
model4_inter2 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 * vdem$diffusion_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$communist, data = vdem)
summary(model4_inter2)

model4_inter3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 * vdem$communist + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
summary(model4_inter3)
# No sig. interaction effects between Pop_cat_2022 and GDPpc_log_2022 or diffusion_2022 or communist

library(car)
# Checking multicollinearity
vif(lm(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist, data = vdem))
# VIF < 5 for all variables → No serious multicollinearity issues. Generally, VIF > 5 indicates high collinearity, and VIF > 10 is a strong concern.


# 5. Pop_log_2022 (IV) - V_Dem (DV) - betareg() - CVs Extended (+ MENA, sub_saharan_africa, latin_america, west_europe, southeast_asia, central_asia) -----------------------------------------------------------------------

model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist, data = vdem)

model5a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe, data = vdem)

model5b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia, data = vdem)

model5c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia, data = vdem)

stargazer(model3, model5a, model5b, model5c, type = "text", title = "Pop_log_2022 (IV) - V_Dem (DV) - betareg() - CVs Extended")

model5 <- model5c
# The results suggest that economic development (GDP per capita) and democratic diffusion are the strongest positive predictors of democracy, while being in the MENA region or a (former) communist country has negative associations. 
# Population size maintains a small but significant negative relationship with democracy scores, suggesting larger countries tend to be slightly less democratic, all else equal.
# Island state: Has no significance when more controls are added. the effect is positive but not statistically significant
# Communist: sig and strong neg. effect
# Regional effects: MENA (Middle East and North Africa): Strong negative effect (-1.292 in model 4, p<0.01)
# Sub-Saharan Africa: Initially positive and significant, becomes non-significant in model 3 & 4. 
# Latin America later sig. (neg.) and West Europe: Pos., but no significant effects
# southeast asia: sig. neg. effect, central_asia: not sig. neg. effect
#Additional CV: Landlocked status: No significant effect, Former communist countries: Negative effect (-0.368, p<0.05) in model 4
# R² increases from 0.455 to 0.555 across models


# 6. Pop_log_2022 (IV) - V_Dem (DV) - betareg() - CVs Maximal (+ landlocked, area(ln), lat(ln)) -------------

range(vdem$lat_log)
range(vdem$area)
range(vdem$area_log)

model5 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia, data = vdem)

model6a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                    vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked,
                  data = vdem)

model6b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + 
                     vdem$area_log, data = vdem)

model6c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + 
                     vdem$area_log + vdem$lat_log, data = vdem)
# something is wrong with Pop_Density_log_2022, which is why I will exclude it from the analysis!

stargazer(model5, model6a, model6b, model6c, type = "text", title = "Pop_log_2022 (IV) - V_Dem (DV) - betareg() - CVs Maximal")
model6 <- model6c


# Interaction Effects & Robustness Checks ---------------------------------


# 7. Model Comparison (unscaled) -----------------------------------------------------

# Pop_log_2022 (IV) - 2022V_Dem (DV) Models with CVs
stargazer(model3, model5, model6, type = "text", title = "Model Comparison: Pop_log_2022 (IV) - 2022V_Dem (DV)")

# Pop_cat_2022 (IV) - 2022V_Dem (DV) Model with CVs
stargazer(model4, type = "text", title = "Pop_cat_2022 (IV) - 2022V_Dem (DV) - polr()")


# 8. Model Comparison (scaled) --------------------------------------------

# Scaling `2022V_Dem` (DV) and Diffusion Variable to enable better comparison with FH Scores
vdem$`2022V_Dem_scaled` <- vdem$`2022V_Dem` * 100 # Scale `2022V_Dem` by * 100
vdem$diffusion_2022_scaled <- vdem$diffusion_2022 * 100 # Scale diffusion_2022 by * 100 

# Pop_log_2022 (IV) - 2022V_Dem (DV) scaled Models with CVs
model3_scaled <- lm(vdem$`2022V_Dem_scaled` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022_scaled + vdem$communist, data = vdem)

model5_scaled <- lm(vdem$`2022V_Dem_scaled` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022_scaled + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia, data = vdem)

model6_scaled <- lm(vdem$`2022V_Dem_scaled` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022_scaled + vdem$communist + vdem$MENA + 
                     vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + 
                     vdem$area_log + vdem$lat_log, data = vdem)

stargazer(model3, model3_scaled, model5, model5_scaled, model6, model6_scaled, type = "text", title = "Model Comparison: Pop_log_2022 (IV) - 2022V_Dem (DV) - Unscaled + Scaled")


# Pop_cat_2022 (IV) - 2022V_Dem (DV) scaled Model with CVs
model4_scaled <- lm(vdem$`2022V_Dem_scaled` ~ vdem$Pop_cat_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022_scaled + vdem$communist, data = vdem)
coef_table4c <- tidy(model4_scaled)
kable(coef_table4c, digits = 3)
stargazer(model4, model4_scaled, type = "text",
          covariate.labels = c("Population (Small)", "Population (Large)", "Population (Huge)",
                               "Population (Small)", "Population (Large)", "Population (Huge)",
                               "GDPpc log 2022", "Island State", "Diffusion", "Diffusion scaled",
                               "Communist", "Pop (Micro)/Intercept"),
          title = "Model Comparison: Pop_cat_2022 (IV) - 2022V_Dem (DV) - Unscaled + Scaled")

stargazer(model4, model4_scaled, type = "text", title = "Model Comparison: Pop_cat_2022 (IV) - 2022V_Dem (DV) - Unscaled + Scaled")


# Next Steps - Proposals -------------------------------------------------------

# 1. Check for multicollinearity
# 2. Check for heteroskedasticity
# 3. Check for normality of residuals
# 4. Consider alternative specifications or transformations of variables
# 5. Consider interaction effects or non-linear relationships
# 6. Consider robustness checks or sensitivity analysis
# 7. Consider additional control variables or model specifications
# 8. Interpret the results and consider implications for theory


# Interaction Effects & Robustness Checks ---------------------------------

# Interaction Effects & Robustness Checks ---------------------------------

# Log-Likelihood
logLik(model5)             # For betareg

# AIC (only for betareg, as quasibinomial doesn't have it)
AIC(model5)

# Pseudo R² for Beta regression
1 - (model5$deviance / model5$null.deviance)  # McFadden's R²

# Precision parameter for Beta regression
summary(model5)$phi  # Higher values = more precise estimates# Log-Likelihood
logLik(model5)             # For betareg


# Pseudo R² for Beta regression
#1 - (model5$deviance / model5$null.deviance)  # McFadden's R²
pseudo_r2 <- 1 - (summary(model5)$deviance / summary(model5)$null.deviance)
print(pseudo_r2)

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