### **Regression Analysis for IV FH** ###

# Cleaning environment
#rm(list = ls())

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
is.factor(fh$Pop_cat_2022)

# Transforming dummy variables into factors
fh$landlocked <- factor(fh$landlocked, levels = c(0, 1))
fh$island_state <- factor(fh$island_state, levels = c(0, 1))
fh$communist <- factor(fh$communist, levels = c(0, 1))
is.factor(fh$communist)
is.factor(fh$landlocked)
is.factor(fh$island_state)

# Same for culture variables
fh$MENA <- factor(fh$MENA, levels = c(0, 1))
fh$sub_saharan_africa <- factor(fh$sub_saharan_africa, levels = c(0, 1))
fh$latin_america <- factor(fh$latin_america, levels = c(0, 1))
fh$west_europe <- factor(fh$west_europe, levels = c(0, 1))
fh$central_asia <- factor(fh$central_asia, levels = c(0, 1))
fh$southeast_asia <- factor(fh$southeast_asia, levels = c(0, 1))
is.factor(fh$MENA)
is.factor(fh$sub_saharan_africa)
is.factor(fh$latin_america)
is.factor(fh$west_europe)
is.factor(fh$central_asia)
is.factor(fh$southeast_asia)

range(fh$Pop_log_2022)
range(fh$total_fh_2022)
range(fh$Pop_cat_2022)
range(fh$`2022Status`) # character, not ordered! -> transformation necessary! NF < PF < F
range(fh$GDPpc_log_2022)
range(fh$diffusion_fh_2022)
range(fh$area_log)
range(fh$lat_log)
range(fh$Pop_Density_2022)
range(fh$Pop_Density_log_2022)


# 1. Pop_log_2022 (IV) - FH Total Score (DV) - Simple - OLS --------------

# Model A: DV (FH_Total_Score) & IV (Log Pop)
range(fh$Pop_log_2022)
range(fh$total_fh_2022)

library(tidyverse)
modelA <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022, data = fh)
summary(modelA)

# Checking for heteroscedasticity
plot(modelA, which = 1)
# Normal Q-Q plot to check normality of residuals
plot(modelA, which = 2)


# 2. Pop_log_2022 (IV) - FH Status (DV) - Simple - Ordinal Logistic Regression polr()-------------

# Model B: DV (FH Status) & IV (Log Pop)
class(fh$`2022Status`) # character -> transformation necessary!
fh$`2015Status` <- factor(fh$`2015Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
class(fh$`2022Status`) # ordered factor
fh$`2016Status` <- factor(fh$`2016Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2017Status` <- factor(fh$`2017Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2018Status` <- factor(fh$`2018Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2019Status` <- factor(fh$`2019Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2020Status` <- factor(fh$`2020Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2021Status` <- factor(fh$`2021Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
fh$`2022Status` <- factor(fh$`2022Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
levels(fh$`2022Status`)
fh$`2023Status` <- factor(fh$`2023Status`, levels = c('NF', 'PF', 'F'), ordered = TRUE)
str(fh)

library(MASS)
modelB <- polr(fh$`2022Status` ~ fh$Pop_log_2022, data = fh, Hess = TRUE)
summary(modelB)
#This is highly significant (t-value = -5.478)
#The negative coefficient indicates that as log population increases, the probability of being in a higher freedom category (F vs PF or PF vs NF) decreases
#NF|PF: -7.3963 This is the threshold between Not Free and Partly Free categories
#PF|F: -5.9925 This is the threshold between Partly Free and Free categories


# 3. Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Benchmark (GDPpc_log_2022, island_state, diffusion_2022, communist) --------------

# Model C: DV (FH Total Score) & IV (Log Pop) with CVs (Log GDP per Capita, island_state, diffusion variable, communist)
modelC1 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 , data = fh)

modelC2 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh)

modelC3 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

modelC4 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

stargazer(modelA, modelC1, modelC2, modelC3, modelC4, type = "text", title = "Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Benchmark")
modelC <- modelC4
# Unlike V-dem Island_state has greater pos. effect and is more sig. On the other hand, diffusion variable has still positive effect, but
# much less magnitude. I suspect this is due to the many small island states


# Testing -----------------------------------------------------------------

# load required packages
library(ggplot2)
library(ggfortify)
library(car)          # for crPlots(), vif()
library(lmtest)       # for bptest()
install.packages("nortest") # for ad.test() or use shapiro.test()
library(nortest)      # for ad.test() or use shapiro.test()

# visual diagnostic plots
autoplot(modelC, which = 1:4, ncol = 2, label.id = NULL) +
  theme_bw() 
# Normality of residuals and homoscedasticity seem reasonably met
# hint of potential non-linearity in the Residuals vs Fitted plot
# Outliers: Belarus, Cuba, Venezuela, Bahrain, Brunei

# 1. Formal test of linearity (component + residual plots)
crPlots(modelC)
# linearity assumption seems appropriate for all the continuous predictors
# cat predictors show some differences between their groups in the expected direction based on the model fit, but the component+residual distributions overlap substantially, 
# suggesting their unique contribution might be relatively small compared to the continuous predictors, particularly GDP and diffusion.

# 2. Formal test of normality
shapiro.test(residuals(modelC))  # for small samples (< 5000)
# p = 0.004 -> residuals are not normally distributed

# 3. Check for multicollinearity
vif(modelC)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 2.5, indicating no multicollinearity issues

# 4. Formal test for homoscedasticity (constant variance)
bptest(modelC)  # Breusch-Pagan test
# p = 0.2 -> no heteroscedasticity present

# Interpretation quick guide:
#	Linearity: crPlots() — plots should look roughly linear.
#	Normality: shapiro.test() p > 0.05 → residuals normal.
#	Multicollinearity: vif() values < 5 → acceptable.
#	Homoscedasticity: bptest() p > 0.05 → no heteroscedasticity.


# 4. Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Extended (+ MENA, sub_saharan_africa, latin_america, west_europe, southeast_asia, central_asia) --------------

# Model D: DV (FH Total Score) & IV (Log Pop) with more regional CVs (MENA, sub_saharan_africa, latin_america, west_europe, southeast_asia, central_asia)

modelC4 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

modelD1 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe, data = fh)

modelD2 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia, data = fh)

modelD3 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia, data = fh)

stargazer(modelC, modelD1, modelD2, modelD3, type = "text", title = "Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Extended")
modelD <- modelD3


# Testing -----------------------------------------------------------------

# visual diagnostic plots
autoplot(modelD, which = 1:4, ncol = 2, label.id = NULL) +
  theme_bw()
# Normality of residuals and homoscedasticity seem reasonably met

# 1. Formal test of linearity (component + residual plots)
crPlots(modelD)
# linearity assumption seems appropriate for all the continuous predictors

# 2. Formal test of normality
shapiro.test(residuals(modelD))  # for small samples (< 5000)
# residuals are not normally distributed

# 3. Check for multicollinearity
vif(modelD)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# 4. Formal test for homoscedasticity (constant variance)
bptest(modelD)  # Breusch-Pagan test
# no heteroscedasticity present


# 5. Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Maximal (+ landlocked, area(ln), lat(ln)) --------------------------------------------------------------------

# Model E: Adding more Control Variables (landlocked, area(ln), Pop_Density, lat(ln))

modelD3 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia, data = fh)

modelE1 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia +
                fh$landlocked, data = fh)

modelE2 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia + 
                fh$landlocked + fh$area_log, data = fh)

modelE3 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia + 
                fh$landlocked + fh$area_log + fh$lat_log, data = fh)

# Again Pop Density (ln) does not show up, so I remove it from the model

stargazer(modelD3, modelE1, modelE2, modelE3, type = "text", title = "Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Maximal")
modelE <- modelE3
# lat (ln) has neg. effect. is against the theory.


# Testing -----------------------------------------------------------------

# visual diagnostic plots
autoplot(modelE, which = 1:4, ncol = 2, label.id = NULL) +
  theme_bw()
# Normality of residuals and homoscedasticity seem reasonably met

# 1. Formal test of linearity (component + residual plots)
crPlots(modelE)

# 2. Formal test of normality
shapiro.test(residuals(modelE))  # for small samples (< 5000)
# residuals are not normally distributed

# 3. Check for multicollinearity
vif(modelE)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# 4. Formal test for homoscedasticity (constant variance)
bptest(modelE)  # Breusch-Pagan test
# no heteroscedasticity present

# Interpretation quick guide:
#	Linearity: crPlots() — plots should look roughly linear.
#	Normality: shapiro.test() p > 0.05 → residuals normal.
#	Multicollinearity: vif() values < 5 → acceptable.
#	Homoscedasticity: bptest() p > 0.05 → no heteroscedasticity.

# 6. Pop_log_2022 (IV) - FH Status (DV) - polr() - CVs Benchmark (GDPpc_log_2022, island_state, diffusion_2022, communist) -------------

# Model F: DV (FH Status) & IV (Log Pop) with CVs Benchmark (Log GDP per Capita, island_state, diffusion variable, communist)
modelFa <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022, data = fh, Hess = TRUE)

modelFb <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh, Hess = TRUE)

modelFc <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh, Hess = TRUE)

modelFd <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh, Hess = TRUE)

stargazer(modelB, modelFa, modelFb, modelFc, modelFd, type = "text", title= "DV (FH Status) & IV (Log Pop) with CVs Benchmark")
modelF <- modelFd


# Testing -----------------------------------------------------------------

# Check for multicollinearity
vif(modelF)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# Formal test for homoscedasticity (constant variance)
bptest(modelF)  # Breusch-Pagan test
# no heteroscedasticity present

# Check for proportional odds assumption
install.packages("brant")
library(brant)
brant(modelF) # H0: Parallel Regression Assumption holds

install.packages("pscl")
library(pscl)
pR2(modelF) # fit is good for a cat. model


# 7. Pop_log_2022 (IV) - FH Status (DV) - polr() - CVs Extended (+ MENA, sub_saharan_africa, latin_america, west_europe, southeast_asia, central_asia) -------------

# Model G: DV (FH Total Score) & IV (Log Pop) with more regional CVs (MENA, sub_saharan_africa, latin_america, west_europe, southeast_asia, central_asia)

modelFd <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh, Hess = TRUE)

modelGa <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe, data = fh, Hess = TRUE)

modelGb <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$MENA + 
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia, data = fh, Hess = TRUE)

modelGc <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$MENA +
                   fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia, data = fh, Hess = TRUE)

stargazer(modelFd, modelGa, modelGb, modelGc, type = "text", title= "DV (FH Status) & IV (Log Pop) with CVs Extended")
modelG <- modelGc


# Testing -----------------------------------------------------------------

# Check for multicollinearity
vif(modelG)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# lots of multicollinearity issues!!

# Check for proportional odds assumption
library(brant)
brant(modelG) # H0: Parallel Regression Assumption holds

library(pscl)
pR2(modelG) # fit is good for a cat. model

# 8. Pop_log_2022 (IV) - FH Status (DV) - polr() - CVs Maximal (landlocked, area(ln), Pop_Density, lat(ln))-------------------------------------------------------------------------

# Model H: Pop_log_2022 (IV) - FH Status (DV) - polr() - CVs Maximal (landlocked, area(ln), Pop_Density, lat(ln))

modelGc <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$MENA +
                  fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + fh$central_asia, data = fh, Hess = TRUE)

modelHa <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + 
                   fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + 
                   fh$central_asia + fh$landlocked, data = fh, Hess = TRUE)

modelHb <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                   fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + 
                   fh$central_asia + fh$landlocked + fh$area_log, data = fh, Hess = TRUE)

modelHc <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist +
                   fh$MENA + fh$sub_saharan_africa + fh$latin_america + fh$west_europe + fh$southeast_asia + 
                   fh$central_asia + fh$landlocked + fh$area_log + fh$lat_log, data = fh, Hess = TRUE)

stargazer(modelGc, modelHa, modelHb, modelHc, type = "text", title= "DV (FH Status) & IV (Log Pop) with CVs Maximal")
modelH <- modelHc


# Testing -----------------------------------------------------------------

# Check for multicollinearity
vif(modelH)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# lots of multicollinearity issues!

# Check for proportional odds assumption
library(brant)
brant(modelH) # H0: Parallel Regression Assumption holds

library(pscl)
pR2(modelH) # fit is good for a cat. model


# 9. Pol Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark --------------

range(fh$`2022PR`) # There negative values. This could be a problem for OLS regression.
fh$PR_2022_recoded <- ifelse(fh$`2022PR` < 0, 0, fh$`2022PR`) # Creating a new variable to avoid overwriting original
# Three countries were impacted (China, South Sudan, Syria) and reassigned with a value of 0

# Model I: DV (2022PR) & IV (Pop_log_2022) with CVs (Log GDP per Capita, island_state, diffusion variable, communist)
modelI1 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 , data = fh)

modelI2 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh)

modelI3 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

modelI4 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

modelI5 <- lm(fh$PR_2022_recoded ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

stargazer(modelI1, modelI2, modelI3, modelI4, modelI5, type = "text", title = "Pop_log_2022 (IV) - 2022PR (DV) - OLS - CVs Benchmark")
modelI <- modelI5


# Testing -----------------------------------------------------------------

# visual diagnostic plots
autoplot(modelI, which = 1:4, ncol = 2, label.id = NULL) +
  theme_bw()
# Normality of residuals and homoscedasticity seem reasonably met

# 1. Formal test of linearity (component + residual plots)
crPlots(modelI)
# linearity assumption seems appropriate for all the continuous predictors

# 2. Formal test of normality
shapiro.test(residuals(modelI))  # for small samples (< 5000)
# residuals are not normally distributed

# 3. Check for multicollinearity
vif(modelI)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# 4. Formal test for homoscedasticity (constant variance)
bptest(modelI)  # Breusch-Pagan test
# no heteroscedasticity present

# betareg() for 2022PR ----------------------------------------------------

# betareg() option
#fh$PR_01_scaled <- fh$PR_2022_recoded / 40
#n <- nrow(fh) # Get sample size
#rm(n)
#fh$PR_01_squeezed <- (fh$PR_01_scaled * (196 - 1) + 0.5) / 196 # Squeeze to [0, 1] range

#library(betareg)
#modelI_beta_squeezed <- 
  #betareg(PR_01_squeezed ~ Pop_log_2022 + GDPpc_log_2022 + island_state + diffusion_fh_2022 + communist, data = fh, link = "logit") # logit link is common
#stargazer(modelI, modelI_beta_squeezed, type = "text", title = "Pop_log_2022 (IV) - 2022PR (DV) - OLS - CVs Benchmark")
#modelI <- modelI

# Assuming modelI_beta_squeezed is your fitted betareg model

# Generate diagnostic plots using base R plot function
# which=1:4 might select specific plots, or omit 'which' to see the default sequence
# oma and mar might need adjusting depending on your plotting window
#par(mfrow=c(2,2), oma = c(0, 0, 2, 0), mar = c(4, 4, 2, 1)) # Set up 2x2 plot layout
#plot(modelI_beta_squeezed, which = 1:4, type = "pearson") # Or type="deviance", etc.

# Reset plotting parameters when done
#par(mfrow=c(1,1), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)


# 10.`2022PR rating` (DV) - Pop_log_2022 (IV) - polr() - CVs Benchmark-----------------

# Model J: DV (2022PR rating) & IV (Pop_log_2022) with CVs Benchmark (Log GDP per Capita, island_state, diffusion variable, communist)

fh$`2022PR rating` <- factor(fh$`2022PR rating`, levels = c(7, 6, 5, 4, 3, 2, 1), ordered = TRUE)
levels(fh$`2022PR rating`)

modelJa <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022, data = fh, Hess = TRUE)

modelJb <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh, Hess = TRUE)

modelJc <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh, Hess = TRUE)

modelJd <- polr(fh$`2022PR rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh, Hess = TRUE)

stargazer(modelJa, modelJb, modelJc, modelJd, type = "text", title= "DV (2022PR rating) & IV (Pop_log_2022) with CVs Benchmark")
modelJ <- modelJd


# Testing -----------------------------------------------------------------

# Check for multicollinearity
vif(modelJ)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# Check for proportional odds assumption
library(brant)
brant(modelJ) # H0: Parallel Regression Assumption holds

library(pscl)
pR2(modelJ) # fit is good for a cat. model

# 11. Civ Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark ----------------------------------

range(fh$`2022CL`)

# Model K: DV (2022CL) & IV (Pop_log_2022) with CVs (Log GDP per Capita, island_state, diffusion variable, communist)
modelK1 <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 , data = fh)

modelK2 <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh)

modelK3 <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

modelK4 <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

stargazer(modelK1, modelK2, modelK3, modelK4, type = "text", title = "Pop_log_2022 (IV) - 2022CL (DV) - OLS - CVs Benchmark")
modelK <- modelK4

# Interestingly the negative relationship seems to be much stronger and stat. sig. with Civ rights than with Pol rights. 
# Exactly the opposite of Gerrig & Zarecki (2011), where Civ rights were not significant and weakest. Could it be that in the liberal dimension
# the relationship is indeed neg. whereas in the electoral dimension it is positive? this would allow for a more nuanced interpretation of the correlation
# between pop size and democracy levels.


# Testing -----------------------------------------------------------------

# visual diagnostic plots
autoplot(modelK, which = 1:4, ncol = 2, label.id = NULL) +
  theme_bw()
# Normality of residuals and homoscedasticity seem reasonably met

# 1. Formal test of linearity (component + residual plots)
crPlots(modelK)
# linearity assumption seems appropriate for all the continuous predictors

# 2. Formal test of normality
shapiro.test(residuals(modelK))  # for small samples (< 5000)
# residuals are not normally distributed

# 3. Check for multicollinearity
vif(modelK)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# 4. Formal test for homoscedasticity (constant variance)
bptest(modelK)  # Breusch-Pagan test
# no heteroscedasticity present


# 12. `2022CL rating` (DV) - Pop_log_2022 (IV) - polr() - CVs Benchmark-----------------

fh$`2022CL rating` <- factor(fh$`2022CL rating`, levels = c(7, 6, 5, 4, 3, 2, 1), ordered = TRUE)
levels(fh$`2022CL rating`)

# Model L: DV (2022CL rating) & IV (Pop_log_2022) with CVs Benchmark (Log GDP per Capita, island_state, diffusion variable, communist)

modelLa <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022, data = fh, Hess = TRUE)

modelLb <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh, Hess = TRUE)

modelLc <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh, Hess = TRUE)

modelLd <- polr(fh$`2022CL rating` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh, Hess = TRUE)

stargazer(modelLa, modelLb, modelLc, modelLd, type = "text", title= "DV (2022PR rating) & IV (Pop_log_2022) with CVs Benchmark")
modelL <- modelLd

# Same here, very strong sig. neg. relationship. There is something here.


# Testing -----------------------------------------------------------------

# Check for multicollinearity
vif(modelL)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

brant(modelL) # H0: Parallel Regression Assumption holds

pR2(modelL) # fit is good for a cat. model


# 13. FH Total Score (DV) - Pop_cat_2022 (IV) - OLS - CVs Benchmark-------------------------------------------------------------------------

# Model M: DV (FH Total Score) & IV (Pop_cat_2022) with CVs Benchmark (Log GDP per Capita, island_state, diffusion variable, communist)

contrasts(fh$Pop_cat_2022) <- contr.treatment(4)

modelMa <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022, data = fh)
coef_tableA <- tidy(modelMa)
kable(coef_tableA, digits = 3)

modelMb <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh)
coef_tableB <- tidy(modelMb)
kable(coef_tableB, digits = 3)

modelMc <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)
coef_tableC <- tidy(modelMc)
kable(coef_tableC, digits = 3)

modelMd <- lm(fh$total_fh_2022 ~ fh$Pop_cat_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)
coef_tableD <- tidy(modelMd)
kable(coef_tableD, digits = 3)

stargazer(modelMa, modelMb, modelMc, modelMd, type = "text",
          covariate.labels = c("Population (Small)", "Population (Large)", 
                               "Population (Huge)", "GDPpc log 2022", "Island State", 
                               "Diffusion", "Communist", "Pop (Micro)/Intercept"),
          title = "Pop_cat (IV) - FH Total Score (DV) - OLS - CVs Benchmark")
modelM <- modelMd


# Testing -----------------------------------------------------------------

# visual diagnostic plots
autoplot(modelM, which = 1:4, ncol = 2, label.id = NULL) +
  theme_bw()
# Normality of residuals and homoscedasticity seem reasonably met

# 1. Formal test of linearity (component + residual plots)
crPlots(modelM)
# linearity assumption seems appropriate for all the continuous predictors

# 2. Formal test of normality
shapiro.test(residuals(modelM))  # for small samples (< 5000)
# residuals are not normally distributed

# 3. Check for multicollinearity
vif(modelM)   # Variance Inflation Factor, should be < 5 (preferably < 2.5)
# VIF values are all < 5, indicating no multicollinearity issues

# 4. Formal test for homoscedasticity (constant variance)
bptest(modelM)  # Breusch-Pagan test
# no heteroscedasticity present


# 14. Model Comparisons ----------------------------------------------------

library(tidyverse)
#install.packages("betareg") 
library(betareg)
library(broom)
library(knitr)
#install.packages("MASS")
library(MASS)
library(stargazer)

## Only FH Variables
# Pop_log_2022 (IV) - FH Total Score (DV) - OLS
stargazer(modelC, modelD, modelE, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - FH Total Score (DV) - OLS")

# Pop_log_2022 (IV) - FH Status (DV) - polr()
stargazer(modelF, modelG, modelH, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - FH Status (DV) - polr()")

# Pop_log_2022 (IV) - various FH DVs - All Benchmark
stargazer(modelC, modelF, modelI, modelJ, modelK, modelL, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various FH DVs - All Benchmark")


# FH & Vdem Variables (unscaled) ------------------------------------------

## Continuous-IV Tests Summary FH & Vdem
# Benchmark
stargazer(model3, modelC, modelF, modelI, modelJ, modelK, modelL, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark")

# Extended 
stargazer(model5, modelD, modelG, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - CV Extended")

# Maximal
stargazer(model6, modelE, modelH, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - CV Maximal")


## Categorical-IV Tests Summary FH & Vdem
stargazer(model4, modelM, type = "text", 
          covariate.labels = c("Population (Small)", "Population (Large)", "Population (Huge)",
          "GDPpc log 2022", "Island State", "Diffusion", "Communist",
          "Population (Small)", "Population (Large)", "Population (Huge)",
          "GDPpc log 2022", "Island State", "Diffusion",  "Communist",
          "Pop (Micro)/Intercept"),
          title = "Pop_cat (IV) - DV: 2022 V_dem betareg() & FH Total Score (OLS) - CVs Benchmark")


# FH & Vdem Variables (scaled) --------------------------------------------

## Continuous-IV Tests Summary FH & Vdem
# Benchmark
stargazer(model7, modelC, modelF, modelI, modelJ, modelK, modelL, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark")

# Extended 
stargazer(model9, modelD, modelG, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - CV Extended")

# Maximal
stargazer(model10, modelE, modelH, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - CV Maximal")


## Categorical-IV Tests Summary FH & Vdem
stargazer(model8, modelM, type = "text", 
          covariate.labels = c("Population (Small)", "Population (Large)", "Population (Huge)",
                               "GDPpc log 2022", "Island State", "Diffusion scaled", "Communist",
          "Population (Small)", "Population (Large)", "Population (Huge)",
          "GDPpc log 2022", "Island State", "Diffusion",  "Communist",
          "Pop (Micro)/Intercept"),
          title = "Pop_cat (IV) - DV: 2022 V_dem betareg() & FH Total Score (OLS) - CVs Benchmark")



# 15. Saving Tables --------------------------------------------------------

library(here)

## Saving with stargazer
library(stargazer)
# Benchmark - table 1
stargazer(model7, modelC, modelF, modelI, modelJ, modelK, modelL,
          type = "text", # Output format is text
          out = here("Output", "Tables", "table1.txt"), # Specify the output file name
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark")

# Extended - table 3
stargazer(model9, modelD, modelG, type = "text",
          out = here("Output", "Tables", "table3.txt"), # Specify the output file name
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - CV Extended")

# Maximal - table 4
stargazer(model10, modelE, modelH, type = "text",
          out = here("Output", "Tables", "table4.txt"), # Specify the output file name
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - CV Maximal")


## Same DV, but all model specifications
# Vdem all model specifications
stargazer(model7, model9, model10,
          type = "text", # Output format is text
          out = here("Output", "Tables", "Vdem_All_Spec_Stargazer.txt"), # Specify the output file name
          title = "Pop_log_2022 (IV) - Vdem (DV) - All Model Specifications")

# FH Total score all model specifications
stargazer(modelC, modelD, modelE,
          type = "text", # Output format is text
          out = here("Output", "Tables", "FH_Total_All_Spec_Stargazer.txt"), # Specify the output file name
          title = "Pop_log_2022 (IV) - FH Total Score (DV) - All Model Specifications")

# FH Status all model specifications 
stargazer(modelF, modelG, modelH,
          type = "text", # Output format is text
          out = here("Output", "Tables", "FH_Status_All_Spec_Stargazer.txt"), # Specify the output file name
          title = "Pop_log_2022 (IV) - FH Status (DV) - All Model Specifications")


# Categorical-IV Tests Summary FH & Vdem
# Benchmark - table 2
stargazer(model4, modelM, type = "text", 
          covariate.labels = c("Population (Small)", "Population (Large)", "Population (Huge)",
                               "GDPpc log 2022", "Island State", "Diffusion scaled", "Communist",
                               "Population (Small)", "Population (Large)", "Population (Huge)",
                               "GDPpc log 2022", "Island State", "Diffusion",  "Communist",
                               "Pop (Micro)/Intercept"),
          out = here("Output", "Tables", "table2.txt"), # Specify the output file name
          title = "Pop_cat (IV) - DV: 2022 V_dem betareg() & FH Total Score (OLS) - CVs Benchmark")

## For comparision, low count of Covariates
# table 5
stargazer(model3a, modelC1, modelC2, modelFa, modelFb, modelI1, modelI2,
          type = "text", # Output format is text
          out = here("Output", "Tables", "table5.txt"), 
          title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - Few Covariates")

# table 6
stargazer(modelMa, modelMb, type = "text", 
          covariate.labels = c("Population (Small)", "Population (Large)", "Population (Huge)",
                               "GDPpc log 2022", "Island State",
                               "Pop (Micro)/Intercept"),
          out = here("Output", "Tables", "table6.txt"), # Specify the output file name
          title = "Pop_cat (IV) - DV: 2022 FH Total Score (OLS) - Few Covariates")

# Model summary -----------------------------------------------------------

library(modelsummary)
modelsummary(
  list("Model 1" = model7, "Model 2" = modelC, "Model 3" = modelF, 
       "Model 4" = modelI, "Model 5" = modelJ, "Model 6" = modelK, "Model 7" = modelL),
  #output = here("Output", "Tables", "All_Benchmark_Tinytable.txt"),
  title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark",
  stars = TRUE)

modelsummary(
  list("2022V_Dem_scaled" = model7, "total_fh_2022" = modelC, "2022Status" = modelF, 
       "PR_2022_recoded" = modelI, "2022PR rating" = modelJ, "2022CL" = modelK, "2022CL rating" = modelL),
  #output = here("Output", "Tables", "All_Benchmark_Tinytable.txt"),
  title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark",
  stars = TRUE)

modelsummary(
  list("2022V_Dem" = model4, "total_fh_2022" = modelM),
  #output = here("Output", "Tables", "Cat_Benchmark_Tinytable.txt"),
  title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark",
  stars = TRUE)

modelsummary(
  list("2022V_Dem" = model4, "total_fh_2022" = modelM),
  #output = here("Output", "Tables", "Cat_Benchmark_Tinytable.txt"),
  title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark",
  stars = TRUE)

modelsummary(
  list("G" = modelG, "H" = modelH),
  #output = here("Output", "Tables", "Cat_Benchmark_Tinytable.txt"),
  title = "Model Comparison - Pop_log_2022 (IV) - various DVs FH & Vdem - All Benchmark",
  stars = TRUE)


