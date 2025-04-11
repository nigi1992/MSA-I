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
is.factor(fh$Pop_cat_2022)

range(fh$Pop_log_2022)
range(fh$total_fh_2022)
range(fh$Pop_cat_2022)
range(fh$`2022Status`) # character, not ordered! -> transformation necessary! NF < PF < F
range(fh$GDPpc_log_2022)
range(fh$island_state)
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

stargazer(modelC1, modelC2, modelC3, modelC4, type = "text", title = "Pop_log_2022 (IV) - FH Total Score (DV) - OLS - CVs Benchmark")
modelC <- modelC4
# Unlike V-dem Island_state has greater pos. effect and is more sig. On the other hand, diffusion variable has still positive effect, but
# much less magnitude. I suspect this is due to the many small island states

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

# 6. Pop_log_2022 (IV) - FH Status (DV) - polr() - CVs Benchmark (GDPpc_log_2022, island_state, diffusion_2022, communist) -------------

# Model F: DV (FH Status) & IV (Log Pop) with CVs Benchmark (Log GDP per Capita, island_state, diffusion variable, communist)
modelFa <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022, data = fh, Hess = TRUE)

modelFb <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh, Hess = TRUE)

modelFc <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh, Hess = TRUE)

modelFd <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh, Hess = TRUE)

stargazer(modelFa, modelFb, modelFc, modelFd, type = "text", title= "DV (FH Status) & IV (Log Pop) with CVs Benchmark")
modelF <- modelFd

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

# 9. Pol Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark --------------

range(fh$`2022PR`)

# Model I: DV (2022PR) & IV (Pop_log_2022) with CVs (Log GDP per Capita, island_state, diffusion variable, communist)
modelI1 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 , data = fh)

modelI2 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state, data = fh)

modelI3 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022, data = fh)

modelI4 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

stargazer(modelI1, modelI2, modelI3, modelI4, type = "text", title = "Pop_log_2022 (IV) - 2022PR (DV) - OLS - CVs Benchmark")
modelI <- modelI4


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


# 14. Model Comparison ----------------------------------------------------

library(tidyverse)
#install.packages("betareg") 
library(betareg)
library(broom)
library(knitr)
#install.packages("MASS")
library(MASS)
library(stargazer)
# Pop_log_2022 (IV) - FH Total Score (DV) - OLS
stargazer(modelC, modelD, modelE, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - FH Total Score (DV) - OLS")

# Pop_log_2022 (IV) - FH Status (DV) - polr()
stargazer(modelF, modelG, modelH, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - FH Status (DV) - polr()")

# Pop_log_2022 (IV) - various FH DVs - All Benchmark
stargazer(modelC, modelF, modelI, modelJ, modelK, modelL, type = "text", 
          title = "Model Comparison - Pop_log_2022 (IV) - various FH DVs - All Benchmark")


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
          #covariate.labels = c("Population (Small)", "Population (Large)", 
                     #          "Population (Huge)", "GDPpc log 2022", "Island State", 
                       #        "Diffusion", "Communist", "Pop (Micro)/Intercept"),
          title = "Pop_cat (IV) - DV: 2022V_demFH betareg() & Total Score (OLS) - CVs Benchmark")


# adding all cultures, regions, sub-regions------------------------------------------

model5h_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$landlocked + 
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$culture, data = fh)

model5i_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$landlocked +
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$region, data = fh)

model5j_fh <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist + fh$landlocked +
                   fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$sub_region, data = fh)

stargazer(model5h_fh, model5i_fh, model5j_fh, type = "text")



# adding all cultures, regions, sub-regions-------------------------------------------------------------------------

model6h_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + 
                     fh$communist + fh$landlocked + fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$culture, data = fh)

model6i_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 +
                     fh$communist + fh$landlocked + fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$region, data = fh)

model6j_fh <- polr(fh$`2022Status` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 +
                     fh$communist + fh$landlocked + fh$area_log + fh$lat_log + fh$Pop_Density_2022 + fh$sub_region, data = fh)

stargazer(model6h_fh, model6i_fh, model6j_fh, type = "text")

