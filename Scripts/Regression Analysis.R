### **Regression Analysis** ###

# importing the data
library(readr)
file_path_vdem
vdem <- read_csv(file_path_vdem)

# Transformation of categorical variables ---------------------------------
is.factor(vdem$Pop_cat_2015)
# Transform into ordered factors for Ordered Logistic Regression
#install.packages("MASS")
library(MASS)
# converting 'Status' to an ordered factor
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

# 1. Tests: Simple Model: Simple Regression for 2022: Pop (IV) - V_Dem (DV) -----------
library(tidyverse)
test_model <- lm(vdem$`2022V_Dem` ~ vdem$Pop_2022, data = vdem)
summary(test_model)

range(vdem$Pop_log_2022)
range(vdem$`2022V_Dem`)

simple_model <- lm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022, data = vdem)
summary(simple_model)


install.packages("betareg")
library(betareg)
simple_model2 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022, data = vdem)
summary(simple_model2)

# without intercept
simple_model2b <- betareg(vdem$`2022V_Dem` ~ 0 + vdem$Pop_log_2022, data = vdem)
summary(simple_model2b)



# Run Fractional Logit Regression
frac_logit_model <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022, 
                        family = quasibinomial(link = "logit"), 
                        data = vdem)
# Summary of the model
summary(frac_logit_model)


# Simple Model 3 Categorical IV
table(vdem$Pop_cat_2022)
vdem$Pop_cat_2022 <- factor(vdem$Pop_cat_2022, levels = c('Micro', 'Small', 'Large', 'Huge'), ordered=TRUE)
contrasts(vdem$Pop_cat_2022) <- contr.treatment(4)
is.factor(vdem$Pop_cat_2022)
simple_model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_cat_2022, data = vdem)
summary_simple_model3 <- summary(simple_model3)
# Display the results in a cleaner format
library(broom)
library(knitr)
# Get the coefficients table
coef_table <- tidy(simple_model3)
kable(coef_table, digits = 3)
# For prettier output with stars
library(stargazer)
stargazer(simple_model3, type = "text", 
          covariate.labels = c("Population (Small)", "Population (Large)", 
                               "Population (Huge)", "Population (Micro)"))









# Plotting simple model 2
library(ggplot2)
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Simple Model: Simple Regression 2022: log_Pop (IV) - V_Dem (DV)",
       x = "log_Pop_2022",
       y = "V_Dem_2022")

# Plotting simple model 3
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`)) +
  geom_boxplot() +
  labs(title = "Simple Model: Simple Regression 2022: Pop_cat (IV) - V_Dem (DV)",
       x = "Pop_cat_2022",
       y = "V_Dem_2022")



# Create a categorical plot
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  # Add predicted means from each model
  stat_summary(fun = function(x) mean(x), 
               geom = "point", 
               shape = 23, 
               size = 4, 
               fill = "blue",
               aes(group = 1)) +
  labs(title = "Democracy Score by Population Category",
       x = "Population Category",
       y = "Democracy Score (V_Dem_2022)") +
  theme_minimal()

# Compare model results side by side
library(broom)
models_compared <- bind_rows(
  tidy(simple_model3_with_i) %>% mutate(model = "With intercept"),
  tidy(simple_model3_without_i) %>% mutate(model = "Without intercept")
)

# Display comparison table
library(knitr)
kable(models_compared)

