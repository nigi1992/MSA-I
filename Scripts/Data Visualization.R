### **Regression Analysis** ###

# Loading Libraries
library(ggplot2)


# 1. Simple Model 2022, no CV ---------------------------------------------

# Plotting Model 1 log
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Simple Model: Simple Regression 2022: log_Pop (IV) - V_Dem (DV)",
       x = "log_Pop_2022",
       y = "V_Dem_2022")

# Plotting Model 1 cat
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`)) +
  geom_boxplot() +
  labs(title = "Simple Model: Simple Regression 2022: Pop_cat (IV) - V_Dem (DV)",
       x = "Pop_cat_2022",
       y = "V_Dem_2022")


# Plotting Model 1 cat with and without intercept for illustration purposes
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
# no worries if it breaks!

