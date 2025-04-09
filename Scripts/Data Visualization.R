### **Regression Analysis** ###

# Loading Libraries
library(ggplot2)
library(here)

# 1. Simple Model 2022 DV V-Dem, no CV ---------------------------------------------

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


# Plotting Model 1 Cat with and without intercept for illustration purposes
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

# Simple OLS Regression: DV (FH Total Score) & IV (Log Pop) --------------

ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$total_fh_2022)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "FH Total Score vs Population (log)",
       x = "Log Population 2022",
       y = "FH Total Score 2022") +
  theme_minimal()


# Ordinal Logistic Regression polr(): DV (FH Status) & IV (Log Pop) --------

library(tidyr)
library(ggplot2)
library(tidyverse)

# creating a new data frame for predictions
plot_fh_DV_status <- data.frame(Pop_log_2022 = seq(min(fh$Pop_log_2022), 
                                                        max(fh$Pop_log_2022), 
                                                        length.out = 196))
# predicting probabilities
plot_fh_DV_status$Status_prob <- predict(model2_fh, newdata = plot_fh_DV_status, type = "probs")
predicted_probs <- predict(model2_fh, newdata = plot_fh_DV_status, type = "probs")
# adding the predicted probabilities to new_data 
plot_fh_DV_status[c("NF", "PF", "F")] <- predicted_probs
# reshaping the data to long format for ggplot2
plot_fh_DV_status_long <- tidyr::pivot_longer(plot_fh_DV_status, 
                                     cols = c("NF", "PF", "F"),
                                     names_to = "Status",
                                     values_to = "Probability")
# Plotting 
ggplot(plot_fh_DV_status_long, aes(x = Pop_log_2022, y = Probability, color = Status)) +
  geom_smooth(size = 0.8, method= "loess", span = 1, se= FALSE) +
  #geom_smooth(size = 1.2, method = "loess", span = 2, se = FALSE) +
  labs(title = "Predicted Probabilities by 2022 Population Size",
       x = "Log Population 2022",
       y = "Predicted Probability") +
  scale_color_manual(
    values = c("NF" = "orangered", "PF" = "dodgerblue2", "F" = "green4"),
    labels = c("NF" = "Not Free", "PF" = "Partly Free", "F" = "Free")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file = here("Output", "Predicted Prob Status 2022.png"), width = 8, height = 6)

  