### **Regression Analysis** ###

# Loading Libraries
library(ggplot2)
library(here)

# 1. Simple Regressions: Pop 2022 (IV) - V-Dem (DV), No CV ---------------------------------------------

# Plotting Model 1: Pop_log_2022
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Democracy Score (V-Dem) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "V-Dem LDI Score for 2022") +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting Model 2: Pop_cat_2022
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`)) +
  geom_boxplot() +
  labs(title = "Democracy Score (V-Dem) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "V-Dem LDI Score for 2022") +
  theme(plot.title = element_text(hjust = 0.5))
  

# Plotting Model 1 Pop_cat_2022 with and without intercept for illustration purposes
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
  labs(title = "Democracy Score (V-Dem) by Population Category with and without Intercept",
       x = "Population Category",
       y = "Democracy Score (V_Dem_2022)") +
  theme_grey()
# no worries if it breaks!


# 2. betareg() Regression: Pop 2022 (IV) - V-Dem (DV), with CVs --------

library(ggplot2)
library(betareg)

# Define a sequence of values for the independent variable
pop_seq <- seq(min(vdem$Pop_log_2022, na.rm = TRUE), max(vdem$Pop_log_2022, na.rm = TRUE), length.out = 100)

# Create a new data frame with fixed values for the control variables
control_values <- data.frame(
  Pop_log_2022 = pop_seq, 
  GDPpc_log_2022 = mean(vdem$GDPpc_log_2022, na.rm = TRUE), # fixing GDPpc_log_2022 at its mean
  island_state = median(vdem$island_state, na.rm = TRUE),   # fixing island_state at its median
  diffusion_2022 = mean(vdem$diffusion_2022, na.rm = TRUE), # fixing diffusion_2022 at its mean
  communist = median(vdem$communist, na.rm = TRUE)          # fixing communist at its median
)

# Predict the dependent variable using the model
control_values$predicted <- predict(model3, newdata = control_values, type = "response")

# Create the ggplot
ggplot(control_values, aes(x = Pop_log_2022, y = predicted)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of Population (Log) on V-Dem 2022 Score",
    x = "Log of Population (2022)",
    y = "Predicted V-Dem 2022 Score"
  ) +
  theme_minimal()  


# 3. Simple Regressions: Pop 2022 (IV) - 2022 FH Total Score (DV), No CV --------------

# Plotting Model A: Pop_log_2022
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$total_fh_2022)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Democracy Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Total Freedom House Score for 2022")  +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting Model M: Pop_cat_2022
ggplot(fh, aes(x = Pop_cat_2022, y = `total_fh_2022`)) +
  geom_boxplot() +
  labs(title = "Democracy Score (V-Dem) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "Total Freedom House Score for 2022") +
  theme(plot.title = element_text(hjust = 0.5))

# 4. Ordinal Logistic Regression polr(): DV (FH Status) & IV (Log Pop) --------

library(tidyr)
library(ggplot2)
library(tidyverse)

# creating a new data frame for predictions
plot_fh_DV_status <- data.frame(Pop_log_2022 = seq(min(fh$Pop_log_2022), 
                                                        max(fh$Pop_log_2022), 
                                                        length.out = 196))
# predicting probabilities
plot_fh_DV_status$Status_prob <- predict(modelB, newdata = plot_fh_DV_status, type = "probs")
predicted_probs <- predict(modelB, newdata = plot_fh_DV_status, type = "probs")
# adding the predicted probabilities to new_data 
plot_fh_DV_status[c("NF", "PF", "F")] <- predicted_probs
# reshaping the data to long format for ggplot2
plot_fh_DV_status_long <- tidyr::pivot_longer(plot_fh_DV_status, 
                                     cols = c("NF", "PF", "F"),
                                     names_to = "Status",
                                     values_to = "Probability")
# Plotting 
ggplot(plot_fh_DV_status_long, aes(x = Pop_log_2022, y = Probability, color = Status)) +
  geom_smooth(size = 1.2, method= "loess", span = 0.75, se= FALSE) +
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


