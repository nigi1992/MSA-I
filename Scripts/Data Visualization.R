### **Data Visualization** ###


# FH Ranking For illustration and explanation purposes ---------------------------------------------------------------

# create ranking of countries their fh scores, log and log pop to  compare plausibility
# empty dataframe for ranking
ranking_fh_score <- fh
ranking_fh_pop <- fh

ranking_fh_score$rank_score <- rank(-fh$total_fh_2022, na.last =TRUE, ties.method = "first")
ranking_fh_pop$rank_pop <- rank(-fh$Pop_log_2022, na.last =TRUE)

#install.packages("dplyr")
library(dplyr)
ranking_fh_score <- ranking_fh_score %>% 
  select(c(country_name, rank_score, total_fh_2022))

ranking_fh_pop <- ranking_fh_pop %>% 
  select(c(country_name, rank_pop, Pop_log_2022, Pop_2022, Pop_cat_2022))

ranking_fh <- left_join(ranking_fh_score, ranking_fh_pop, by = "country_name")


# V-Dem Ranking For illustration and explanation purposes-------------------

# create ranking of countries their v-dem scores, log and log pop to  compare plausibility
ranking_vdem_score <- vdem
ranking_vdem_pop <- vdem

ranking_vdem_score$rank_score <- rank(-vdem$`2022V_Dem`, na.last =TRUE, ties.method = "first")
ranking_vdem_pop$rank_pop <- rank(-vdem$Pop_log_2022, na.last =TRUE)

library(dplyr)
ranking_vdem_score <- ranking_vdem_score %>% 
  select(c(country_name, rank_score, `2022V_Dem`))

ranking_vdem_pop <- ranking_vdem_pop %>% 
  select(c(country_name, rank_pop, Pop_log_2022, Pop_2022, Pop_cat_2022))

ranking_vdem <- left_join(ranking_vdem_score, ranking_vdem_pop, by = "country_name")


# Loading libraries -------------------------------------------------------

# Loading Libraries
library(ggplot2)
library(here)

# 1. Simple Regressions: Pop 2022 (IV) - V-Dem (DV), No CV (unscaled) ---------------------------------------------

# Plotting Model 1: Pop_log_2022
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Democracy Score (V-Dem) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "V-Dem LDI Score for 2022") +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting Model 1: Pop_log_2022 with color and country labels
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem`, color=`2022V_Dem`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem`, label = country_name), 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Democracy Score (V-Dem) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "V-Dem LDI Score for 2022",
       color = "V-Dem Score 2022") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()


# Simple Regressions: Pop 2022 (IV) - V-Dem (DV), No CV (scaled) ----------

# Plotting Model 1: Pop_log_2022
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem_scaled`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Democracy Score (V-Dem) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "V-Dem LDI Score for 2022") +
  #theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (V-Dem) by Population Size (ln).png"), width = 8, height = 6)

# Plotting Model 1: Pop_log_2022 with color and country labels
ggplot(vdem, aes(x = Pop_log_2022, y = `2022V_Dem_scaled`, color=`2022V_Dem`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem_scaled`, label = country_name), 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Democracy Score (V-Dem) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "V-Dem LDI Score for 2022",
       color = "V-Dem Score 2022") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()
ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (V-Dem) by Population Size (ln) - Complex.png"), width = 8, height = 6)


# Pop_cat_2022 ------------------------------------------------------------

# Plotting Model 2: Pop_cat_2022
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`)) +
  geom_boxplot() +
  labs(title = "Democracy Score (V-Dem) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "V-Dem LDI Score for 2022") +
  theme(plot.title = element_text(hjust = 0.5))
  
# Plotting Model 2: Pop_cat_2022 with color and country labels
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`, color=Pop_cat_2022)) +
  geom_boxplot(alpha=0.7)+
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_text(data = 
              #subset(vdem, `2022V_Dem` > 0.65 | `2022V_Dem` < 0.175), 
            vdem, aes(x = Pop_cat_2022, y = `2022V_Dem`, label = country_name),
            color = "black", fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("Micro"="green4", "Small" = "gold", "Large" = "orange2", "Huge" = "orangered2")
  ) +
  scale_fill_manual(
    name = "Population Categories", # Legend title (matches color)
    values = c("Micro"="green4", "Small" = "gold", "Large" = "orange2", "Huge" = "orangered2")
  ) +
  labs(title = "Democracy Score (V-Dem) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "V-Dem LDI Score for 2022",
       color = "Population Categories") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (V-Dem) by Population Category (Ordered Logit).png"), width = 8, height = 6)


# Plotting Model 2 (scaled V-dem score): Pop_cat_2022 with color and country labels
ggplot(vdem, aes(x = Pop_cat_2022, y = `2022V_Dem_scaled`, color=Pop_cat_2022)) +
  geom_boxplot(alpha=0.7)+
  geom_jitter(width = 0.2, alpha = 0.75) +
  geom_text(data = 
              #subset(vdem, `2022V_Dem` > 0.65 | `2022V_Dem` < 0.175), 
              vdem, aes(x = Pop_cat_2022, y = `2022V_Dem_scaled`, label = country_name),
            color = "black", fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("Micro"="green4", "Small" = "gold", "Large" = "orange2", "Huge" = "orangered2")
  ) +
  scale_fill_manual(
    name = "Population Categories", # Legend title (matches color)
    values = c("Micro"="green4", "Small" = "gold", "Large" = "orange2", "Huge" = "orangered2")
  ) +
  labs(title = "Democracy Score (V-Dem) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "V-Dem LDI Score for 2022",
       color = "Population Categories") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (V-Dem) by Population Category (Ordered Logit).png"), width = 8, height = 6)


# Plotting Model 1 Pop_cat_2022 with and without intercept for illustration purposes------------------------

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


# 2. betareg() Regression: Pop 2022 (IV) - V-Dem (DV), with CVs Benchmark (unscaled) --------

library(betareg)

# Define a sequence of values for the independent variable
plot_model3 <- seq(min(vdem$Pop_log_2022, na.rm = TRUE), max(vdem$Pop_log_2022, na.rm = TRUE), length.out = 175)

# Calculate modes (simplest way for 0/1: use table)
island_mode <- as.numeric(names(which.max(table(vdem$island_state))))
communist_mode <- as.numeric(names(which.max(table(vdem$communist))))

control_values_model3 <- data.frame(
  Pop_log_2022 = plot_model3, 
  GDPpc_log_2022 = mean(vdem$GDPpc_log_2022, na.rm = TRUE), 
  island_state = island_mode, # Fix at mode
  diffusion_2022 = mean(vdem$diffusion_2022, na.rm = TRUE), 
  communist = communist_mode # Fix at mode
)

# Predict the dependent variable using the model
control_values_model3$predicted <- predict(model3, newdata = control_values_model3, type = "response")

# Plotting the predicted values
ggplot(control_values_model3, aes(x = Pop_log_2022, y = predicted)) +
  #geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  labs(
    title = "Effect of Population (Log) on V-Dem 2022 Score",
    x = "Log of Population (2022)",
    y = "Predicted V-Dem 2022 Score"
  ) +
  theme_bw()  
# The curve first ascends and then descends, indicating a non-linear relationship between population size and democracy score.
# I suppose this is due to the missing micro states in the v-dem data set and the fact that many of the "best" democracies are 'small' or 'large'
# and have a log_pop of 15-16, whereas in the fh cure you can first see a sharp drop before it starts to ascend again, due to the 'best' democracies only 
# showing up later on.

# From afar with country labels
ggplot(control_values_model3, aes(x = Pop_log_2022, y = predicted, color = predicted)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  geom_point(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem`, color= `2022V_Dem`), size=0.5, alpha = 0.5) +
  geom_text(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem`, color= `2022V_Dem`,label = country_name),
            fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  labs(title = "Effect of Population (Log) on V-Dem 2022 Score",
       x = "Log of Population (2022)",
       y = "Predicted V-Dem 2022 Score",
       color = "V-Dem Score 2022") +
  theme_bw()
ggsave(file = here("Output", "Plots", "Relationship", "Effect of Population (Log) on V-Dem 2022 Score.png"), width = 8, height = 6)


# Close with country labels
ggplot(control_values_model3, aes(x = Pop_log_2022, y = predicted)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  geom_point(data = subset(vdem, `2022V_Dem` >= 0.25 & `2022V_Dem` <= 0.45), aes(x = Pop_log_2022, y = `2022V_Dem`), size=0.5, alpha = 0.5) +
  geom_text(data = subset(vdem, `2022V_Dem` >= 0.25 & `2022V_Dem` <= 0.45), aes(x = Pop_log_2022, y = `2022V_Dem`,label = country_name),
            fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  labs(title = "Effect of Population (Log) on V-Dem 2022 Score",
    x = "Log of Population (2022)",
    y = "Predicted V-Dem 2022 Score",
    color = "V-Dem Score 2022") +
  theme_bw() 


# betareg() Regression: Pop 2022 (IV) - V-Dem (DV), with CVs Benchmark (scaled) --------

library(betareg)

# Define a sequence of values for the independent variable
plot_model3_scaled <- seq(min(vdem$Pop_log_2022, na.rm = TRUE), max(vdem$Pop_log_2022, na.rm = TRUE), length.out = 175)

# Calculate modes (simplest way for 0/1: use table)
island_mode <- as.numeric(names(which.max(table(vdem$island_state))))
communist_mode <- as.numeric(names(which.max(table(vdem$communist))))

control_values_model3_scaled <- data.frame(
  Pop_log_2022 = plot_model3_scaled, 
  GDPpc_log_2022 = mean(vdem$GDPpc_log_2022, na.rm = TRUE), 
  island_state = island_mode, # Fix at mode
  diffusion_2022_scaled = mean(vdem$diffusion_2022_scaled, na.rm = TRUE), 
  communist = communist_mode # Fix at mode
)

# Predict the dependent variable using the model
control_values_model3_scaled$predicted <- predict(model3_scaled, newdata = control_values_model3_scaled, type = "response")

# Plotting the predicted values
ggplot(control_values_model3_scaled, aes(x = Pop_log_2022, y = predicted)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  labs(
    title = "Effect of Population (Log) on V-Dem 2022 Score",
    x = "Log of Population (2022)",
    y = "Predicted V-Dem 2022 Score"
  ) +
  theme_bw()  

# From afar with country labels
ggplot(control_values_model3_scaled, aes(x = Pop_log_2022, y = predicted)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  geom_point(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem_scaled`, color=`2022V_Dem_scaled`), size=0.5, alpha = 0.5) +
  geom_text(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem_scaled`, color= `2022V_Dem_scaled`,label = country_name),
            fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  labs(title = "Effect of Population (Log) on V-Dem 2022 Score",
       x = "Log of Population (2022)",
       y = "Predicted V-Dem 2022 Score",
       color = "V-Dem Score 2022") +
  theme_bw() 

# Close with country labels
ggplot(control_values_model3_scaled, aes(x = Pop_log_2022, y = predicted)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  geom_point(data = subset(vdem, `2022V_Dem_scaled` >= 25 & `2022V_Dem_scaled` <= 45), 
             aes(x = Pop_log_2022, y = `2022V_Dem_scaled`), size=0.5, alpha = 0.5) +
  geom_text(data = subset(vdem, `2022V_Dem_scaled` >= 25 & `2022V_Dem_scaled` <= 45), 
            aes(x = Pop_log_2022, y = `2022V_Dem_scaled`,label = country_name),
            fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  labs(title = "Effect of Population (Log) on V-Dem 2022 Score",
       x = "Log of Population (2022)",
       y = "Predicted V-Dem 2022 Score",
       color = "V-Dem Score 2022") +
  theme_bw() 


# 3. Simple Regressions: Pop 2022 (IV) - 2022 FH Total Score (DV), No CV --------------

# Plotting Model A: Pop_log_2022
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$total_fh_2022)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Democracy Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Total Freedom House Score for 2022")  +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting Model A: Pop_log_2022 with country names
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$total_fh_2022)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = `total_fh_2022`, label = country_name),
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Democracy Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Total Freedom House Score for 2022")  +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (FH) by Population Size (ln).png"), width = 8, height = 6)


# Plotting Model A: Pop_log_2022 with country names and color by 2022Status
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$total_fh_2022, color = `2022Status`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = `total_fh_2022`, label = country_name),
            #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "blue1") +
  labs(title = "Democracy Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Total Freedom House Score for 2022",
       color = "FH Status 2022")  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (FH) by Population Size (ln) - Status colored.png"), width = 8, height = 6)


# Pop_cat_2022 ------------------------------------------------------------

# Plotting Model M: Pop_cat_2022
ggplot(fh, aes(x = Pop_cat_2022, y = `total_fh_2022`)) +
  geom_boxplot() +
  labs(title = "Democracy Score (V-Dem) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "Total Freedom House Score for 2022") +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting Model M: Pop_cat_2022 with color and country names
ggplot(fh, aes(x = Pop_cat_2022, y = `total_fh_2022`, color = `Pop_cat_2022`)) +
  geom_boxplot(alpha=0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_text(data = fh, aes(x = Pop_cat_2022, y = `total_fh_2022`, label = country_name),
            color = "black", fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("Micro"="green4", "Small" = "gold", "Large" = "orange2", "Huge" = "orangered2")
  ) +
  labs(title = "Democracy Score (FH) by Population Category (Ordered Logit)",
       x = "2022 Population Categories",
       y = "Total Freedom House Score for 2022",
       color = "Population Categories") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Democracy Score (FH) by Population Category (Ordered Logit).png"), width = 8, height = 6)


# 4. Ordinal Logistic Regression polr(): DV (FH Status) & IV (Log Pop), No CVs --------

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
  #geom_smooth(size = 1.2, method= "loess", span = 0.75, se= TRUE) +
  geom_smooth(method = "lm", se = TRUE) +
  #geom_point(size=0.75) +
  labs(title = "Predicted Probabilities by 2022 Population Size",
       x = "Log Population 2022",
       y = "Predicted Probability") +
  scale_color_manual(
    values = c("NF" = "orangered", "PF" = "dodgerblue2", "F" = "green4"),
    labels = c("NF" = "Not Free", "PF" = "Partly Free", "F" = "Free")
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file = here("Output", "Plots", "Relationship", "Predicted Prob Status 2022.png"), width = 8, height = 6)


# 5. OLS Regression: Pop 2022 (IV) - FH Total Score (DV), with CVs Benchmark--------

# modelC4 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

# Define a sequence of values for the independent variable
plot_modelC <- seq(min(fh$Pop_log_2022, na.rm = TRUE), max(fh$Pop_log_2022, na.rm = TRUE), length.out = 196)

# Calculate modes (simplest way for 0/1: use table)
island_mode <- as.numeric(names(which.max(table(fh$island_state))))
communist_mode <- as.numeric(names(which.max(table(fh$communist))))

control_values_modelC <- data.frame(
  Pop_log_2022 = plot_modelC, 
  GDPpc_log_2022 = mean(fh$GDPpc_log_2022, na.rm = TRUE), 
  island_state = island_mode, # Fix at mode
  diffusion_fh_2022 = mean(fh$diffusion_fh_2022, na.rm = TRUE), 
  communist = communist_mode # Fix at mode
)

# Predict the dependent variable using the model
control_values_modelC$predicted <- predict(modelC, newdata = control_values_modelC, type = "response")

# Plot
ggplot(control_values_modelC, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  labs(
    title = "Effect of Population (Log) on predicted FH Total Score",
    x = "Log of Population (2022)",
    y = "Predicted FH Total Score"
  ) +
  theme_bw()  


ggplot(control_values_modelC, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = fh, aes(x = Pop_log_2022, y = `total_fh_2022`),size=0.5,alpha = 0.3,
             color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = `total_fh_2022`, label = country_name),
            fontface = "bold",
            color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(
    title = "Effect of Population (Log) on FH Total Score",
    x = "Log of Population (2022)",
    y = "Predicted FH Total Score",
    color = "FH Status 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Predicted FH Total Score by Pop 2022.png"), width = 8, height = 6)


ggplot(control_values_modelC, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = subset(fh, `total_fh_2022` >= 50.00 & `total_fh_2022` <= 70), aes(x = Pop_log_2022, y = `total_fh_2022`),size=0.5,alpha = 0.3,
             #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
             ) +
  geom_text(data = subset(fh, `total_fh_2022` >= 50.00 & `total_fh_2022` <= 70), aes(x = Pop_log_2022, y = `total_fh_2022`, label = country_name),
            fontface = "bold",
            #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  labs(
    title = "Effect of Population (Log) on FH Total Score",
    x = "Log of Population (2022)",
    y = "Predicted FH Total Score",
    color = "FH Status 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Predicted FH Total Score by Pop 2022.png"), width = 8, height = 6)

plot(modelC, which = 1) # Residuals vs Fitted


# 6.  Pol Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark-------------------

# modelI4 <- lm(fh$`2022PR` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

# Plotting Pol Rights (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022PR`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022")  +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Pol Rights (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022PR`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022PR`, label = country_name),
            color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022",
       color = "FH Status 2022")  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Political Rights Score (FH) by Population Size (ln) - Status colored.png"), width = 8, height = 6)


ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022PR`)) +
  geom_point(size=0.5,alpha = 0.3,
             #color = c("purple3", "royalblue2", "dodgerblue1", "yellow1", "yellowgreen", "green4")[as.numeric(fh$`2022PR rating`)],
             #check_overlap = TRUE, size = 0.5, hjust = 0.5, vjust = -0.5, alpha=0.2, show.legend = FALSE
             ) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022PR`, label = country_name),
            color = c("purple3", "royalblue2", "dodgerblue1", "yellow1", "yellowgreen", "green4")[as.numeric(fh$`2022PR rating`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE
            ) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  #scale_color_manual(
  #  values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
  #  labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  #) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022",
       color = "PR Rating 2022")  +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Plots", "Relationship", "Political Rights Score (FH) by Population Size (ln) - Rating colored.png"), width = 8, height = 6)


# Plotting Pol Rights (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022PR`, color= fh$`2022PR`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022PR`, label = country_name),
            #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022",
       color = "PR Rights 2022"
       )  +
  theme(plot.title = element_text(hjust = 0.5))


# No negative PR Values ---------------------------------------------------

# modelI5 <- lm(fh$PR_2022_recoded ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

# Plotting Pol Rights (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$PR_2022_recoded)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022")  +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Pol Rights (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$PR_2022_recoded)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$PR_2022_recoded, label = country_name),
            color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022",
       color = "FH Status 2022")  +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Pol Rights (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$PR_2022_recoded, color= fh$PR_2022_recoded)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$PR_2022_recoded, label = country_name),
            #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Political Rights Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Political Rights Score (PR) for 2022",
       color = "PR Rights 2022"
  )  +
  theme(plot.title = element_text(hjust = 0.5))

# Prediction Plots with CVs Benchmark -------------------------------------

# Do it like above betareg() model CV for vdem and fh total score with predicted values

# install.packages("ggplot2") # Uncomment and run if you haven't installed it
library(ggplot2)
# Also potentially useful packages
install.packages("ggfortify") # For easy diagnostic plots
install.packages("ggeffects") # For effect plots
# install.packages("broom") # For tidying model output
library(ggfortify)
library(ggeffects)
library(broom)

# Generate standard diagnostic plots (Residuals vs Fitted, Q-Q, Scale-Location, Residuals vs Leverage)
autoplot(modelI, which = 1:4, ncol = 2, label.id = NULL) + # Use which=1:6 for all 6 plots
  theme_bw() # Optional: apply a cleaner theme

# modelI5 <- lm(fh$PR_2022_recoded ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

# Define a sequence of values for the independent variable
plot_modelI <- seq(min(fh$Pop_log_2022, na.rm = TRUE), max(fh$Pop_log_2022, na.rm = TRUE), length.out = 196)

# Calculate modes (simplest way for 0/1: use table)
island_mode <- as.numeric(names(which.max(table(fh$island_state))))
communist_mode <- as.numeric(names(which.max(table(fh$communist))))

control_values_modelI <- data.frame(
  Pop_log_2022 = plot_modelI, 
  GDPpc_log_2022 = mean(fh$GDPpc_log_2022, na.rm = TRUE), 
  island_state = island_mode, # Fix at mode
  diffusion_fh_2022 = mean(fh$diffusion_fh_2022, na.rm = TRUE), 
  communist = communist_mode # Fix at mode
)

# Predict the dependent variable using the model
control_values_modelI$predicted <- predict(modelI, newdata = control_values_modelI, type = "response")

# Plot
ggplot(control_values_modelI, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  labs(
    title = "Effect of Population (ln) on Political Rights Score (PR)",
    x = "Population Size (ln) for 2022",
    y = "Predicted Political Rights Score (PR) for 2022"
  ) +
  theme_bw()  


ggplot(control_values_modelI, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = fh, aes(x = Pop_log_2022, y = `2022PR`),size=0.5,alpha = 0.3,
             color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = `2022PR`, label = country_name),
            fontface = "bold",
            color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(
    title = "Effect of Population (Log) on Political Rights Score",
    x = "Log of Population (2022)",
    y = "Predicted Political Rights Score (PR) for 2022",
    color = "FH Status 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Predicted PR Score by Pop 2022.png"), width = 8, height = 6)
#ggsave(file = here("Output", "Predicted FH Total Score by Pop 2022.png"), width = 8, height = 6)
  
  
ggplot(control_values_modelI, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = subset(fh, `2022PR` >= 19 & `2022PR` <= 27), aes(x = Pop_log_2022, y = `2022PR`),size=0.5,alpha = 0.3
  ) +
  geom_text(data = subset(fh, `2022PR` >= 19 & `2022PR` <= 27), aes(x = Pop_log_2022, y = `2022PR`, label = country_name),
            fontface = "bold",
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  labs(
    title = "Effect of Population (Log) on PR Score",
    x = "Log of Population (2022)",
    y = "Predicted Political Rights Score (PR) for 2022",
    color = "FH Status 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Predicted FH Total Score by Pop 2022.png"), width = 8, height = 6)

#plot(modelI, which = 1) # Residuals vs Fitted


# 7.  Civ Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark------------

# modelK4 <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)
range(fh$`2022CL`)

# Plotting Civil Liberties (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022CL`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Civil Liberties Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Civil Liberties Score (CL) for 2022")  +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Civil Liberties (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022CL`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022CL`, label = country_name),
            color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(title = "Civil Liberties Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Civil Liberties Score (CL) for 2022",
       color = "FH Status 2022")  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Civil Liberties Score (FH) by Population Size (ln) - Status colored.png"), width = 8, height = 6)



# Plotting Civil Liberties (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022CL`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022CL`, label = country_name),
            color = c("purple3", "royalblue2", "dodgerblue1", "yellow1", "yellowgreen", "green4")[as.factor(fh$`2022CL rating`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  scale_color_manual(
    #values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    #labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(title = "Civil Liberties Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Civil Liberties Score (CL) for 2022",
       color = "CL Rating 2022")  +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Plots", "Relationship", "Civil Liberties Score (FH) by Population Size (ln) - Ratings colored.png"), width = 8, height = 6)


# Plotting Civil Liberties (DV) - Pop_log_2022 (IV) - Simple - No CVs
ggplot(fh, aes(x = fh$Pop_log_2022, y = fh$`2022CL`, color= fh$`2022CL`)) +
  geom_point(size=0.5,alpha = 0.3) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022CL`, label = country_name),
            #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.25) +
  labs(title = "Civil Liberties Score (FH) by Population Size (ln)",
       x = "Population Size (ln) for 2022",
       y = "Civil Liberties Score (CL) for 2022",
       color = "PR Rights 2022"
  )  +
  theme(plot.title = element_text(hjust = 0.5))


# Prediction Plots with CVs Benchmark----------------------------

# Generate standard diagnostic plots (Residuals vs Fitted, Q-Q, Scale-Location, Residuals vs Leverage)
autoplot(modelK, which = 1:4, ncol = 2, label.id = NULL) + # Use which=1:6 for all 6 plots
  theme_bw() # Optional: apply a cleaner theme

# modelK4 <- lm(fh$`2022CL` ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

# Define a sequence of values for the independent variable
plot_modelK <- seq(min(fh$Pop_log_2022, na.rm = TRUE), max(fh$Pop_log_2022, na.rm = TRUE), length.out = 196)

# Calculate modes (simplest way for 0/1: use table)
island_mode <- as.numeric(names(which.max(table(fh$island_state))))
communist_mode <- as.numeric(names(which.max(table(fh$communist))))

control_values_modelK <- data.frame(
  Pop_log_2022 = plot_modelK, 
  GDPpc_log_2022 = mean(fh$GDPpc_log_2022, na.rm = TRUE), 
  island_state = island_mode, # Fix at mode
  diffusion_2022 = mean(fh$diffusion_fh_2022, na.rm = TRUE), 
  communist = communist_mode # Fix at mode
)

# Predict the dependent variable using the model
control_values_modelK$predicted <- predict(modelK, newdata = control_values_modelK, type = "response")

# Plot
ggplot(control_values_modelK, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  labs(
    title = "Civil Liberties Score (CL) by Population Size (ln)",
    x = "Population Size (ln) for 2022",
    y = "Predicted Civil Liberties Score (CL) for 2022"
  ) +
  theme_bw()  


ggplot(control_values_modelK, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = fh, aes(x = Pop_log_2022, y = fh$`2022CL`),size=0.5,alpha = 0.3,
             color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],) +
  geom_text(data = fh, aes(x = Pop_log_2022, y = fh$`2022CL`, label = country_name),
            fontface = "bold",
            color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  scale_color_manual(
    values = c("F" = "green4", "PF" = "dodgerblue2", "NF" = "orangered"),
    labels = c("F" = "Free", "PF" = "Partly Free", "NF" = "Not Free")
  ) +
  labs(
    title = "Civil Liberties Score (CL) by Population Size (ln)",
    x = "Log of Population (2022)",
    y = "Predicted Civil Liberties Score (CL) for 2022",
    color = "FH Status 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Relationship", "Predicted CL Score by Pop 2022.png"), width = 8, height = 6)


ggplot(control_values_modelK, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = subset(fh, `2022CL` >= 30 & `2022CL` <= 40), aes(x = Pop_log_2022, y = `2022CL`),size=0.5,alpha = 0.3
  ) +
  geom_text(data = subset(fh, `2022CL` >= 30 & `2022CL` <= 40), aes(x = Pop_log_2022, y = `2022CL`, label = country_name),
            fontface = "bold",
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, alpha=2, show.legend = FALSE) +
  labs(
    title = "Civil Liberties Score (CL) by Population Size (ln)",
    x = "Log of Population (2022)",
    y = "Predicted Civil Liberties Score (CL) for 2022",
    color = "FH Status 2022") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# 8. DAG ------------------------------------------------------------------

install.packages("dagitty")
#install.packages("ggdag")
library(dagitty)
library(ggdag)

# Simple DAG
dag_text <- "dag {
Pop [exposure,pos=\"-0.5,0\"]
DemScore [outcome,pos=\"0.5,0\"]
Island [pos=\"-1,-1\"]
GDPpc [pos=\"-1,1\"]
Diffu [pos=\"1,1\"]
Commu [pos=\"1,-1\"]
Pop -> DemScore
Island -> DemScore
GDPpc -> DemScore
Diffu -> DemScore
Commu -> DemScore}"
g <- dagitty(dag_text)
ggdag(g, layout = "auto") + theme_dag_grey() + theme(legend.position = "right") +
  ggtitle("DAG of effect of Population Size on Democracy Score with Covariates")
#ggsave(file = here("Output", "Plots", "DAGs", "simple dag.png"), width = 8, height = 6, dpi = 300)


# Labeled DAG
dag_full_text <- 
"dag {
  Population [exposure,pos=\"-0.5,0\"]
  Democracy_Score [outcome,pos=\"0.5,0\"]
  Island_State [pos=\"-1,-1\"]
  GDPpc [pos=\"-1,1\"]
  Diffusion_Variable [pos=\"1,1\"]
  Communist_Regime [pos=\"1,-1\"]
  Population -> Democracy_Score
  Island_State -> Democracy_Score
  GDPpc -> Democracy_Score
  Diffusion_Variable -> Democracy_Score
  Communist_Regime -> Democracy_Score
}"
g_full <- dagitty(dag_full_text)

# Plotting with extra labels
ggdag(g_full, layout = "auto") +
  geom_dag_point(size = 16, color="black") + # REDUCED NODE SIZE SIGNIFICANTLY
  geom_dag_edges() +                       # Removed check_overlap, not needed here
  geom_dag_label(aes(label = name),
                   size = 5, label.padding = unit(0.5, "lines"),
                   label.r = unit(0.15, "lines"), alpha = 0.7, hjust = 0.575, vjust = -0.35, check_overlap = TRUE,
                   fill = "lightblue", color = "black" ) +# Adjusted text size for smaller node
  theme_dag_blank() +                      # Use a theme without axes/gridlines
  ggtitle("DAG of effect of Population Size on Democracy Score with Covariates") +
  #theme_dag() +
  theme(plot.title = element_text(hjust = 0.5)) # Center title
ggsave(file = here("Output", "Plots", "DAGs", "dag blue label.png"), width = 12, height = 7, dpi = 300)


# Plotting with blacked out nodes
ggdag(g_full, layout = "auto") +
  geom_dag_point(size = 16, color="black") + # REDUCED NODE SIZE SIGNIFICANTLY
  geom_dag_edges() +                       # Removed check_overlap, not needed here
  geom_dag_text(aes(label = name),
                  color = "black",           # Text color
                  size = 4,
                  vjust = -3.5
                  ) + 
  theme_dag_blank() +                      # Use a theme without axes/gridlines
  ggtitle("DAG of effect of Population Size on Democracy Score with Covariates") +
  theme(plot.title = element_text(hjust = 0.5)) # Center title 
ggsave(file = here("Output", "Plots", "DAGs", "dag simple.png"), width = 10, height = 7, dpi = 300)


# 9. Descriptive Statistics - Plots -----------------------------------------------

# Descriptive statistics for the dataset
# Load necessary libraries
library(ggplot2)

### V-Dem

# Distribution of Population
ggplot(vdem, aes(x = Pop_log_2022)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Population (Log - V-Dem)",
       x = "Log of Population (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of V-Dem Population.png"), width = 8, height = 6)


# Dist of Population Category
ggplot(vdem, aes(x = Pop_cat_2022)) +
  geom_bar(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Population Categories (V-Dem)",
       x = "Population Category (2022)",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of V-Dem Population Categories.png"), width = 8, height = 6)

table(vdem$Pop_cat_2022)


# Dist of V-Dem Democracy Score
ggplot(vdem, aes(x = `2022V_Dem`)) +
  geom_histogram(binwidth = 0.05, fill = "violet", color = "black", alpha = 0.7) +
  labs(title = "Distribution of V-Dem Democracy Score (2022)",
       x = "V-Dem Democracy Score (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of V-Dem Score.png"), width = 8, height = 6)


summary(vdem$`2022V_Dem`)

# Dist of V-Dem Democracy Score
ggplot(vdem, aes(x = `2022V_Dem_scaled`)) +
  geom_histogram(binwidth = 5, fill = "violet", color = "black", alpha = 0.7) +
  labs(title = "Distribution of V-Dem Democracy Score (2022)",
       x = "V-Dem Democracy Score (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

summary(vdem$`2022V_Dem_scaled`)

## Dist of Benchmark CV for V-Dem

# Dist of GDP per Capita
ggplot(vdem, aes(x = GDPpc2022)) +
  geom_histogram(binwidth = 5000, fill = "green3", color = "black", alpha = 0.7) +
  labs(title = "Distribution of GDP per Capita (2022 - V-Dem)",
       x = "GDP per Capita (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Dist of Island
ggplot(vdem, aes(x = island_state)) +
  geom_bar(fill = "brown", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Island States (V-Dem)",
       x = "Island State",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Dist of Communism
ggplot(vdem, aes(x = communist)) +
  geom_bar(fill = "red2", color = "black", alpha = 0.7) +
  labs(title = "Distribution of former & current communist states (V-Dem)",
       x = "Communism",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


### FH
# Distribution of Population
ggplot(fh, aes(x = Pop_log_2022)) +
  geom_histogram(binwidth = 1, fill = "green1", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Population (Log - FH)",
       x = "Log of Population (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Population.png"), width = 8, height = 6)

# Dist of Population Category
ggplot(fh, aes(x = Pop_cat_2022)) +
  geom_bar(fill = "green1", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Population Categories (FH)",
       x = "Population Category (2022)",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Population Categories.png"), width = 8, height = 6)

table(fh$Pop_cat_2022)


## Dist of Democracy Scores
# Dist of FH Total Score
ggplot(fh, aes(x = `total_fh_2022`)) +
  geom_histogram(binwidth = 5, fill = "yellow1", color = "black", alpha = 0.5) +
  labs(title = "Distribution of Freedom House Total Score (2022)",
       x = "Freedom House Total Score (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Total Score.png"), width = 8, height = 6)

summary(fh$total_fh_2022)

# Dist of FH Status
ggplot(fh, aes(x = `2022Status`)) +
  geom_bar(fill = "yellow1", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Freedom House Status (2022)",
       x = "Freedom House Status (2022)",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Status.png"), width = 8, height = 6)
table(fh$`2022Status`)

# Dist of FH Political Rights
ggplot(fh, aes(x = `PR_2022_recoded`)) +
  geom_histogram(binwidth = 2.5, fill = "green4", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Freedom House Political Rights Score (2022)",
       x = "Freedom House Political Rights Score (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Political Rights Score.png"), width = 8, height = 6)
summary(fh$`PR_2022_recoded`)

# Dist of FH PR rating
ggplot(fh, aes(x = `2022PR rating`)) +
  geom_bar(fill = "green4", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Freedom House Political Rights Rating (2022)",
       x = "Freedom House Political Rights Rating (2022)",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Political Rights Rating.png"), width = 8, height = 6)
table(fh$`2022PR rating`)

# Dist of FH Civil Liberties
ggplot(fh, aes(x = `2022CL`)) +
  geom_histogram(binwidth = 3.75, fill = "lightblue1", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Freedom House Civil Liberties Score (2022)",
       x = "Freedom House Civil Liberties Score (2022)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Civil Liberties Score.png"), width = 8, height = 6)
summary(fh$`2022CL`)

# Dist of FH CL rating
ggplot(fh, aes(x = `2022CL rating`)) +
  geom_bar(fill = "lightblue1", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Freedom House Civil Liberties Rating (2022)",
       x = "Freedom House Civil Liberties Rating (2022)",
       y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = here("Output", "Plots", "Distribution", "Distribution of FH Civil Liberties Rating.png"), width = 8, height = 6)
table(fh$`2022CL rating`)


# 10. Descriptive Statistics - Tables -------------------------------------

## Population
# Dist of Population
summary(vdem$Pop_log_2022)
summary(fh$Pop_log_2022)

# Dist of Population Category
table(fh$Pop_cat_2022)
table(vdem$Pop_cat_2022)

## Democracy Scores
# V-Dem
# Dist of V-Dem Democracy Score
summary(vdem$`2022V_Dem`)

# Dist of V-Dem Democracy Score Scaled
summary(vdem$`2022V_Dem_scaled`)

# FH
# Dist of FH Total Score
summary(fh$total_fh_2022)

# Dist of FH Status
table(fh$`2022Status`)

# Dist of FH Political Rights
summary(fh$`PR_2022_recoded`)

# Dist of FH PR rating
table(fh$`2022PR rating`)

# Dist of FH Civil Liberties
summary(fh$`2022CL`)

# Dist of FH CL rating
table(fh$`2022CL rating`)


## Distribtution of Controls
# Dist of GDP per Capita
summary(vdem$GDPpc_log_2022)
summary(fh$GDPpc_log_2022)

# Dist of Island Status
table(vdem$island_state)
table(fh$island_state)

# Dist of Diffusion
summary(vdem$diffusion_2022)
summary(vdem$diffusion_2022_scaled)
summary(fh$diffusion_fh_2022)

# Dist of Communism
summary(vdem$communist)
summary(fh$communist)

# Dist Region
table(vdem$MENA)
table(vdem$sub_saharan_africa)
table(vdem$west_europe)
table(vdem$latin_america)
table(vdem$southeast_asia)
table(vdem$central_asia)

table(fh$MENA)
table(fh$sub_saharan_africa)
table(fh$west_europe)
table(fh$latin_america)
table(fh$southeast_asia)
table(fh$central_asia)

# Dist of Landlocked
table(vdem$landlocked)
table(fh$landlocked)

# Dist of Area
summary(vdem$area_log)
summary(fh$area_log)

# Dist of Latitude
summary(vdem$lat_log)
summary(fh$lat_log)


# 11. Descriptive Statistics - Correlation Plots ---------------------------

# V-Dem
# Correlation plot of predictor variables


