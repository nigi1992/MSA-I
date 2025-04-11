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

# 1. Simple Regressions: Pop 2022 (IV) - V-Dem (DV), No CV ---------------------------------------------

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
  theme_minimal()


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
  theme_grey() +
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

library(betareg)

# Define a sequence of values for the independent variable
plot_model3 <- seq(min(vdem$Pop_log_2022, na.rm = TRUE), max(vdem$Pop_log_2022, na.rm = TRUE), length.out = 175)

# Create a new data frame with fixed values for the control variables
control_values_model3 <- data.frame(
  Pop_log_2022 = plot_model3, # using the sequence of values for Pop_log_2022
  GDPpc_log_2022 = mean(vdem$GDPpc_log_2022, na.rm = TRUE), # fixing GDPpc_log_2022 at its mean
  island_state = median(vdem$island_state, na.rm = TRUE),   # fixing island_state at its median
  diffusion_2022 = mean(vdem$diffusion_2022, na.rm = TRUE), # fixing diffusion_2022 at its mean
  communist = median(vdem$communist, na.rm = TRUE)          # fixing communist at its median
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
  theme_minimal()  
# The curve first ascends and then descends, indicating a non-linear relationship between population size and democracy score.
# I suppose this is due to the missing micro states in the v-dem data set and the fact that many of the "best" democracies are 'small' or 'large'
# and have a log_pop of 15-16, whereas in the fh cure you can first see a sharp drop before it starts to ascend again, due to the 'best' democracies only 
# showing up later on.

# From afar with country labels
ggplot(control_values_model3, aes(x = Pop_log_2022, y = predicted, color = predicted)) +
  #geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  geom_point(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem`, color= `2022V_Dem`), size=0.5, alpha = 0.5) +
  geom_text(data = vdem, aes(x = Pop_log_2022, y = `2022V_Dem`, color= `2022V_Dem`,label = country_name),
            fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  labs(title = "Effect of Population (Log) on V-Dem 2022 Score",
       x = "Log of Population (2022)",
       y = "Predicted V-Dem 2022 Score",
       color = "V-Dem Score 2022") +
  theme_minimal() 

# Close with country labels
ggplot(control_values_model3, aes(x = Pop_log_2022, y = predicted, color = predicted)) +
  #geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_smooth(method = "loess", se = TRUE, span=0.75, color = "blue") +
  geom_point(data = subset(vdem, `2022V_Dem` > 0.25 & `2022V_Dem` < 0.45), aes(x = Pop_log_2022, y = `2022V_Dem`, color= `2022V_Dem`), size=0.5, alpha = 0.5) +
  geom_text(data = subset(vdem, `2022V_Dem` > 0.25 & `2022V_Dem` < 0.45), aes(x = Pop_log_2022, y = `2022V_Dem`, color= `2022V_Dem`,label = country_name),
            fontface = "bold", 
            check_overlap = TRUE, size = 3, hjust = 0.5, vjust = -0.5, show.legend = FALSE) +
  labs(title = "Effect of Population (Log) on V-Dem 2022 Score",
    x = "Log of Population (2022)",
    y = "Predicted V-Dem 2022 Score",
    color = "V-Dem Score 2022") +
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
  theme(plot.title = element_text(hjust = 0.5))


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
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5))


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
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )
#ggsave(file = here("Output", "Predicted Prob Status 2022.png"), width = 8, height = 6)




# 5. OLS Regression: Pop 2022 (IV) - FH Total Score (DV), with CVs--------

# modelC4 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 + fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)

# Define a sequence of values for the independent variable
plot_modelC <- seq(min(fh$Pop_log_2022, na.rm = TRUE), max(fh$Pop_log_2022, na.rm = TRUE), length.out = 196)

# Create a new data frame with fixed values for the control variables
control_values_modelC <- data.frame(
  Pop_log_2022 = plot_modelC, # using the sequence of values for Pop_log_2022
  GDPpc_log_2022 = mean(fh$GDPpc_log_2022, na.rm = TRUE), # fixing GDPpc_log_2022 at its mean
  island_state = median(fh$island_state, na.rm = TRUE),   # fixing island_state at its median
  diffusion_2022 = mean(fh$diffusion_fh_2022, na.rm = TRUE), # fixing diffusion_2022 at its mean
  communist = median(fh$communist, na.rm = TRUE)          # fixing communist at its median
)

# Predict the dependent variable using the model
control_values_modelC$predicted <- predict(modelC, newdata = control_values_modelC, type = "response")

# Plot
ggplot(control_values_modelC, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  labs(
    title = "Effect of Population (Log) on FH Total Score",
    x = "Log of Population (2022)",
    y = "Predicted FH Total Score"
  ) +
  theme_minimal()  


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
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Predicted FH Total Score by Pop 2022.png"), width = 8, height = 6)


ggplot(control_values_modelC, aes(x = Pop_log_2022, y = predicted)) +
  #geom_line(color = "blue", size = 1) +
  #geom_smooth(method = "loess", se = TRUE, span=1, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point(data = subset(fh, `total_fh_2022` > 50.00 & `total_fh_2022` < 70), aes(x = Pop_log_2022, y = `total_fh_2022`),size=0.5,alpha = 0.3,
             #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
             ) +
  geom_text(data = subset(fh, `total_fh_2022` > 50.00 & `total_fh_2022` < 70), aes(x = Pop_log_2022, y = `total_fh_2022`, label = country_name),
            fontface = "bold",
            #color = c("orangered", "dodgerblue2", "green4")[as.numeric(fh$`2022Status`)],
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
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(file = here("Output", "Predicted FH Total Score by Pop 2022.png"), width = 8, height = 6)

## ** To Do:** ##
# 1. Pol Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark
# 2. Civ Rights (DV) - Pop_log_2022 (IV) - OLS - CVs Benchmark
# 3. Übersichts Tabellen herstellen
# 4. (Maybe DAG)
# 5. (Maybe Adding Shapes & Sizes for CVs)
