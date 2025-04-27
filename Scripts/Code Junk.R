# 5b. Diffusion Variable for V-Dem with Distance Threshold and Combo with exponential decay----------------------

## trying with distance threshold
# Computing inverse distance weights (excluding self-distance)
inverse_distance_vdem2 <- 1 / (distance_matrix_vdem + 1)  # Adding 1 to avoid division by zero
diag(inverse_distance_vdem2) <- 0  # Setting diagonal (self-distance) to 0

# Setting a distance threshold of 5000 km
inverse_distance_vdem2[distance_matrix_vdem > 5000] <- 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

df_vdem2 <- df_vdem

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem2 <- inverse_distance_vdem2 %*% v_dem_scores
  total_weights_vdem2 <- rowSums(inverse_distance_vdem2)
  
  # Adding diffusion variable with proper naming 
  df_vdem2 <- df_vdem2 %>%
    mutate(
      !!paste0("diffusion2_", year) := as.numeric(weighted_sum_vdem2 / total_weights_vdem2)
    )
}
head(df_vdem2$diffusion_2015)
head(df_vdem2$diffusion2_2015)
summary(df_vdem2$diffusion_2015)
summary(df_vdem2$diffusion2_2015)

# The diffusion variable with a distance threshold of 4500 km seems to have a lower mean and median compared to the original diffusion variable.

## With distance threshold and exponential decay

# Distance threshold for very remote islands
distance_threshold <- 5000

# Exponential decay parameter
lambda <- 0.001

# Option 1: Computing inverse distance weights with distance threshold and exponential decay
inverse_distance_vdem3 <- ifelse(
  distance_matrix_vdem > distance_threshold, 
  0,  # Setting weight to 0 if distance exceeds threshold
  exp(-lambda * distance_matrix_vdem)  # Applying exponential decay
)

# Option 2: Computing inverse distance weights with only exponential decay
lambda <- 0.001
inverse_distance_vdem3 <- exp(-lambda * distance_matrix_vdem)  # Applying exponential decay


# Excluding self-distance
diag(inverse_distance_vdem3) <- 0  # Setting diagonal (self-distance) to 0

# Defining time frame for diffusion variable
years_vdem <- 2015:2023

df_vdem3 <- df_vdem2

# Looping through each year
for (year in years_vdem) {
  # Generating correct column name 
  v_dem_col <- paste0(year, "V_Dem")
  
  # Extracting democracy scores as a numeric vector
  v_dem_scores <- df_vdem[[v_dem_col]]
  
  # Computing weighted average (ensures matrices are numeric)
  weighted_sum_vdem3 <- inverse_distance_vdem3 %*% v_dem_scores
  total_weights_vdem3 <- rowSums(inverse_distance_vdem3)
  
  # Adding diffusion variable with proper naming 
  df_vdem3 <- df_vdem3 %>%
    mutate(
      !!paste0("diffusion3_", year) := as.numeric(weighted_sum_vdem3 / total_weights_vdem3)
    )
}

head(df_vdem3$diffusion_2015)
head(df_vdem3$diffusion2_2015)
head(df_vdem3$diffusion3_2015)
summary(df_vdem3$diffusion_2015)
summary(df_vdem3$diffusion2_2015)
summary(df_vdem3$diffusion3_2015)



# . Pop_log_2022 (IV) - V_Dem (DV) - felm() - CVs Extended ----------------

# In R, the scale() function is used to standardise or centre the data. It can be used to scale the columns of a matrix or data frame,
# so that they have zero mean and unit variance, often useful in data preprocessing for statistical analysis or machine learning. 
# By default, it centres the data but you can specify scale = FALSE to only centre or center = FALSE to only scale.
install.packages("lfe")
library(lfe)
#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

model3_felm_log_base <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 | 0 |0 | iso3, data = vdem)

model5_felm_log_max <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log | 0 |0 | iso3, data = vdem)

model5_felm_log_fixed_base <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 | vdem$sub_region |0 | iso3, data = vdem)

model5_felm_log_fixed_max <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log | vdem$sub_region |0 | iso3, data = vdem)

model5_felm_log_fixed_max_max <- felm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$communist + vdem$area_log + vdem$Pop_Density_2022 + vdem$lat_log | vdem$sub_region |0 | iso3, data = vdem)

# Alternative approach using lapply to work with a list of models
models_list <- list(model5d, model5f, model5g, model5_felm_log_fixed_base, model5_felm_log_fixed_max, model5_felm_log_fixed_max_max)
valid_models <- models_list[sapply(models_list, function(x) !inherits(tryCatch(summary(x), error = function(e) e), "error"))]

# Using only valid models in stargazer
stargazer(valid_models, 
          type = "text", 
          title = "Simple Regression 2022: Pop (IV) - V_Dem (DV)")
# The results are worse in many ways, going back to betareg() models, maybe try glm()
model5 <- model5d


# . CVs region, subregion culture vdem ----------------------------------------------------

#model3, model5, model5d, model5e
#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)
#model5d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$communist + vdem$area_log , data = vdem)
#model5e <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$communist + vdem$area_log + vdem$lat_log, data = vdem)
#model5f <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$communist + vdem$area_log +  vdem$lat_log + vdem$Pop_Density_2022, data = vdem)

# Adding more cultural variables
unique(df_vdem$culture)
table(df_vdem$culture)
model6a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$communist + vdem$area_log, data = vdem) 

model6b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + vdem$area_log + vdem$culture, data = vdem)

stargazer(model3, model5d, model6a, model6b, type = "text", title = "Pop_log (IV) - V_Dem (DV) with more CVs 2022")

# Adding regional variables
table(df_vdem$region)
table(df_vdem$Asia)
table(df_vdem$Europe)
table(df_vdem$Africa)
table(df_vdem$Americas)
table(df_vdem$Oceania)

model6c <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + area_log + vdem$region, data = vdem)

# Adding sub region variables
unique(df_vdem$sub_region)
table(df_vdem$sub_region)
names(df_vdem)[(ncol(df_vdem)-50):ncol(df_vdem)] # names of last 50 columns

#model6d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + vdem$srEast_Central_Europe + 
#                    vdem$srWestern_Europe +vdem$srNordic + vdem$srSouthern_Europe + vdem$srBalkans + vdem$srNorth_Africa + vdem$srCentral_Asia + vdem$srEast_Asia + vdem$srMiddle_East + 
#                   vdem$srSouth_Asia + vdem$srSoutheast_Asia + vdem$srWest_Africa + vdem$srEast_Africa + vdem$srCentral_Africa + vdem$srSouthern_Africa + vdem$srCarribbean + 
#                  vdem$srCentral_America + vdem$srSouth_America + vdem$srOceania, data = vdem)

model6d <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + area_log + vdem$sub_region, data = vdem)

stargazer(model5, model6a, model6b, model6c, model6d, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

model6 <- model6a


# Best Results of this round
stargazer(model3, model5, model6, model6b, type = "text", title = "Pop_log (IV) - V_Dem (DV) with max CVs 2022")

#model3 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, data = vdem)

#model5 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$communist + vdem$area_log , data = vdem)

#model6a <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$communist + vdem$area_log, data = vdem) 

#model6b <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + vdem$area_log + vdem$culture, data = vdem)

# Interaction Effects & Robustness Checks glm() ---------------------------------

# Try with glm() function
model3_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022, family = quasibinomial(link = "logit"), data = vdem)

model5_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$landlocked + vdem$communist + vdem$area_log , family = quasibinomial(link = "logit"), data = vdem)

model6_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$MENA + vdem$sub_saharan_africa + vdem$latin_america + vdem$west_europe + vdem$southeast_asia + vdem$central_asia + vdem$landlocked + vdem$communist + vdem$area_log, family = quasibinomial(link = "logit"), data = vdem)

model6b_fractional <- glm(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 + vdem$GDPpc_log_2022 + vdem$island_state + vdem$diffusion_2022 + vdem$landlocked + vdem$communist + vdem$area_log + vdem$culture, family = quasibinomial(link = "logit"), data = vdem)

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



# . Illustration Generation (Experimental) ----------------------------------------------

# Install necessary packages if you haven't already
# install.packages("ggplot2")
# install.packages("ggforce")
# install.packages("patchwork")

# Load the libraries
library(ggplot2)
library(ggforce) # For geom_circle
library(patchwork) # For combining plots

# --- Define data for the circles ---
# We estimate positions and sizes based on the image.
# x0, y0 are center coordinates, r is the radius.
circle_past <- data.frame(x0 = 5, y0 = 5, r = 2.5)
circle_future <- data.frame(x0 = 6, y0 = 3.5, r = 1.5) # Smaller, lower, slightly right

# --- Create the "Past" Plot ---
p_past <- ggplot() +
  # Set plot limits and maintain aspect ratio (makes circles look like circles)
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10), expand = FALSE) +
  
  # Add the circle for the "Past"
  geom_circle(data = circle_past, aes(x0 = x0, y0 = y0, r = r),
              fill = NA, color = "black", linewidth = 0.8) +
  
  # Add the diagonal dashed line (approx. y = x)
  # Using geom_segment for more control over start/end points
  geom_segment(aes(x = 1, y = 1, xend = 9, yend = 9),
               linetype = "dotted", color = "black", linewidth = 0.6) +
  
  # Add labels and title
  labs(title = "Past",
       x = "Heterogeneity Costs",
       y = "Scale Advantages") +
  
  # Apply a minimal theme and customize
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
    axis.text = element_blank(),        # Remove axis number labels
    axis.ticks = element_blank(),       # Remove axis ticks
    panel.grid = element_blank(),       # Remove grid lines
    axis.title = element_text(size = 10) # Adjust axis title size
  )

# --- Create the "Future" Plot ---
p_future <- ggplot() +
  # Set plot limits and maintain aspect ratio
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10), expand = FALSE) +
  
  # Add the circle for the "Future"
  geom_circle(data = circle_future, aes(x0 = x0, y0 = y0, r = r),
              fill = NA, color = "black", linewidth = 0.8) +
  
  # Add the diagonal solid line (less steep than the past one, based on image)
  # Adjust y-coordinates to make it less steep (e.g., starts lower, ends lower for same x range)
  geom_segment(aes(x = 1, y = 2, xend = 9, yend = 7),
               linetype = "solid", color = "black", linewidth = 0.6) +
  
  # Add labels and title (remove y-axis label for combined plot)
  labs(title = "Future",
       x = "Heterogeneity Costs",
       y = NULL) + # Set y label to NULL
  
  # Apply minimal theme and customize
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(), # Completely remove y-axis title space
    axis.title.x = element_text(size = 10)
  )

# --- Combine the plots side-by-side ---
combined_plot <- p_past + p_future

# --- Add overall title and caption using patchwork ---
final_plot <- combined_plot +
  plot_annotation(
    title = "Changing Dynamics of State Formation",
    caption = "Source: Based on Lambais and Breiding 2019; Alesina and Spolaore 2005.\nNote: Axes represent qualitative 'Low' to 'High' ranges.",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.caption = element_text(hjust = 0, size = 8)) # Left-align caption
  )

# --- Display the final plot ---
print(final_plot)

# --- Optional: Save the plot ---
# ggsave("state_formation_graph.png", plot = final_plot, width = 8, height = 4.5, dpi = 300)  


# Interaction Effects ----------------
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
# Interaction between Pop_log_2022 and Island_state
model3_inter4 <- betareg(vdem$`2022V_Dem` ~ vdem$Pop_log_2022 * vdem$island_state + vdem$GDPpc_log_2022 + vdem$communist + 
                           vdem$diffusion_2022, data = vdem)

summary(model3_inter1) 
# The interaction between population and GDP per capita is not significant, meaning the effect of population size on democracy is not conditional on GDP per capita
summary(model3_inter2)
# The interaction between population and democratic diffusion is not significant, meaning the effect of population size on democracy does not depend on the regional democratic environment.
summary(model3_inter3)
# The interaction between population and communist regime form is significant, meaning the effect of population size on democracy is at least partially conditional on the type of regime.
cor(vdem$Pop_log_2022, as.numeric(vdem$communist)) # no sig. cor.
summary(model3_inter4) # The interaction between population and island state is not significant, meaning the effect of population size on democracy is not conditional on being an island state.

stargazer(model3, model3_inter3, type="text")

# Interaction Effects ----------------
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

# The felm() function in R, provided by the "lfe" package, is used to fit linear models with large fixed effects. 
# It is particularly useful for handling panel data or models with multiple group fixed effects, allowing for efficient estimation and inference in high-dimensional fixed effects regression settings.

# The plm() function in R, provided by the "plm" package, is used to fit linear models with panel data. 
# It is particularly useful for handling panel data or models with multiple group fixed effects, allowing for efficient estimation and inference in high-dimensional fixed effects regression settings.

# Interaction Effects -----------------------------------------------------

# Interaction between Pop_log_2022 and GDPpc_log_2022
modelC_inter1 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 * fh$GDPpc_log_2022 + fh$island_state + fh$diffusion_fh_2022 + fh$communist, data = fh)
# Interaction between Pop_log_2022 and diffusion_2022
modelC_inter2 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 * fh$diffusion_fh_2022 + fh$island_state + fh$GDPpc_log_2022 + fh$communist, data = fh)
# Interaction between Pop_log_2022 and communist
modelC_inter3 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 * fh$communist + fh$island_state + fh$diffusion_fh_2022 + fh$GDPpc_log_2022, data = fh)
# Interaction between Pop_log_2022 and island_state
modelC_inter4 <- lm(fh$total_fh_2022 ~ fh$Pop_log_2022 * fh$island_state + fh$diffusion_fh_2022 + fh$GDPpc_log_2022 + fh$communist, data = fh)

summary(modelC_inter1) 
# The interaction between population size and GDP per capita is marginally significant, suggesting the effect of population might depend on GDP per capita.
summary(modelC_inter2)
# The interaction term has a very high p-value (0.870), meaning no interaction effect.
summary(modelC_inter3)
# The interaction term is negative and significant
summary(modelC_inter4) # interatction term not significant and positive

stargazer(modelC, modelC_inter3, type = "text")
par(mfrow = c(2,2))
plot(modelC_inter3)
anova(modelC, modelC_inter3)





