library(tidyverse)  # Data wrangling and visualization
library(lmtest)  # Diagnostic tests for regression models
library(car)  # Companion to applied regression
library(modelsummary)  # Model summary tables
library(broom)  # Convert statistical models into tidy format
library(sandwich)  # Robust covariance matrix estimators
library(sf)  # Handling spatial data
library(ggspatial)  # Spatial data visualization


# Set working directory (modify the path as needed)


# Read the rental data CSV file into a dataframe
Hashi_df <- read_csv("rent_data.csv")

# Data cleaning and transformation
rent_df <- Hashi_df |> 
  filter(!district %in% c("Abdi Aziz", "Karan")) |>  # Exclude specific districts
  mutate(across(where(is.character), as.factor))  # Convert character variables to factors

# Rename columns for easier reference
rent_df_clean <- rent_df |> 
  rename(NoR = number_of_rooms, NoB= number_of_bathrooms,
         Sqm = house_size, DST = district, YR = year_built,
         HT = house_type, AMN = presence_of_amenities,
         PMR = proximity_to_main_roads, GTD = gated)

## Summary Statistics
rent_df_clean |> 
  summarise(across(where(is.numeric), list(mean = mean, median = median, sd = sd, max = max,
                                           min = min, kurtosis = kurtosis, 
                                           cv = ~ (sd(.) / mean(.))),
                   .names = "{.col}.{.fn}")) |> 
  pivot_longer(cols = everything(), names_to = "variable_stat", 
               values_to = "value") |> 
  separate_wider_delim(variable_stat, delim = ".", 
                       names = c("variable", "statistic")) |> 
  pivot_wider(names_from = statistic, values_from = value)

## Modelling the Data
rent_df_clean <- rent_df_clean |> 
  mutate(log_house_rent = log(house_rent_price))  # Log transform rent price

# Create multiple regression models with increasing complexity
model_one <- lm(log_house_rent ~ Sqm, data = rent_df_clean)
model_two <- lm(log_house_rent ~ Sqm + NoR, data = rent_df_clean)
model_three <- lm(log_house_rent ~ Sqm + NoR, data = rent_df_clean)
model_four <- lm(log_house_rent ~ Sqm + NoR + NoB , data = rent_df_clean)
model_five <- lm(log_house_rent ~ Sqm + NoR + NoB + HT , data = rent_df_clean)
model_six <- lm(log_house_rent ~ Sqm + NoR + NoB+ HT + AMN , data = rent_df_clean)
model_seven <- lm(log_house_rent ~ Sqm + NoR + NoB + HT + AMN + GTD, data = rent_df_clean)
model_eight <- lm(log_house_rent ~ Sqm + NoR + NoB + HT + AMN + GTD + YR , data = rent_df_clean)
model_nine <- lm(log_house_rent ~ Sqm + NoR + NoB + HT + AMN + GTD + YR + DST, data = rent_df_clean)
model_ten <- lm(log_house_rent ~ Sqm + NoR + NoB + HT + AMN + GTD + YR + DST + PMR, data = rent_df_clean)

# Store models in a list for summary generation
model_list <- list(
  "1" = model_one,
  "2" = model_two,
  "3" = model_three,
  "4" = model_four,
  "5" = model_five,
  "6" = model_six,
  "7" = model_seven,
  "8" = model_eight,
  "9" = model_nine,
  "10" = model_ten
)

# Generate model summary with significance stars and standard errors
modelsummary(
  model_list,
  statistic = "({std.error})", 
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik"
) 

# Display detailed summary of the most complex model
summary(model_ten)

## Checking Assumptions

# Homoscedasticity test using Breusch-Pagan test
bptest(model_ten, ~ fitted(model_ten) + I(fitted(model_ten)^2), data = rent_df_clean)

# Multicollinearity test using Variance Inflation Factor (VIF)
vif(model_ten)

# Normality assumption check using density plot
augment(model_ten) |> 
  ggplot(aes(.resid)) + geom_density()

# Normality assumption check using histogram
augment(model_ten) |> 
  ggplot(aes(.resid)) + geom_histogram()

# Shapiro-Wilk normality test on standardized residuals
shapiro.test(rstandard(model_ten))

## Study Area Mapping
# Read shapefile for Somalia administrative boundaries

somalia_admin <- st_read("somalia_admin.shp", quiet = TRUE)
# Convert shape names to character format
somalia_admin$shapeName <- as.character(somalia_admin$shapeName)

# Define Mogadishu districts for analysis
districts <- str_to_upper(c("ABDUL AZIZ", "Bondere", "DANYILE", "Hodan", 
                            "Hamar JabJab", "Hamar Weyn", "Karan", 
                            "Shangani", "MADINA"  , "Waberi", "Yaqshiid", 
                            "DHARKENLY", "YAQSHID", "SHIBIS", "KAHDA",
                            "WARDEGLY", "HAWAL WADAG" ,"HELIWA"))

# Filter for districts in Mogadishu
mogadishu <- somalia_admin %>%
  filter(shapeName %in% districts)

# Standardize district names for consistency
mogadishu_map <- mogadishu |> 
  mutate(shapeName = case_when(
    shapeName == "MADINA" ~ "WADAJIR",
    shapeName == "DANYILE" ~  "DEYNILE",
    shapeName == "WARDEGLY" ~ "WARTANABADA",
    shapeName == "HAWAL WADAG" ~ "HOWLWADAG",
    shapeName == "BONDERE" ~ "BONDHERE",
    shapeName == "DHARKENLY" ~ "DHARKENLEY",
    shapeName == "HAMAR JABJAB" ~ "HAMAR JAJAB",
    shapeName == "HELIWA" ~ "HILWA",
    shapeName == "ABDUL AZIZ" ~ "ABDI AZIZ",
    .default = shapeName
  ))

# Define study area districts
study_df <- c("YAQSHID", "WARTANABADA", "HODAN",  "DEYNILE" ,
              "DHARKENLEY",  "WABERI",  "WADAJIR", "KAHDA" ,
              "HOWLWADAG" ,"HILWA", "SHIBIS" , "HAMAR JAJAB",
              "BONDHERE" )

# Mark districts as included/excluded in the study
Mogadishu_map_two <- mogadishu_map |> 
  mutate(study_area = if_else(shapeName %in% study_df, "Included Districts", "Excluded Districts"))

# Compute centroids for district labeling
mogadishu_centroids <- st_centroid(Mogadishu_map_two)

# Plot the map with districts
# (Visualization details omitted for brevity)




ggplot() +
  geom_sf(data = Mogadishu_map_two, aes(fill = study_area), color = "black", 
          alpha = 0.6) + # Color by study area
  geom_sf_text(data = mogadishu_centroids, aes(label = shapeName), size = 2,
               color = "black") + # Add district names
  labs(#title = "Administrative Boundaries of Mogadishu with Study Area overlayed",
    x = "", 
    y = "",
    fill = "Study Area") + # Add legend title
  #caption = "Producer: HASHI") +
  coord_sf(xlim = c(45.20, 45.42), ylim = c(1.99, 2.15)) + # Zoom into Mogadishu
  theme_minimal() +
  theme(legend.position = "bottom") +
  # Add north arrow
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  
  # Add scale bar
  annotation_scale(location = "br", width_hint = 0.3)

