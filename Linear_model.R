# 1. Load the library
library(tidyverse)

# 2. Read the file
df <- read_csv("C:/Users/karas/Documents/ES data/flood_reg2.csv")

# 3. Select the specific columns
df_model_data <- df %>%
  select(
    WATER_AVERAGE, 
    LON, LAT, 
    PROFFESION_ECO, COVID, NATURE_RELATION, 
    MAN_HIST_PARK, MAN_NEIGH_PARK, MAN_PARK, MAN_PROMENADE, OBJECT_OTHER, 
    SEMI_MAN_BEACH, SEMI_MAN_MEADOW, SEMI_MAN_SHRUBS, UNMAN_FOREST, 
    GENDER_MEN, 
    AGE_16_24, AGE_35_44, AGE_45_54, `AGE_55+`, # Note backticks for special char
    WORK_SITUATION_IN_EDUCATION, WORK_SITUATION_OTHER, 
    CHILDHOOD_PLACE_CITY, CHILDHOOD_PLACE_COUNTRYSIDE, 
    CHILDHOOD_PLACE_FOREST, CHILDHOOD_PLACE_FRESH_WATER, CHILDHOOD_PLACE_SEASIDE, 
    VISITING_FREQ_EVERYDAY, VISITING_FREQ_FEW_TIMES_LAST_6_MONTHS, VISITING_FREQ_FEW_TIMES_WEEK, 
    ACCESSIBILITY_BALCONY_PATIO, ACCESSIBILITY_COMMUNITY_GARDEN, ACCESSIBILITY_PRIVATE_GARDEN
  ) %>%
  na.omit() # Basic cleaning: remove rows with missing values

# 4. Build the Linear Regression Model
# The "~ ." formula means "Predict WATER_AVERAGE using ALL other columns in the data"
model <- lm(WATER_AVERAGE ~ ., data = df_model_data)

# 5. View the results (Coefficients, R-Squared, P-values)
summary(model)
