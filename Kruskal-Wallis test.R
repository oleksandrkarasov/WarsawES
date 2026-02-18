# 1. Load necessary libraries
library(tidyverse)
library(FSA)

# 2. Read the file
df <- read_csv("C:/Users/karas/Downloads/what_where_who_df_1.csv")

# 3. Filter Data
df_clean <- df %>%
  filter(!is.na(TYPE_1))

# 4. Run Dunn's Test with Bonferroni Correction
dunn_results <- dunnTest(ATTACHMENT ~ TYPE_1, 
                         data = df_clean, 
                         method = "bonferroni")

# 5. Display the results
print(dunn_results$res)