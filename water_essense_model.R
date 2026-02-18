# Set libPaths.
.libPaths("C:\\Users\\karas\\.exploratory\\R\\4.3")

# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(cpp11)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(zipangu)
library(exploratory)

# Steps to produce the output
exploratory::read_delim_file("C:\\Users\\karas\\Documents\\ES data\\flood_reg2.csv", delim = NULL, quote = "\"" , col_names = TRUE , na = c('') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "Europe/Tallinn", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  select(WATER_AVERAGE, LON, LAT, PROFFESION_ECO, COVID, NATURE_RELATION, MAN_HIST_PARK, MAN_NEIGH_PARK, MAN_PARK, MAN_PROMENADE, OBJECT_OTHER, SEMI_MAN_BEACH, SEMI_MAN_MEADOW, SEMI_MAN_SHRUBS, UNMAN_FOREST, GENDER_MEN, AGE_16_24, AGE_35_44, AGE_45_54, `AGE_55+`, WORK_SITUATION_IN_EDUCATION, WORK_SITUATION_OTHER, CHILDHOOD_PLACE_CITY, CHILDHOOD_PLACE_COUNTRYSIDE, CHILDHOOD_PLACE_FOREST, CHILDHOOD_PLACE_FRESH_WATER, CHILDHOOD_PLACE_SEASIDE, VISITING_FREQ_EVERYDAY, VISITING_FREQ_FEW_TIMES_LAST_6_MONTHS, VISITING_FREQ_FEW_TIMES_WEEK, ACCESSIBILITY_BALCONY_PATIO, ACCESSIBILITY_COMMUNITY_GARDEN, ACCESSIBILITY_PRIVATE_GARDEN) %>%
  build_lm(WATER_AVERAGE ~ . ) %>%
  select(-fid_3, -osm_id, -code, -name) %>%
  rename(`Wisła_500m` = fclass) %>%
  mutate(Wisła_500m = impute_na(Wisła_500m, type = "value", val = "FALSE")) %>%
  mutate(Wisła_500m = recode(Wisła_500m, "riverbank" = "TRUE", type_convert = TRUE)) %>%
  filter(!is.na(FLOOD_REG))