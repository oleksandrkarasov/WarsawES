# Load required packages
library(ggplot2)
library(ggridges)
library(viridis)
library(plyr)
library(tidyverse)

# Reading datasets
data_where = read.csv("where_what_comparison.csv", header= TRUE)
data_who = read.csv("who_for_ridges.csv", header= TRUE)

# Visualising ridges plots
ggplot(data_where, aes(
  x = value, 
  y = fct_reorder(WHERE, value, .fun = mean), 
  fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01, jittered_points = TRUE, alpha = 0.3) +
  scale_fill_viridis_c(name = "ES score", option = "C") +
  xlab("ES score") +
  ylab("")

ggplot(data_who, aes(
  x = value, 
  y = fct_reorder(WHO, value, .fun = sum), 
  fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01, jittered_points = TRUE, alpha = 0.3) +
  scale_fill_viridis_c(name = "ES score", option = "C") +
  xlab("Median ES score") +
  ylab("")

ggplot(data_where, aes(
  x = value, 
  y = fct_reorder(key, value, .fun = mean), 
  fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01, jittered_points = TRUE, alpha = 0.3) +
  scale_fill_viridis_c(name = "ES score", option = "C") +
  xlab("ES score") +
  ylab("")


ggplot(data_who, aes(
  x = value, 
  y = fct_reorder(key, value, .fun = sum), 
  fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01, jittered_points = TRUE, alpha = 0.3) +
  scale_fill_viridis_c(name = "ES score", option = "C") +
  xlab("ES score") +
  ylab("")

