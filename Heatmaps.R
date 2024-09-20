# Load required packages
library(scales)
library(wesanderson)
library(vegan)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(heatmaply)
library(pheatmap)

# Reading datasets
mydata_who = read.csv("what_who_mean.csv", header= TRUE)
mydata_where = read.csv("what_where_mean.csv", header= TRUE)

# Pre-processing datasets
mydata_where <- mydata_where %>% mutate(across(where(is.numeric), ~ round(., 1)))
mydata_who <- mydata_who %>% mutate(across(where(is.numeric), ~ round(., 1)))

#set specific column as row names
rownames(mydata_who) <- mydata_who$WHO
#set specific column as row names
rownames(mydata_where) <- mydata_where$TYPE_1

#remove original column from data frame
mydata_who$WHO <- NULL
mydata_where$TYPE_1 <- NULL

webshot::install_phantomjs()

# Visualising heatmaps

heatmaply(
  mydata_where,
  colors = wes_palette("Zissou1"),
  seriate = "mean",
  cellnote = mydata_where,
  xlab = "",
  ylab = "", 
  main = "",
  width = 1200,
  height = 700,
  show_dendrogram = FALSE,
  Rowv = FALSE,
  file = "heatmaply_ES_where_mean.html")



heatmaply(
  mydata_who,
  colors = wes_palette("Zissou1"),
  seriate = "mean",
  cellnote = mydata_who,
  xlab = "",
  ylab = "", 
  main = "",
  width = 1200,
  height = 700,
  show_dendrogram = FALSE,
  Rowv = FALSE,
  file = "heatmaply_ES_who_mean.html")



