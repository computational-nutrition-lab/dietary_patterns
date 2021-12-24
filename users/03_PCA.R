# For USERS ==============================================================================

# ========================================================================================
# PCA analysis on nutrients.
# Version 1
# Created on 12.16.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================
# 
# Folder structure 
# 
#                          |----- eg_data 
#                          |
#                          |----- lib
#                          |
#                          |----- users
#  Main -------------------|
#  (dietary_patterns)      |----- 
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/load_and_check.R")
  source("lib/PCA.R")

# Load example totals data =============================================================== 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  # SpecifyDataDirectory(directory.name = "eg_data/salt/")
  
# Load the totals.csv
  totals <- read.table("Totals_to_use.txt",  sep = "\t", header = T)
  
  # If totals data is a csv:
  # totals <- read.csv(list.files(pattern = '\\Totals.csv$'))
  
# Come back to the main directory
  setwd(main.wd)
  
# ========================================================================================  
# 
# ========================================================================================
# Specify the plot theme and base font size to use.
  library(ggplot2)
  ggplot2::theme_set(theme_bw(base_size = 14))

# Subset nutrition data.
# The columns specified as start.col, end.col, and all columns in between will be selected.
# Nutrient analysis --> start.col = "PROT",    end.col = "B12_ADD"
# Nutrient analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS"
  SubsetColumns(data = totals, start.col = "F_TOTAL", end.col = "A_DRINKS")  

# Pick up only the columns with non-zero variance, in order to do PCA.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)

# Perform PCA with all the nutrients, scaled.
  scaled_pca <- prcomp(x = subsetted_non0var, scale = T)   

# Create a scree plot.
  LineScreePlot(pca.result = scaled_pca)

# Create a biplot.
  # A biplot with the individuals as black dots.
  BiplotDots(   pca.result = scaled_pca, pca.data = subsetted_non0var)
  
  # A biplot with the individuals labeled.
  BiplotLabeled(pca.result = scaled_pca, pca.data = subsetted_non0var, individuals.label = TRUE)
  
#

  