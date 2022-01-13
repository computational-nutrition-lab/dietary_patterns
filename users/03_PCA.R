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
#  (dietary_patterns)      |----- results
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/load_and_check.R")
  source("lib/prep_data.R")
  source("lib/PCA.R")

# May not need this section since data are loaded in 03.prep_data first.===================  
# Load example totals data ==(may not needed because data are loaded in prep_data.R)======= 
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
  
 

  
# PCA ====================================================================================
# Specify the plot theme and base font size to use.
  library(ggplot2)
  ggplot2::theme_set(theme_bw(base_size = 14))

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x = subsetted_non0var, scale = T)   

# Create a scree plot.
  LineScreePlot(pca.result = scaled_pca)

# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  BiplotDots(pca.result = scaled_pca, pca.data = subsetted_non0var)
  
  # A biplot with the individuals labeled.
  BiplotLabeled(pca.result = scaled_pca, 
                pca.data = subsetted_non0var, 
                individuals.label = TRUE)
  
# calculate loadings of each PC to the variables and 
# save it as a csv file in the results folder.
  # Change the name of the csv file to be saved if necessary. 
  SaveLoadings(pca.result = scaled_pca, name = "PC_loadings_nutrients")
  SaveLoadings(pca.result = scaled_pca, name = "PC_loadings_fooditems")
# ========================================================================================    

  