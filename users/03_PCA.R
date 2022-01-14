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
# ---------------------------------------------------------------------------------------------------------------
# Load necessary functions and data
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


# ========================================================================================
# Perform Principal Component Analysis.
# ========================================================================================
# 
# ---------------------------------------------------------------------------------------------------------------  
# Specify the plot theme and base font size to use.
  library(ggplot2)
  ggplot2::theme_set(theme_bw(base_size = 14))
  
# Name your input data.
# Your input data should be a data frame with variables with non-zero variance. 
  my_input <- selected_variables
  my_input <- selected_variables[, 1:8]
  
# Perform PCA with the subset data, scaled.
  # scaled_pca <- prcomp(x = subsetted_non0var, scale = T)   
  scaled_pca <- prcomp(x = my_input, scale = T)   

# Create a scree plot.
  LineScreePlot(pca.data = my_input, pca.result = scaled_pca)

# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  BiplotDots(pca.result = scaled_pca, pca.data = my_input)
  
  # A biplot with the individuals labeled.
  BiplotLabeled(pca.result = scaled_pca, 
                pca.data = my_input, 
                individuals.label = TRUE)
  
# Save the variance explained by each PC in the result folder.
  # Change the file name as necessary.  
  SaveVarExplained(x=var_explained_df, name = "PC_var_explained")

# calculate loadings of each PC to the variables and 
# save it as a csv file in the results folder.
  # Change the file name as necessary.  
  SaveLoadings(pca.result = scaled_pca, name = "PC_loadings")
# ---------------------------------------------------------------------------------------------------------------  

    