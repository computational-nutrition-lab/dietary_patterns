# For USERS ==============================================================================

# ========================================================================================
# k-means clustering.
# Version 1
# Created on 01.06.2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Import necessary functions and data
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
  # source("lib/k-means.R")
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Define your input file. Need to scale it to accommodate measurements in different units.  
  colnames(selected_variables)  
  kmeans_input <- scale(selected_variables) # correlated variables removed.
  # kmeans_input <- subsetted_non0var  # before removing correlated variables.
  
# Set your ggplot2 theme.
  require(ggplot2)
  theme_set(theme_bw(base_size = 14))
# ---------------------------------------------------------------------------------------------------------------

  
# ---------------------------------------------------------------------------------------------------------------
# Use the elbow method to find the ideal K.
  ElbowMethod(k.values = 1:15)
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Use the Silhouette method to find the ideal K.  Uses cluster package.
  SilhouetteMethod(k.values = 2:15)

# or use the factoextra package to use the Silhouette method.
  factoextra::fviz_nbclust(kmeans_input, kmeans, method="silhouette")
  # This plots 'average Silhouette width' instead of 'average Silhouette'.
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Use the Gap statistic method to find the ideal K. 
  set.seed(123)
  GapMethod(k.values = 1:15)
  
# or use the factoextra package to use the Gap statistic method. 
  set.seed(123)
  FactoextraGapMethod(k.values = 1:15)
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with one specified k. 
  
# ---------------------------------------------------------------------------------------------------------------

  
# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with multiple ks. 

# ---------------------------------------------------------------------------------------------------------------
  