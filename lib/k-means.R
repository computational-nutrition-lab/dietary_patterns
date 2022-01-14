# FUNCTIONS ==============================================================================

# ========================================================================================
# k-means clustering 
#  
# Version 1
# Created on 01.06.2022 by Rie Sadohara
# ========================================================================================

# Codes to build:
# Collapse variables by correlation.  --OK!
# option to average by participants or not. --> prep_data. -- OK!
# Find the optimum k. 

# ---------------------------------------------------------------------------------------------------------------
# Function to find the ideal k
# Define your input file. 
  kmeans_input <- 
  

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Define your input file. 
  kmeans_input <- 
# Calculate k-means, start with 4.
  km.results <- kmeans(x = subsetted_non0var, centers = 5, nstart = 25)
# Calculate the means of each variable for each cluster. 
  aggregate(subsetted_non0var, by=list(cluster=km.results$cluster), mean)
# Add the cluster assignment to the original (subsetted) data. 
  dd <- cbind(subsetted_non0var, cluster = km.results$cluster)
# Take a look
  dd$cluster
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Use factoextra package for now, but I could just use ggplot2.
  library(ggplot2)
  factoextra::fviz_cluster(km.results, 
                           data = subsetted_non0var, 
                           ellipse = T, ellipse.alpha = 0.1,  
                           ggtheme = theme_bw(base_size = 10),
                           repel = F, labelsize = 10)
  
  # This clusters 580 datapoints: participants x days.  Should have average of each participant? 
  # Abby: good to have an option to take average or not.
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Function to 


# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

