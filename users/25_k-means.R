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
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Come back to the main directory
  setwd(main_wd) 

# Import source code to run the analyses to follow.
  # source("lib/load_and_check.R") # nonexistent
  source("lib/k-means.R")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Define your input file. Need to scale it to accommodate measurements in different units.  
# Average data may not have enough users for this. 
  selected_variables <- read.table("VVKAJ_Tot_m_QCed_Nut_asis.txt", sep="\t", header=T)
  selected_variables <- read.table("VVKAJ_Tot_m_QCed_Nut_ave_subset.txt", sep="\t", header=T)
  selected_variables <- read.table("VVKAJ_Tot_m_QCed_Cat_asis.txt", sep="\t", header=T)
  selected_variables <- read.table("VVKAJ_Tot_m_QCed_Cat_ave_subset.txt", sep="\t", header=T)
  
# Check the column names (variables)
  colnames(selected_variables)

# Scale the variables and define it as an input for k-means analysis.
  kmeans_input <- scale(selected_variables) # correlated variables removed.
  # kmeans_input <- subsetted_non0var  # before removing correlated variables.

# Set your ggplot2 theme.
  require(ggplot2)
  theme_set(theme_bw(base_size = 14))

# ---------------------------------------------------------------------------------------------------------------
# Use the elbow method to find the ideal K. K cannot be larger than the number of datapoints (rows) in input. 
  ElbowMethod(k.values = 1:15)

# ---------------------------------------------------------------------------------------------------------------
# Use the Silhouette method to find the ideal K.  Uses cluster package.
  SilhouetteMethod(k.values = 2:9)

# or use the factoextra package to use the Silhouette method.
  factoextra::fviz_nbclust(kmeans_input, kmeans, method="silhouette")
  # This plots 'average Silhouette width' instead of 'average Silhouette'.
  
# ---------------------------------------------------------------------------------------------------------------
# Use the Gap statistic method to find the ideal K. 
  set.seed(123)
  GapMethod(k.values = 1:9)
  
# Or use the factoextra package to use the Gap statistic method. 
  set.seed(123)
  FactoextraGapMethod(k.values = 1:9)
  
# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with one specified k. 
  One_K(myK = 5)

# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with multiple (2-4) Ks, and plot them in one window. 
  MultipleK(myKs = c(3,4,5,6))
  
# ---------------------------------------------------------------------------------------------------------------
# For individuals, I think...
        # Calculate the means of each variable for each cluster. 
        aggregate(kmeans_input, by=list(cluster = km.results$cluster), mean)
        
        # Add the cluster assignment to the original data. 
        totals_cl <- cbind(totals, cluster = km.results$cluster)
        
        # Filter for a particular cluster.
        library(dplyr)
        mysubset <- as.data.frame(totals_cl) %>% filter(cluster==13)
        
        # Let's see if they have something in common...
        table(mysubset$UserName)
        table(mysubset$X.SampleID)
        table(mysubset$StudyDayNo)
  
# ---------------------------------------------------------------------------------------------------------------
  
  