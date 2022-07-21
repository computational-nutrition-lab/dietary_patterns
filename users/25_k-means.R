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
  Session --> Set working direHctory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Come back to the main directory
  setwd(main_wd) 

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")
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

# Set your ggplot2 theme.
  require(ggplot2)
  theme_set(theme_bw(base_size = 14))
  
# Set seed for consistent results.
  set.seed(123)

# The functions below assume that the input file is named "kmeans_input".
  
# ---------------------------------------------------------------------------------------------------------------
# Use the elbow method to find the ideal K. K cannot be larger than the number of datapoints (rows) in input. 
  ElbowMethod(k.values = 1:15)

# ---------------------------------------------------------------------------------------------------------------
# Use the Silhouette method to find the ideal K. This uses the cluster and factoextra package.
  silhouettechart <- factoextra::fviz_nbclust(kmeans_input, kmeans, method="silhouette")
  silhouettechart
  

# Or if the factoextra package does not work for some reason, there is a way to only use the 
# cluster package. 
  SilhouetteMethod(k.values = 2:9)
  
# ---------------------------------------------------------------------------------------------------------------
# Use the Gap statistic method to find the ideal K. The highest K is the optimum K.
  GapMethod(k.values = 1:15)
  
# Or use the factoextra package to use the Gap statistic method. 
  FactoextraGapMethod(k.values = 1:15)
  
# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with one specified k. 
  oneKplot <- One_K(myK = 5)
  oneKplot
  
# Define the folder to save the chart.
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ/Nut_asis_PCA")

# Save the plot as a PDF file.
  ggsave("VKAJ_Tot_m_QCed_Nut_asis_K5.pdf", oneKplot, 
         device="pdf", width=4, height=4, units="in")  
  

  
  
  ######## RESUME FROM HERE ########
  

# ---------------------------------------------------------------------------------------------------------------
# Perform k-means analysis with multiple (2-4) Ks, and plot them in one window. 
  multipleKplots <- MultipleK(myKs = c(3,4,5,6))
  multipleKplots
  
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
  
  