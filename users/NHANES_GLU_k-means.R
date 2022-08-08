# ===============================================================================================================
# Perform k-means analysis with the males in their 50s.
# Version 1
# Created on 08/08/2022 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/dietary_patterns/eg_data/NHANES/Laboratory_data/")

# Load the necessary functions 
  source("../../../lib/prep_data_for_clustering.R")
  source("../../../lib/PCA.R")
  source("../../../lib/k-means.R")
  source("../../../lib/ggplot2themes.R")

# ===============================================================================================================
# Prep for k-means with nutrients
# ===============================================================================================================
# Your input data should be a data frame with variables with non-zero variance. 
  selected_variables <- read.table("QCtotalANDglu_body_meta_demo_males50s_Nut_rv.txt", 
                                   sep="\t", header=T)

# Ensure your input file has the correct number of rows and columns.
  dim(selected_variables)
  
# Scale the variables and define it as an input for k-means analysis.
  kmeans_input <- scale(selected_variables) # correlated variables removed.
  
# Set seed for consistent results.
  set.seed(123)
  
# The functions below assume that the input file is named "kmeans_input".
# ===============================================================================================================
# Perform k-means with nutrients
# ===============================================================================================================
  
# Specify the directory (folder) to save the results.
  res_dir_nut = "k-means_Nut_males50s"
  
# Specify the prefix of filenames to be saved. 
  res_prefix_nut = "males50s_Nut"
  
# Run elbow, silhouette, and gap methods to find an optimum K (number of clusters). 
  ChooseK(out.dir= res_dir_nut, out.prefix= res_prefix_nut)

# Perform k-means analysis with one specified k. It will be saved as a PDF. 
  oneK <- OneK(myK = 3, out.dir= res_dir_nut, out.fn = "males50s_Nut_kmeans_K3")
  oneK

# Try multiple Ks and print the biplots in one panel.
  MultipleK(myKs = c(2,3,4), out.dir = res_dir_nut, out.fn = "males50s_Nut_kmeans_K2-4")
  

# ---------------------------------------------------------------------------------------------------------------

# ===============================================================================================================
# Prep for k-means with food categories.
# ===============================================================================================================
# Your input data should be a data frame with variables with non-zero variance. 
  selected_variables <- read.table("QCtotalANDglu_body_meta_demo_males50s_Cat_rv.txt", 
                                   sep="\t", header=T)
  
  # Ensure your input file has the correct number of rows and columns.
  dim(selected_variables)
  
  # Scale the variables and define it as an input for k-means analysis.
  kmeans_input <- scale(selected_variables) # correlated variables removed.
  
  # Set seed for consistent results.
  set.seed(123)
  
  # The functions below assume that the input file is named "kmeans_input".
# ===============================================================================================================
# Perform k-means with nutrients
# ===============================================================================================================
  
# Specify the directory (folder) to save the results.
  res_dir_cat = "k-means_Cat_males50s"
  
# Specify the prefix of filenames to be saved. 
  res_prefix_cat = "males50s_Cat"
  
# Run elbow, silhouette, and gap methods to find an optimum K (number of clusters). 
  ChooseK(out.dir= res_dir_cat, out.prefix= res_prefix_cat)
  
# Perform k-means analysis with one specified k. It will be saved as a PDF. 
  oneK <- OneK(myK= 3, out.dir= res_dir_cat, out.fn= "males50s_Cat_kmeans_K3")
  
# Try multiple Ks and print the biplots in one panel.
  MultipleK(myKs= c(2,3,4,5), out.dir= res_dir_cat, out.fn= "males50s_Cat_kmeans_K2-5")
  
  
# ---------------------------------------------------------------------------------------------------------------



  
