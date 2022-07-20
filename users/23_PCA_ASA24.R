# For USERS ==============================================================================

# ========================================================================================
# PCA analysis with ASA24 data.
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
  Session --> Set working direHctory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# Come back to the main directory
  setwd(main_wd) 

# Import source code to run the analyses to follow.
  # source("lib/specify_dir_and_check_col.R")
  # source("lib/prep_data_for_clustering.R")
  source("lib/PCA.R")

# Define ggplot themes to use in creating plots.
  library(ggplot2)
  theme_set(theme_bw(base_size = 14))

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# ===============================================================================================================
# Nutrient data as is, processed for clustering analyses.
# ===============================================================================================================

# Load Nut_asis data.
  Tot_m_QCed_Nut_asis <- read.table(file="VVKAJ_Tot_m_QCed_Nut_asis.txt", sep="\t", header=T)
    
# Name your input data.
  pca_input <- Tot_m_QCed_Nut_asis

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
# Specify the directory (folder) to save the results.
  res_dir = "Nut_asis_PCA" 

# Specify the prefix of filenames to be saved. 
  res_prefix = "VVKAJ_Nut_asis"
  
# Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, out.dir= res_dir, out.prefix= res_prefix )
  
# Combine the input (totals before processing) with all the variables and the PC results. 
  SaveInputAndPCs(input="VVKAJ_Tot_m_QCed.txt", pca.results = scaled_pca, 
                  out.dir= res_dir, out.prefix= res_prefix)

# Note that even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.
  
# ===============================================================================================================
# Nutrient data averaged and processed for clustering analyses.
# ===============================================================================================================
  
# Load Nut_ave data.
  Tot_m_QCed_Nut_ave <- read.table(file="VVKAJ_Tot_m_QCed_Nut_ave_subset.txt", sep="\t", header=T)
  
# Name your input data.
  pca_input <- Tot_m_QCed_Nut_ave
  
# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
# Specify the directory (folder) to save the results.
  res_dir = "Nut_ave_PCA" 
  
# Specify the prefix of filenames to be saved. 
  res_prefix = "VVKAJ_Nut_ave"
  
# Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
# Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, out.dir= res_dir, out.prefix= res_prefix)
  
# Combine the input (totals before processing) with all the variables and the PC results.
# In the case of aveaged totals data / user, the input file used here is xxx_ave_allvar.txt, which 
# has all the variables before filtering out by correlation or zero variance.
  SaveInputAndPCs(input="VVKAJ_Tot_m_QCed_Nut_ave_allvar.txt", pca.results = scaled_pca, 
                  out.dir= res_dir, out.prefix= res_prefix)

# ===============================================================================================================
# Food Category data as is, processed for clustering analyses.
# ===============================================================================================================
  
# Load Cat_asis data.
  Tot_m_QCed_Cat_asis <- read.table(file="VVKAJ_Tot_m_QCed_Cat_asis.txt", sep="\t", header=T)
  
  # Name your input data.
  pca_input <- Tot_m_QCed_Cat_asis
  
  # Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  # Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
  # Specify the directory (folder) to save the results.
  res_dir = "Cat_asis_PCA" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix = "VVKAJ_Cat_asis"
  
  # Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, out.dir= res_dir, out.prefix= res_prefix )
  
  # Combine the input (totals before processing) with all the variables and the PC results. 
  SaveInputAndPCs(input="VVKAJ_Tot_m_QCed.txt", pca.results = scaled_pca, 
                  out.dir= res_dir, out.prefix= res_prefix)
  
  # Note that even though the input file has both Nutrients (Nut) and food categories (Cat) data,  
  # PCA was done with only either Nut or Cat, not both.
  
# ===============================================================================================================
# Food category data averaged and processed for clustering analyses.
# ===============================================================================================================
  
# Load Cat_ave data.
  Tot_m_QCed_Cat_ave <- read.table(file="VVKAJ_Tot_m_QCed_Cat_ave_subset.txt", sep="\t", header=T)
  
  # Name your input data.
  pca_input <- Tot_m_QCed_Cat_ave
  
  # Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  # Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
  # Specify the directory (folder) to save the results.
  res_dir = "Cat_ave_PCA" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix = "VVKAJ_Cat_ave"
  
  # Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  # Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, out.dir= res_dir, out.prefix= res_prefix)
  
  # Combine the input (totals before processing) with all the variables and the PC results. 
  # In the case of aveaged totals data / user, the input file used here is xxx_ave_allvar.txt, which 
  # has all the variables before filtering out by correlation or zero variance.
  SaveInputAndPCs(input="VVKAJ_Tot_m_QCed_Cat_ave_allvar.txt", pca.results = scaled_pca, 
                  out.dir= res_dir, out.prefix= res_prefix)  

# ===============================================================================================================
# Come back to the main directory
  setwd(main_wd) 


# ===============================================================================================================
# Code to create and make adjustments to each plot/file, if desired.
# ===============================================================================================================

# You can specify different directory and prefix to avoid overwriting files 
# produced by the PerformPCA function. 
  
  res_dir =    "Nut_asis_PCA_2" 
  res_prefix = "VVKAJ_Nut_asis_2"
  
# Create a scree plot.
  screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
  screep
  ggsave( paste(res_dir, paste(res_prefix, "_scree.pdf"), sep= .Platform$file.sep), 
          screep, device="pdf", width=5, height=5, units="in") 
  
# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
  biplotdots
  ggsave( paste(res_dir, paste(res_prefix, "_biplotdots.pdf"), sep= .Platform$file.sep),
          biplotdots, device="pdf", width=5, height=5, units="in")

# A biplot with the individuals labeled.
  biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=T)
  biplotlabeled
  ggsave( paste(res_dir, paste(res_prefix, "_biplotlabeled.pdf"), sep= .Platform$file.sep),
          biplotlabeled, device="pdf", width=5, height=5, units="in")
  
# A biplot with the individuals labeled without the variables' arrows.
  biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input, 
                                                 individuals.label=T)
  biplotlabeledwoarrows 
  # Zoom in to a particular area of interest in the plot 
  biplotlabeledwoarrows + coord_cartesian(xlim=c(-0.1, 0.1), ylim=c(0.05, 0.1))
  
  ggsave( paste(res_dir, paste(res_prefix, "_biplotlabeledwoarrows.pdf"), sep= .Platform$file.sep),
          biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")
  
# Plot the directions of the variables.
  directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
  directions
  ggsave( paste(res_dir, paste(res_prefix, "_directions.pdf"), sep= .Platform$file.sep),
          directions, device="pdf", width=5, height=5, units="in")

# Plot the contribution of the variables to a given PC: Change the PC and the file name as desired.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave( paste(res_dir, paste(res_prefix, "_loadings_PC1.pdf"), sep= .Platform$file.sep),
          loadings_plot, device="pdf", width=8, height=4.8, units="in")

