# For USERS ==============================================================================

# ========================================================================================
# PCA analysis on NHANES data.
# Version 1
# Created on 07/04/2022 by Rie Sadohara
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
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  # source("lib/specify_dir_and_check_col.R")
  # source("lib/prep_data_for_clustering.R")
  source("../../lib/PCA.R")

# Define ggplot themes to use in creating plots.
  library(ggplot2)
  ggplot2::theme_set(theme_bw(base_size = 14))

# ========================================================================================
# Load your data.
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------  
# Nutrient data as is, processed for clustering analyses.
  cluster_ready <- read.table(file="VVKAJ_2021-11-09_7963_Tot_m_QCed_Nut_asis.txt", sep="\t", header=T)
    
# ========================================================================================
# Perform Principal Component Analysis (PCA).
# ========================================================================================
# 
# Name your input data.
# Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- cluster_ready

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
#### Save PCA plots and txt all at once. ####
  
# Specify the directory (folder) to save the results.
  res_dir = "Nut_asis_PCA" 

# Specify the prefix of filenames to be saved. 
  res_prefix = "VVKAJ_Nut_asis_fn"
  
# Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  
  # Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.

  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, out.dir= res_dir, out.prefix= res_prefix,
             input= "VVKAJ_2021-11-09_7963_Tot_m_QCed.txt")

# Note that even though the input file has both nutrients (Nut) and food categories (Cat) data,  
# PCA was done with only either Nut or Cat, not both.


  
# ========================================================================================
# Code to create and make adjustments to each plot/file.
# ========================================================================================

# Create a scree plot.
  screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
  screep
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_scree.pdf", screep, device="pdf", width=5, height=5, units="in")
  
# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
  biplotdots
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_biplotdots.pdf", biplotdots, device="pdf", width=5, height=5, units="in")
  
# A biplot with the individuals labeled.
  biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=T)
  biplotlabeled
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_biplotlabeled.pdf", biplotlabeled, device="pdf", width=5, height=5, units="in")
  
# A biplot with the individuals labeled without the variables' arrows.
  biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input, 
                                                 individuals.label=T)
  biplotlabeledwoarrows #+coord_cartesian(xlim=c(-0.1, 0.1), ylim=c(0.05, 0.1))   
  
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_biplotlabeledwoarrows.pdf", biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")

# Plot the directions of the variables.
  directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
  directions
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_directions.pdf", directions, device="pdf", width=5, height=5, units="in")

# Plot the contribution of the variables to a given PC: PC1 here.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_loadings_PC1.pdf", loadings_plot, device="pdf", width=8, height=4.8, units="in")

# Plot the contribution of the variables to a given PC: PC2 here.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC2", 
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_loadings_PC2.pdf", loadings_plot, device="pdf", width=8, height=4.8, units="in")

# ---------------------------------------------------------------------------------------------------------------  
# Save the variance explained by each PC as a .txt file. 
# Change the file name as necessary.  
  SaveVarExplained(pca.data = pca_input, pca.result = scaled_pca, 
                   out.fn = "results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_PC_var_explained.txt")

# ---------------------------------------------------------------------------------------------------------------  
# Calculate loadings of each PC to the variables and 
# save it as a txt file in the results folder.
  # Change the file name as necessary.  
  SaveLoadings(pca.result=scaled_pca, 
               out.fn="results/PCA_results/NHANES1516_totalsbyhand_FC_n98/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet_PC_loadings.txt")
  
  