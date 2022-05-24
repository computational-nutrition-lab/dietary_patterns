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
  Session --> Set working direHctory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  # source("lib/specify_dir_and_check_col.R")
  # source("lib/prep_data_for_clustering.R")
  source("lib/PCA.R")

# Define ggplot themes to use in creating plots.
  library(ggplot2)
  ggplot2::theme_set(theme_bw(base_size = 14))
  
# ========================================================================================
# Perform Principal Component Analysis.
# ========================================================================================
# 
# ---------------------------------------------------------------------------------------------------------------  
# Name your input data.
# Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- selected_variables

# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   

# Create a scree plot.
  screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
  screep
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_scree.pdf", screep, device="pdf", width=5, height=5, units="in")
  
# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
  biplotdots
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_biplotdots.pdf", biplotdots, device="pdf", width=5, height=5, units="in")
  
# A biplot with the individuals labeled.
  biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label = T)
  biplotlabeled
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_biplotlabeled.pdf", biplotlabeled, device="pdf", width=5, height=5, units="in")
  
# A biplot with the individuals labeled without the variables' arrows.
  biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input, 
                                                 individuals.label=T)
  biplotlabeledwoarrows #+coord_cartesian(xlim=c(-0.1, 0.1), ylim=c(0.05, 0.1))   
  
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_biplotlabeledwoarrows.pdf", biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")

# Plot the directions of the variables.
  directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
  directions
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_directions.pdf", directions, device="pdf", width=5, height=5, units="in")

# Plot the contribution of the variables to a given PC: PC1 here.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_loadings_PC1.pdf", loadings_plot, device="pdf", width=8, height=4.8, units="in")

# Plot the contribution of the variables to a given PC: PC2 here.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC2", 
               positive.color="green2", negative.color="grey70", sort.variables = T)
  loadings_plot
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_loadings_PC2.pdf", loadings_plot, device="pdf", width=8, height=4.8, units="in")

# ---------------------------------------------------------------------------------------------------------------  
# Save the variance explained by each PC as a .txt file. 
# Change the file name as necessary.  
  SaveVarExplained(pca.data = pca_input, pca.result = scaled_pca, 
                   out.fn = "results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_PC_var_explained.txt")

# ---------------------------------------------------------------------------------------------------------------  
# Calculate loadings of each PC to the variables and 
# save it as a txt file in the results folder.
  # Change the file name as necessary.  
  SaveLoadings(pca.result=scaled_pca, 
               out.fn="results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_PC_loadings.txt")
  
# ---------------------------------------------------------------------------------------------------------------  
# Save the PC values with the input which has the metadata and food codes, food names.  
# Input is your food input file before any prep for clustering, from which you derived the input for the PCA.
  SaveInputAndPCs(input = "eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_82diffdiet.txt",
                  pca.results = scaled_pca, 
                  out.fn = "results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_input_PCs.txt")
  

  