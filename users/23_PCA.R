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
  source("lib/specify_dir_and_check_col.R")
  source("lib/prep_data_for_clustering.R")
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
  dim(pca_input)

# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = T)   

# Create a scree plot.
  screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
  screep
  ggsave("results/PCA_results/50 ind/50ind_screep.png", screep, device="png", width=5, height=5, dpi=200)
  
# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input)
  biplotdots
  ggsave("results/PCA_results/50 ind/50ind_biplotdots.png", biplotdots, device="png", width=5, height=5, dpi=200)
  
  
# A biplot with the individuals labeled.
  biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label = T)
  biplotlabeled
  ggsave("results/PCA_results/50 ind/50ind_biplotlabeled.png", biplotlabeled, device="png", width=5, height=5, dpi=200)
  
# A biplot with the individuals labeled without the variables' arrows.
  biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result = scaled_pca, 
                                                 pca.data = pca_input, 
                                                 individuals.label = T)
  biplotlabeledwoarrows
  ggsave("results/PCA_results/50 ind/50ind_biplotlabeledwoarrows.png", biplotlabeledwoarrows, device="png", width=5, height=5, dpi=200)

# Plot the directions of the variables.
  directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
  directions
  ggsave("results/PCA_results/50 ind/50ind_directions.png", directions, device="png", width=5, height=5, dpi=200)
  
  
# Plot the contribution of the variables to a given PC.
  # Variables' labels aligned on the X axis.
  loadings_aligned <- LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
                      positive.color="green2", negative.color="grey70", labels.aligned= TRUE)
  loadings_aligned
  # cannot use png function as it only creates a null image.
  png(filename="results/PCA_results/18 ind/18ind_loadings_aligned.png", loadings_aligned, width=5, height=5, res=200)

  # Variables' labels are placed right below the bars.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", labels.aligned= FALSE)

# ---------------------------------------------------------------------------------------------------------------  
# Save the variance explained by each PC as a .txt file. 
# Change the file name as necessary.  
  SaveVarExplained(pca.data = pca_input, pca.result = scaled_pca, 
                   out.fn = "results/PCA_results/50 ind/PC_var_explained_50ind.txt")

# ---------------------------------------------------------------------------------------------------------------  
# Calculate loadings of each PC to the variables and 
# save it as a txt file in the results folder.
  # Change the file name as necessary.  
  SaveLoadings(pca.result=scaled_pca, 
               out.fn = "results/PCA_results/50 ind/PC_loadings_50ind.txt")
  
# ---------------------------------------------------------------------------------------------------------------  
# Save the PC values with the input which has the metadata and food codes, food names.  
  # This may not be correct
  # SaveInputAndPCs(input = "E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/NHANES_totals_QCed_sampled_PCAs_18ind_input.txt",
  #                 pca.results = scaled_pca, 
  #                 out.fn = "results/PCA_results/18 ind/ind18_totalsinput_QCed_PCs.txt")
  
  SaveInputAndPCs(input = "E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/NHANES_totals_QCed_sampled_PCAs_18ind.txt",
                  pca.results = scaled_pca, 
                  out.fn = "results/PCA_results/18 ind/ind18_totalsinput_QCed_PCs_2.txt")
  
  SaveInputAndPCs(input = "E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/NHANES_totals_QCed_sampled.txt",
                  pca.results = scaled_pca, 
                  out.fn = "results/PCA_results/50 ind/ind50_totalsinput_QCed_PCs.txt")
  
