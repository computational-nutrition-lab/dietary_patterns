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
  LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)

# Create a biplot.
  # A biplot with the individuals as black dots and variables labelled.
  BiplotDots(pca.result = scaled_pca, pca.data = pca_input)
  
# A biplot with the individuals labeled.
  BiplotLabeled(pca.result = scaled_pca, 
                pca.data = pca_input, 
                individuals.label = T)

# A biplot with the individuals labeled without the variables' arrows.
  BiplotLabeledwoArrows(pca.result = scaled_pca, 
                        pca.data = pca_input, 
                        individuals.label = T)

# A The directions of the variables.
  BiplotLabeled(pca.result = scaled_pca, 
                pca.data = pca_input, 
                individuals.label = F)

# Plot the contribution of the variables to a given PC.
  # Variables' labels aligned on the X axis.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", labels.aligned= TRUE)
  # Variables' labels are placed right below the bars.
  LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
               positive.color="green2", negative.color="grey70", labels.aligned= FALSE)

# ---------------------------------------------------------------------------------------------------------------  
# Save the variance explained by each PC as a .txt file. 
# Change the file name as necessary.  
  SaveVarExplained(pca.data = pca_input, pca.result = scaled_pca, out.fn = "results/PCA_results/PC_var_explained_18ind.txt")

# ---------------------------------------------------------------------------------------------------------------  
# Calculate loadings of each PC to the variables and 
# save it as a txt file in the results folder.
  # Change the file name as necessary.  
  SaveLoadings(pca.result = scaled_pca, out.fn = "PCA_results/PC_loadings_18ind.txt")
  
# ---------------------------------------------------------------------------------------------------------------  
# Save the PC values with the input  ===== gives error... why???
  # Food items ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SaveInputAndPCs <- function(input, pca.results, out.fn){
    
    # Define your food input file from which you derived the input for the PCA. 
    pca_input <- read.table(input, sep="\t", header=T)
    # This has food codes and food names.
   print(head(pca.results))
   print(out.fn)
    
    # extract PCs
    PCs <- as.data.frame(pca.results["x"]) 
    
    # these should have the same number of rows, so their difference should be zero.  
    diff <- nrow(pca_input) - nrow(PCs)
    
    # Give an error message if the input and pca.result have a different number of rows.
    if(diff != 0){
      cat("Error: The input and the PCA results should have the same number of rows.")
    }else{

      # Add columns
      Input_PCs <<-  cbind(pca_input, PCs)

      # Save as a txt file.
      write.table(Input_PCs, "PCA_results/foods_QCed_30_PCs.txt", sep="\t", row.names = F)
    }
  }
  
  SaveInputAndPCs(input = "NHANES_foods_QCed_30.txt",
                  pca.results = scaled_pca, 
                  out.fn = "PCA_results/foods_QCed_30_PCs_fn.txt")
  
  
  
  # before making it into function -----------------------------------------------------
  foods_QCed_30 <- read.table("NHANES_foods_QCed_30.txt", sep="\t", header=T)
  # This has food codes and food names.
  
  aaa <-  scaled_pca[["x"]]
  PCs <- as.data.frame(scaled_pca$x) # PCs
  
  # these should have the same number of rows, so their difference should be zero.  
  nrow(foods_QCed_30) - nrow(PCs)
  
  # Add columns
  foods_QCed_30_PCs = cbind(foods_QCed_30, PCs)
  
  # Save as a txt file.
  write.table(foods_QCed_30_PCs, "PCA_results/foods_QCed_30_PCs.txt", sep="\t", row.names = F)
  

