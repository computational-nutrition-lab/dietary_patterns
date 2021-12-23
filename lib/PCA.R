# FUNCTIONS ==============================================================================

# ========================================================================================
# Principal component analysis (PCA)  
#  
# Version 1
# Created on 12.17.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Set working directory 
# ========================================================================================

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/load_and_check.R")

# Load example data files =============================================================== 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")

# Load the totals data.
  totals <- read.table("Totals_to_use.txt",  sep = "\t", header = T)

 
# Function to subset nutrition data (columns "PROT" through "B12_ADD").

SubsetNutrients <- function(data = totals, start_col = "PROT", end_col = "B12_ADD"){

  # Column Variables of "totals" dataframe.
  colvars <- names(totals)
  
  # Get the first ID
  start_col <- match(start_col, colvars)
  
  # Get the second ID
  end_col <- match(end_col, colvars)
  
  # Subset range
  nutrients <<- totals[, start_col:end_col]
  
  # Print what was loaded.
  cat("nutrients dataset contains the following", length(colnames(nutrients)), "columns.", "\n")
  print(colnames(nutrients))
  }
  
  
# The columns "PROT" and "B12_ADD" and all columns in between will be selected. 
#   Edit the start_col and/or end_col if different from the default.  
  SubsetNutrients(data = totals, start_col = "PROT", end_col = "B12_ADD")  

# Perform PCA with all the nutrients, scaled.
  scaled_pca <- prcomp(nutrients, scale = T) 

  summary(scaled_pca)

# Calculate the variance explained for each PC.
  var_explained_df <- data.frame(PC = rep(1:length(colnames(nutrients))),
                               var_explained = (scaled_pca$sdev)^2/sum((scaled_pca$sdev)^2))

# Subset the first 10 PCs
  first10PCs <- var_explained_df[1:10, ]

# Create a scree plot.
  require(ggplot2)
  ggplot(first10PCs, aes(x=PC, y=var_explained*100)) + 
    geom_line() + 
    geom_point() +
    theme_bw(base_size = 15) +
    labs(x = "Number of PCs",
         y = "Variance explained by PCs (%)") +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(aspect.ratio = 0.9)
  



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

