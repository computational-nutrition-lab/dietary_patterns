# For USERS ==============================================================================

# ========================================================================================
# k-means clustering.
# Version 1
# Created on 01.06.2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================
# 
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
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/load_and_check.R")
  source("lib/PCA.R")
  source("lib/k-means.R")

# Load example totals data =============================================================== 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  # SpecifyDataDirectory(directory.name = "eg_data/salt/")
  
# Load the totals.csv
  totals <- read.table("Totals_to_use.txt",  sep = "\t", header = T)
  
  # If totals data is a csv:
  # totals <- read.csv(list.files(pattern = '\\Totals.csv$'))
  
# Come back to the main directory
  setwd(main.wd)
  
# ========================================================================================  
# 
# ========================================================================================
# Specify the plot theme and base font size to use.
  library(ggplot2)
  ggplot2::theme_set(theme_bw(base_size = 14))

# Subset nutrition data.
# The columns specified as start.col, end.col, and all columns in between will be selected.
# Nutrient analysis   --> start.col = "PROT",    end.col = "B12_ADD"
# Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS"
  SubsetColumns(data = totals, start.col = "PROT", end.col = "B12_ADD")  
  SubsetColumns(data = totals, start.col = "F_TOTAL", end.col = "A_DRINKS")  

# Pick up only the columns with non-zero variance, in order to do a PCA.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  
  
  


  