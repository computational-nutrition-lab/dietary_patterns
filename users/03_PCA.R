# For USERS ==============================================================================

# ========================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 12.16.2021 by Rie Sadohara
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
#  (dietary_patterns)      |----- 
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/load_and_check.R")
  source("lib/percent_kcal.R")

# Load example totals data =============================================================== 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  
# Load the totals.csv
  totals <- read.table("Totals_to_use.txt",  sep = "\t", header = T)

# Come back to the main directory
  setwd(main.wd)
  
# ========================================================================================  
# 
# ========================================================================================


  
  
  
  
  
  
  
  
  
  
 