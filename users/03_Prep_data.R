# For USERS ==============================================================================

# ========================================================================================
# Prepare data for PCA and other cluster analysis.
# Version 1
# Created on 01.13.2022 by Rie Sadohara
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
  # source("lib/PCA.R")

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


# If using each dataponits as is without averaging ======================================
# Pick up only the columns with non-zero variance, in order to do a PCA.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)

# Subset nutrition data.
# The columns specified as start.col, end.col, and all columns in between will be selected.
# Nutrient analysis   --> start.col = "PROT",    end.col = "B12_ADD"
# Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS"
  SubsetColumns(data = totals, start.col = "PROT",    end.col = "B12_ADD")  
  SubsetColumns(data = totals, start.col = "F_TOTAL", end.col = "A_DRINKS")  
# ========================================================================================    

# Take average by each category (user in this case)=======================================
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variables
  AverageBy(data = totals, by = "UserName", start.col = "PROT", end.col = "B12_ADD")

# Results are saved in this dataframe.  Probably too large to see as it is.
  meansbycategorydf

# The column names should be the same as start.col-end.col. 
  colnames(meansbycategorydf)

# The row names should be each category entry to calculate means for.
  rownames(meansbycategorydf)

# Remove variables with zero variance for PCA.   
 RESUME FROM HERE
# ========================================================================================

 
 
 