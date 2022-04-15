# For USERS ==============================================================================

# ========================================================================================
# Prepare data for PCA and other cluster analysis.
# Version 1
# Created on 01.13.2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data and prepare them for analyses
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------# 
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
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")
  source("lib/prep_data_for_clustering.R")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")

# ASA24 data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the totals data:
  totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)

# If totals data is a csv:
# totals <- read.csv(list.files(pattern = '\\Totals.csv$'))

# Load the items.csv
  items <- read.table("Items_to_use.txt", quote = "", sep = "\t", header = T)

# NHANES data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  totals <- read.table("nhanes_totals_1000.txt", quote = "", sep = "\t", header = T)
  
# Come back to the main directory
  setwd(main.wd)
# ---------------------------------------------------------------------------------------------------------------
 
######## CHOOSE EITHER 1 OR 2 OF THE FOLLOWING: 1: WITHOUT AVEAGING; 2: WITH AVERAGING. #########
  
# ---------------------------------------------------------------------------------------------------------------
# 1. If using each dataponit as is WITHOUT AVERAGING, 
  

# Subset nutrients or food items data.
# The columns specified as start.col, end.col, and all columns in between will be selected.
# Nutrients analysis  --> start.col = "PROT",    end.col = "B12_ADD", 64 variablees in total.
  SubsetColumns(data = totals_selected, start.col = "PROT",    end.col = "B12_ADD")  
# Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS", 37 varialbes in total.
  SubsetColumns(data = totals_selected, start.col = "F_TOTAL", end.col = "A_DRINKS")  

# pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # "subsetted_non0var" is the dataframe to be used in the subsequent
  # collapse by correlation procedure.
  colnames(subsetted_non0var)
# ---------------------------------------------------------------------------------------------------------------
  
######### OR #########
  
# ---------------------------------------------------------------------------------------------------------------
# 2. Collapse variables by correlation: take only one variables if they are highly correlated.
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variables
# Nutrients analysis  --> start.col = "PROT",    end.col = "B12_ADD"
  AverageBy(data = totals_selected, by = "UserName", start.col = "PROT", end.col = "B12_ADD")
# Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS"
  AverageBy(data = totals_selected, by = "UserName", start.col = "F_TOTAL", end.col = "A_DRINKS")

# Results are saved in this dataframe.  Probably too large to see as is.
  meansbycategorydf

# The column names should be the same as start.col-end.col. 
  colnames(meansbycategorydf)

# The row names should be each category entry to calculate means for.
  rownames(meansbycategorydf)

# pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = meansbycategorydf)
  
  # "subsetted_non0var" is the dataframe to be used in the subsequent 
  # collapse by correlation procedure.
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                   min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
# Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]

# ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
# Check to see the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)     
  dim(selected_variables)     
  
  # original
  head(subsetted_non0var, 1)
  dim(subsetted_non0var)
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, name = "corr_matrix")
# ---------------------------------------------------------------------------------------------------------------

