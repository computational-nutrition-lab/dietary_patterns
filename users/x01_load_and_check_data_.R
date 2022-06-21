# For USERS ==============================================================================

# ========================================================================================
# Load ASA24 output (data) files for analysis and visualization
# Version 1
# Created on 12.11.2021 by Rie Sadohara
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

# Load example data files ============================================================= 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  
# Leave "check_col = TRUE" as is to check the column names of the loaded responses.csv.
# Load your response file.
  resp <- LoadResponses(check_col = TRUE)

# ???????????????????????
# Most of the time, the columns will be different from the ASA24 default setup.
# could just load data here and check column names later for each analysis?
  
  
  
  
 