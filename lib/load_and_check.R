# FUNCTIONS ==============================================================================

# ========================================================================================
# Load ASA24 output (data) files for analysis and visualization
# Version 1
# Created on 12.11.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================

# 
# GitHub folder structure 
# 
#          |----- eg_data --- ASA24 example datasets
#          | 
#          |----- lib ---- functions
#          |           |-- ASA24_colnames
#          |
#          |----- users -- R_scripts that users will run.
#  Main ---|
#          |----- 
#          |
#          |----- 
#          |
#          |----- ...
#


# Name the main directory for future use. 
#  main.wd <- file.path(getwd())
#  main.wd

# Function to specify the name of the folder containing the data, 
# and make it as a current directory.
   SpecifyDataDirectory <- function(directory.name){
     data.folder <<- paste(main.wd, directory.name,  sep = "" )  # '<<-' makes objects usable outside function
     setwd(data.folder)
     print("The data directory has been set as")
     return(data.folder)
     }
    
