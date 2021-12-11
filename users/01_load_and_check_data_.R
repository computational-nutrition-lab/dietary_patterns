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
#          |----- data 
#          |
#          |----- lib
#          |
#          |----- users
#  Main ---|
#          |----- 
#          |
#          |----- 
#          |
#          |----- ...
#

# Import source code to run the analyses to follow.
  source("../lib/load_and_check.R")

# Set your main directory from Session tab --> Set working directory --> Choose directory
# Choose your Main directory here.

# Name the main directory for future use. 
  main.wd <- file.path(getwd())
  main.wd

# Specify the name of the folder containing the data.
# Change the folder name between the three slashes.
  
# May need to specify by hand...
  # SpecifyDataDirectory("../eg_data/dietstudy//")

# Load response file.
Resp <- LoadResponses(check_col = T)
  
  
  
  
  
  
  
  
  
  
  

  
# Load the csv files that end with each identifier.
# Ensure there is only one file of each type in your data.folder.
  resp   <- read.csv(list.files(data.folder, pattern = '\\Responses.csv$'))
  items  <- read.csv(list.files(data.folder, pattern = '\\Items.csv$'))
  INS    <- read.csv(list.files(data.folder, pattern = '\\INS.csv$'))
  totals <- read.csv(list.files(data.folder, pattern = '\\Totals.csv$'))
  TS     <- read.csv(list.files(data.folder, pattern = '\\TS.csv$'))
  TNS    <- read.csv(list.files(data.folder, pattern = '\\TNS.csv$'))
  # If an error occurs saying "invalid 'description' argument", there are
  # more than one files that end with the same identifier.
  
  # Change back to the main directory.
  setwd(main.wd)

# ========================================================================================
# Check if each file has correct columns. 
# ========================================================================================

### INS file -----------------------------------------------------------------------------  

  # Load the csv file containing the INS headers in the ASA24_colnames folder. 
  INS.colnames.df <- read.csv("ASA24_colnames/2020Adult_Recall_INS.csv", header = F)
  
  # Make a vector containing the colnames.
  INS.colnames <- as.character(INS.colnames.df[1, ]) 
  
  # Count the number of columns of the vector. Should be 72.
  INS.number.col <- length(INS.colnames)
  
  # Create a vector containing the column names of your INS.csv.  
  my.INS.colnames <- colnames(INS) 
  
  # Count the number of columns of your INS file.
  my.number.col <- length(my.INS.colnames)
  
  # If my.number.col > INS.number.col, say "There are more columns than expected."
  # If my.number.col < INS.number.col, say "There are less columns than expected."
  # If my.number.col = INS.number.col, proceed to check the column names.
  
  if (my.number.col < INS.number.col) {
    cat("There are", my.number.col, "columns in INS.csv, less than expected.")
  } else if (my.number.col > INS.number.col) {
    cat("There are", my.number.col, "columns in INS.csv, more than expected.")
  } else {
    cat("There are", my.number.col, "columns in INS.csv as expected.", "\n")
    cat("Proceed to checking the column names.", "\n")
    # Join the two column names (default and loaded) as a dataframe.
    temp1 <-  data.frame(my.INS.colnames, INS.colnames) 
    # Add a new column (match) of T or F to indicate whether the two column names match.
    temp1$match <-  temp1$my.INS.colnames == temp1$INS.colnames
    # Go through all the rows of the match column and if there is FALSE,
    # print the row(s) containing the mismatching column names.
    for(i in 1:nrow(temp1)) {
      current.row <- temp1[i, ]
      if(current.row[3] == "FALSE"){
        cat("This column name is different from the default.", "\n")
        cat("\n")
        print(current.row)
        cat("\n")
      }
    }
    cat("All the column names have been checked.")
  }
  
### items file ---------------------------------------------------------------------------
  
  # Load the csv file containing the items headers. 
  items.colnames.df <- read.csv("ASA24_colnames/2020Adult_Recall_Items.csv", header = F)
  
  # Make a vector containing the colnames.
  items.colnames <- as.character(items.colnames.df[1, ])  
  
  # Count the number of columns of the vector. Should be 130.
  items.number.col <- length(items.colnames)
  
  # Create a vector containing the column names of your items.csv.  
  my.items.colnames <- colnames(items) 
  
  # Count the number of columns of your items file. 
  my.number.col <- length(my.items.colnames)
  
  # If my.number.col > items.number.col, say "There are more columns than expected."
  # If my.number.col < items.number.col, say "There are less columns than expected."
  # If my.number.col = items.number.col, proceed to check the column names.
  
  if (my.number.col < items.number.col) {
    cat("There are", my.number.col, "columns in items.csv, less than expected.")
  } else if (my.number.col > items.number.col) {
    cat("There are", my.number.col, "columns in items.csv, more than expected.")
  } else {
    cat("There are", my.number.col, "columns in items.csv as expected.", "\n")
    cat("Proceed to checking the column names.", "\n")
    # Join the two column names (default and loaded) as a dataframe.
    temp1 <-  data.frame(my.items.colnames, items.colnames) 
    # Add a new column (match) of T or F to indicate whether the two column names match.
    temp1$match <-  temp1$my.items.colnames == temp1$items.colnames
    # Go through all the rows of the match column and if there is FALSE,
    # print the row(s) containing the mismatching column names.
    for(i in 1:nrow(temp1)) {
      current.row <- temp1[i, ]
      if(current.row[3] == "FALSE"){
        cat("This column name is different from the default.", "\n")
        cat("\n")
        print(current.row)
        cat("\n")
      }
    }
    cat("All the column names have been checked.")
  }  
  
# resp file -------------------------------------------------------------------------------
  
  # Load the csv file containing the Responses headers. 
  resp.colnames.df <- read.csv("ASA24_colnames/2020Adult_Recall_Responses.csv", header = F)
  
  # Make a vector containing the colnames.
  resp.colnames <- as.character(resp.colnames.df[1, ])  
  
  # Count the number of columns of the vector. Should be 28.
  resp.number.col <- length(resp.colnames)
  
  # Create a vector containing the column names of your responses.csv.
  my.resp.colnames <- colnames(resp) 
  
  # Count the number of columns of your resp file.
  my.number.col <- length(my.resp.colnames)
  
  # If my.number.col > resp.number.col, say "There are more columns than expected."
  # If my.number.col < resp.number.col, say "There are less columns than expected."
  # If my.number.col = resp.number.col, proceed to check the column names.
  
  if (my.number.col < resp.number.col) {
    cat("There are", my.number.col, "columns in responses.csv, less than expected.")
  } else if (my.number.col > resp.number.col) {
    cat("There are", my.number.col, "columns in responses.csv, more than expected.")
  } else {
    cat("There are", my.number.col, "columns in responses.csv as expected.", "\n")
    cat("Proceed to checking the column names.", "\n")
    # Join the two column names (default and loaded) as a dataframe.
    temp1 <-  data.frame(my.resp.colnames, resp.colnames) 
    # Add a new column (match) of T or F to indicate whether the two column names match.
    temp1$match <-  temp1$my.resp.colnames == temp1$resp.colnames
    # Go through all the rows of the match column and if there is FALSE,
    # print the row(s) containing the mismatching column names.
    for(i in 1:nrow(temp1)) {
      current.row <- temp1[i, ]
      if(current.row[3] == "FALSE"){
        cat("This column name is different from the default.", "\n")
        cat("\n")
        print(current.row)
        cat("\n")
      }
    }
    cat("All the column names have been checked.")
  }

    
### TNS file ---------------------------------------------------------------------------  
  
  # Load the csv file containing the TNS headers. 
  TNS.colnames.df <- read.csv("ASA24_colnames/2020Adult_Recall_TNS.csv", header = F)
  
  # Make a vector containing the colnames.
  TNS.colnames <- as.character(TNS.colnames.df[1, ])  

  # Count the number of columns of the vector. Should be 91.
  TNS.number.col <- length(TNS.colnames)
  
  # Create a vector containing the column names of your TNS.csv.  
  my.TNS.colnames <- colnames(TNS) 
  
  # Count the number of columns of your TNS file.
  my.number.col <- length(my.TNS.colnames)
  
  # If my.number.col > TNS.number.col, say "There are more columns than expected."
  # If my.number.col < TNS.number.col, say "There are less columns than expected."
  # If my.number.col = TNS.number.col, proceed to check the column names.
  
  if (my.number.col < TNS.number.col) {
    cat("There are", my.number.col, "columns in TNS.csv, less than expected.")
  } else if (my.number.col > TNS.number.col) {
    cat("There are", my.number.col, "columns in TNS.csv, more than expected.")
  } else {
    cat("There are", my.number.col, "columns in TNS.csv as expected.", "\n")
    cat("Proceed to checking the column names.", "\n")
    # Join the two column names (default and loaded) as a dataframe.
    temp1 <-  data.frame(my.TNS.colnames, TNS.colnames) 
    # Add a new column (match) of T or F to indicate whether the two column names match.
    temp1$match <-  temp1$my.TNS.colnames == temp1$TNS.colnames
    # Go through all the rows of the match column and if there is FALSE,
    # print the row(s) containing the mismatching column names.
    for(i in 1:nrow(temp1)) {
      current.row <- temp1[i, ]
      if(current.row[3] == "FALSE"){
        cat("This column name is different from the default.", "\n")
        cat("\n")
        print(current.row)
        cat("\n")
      }
    }
    cat("All the column names have been checked.")
  }
  
  
### totals file ---------------------------------------------------------------------------  
  
  # Load the csv file containing the totals headers. 
  totals.colnames.df <- read.csv("ASA24_colnames/2020Adult_Recall_Totals.csv", header = F)
  
  # Make a vector containing the colnames.
  totals.colnames <- as.character(totals.colnames.df[1, ])  
  
  # Count the number of columns of the vector. Should be 116.
  totals.number.col <- length(totals.colnames)
  
  # Create a vector containing the column names of your totals.csv.  
  my.totals.colnames <- colnames(totals) 
  
  # Count the number of columns of your totals file.
  my.number.col <- length(my.totals.colnames)
  
  # If my.number.col > totals.number.col, say "There are more columns than expected."
  # If my.number.col < totals.number.col, say "There are less columns than expected."
  # If my.number.col = totals.number.col, proceed to check the column names.
  
  if (my.number.col < totals.number.col) {
    cat("There are", my.number.col, "columns in totals.csv, less than expected.")
  } else if (my.number.col > totals.number.col) {
    cat("There are", my.number.col, "columns in totals.csv, more than expected.")
  } else {
    cat("There are", my.number.col, "columns in totals.csv as expected.", "\n")
    cat("Proceed to checking the column names.", "\n")
    # Join the two column names (default and loaded) as a dataframe.
    temp1 <-  data.frame(my.totals.colnames, totals.colnames) 
    # Add a new column (match) of T or F to indicate whether the two column names match.
    temp1$match <-  temp1$my.totals.colnames == temp1$totals.colnames
    # Go through all the rows of the match column and if there is FALSE,
    # print the row(s) containing the mismatching column names.
    for(i in 1:nrow(temp1)) {
      current.row <- temp1[i, ]
      if(current.row[3] == "FALSE"){
        cat("This column name is different from the default.", "\n")
        cat("\n")
        print(current.row)
        cat("\n")
      }
    }
    cat("All the column names have been checked.")
  }
  
  
### TS file ------------------------------------------------------------------------------   
  
  # Load the csv file containing the TS headers. 
  TS.colnames.df <- read.csv("ASA24_colnames/2020Adult_Recall_TS.csv", header = F)

  # Make a vector containing the colnames.
  TS.colnames <- as.character(TS.colnames.df[1, ])  
  
  # Count the number of columns of the vector. Should be 65.
  TS.number.col <- length(TS.colnames)

  # Create a vector containing the column names of your TS.csv.  
  my.TS.colnames <- colnames(TS) 
  
  # Count the number of columns of your TS file. 
  my.number.col <- length(my.TS.colnames)
  
  # If my.number.col > TS.number.col, say "There are more columns than expected."
  # If my.number.col < TS.number.col, say "There are less columns than expected."
  # If my.number.col = TS.number.col, proceed to check the column names.
  
  if (my.number.col < TS.number.col) {
    cat("There are", my.number.col, "columns in TS.csv, less than expected.")
  } else if (my.number.col > TS.number.col) {
    cat("There are", my.number.col, "columns in TS.csv, more than expected.")
  } else {
    cat("There are", my.number.col, "columns in TS.csv as expected.", "\n")
    cat("Proceed to checking the column names.", "\n")
    # Join the two column names (default and loaded) as a dataframe.
    temp1 <-  data.frame(my.TS.colnames, TS.colnames) 
    # Add a new column (match) of T or F to indicate whether the two column names match.
    temp1$match <-  temp1$my.TS.colnames == temp1$TS.colnames
    # Go through all the rows of the match column and if there is FALSE,
    # print the row(s) containing the mismatching column names.
    for(i in 1:nrow(temp1)) {
      current.row <- temp1[i, ]
      if(current.row[3] == "FALSE"){
        cat("This column name is different from the default.", "\n")
        cat("\n")
        print(current.row)
        cat("\n")
      }
    }
    cat("All the column names have been checked.")
  }
  
