# FUNCTIONS ==============================================================================

# ========================================================================================
# Load ASA24 output (data) files 
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

# Specify the name of the folder containing the data, ------------------------------------ 
# and make it as a current directory.
  SpecifyDataDirectory <- function(directory.name){
      data.folder <<- paste(main_wd, directory.name,  sep = .Platform$file.sep )  
      # '<<-' makes objects usable outside function
      setwd(data.folder)
      print("The data directory has been set as")
      return(data.folder)
  }

  
# Not needed...?
# =========================================================================================
# Load the data files and check the column names if check_col = TRUE.
# resp file -------------------------------------------------------------------------------
  LoadResponses <- function(check_col = TRUE){
    resp <- read.csv(list.files(pattern = '\\Responses.csv$'))
    cat(paste("Loaded ", list.files(pattern = '\\Responses.csv$'), sep = ""), "\n")

    if(check_col == TRUE)
    # Load the csv file containing the Responses headers.
    resp.colnames.df <- read.csv("../../lib/ASA24_colnames/2020Adult_Recall_Responses.csv",
                                 header = F)
    cat("Loaded ASN24 resp.colnames", "\n")

    # Make a vector containing the colnames.
    resp.colnames <- as.character(resp.colnames.df[1, ])

    # Count the number of columns of the vector. ASA24 default is 28.
    resp.number.col <- length(resp.colnames)

    # Create a vector containing the column names of your responses.csv.
    my.resp.colnames <- colnames(resp)

    # Count the number of columns of your resp file.
    my.number.col <- length(my.resp.colnames)

    # If my.number.col < resp.number.col,
    #   then say "There are less columns than expected." and show the columns loaded.
    if (my.number.col < resp.number.col) {
        cat("There are", my.number.col, "columns in responses.csv, less than expected.", "\n")
        cat("Here are the columns of your responses data.", "\n")
        print(as.data.frame(my.resp.colnames))
    }
    # If my.number.col > resp.number.col,
    #   then say "There are more columns than expected." and show the columns loaded.
    else if (my.number.col > resp.number.col) {
        cat("There are", my.number.col, "columns in responses.csv, more than expected.", "\n")
        cat("Here are the columns of your responses data.", "\n")
        print(as.data.frame(my.resp.colnames))
    }
    # If my.number.col = resp.number.col,
    #   then proceed to check the column names.
    else {
        cat("There are", my.number.col, "columns in responses.csv as expected.", "\n")
        cat("Proceed to checking the column names.", "\n")

        # Join the two column names (default and loaded) as a dataframe.
        temp1 <- data.frame(my.resp.colnames, resp.colnames)

        # Add a new column (match) of T or F to indicate whether the two column names match.
        temp1$match <- temp1$my.resp.colnames == temp1$resp.colnames

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
    }

  # LoadResponses(check_col = F)


