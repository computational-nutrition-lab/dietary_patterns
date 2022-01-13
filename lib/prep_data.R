# FUNCTIONS ==============================================================================

# ========================================================================================
# Prep data for PCA and other cluster analysis.
# Version 1
# Created on 01.13.2021 by Rie Sadohara
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Function to subset specific data from the totals.

SubsetColumns <- function(data, start.col, end.col){
  # Column Variables of "totals" dataframe.
  colvars <- names(data)
  # Get the first ID
  start.col <- match(start.col, colvars)
  # Get the second ID
  end.col <- match(end.col, colvars)
  # Subset range
  subsetted <<- data[, start.col:end.col]
  # Print what was loaded.
  cat("'subsetted' contains the following", length(colnames(subsetted)), "columns.", "\n")
  print(colnames(subsetted))
}
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Keep only the columns with non-zero variance in order to perform PCA.
KeepNonZeroVarColumns <- function(data){
  subsetted_non0var <<- data[, which(apply(data, 2, var) != 0)] 
  # Print which column(s) were removed.
  if(ncol(data) == ncol(subsetted_non0var)){
    cat("No columns were removed.", "\n")
  }
  if(ncol(data) != ncol(subsetted_non0var)){
    cat("The following column(s) in ", deparse(substitute(data)), " had zero variance and were removed.", "\n")
    print(which(apply(data, 2, var) == 0))
  }
}
# ---------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------
# Average each participant: average by username.
# Need to take average per user in the totals because subsetted_non0var does not have UserName.

# Function to take average by a specified column. 
AverageBy <- function(data, by, start.col, end.col){
  library(dplyr)
  # Column Variables of "totals" as a dataframe.
  colvars <<- names(data)
  # Get the first ID
  start_col_number <<- match(start.col, colvars)
  # Get the last ID
  end_col_number <<- match(end.col, colvars)
  # Subset the 'by' column and the columns in the start-end range.
  by_start_end <<- data[, c(which(colnames(data)==by), start_col_number:end_col_number)]
  
  # Define which variablese to take means. and a list to save restuls
  myvar <<- colnames(by_start_end)[-1]
  # Define the category entries to calculate means for.
  category_by <<- unique(by_start_end[, 1]) 
  # Create a dataframe with the rownames corresponding to the category entries to calculate means for. 
  meansbycategorydf <<- data.frame(row.names = category_by)
  
  # Calculate means for each colname in myvar. 
  for(i in 1:length(myvar)){
    # mymean <<- tapply(X = df[, i+1], INDEX = as.factor(df[, 1]), FUN = mean )
    resarray <<- tapply(X = by_start_end[, i+1], INDEX = as.factor(by_start_end[, 1]), FUN = mean)
    resdf <<- data.frame(resarray)   # save the results (array) as a dataframe.
    colnames(resdf) <<- myvar[i]     # name the column with the variable.
    meansbycategorydf <<- cbind(meansbycategorydf, resdf)   # add the new resdf to the existing result meansbycategorydf.
  }
}
# ---------------------------------------------------------------------------------------------------------------



