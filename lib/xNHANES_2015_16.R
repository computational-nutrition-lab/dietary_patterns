# ========================================================================================
# Functions to Functions to load NHANES 2015-16 data and take random samples.
# Copied to load_clean_NHANES.R.
# Version 1
# Created on 04/13/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import NHANES 2015-16 data. 
# ========================================================================================
# 
# Import NHANES data using the SASexport package.
# install.packages("SASxport")
  library(SASxport)
  library(foreign)

# ========================================================================================
# Load Food items data and add food descriptions.  
# ========================================================================================

# Load food items and bring the food description to the first column.
# 
  LoadNHANESFoodItems <- function(data.name, food.code.column, food.code.table){
    
    nhanes1516_items <- read.xport(data.name)
  
    # Add a sequential number to sort the rows after merging.
    nhanes1516_items$id <- 1:nrow(nhanes1516_items)
    
    # Make a copy of the food code column as integer at the end of nhanes1516_items 
    nhanes1516_items$Food_code <- as.integer(nhanes1516_items[, food.code.column])
    
    # Bring the food code to the first column for merging. 
    nhanes1516_items_s <- nhanes1516_items[, c(length(colnames(nhanes1516_items)), 
                                               1:length(colnames(nhanes1516_items))-1)]
    
    # Load the text file with food code and descriptions. 
    codetable <- read.xport(food.code.table)
    
    # Change the first column name (DRXFDCD) to "Food_code" for merging.
    colnames(codetable)[1] <- "Food_code"
    
    # Make the Food code as integer here, too. 
    codetable$Food_codeint <- as.integer(codetable$Food_code)  
    
    # Merge the NHANES data and codetable.
    nhanes1516 <- merge(x=nhanes1516_items_s, y=codetable, 
                        by="Food_code", all.x=T)        # all.x=T matches all the rows in the 1st dataframe.
    
    # Merging sorts the dataframe by Food_code, so   
    # sort back by the id (the original order of nhanes1516_raw)
    nhanes1516_s <<- nhanes1516[order(nhanes1516$id), ]  
    
    # Food description added! DRXFCSD is short descriptions, and DRXFCLD are long descriptions.
  }


# ========================================================================================
# Take a random subsample.   
# ========================================================================================

  RandomSample <- function(data, n){

    # Define your whole dataset. 
    wholedata <- data
    
    if(n > length(unique(wholedata$SEQN))){
      cat("Error: n is larger than the number of original data (", 
          length(unique(Food_D1$SEQN)),").\nPlease specify a smaller n to sample.", sep="")
    }else{
      # Choose n random samples of participant ID
      subsetusers <- sample(unique(wholedata$SEQN), n)
      
      # Subset only those found in the chosen ID list.
      nhanes_sub1 <<- wholedata[wholedata$SEQN %in% subsetusers, ]
      
      # Confirm the desired number of participants were selected.
      cat( length(unique(nhanes_sub1$SEQN)), " participants were subsampled.", sep = "") 
      
    }
  }

 