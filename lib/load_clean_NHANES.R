# ========================================================================================
# Functions to load NHANES 2015-16 data and take random samples.
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
# Load and prepare food code table.  
# ========================================================================================

# Load food items and bring the food description to the first column.

  PrepareFoodCodeTable <- function(raw.food.code.table, out.fn){
  
    codetable <- read.xport(raw.food.code.table)
    
    # Replace symbols (/, \, ', #, &) in DRXFCSD column that cannot be loaded correctly 
    codetable[, "DRXFCSD"] <- gsub("\"", "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("'", "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("#",  "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("&",  "and", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("/",  "_", codetable[, "DRXFCSD"])
    codetable[, "DRXFCSD"] <- gsub("\"", "_", codetable[, "DRXFCSD"])
  
    # Replace symbols (/, \, ', #, &) in DRXFCLD column that cannot be loaded correctly 
    codetable[, "DRXFCLD"] <- gsub("\"", "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("'", "_", codetable[, "DRXFCLD"]) 
    codetable[, "DRXFCLD"] <- gsub("#",  "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("&",  "and", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("/",  "_", codetable[, "DRXFCLD"])
    codetable[, "DRXFCLD"] <- gsub("\"", "_", codetable[, "DRXFCLD"])
  
    # Save as a txt file.   
    write.table(codetable, out.fn, sep="\t", row.names=F)
  
  }

# ========================================================================================
# Load Food items data and add food descriptions.  
# ========================================================================================

# Load food items and bring the food description to the first column.
 
ImportNHANESFoodItems <- function(data.name, food.code.column, food.code.table, out.fn){
  
  nhanes1516_items <- read.xport(data.name)
  
  # make the rownames as a column called originalrownames
  nhanes1516_items$originalrownames <- rownames(nhanes1516_items)
  
  # Add a sequential number to sort the rows after merging.
  nhanes1516_items$id <- 1:nrow(nhanes1516_items)
  
  # Make a copy of the food code column as integer at the end of nhanes1516_items 
  nhanes1516_items$Food_code <- as.integer(nhanes1516_items[, food.code.column])
  
  # Bring the food code to the first column for merging. 
  nhanes1516_items_s <- nhanes1516_items[, c(length(colnames(nhanes1516_items)), 
                                             1:length(colnames(nhanes1516_items))-1)]
  
  # Load the text file with food code and descriptions. 
  codetable <- food.code.table
  
  # Change the first column name (DRXFDCD) to "Food_code" for merging.
  colnames(codetable)[1] <- "Food_code"
  
  # Make the Food code as integer here, too. 
  codetable$Food_codeint <- as.integer(codetable$Food_code)  
  
  # Merge the NHANES data and codetable.
  nhanes1516 <- merge(x=nhanes1516_items_s, y=codetable, 
                      by="Food_code", all.x=T)        # all.x=T matches all the rows in the 1st dataframe.
  
  # Merging sorts the dataframe by Food_code, so   
  # sort back by the id (the original order of nhanes1516_raw)
  nhanes1516_s <- nhanes1516[order(nhanes1516$id), ]  
  # Food description added! DRXFCSD is short descriptions, and DRXFCLD are long descriptions.
  
  # Save it as a txt file with the specified name. 
  write.table(nhanes1516_s, out.fn, sep="\t", row.names=F, quote=F)
  
}

# ========================================================================================
# Take a random subsample.   
# ========================================================================================

RandomSample <- function(data, n, out.fn){
  
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
    
    # Save the samples as a txt file with the specified name. 
    write.table(nhanes_sub1, out.fn, sep="\t", row.names=F, quote=F)
    
  }
}

