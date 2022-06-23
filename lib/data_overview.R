# ========================================================================================
# Functions used in data overview.
# Version 1
# Created on 06/23/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Generate a summary of each variable in a dataframe 
# ========================================================================================

# Calculate minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe.   

  SummaryStats <- function(inputdf, outfn){
    # Create an empty table to save results.
    summarybox <- data.frame(Variables=names(inputdf), 
                             Min=NA, FirstQu=NA, Median=NA, Mean=NA, ThirdQu=NA, Max=NA, SD=NA)
    # Define input.
    input <- inputdf
    
    # For each column (variable), calculate summary statistics. If not numeric, indicate so.
    for(i in 1:ncol(input)){
      
      ith_col <- input[, i]
      
      # if numeric, calculate summary stats.
      if(is.numeric(ith_col)){
        summarybox[i, 2] <- min(ith_col)
        summarybox[i, 3] <- quantile(ith_col, 0.25)
        summarybox[i, 4] <- median(ith_col)
        summarybox[i, 5] <- mean(ith_col)
        summarybox[i, 6] <- quantile(ith_col, 0.75)
        summarybox[i, 7] <- max(ith_col)
        summarybox[i, 8] <- sd(ith_col)
        
      }else{
        
        # if not numeric, say not numeric.
        summarybox[i, 2:8] <- "not_numeric"
      }  
    }
    # Save the summary information.
    write.table(summarybox, file=outfn, sep="\t", row.names=F, quote=F)
  }

# ---------------------------------------------------------------------------------------------------------------
  
