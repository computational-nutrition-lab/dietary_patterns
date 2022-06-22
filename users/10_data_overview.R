# ========================================================================================
# Take a overview of ASA24 items and totals data.
# Version 1
# Created on 06/22/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Set working directory 
# ========================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  # source("lib/specify_dir_and_check_col.R")  
  # source("lib/load_clean_ASA24.R")
  # source("lib/format.file.R")

# You can come back to the main directory by:
  setwd(main_wd)

# ========================================================================================
# Load and analyze (QC-ed) ASA24 items data
# ========================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  

# Load your items data to be analyzed.
  items_f_s_m <- read.table("VVKAJ_2021-11-09_7963_Items_f_s_m.txt", sep="\t", header=T)
  head(items_f_s_m)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics

# for each column (variable), calculate summary statistics. If not numeric, indicate so.
  # create a table to save results.
  summarybox <- data.frame(Variables=names(items_f_s_m), Min=NA, FirstQu=NA, Median=NA, Mean=NA, ThirdQu=NA, Max=NA)
  
  input <- items_f_s_m
  
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
    
    }else{
      
    # if not numeric, say not numeric.
      summarybox[i, 2:7] <- "not_numeric"
    }  
  }
  summarybox

# Save the summary information.
  write.table(summarybox, "VVKAJ_2021-11-09_7963_Items_f_s_m_summ.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Boxplot
  
  ######### JUNE 2022 REVISION GOING ON. RESUME FROM HERE. ########## 
  
  
  
  
  
# ========================================================================================
# Load (QC-ed) ASA24 totals data
# ========================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  

# Load your totals data to be analyzed.
  totals <- read.table(QCtotals, "VVKAJ_2021-11-09_7963_Tot_m_QCed.txt", sep="\t", header=T)


 
# ---------------------------------------------------------------------------------------------------------------
# Header 2 -- explain the purpose of this subsection. 
# Operations....
  myfunction(arg1 = mydata)  # comment in lower case

  head(mydata_analysis)
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# A function or a chunk of code to do one operation. 
  MyFunctionToRun <- function(arg.one = my_object_1, arg.two = my_object_2){
    #dothis <<- basefunction()  # comment in lower case
  }

# ---------------------------------------------------------------------------------------------------------------
  
