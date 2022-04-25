# ========================================================================================
# Prepare NHANES food and items data for clustering analysis.
# 
# Version 1
# Created on 04/12 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Use the prep_data, PCA, and k_means scripts to analyze this data!   
# ========================================================================================

Set working directory by: Session --> Set Working Directory --> Choose Directory
setwd("~/GitHub/dietary_patterns")

# Load the necessary functions
source("lib/prep_data_for_clustering.R")
source("lib/PCA.R")
source("lib/k-means.R")

# ========================================================================================
# Load QC-ed NHANES items or totals    
# ========================================================================================

  # Load the QC-ed and sampled totals file. 
    totals_QCed_sampled <- read.table("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/NHANES_totals_QCed_sampled_PCAs_18ind.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_totals_QCed_sampled_PCAs_18ind.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_totals_QCed_sampled.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_2days_totals_QCed_sampled.txt", sep="\t", header=T)
    
    dim(totals_QCed_sampled)                                                            
    head(totals_QCed_sampled,1)
  
  # OR load the QC-ed and sampled food items file.  
    foods_QCed <- read.table("eg_data/NHANES/NHANES_foods_QCed_sampled.txt", sep="\t", header=T)
  
###### CHOOSE EITHER 1 OR 2 OF THE FOLLOWING: 1: WITHOUT AVEAGING; 2: WITH AVERAGING. #######

  # ---------------------------------------------------------------------------------------------------------------
  # 1. If using each dataponit as is WITHOUT AVERAGING, 
  # Subset totals or food items data.
  
    # FOOD ITEMS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Define the input data to be used.
       input_data <- foods_QCed
      # The columns specified as start.col, end.col, and all columns in between will be selected.
      # Items   --> start.col = "DR1IPROT",     end.col = "DR1IP226"
      SubsetColumns(data=input_data, start.col="DR1IPROT", end.col = "DR1IP226")
       
    # TOTALS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Define the input data to be used.
       input_data <- totals_QCed_sampled
      # The columns specified as start.col, end.col, and all columns in between will be selected.
      # Totals  --> start.col = "KCAL",    end.col = "P226"
      SubsetColumns(data=input_data, start.col="KCAL", end.col = "P226")

    # The output is a df called "subsetted".
    
  # pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)

  # The out put is a df called "subsetted_non0var".
  colnames(subsetted_non0var)
# ---------------------------------------------------------------------------------------------------------------
    
### No. 2 below is not needed, I think, because the average is totals, and as is is food items.
###    since there is only 1 day of NHANES data. 
  # 
  # # ---------------------------------------------------------------------------------------------------------------
  # #  2. If taking average of the food items of each user for each of the nutrients.
  #   
  # # Define the input data to be used.
  #   input_data <- foods_QCed_30
  #   input_data <- totals_QCed_1500
  #   colnames(input_data)
  #   
  #   # Totals  --> start.col = "DR1TPROT",    end.col = "DR1TP226"
  #   SubsetColumns(data=input_data, start.col="DR1TPROT", end.col = "DR1TP226")
  #   # Items   --> start.col = "DR1IPROT",     end.col = "DR1IP226"
  #   SubsetColumns(data=input_data, start.col="DR1IPROT", end.col = "DR1IP226")
  #   # The output is a df called "subsetted".
  #   
  #   AverageBy(data = subsetted, by = "SEQN", start.col = "DR1TKCAL", end.col = "DR1TP226")
  #   
  #   # The column names should be the same as start.col - end.col. 
  #   colnames(meansbycategorydf)
  #   
  #   # pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  #   # The removed columns will be shown if any.
  #   KeepNonZeroVarColumns(data = subsetted)
  #   
  #   # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  #   
  # # ---------------------------------------------------------------------------------------------------------------

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
  
# Save the variables after removing correlated variables
  write.table(selected_variables, "results/PCA_results/2 days 50 ind/variables_retained_2day_50.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
  # Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, out.fn = "results/PCA_results/2 days 50 ind/NHANES_2days_totals_QCed_sampled_50ind_corr_matrix.txt")
# ---------------------------------------------------------------------------------------------------------------
  

