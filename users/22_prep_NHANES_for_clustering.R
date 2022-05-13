# ========================================================================================
# Prepare NHANES food and items data for clustering analysis.
# Modified version, after finishing writing code to load_clean_NHANES_food.R.
# Version 2
# Created on 05/04/2022 by Rie Sadohara
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
# Load QC-ed NHANES FOOD ITEMS    
# ========================================================================================

# Load the QC-ed food items. 
  # food12d <- read.table("eg_data/NHANES/NHANES1516_items_d12_QC_500sampled.txt", sep="\t", header=T)

# How do you use food items data of just 2 days for clustering? Maybe not needed.

# ========================================================================================
# OR Load QC-ed NHANES TOTAL     
# ========================================================================================

# Load the subsetted totals file. 
  totals_QCed_sampled <- read.table(        "eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_100sampled.txt", sep="\t", header=T)

    # Some checking... about MOIS and GRMS...
     head(totals_QCed_sampled, 2)
     hist(totals_QCed_sampled$MOIS)
     plot(totals_QCed_sampled$MOIS, totals_QCed_sampled$GRMS)
     totals_QCed_sampled$GRMSminusMOIS <- totals_QCed_sampled$GRMS - totals_QCed_sampled$MOIS
     hist(totals_QCed_sampled$GRMSminusMOIS)
     plot(totals_QCed_sampled$GRMS, totals_QCed_sampled$GRMSminusMOIS)
     cor.test(totals_QCed_sampled$GRMS, totals_QCed_sampled$GRMSminusMOIS)
     colnames(totals_QCed_sampled)
  
  # Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")
  
  # Take only the columns whose names are NOT in the drop vector. 
  aaa <- totals_QCed_sampled[ , !(names(totals_QCed_sampled) %in% drops)]
  
  # Save it as totals_QCed_sampled. 
  totals_QCed_sampled <- aaa
  colnames(totals_QCed_sampled)

# Define the input data to be used.
  input_data <- totals_QCed_sampled

# The columns specified as start.col, end.col, and all columns in between will be selected.
  # Nutrients
  SubsetColumns(data=input_data, start.col="PROT", end.col="P226")
  # OR food categories
  SubsetColumns(data=input_data, start.col="F_CITMLB", end.col="A_DRINKS")

  # The output is a df called "subsetted".
  
# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # The output is a df called "subsetted_non0var".
  
# Check the columns (variables) remained.
  colnames(subsetted_non0var)  
  dim(subsetted_non0var)  

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
  write.table(selected_variables, 
              "results/PCA_results/NHANES1516_totalsbyhand_nut_n100/NHANES1516_total_d12_nut_mean_QC_2_100sampled_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, 
                 out.fn = "results/PCA_results/NHANES1516_totalsbyhand_nut_n100/NHANES1516_total_d12_nut_mean_QC_2_100sampled_corr_mat.txt")
  # ---------------------------------------------------------------------------------------------------------------
  

   ###### DONE ######
  
  
  
  
  
  
  
  
  ###### CODE BELOW MAY OR MAY NOT BE USEFUL --- TO BE DELETED AS IT'S NOT NEEDED #########  



  # Load the QC-ed and sampled totals file. 
    totals_QCed_sampled <- read.table("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/NHANES_totals_QCed_sampled_PCAs_18ind.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_totals_QCed_sampled_PCAs_18ind.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_totals_QCed_sampled.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_2days_totals_QCed_sampled.txt", sep="\t", header=T)
    totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_2days_totals_QCed_1000sampled.txt", sep="\t", header=T)
    
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
  # The output is a df called "subsetted_non0var".

  # Check the columns (variables) remained.
  colnames(subsetted_non0var)
# ---------------------------------------------------------------------------------------------------------------
    
