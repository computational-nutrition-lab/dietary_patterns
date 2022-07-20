# For USERS ==============================================================================

# ===============================================================================================================
# Prepare data for PCA and other cluster analysis.
# Version 1
# Created on 01.13.2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Import data and prepare them for analyses
# ===============================================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Folder structure 
# 
#                          |----- eg_data 
#                          |
#                          |----- lib
#                          |
#                          |----- users
#  Main -------------------|
#  (dietary_patterns)      |----- results
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# Come back to the main directory
  setwd(main_wd)   

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")
  source("lib/prep_data_for_clustering.R")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  # SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  SpecifyDataDirectory(directory.name= "eg_data/VVKAJ/")

# ASA24 data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the totals data:
  # totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)
  totals <- read.table("VVKAJ_Tot_m_QCed.txt", sep = "\t", header = T)

# Load the items.txt
# items <- read.table("VVKAJ_Items_f_s_m.txt", quote = "", sep = "\t", header = T)

# Define your input dataset (may not be necessary, but keeping this because I'm not
  # sure how totals_selected are made. I think it's the same as totals_QC, though.)
  totals_selected <- totals
  

# ===============================================================================================================
# NUTRIENTS: Use data as is. 
# ===============================================================================================================

# Subset nutrients data.
  # The columns specified as start.col, end.col, and all columns in between will be selected.
  # Nutrients analysis --> start.col = "PROT",  end.col = "B12_ADD", 64 variables in total.
  SubsetColumns(data = totals_selected, start.col = "PROT", end.col = "B12_ADD")  
  
  # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var, min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
  # Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]
  
  # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
  # Check the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)
  dim( selected_variables)
  
  # original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the selected_variables as a .txt file. This will be the input for clustering analyses. 
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_asis.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation by using 
  # the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_asis_corr_matrix.txt")
  
  
# ===============================================================================================================
# NUTRIENTS: Take average of each user across all days 
# ===============================================================================================================
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variable.
  # Nutrients analysis  --> start.col = "PROT",    end.col = "B12_ADD"
  AverageBy(data= totals_selected, by= "UserName", start.col= "PROT", end.col= "B12_ADD")
  
  # Save the averaged results.
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Nut_ave_allvar.txt", sep="\t", row.names=F, quote=F)
  
  # The column names should be the same as start.col-end.col. 
  colnames(meansbycategorydf)
  
  # The 'UserName' column has the users to calculate means for.
  meansbycategorydf$UserName
  
  # Pick up only the columns with non-zero variance, in order to run PCA and cluster analysis etc.
  # The removed columns will be shown if any.
  # [,-1] is to exclude the UserName column that is not numeric and not used for variance calculation. 
  KeepNonZeroVarColumns(data = meansbycategorydf[, -1])
  
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var, min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
  # Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]
  
  # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
  # Check the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)
  dim( selected_variables)
  
  # original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the selected_variables as a .txt file. This will be the input for clustering analyses.
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_ave_subset.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation by using 
# the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_ave_corr_matrix.txt")
  
  
# ===============================================================================================================
# FOOD CATEGORIES: Use data as is.
# ===============================================================================================================
# Subset food items data.
  # The columns specified as start.col, end.col, and all columns in between will be selected.
  # Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS", 37 varialbes in total.
  SubsetColumns(data = totals_selected, start.col = "F_TOTAL", end.col = "A_DRINKS")  
  
  # Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var, min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
  # Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]
  
  # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
  # Check the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)
  dim( selected_variables)
  
  # original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the selected_variables as a .txt file. This will be the input for clustering analyses.
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Cat_asis.txt", sep="\t", row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation by using 
  # the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Cat_asis_corr_matrix.txt")
  

# ===============================================================================================================
# FOOD CATEGORIES: Take average of each user across all days
# ===============================================================================================================
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variable.
  # Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS"
  AverageBy(data= totals_selected, by= "UserName", start.col= "F_TOTAL", end.col= "A_DRINKS")
  
  # Save the averaged results.
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Cat_ave_allvar.txt", sep="\t", row.names=F, quote=F)
  
  # The column names should be UserName + start.col-end.col. 
  colnames(meansbycategorydf)
  
  # The 'UserName' column has the users to calculate means for.
  meansbycategorydf$UserName
  
  # Pick up only the columns with non-zero variance, in order to run PCA and cluster analysis etc.
  # The removed columns will be shown if any.
  # [,-1] is to exclude the UserName columns that is not numeric and not used for variance calculation. 
  KeepNonZeroVarColumns(data = meansbycategorydf[, -1])
  
  # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var, min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
  # Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]
  
  # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
  # Check the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)
  dim( selected_variables)
  
  # original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the selected_variables as a .txt file. This will be the input for clustering analyses.
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Cat_ave_subset.txt", sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation by using 
# the CollapseByCorrelation function.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Cat_ave_corr_matrix.txt")
  
  
# ===============================================================================================================
# Come back to the main directory
  setwd(main_wd) 
  
  #
  
  
  
  
  
  
  
  
  
  
  
  
  ######### OLD BELOW #########
  
 
######## CHOOSE EITHER 1 OR 2 OF THE FOLLOWING: 1: WITHOUT AVEAGING; 2: WITH AVERAGING. #########
  
# ---------------------------------------------------------------------------------------------------------------
# 1. If using each datapoint as is WITHOUT AVERAGING by users, 

# Subset nutrients or food items data.
# The columns specified as start.col, end.col, and all columns in between will be selected.
# Nutrients analysis --> start.col = "PROT",  end.col = "B12_ADD", 64 variablees in total.
  SubsetColumns(data = totals_selected, start.col = "PROT",    end.col = "B12_ADD")  
# Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS", 37 varialbes in total.
  SubsetColumns(data = totals_selected, start.col = "F_TOTAL", end.col = "A_DRINKS")  

# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # "subsetted_non0var" is the dataframe to be used in the subsequent
  # collapse by correlation procedure.
# ---------------------------------------------------------------------------------------------------------------
  
######### OR #########
  
# ---------------------------------------------------------------------------------------------------------------
# 2. If taking average of each user across all days first,
# Specify the data to be used, category to group by, and the range of columns (variables) 
# to calculate the means of each variable.
# Nutrients analysis  --> start.col = "PROT",    end.col = "B12_ADD"
  AverageBy(data= totals_selected, by= "UserName", start.col= "PROT", end.col= "B12_ADD")
# Food items analysis --> start.col = "F_TOTAL", end.col = "A_DRINKS"
  AverageBy(data= totals_selected, by= "UserName", start.col= "F_TOTAL", end.col= "A_DRINKS")

# Save the averaged results.
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Nut_ave_allvar.txt", sep="\t", row.names=F, quote=F)
  write.table(x=meansbycategorydf, "VVKAJ_Tot_m_QCed_Cat_ave_allvar.txt", sep="\t", row.names=F, quote=F)

# The column names should be the same as start.col-end.col. 
  colnames(meansbycategorydf)

# The row names should be the users to calculate means for.
  rownames(meansbycategorydf)

# Pick up only the columns with non-zero variance, in order to run PCA and cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = meansbycategorydf)
  
  # "subsetted_non0var" is the dataframe to be used in the subsequent 
  # collapse by correlation procedure.
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                   min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
# Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]

# ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
# Check the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)
  dim( selected_variables)

  # original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)

# ---------------------------------------------------------------------------------------------------------------
# Save the selected_variables as a .txt file. This will be the input for clustering analyses.

# Make sure you give a correct name to the correct result, depending on whether nutrients or
# foods were used, and whether data were processed as is or averages were taken. 
  
  # 1. If using each datapoint as is WITHOUT AVERAGING by users -- 1 row per user per each day.
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_asis.txt", sep="\t", row.names=F, quote=F)
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Cat_asis.txt", sep="\t", row.names=F, quote=F)
  
  # 2. If taking average of each user across all days first,
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Nut_ave.txt", sep="\t", row.names=F, quote=F)
  write.table(x=selected_variables, file="VVKAJ_Tot_m_QCed_Cat_ave.txt", sep="\t", row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation by using 
# the CollapseByCorrelation function.
  
# Make sure you give a correct name to the correct result, depending on whether nutrients or
  # foods were used, and whether data were processed as is or averages were taken. 

  # 1. If using each datapoint as is WITHOUT AVERAGING by users -- 1 row per user per each day.
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_asis_corr_matrix.txt")
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Cat_asis_corr_matrix.txt")
  
  # 2. If taking average of each user across all days first,
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Nut_ave_corr_matrix.txt")
  SaveCorrMatrix(x=cc, out.fn = "VVKAJ_Tot_m_QCed_Cat_ave_corr_matrix.txt")

# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
  setwd(main_wd)
  
