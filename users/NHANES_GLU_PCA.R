# ===============================================================================================================
# Perform PCA with NHANES data of males in their 50s who are not following any particular diet. 
# Version 1
# Created on 08/06/2022 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/dietary_patterns/eg_data/NHANES/Laboratory_data/")

# ---------------------------------------------------------------------------------------------------------------
# Load the glu_3_males50s data. 
  glu_3_males50s <- read.table("QCtotalANDglu_body_meta_demo_males50s.txt", 
                               sep="\t", header=T)
  dim(glu_3_males50s)

# ===============================================================================================================
# Prep for PCA
# ===============================================================================================================
  # Perform PCA and plot the individuals. color code by GLU_index.
  head(glu_3_males50s)
  # Load the necessary functions
  source("../../../lib/prep_data_for_clustering.R")
  source("../../../lib/PCA.R")
  source("../../../lib/k-means.R")
  
  # Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")
  
  # Take only the columns whose names are NOT in the drop vector. 
  aaa <- glu_3_males50s[ , !(names(glu_3_males50s) %in% drops)]
  dim(glu_3_males50s)
  dim(aaa)
  
  # Save it as glu_3_males50s.  
  glu_3_males50s <- aaa
  
  # Define the input data to be used.
  input_data <- glu_3_males50s
  
# ---------------------------------------------------------------------------------------------------------------  
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
              "QCtotalANDglu_body_meta_demo_males50s_Nut_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
  # ---------------------------------------------------------------------------------------------------------------
  # Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, 
                 out.fn = "QCtotalANDglu_body_meta_demo_males50s_Nut_corr_mat.txt")
  # ---------------------------------------------------------------------------------------------------------------
  
# ===============================================================================================================
# PCA with nutrients
# ===============================================================================================================

# Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- read.table("QCtotalANDglu_body_meta_demo_males50s_Nut_rv.txt", 
                          sep="\t", header=T)
  
  # Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  # Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x=pca_input, scale = TRUE)   
  
  #### Save PCA plots and txt all at once. ####
  
  # Specify the directory (folder) to save the results.
  res_dir_nut = "PCA_Nut_males50s" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix_nut = "males50s_Nut"
  
# Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, out.dir= res_dir_nut, out.prefix= res_prefix_nut)
  
# Combine the input (totals before processing) with all the variables and the PC results. 
  # Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  SaveInputAndPCs(input="QCtotalANDglu_body_meta_demo_males50s.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_nut, out.prefix= res_prefix_nut)
  # Note that even though the input file has both nutrients (Nut) and food categories (Cat) data,  
  # PCA was done with only either Nut or Cat, not both.
  
# ---------------------------------------------------------------------------------------------------------------
# Do the same with Food categories.
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# Create a biplot with Normal, Prediabetes, and Diabetes people color-coded.   
  
  
  
  
  
