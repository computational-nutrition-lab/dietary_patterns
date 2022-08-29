# ===============================================================================================================
# Prepare NHANES males in their 50s data for PCA and other cluster analysis.
# Version 1
# Created on 08/29/2022 by Rie Sadohara
# ===============================================================================================================

# READY TO BE COPIED TO TUTORIAL **********************************

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Come back to the main directory
  setwd(main_wd)   

# Load the necessary functions 
  source("lib/specify_dir_and_check_col.R")
  source("lib/prep_data_for_clustering.R")
  source("lib/PCA.R")
  source("lib/k-means.R")
  source("lib/ggplot2themes.R")

# Specify where the ata is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/")

# ===============================================================================================================
# Prep for PCA with nutrients
# ===============================================================================================================

# Load the glu_3_males50s data. 
  glu_3_males50s <- read.table("QCtotal_d_glu_body_meta_demo_males50s.txt", 
                             sep="\t", header=T)
# There should be 128 individuals (rows)
  dim(glu_3_males50s)

# make GLU_index as a factor for plotting.
  glu_3_males50s$GLU_index <- factor(glu_3_males50s$GLU_index, 
                                   levels = c('Normal', 'Prediabetic', 'Diabetic'))

# Are BMI and body weight correlated? - Yes.
  plot(glu_3_males50s$BMXBMI, glu_3_males50s$BMXWT)
  cor.test(glu_3_males50s$BMXBMI, glu_3_males50s$BMXWT)

# Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")

# Take only the columns whose names are NOT in the drop vector. 
  aaa <- glu_3_males50s[ , !(names(glu_3_males50s) %in% drops)]
  aaa(glu_3_males50s)

# ===============================================================================================================
# Scenario A: PCA with nutrients and body weight
# ===============================================================================================================
# Add BMI or (weight) to the PCA input.
# Nutrients
# Take  start.col="PROT" through end.col="P226" plus, "BMXBMI" and "BMXWT".
  BMI_col   <- match("BMXBMI" , names(aaa)) 
  WT_col    <- match("BMXWT" ,  names(aaa)) 
  start_col <- match("PROT"  , names(aaa))  
  end_col   <- match("P226"  , names(aaa)) 
  
# Pick up BMI, weight, and nutrient variables.
  subsetted <- aaa[ , c(BMI_col, WT_col, start_col:end_col)]
  head(subsetted, 1)
  
# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
# The output is a df called "subsetted_non0var".
  
# Check the columns (variables) remained.
  colnames(subsetted_non0var)  
  dim(subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variable if they are highly correlated.
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
  dim( selected_variables)     
  
# original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the variables after removing correlated variables
  write.table(selected_variables,
              "males50s_QCtotal_d_glu_body_meta_demo_Nut_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc,
                 out.fn= "males50s_QCtotal_d_glu_body_meta_demo_Nut_corr_mat.txt")
  
# ===============================================================================================================
# Scenario B: PCA with food category and body weight
# ===============================================================================================================
# Add BMI or (weight) to the PCA input.
# Food categories.
# The columns specified as start.col, end.col, and all columns in between will be selected.
# Take  start.col="F_CITMLB" through end.col="A_DRINKS" plus, "BMXBMI" and "BMXWT".
# The output is a df called "subsetted".
  BMI_col   <- match("BMXBMI" , names(aaa)) 
  WT_col    <- match("BMXWT" , names(aaa)) 
  start_col <- match("F_CITMLB"  , names(aaa))  
  end_col   <- match("A_DRINKS"  , names(aaa))   
  
# Pick up BMI, weight, and food category variables.
  subsetted <- aaa[ , c(BMI_col, WT_col, start_col:end_col)]
  head(subsetted, 1)
  
# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)
  # The output is a df called "subsetted_non0var".
  
# Check the columns (variables) remained.
  colnames(subsetted_non0var)  
  dim(subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variable if they are highly correlated.
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
  dim( selected_variables)     
  
# original
  head(subsetted_non0var, 1)
  dim( subsetted_non0var)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the variables after removing correlated variables
  write.table(selected_variables,
              "males50s_QCtotal_d_glu_body_meta_demo_Cat_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc,
                 out.fn = "males50s_QCtotal_d_glu_body_meta_demo_Cat_corr_mat.txt")
# ---------------------------------------------------------------------------------------------------------------
  
  
