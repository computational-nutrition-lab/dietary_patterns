# ===============================================================================================================
# Perform PCA with NHANES data of males in their 50s who are not following any particular diet. 
# Version 1
# Created on 08/06/2022 by Rie Sadohara
# ===============================================================================================================

  setwd("~/GitHub/dietary_patterns/eg_data/NHANES/Laboratory_data/")

# Load the necessary functions 
  source("../../../lib/prep_data_for_clustering.R")
  source("../../../lib/PCA.R")
  source("../../../lib/k-means.R")

# ===============================================================================================================
# Prep for PCA with nutrients
# ===============================================================================================================
  
# ---------------------------------------------------------------------------------------------------------------
# Load the glu_3_males50s data. 
  glu_3_males50s <- read.table("QCtotalANDglu_body_meta_demo_males50s.txt", 
                               sep="\t", header=T)
  # There should be 124 individuals (rows)
  dim(glu_3_males50s)
  
  # make GLU_index as a factor for plotting.
  glu_3_males50s$GLU_index <- factor(glu_3_males50s$GLU_index, 
                                     levels = c('Normal', 'Prediabetic', 'Diabetic'))

  # are BMI and weight correlated? - Yes.
  plot(glu_3_males50s$BMXBMI, glu_3_males50s$BMXWT)
  cor.test(glu_3_males50s$BMXBMI, glu_3_males50s$BMXWT)
  
# Define which columns to drop.
  drops <- c("KCAL","GRMS", "MOIS", "NoOfItems")
  
# Take only the columns whose names are NOT in the drop vector. 
  aaa <- glu_3_males50s[ , !(names(glu_3_males50s) %in% drops)]
  dim(glu_3_males50s)
  dim(aaa)
  colnames(aaa)

# ---------------------------------------------------------------------------------------------------------------  
# Add BMI or (weight) to the PCA input.
# Nutrients
    # The columns specified as start.col, end.col, and all columns in between will be selected.
    # SubsetColumns(data=input_data, start.col="PROT", end.col="P226")
    # The output is a df called "subsetted".
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
              "males50s_QCtotalANDglu_body_meta_demo_Nut_rv.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, 
                 out.fn = "males50s_QCtotalANDglu_body_meta_demo_Nut_corr_mat.txt")
# ---------------------------------------------------------------------------------------------------------------
  
# ===============================================================================================================
# PCA with nutrients and body weight
# ===============================================================================================================

# Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- read.table("males50s_QCtotalANDglu_body_meta_demo_Nut_rv.txt", 
                          sep="\t", header=T)
  
# Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
# Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x= pca_input, scale= TRUE)   
  
  #### Save PCA plots and .txt all at once. ####
  
  # Specify the directory (folder) to save the results.
  res_dir_Nut = "males50s_Nut_PCA" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix_Nut = "males50s_Nut"
  
# Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, 
             out.dir= res_dir_Nut, out.prefix= res_prefix_Nut)
  
# Combine the input (totals before processing) with all the variables and the PC results. 
  # Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  SaveInputAndPCs(input="QCtotalANDglu_body_meta_demo_males50s.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_Nut, out.prefix= res_prefix_Nut)
  # Note that even though the input file has both nutrients (Nut) and food categories (Cat) data,  
  # PCA was done with only either Nut or Cat, not both.
  
# ---------------------------------------------------------------------------------------------------------------
# Create a biplot with Normal, Prediabetes, and Diabetes people color-coded.   

# Load the input & PC info.
  Nut_PCs <- read.table("males50s_Nut_PCA/males50s_Nut_PCs.txt", sep="\t", header=T)

# Change to a factor so that factors will be displayed in order.
  Nut_PCs$GLU_index <- factor(Nut_PCs$GLU_index, levels= c("Normal", "Prediabetic", "Diabetic"))
  
  head(Nut_PCs, 1)
  dim(Nut_PCs)
  
# Ellipses.
  ell <- ggplot(data= Nut_PCs, aes(x=PC2, y=PC3, color= GLU_index)) +
    geom_point(aes(color=GLU_index), size=3 ) + 
    theme_bw(base_size = 12) + no_grid + theme(aspect.ratio = 1) +
    scale_color_manual( values= c("steelblue3", "gold3", "hotpink")) +
    stat_ellipse(level=0.95)
  ell
  ggsave("males50s_Nut_PCA/males50s_Nut_PCA_by_GLU_index_PC23_ell.png", ell, 
         device="png", width=7, height=6.5)
  # No visual separation between the groups...
  
# Use the autoplot function.
  foodnut_PCA <- autoplot(scaled_pca, x=2, y=3,  # Specify which PC 
                          loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
                          data = input_data,  size= 3 ) +
    # coord_cartesian(xlim =c(-0.2, 0.2), ylim = c(-0.2, 0.25)) +
    geom_point(size = 3, alpha = 1, na.rm = T, shape = 21,  aes(fill= GLU_index)) +
    theme_bw(base_size = 12) + theme(aspect.ratio = 1) +  
    # xlim(-0.3, 0.2) + ylim(-0.3, 0.2) +
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual( values= c("hotpink",  "steelblue3", "yellow")) 
  foodnut_PCA
  
  ggsave("males50s_Nut_PCA/males50s_Nut_PCA_by_GLU_index_PC23.png", foodnut_PCA, 
         device="png", width=7, height=6.5)
  
# ---------------------------------------------------------------------------------------------------------------
# Check beta-diversity of PCA plot (each GLU_index group has the same dispersion?)  Need to look it up.
  
#### RESUME FROM HERE #####  
  
  
  #
  
  
  
  
  
# ===============================================================================================================
# Prep Food categories data for PCA
# ===============================================================================================================
  
# ---------------------------------------------------------------------------------------------------------------  
# The columns specified as start.col, end.col, and all columns in between will be selected.
# food categories
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
  dim(selected_variables)     
  
  # original
  head(subsetted_non0var, 1)
  dim(subsetted_non0var)
  
  # ---------------------------------------------------------------------------------------------------------------
  # Save the variables after removing correlated variables
  write.table(selected_variables, 
              "QCtotalANDglu_body_meta_demo_males50s_Cat_rv.txt", 
              sep="\t", row.names= F, quote= F)
  
  # ---------------------------------------------------------------------------------------------------------------
  # Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, 
                 out.fn = "QCtotalANDglu_body_meta_demo_males50s_Cat_corr_mat.txt")
  # ---------------------------------------------------------------------------------------------------------------
  
# ===============================================================================================================
# PCA with Food categories.
# ===============================================================================================================
  
  # Your input data should be a data frame with variables with non-zero variance. 
  pca_input <- read.table("QCtotalANDglu_body_meta_demo_males50s_Cat_rv.txt", 
                          sep="\t", header=T)
  
  # Ensure your input file has the correct number of rows and columns.
  dim(pca_input)
  
  # Perform PCA with the subset data, scaled.
  scaled_pca <- prcomp(x= pca_input, scale= TRUE)   
  
  #### Save PCA plots and .txt all at once. ####
  
  # Specify the directory (folder) to save the results.
  res_dir_Cat = "PCA_Cat_males50s" 
  
  # Specify the prefix of filenames to be saved. 
  res_prefix_Cat = "males50s_Cat"
  
  # Perform PCA and save the results in a specified folder (out.dir) and a prefix (out.prefix).
  PerformPCA(pca.data=pca_input, pca.result=scaled_pca, 
             out.dir= res_dir_Cat, out.prefix= res_prefix_Cat)
  
  # Combine the input (totals before processing) with all the variables and the PC results. 
  # Input is your items/totals input file before any prep for clustering, from which you derived the input for the PCA.
  SaveInputAndPCs(input="QCtotalANDglu_body_meta_demo_males50s.txt", pca.results = scaled_pca, 
                  out.dir= res_dir_Cat, out.prefix= res_prefix_Cat)
  # Note that even though the input file has both nutrients (Nut) and food categories (Cat) data,  
  # PCA was done with only either Nut or Cat, not both.
  
# ---------------------------------------------------------------------------------------------------------------
# Create a biplot with Normal, Prediabetes, and Diabetes people color-coded.   
  
  # Load the input & PC info.
  Cat_PCs <- read.table("PCA_Cat_males50s/males50s_Cat_PCs.txt", sep="\t", header=T)
  
  head(Cat_PCs)
  dim(Cat_PCs)
  
  ggplot(data=Cat_PCs, aes(x=PC1, y=PC2, color= GLU_index)) +
    geom_point(aes(fill=GLU_index), size=3) +
    theme_bw(base_size = 14)
  
# Use the autoplot function.
  foodcat_PCA <- autoplot(scaled_pca, loadings=T, loadings.label=T, loadings.colour = 'grey50',  # loadings.label=T if want to see it
           data = input_data,  size= 3 ) +
    # coord_cartesian(xlim =c(-0.2, 0.2), ylim = c(-0.2, 0.25)) +
    geom_point(size= 3, alpha= 1, na.rm= T, shape= 21,  aes(fill=GLU_index)) +
    theme_bw(base_size= 12) + theme(aspect.ratio = 1) +  
    # xlim(-0.3, 0.2) + ylim(-0.3, 0.2) +
    theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual( values= c("hotpink",  "steelblue3",  "yellow") ,
                       labels= c("Diabetic", "Normal",      "Prediabetic") )
  foodcat_PCA
  
  ggsave("PCA_Cat_males50s/males50s_Cat_PCA_by_GLU_index.png", foodcat_PCA, 
         device="png", width=7, height=6.5)

  ?autoplot
  
