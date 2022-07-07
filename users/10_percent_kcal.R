# For USERS ==============================================================================

# ========================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 12.16.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================
# 
# Folder structure 
# 
#                          |----- eg_data 
#                          |
#                          |----- lib
#                          |
#                          |----- users
#  Main -------------------|
#  (dietary_patterns)      |----- 
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")
  source("lib/percent_kcal.R")
  
# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# Load example totals data =============================================================== 
# Specify the directory where the data is.
  # SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Load the totals data.
  # totals <- read.table("Totals_to_use.txt",  sep = "\t", header = T)
  totals <- read.table("VVKAJ_Tot_m_QCed.txt",  sep = "\t", header = T)
 
# --------------------------------------------------------------------------------------------------------------
# Calculate the mean and SD of CARB, PROT, and TFAT.
 CPTgramsPerUser(inputfn= totals, user.name = "UserName", recall.no = "RecallNo",
                 outfn='VVKAJ_Tot_m_QCed_CPT_g.txt')
# Not used in the visualization below, but you may want to take a look at it.

 
# Calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
 CPTpctKcalPerUser(inputfn=totals, user.name='UserName', recall.no='RecallNo', 
                   outfn="VVKAJ_Tot_m_QCed_CPT_kcal.txt")
 
# Load the %kcal values 
 CPT_kcal <- read.table("VVKAJ_Tot_m_QCed_CPT_kcal.txt", sep="\t", header=T)
 CPT_kcal

# --------------------------------------------------------------------------------------------------------------
# Define ggplot1 themes
  library(ggplot2)

# Theme black and white, with the base font size 14: change if necessary.
  theme_set(theme_bw(base_size = 14))
  
# No gridlines inside charts
  no_grid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
# Insert some space between axes and axes labels. 
  space_axes <- theme(axis.title.x = element_text(margin=margin(t = 8, r = 0, b = 0, l = 0) ),
                      axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) 

# Rotate the X axis labels 45 degrees for visibility.   
  rotate_X_labels <- theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1) )
 
# --------------------------------------------------------------------------------------------------------------
# Plot a barchart without SD. 
  ggplot(CPT_kcal, aes(x = UserName, y = mean, fill = macronutrient)) + 
    geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
    # change colors and labels of legend. Ensure the factor order is correct. 
    scale_fill_manual(values = distinct100colors, 
                      labels=c( "Carbohydrates", "Protein", "Total fat")) +
    labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
    # Specify the y axis breaks.
    no_grid + space_axes +
    # Specify the font size and angle of xlabel.  
    theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  
# --------------------------------------------------------------------------------------------------------------
 # Plot "dodge"-type of barchart (3 bars per user, NOT STACKED).
  ggplot(CPT_kcal, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
    geom_bar(stat = "identity", position = "dodge", color="black")  +
    geom_errorbar(aes(ymin= mean, ymax= mean+sd), position = position_dodge(0.9), width = 0.25,
                  color="black") +
    scale_fill_manual(values = distinct100colors,
                      labels=c( "Carbohydrates", "Protein", "Total fat")) +
    labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
    no_grid + space_axes +
    theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  
 
# --------------------------------------------------------------------------------------------------------------
# Using CPT_kcal, create a stacked barchart.  
  
# Create a vector that contains all the users (individuals). 
  individuals <- unique(CPT_kcal$UserName)

# Generate a dataframe to save sd data.
  CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(individuals)*3, ncol=7)) 

# Specify its column names.
  colnames(CPT_kcal_forstacked) <- c("UserName", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
  
# Calculate sd_base and sd_forstacked for stacked barchart. 
# Note that this function assumes all users (individuals) have CARB, PROT, and TFAT values.
  CalcStackedSD(input.df = CPT_kcal, out.fn = "CPT_kcal_forstacked.txt")
  
# Load the saved file that has SD for stacked barchart.
  CPT_kcal_forstacked_read <- read.table("CPT_kcal_forstacked.txt", sep="\t", header=T)
  CPT_kcal_forstacked_read_1 <- CPT_kcal_forstacked_read[1:15, ] 
  CPT_kcal_forstacked_read_2 <- CPT_kcal_forstacked_read[16:30, ]
  CPT_kcal_forstacked_read_3 <- rbind(CPT_kcal_forstacked_read_2, CPT_kcal_forstacked_read_1) 
  
# Stacked barchart with SD as error bars.
  ggplot(CPT_kcal_forstacked_read, aes(x = UserName, y = mean, fill=macronutrient, colour=macronutrient)) + 
    geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7)  +
    geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.15, color="grey10") + 
    scale_fill_manual(values = distinct100colors,
    labels=c( "Carbohydrates", "Protein", "Total fat")) +
    labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
    no_grid + space_axes +
    theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  
# Change the Y axis scale if necessary. Note that if error bars of Carbohydrates disappear after 
# changing the limits of Y axis, that may be because the error bars are higher than the max Y.
# Ensure you have enough max value for the Y axis.  

# Come back to the main directory
  setwd(main_wd)
  