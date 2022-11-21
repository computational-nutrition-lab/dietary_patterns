# ===============================================================================================================
# Look at the proportion of Normal, Prediabetic, and Diabetic in each age group. 
# Version 1
# Created on 11/21/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.
  setwd("~/GitHub/dietarry_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())  

# Load necessary packages.
  library(SASxport)

# Load necessary functions.
  source("lib/specify_data_dir.R")
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")
  source("lib/ggplot2themes.R") 

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

  
# ===============================================================================================================
# Load the mean totals and add a new variable called "agegroup".
# ===============================================================================================================
  
# Load the data of those to be used in the diabetes status analysis. 
  glu_2 <- read.delim( file="Laboratory_data/QCtotal_d_glu_body_meta_2.txt", sep= "\t", header= T )
  
# Make GLU_index as a factor for plotting.
  glu_2$GLU_index <- factor(glu$GLU_index, levels = c("Normal", "Prediabetic", "Diabetic"))
  
# Add age group column.
  glu_2$agegroup <- NA
  
# Add age group according to the age in RIDAGEYR.
  for(i in 1: nrow(glu_2)){
    if(     glu_2$RIDAGEYR[i] < 30){ glu_2$agegroup[i] <- "29_and_below" }
    else if(glu_2$RIDAGEYR[i] < 40){ glu_2$agegroup[i] <- "30s" }
    else if(glu_2$RIDAGEYR[i] < 50){ glu_2$agegroup[i] <- "40s" }
    else if(glu_2$RIDAGEYR[i] < 60){ glu_2$agegroup[i] <- "50s" }
    else{                            glu_2$agegroup[i] <- "60s_and_over" }
  }
  
# Check the result.
  table(glu_2$agegroup)


# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender - FEMALE
# ===============================================================================================================

# Select females 
  glu_2_females <- subset(glu_2, RIAGENDR == 2) 
  
# Check the dimension of the selected data - 851 rows.
  nrow(glu_2_females)
  
# Create a table with the count of each agegroup and GLU_index combination.
# The variable to be counted can be any one with no missing data; therefore, "SEQN" is selected here.
  longtable <- aggregate(SEQN ~ agegroup + GLU_index,                                    
                         data = glu_2_females,   # Change the input dataset here.
                         FUN = length)
  longtable 

# Make a short table with GLU_index levels in each column in order to compute their proportions. 
  shorttable <- reshape2::dcast(longtable, agegroup ~ GLU_index, sum)
  shorttable
  
# Calculate the sum of all the levels of GLU_index.
  shorttable$sum <- rowSums(shorttable[, 2:4])
  
# Calculate the proportion of each - to be used as percentages.
  shorttable$Normal_pr      <- shorttable$Normal/shorttable$sum         
  shorttable$Prediabetic_pr <- shorttable$Prediabetic/shorttable$sum
  shorttable$Diabetic_pr    <- shorttable$Diabetic/shorttable$sum
  
# Calculate the sum of the proportions - should be all 1.
  shorttable$sum_pr <- rowSums(shorttable[, 6:8])
  shorttable
  
# Take the proportions and make it into a long table again for plotting.
  longtable_pr <- reshape2::melt(shorttable[, c(1, 6:8)])
  longtable_pr
  
# Generate a stacked barchart for females: GLU_index_pr_F. 
  GLU_index_pr_F <- ggplot(longtable_pr, aes(x= agegroup, y= value, fill= variable)) +
    geom_bar(position = "fill", stat = "identity",color='black',width=0.9) + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("steelblue2", "lightgoldenrod1", "lightpink1") ) +
    geom_text(aes(label = paste0( round(value*100, 0),"%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    rotate_X_labels + space_axes + 
    labs(x="Age Group", y="")
  GLU_index_pr_F

# Save the plot.
  ggsave("Laboratory_data/QCtotal_d_glu_body_meta_2_agegroup_GLU_index_female.png", 
         GLU_index_pr_F, device="png", width=6, height=4.5, unit="in", dpi=300)

# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender - MALE
# ===============================================================================================================
# Repeat the same operation and tables for females will be created. 
  
# Select males 
  glu_2_males <- subset(glu_2, RIAGENDR == 1) 
  
# Check the number of rows of the selected data - 774 rows.
  nrow(glu_2_males)
  
# Create a table with the count of each agegroup and GLU_index combination.
# The variable to be counted can be any one with no missing data; therefore, "SEQN" is selected here.
  longtable <- aggregate(SEQN ~ agegroup + GLU_index,                                    
                         data = glu_2_males,
                         FUN = length)
  longtable 
  
# Make a short table with GLU_index levels in each column in order to compute their proportions. 
  shorttable <- reshape2::dcast(longtable, agegroup ~ GLU_index, sum)
  shorttable
  
# Calculate the sum of all the levels of GLU_index.
  shorttable$sum <- rowSums(shorttable[, 2:4])
  
# Calculate the proportion of each - to be used as percentages.
  shorttable$Normal_pr      <- shorttable$Normal/shorttable$sum         
  shorttable$Prediabetic_pr <- shorttable$Prediabetic/shorttable$sum
  shorttable$Diabetic_pr    <- shorttable$Diabetic/shorttable$sum
  
# Calculate the sum of the proportions - should be all 1.
  shorttable$sum_pr <- rowSums(shorttable[, 6:8])
  shorttable
  
# Take the proportions and make it into a long table again for plotting.
  longtable_pr <- reshape2::melt(shorttable[, c(1, 6:8)])
  longtable_pr
  
# Plot it with percentage labels.
  GLU_index_pr_M <- ggplot(longtable_pr, aes(x= agegroup, y= value, fill= variable)) +
    geom_bar(position = "fill", stat = "identity",color='black',width=0.9) + 
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("steelblue2", "lightgoldenrod1", "lightpink1") ) +
    geom_text(aes(label = paste0( round(value*100, 0),"%")), 
              position = position_stack(vjust = 0.5), size = 5) +
    rotate_X_labels + space_axes + 
    labs(x="Age Group", y="")
  GLU_index_pr_M
  
# Save the plot.
  ggsave("Laboratory_data/QCtotal_d_glu_body_meta_2_agegroup_GLU_index_male.png", 
         GLU_index_pr_M, device="png", width=6, height=4.5, unit="in", dpi=300)
  
  
# By looking at the distribution, males in 60s and over has the highest percentages of Diabetic individuals.
# We will use this gender-age group to analyze their diets further. 
    

  