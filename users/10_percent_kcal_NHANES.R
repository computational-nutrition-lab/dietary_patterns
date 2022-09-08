# ==============================================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 09/07/2022 by Rie Sadohara
# ==============================================================================================================

# ==============================================================================================================
# Import data from your data directory 
# ==============================================================================================================
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

# Set working directory to "dietary_patterns".
  Session --> Set working direHctory --> Choose directory.  
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/ggplot2themes.R")
  source("lib/percent_kcal.R")
  
# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# --------------------------------------------------------------------------------------------------------------
# Load example totals data
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/")
  
# Load the totals with demographic data.
  totals <- read.table("Total_D12_FC_QC_mean_QC_d.txt",  sep = "\t", header = T)
 
# Totals has the mean dietary intake of two days for each participant and also their metadata. 
# We are going to use the following columns in totals:
  # RIAGENDR = gender
  # RIDAGEYR = age
  # CARB, PROT, TFAT, KCAL columns.
  
# --------------------------------------------------------------------------------------------------------------
# Add Gender and Age, and Gender_Age for grouping. 

# Check the distribution of gender. 1: male, 2: female.  
  table(totals$RIAGENDR)
  
# Add a new column of Gender.
  totals$Gender = NA
  
# Add gender index to totals. 
  for(i in 1:nrow(totals)){
    if(     totals$RIAGENDR[i]==1){totals$Gender[i] <- "M"}
    else if(totals$RIAGENDR[i]==2){totals$Gender[i] <- "F"}
  }
  
# Ensure the gender distribution is correct.
  table(totals$Gender)
  
# Look at the summary of age. Min is 18 and max is 80.
  summary(totals$RIDAGEYR)
  
# Add a new column of AgeGroup.
  totals$AgeGroup = NA
  
# Add age group index to totals. 
  for(i in 1:nrow(totals)){
    if(     totals$RIDAGEYR[i] < 20){totals$AgeGroup[i] <- "18_19"}
    else if(totals$RIDAGEYR[i] < 30){totals$AgeGroup[i] <- "20s"}
    else if(totals$RIDAGEYR[i] < 40){totals$AgeGroup[i] <- "30s"}
    else if(totals$RIDAGEYR[i] < 50){totals$AgeGroup[i] <- "40s"}
    else if(totals$RIDAGEYR[i] < 60){totals$AgeGroup[i] <- "50s"}
    else if(totals$RIDAGEYR[i] < 70){totals$AgeGroup[i] <- "60s"}
    else                                     {totals$AgeGroup[i] <- "70s_80s"}
  }
  
# Combine Age_Group and Gender as a new factor. e.g. "M_40s".
  totals$Gender_Age <- paste(totals$Gender, totals$AgeGroup, sep="_")

# Function to add gender and age groups to NHANES totals data.
  AddGenderAgeGroups <- function(input=totals, age.col="RIDAGEYR", gender.col="RIAGENDR"){
    
    # Rename input. 
    totals2 <- input
    
    # column number of gender 
    gender.col.number <- which(colnames(totals2)==gender.col)
    
    # Add a new column of Gender.
    totals2$Gender = NA
    
    # Add gender index to totals.
    for(i in 1:nrow(totals2)){
      if(     totals2[i, gender.col.number]==1){totals2$Gender[i] <- "M"}
      else if(totals2[i, gender.col.number]==2){totals2$Gender[i] <- "F"}
    }  
    
    # column number of age 
    age.col.number <- which(colnames(totals2)==age.col)
    
    # Add age group. 
    
    #### RESUME FROM HERE. ####
    
    
    
    print(head(totals2))
      
    }
    
  AddGenderAgeGroups(input=totals, age.col="RIDAGEYR", gender.col="RIAGENDR")
  colnames(totals)  
  
  
  
# Ensure grouping has been done correctly. 
  head(totals[, c("RIAGENDR", "Gender", "RIDAGEYR", "AgeGroup", "Gender_Age")])
  
# Check the distribution. 
  table(totals$Gender_Age)
  
# --------------------------------------------------------------------------------------------------------------
# For NHANES, we will calculate the percentage of calories from each of the three macronutrients in the sum of 
# calories from the three macronutrients.
# Thus, the percentage of calories from CARB, PROT, and TFAT will add up to 100.   

# Calculate the %kcal of CARB, PROT, and TFAT for each user and take means by Gender_Age.   
  CPTpctKcalPerUser_NHANES(inputfn=totals, group='Gender_Age', across='SEQN', outfn="Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt")
  
# Load the output.
  CPT_kcal <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt", sep="\t", header=T)

# CPT_kcal has Group, macronutrient, n, mean, and sd of each group.
  head(CPT_kcal)
  
# Plot a barchart without SD.
  # Change the font size if necessary.
  # This assumes that CPT_kcal has "Group" column.
  stacked_wo_SD <- StackedwoSD_NHANES(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  stacked_wo_SD

  # Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_wo_SD.pdf", stacked_wo_SD,
         device="pdf", width=6.2, height=4.2, units="in", dpi=300)
  
# --------------------------------------------------------------------------------------------------------------
# Plot the "dodge"-type of barchart (3 bars per group, NOT STACKED).
  # Change the font size if necessary.
  dodgedtypebarchart <- DodgedBarchart_NHANES(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  dodgedtypebarchart
  
  # Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_dodgedtypebarchart.pdf", dodgedtypebarchart,
         device="pdf", width=9.0, height=4, units="in", dpi=300)
  
# --------------------------------------------------------------------------------------------------------------
# Using CPT_kcal, create a stacked barchart.
  
  # Create a vector that contains all the groups. 
  groups <- unique(CPT_kcal$Group)
  
  # Calculate sd_base and sd_forstacked for stacked barchart. 
  # Note that this function assumes all users (individuals) have CARB, PROT, and TFAT values.
  CalcStackedSD_NHANES(input.df= CPT_kcal, out.fn= "Total_D12_FC_QC_mean_QC_d_CPT_kcal_forstacked.txt")
  
  # Load the saved file that has SD for stacked barchart.
  CPT_kcal_forstacked_read <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal_forstacked.txt", sep="\t", header=T)
  
  # Stacked barchart with SD as error bars.
  stacked_with_SD <- StackedWithSD_NHANES(data= CPT_kcal_forstacked_read) + theme(axis.text.x=element_text(size=11))
  stacked_with_SD
  
  # Save as a .pdf.
  ggsave("Total_D12_FC_QC_mean_QC_d_CPT_kcal_with_SD.pdf", stacked_with_SD,
         device="pdf", width=6.2, height=4.3, units="in", dpi=300)
  
# Change the Y axis scale if necessary. Note that if the error bars of Carbohydrates disappear 
# after changing the limits of Y axis, it may be because the error bars are higher than the max Y.
# Ensure you have enough max Y value to accommodate the error bars.
  
  # You can also change the breakpoints of the Y axis.
  stacked_with_SD + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100))
  
# --------------------------------------------------------------------------------------------------------------
  # Come back to the main directory
  setwd(main_wd)
  
  
  
  
  
    
  

 
#  OLD BELOW. TO BE DELETED.  
# --------------------------------------------------------------------------------------------------------------
#   
# # Define function to calculate %kcal of the three macronutrients.   
#   CPTpctKcalPerSEQN <- function(inputfn, user.name="SEQN", outfn){
#     
#   # Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
#     indexno_username <- which(names(inputfn)== user.name)
#     indexno_carb <-     which(names(inputfn)== "CARB")
#     indexno_prot <-     which(names(inputfn)== "PROT")
#     indexno_tfat <-     which(names(inputfn)== "TFAT")
#   
#   # Take only the relevant columns from inputfn.
#   totalssub2 <- inputfn[, c(indexno_username, 
#                             indexno_carb,
#                             indexno_prot,
#                             indexno_tfat
#                             )]
#   # % KCAL
#   # Calculate calories
#   totalssub2$CARB_kcal <- totalssub2$CARB * 4
#   totalssub2$PROT_kcal <- totalssub2$PROT * 4
#   totalssub2$TFAT_kcal <- totalssub2$TFAT * 9
#   totalssub2$kcal_sum <-  totalssub2$CARB_kcal + totalssub2$PROT_kcal + totalssub2$TFAT_kcal
#   
#   # calculate kcal of each macronutrient per engergy (%)
#   totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$kcal_sum * 100
#   totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$kcal_sum * 100
#   totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$kcal_sum * 100
#   
#   
#   write.table(x=totalssub2, file=outfn, sep="\t", quote=F, row.names=F)
#   } 
# 
# # Or just add columns as necessary... 
#   CPTpctKcalPerSEQN <- function(inputfn, outfn){
#     
#     # Define
#     input_total <- inputfn 
#     
#     # Calculate calories
#     input_total$CARB_kcal <- input_total$CARB * 4
#     input_total$PROT_kcal <- input_total$PROT * 4
#     input_total$TFAT_kcal <- input_total$TFAT * 9
#     input_total$kcal_sum <-  input_total$CARB_kcal + input_total$PROT_kcal + input_total$TFAT_kcal
#     
#     # calculate %kcal of each macronutrient per engergy (%)
#     input_total$CARB_kcal_pct <- input_total$CARB_kcal / input_total$kcal_sum * 100
#     input_total$PROT_kcal_pct <- input_total$PROT_kcal / input_total$kcal_sum * 100
#     input_total$TFAT_kcal_pct <- input_total$TFAT_kcal / input_total$kcal_sum * 100
#     
#     write.table(x=input_total, file=outfn, sep="\t", quote=F, row.names=F)
#   }
# 
# # Calculate %kcal of each macronutrient.
#   CPTpctKcalPerSEQN(inputfn= totals, outfn="Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt")
# 
# # Load in the output.
#   totals_CPT_kcal <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt", sep="\t", header=T) 
#   
# # Sum %kcal of the three macronutrients. They should add up to 100. 
#   totals_CPT_kcal$totalKCAL <- totals_CPT_kcal$CARB_kcal_pct +
#                                totals_CPT_kcal$PROT_kcal_pct + 
#                                totals_CPT_kcal$TFAT_kcal_pct
#   
#   summary(totals_CPT_kcal$totalKCAL)
#   
#   head(totals_CPT_kcal[, c("CARB_kcal_pct","PROT_kcal_pct","TFAT_kcal_pct") ])
#   
# #########
#   
# # Add gender group -----------------------------------------------------------------
#   table(totals_CPT_kcal$RIAGENDR)
#   
#   # Add NA to save Gender spelt out.
#   totals_CPT_kcal$Gender = NA
#   
#   for(i in 1:nrow(totals_CPT_kcal)){
#     if(     totals_CPT_kcal$RIAGENDR[i]==1){totals_CPT_kcal$Gender[i] <- "M"}
#     else if(totals_CPT_kcal$RIAGENDR[i]==2){totals_CPT_kcal$Gender[i] <- "F"}
#   }
# 
#   table(totals_CPT_kcal$Gender)
# 
#   
#     
# # Add Age group ---------------------------------------------------------------------------- 
# # Look at the summary of age. Min is 18 and max is 80.
#   summary(totals_CPT_kcal$RIDAGEYR)
#   
#   # Add a new column of AgeGroup to save age group info.
#   totals_CPT_kcal$AgeGroup = NA
#   
#   for(i in 1:nrow(totals_CPT_kcal)){
#     if(     totals_CPT_kcal$RIDAGEYR[i] < 20){totals_CPT_kcal$AgeGroup[i] <- "18_19"}
#     else if(totals_CPT_kcal$RIDAGEYR[i] < 30){totals_CPT_kcal$AgeGroup[i] <- "20s"}
#     else if(totals_CPT_kcal$RIDAGEYR[i] < 40){totals_CPT_kcal$AgeGroup[i] <- "30s"}
#     else if(totals_CPT_kcal$RIDAGEYR[i] < 50){totals_CPT_kcal$AgeGroup[i] <- "40s"}
#     else if(totals_CPT_kcal$RIDAGEYR[i] < 60){totals_CPT_kcal$AgeGroup[i] <- "50s"}
#     else if(totals_CPT_kcal$RIDAGEYR[i] < 70){totals_CPT_kcal$AgeGroup[i] <- "60s"}
#     else                                     {totals_CPT_kcal$AgeGroup[i] <- "70s_80s"}
#   }
# 
#   # Ensure grouping has been done correctly. 
#   head(totals_CPT_kcal[, c("RIAGENDR", "Gender", "RIDAGEYR", "AgeGroup")])
#   
#   # Check the distribution
#   table(totals_CPT_kcal$AgeGroup)
# 
# # Add Age_Group and Gender to create a new factor.
#   totals_CPT_kcal$Gender_Age <- paste(totals_CPT_kcal$Gender, totals_CPT_kcal$AgeGroup, sep="_")
#   table(totals_CPT_kcal$Gender_Age)
#   
# # Calc the mean by gender
#   totals_CPT_kcal$RIAGENDR
#   
#   # aaa <- subset(totals_CPT_kcal, Gender_Age=="M_50s")
#   # bbb <- subset(totals_CPT_kcal, Gender_Age=="F_50s")
#   # write.table(bbb, "clipboard-16384", sep="\t", quote = F, row.names = F)
# 
#   CARBmean <- aggregate(totals_CPT_kcal$CARB_kcal_pct, by=list(totals_CPT_kcal$Gender_Age), FUN=mean)
#   colnames(CARBmean) <- c("Group", "CARB_mean")
#   PROTmean <- aggregate(totals_CPT_kcal$PROT_kcal_pct, by=list(totals_CPT_kcal$Gender_Age), FUN=mean)
#   colnames(PROTmean) <- c("Group", "PROT_mean")
#   TFATmean <- aggregate(totals_CPT_kcal$TFAT_kcal_pct, by=list(totals_CPT_kcal$Gender_Age), FUN=mean)
#   colnames(TFATmean) <- c("Group", "TFAT_mean")
#   CP <-  merge(x=CARBmean, y=PROTmean, by="Group", all.x=T)
#   CPT <- merge(x=CP,       y=TFATmean, by="Group", all.x=T)
#   # SD
#   CARBsd <- aggregate(totals_CPT_kcal$CARB_kcal_pct, by=list(totals_CPT_kcal$Gender_Age), FUN=sd)
#   colnames(CARBsd) <- c("Group", "CARB_sd")
#   PROTsd <- aggregate(totals_CPT_kcal$PROT_kcal_pct, by=list(totals_CPT_kcal$Gender_Age), FUN=sd)
#   colnames(PROTsd) <- c("Group", "PROT_sd")
#   TFATsd <- aggregate(totals_CPT_kcal$TFAT_kcal_pct, by=list(totals_CPT_kcal$Gender_Age), FUN=sd)
#   colnames(TFATsd) <- c("Group", "TFAT_sd")
#   CPT_sd1 <- merge(x=CPT,     y=CARBsd, by="Group", all.x=T)
#   CPT_sd2 <- merge(x=CPT_sd1, y=PROTsd, by="Group", all.x=T)
#   CPT_sd3 <- merge(x=CPT_sd2, y=TFATsd, by="Group", all.x=T)
#   
#   
#   library(reshape2)
#   CPT_l <- melt(CPT)
#   head(CPT_l)
#   
#   CPT$Total <- CPT$CARB + CPT$PROT + CPT$TFAT
#   hist(totals_CPT_kcal$RIDAGEYR)
#   
# # Plot a stacked barchart.
#   data= CPT_l
#   ggplot(data, aes(x = Group, y = value, fill = variable)) + 
#     geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
#     # change colors and labels of legend. Ensure the factor order is correct. 
#     # scale_fill_manual(values = distinct100colors, 
#                       # labels=c( "Carbohydrates", "Protein", "Total fat")) +
#     labs(x= element_blank(), y= "Percentages of total kcal intake", fill = "Macronutrients") +
#     # Specify the font size and angle of the x axis label.  
#     theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) + no_grid
# 
#   
#     
#   
# # Safety check
#   library(dplyr)
#   totals_CPT_kcal %>% group_by(RIAGENDR) %>% summarise(mymean = mean(CARB))
#   totals_CPT_kcal %>% group_by(RIAGENDR) %>% summarise(mymean = mean(PROT))
#   
#   
#   colnames(CARBmean) <- c("UserName", "CARB_mean")

