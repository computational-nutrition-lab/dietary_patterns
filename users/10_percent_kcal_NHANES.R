# ========================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 09/07/2022 by Rie Sadohara
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

# Set working directory to "dietary_patterns".
  Session --> Set working direHctory --> Choose directory.  
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/percent_kcal.R")
  
# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# Load example totals data
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/")
  
# Load the totals with demographic data.
  totals <- read.table("Total_D12_FC_QC_mean_QC_d.txt",  sep = "\t", header = T)
 
  head(totals)
  # RIAGENDR = gender
  # RIDAGEYR = age
  # CARB, PROT, TFAT, KCAL columns exist.
  
# Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
  CPTpctKcalPerSEQN <- function(inputfn, user.name="SEQN", outfn){
    indexno_username <- which(names(inputfn)== user.name)
    indexno_carb <-     which(names(inputfn)== "CARB")
    indexno_prot <-     which(names(inputfn)== "PROT")
    indexno_tfat <-     which(names(inputfn)== "TFAT")
    indexno_kcal <-     which(names(inputfn)== "KCAL")
    indexno_alco <-     which(names(inputfn)== "ALCO")
  
  # print(outfn, indexno_username, indexno_carb, indexno_prot, indexno_tfat, indexno_kcal)

  # Take only the relevant columns from inputfn.
  totalssub2 <- inputfn[, c(indexno_username, 
                            indexno_carb,
                            indexno_prot,
                            indexno_tfat,
                            indexno_kcal,
                            indexno_alco
                            )]

  # % KCAL
  # calculate calories
  totalssub2$CARB_kcal <- totalssub2$CARB * 4
  totalssub2$PROT_kcal <- totalssub2$PROT * 4
  totalssub2$TFAT_kcal <- totalssub2$TFAT * 9
  
  # calculate kcal of each macronutrient per engergy (%)
  totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$KCAL * 100
  totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$KCAL * 100
  totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$KCAL * 100
  totalssub2$kcal_pct_sum <-  totalssub2$CARB_kcal_pct + totalssub2$PROT_kcal_pct + totalssub2$TFAT_kcal_pct
  
  print(head(totalssub2, 6))
  
  write.table(x=totalssub2, file=outfn, sep="\t", quote=F, row.names=F)
      
  } 
  
  
  hist(totals[, "ALCO" ])
  CPTpctKcalPerSEQN(inputfn= totals, user.name= "SEQN", 
                    outfn="Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt")
  colnames(totals)
  
  totals_CPT_kcal <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt", sep="\t", header=T) 
  colnames(totals_CPT_kcal)

  # What's the relationship between ALCO and total KCAL %?
  plot(totals_CPT_kcal$ALCO, totals_CPT_kcal$kcal_pct_sum)  
  
  hist(totals_CPT_kcal[, "kcal_pct_sum" ])
  
  totals_CPT_kcal[order(totals_CPT_kcal$kcal_pct_sum, decreasing=T), ]
  
  
  
###### Below was copied from 10_percentkcal_ASA24.R #####
# --------------------------------------------------------------------------------------------------------------
# Calculate the mean and SD of CARB, PROT, and TFAT.
  CPTgramsPerUser(inputfn= totals, user.name = "UserName", recall.no = "RecallNo",
                  outfn="VVKAJ_Tot_m_QCed_CPT_g.txt")
# Not used in the visualization below, but you may want to take a look at it.

 
# Calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
  CPTpctKcalPerUser(inputfn=totals, user.name = "UserName", recall.no = "RecallNo", 
                   outfn="VVKAJ_Tot_m_QCed_CPT_kcal.txt")
 
# Load the %kcal values 
  CPT_kcal <- read.table("VVKAJ_Tot_m_QCed_CPT_kcal.txt", sep="\t", header=T)
  CPT_kcal

# **NOTE** Do not alter the columnnames of CPT_kcal because the plotting functions below assume that 
# CPT_kcal has "UserName", "macronutrient", "n", "mean", and "sd" columns in it.

# --------------------------------------------------------------------------------------------------------------
# Define ggplot2 themes
  library(ggplot2)

# Theme black and white, with the base font size 14: change if necessary.
  theme_set(theme_bw(base_size = 14))

# --------------------------------------------------------------------------------------------------------------
# Plot a barchart without SD. 
  # Change the font size if necessary.
  stacked_wo_SD <- StackedwoSD(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  stacked_wo_SD
  
# --------------------------------------------------------------------------------------------------------------
 # Plot the "dodge"-type of barchart (3 bars per user, NOT STACKED).
  # Change the font size if necessary.
  dodgedtypebarchart <- DodgedBarchart(data= CPT_kcal) + theme(axis.text.x=element_text(size=11))  
  dodgedtypebarchart
 
# --------------------------------------------------------------------------------------------------------------
# Using CPT_kcal, create a stacked barchart.
  
# Create a vector that contains all the users (individuals). 
  individuals <- unique(CPT_kcal$UserName)

# Calculate sd_base and sd_forstacked for stacked barchart. 
# Note that this function assumes all users (individuals) have CARB, PROT, and TFAT values.
  CalcStackedSD(input.df= CPT_kcal, out.fn= "CPT_kcal_forstacked.txt")
  
# Load the saved file that has SD for stacked barchart.
  CPT_kcal_forstacked_read <- read.table("CPT_kcal_forstacked.txt", sep="\t", header=T)
  
# Stacked barchart with SD as error bars.
  stacked_with_SD <- StackedWithSD(data=CPT_kcal_forstacked_read) + theme(axis.text.x=element_text(size=11))
  stacked_with_SD
  
# Change the Y axis scale if necessary. Note that if the error bars of Carbohydrates disappear 
# after changing the limits of Y axis, it may be because the error bars are higher than the max Y.
# Ensure you have enough max Y value to accommodate the error bars.
  
# You can also change the breakpoints of the Y axis.
  stacked_with_SD + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100))

  
# Come back to the main directory
  setwd(main_wd)
  
  
  