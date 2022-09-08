# ===============================================================================================================
# Calculate the percentage of carb, prot, and tfat in kcal. 
# But the total of calories from those three macronutients do not add up to 100% for some samples.
# Version 1
# Created on 09/08/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Load 
# ===============================================================================================================

# Set working directory to "dietary_patterns".
  Session --> Set working direHctory --> Choose directory.  
  setwd("~/GitHub/dietary_patterns")

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_data_dir.R")
  source("lib/percent_kcal.R")

# Load example totals data
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/")

# Load the totals with demographic data.
  totals <- read.table("Total_D12_FC_QC_mean_QC_d.txt",  sep = "\t", header = T)
  head(totals)
# RIAGENDR = gender
# RIDAGEYR = age
# CARB, PROT, TFAT, KCAL columns exist.

# Define function to calculate %kcal 
  CPTpctKcalPerSEQN <- function(inputfn, user.name="SEQN", outfn){
  
  # Get index numbers for username, recallno, "CARB","PROT","TFAT", "KCAL"
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
  totalssub2$kcal_sum <-  totalssub2$CARB_kcal + totalssub2$PROT_kcal + totalssub2$TFAT_kcal
  
  # calculate kcal of each macronutrient per engergy (KCAL column, %)
  totalssub2$CARB_kcal_pct <- totalssub2$CARB_kcal / totalssub2$KCAL * 100
  totalssub2$PROT_kcal_pct <- totalssub2$PROT_kcal / totalssub2$KCAL * 100
  totalssub2$TFAT_kcal_pct <- totalssub2$TFAT_kcal / totalssub2$KCAL * 100
  totalssub2$kcal_pct_sum <-  totalssub2$CARB_kcal_pct + totalssub2$PROT_kcal_pct + totalssub2$TFAT_kcal_pct
  
  print(head(totalssub2, 6))
  
  write.table(x=totalssub2, file=outfn, sep="\t", quote=F, row.names=F)
  
} 

# Some have really high ALCO consumption.
  hist(totals[, "ALCO" ])
  
# Run the function
  CPTpctKcalPerSEQN(inputfn= totals, user.name= "SEQN", 
                    outfn="Total_D12_FC_QC_mean_QC_d_CPT_kcal_TEST.txt")

# Load the output.
  totals_CPT_kcal <- read.table("Total_D12_FC_QC_mean_QC_d_CPT_kcal.txt", sep="\t", header=T) 
  summary(totals_CPT_kcal)

# What's the relationship between ALCO and total KCAL %?
  plot(totals_CPT_kcal$ALCO, totals_CPT_kcal$KCAL)

  hist(totals_CPT_kcal[, "kcal_pct_sum" ])

  head(totals_CPT_kcal[order(totals_CPT_kcal$kcal_pct_sum, decreasing=F), ], 10)

# calories from carb, prot, and tfat do not add up to 100%. And the deviation from 100% is larger 
# for those who consumed more alcohol. The "KCAL" seems not to be a plain sum of those three
# macronutrients, but it probably counted fiber and alcohol and other nutrients. 




 
# ---------------------------------------------------------------------------------------------------------------
