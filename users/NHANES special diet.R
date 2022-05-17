# ========================================================================================
# Pick up people following specific diets - then do PCA
# Version 1
# Created on 05/17/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# 
# ========================================================================================
# 
# ---------------------------------------------------------------------------------------------------------------
# # Load necessary packages.
  library(SASxport)
  # library(foreign)

# Set where the NHANES data and food code table are.
# it is not in the eg_data folder because it's too large to save in GitHub folder. 
# setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")
  
# ---------------------------------------------------------------------------------------------------------------
# Load the QC-ed total (with food categories)
  QCtotal <- read.table("eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2.txt", sep="\t", header=T)
  head(QCtotal)
  length(unique(QCtotal$SEQN))
  head(unique(QCtotal$SEQN))
  tail(unique(QCtotal$SEQN))
  
# Load the metadata of people, which is in Total Day 1.  
  metadata_raw <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1TOT_I.XPT")
  head(metadata_raw, 2)
  length(unique(metadata_raw$SEQN))
  head(metadata_raw$SEQN)
  head(QCtotal$SEQN)

# Load the body measure data and see.
  bodymea <- read.xport("eg_data/NHANES/BodyMeasures/BMX_I.XPT")
  head(bodymea, 2)
  head(bodymea$SEQN)
  tail(bodymea$SEQN)

# Pick up only the metadata of people in QCtotal.
  keepnamesTF <- metadata_raw$SEQN %in% QCtotal$SEQN  # gives T or F for each. 
  head(keepnamesTF)
  head(metadata_raw$SEQN)
  head(QCtotal$SEQN)
  table(keepnamesTF)
  # Take only the participants whose names are in keepnamesTF
  metadata <- metadata_raw[keepnamesTF, ]   
  head(metadata$SEQN)

# ---------------------------------------------------------------------------------------------------------------
# Take a look at how many people are following a specific diet?
  table(metadata$DRQSDIET)
  # 682 people.

# Extract only those following any specific diet.
  metadata_1 <- metadata %>% filter(DRQSDIET == 1) 
  # metadata_1 <- metadata %>% filter(DRQSDIET == 2)  # for checking.
  table(metadata_1$DRQSDT1) 
  table(metadata_1$DRQSDT2) 
  table(metadata_1$DRQSDT3) 
  table(metadata_1$DRQSDT4) 
  table(metadata_1$DRQSDT5) 
  table(metadata_1$DRQSDT6) 
  table(metadata_1$DRQSDT7) 
  table(metadata_1$DRQSDT8) 
  table(metadata_1$DRQSDT9) 
  table(metadata_1$DRQSDT10) 
  table(metadata_1$DRQSDT11) 
  table(metadata_1$DRQSDT12) 
  table(metadata_1$DRQSDT91) 

  # Create a vector to use in a table to save count results. 
  diets <- c("DRQSDT1","DRQSDT2","DRQSDT3","DRQSDT4","DRQSDT5","DRQSDT6","DRQSDT7","DRQSDT8","DRQSDT9","DRQSDT10","DRQSDT11","DRQSDT12","DRQSDT91")
  
  # Create a dataframe to count results.
  dietfreq = data.frame(diet=diets, Yes=c(345, 94, 97, 37, 1, 7, 124, 14, 58, 15, 5, 5, 17))
  sum(dietfreq$Yes)
  # 819 > 682. So, some people are on more than 1 diet. 
  
# Extract SEQN and diet variables only. 
  metadata_2 <- metadata_1[, c("SEQN", diets)]
  head(metadata_2)
  
# Count the number of variables that are not NA. If > 1, they are on multiple diets.
  metadata_2$count_na <- rowSums(is.na(metadata_2))
  head(metadata_2)
  
# Compute the number of diets they are on.
  metadata_2$n_diets <- 13 - metadata_2$count_na
  table(metadata_2$n_diets)
  sum(metadata_2$n_diets) # Should be the same as sum(dietfreq$Yes).
  # So, 575 people are on 1 diet.
  
# Select only those following 1 diet.
  metadata_3 <- subset(metadata_2, n_diets==1)
  head(metadata_3)
  table(metadata_3$n_diets)  # OK
  table(metadata_3$count_na) # OK
  
# Find the diet they are on.
# Calc rowSums for the diet columns.
  dietcode <- rowSums(metadata_3[, 2:14], na.rm = T) 
  head(dietcode, 10)

# Append the dietcode.
  metadata_4 <- cbind(metadata_3, dietcode)
  head(metadata_4)
  table(metadata_4$dietcode)
  
# Load the dietcode and dietname table
  diettable <- read.table("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/NHANES_Totals_VarNames_Dietnames.txt", sep="\t", header=T)
  # Create the number only dietcode...
  diettable$dietcode_nonly <- c(1,2,3,4,5,6,7,8,9,10, 11, 12, 91)
  # Change the columnnames, to be able to do merge by dietcode.
  colnames(diettable) <- c("dietcode_full", "dietname", "dietcode")
  
# Merge (append the dietname)
  metadata_5 <- merge(x=metadata_4, y=diettable, by="dietcode", all.x=T)
  head(metadata_5)

# The frequency of people on each diet!!
  as.data.frame(table(metadata_5$dietname))
  # Weight loss, diabetic, low salt are the top 3.   
   
  
  
  
  
  
  
  
  
  
  