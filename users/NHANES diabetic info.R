# ===============================================================================================================
# Look at the fasting glucose and oral glucose tolerance test, and group indvididuals
# if possible. 
# Version 1
# Created on 08/04/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Load NHANES15-16 data
# ===============================================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Load necessary packages.
library(SASxport)
# library(foreign)

# Set where the NHANES data and food code table are.
# it is not in the eg_data folder because it's too large to save in GitHub folder. 
# setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")
  source("lib/ggplot2themes.R") 

# Load the QC-ed total (with food categories)
  QCtotal <- read.table("eg_data/NHANES/Total_D12_FC_mean_QC_1.txt", sep="\t", header=T) # filtered for KCAL, PROT, TFAT, VC, BCAR. 3992 people.
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
  
# Load the blood glucose data and see.
  glu <- read.xport("eg_data/NHANES/Laboratory_data/GLU_I.XPT")
  tail(glu, 50)
  head(glu$SEQN)
  dim(glu)
  
# LBXGLU - Fasting Glucose (mg/dL). 
  # Count the number of rows with missing data.
  sum(complete.cases(glu))
  library(naniar)
  vis_miss(glu)

  # Take out only rows with no missing data in LBXGLU and LBXGLUSI.
  glu_comp <- glu[!is.na(glu$LBXGLU), ]
  
# Load the oral glucose tolerance test.
  ogtt <- read.xport("eg_data/NHANES/Laboratory_data/OGTT_I.XPT")
  head(ogtt)
  head(ogtt$SEQN)
  dim(ogtt)
  # LBXGLT - Two Hour Glucose (OGTT) (mg/dL) is of interest.
  hist(ogtt$LBXGLT)
  
  # GTDCODE - Incomplete OGTT Comment Code. Should only choose zero (=complete record).
  library(dplyr)
  ogtt_comp <- ogtt %>% filter(GTDCODE == 0)
  
  # GTXDRANK - Amount of glucose challenge drank. Should choose 1= all.
  table(ogtt_comp$GTXDRANK)
  # Those with complete record drank all glucose liquid. Good.
  
# Merge the complete glu and complete ogtt datasets. 
  GO <- merge(x=glu_comp, y=ogtt_comp, by="SEQN")
  colnames(GO)
  head(GO)

# Count the number of rows with no missing data.
  sum(complete.cases(GO))
  vis_miss(GO)
# No missing data, OK. 

    
# ---------------------------------------------------------------------------------------------------------------
# Use dataframe GO for further analysis. 
# ---------------------------------------------------------------------------------------------------------------

# Add index according to their glucose level: Normal, Prediabetic, and Diabetic. 
  # Norm: 99 mg/dL or lower 
  # Pred: 100 to 125 mg/dL 
  # Diab: 126 mg/dL or higher 
  
  summary(GO$LBXGLU)
  
  GO$GLU_index <- NA
  for(i in 1: nrow(GO)){
    if(     GO$LBXGLU[i] < 100){ GO$GLU_index[i] <- "Norm" }
    else if(GO$LBXGLU[i] < 126){ GO$GLU_index[i] <- "Pred" }
    else{                        GO$GLU_index[i] <- "Diab" }
  }
  head(GO)
  # Convert to factor
  GO$GLU_index <- factor(GO$GLU_index, levels = c('Norm', 'Pred', 'Diab'))
  # Look at the frequency.
  table(GO$GLU_index)

# Add index according to their glucose tolerance test type: Normal, Prediabetic, and Diabetic. 
# Norm: 140 mg/dL or lower 
# Pred: 141 to 199 mg/dL 
# Diab: 200 mg/dL or higher 

  summary(GO$LBXGLT)
  GO$OGTT_index <- NA
  for(i in 1: nrow(GO)){
    if(     GO$LBXGLT[i] < 141){ GO$OGTT_index[i] <- "Norm" }
    else if(GO$LBXGLT[i] < 200){ GO$OGTT_index[i] <- "Pred" }
    else{                        GO$OGTT_index[i] <- "Diab" }
  }
  head(GO)

# -------------------------------------------------------------------------------------------------------
# Change workind dir.
  setwd("~/GitHub/dietary_patterns/eg_data/NHANES/Laboratory_data/")

# Save GO for later use.
  write.table(x=GO, "GO.txt", sep="\t", quote=F, row.names = F)

# Load GO.txt
  GO <-  read.delim("GO.txt", sep="\t", header=T) 
  head(GO)
  
  # Convert to factor
  GO$OGTT_index <- factor(GO$OGTT_index, levels = c('Norm', 'Pred', 'Diab'))
  # Look at the frequency.
  table(GO$OGTT_index)

# Look at the frequency of GO$GLU_index (by fasting glucose level) and GO$OGTT_index (oral glucose
# tolerance test).
  twoway = table(GO$`GLU_index`, GO$OGTT_index)
  twoway
  ttwoway = t(twoway) #transpose
  ttwoway = as.data.frame.matrix(ttwoway)
  Total = colSums(twoway) # Column totals
  cc = cbind(ttwoway, Total)  # Add column totals
  sortedcc = cc[ order(cc$Total, decreasing = T), ]  # sort by col totals
  sortedccrr = rbind(sortedcc, colSums(sortedcc))  # Add row totals 
  rownames(sortedccrr)[length(rownames(sortedccrr))] <- "Total"  # Change the last rowname
  sortedccrr
  
  # The vertical ones are OGTT_index with 1623 normal ones.    
  
  
   
# ---------------------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  