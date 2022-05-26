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

  library(dplyr)
# Extract only those following any specific diet.
  metadata_1 <- metadata %>% filter(DRQSDIET == 1)
  # metadata_1 <- metadata %>% filter(DRQSDIET == 2)  # for checking.
# How many are on each diet?
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
  dietsvec <- c("DRQSDT1","DRQSDT2","DRQSDT3","DRQSDT4","DRQSDT5","DRQSDT6","DRQSDT7","DRQSDT8","DRQSDT9","DRQSDT10","DRQSDT11","DRQSDT12","DRQSDT91")
  
  # Create a dataframe to count results.
  dietfreq = data.frame(diet=dietsvec, Yes=c(345, 94, 97, 37, 1, 7, 124, 14, 58, 15, 5, 5, 17))
  sum(dietfreq$Yes)
  # 819 > 682. So, some people are on more than 1 diet. 

# Count the number of diet variables that are not NA. If 13, they are not on a specific diet. If 12, they are on 1 diet. If < 12, they are on multiple diets.
  metadata$count_na <- rowSums(is.na(metadata[, dietsvec])) # count number of NA's in the columns specified in "dietsvec".
  table(metadata$count_na)
  
# Compute the number of diets they are on.
  metadata$n_diets <- 13 - metadata$count_na
  table(metadata$n_diets)
  # So, 575 people are on 1 diet.
  sum(metadata$n_diets) # Should be the same as sum(dietfreq$Yes).
  
# -----------------------------------------------------------------------------------------------------------------
# Append the name of the diet to metadata and freqtable.
# Load the dietcode and dietname reference table.
  diettable <- read.table("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/NHANES_Totals_VarNames_Dietnames.txt", sep="\t", header=T)
  # Create the number only dietcode...
  diettable$dietcode_nonly <- c(1,2,3,4,5,6,7,8,9,10,11,12,91)
  # Change the columnnames, to be able to do merge by dietcode.
  colnames(diettable) <- c("dietcode_full", "dietname", "dietcode")
  
  
# Select only those following 1 diet. -------------------------------------------------------------------
  metadata_a <- subset(metadata, n_diets==1)
  dim(metadata_a)
  head(metadata_a)
  table(metadata_a$n_diets)  # OK
  table(metadata_a$count_na) # OK
  
# Find the diet they are on. 
  # Calc rowSums for the diet columns. (because only the columns of the diet that each individual is on has a number, not NA.)
  metadata_a$dietcode <- rowSums(metadata_a[, dietsvec], na.rm = T) 
  # Check
  head(metadata_a[, c("SEQN", dietsvec, "dietcode")], 3)
  # freq table of diets followed by them.
  onedietfreq <- as.data.frame(table(metadata_a$dietcode))
  colnames(onedietfreq)[1] <- "dietcode"
  onedietfreq

# Append the dietname to the frequency table as well. 
  onedietfreq_name <- merge(x=onedietfreq, y=diettable, by="dietcode", all.x=T)
  sum(onedietfreq_name$Freq) # Should total to the number of individuals following one diet. 
  onedietfreq_name
# Weight loss, diabetic, low salt are the top 3.   

# Add the dietname to metadata.
  metadata_1diet <- merge(x=metadata_a, y=diettable, by="dietcode", all.x=T)
  head(metadata_1diet,1)

# metadata_1diet has the metadata of the individuals who are in QCtotal and following one specific diet.
  # Extract only necessary info.
  metadata_b <- metadata_1diet[, c("SEQN", "dietcode", "dietname")]
  head(metadata_b)

# ========================================================================================
# Scenario A: the top most followed diets 
#  Weight_loss, Diabetic, Low_salt, Low_fat, Low_carb, Regular 
# ========================================================================================  

# Take the first 20 individuals from weight-loss (low calorie; n=300), diabetic (n=97), 
  # low-salt (n=51), low-fat (n=43), and low-carb (n=33)
  weightloss <- subset(metadata_1diet, dietname == "Weight_loss")[1:20, "SEQN"]   
  diabetic <- subset(metadata_1diet, dietname   == "Diabetic")[1:20, "SEQN"]   
  lowsalt <- subset(metadata_1diet, dietname    == "Low_salt")[1:20, "SEQN"]   
  lowfat <- subset(metadata_1diet, dietname     == "Low_fat")[1:20, "SEQN"]   
  lowcarb <- subset(metadata_1diet, dietname    == "Low_carb")[1:20, "SEQN"]   
  
  regulardiet <- subset(metadata, DRQSDIET == 2)[1:20, "SEQN"] # those are not following any specific diet.
  
# Combine those
  diffdiet120_short <- data.frame(Weight_loss=weightloss, 
                            Diabetic=diabetic, 
                            Low_salt=lowsalt, 
                            Low_fat=lowfat, 
                            Low_carb=lowcarb, 
                            Regular=regulardiet)
  diffdiet120_short
  
# Create a long table out of this.
  library(reshape2)
  diffdiet120 <- melt(diffdiet120_short)
  colnames(diffdiet120) <- c("Diet", "SEQN")
  head(diffdiet120)
  
# ---------------------------------------------------------------------------------------------------
# Add diet info while picking up the individuals from totalQC.
  QCtotal_1diet <- merge(x=diffdiet120, y=QCtotal, by="SEQN", all.x=T)
  head(QCtotal_1diet, 2)

# Save it as an input file.
  write.table(QCtotal_1diet, "eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_120diffdiet.txt",
              sep="\t", quote=F)
  
# Let's see how different those diet groups are .....  
  library(ggplot2)
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=KCAL)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=TFAT)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=PROT)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=CARB)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=FIBE)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=PF_TOTAL)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=V_TOTAL)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=V_STARCHY_OTHER)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=V_LEGUMES)) + geom_boxplot()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=D_TOTAL)) + geom_boxplot()
# Not super different???
         
# ---------------------------------------------------------------------------------------------------
# Define the sample total data with which you are going to do clustering. 
  totals_QCed_sampled <- QCtotal_1diet

# Follow the code in prep_for_clustering.    

#### Clustering analysis ####  

# Plot PC1 and PC2 and color-code individuals by their diet.
# Load the FC results
  PCA_FC <- read.table("results/PCA_results/NHANES1516_totalsbyhand_FC_n120/NHANES1516_total_d12_FC_mean_QC_2_120diffdiet_input_PCs.txt",
                       sep="\t", header=T)
  head(PCA_FC)

# Plot PC1 and PC2 of the PCA results of Foof Categories.  
  fillcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  colcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  ggplot(data=PCA_FC, aes(x=PC1, y=PC2, fill=Diet, color=Diet, shape=Diet))+
    geom_point(size=3) + 
    scale_fill_manual(values=fillcolor) +
    scale_color_manual(values=colcolor) +
    theme(legend.position = "bottom")

# Load the nutrient results
  PCA_nut <- read.table("results/PCA_results/NHANES1516_totalsbyhand_nut_n120/NHANES1516_total_d12_nut_mean_QC_2_120diffdiet_input_PCs.txt",
                       sep="\t", header=T)
  head(PCA_nut)
  
  # Plot PC1 and PC2 of the PCA results of Food Categories.  
  fillcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  colcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  ggplot(data=PCA_nut, aes(x=PC1, y=PC2, fill=Diet, color=Diet, shape=Diet))+
    geom_point(size=3) + 
    scale_fill_manual(values=fillcolor) +
    scale_color_manual(values=colcolor) +
    theme(legend.position = "bottom")
  
  #  
  
# ========================================================================================
# Scenario B: diets expected to separate more. 
#  Low_salt, Low_carb, Weight_gain, GF, High_prot,Regular
# ========================================================================================  
  
# Take the first 20 individuals from High_prot (n=6), GF (n=4), Low_salt (n=51), weight_gain (n=12), 
  # Low-carb (n=33).
  highprot <- subset(metadata_1diet, dietname   == "High_prot")[1:6, "SEQN"]   
  GF <-      subset(metadata_1diet, dietname    == "GF")[1:4, "SEQN"]   
  lowsalt <- subset(metadata_1diet, dietname    == "Low_salt")[1:20, "SEQN"]   
  wtgain <-  subset(metadata_1diet, dietname    == "Weight_gain")[1:12, "SEQN"]   
  lowcarb <- subset(metadata_1diet, dietname    == "Low_carb")[1:20, "SEQN"]   
  
  regulardiet <- subset(metadata, DRQSDIET == 2)[1:20, "SEQN"] # those are not following any specific diet.
  
  head(highprot,2)
  
  # Combine those
  diffdiet82 <- data.frame( Diet = c(rep('Highprot', length(highprot)), rep('Gluten_free', length(GF)),
                                     rep('Low_salt', length(lowsalt)), rep('Weight_gain', length(wtgain)),
                                     rep('Low_carb', length(lowcarb)), rep('Regular', length(regulardiet))),
                            SEQN = c(highprot, GF, lowsalt, wtgain, lowcarb, regulardiet))
  diffdiet82
  
# ---------------------------------------------------------------------------------------------------
  # Add diet info while picking up the individuals from totalQC.
  QCtotal_1diet <- merge(x=diffdiet82, y=QCtotal, by="SEQN", all.x=T)
  head(QCtotal_1diet, 2)
  
  # Save it as an input file.
  write.table(QCtotal_1diet, "eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_82diffdiet.txt",
              sep="\t", quote=F)
  
  # Let's see how different those diet groups are .....  
  library(ggplot2)
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=KCAL)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=TFAT)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=PROT)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=CARB)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=FIBE)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=PF_TOTAL)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=V_TOTAL)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=V_STARCHY_OTHER)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=V_LEGUMES)) + geom_boxplot() + theme_bw()
  ggplot(QCtotal_1diet, aes(x=factor(Diet), y=D_TOTAL)) + geom_boxplot() + theme_bw()
  
  # ---------------------------------------------------------------------------------------------------
  # Define the sample total data with which you are going to do clustering. 
  totals_QCed_sampled <- QCtotal_1diet
  
  # Follow the code in prep_for_clustering.    
  
  #### Clustering analysis ####  
  
  
  # Plot PC1 and PC2 and color-code individuals by their diet.
  # Load the FC results
  PCA_FC <- read.table("results/PCA_results/NHANES1516_totalsbyhand_FC_n82/NHANES1516_total_d12_FC_mean_QC_2_82diffdiet_input_PCs.txt",
                       sep="\t", header=T)
  head(PCA_FC,2)
  
  # Plot PC1 and PC2 of the PCA results of Foof Categories.  
  fillcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  colcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  FCbiplot <- 
    ggplot(data=PCA_FC, aes(x=PC1, y=PC2, fill=Diet, color=Diet, shape=Diet))+
      geom_point(size=3) + 
      scale_fill_manual(values=fillcolor) +
      scale_color_manual(values=colcolor) +
      theme(legend.position = "bottom") +
      theme(aspect.ratio = 1)
  FCbiplot
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_FC_n82/FCbiplot.tif", FCbiplot, device = "tiff", width = 7, height=7, dpi=250)
  
  
  # Load the nutrient results
  PCA_nut <- read.table("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/NHANES1516_total_d12_nut_mean_QC_2_82diffdiet_input_PCs.txt",
                        sep="\t", header=T)
  head(PCA_nut)
  
  # Plot PC1 and PC2 of the PCA results of Foof Categories.  
  fillcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  colcolor = c("darkred", "orange", "darkgreen", "darkblue", "violet", "grey45")
  nutbiplot <- 
    ggplot(data=PCA_nut, aes(x=PC1, y=PC2, fill=Diet, color=Diet, shape=Diet))+
      geom_point(size=3) + 
      scale_fill_manual(values=fillcolor) +
      scale_color_manual(values=colcolor) +
      theme(legend.position = "bottom") +
      theme(aspect.ratio = 1)
  nutbiplot
  ggsave("results/PCA_results/NHANES1516_totalsbyhand_nut_n82/nutbiplot.tif", nutbiplot, device = "tiff", width = 7, height=7, dpi=250)
  
  
# ---------------------------------------------------------------------------------------------------  
# Create a food tree and see.
# Food tree uses individual food data, not total, so need to subset a QC-ed individual food data.
# QC-ed individual food data has been created by create_food_tree_NHANES.R. So borrow from there:
  
  # This has the SEQN of people and their specific diets
  diffdiet82 
  
  food12_QCed <- read.table("eg_data/NHANES1516/processed/foodday1and2.txt", sep = "\t", header=T)
  head(food12_QCed)
  
  tail(food12)
  write.table(head(food12_QCed),  "eg_data/NHANES1516/processed/foodday1and2_head.txt", sep = "\t", row.names = F, quote=F)
  write.table(tail(food12_QCed)[, c("SEQN", "FoodCode", "FoodAmt", "Day")],  "eg_data/NHANES1516/processed/foodday1and2_tail_4var.txt", sep = "\t", row.names = F, quote=F)
  
  write.table(tail(food12), "eg_data/NHANES1516/processed/food12_tail.txt", sep = "\t", row.names=F, quote=F)
  food12_tail <- read.table("eg_data/NHANES1516/processed/food12_tail.txt", sep = "\t", header=T)
  food12_tail
  
  
  
  
  totalsof82 <- read.table("eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_82diffdiet.txt",
                           sep="\t", header=T) 
  
  
  
  
  
  