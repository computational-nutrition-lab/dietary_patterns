# ===============================================================================================================
# Look at the fasting glucose, and group individuals if possible. 
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
# It is not in the eg_data folder because it's too large to save in GitHub folder. 
# setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")
  source("lib/ggplot2themes.R") 

# Load the QC-ed total (with food categories)
  QCtotal <- read.table("eg_data/NHANES/Total_D12_FC_mean_QC_1.txt", sep="\t", header=T) # filtered for KCAL, PROT, TFAT, VC, BCAR. 3992 people.
  head(QCtotal,1)
  length(unique(QCtotal$SEQN))
  head(unique(QCtotal$SEQN))
  tail(unique(QCtotal$SEQN))

  # Load the metadata of people, which is in Total Day 1.
  metadata_raw <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1TOT_I.XPT")
  head(metadata_raw, 1)
  dim(metadata_raw)
  colnames(metadata_raw)
  vis_miss(metadata_raw, warn_large_data=F)
  length(unique(metadata_raw$SEQN))
  head(metadata_raw$SEQN)
  head(QCtotal$SEQN)
  
  # Load the body measure data and see.
  bodymea <- read.xport("eg_data/NHANES/BodyMeasures/BMX_I.XPT")
  head(bodymea, 2)
  colnames(bodymea)
  tail(bodymea$SEQN)
  vis_miss(bodymea)
  head(bodymea$SEQN)
  # Explanation of variables - https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm
  # BMDSTATS - Body Measures Component Status Code: 1	Complete data for age group. 2	Partial: Only height and weight obtained
  # BMXHT - Standing Height (cm) 
  # BMIHT - Standing Height Comment
  # BMXBMI - Body Mass Index (kg/m**2)
  BMXWAIST - Waist Circumference (cm)
    
  # # Pick up only the metadata of people in QCtotal.
  # keepnamesTF <- metadata_raw$SEQN %in% QCtotal$SEQN  # gives T or F for each. 
  # head(keepnamesTF)
  # head(metadata_raw$SEQN)
  # head(QCtotal$SEQN)
  # table(keepnamesTF)
  # 
  # # Take only the participants whose names are in keepnamesTF
  # metadata <- metadata_raw[keepnamesTF, ]   
  # head(metadata)
  # head(metadata$SEQN)
  
# Load the blood glucose data and see.
  glu <- read.xport("eg_data/NHANES/Laboratory_data/GLU_I.XPT")
  tail(glu, 50)
  head(glu$SEQN)
  dim(glu)
  
# LBXGLU - Fasting Glucose (mg/dL). 
  # Count the number of rows with no missing data.
  sum(complete.cases(glu))
  library(naniar)
  vis_miss(glu)

  # # Take out only rows with no missing data in LBXGLU and LBXGLUSI.
  # glu_comp <- glu[!is.na(glu$LBXGLU), ]
  # head(glu_comp)
  # dim(glu_comp)
  # hist(glu_comp$LBXGLU)

  # 2972 individuals have glucose data.
  
# Use default of merge to only keep SEQNs found in both datasets.
  QCtotalANDglu <- merge(x=QCtotal, y=glu_comp, by="SEQN")
  dim(QCtotalANDglu)      # 1847 rows.
  vis_miss(QCtotalANDglu) # No missing data!!!!
  
# Add body measure
  QCtotalANDglu_body <- merge(x=QCtotalANDglu, y=bodymea, by="SEQN")
  head(bodymea)
  dim(QCtotalANDglu_body)      # 1847 rows.
  vis_miss(QCtotalANDglu_body) # Some columns have all missing data.
  vis_miss(QCtotalANDglu_body[, c('SEQN', 'LBXGLU')]) # But GLU has no missing data.
  
# Add meatadata
  QCtotalANDglu_body_meta <- merge(x=QCtotalANDglu_body, y=metadata_raw, by="SEQN")
  head(metadata_raw) 
  dim(QCtotalANDglu_body_meta)      # 1847 rows.
  vis_miss(QCtotalANDglu_body_meta) # Some columns have all missing data.
  vis_miss(QCtotalANDglu_body_meta[, c('SEQN', 'DRQSDIET')]) # But DRQSDIET has no missing data.
  table(QCtotalANDglu_body_meta$LBXGLU) 
  head(QCtotalANDglu_body_meta) 

### In summary, indiduals were kept who were in the QCtotal and also had glucose test measurements. 
  # bodymeasures and metadata were added in a way that only individuals present in all the datasets
  # will be kept. 1847 individuals were kept.
  
# ---------------------------------------------------------------------------------------------------------------
# Use QCtotalANDglu_body_meta dataframe for further analysis. 
# ---------------------------------------------------------------------------------------------------------------
  
# Add index according to their glucose level: Normal, Prediabetic, and Diabetic. 
  # Norm: 99 mg/dL or lower 
  # Pred: 100 to 125 mg/dL 
  # Diab: 126 mg/dL or higher 
  
  summary(QCtotalANDglu_body_meta$LBXGLU)
  
  QCtotalANDglu_body_meta$GLU_index <- NA
  for(i in 1: nrow(QCtotalANDglu_body_meta)){
    if(     QCtotalANDglu_body_meta$LBXGLU[i] < 100){ QCtotalANDglu_body_meta$GLU_index[i] <- "Norm" }
    else if(QCtotalANDglu_body_meta$LBXGLU[i] < 126){ QCtotalANDglu_body_meta$GLU_index[i] <- "Pred" }
    else{                        QCtotalANDglu_body_meta$GLU_index[i] <- "Diab" }
  }
  head(QCtotalANDglu_body_meta)
  # Convert to factor
  QCtotalANDglu_body_meta$GLU_index <- factor(QCtotalANDglu_body_meta$GLU_index, levels = c('Norm', 'Pred', 'Diab'))
  # Look at the frequency.
  table(QCtotalANDglu_body_meta$GLU_index)
  
# Save this as a .txt file. 
  write.table(QCtotalANDglu_body_meta, file="QCtotalANDglu_body_meta.txt", sep= "\t", row.names = F, quote= F)
  
# ---------------------------------------------------------------------------------------------------------------
# Load the data of those to be used in the diabetes status analysis. 
  glu <- read.delim( file="QCtotalANDglu_body_meta.txt", sep= "\t", header= T )
  vis_miss( glu[, c("SEQN", "DRQSDIET")] )
  vis_miss( glu[, c("SEQN", "LBXGLU")] )

# make GLU_index as a factor for plotting.
  glu$GLU_index <- factor(glu$GLU_index, levels = c('Norm', 'Pred', 'Diab'))
    
# Exclude those who are following special diets.   
# Extract only those following any specific diet.
  table(QCtotalANDglu_body_meta$DRQSDIET)
  table(glu$DRQSDIET)
# 1 is following a special diet.
  library(dplyr)
  glu_2 <- glu %>% filter(DRQSDIET == 2)

# safety check  
  table(glu_2$DRQSDT1) # Should be zero.
  table(glu_2$DRQSDT2) # Should be zero.  
  vis_miss( glu_2[, c("SEQN", "LBXGLU")] )
  
# ---------------------------------------------------------------------------------------------------------------
# Look at the BMI frequency of each group.   
  
# The sample size are OK.. 
  table(glu_2$GLU_index)
  # Norm Pred Diab 
  # 656  694  204 
  
# Look for column names that end with 'BMI'.
# Not super useful because all the variables start with "BMI", because the alphabet for 15-16 is "I", 
  # so the prefix "BMX" is "BMI" for these years.... 
  grep("*BMI", colnames(glu_2) )
  colnames(glu_2)[114]
  
# The columnname for BMI is BMXBMI

    summary(glu_2$BMXBMI)
  vis_miss(glu_2[, c("SEQN", 'BMXBMI')])
  # 14 are missing BMI and has NA's...
  
  # Make sure the labels in the legend are correct. 
  BMIfreq <- ggplot(data=glu_2, aes(x=BMXBMI, group=GLU_index, fill=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4) + space_axes + no_grid +
    scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ,
                      labels= c("Normal", "Prediabetes", "Diabetes")) +
    labs(x="BMI", y="Density") 
  BMIfreq

# Save the chart as .png. n=1554 - 14 missing= 1540. 
  ggsave("eg_data/NHANES/Laboratory_data/BMI_by_GLU_index_n1540.png", BMIfreq,  device="png")
  
#### The diabetic population had higher BMI than prediabetes, and the lowest BMI was
  # the normal population.
  

    
  glu_2 %>% group_by(GLU_index) 
  
# ---------------------------------------------------------------------------------------------------------------
# Look at the KCAL frequency of each group.   
  # Make sure the labels in the legend are correct. 
  colnames(glu_2)
  KCALfreq <- ggplot(data=glu_2, aes(x=KCAL, group=GLU_index, color=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4, size=1.2, linetype="longdash") + space_axes + no_grid +
    scale_color_manual(values= c("steelblue3", "gold3", "hotpink") ,
                      labels= c("Normal", "Prediabetes", "Diabetes")) +
    labs(x="KCAL", y="Density") 
  KCALfreq
  
# Save the chart as .png.
  ggsave("KCAL_by_GLU_index_line_n1554.png", KCALfreq, device="png")
  
# ---------------------------------------------------------------------------------------------------------------
# Select only men in their 50s, for example, so that the samples are more uniform and smaller?
  
# ---------------------------------------------------------------------------------------------------------------
# Use Diabetes questionnaire results...  
# ---------------------------------------------------------------------------------------------------------------  
# Load the demographics data.   
  demo <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  tail(demo, 50)
  dim(demo)
  dim(glu)
  
  grep("RIDAGEYR", colnames(glu_2))
  grep("RIAGENDR", colnames(glu_2))
  
# Add the demographics info to glu_2.
  glu_3 <- merge(x=glu_2, y=demo, all.x=T, by="SEQN") # 1554 rows.
  colnames(glu_3)
    
# Age - no missing data. 
  hist(glu_3$RIDAGEYR)
  table(glu_3$RIDAGEYR)
  summary(glu_3$RIDAGEYR)
  
# Gender - no missing data. 1: male, 2: female.
  table(glu_3$RIAGENDR)     
  summary(glu_3$RIAGENDR)
  
# Select males in their 50s
  glu_3_males <- glu_3 %>% filter(RIAGENDR == 1) 
  glu_3_males50s <- glu_3_males %>% filter(RIDAGEYR >= 50 & RIDAGEYR <= 59 ) 
  table(glu_3_males50s$RIDAGEYR)
  table(glu_3_males50s$GLU_index)
  
  colnames(glu_3_males50s)

# Look at the KCAL frequency of each group.   
# Make sure the labels in the legend are correct. 
  KCALfreq <- ggplot(data=glu_3_males50s, aes(x=KCAL, group=GLU_index, color=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4, size=1.2, linetype="longdash") + space_axes + no_grid +
    scale_color_manual(values= c("steelblue3", "gold3", "hotpink") ,
                       labels= c("Normal", "Prediabetes", "Diabetes")) +
    labs(x="KCAL", y="Density") 
  KCALfreq
  
  # Save the chart as .png.
  ggsave("KCAL_by_GLU_index_males50s.png", KCALfreq, device="png")
  
# Create a boxplot of KCAL of each GLU_index group.
  KCAL_dots <- ggplot(glu_3_males50s, aes(x=GLU_index, y=KCAL)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    geom_jitter(width=0.3)
  KCAL_dots
  
  ggsave("KCAL_by_GLU_index_box_males50s.png", KCAL_dots, device="png")
  
  
# T-test if they are different...
  
  
  
# perform PCA and plot the individuals. color code by GLU_index.
  
  
  
  
  
  
  
