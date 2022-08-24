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

# Set where the NHANES data and food code table are.
# It is not in the eg_data folder because it's too large to save in GitHub folder. 
# setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")
  source("lib/ggplot2themes.R") 

# Load the QC-ed total (with food categories), filtered for KCAL, PROT, TFAT, VC. 4207 people.
  QCtotals <- read.table("eg_data/NHANES/Total_D12_FC_QC_mean_QC.txt", sep="\t", header=T) 
  head(QCtotals,1)
  length(unique(QCtotals$SEQN)) # 4207 people. 
  head(unique(QCtotals$SEQN))
  tail(unique(QCtotals$SEQN))

  # Load the metadata of people, which is in Total Day 1.
  metadata_raw <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1TOT_I.XPT")
  
  # Total Day 1 has data for day 1 and metadata, but we only need the metadata; thus, remove 
  # The day 1 data before merging.
  
  ####  WORK IN PROGRESS ####
  # First, speicify the first and the last column (variable) names to calculate totals for. 
  # Which column names mean what: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.htm#DRQSPREP
  # first.val_a <- "DBQ095Z"   
  # last.val_a <-  "DRQSPREP"  
  # first.val_b <- "DRQSDIET"
  # last.val_b <-  "DRQSDT91"
  # first.val_c <- "DRD340"  
  # last.val_c <-  "DRD370V" 
  
  # Look for the column number that matches the first and last variable specified.
  start_col_num_a <- match("DBQ095Z"  , names(metadata_raw))  # Salt-related questions
  end_col_num_a   <- match("DRQSPREP" , names(metadata_raw)) 
  start_col_num_b <- match("DRQSDIET" , names(metadata_raw))  # Diet-related questions 
  end_col_num_b   <- match("DRQSDT91" , names(metadata_raw)) 
  start_col_num_c <- match("DRD340"   , names(metadata_raw))  # Fish-related questions
  end_col_num_c   <- match("DRD370V"  , names(metadata_raw)) 
  
  # Only take out the metadata variables we would like to merge. 
  metadata_only <- metadata_raw[, c(1,   # SEQN is in column 1.
                                    start_col_num_a:end_col_num_a, 
                                    start_col_num_b:end_col_num_b, 
                                    start_col_num_c:end_col_num_c 
                                    )]
  
  # This should have only the metadata columns.
  head(metadata_only, 1)

  # Load the body measure data and see.
  bodymea <- read.xport("eg_data/NHANES/BodyMeasures/BMX_I.XPT")
  head(bodymea, 2)
  tail(bodymea$SEQN)
  vis_miss(bodymea)
  head(bodymea$SEQN)
  # Explanation of variables - https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm
  # BMDSTATS - Body Measures Component Status Code: 1	Complete data for age group. 2	Partial: Only height and weight obtained
  # BMXHT - Standing Height (cm) 
  # BMIHT - Standing Height Comment
  # BMXBMI - Body Mass Index (kg/m**2)
  # BMXWAIST - Waist Circumference (cm)
    
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

# Take out only rows with no missing data in LBXGLU and LBXGLUSI.
  glu_comp <- glu[!is.na(glu$LBXGLU), ]
  head(glu_comp)
  dim(glu_comp)
  hist(glu_comp$LBXGLU)

  # 2972 individuals have glucose data.
  
# Use default of merge to only keep SEQNs found in both datasets.
  QCtotalANDglu <- merge(x=QCtotals, y=glu_comp, by="SEQN")
  dim(QCtotalANDglu)      # 1943 rows.
  vis_miss(QCtotalANDglu) # No missing data!!!!
  
# Add body measure
  QCtotalANDglu_body <- merge(x=QCtotalANDglu, y=bodymea, by="SEQN")
  head(bodymea)
  dim(QCtotalANDglu_body)      # 1943 rows.
  vis_miss(QCtotalANDglu_body) # Some columns have all missing data.
  vis_miss(QCtotalANDglu_body[, c('SEQN', 'LBXGLU')]) # But GLU has no missing data.
  
# Add meatadata
  QCtotalANDglu_body_meta <- merge(x=QCtotalANDglu_body, y=metadata_only, by="SEQN")
  head(metadata_only) 
  dim(QCtotalANDglu_body_meta)      # 1943 rows.
  vis_miss(QCtotalANDglu_body_meta) # Some columns have all missing data.
  vis_miss(QCtotalANDglu_body_meta[, c('SEQN', 'DRQSDIET')]) # But DRQSDIET has no missing data.
  table(QCtotalANDglu_body_meta$LBXGLU) 

### In summary, individuals were kept who were in the QCtotal AND also had glucose test measurements. 
  # bodymeasures and metadata were added in a way that only individuals present in all the datasets
  # will be kept. 1943 individuals were kept.
  
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
    if(     QCtotalANDglu_body_meta$LBXGLU[i] < 100){ QCtotalANDglu_body_meta$GLU_index[i] <- "Normal" }
    else if(QCtotalANDglu_body_meta$LBXGLU[i] < 126){ QCtotalANDglu_body_meta$GLU_index[i] <- "Prediabetic" }
    else{                                             QCtotalANDglu_body_meta$GLU_index[i] <- "Diabetic" }
  }
  head(QCtotalANDglu_body_meta)
  # Convert it to factor
  QCtotalANDglu_body_meta$GLU_index <- factor(QCtotalANDglu_body_meta$GLU_index, levels = c('Normal', 'Prediabetic', 'Diabetic'))
  # Look at the frequency.
  table(QCtotalANDglu_body_meta$GLU_index)
  
# Save this as a .txt file. 
  write.table(QCtotalANDglu_body_meta, file="eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta.txt", 
              sep= "\t", row.names=F, quote= F)
  
# ---------------------------------------------------------------------------------------------------------------
# Load the data of those to be used in the diabetes status analysis. 
  glu <- read.delim( file="eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta.txt", sep= "\t", header= T )
  vis_miss( glu[, c("SEQN", "DRQSDIET")] )
  vis_miss( glu[, c("SEQN", "LBXGLU")] )

# make GLU_index as a factor for plotting.
  glu$GLU_index <- factor(glu$GLU_index, levels = c('Normal', 'Prediabetic', 'Diabetic'))
    
# Exclude those who are following special diets.   
# Extract only those following any specific diet.
  table(glu$DRQSDIET)
# 1 is following a special diet.
  library(dplyr)
  glu_2 <- glu %>% filter(DRQSDIET == 2)
  
# How many people remained? -- 1625 remained.
  table(glu_2$DRQSDIET)

# safety check  
  table(glu_2$DRQSDT1) # Should be zero.
  table(glu_2$DRQSDT2) # Should be zero.  
  vis_miss( glu_2[, c("SEQN", "LBXGLU")] )
  
# ---------------------------------------------------------------------------------------------------------------
# Look at the BMI frequency of each group.   
  
# The sample sizes are OK.. 
  table(glu_2$GLU_index)
  # Normal Prediabetic    Diabetic 
  # 684         730         211 
  
# The columnname for BMI is BMXBMI

  summary(glu_2$BMXBMI)
  vis_miss(glu_2[, c("SEQN", 'BMXBMI')])
  colSums(is.na(glu_2[, c("SEQN", "BMXBMI")]))
  # 14 are missing BMI and has NA's...
  
  # Make sure the labels in the legend are correct. 
  BMIfreq <- ggplot(data=glu_2, aes(x=BMXBMI, group=GLU_index, fill=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4) + space_axes + no_grid +
    scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
    labs(x="BMI", y="Density") 
  BMIfreq

# Save the chart as .png. n=1625 - 14 missing= 1611. 
  ggsave("eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta_demo_n1611_BMI_by_GLU_index.png", BMIfreq,  device="png")
  
#### The diabetic population had higher BMI than prediabetes, and the lowest BMI was
  # the normal population.
  
# ---------------------------------------------------------------------------------------------------------------
# Look at the body weight frequency of each group.   
  
  # The columnname for bodyweight is BMXWT 
  
  weightcomment <- glu_2[, c('BMXWT' , 'BMIWT')]
  rrr <- filter(weightcomment, is.na(BMXWT))
  rrr
  
  summary(glu_2$BMXBMI)
  vis_miss(glu_2[, c("SEQN", 'BMXWT')])
  colSums(is.na(glu_2[, c("SEQN", "BMXWT")]))
  # 12 are missing body weight and has NA's.
  hist(glu_2$BMXWT)
  
  # Make sure the labels in the legend are correct. 
  weightfreq <- ggplot(data=glu_2, aes(x=BMXWT, group=GLU_index, fill=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4) + space_axes + no_grid +
    scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") 
    ) +
    labs(x="Body weight (kg)", y="Density") 
  weightfreq
  
  # Save the chart as .png. n=1625 - 14 missing= 1611. 
  ggsave("eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta_demo_n1613_weight_by_GLU_index.png", weightfreq,  device="png")

  
# ---------------------------------------------------------------------------------------------------------------
# Look at the KCAL frequency of each group.   
  # Make sure the labels in the legend are correct. 
  colnames(glu_2)
  KCALfreq <- ggplot(data=glu_2, aes(x=KCAL, group=GLU_index, color=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4, size=1.2, linetype="longdash") + space_axes + no_grid +
    scale_color_manual(values= c("steelblue3", "gold3", "hotpink") ) +
    labs(x="KCAL", y="Density") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  KCALfreq
  
# Save the chart as .png.
  ggsave("eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta_demo_n1625_KCAL_by_GLU_index_line.png", KCALfreq, device="png")

# ===============================================================================================================
# Select only men in their 50s, for example, so that the samples are more uniform and smaller?
# ===============================================================================================================
  
# ---------------------------------------------------------------------------------------------------------------
# Use the demographics results...
# ---------------------------------------------------------------------------------------------------------------  
# Load the demographics data.   
  demo <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  tail(demo, 5)
  dim(demo)
  dim(glu)
  
  grep("RIDAGEYR", colnames(glu_2))
  grep("RIAGENDR", colnames(glu_2))
  
# Add the demographics info to glu_2.
  glu_3 <- merge(x=glu_2, y=demo, all.x=T, by="SEQN") # 1625 rows.
  colnames(glu_3)
  
# Save the glu_3 as a txt file.
  write.table(glu_3, "eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta_demo.txt", 
              sep="\t", row.names = F, quote = F)
    
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
  dim(glu_3_males50s)
  table(glu_3_males50s$RIDAGEYR)
  table(glu_3_males50s$GLU_index)
  
# Save the glu_3_males50s as a txt file.
  write.table(glu_3_males50s, "eg_data/NHANES/Laboratory_data/QCtotalANDglu_body_meta_demo_males50s.txt", 
              sep="\t", row.names = F, quote = F)
  
# ----------------------------------------------------------------------------------------------------------------  
# BMI frequency of each group.
  
  males50s_BMIfreq <- ggplot(data=glu_3_males50s, aes(x=BMXBMI, group=GLU_index, fill=GLU_index)) +
    geom_density(adjust=1.5, alpha=0.4) + space_axes + no_grid +
    scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +  # lighter color.
    labs(x="BMI", y="Density") 
  males50s_BMIfreq

  # Save the chart as .png.
  ggsave("eg_data/NHANES/Laboratory_data/males50s_BMI_by_GLU_index.png", males50s_BMIfreq, device="png")
  
# ----------------------------------------------------------------------------------------------------------------  
# Body weight
    # Make sure the labels in the legend are correct. 
  weightfreq <- ggplot(data=glu_3_males50s, aes(x=BMXWT, group=GLU_index, fill=GLU_index)) +
    geom_density(adjust=1.5, alpha=.4) + space_axes + no_grid +
    scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    labs(x="Body weight (kg)", y="Density") 
  weightfreq
  
  ggsave("eg_data/NHANES/Laboratory_data/males50s_weight_by_GLU_index.png", weightfreq,  device="png")

# ----------------------------------------------------------------------------------------------------------------  
# Look at the KCAL frequency of each group.   
# Make sure the labels in the legend are correct. 
  males50s_KCALfreq <- ggplot(data=glu_3_males50s, aes(x=KCAL, group=GLU_index, color=GLU_index)) +
    geom_density(adjust=1.5, alpha=0.4, size=1.2, linetype="longdash") + space_axes + no_grid +
    scale_color_manual(values= c("aquamarine3", "lightgoldenrod3", "lightpink1")) +
    labs(x="KCAL", y="Density") +
    scale_y_continuous(labels= function(x) format(x, scientific = FALSE))
  males50s_KCALfreq
  
  # Save the chart as .png.
  ggsave("eg_data/NHANES/Laboratory_data/males50s_KCAL_by_GLU_index.png", males50s_KCALfreq, device="png")
  
# ----------------------------------------------------------------------------------------------------------------  
# Create a boxplot of KCAL of each GLU_index group.
  males50s_KCAL <- ggplot(glu_3_males50s, aes(x=GLU_index, y=KCAL, fill=GLU_index)) +
    geom_boxplot(outlier.shape = NA) + no_grid + space_axes +
    scale_fill_manual(values= c("aquamarine2", "lightgoldenrod1", "lightpink1") ) +
    geom_jitter(width=0.3)
  males50s_KCAL
  
  ggsave("eg_data/NHANES/Laboratory_data/males50s_KCAL_by_GLU_index_box.png", 
         males50s_KCAL, device="png")
  
# ANOVA if they are different...?
  
  
  
  
  
  
  
