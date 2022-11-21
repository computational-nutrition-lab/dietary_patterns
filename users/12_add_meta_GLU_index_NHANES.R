# ===============================================================================================================
# Add metadata and GLU_index (Normal, Prediabetic, and Diabetic) to the mean totals. 
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

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES")  


# ===============================================================================================================
# Load NHANES15-16totals with demographic data
# ===============================================================================================================
  
# Load the QC-ed total (with food categories), filtered for KCAL, PROT, TFAT, VC. 4207 people.
  QCtotals_d <- read.table("Total_D12_FC_QC_mean_QC_d.txt", sep="\t", header=T)

# Check the number of participants in the QCtotals - should be 4,207 people.
  length(unique(QCtotals_d$SEQN))

# ---------------------------------------------------------------------------------------------------------------
# Load the blood glucose data and see.
  glu <- read.xport("Raw_data/GLU_I.XPT")

# glu has LBXGLU - Fasting Glucose (mg/dL).
  head(glu)

# Count the number of rows with no missing data.
# 2972 individuals have glucose data.
  sum(complete.cases(glu))

# Take out only the rows with no missing data in LBXGLU.
  glu_comp <- glu[!is.na(glu$LBXGLU), ]

# Take a quick look at the distribution of LBXGLU.
  hist(glu_comp$LBXGLU)

# Use default of merge to only keep SEQNs found in both datasets.
  QCtotal_d_glu <- merge(x=QCtotals_d, y=glu_comp, by="SEQN")

# Check the dimension of QCtotal_d_glu - should be 1,943 rows.
  dim(QCtotal_d_glu)

# ---------------------------------------------------------------------------------------------------------------
# Load the body measure data.
  bodymea <- read.xport("Raw_data/BMX_I.XPT")

# Explanation of variables can be found here: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.html
# Relevant variables here include:
  # BMDSTATS - Body Measures Component Status Code: 1	== Complete data for age group.
  #            2 ==	Partial: Only height and weight obtained
  # BMXHT - Standing Height (cm)
  # BMIHT - Standing Height Comment
  # BMXBMI - Body Mass Index (kg/m**2)
  # BMXWAIST - Waist Circumference (cm)

# Add body measure to QCtotal_d_glu
  QCtotal_d_glu_body <- merge(x=QCtotal_d_glu, y=bodymea, by="SEQN")

# ---------------------------------------------------------------------------------------------------------------
# Load the metadata of people, which is in Total Day 1.
  metadata_raw <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1TOT_I.XPT")

# Total Day 1 has "dietary data for day 1" and "metadata", but we only need the metadata; thus, take out
# only the metadata columns (variable) and exclude the day 1 data.
# Column names' descriptions can be found here: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.htm#DRQSPREP

# First, specify the first and the last column names to select.
# Look for the column number that matches the first and last variable specified.
  sta_col_num_a <- match("DBQ095Z"  , names(metadata_raw))  # Salt-related questions
  end_col_num_a <- match("DRQSPREP" , names(metadata_raw))
  sta_col_num_b <- match("DRQSDIET" , names(metadata_raw))  # Diet-related questions
  end_col_num_b <- match("DRQSDT91" , names(metadata_raw))
  sta_col_num_c <- match("DRD340"   , names(metadata_raw))  # Fish-related questions
  end_col_num_c <- match("DRD370V"  , names(metadata_raw))

# Only select the metadata variables and SEQN, which is in column 1.
  metadata_only <- metadata_raw[, c(1,
                                    sta_col_num_a:end_col_num_a,
                                    sta_col_num_b:end_col_num_b,
                                    sta_col_num_c:end_col_num_c
                                    )]

# Check that this has only the SEQN and metadata columns.
  head(metadata_only, 1)

# Add meatadata to QCtotal_d_glu_body
  QCtotal_d_glu_body_meta <- merge(x=QCtotal_d_glu_body, y=metadata_only, by="SEQN")

### In summary, individuals were kept who were in the QCtotal_d AND also had glucose test measurements.
  # bodymeasures and metadata were added in a way that only individuals present in all the datasets
  # will be kept. 1943 individuals were kept.

# ===============================================================================================================
# Use QCtotal_d_glu_body_meta dataframe for further analysis.
# ===============================================================================================================

# Add index according to their glucose level: Normal, Prediabetic, and Diabetic.
  # Norm: 99 mg/dL or lower
  # Pred: 100 to 125 mg/dL
  # Diab: 126 mg/dL or higher

# Create an empty column to insert glucose level index.
  QCtotal_d_glu_body_meta$GLU_index <- NA

# Add glucose level index.
  for(i in 1: nrow(QCtotal_d_glu_body_meta)){
    if(     QCtotal_d_glu_body_meta$LBXGLU[i] < 100){ QCtotal_d_glu_body_meta$GLU_index[i] <- "Normal" }
    else if(QCtotal_d_glu_body_meta$LBXGLU[i] < 126){ QCtotal_d_glu_body_meta$GLU_index[i] <- "Prediabetic" }
    else{                                             QCtotal_d_glu_body_meta$GLU_index[i] <- "Diabetic" }
  }

# Check the first 10 rows of glucose and GLU_index columns in QCtotal_d_glu_body_meta.
  QCtotal_d_glu_body_meta[1:10, c("LBXGLU", "GLU_index")]

# Look at the frequency of GLU_index.
  table(QCtotal_d_glu_body_meta$GLU_index)
  
# ===============================================================================================================
# Exclude individuals who are following special diets.
# ===============================================================================================================

# There may be some participants following special diets such as low-sodium or gluten-free. Detailed explanation 
# about the special diet question can be found on the documentation
# [https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Dietary&Cycle=2017-2018].
# For this demonstration, we will select only those who are eating freely without following any diet.  
  
# Check the number of individuals who are following any specific diet (DRQSDIET==1).
  table(QCtotal_d_glu_body_meta$DRQSDIET)
  
# DRQSDIET==1 is following a special diet, so select only rows with DRQSDIET==2. 
  QCtotal_d_glu_body_meta_2 <- subset(QCtotal_d_glu_body_meta, DRQSDIET == 2)
  
# How many people remained? -- 1625 remained.
  table(QCtotal_d_glu_body_meta_2$DRQSDIET)
  
# Check the sample size of each category.
  table(QCtotal_d_glu_body_meta_2$GLU_index)
  
  # Diabetic      Normal Prediabetic 
  # 211         684         730 
  
# Save the dataset as a .txt file.
  write.table(QCtotal_d_glu_body_meta_2, file="Laboratory_data/QCtotal_d_glu_body_meta_2.txt",
              sep= "\t", row.names=F, quote= F)

# ---------------------------------------------------------------------------------------------------------------

  