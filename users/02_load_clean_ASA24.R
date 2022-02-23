# For USERS ==============================================================================

# ========================================================================================
# Load and clean ASA24 data.
# Version 1
# Created on 02/04/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Load data
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")
  source("lib/load_clean_ASA24.R")
  
# Ensure your input files have no special characters that mess up loading:
#  "
#  '
#  #
#  &

# Use dietstudy data -----------------------------------------------------------
  # Specify the directory where the data is.
    SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
    
  # Load the totals.csv
    totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)
  
  # Load the items.csv
    items <- read.table("Items_to_use.txt", quote = "", sep = "\t", header = T)
    
  # Load your metadata if you have one. 
    metadata_1 <- read.csv("Metadata_1.csv", header=T)
    metadata_2 <- read.csv("Food_map_txt_Metadata_2.csv", header=T)
    
  # Come back to the main directory
    setwd(main.wd)  

# Use VVKAJ data ---------------------------------------------------------------
  # Specify the directory where the data is.
    SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  
  
  # Load your Items data.
    Items_raw <- read.csv("VVKAJ_2021-11-09_7963_Items_NoCommas.csv", sep = ",", header=T)
 
  # Load your totals data if ready to go.
    totals <- read.csv("VVKAJ_2021-11-09_7963_Totals.csv", sep = ",", header=T)
    
  # Come back to the main directory
    setwd(main.wd)
  
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Use metadata to filter 
# ========================================================================================  
# ---------------------------------------------------------------------------------------------------------------
  # Use metadata_1
  # Remove specified rows of 'totals' in metadata_1 (Metadata_1)
  
  # Show which has "yes" in the "Remove" column, and remove them.  
  RemoveRows(data = totals, metadata.file = metadata_1)
  # The resulting dataset, totals_selected, can be used for further analyses.
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
  # Use metadata_2
  # Create a unique list of participants that has their gender, age, height, weight, BMI, and Waist.Circumference, 
  # which did not change over the study days (i.e. only one data per person)
  
  # Take only the first row of each participant. 
  metadata_2_a <- metadata_2[!duplicated(metadata_2$UserName), 
                           c("UserName", "Gender", "Age", "Weight", "Height", "BMI", "Waist.Circumference")] 
  head(metadata_2_a)
  
  # Add this metadata of each participant in totals. 
  totals_selected <- merge(x=totals_selected, y=metadata_2_a, by="UserName", all.x=T)
  
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Generate sum of foods consumed by each user, by day, and by occasion
# ======================================================================================== 
# Use VVKAJ data. ---------------------------------------------------------
  SumByOccasion(items.data=Items_raw, User.Name='UserName', 
                Recall.No='RecallNo',   Occ.No='Occ_No')

  AddOccNames(items.data=Items_raw, User.Name='UserName', 
              Recall.No='RecallNo', Occ.No='Occ_No', Occ.Name='Occ_Name'  )
  # The output Sum_by_User_Day_Occ has the sum of foods consumed by each user, by day, and by occasion,
  # with occasion names in word.
  
# Use diet_study data. -----------------------------------------------------
  SumByOccasion(items.data=items, User.Name='UserName', 
                Recall.No='RecordDayNo',   Occ.No='Occ_No')
  
  AddOccNames(items.data=items, User.Name='UserName', 
              Recall.No='RecordDayNo', Occ.No='Occ_No', Occ.Name='Occ_Name')
  
  # Save as a csv file. ----------------------------------------------------
  write.csv(Sum_by_User_Day_Occ, 'dietstudy_Sum_by_User_Day_Occ.csv')
  # This will be useful if a researcher wants to look at the sum of each eating occasion 
  # per participant. (Because Totals file sums all the occasions in one day.)
  
  
# ========================================================================================
# Generate new totals file if any edits were made. 
# ======================================================================================== 
  # Use VVKAJ data. --------------------------------------------------------
  GenerateTotals(items.data=Items_raw, User.Name='UserName', Recall.No='RecallNo')
  
  # Use dietstudy data. ----------------------------------------------------
  GenerateTotals(items.data=items, User.Name='UserName', Recall.No='RecordDayNo')
  
  # The number of rows should be {No. of users x No. days}.
  dim(New_Totals)
  head(New_Totals, 2)
  
  # Save as a csv file. ----------------------------------------------------
  write.csv(New_Totals, 'dietstudy_New_Totals.csv')
  
    
# ========================================================================================
# Look for outliers in your totals. 
# ======================================================================================== 
# ---------------------------------------------------------------------------------------------------------------

    # Define your totals dataset if necessary.
      New_Totals <- totals # my imported totals

# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  KCALOutliers(totals.data = New_Totals, min = 600, max = 5700)

# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  PROTOutliers(totals.data = New_Totals, min = 10, max = 240)

# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  TFATOutliers(totals.data = New_Totals, min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  VCOutliers(totals.data = New_Totals, min = 5, max = 400)
  # or show the outliers if too many.
  VC_outlier_rows[, c('User_Day', 'KCAL', 'VC', 'V_TOTAL', 'V_DRKGR', 'F_TOTAL')]  # F is fruits.

# Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  BCAROutliers(totals.data = New_Totals, min = 15, max = 8200)
  # or show the outliers if too many.
  bcaroutliers <-  BCAR_outlier_rows[, c('User_Day', 'KCAL', 'BCAR')]
  bcaroutliers[order(bcaroutliers$BCAR, decreasing = T), ]

# Save as "Totals_QCed.csv"
  write.csv(New_Totals, 'Totals_QCed.csv')
# ---------------------------------------------------------------------------------------------------------------
  
