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

# Load example totals data  
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

# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Use metadata to filter 
# ========================================================================================  
# ---------------------------------------------------------------------------------------------------------------
  # Remove specified rows of 'totals' in metadata_1 (Metadata_1)
  
  # Show which has "yes" in the "Remove" column, and remove them.  
  RemoveRows(data = totals, metadata.file = metadata_1)
  # The resulting dataset, totals_selected, can be used for further analyses.
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
  # Use metadata_2
  # Create metadata_3 that has gender, age, height, weight, BMI, and Waist.Circumference, 
  # which did not change over the study days (i.e. only one data per person)
  
  # Take only the first row of each participant. 
  metadata_3 <- metadata_2[!duplicated(metadata_2$UserName), 
                           c("UserName", "Gender", "Age", "Weight", "Height", "BMI", "Waist.Circumference")] 
  head(metadata_3)
  
  # Add this metadata of each participant in totals. 
  totals_selected <- merge(x=totals_selected, y=metadata_3, by="UserName", all.x=T)
  
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Generate sum of foods consumed by each user, by day, and by occasion
# ======================================================================================== 
  # Function to do that...
  
  
  # Save as a csv file.
  write.csv(Sum_by_User_Day_Occ, 'Sum_by_User_Day_Occ.csv')
  # This will be useful if a researcher wants to look at the sum of each eating occasion 
  # per participant. (Because Totals file sums all the occasions in one day.)
  
# ========================================================================================
# Generate new totals file if any edits were made. 
# ======================================================================================== 
  # Save as a csv file.
  write.csv(New_Totals, 'New_Totals.csv')
  
  
    
# ========================================================================================
# Look for outliers in your totals. 
# ======================================================================================== 
# ---------------------------------------------------------------------------------------------------------------
# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  KCALOutliers(data = New_Totals, min = 600, max = 5700)
  KCALOutliers(data = totals,     min = 600, max = 5700)

# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  PROTOutliers(data = New_Totals, min = 10, max = 240)
  PROTOutliers(data = totals,     min = 10, max = 240)  

# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  TFATOutliers(data = New_Totals, min = 15, max = 230)
  TFATOutliers(data = totals,     min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  VCOutliers(data = New_Totals, min = 5, max = 400)
  VCOutliers(data = totals,     min = 5, max = 400)  

# Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  BCAROutliers(data = New_Totals, min = 15, max = 8200)
  BCAROutliers(data = totals,     min = 15, max = 8200)  

# Save as "Totals_QCed.csv"
  write.csv(New_Totals, 'Totals_QCed.csv')
# ---------------------------------------------------------------------------------------------------------------
  
