# For USERS ==============================================================================

# ========================================================================================
# Load and clean ASA24 data.
# Version 1
# Created on 02/04/2022 by Rie Sadohara
# ========================================================================================

# Use Metadata 1 to filter out individuals. 
# Remove users that has only a small number of totals (days of record). - if you know which one to remove.  
# Look for outliers in your totals by nutrient consumed on each day. 

# Caclulate totals by occasion. - extra dataset. 

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
    
  # Load the items.csv
    items <- read.table("Items_to_use.txt", quote = "", sep = "\t", header = T)
  
  # Load the totals.csv
    totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)
    

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
# Use metadata to filter out users marked as Remove = yes in metadata_1.  
# ========================================================================================  
# ---------------------------------------------------------------------------------------------------------------
# Load your metadata that has information about which UserName(s) to remove. 
  metadata_1 <- read.delim("eg_data/VVKAJ101-105/VVKAJ_metadata_1.txt", header=T)
  
# Metadata for this purpose (Metadata_1) should look like this:
    #     UserName Remove
    # 1   MCTs01       
    # 2   MCTs02    yes
    # 3   MCTs03       
    # 4   MCTs04       
    # 5   MCTs05    yes
    # 6   MCTs06       
    # ... ...       ...
  
  # Show which has "yes" in the "Remove" column, and remove them. 
  # Data after QC is named as selected_data, and is saved as a text file with the specified name. 
  RemoveRows(data=totals, metadata.file=metadata_1, output.name="eg_data/VVKAJ101-105/selectedtotals.txt")
  RemoveRows(data=Items_raw,  metadata.file=metadata_1, output.name="eg_data/VVKAJ101-105/selecteditems.txt")
  
  # Load these selected data for further QC.
  totals_selected1 <- read.table("eg_data/VVKAJ101-105/selectedtotals.txt", header=T, sep="\t")
  items_selected1 <- read.delim("eg_data/VVKAJ101-105/selecteditems.txt", header=T, sep="\t")
    
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
  # Use metadata_2 with the participants' gender, age, height, weight, BMI, and Waist.Circumference, etc.
  # Create a unique list of participants that has their gender, age, height, weight, BMI, and Waist.Circumference, 
  # which did not change over the study days (i.e. only one data per person)
  
  # Take only the first row of each participant (de-duplicate). 
  metadata_2_a <- metadata_2[!duplicated(metadata_2$UserName), 
                           c("UserName", "Gender", "Age", "Weight", "Height", "BMI", "Waist.Circumference")] 
  head(metadata_2_a)
  
  # Add this metadata of each participant in totals. 'NA' will be inserted to UserName which is not in metadata_2_a.
  totals_selected2 <- merge(x=totals, y=metadata_2_a, by="UserName", all.x=T)
  
  # A
  
  
  table(totals_selected$UserName)
  table(totals$UserName)
  table(metadata_2$UserName)
  colnames(totals_selected)
  head(totals_selected[, c(1, 100, 112)], 20)
  
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Generate new totals file if any edits were made to the items file. Output is called "New_totals" 
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

# ========================================================================================
# Totals_QCed.csv
# ========================================================================================   

  
  
  