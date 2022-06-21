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

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")  
  source("lib/load_clean_ASA24.R")

# ========================================================================================
# Load data
# ========================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  

# Load your raw items data.
  items_raw <- read.csv("VVKAJ_2021-11-09_7963_Items.csv", sep = ",", header=T) 
  
# Save it as a .txt file for further processing.
  write.table(items_raw, "VVKAJ_2021-11-09_7963_Items.txt", sep="\t", row.names=F)
  
# Replace special characters with an underscore. Takes only .txt files as input. 
  format.file(filename = "VVKAJ_2021-11-09_7963_Items.txt",
              columns =  "Food_Description", 
              outdir =   "VVKAJ_2021-11-09_7963_Items_f.txt")  # _f stands for "formatted".  
  
# Load the formatted Items file.
  items_f <- read.table("VVKAJ_2021-11-09_7963_Items_f.txt", sep="\t", header=T)
  
# All special characters in items_f should have been replaced with an underscore.   
  head(items_f)

# ========================================================================================
# <Optional> Use individuals_to_remove.txt to filter out users marked as Remove = yes.  
# ========================================================================================  
# Load your metadata that has information about which UserName(s) to remove. 
  ind_to_rm <- read.delim("individuals_to_remove.txt", header=T)

  ind_to_rm
# Metadata for this purpose (ind_to_rm) has UserName and which one to be removed:
  #     UserName Remove
  # 1   VVKAJ101    yes   
  # 2   VVKAJ102    
  # 3   VVKAJ103       
  # 4   VVKAJ104       
  # 5   VVKAJ105    

# Show which has "yes" in the "Remove" column, and remove them. 
  subset(ind_to_rm, Remove == "yes")
  
# Data after QC is named as selected_data, and is saved as a text file with the specified name. 
# This assumes the user names are in UserName column, and will print which user(s) will be removed.   
  RemoveRows(data=items_f,  metadata.file=ind_to_rm, output.name="VVKAJ_2021-11-09_7963_Items_f_s.txt")
  # RemoveRows(data=totals, metadata.file=ind_to_rm, output.name="eg_data/VVKAJ101-105/selectedtotals.txt")
  
# Load the selected data for further processing.
  # totals <- read.table("selectedtotals.txt", header=T, sep="\t")
  items_f_s <- read.table("VVKAJ_2021-11-09_7963_Items_f_s.txt", header=T, sep="\t")
  
# ========================================================================================
# <Optional> Merge individuals' metadata to totals or items.   
# ========================================================================================    
  
# ind_metadata has the participants' gender, age, height, weight, BMI, and Waist.Circumference, etc.
# if desired, this individual-specific information can be added to items data.
  
# Load metadata 2
  # ind_metadata <- read.table("eg_data/dietstudy/dietstudy_metadata.txt", sep="\t", header=T)
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)
  
# Look at what the metadata has.
  head(ind_metadata)
  
# Add this metadata of each participant in totals or items.
# 'NA' will be inserted to UserNames which are not in ind_metadata.
  # totals <- merge(x=totals, y=ind_metadata, by="UserName", all.x=T)
  items_f_s_m <- merge(x=items_f_s, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the items data and metadata are merged.
  head(items_f_s_m)
  

# ========================================================================================
# Generate new totals file if any edits were made to the items file. Output is called "New_totals" 
# ======================================================================================== 
# The resultent 'new_totals' have columns of User_Day, UserName, RecallNo, FoodAmt--A_DRINKS.
# Some columns in the original totals will be lost... 

  # Use VVKAJ data. --------------------------------------------------------
  GenerateTotals(items.data=items_f_s_m, User.Name='UserName', Recall.No='RecallNo')
  
  # The number of rows should be {No. of users x No. days}.
  dim(New_Totals)
  head(New_Totals, 2)
  
  
######### JUNE 2022 REVISION GOING ON. RESUME FROM HERE. ########## 
    
# ========================================================================================
# Look for outliers in your totals. 
#  Note that input file (QCtotals) will be overwritten after outlier removal.
# ======================================================================================== 

# Define your totals dataset if necessary.
  # QCtotals <- totals      # imported totals
  QCtotals <- New_Totals  # totals newly generated above.

# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 600, max = 5700)

# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 10, max = 240)

# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 400)
    
      # or show the outliers if too many.
      VC_outlier_rows[, c('UserName', 'KCAL', 'VC', 'V_TOTAL', 'V_DRKGR', 'F_TOTAL')]  # F is fruits.

# Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
    QCOutliers(input.data = QCtotals, target.colname = "BCAR", min = 15, max = 8200)
    
      # or show the outliers if too many.
      bcaroutliers <- BCAR_outlier_rows[, c('UserName', 'KCAL', 'BCAR')]
      bcaroutliers[order(bcaroutliers$BCAR, decreasing = T), ]

# Save as "Totals_QCed.txt"
  write.table(QCtotals, "VVKAJ_2021-11-09_7963_Totals_QCed.txt", sep="\t", quote=F, row.names=F)
# ---------------------------------------------------------------------------------------------------------------

  
# Come back to the main directory if necessary.
  setwd(main.wd)
  