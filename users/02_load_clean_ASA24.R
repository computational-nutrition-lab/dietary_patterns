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

  # Come back to the main directory
    setwd(main.wd)  

# Use VVKAJ data ---------------------------------------------------------------
  # Specify the directory where the data is.
    SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  
  
  # Load your raw items data.
    items_raw <- read.csv("VVKAJ_2021-11-09_7963_Items.csv", sep = ",", header=T)
    
    # Replace commas with space in Food_description column for further analysis.
      items_raw$Food_Description <- gsub(pattern=', ', replacement='_', x=items_raw$Food_Description)
      
    # Save the items with no commas to use in other analyses.
      write.table(items_raw, "VVKAJ_2021-11-09_7963_Items_no_commas.txt", sep="\t", row.names=F)
      
    # Load the items file as items.
      items <- read.table("VVKAJ_2021-11-09_7963_Items_no_commas.txt", sep="\t", header=T)
 
  # Load your totals data if ready to go.
    totals <- read.csv("VVKAJ_2021-11-09_7963_Totals.csv", sep = ",", header=T)
    
  # Come back to the main directory
    setwd(main.wd)
  
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# <Optional> Use individuals_to_remove.txt to filter out users marked as Remove = yes.  
# ========================================================================================  
# Load your metadata that has information about which UserName(s) to remove. 
  ind_to_rm <- read.delim("eg_data/VVKAJ101-105/individuals_to_remove.txt", header=T)
  
# Metadata for this purpose (ind_to_rm) should look like this:
    #     UserName Remove
    # 1   VVKAJ101    yes   
    # 2   VVKAJ102    
    # 3   VVKAJ103       
    # 4   VVKAJ104       
    # 5   VVKAJ105    yes
    # 6   VVKAJ106       
    # ... ...       ...
  
  # Show which has "yes" in the "Remove" column, and remove them. 
  subset(ind_to_rm, Remove == "yes")
      
  # Data after QC is named as selected_data, and is saved as a text file with the specified name. 
  RemoveRows(data=totals, metadata.file=ind_to_rm, output.name="eg_data/VVKAJ101-105/selectedtotals.txt")
  RemoveRows(data=items,  metadata.file=ind_to_rm, output.name="eg_data/VVKAJ101-105/selecteditems.txt")
  
  # Load these selected data for further QC.
  totals <- read.table("eg_data/VVKAJ101-105/selectedtotals.txt", header=T, sep="\t")
  items <- read.delim("eg_data/VVKAJ101-105/selecteditems.txt", header=T, sep="\t")

# ========================================================================================
# <Optional> Merge individuals' metadata to totals or items.   
# ========================================================================================    

# Use ind_metadata with the participants' gender, age, height, weight, BMI, and Waist.Circumference, etc.
  
  # Load metadata 2
  ind_metadata <- read.table("eg_data/dietstudy/dietstudy_metadata.txt", sep="\t", header=T)
  ind_metadata <- read.table("eg_data/VVKAJ101-105/ind_metadata.txt", sep="\t", header=T)
  head(ind_metadata)
  
  # Add this metadata of each participant in totals or items.
  # 'NA' will be inserted to UserNames which are not in ind_metadata.
  totals <- merge(x=totals, y=ind_metadata, by="UserName", all.x=T)
  items <- merge(x=items, y=ind_metadata, by="UserName", all.x=T)
 
# ========================================================================================
# Generate new totals file if any edits were made to the items file. Output is called "New_totals" 
# ======================================================================================== 
# The resultent 'new_totals' have columns of User_Day, UserName, RecallNo, FoodAmt--A_DRINKS.
# Some columns in the original totals will be lost... 

  # Use VVKAJ data. --------------------------------------------------------
  GenerateTotals(items.data=items, User.Name='UserName', Recall.No='RecallNo')
  
  # Use dietstudy data. ----------------------------------------------------
  GenerateTotals(items.data=items, User.Name='UserName', Recall.No='RecordDayNo')
  
  # The number of rows should be {No. of users x No. days}.
  dim(New_Totals)
  head(New_Totals, 2)
    
# ========================================================================================
# Look for outliers in your totals. 
#  Note that input file (QCtotals) will be overwritten after outlier removal.
# ======================================================================================== 

     # Define your totals dataset if necessary.
      QCtotals <- totals      # imported totals
      QCtotals <- New_Totals  # my newly generated totals

# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  KCALOutliers(totals.data = QCtotals, min = 600, max = 5700)

# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  PROTOutliers(totals.data = QCtotals, min = 10, max = 240)

# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  TFATOutliers(totals.data = QCtotals, min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  VCOutliers(totals.data = QCtotals, min = 5, max = 400)
    # or show the outliers if too many.
    VC_outlier_rows[, c('UserName', 'KCAL', 'VC', 'V_TOTAL', 'V_DRKGR', 'F_TOTAL')]  # F is fruits.

# Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  BCAROutliers(totals.data = QCtotals, min = 15, max = 8200)
    # or show the outliers if too many.
    bcaroutliers <- BCAR_outlier_rows[, c('UserName', 'KCAL', 'BCAR')]
    bcaroutliers[order(bcaroutliers$BCAR, decreasing = T), ]

# Save as "Totals_QCed.txt"
  write.table(QCtotals, "eg_data/VVKAJ101-105/VVKAJ_2021-11-09_7963_Totals_QCed.txt", sep="\t", quote=F, row.names=F)
# ---------------------------------------------------------------------------------------------------------------

  
  