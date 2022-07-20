# For USERS ==============================================================================

# ========================================================================================
# Load and clean ASA24 data.
# Version 1
# Created on 02/04/2022 by Rie Sadohara
# ========================================================================================

# Use Metadata 1 to filter out individuals. 
# Remove users that has only a small number of totals (days of record). - if you know which one to remove.  
# Look for outliers in your totals by nutrient consumed on each day. 

# Calculate totals by occasion. - extra dataset. 

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")  
  source("lib/load_clean_ASA24.R")
  source("lib/format.file.R")
  
# You can come back to the main directory by:
  setwd(main_wd)

# ========================================================================================
# Load ASA24 data
# ========================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")  

# Load your raw items data.
  items_raw <- read.csv("VVKAJ_Items.csv", sep = ",", header=T) 
  
# Save it as a .txt file for further processing.
  write.table(items_raw, "VVKAJ_Items.txt", sep="\t", row.names=F)
  
# Special characters such as "'", ",", "%" may interfere correct data loading; thus,
# we replace them with an underscore "_".  Takes only .txt files as input. 
# Specify column(s) to be processed in the "columns" argument.
  format.file(filename = "VVKAJ_Items.txt",
              columns  = "Food_Description", 
              outfn    = "VVKAJ_Items_f.txt")  # _f stands for "formatted".  
  
# Load the formatted Items file.
  items_f <- read.table("VVKAJ_Items_f.txt", sep="\t", header=T)
  
# All special characters in items_f should have been replaced with an underscore.   
  head(items_f)

# Ensure your items file has the expected dimensions (number of rows x number of columns,
# shown as number of obs. and number of variables) in the environment window of R Studio.
  

# ========================================================================================
# <Optional> Use individuals_to_remove.txt to filter out users marked as Remove = yes.  
# ========================================================================================  
# Load your metadata that has information about which UserName(s) to remove. 
  ind_to_rm <- read.table("individuals_to_remove.txt", sep="\t", header=T)

  ind_to_rm
  # Metadata for this purpose (ind_to_rm) has UserName and which one to be removed:
  #     UserName Remove
  # 1   VVKAJ101       
  # 2   VVKAJ102    
  # ... ...        
  # ... ...        
  # 16  VVKAJ116   yes 

# Show which has "yes" in the "Remove" column, and remove them. 
  subset(ind_to_rm, Remove == "yes")

# Remove the specified individuals.  
# The output will be saved as a text file with the specified name. 
# This assumes the usernames are in UserName column, and will print which user(s) will be removed.   
  RemoveRows(data=items_f,  metadata.file= ind_to_rm, output.name= "VVKAJ_Items_f_s.txt")
  
# Load the output for further processing.
  items_f_s <- read.table("VVKAJ_Items_f_s.txt", header=T, sep="\t")
  
# Show unique usernames in items_f_s and confirm "VVKAJ101" has been removed.
  unique(items_f_s$UserName)  
  
# ========================================================================================
# <Optional> Merge individuals' metadata to items.   
# ========================================================================================    
  
# ind_metadata has the participants' gender, age, height, weight, BMI, and Waist.Circumference, etc.
# If desired, this individual-specific information can be added to items data.
  
# Load ind_metadata.txt.
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)
  
# Look at what the metadata has.
  head(ind_metadata)
  
# Add this metadata of each participant in totals or items.
# 'NA' will be inserted to UserNames which are not in ind_metadata.
  items_f_s_m <- merge(x=items_f_s, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the items data and metadata are merged.
  head(items_f_s_m)
  
# Save the merged dataframe as a .txt file.
  write.table(items_f_s_m, "VVKAJ_Items_f_s_m.txt", sep="\t", row.names=F, quote=F)

  
# ========================================================================================
# Generate new totals file if any edits were made to the items file. 
# ======================================================================================== 

# Use one of the input files saved above as an input for calculating totals for.
# Specify which columns have usernames and Recall.No., which is the number of recorded days. 
  GenerateTotals(inputfn = "VVKAJ_Items_f_s_m.txt", 
                 User.Name = 'UserName', 
                 Recall.No = 'RecallNo',
                 outfn = "VVKAJ_Tot.txt")

# Load the total file generated above.
  new_totals <- read.table("VVKAJ_Tot.txt", header=T, sep="\t")

# The number of rows should be {No. of users x No. days}.
# In this case, 15 users x 3 days = 45 rows (observations).
  nrow(new_totals) 

# View the new_total
  head(new_totals)

# ========================================================================================
# <Optional> Add the participants' metadata back to totals.
# ========================================================================================

# Load ind_metadata.txt if you have not done so.
  ind_metadata <- read.table("ind_metadata.txt", sep="\t", header=T)

# Add this metadata of each participant to totals.
# 'NA' will be inserted to UserNames which are not in ind_metadata. --> Checked. Really works.
  new_totals_m <- merge(x=new_totals, y=ind_metadata, by="UserName", all.x=T)
  
# Check that the items data and metadata are merged.
  head(new_totals_m)
# Here, the first three rows have the same metadata because they are the day 1, 2, and 3 
# dietary data of the same participant, VVKAJ102.
  
# Save the merged dataframe as a .txt file.
  write.table(new_totals_m, "VVKAJ_Tot_m.txt", sep="\t", row.names=F, quote=F)


# ========================================================================================
# QC totals data
# ======================================================================================== 
# Look for outliers in your totals. 
# Note that input object (QCtotals) will be overwritten after outlier removal.
  
# Load your totals if necessary - to be used as input for QC.
  new_totals <- read.table("VVKAJ_Tot_m.txt", sep="\t", header=T)
    
# Define your totals dataset to be used as input.
  QCtotals <- new_totals  

# Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "KCAL", min = 600, max = 5700)

# Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "PROT", min = 10, max = 240)

# Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "TFAT", min = 15, max = 230)

# Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "VC", min = 5, max = 400)
    
      # You may find numerous potential outliers here. Then, click "No", and view those 
      # outliers with their other nutrient intake information by running the following;
      VC_outliers <- subset(new_totals, VC < 5 | VC > 400)    
      # sort in the order of VC and show only the specified variables.
      VC_outliers[order(VC_outliers$BCAR, decreasing = T),
                  c('UserName', 'KCAL', 'VC', 'V_TOTAL', 'V_DRKGR', 'F_TOTAL')]  # F is fruits.

# Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, target.colname = "BCAR", min = 15, max = 8200)
    
      # You may find numerous potential outliers here. Then, click "No", and view those 
      # outliers with their other nutrient intake information by running the following;
      BCAR_outliers <- subset(new_totals, BCAR < 15 | BCAR > 8200)
      # sort in the order of BCAR and show only the specified variables.
      BCAR_outliers[order(BCAR_outliers$BCAR, decreasing = T), c('UserName', 'KCAL', 'BCAR')] 

# Save as "Totals_QCed.txt"
  write.table(QCtotals, "VVKAJ_Tot_m_QCed.txt", sep="\t", quote=F, row.names=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Come back to the main directory before you start running another script.
  setwd(main_wd)
  