# ===============================================================================================================
# Load NHANES 2015-16 FOOD data, add food description, QC, and calculate total. 
# Version 2
# Created on 05/18/2022 by Rie Sadohara and Suzie Hoops
# ===============================================================================================================

# Folder structure 
# 
#                          |----- eg_data -- NHANES -- Data -- Food Items and Totals files 
#                          |
#                          |----- lib
#                          |
#                          |----- users -- where this script is located
#  Main -------------------|
#  (dietary_patterns)      |----- results -- PCA_results -- where the results will be saved
#                          |
#                          |----- ...
#

# First time only: install the packages you need.
  # install.packages("SASxport")
  # install.packages("foreign")

# ===============================================================================================================
# Load "food items" data and add food descriptions
# ===============================================================================================================

# Load necessary packages.
  library(SASxport)

# Set where the NHANES data and food code table are.
# it is not in the eg_data folder because it's too large to save in GitHub folder. 
  # setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")
  source("lib/Food_tree_scripts/format.foods.r")

# ---------------------------------------------------------------------------------------------------------------
# Prep the code table - replace special characters with "_" or "and"
  
  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "eg_data/NHANES/FoodCodes_DRXFCD_I.XPT", 
                       out.fn =              "eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted foodcode table.
  foodcodetable_f <- read.table("eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)
  foodcodetable_f[1:10, ]  
  
     # foodcodetable_f_byhand <-  foodcodetable_f
     # foodcodetable_f_byhand[, "DRXFCSD"] <- gsub("%", "_", foodcodetable_f_byhand[, "DRXFCSD"])
     # foodcodetable_f_byhand[1:10, ]  

# ---------------------------------------------------------------------------------------------------------------
# Load FPED15-16, needed for the AddFoodCat function. 
  FPED <- read.table("eg_data/NHANES/FPED/FPED_1516_forR.txt", sep="\t", header=T)
  head(FPED, 2)
  colnames(FPED)[1] <- "Food_code" # Important! Change the food code column name as Food_code.

# ---------------------------------------------------------------------------------------------------------------
# Import items data Day 1, add food item descriptions, and save it as a txt file.
# LIKELY IT WILL BE A HUGE FILE.
  ImportNHANESFoodItems(data.name="E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1IFF_I.XPT", 
                        food.code.column = "DR1IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/DR1IFF_I_d.txt") # 'd' stands for food descriptions

# Load the saved food items file.
  Food_D1 <- read.table("eg_data/NHANES/DR1IFF_I_d.txt", sep="\t", header=T)
  dim(Food_D1)
  head(Food_D1, 1)
  length(unique(Food_D1$SEQN)) #8505 people

# Add the food category info and serving for each item. #### WILL TAKE A FEW MOMENTS. ####
  AddFoodCat(input.food= Food_D1,
             fped= FPED,
             grams= "DR1IGRMS", 
             out.fn= "eg_data/NHANES/Food_D1_FC.txt")
  # OK to see a message saying "NAs introduced by coercion."
  # NAs will be removed later in the filtering process.

# ---------------------------------------------------------------------------------------------------------------
# Import items data Day 2, add food item descriptions, and save it as a txt file.
  ImportNHANESFoodItems(data.name="E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR2IFF_I.XPT",
                        food.code.column = "DR2IFDCD",
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/DR2IFF_I_d.txt")

# Add food item description and save it as a txt file.
  Food_D2 <- read.table("eg_data/NHANES/DR2IFF_I_d.txt", sep="\t", header=T)

  dim(Food_D2)
  length(unique(Food_D2$SEQN)) #7027 people

# Day 2. Add the food items info and serving for each item. #### WILL TAKE A FEW MOMENTS. ####
  AddFoodCat(input.food= Food_D2, 
             fped= FPED, 
             grams= "DR2IGRMS", 
             out.fn= "eg_data/NHANES/Food_D2_FC.txt")
  # OK to see a message saying "NAs introduced by coercion."
  # NAs will be removed later in the filtering process.

# ===============================================================================================================
# Load the Food_Dx_FC which has food category data. 
# ===============================================================================================================
  # Food Day 1 with Food Category *** WILL BE A HUGE TABLE. ***
  Food_D1_FC <- read.table("eg_data/NHANES/Food_D1_FC.txt", sep="\t", header=T)
  dim(Food_D1_FC)
  length(unique(Food_D1_FC$SEQN))
  head(Food_D1_FC,2)
  
  # Food Day 2 with Food Category *** WILL BE A HUGE TABLE. ***
  Food_D2_FC <- read.table("eg_data/NHANES/Food_D2_FC.txt", sep="\t", header=T)
  dim(Food_D2_FC)
  colnames(Food_D2_FC)
  tail(Food_D2_FC)
  table(Food_D2_FC$DR2DRSTZ) # 1902 rows are incomplete.


# Change the colnames for downstream analyses
  colnames(Food_D1_FC)
  # names(food1)[names(food1) == "dr1ifdcd"] <- "FoodCode"
  names(Food_D1_FC)[names(Food_D1_FC) == "DR1IFDCD"] <- "FoodCode"
  names(Food_D2_FC)[names(Food_D2_FC) == "DR2IFDCD"] <- "FoodCode"
  # names(food1)[names(food1) == "DR1MC"] <- "ModCode"  # No such column.
  # names(food2)[names(food2) == "DR2MC"] <- "ModCode"
  names(Food_D1_FC)[names(Food_D1_FC) == "DR1IGRMS"] <- "FoodAmt"
  names(Food_D2_FC)[names(Food_D2_FC) == "DR2IGRMS"] <- "FoodAmt"
  names(Food_D1_FC)[names(Food_D1_FC) == "DRXFCLD"] <- "Main.food.description"
  names(Food_D2_FC)[names(Food_D2_FC) == "DRXFCLD"] <- "Main.food.description"
  
  head(Food_D1_FC,1)
  
# Save after changing the columnnames. cc stands for columnnames changed.
  write.table(Food_D1_FC, "eg_data/NHANES/Food_D1_FC_cc.txt", sep="\t", row.names=F, quote=F)
  write.table(Food_D2_FC, "eg_data/NHANES/Food_D2_FC_cc.txt", sep="\t", row.names=F, quote=F)
  
# Replace the special characters with "_" using FormatFoods
# FotmatFoods() function adds "Main.Food.Description" where special characters are removed/replaced, the previous
# Main.Food.Description as Old.Main.Food.Description, ModCode, and FoodID. $FoodID is a cha vector, but has .0 at the end. 
  # MAKE SURE dedupe=F. If true (default!), duplicated foods will be removed! 
  FormatFoods(input_fn="eg_data/NHANES/Food_D1_FC_cc.txt", output_fn= "eg_data/NHANES/Food_D1_FC_cc_f.txt", dedupe=F)
  FormatFoods(input_fn="eg_data/NHANES/Food_D2_FC_cc.txt", output_fn= "eg_data/NHANES/Food_D2_FC_cc_f.txt", dedupe=F)
  
  length(Food_D1_FC$Food_code)
  head(Food_D1_FC$Food_code)
  
  # Result
  Food_D1_FC_cc_f <- read.table("eg_data/NHANES/Food_D1_FC_cc_f.txt", sep="\t", header=T)
  
  is(Food_D1_FC_cc_f$FoodID)
  head(Food_D1_FC_cc_f$FoodID)
  colnames(Food_D1_FC_cc_f)
  Food_D1_FC_cc_f[1:10, c(89:90, 128:130)]
  Food_D1_FC_cc_f[100:120, c(89:90, 128:130)]
  length(unique(Food_D1_FC_cc_f$SEQN))
  
  Food_D2_FC_cc_f <- read.table("eg_data/NHANES/Food_D2_FC_cc_f.txt", sep="\t", header=T)
  head(Food_D2_FC_cc_f, 1)
  length(unique(Food_D2_FC_cc_f$SEQN))
  # OK
  # Food_code and foodcode are the same though 'identical()' says they are not...
  
  # Use these resultant objects for the following procedures.
  # Food_D1_FC_cc_f
  # Food_D2_FC_cc_f 

  
# ===============================================================================================================
# QC the food data: filter by age, completeness, >1 food item reported/day, data exists on both days. 
# ===============================================================================================================

# Remove children.  
  # Load the demographics file, then filter by age > 18.
  demog <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  adults <- demog[demog$RIDAGEYR >= 18, ]
    length(unique(adults$SEQN)) # 5992 adults
  
# Retain those with complete data (STZ==1)
  # DR1DRSTZ == 1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  # Code descriptions in Analytic notes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
  food1 <- subset(Food_D1_FC_cc_f, DR1DRSTZ == 1)
  food2 <- subset(Food_D2_FC_cc_f, DR2DRSTZ == 1)
  head(food1)

# Create a vector of SEQN of those who reported data for both days and are adults.   
  food1names <- unique(food1$SEQN) # 8326 adults
  food2names <- unique(food2$SEQN) # 6875 adults
  keepnames <- food1names[food1names %in% food2names]  # 6863
  keepnames_adults <- keepnames[keepnames %in% adults$SEQN] # 4405

# Keep those who reported more than 1 food item per day.
  freqtable1 <- as.data.frame(table(food1$SEQN))
  freqtable1_m <- freqtable1[freqtable1$Freq > 1, ]
  colnames(freqtable1_m)[1] <- "SEQN"
  keepnames_adults_mult1 <- keepnames_adults[keepnames_adults %in% freqtable1_m$SEQN] # 4405
  # Take only the participants whose names are in keepnames_adults_mult1.
  food1b <- food1[food1$SEQN %in% keepnames_adults_mult1, ] # 66,304 rows
  
  # Do the same for food2
  freqtable2 <- as.data.frame(table(food2$SEQN))
  freqtable2_m <- freqtable2[freqtable2$Freq > 1, ]
  colnames(freqtable2_m)[1] <- "SEQN"
  keepnames_adults_mult2 <- keepnames_adults[keepnames_adults %in% freqtable2_m$SEQN] # 4401
  food2b <- food2[food2$SEQN %in% keepnames_adults_mult2, ] # 66,690 rows
  head(food2b)

# Create a vector of SEQN of those that have both day 1 and day 2 data.
  food1bnames <- unique(food1b$SEQN)
  food2bnames <- unique(food2b$SEQN)
  keepnames12 <- food1bnames[food1bnames %in% food2bnames] # 4,401 people.


# ================================================================================================================  
# Now, choose Scenario A or B. (B consists of B-1, B-2, ...)
# ================================================================================================================  
  
# ================================================================================================================  
# Scenario A: Further processing of food1b and food2b for building a food tree.  
# ================================================================================================================  
  
# Make a day variable to distinguish them.
  food1b$Day <- 1
  food2b$Day <- 2

  colnames(food1b)

# Rename these...
  food1e <- food1b
  food2e <- food2b

# Remove "^DR1I", "^DR1" from the columnnames 
  colnames(food1e) <- gsub(colnames(food1e), pattern = "^DR1I", replacement = "")
  colnames(food1e) <- gsub(colnames(food1e), pattern = "^DR1",  replacement = "")
  colnames(food1e)
  colnames(food2e) <- gsub(colnames(food2e), pattern = "^DR2I", replacement = "")
  colnames(food2e) <- gsub(colnames(food2e), pattern = "^DR2",  replacement = "")
  colnames(food2e)
      
# Ensure the columns of food1c and food2c match before joining them.
  identical(colnames(food1e), colnames(food2e))
  
# Combine food1 and food2 as a longtable.
  food12e <- rbind(food1e, food2e)
  
# Pick up only the individuals listed in keepnames12.
  food12f <- food12e[food12e$SEQN %in% keepnames12, ]
  
    # Some checking -- to be removed.  
    length(unique(food12f$SEQN))
    sum(table(food12f$SEQN, food12f$Day)[,1] <2) # how many has <2 food entries? Should be zero. 
    sum(table(food12f$SEQN, food12f$Day)[,2] <2) # how many has <2 food entries? Should be zero.
    hhh = as.data.frame(table(food12f$SEQN, food12f$Day))
    head(hhh, 10)
    table(hhh$Var2)
    head(hhh[order(hhh$Freq), ], 10)
    tail(hhh[order(hhh$Freq), ], 10)
    subset(food12f, SEQN==86563)[, 'Day'] # Participant No. 86563 reported only 1 food/day. Should be nonexistent.
  
  
# food12f has all information (SEQN, nutrients, food categories, food.description, day etc.)
# Save. It will be a HUGE file.
  write.table(food12f, "eg_data/NHANES/Food_D12_FC_cc_f.txt", sep="\t", row.names=F, quote=F)

  # Use this as an input for food tree.  
  
# ================================================================================================================  
# Scenario B-1: Further processing of food1b and food2b for calculating totals and clustering.  
# ================================================================================================================  

# Copy to avoid overwriting
  food1bb <- food1b
  food2bb <- food2b
  
# Change "FoodAmt" back to "DR1GRMS" to be consistent with the variable names in dayXvariables
  names(food1bb)[names(food1bb) == "FoodAmt"] <- "DR1IGRMS"
  names(food2bb)[names(food2bb) == "FoodAmt"] <- "DR2IGRMS"

# Combine day 1 and day 2 data.
# Day 1
  # Import the list of variables to be picked up in Day 1. 
  # day1variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_Day1.txt', header=F)  # OLD, before adding food category data.
  day1variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_FC_Day1.txt', header=F)
  tail(day1variables)
  # Which variables to pick up from the food data
  var_to_use1 <- names(food1bb) %in% day1variables$V1
  # pick up only the specified variables
  food1c <- food1bb[, var_to_use1]
  
  # Remove "DR1T", "DR1" from the column names 
  colnames(food1c) <- gsub(colnames(food1c), pattern = "^DR1I", replacement = "")
  colnames(food1c) <- gsub(colnames(food1c), pattern = "^DR1",  replacement = "")
  # Check
  head(food1c, 1)
 
# Do the same for Day 2  
  # day2variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_Day2.txt', header=F) # OLD, before adding food category data.
  day2variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_FC_Day2.txt', header=F)
  var_to_use2 <- names(food2bb) %in% day2variables$V1
  food2c <- food2bb[, var_to_use2]
  colnames(food2c) <- gsub(colnames(food2c), pattern = "^DR2I", replacement = "")
  colnames(food2c) <- gsub(colnames(food2c), pattern = "^DR2", replacement = "")
  head(food2c, 1)
  
# Make a day variable before combining
  food1c$Day <- 1
  food2c$Day <- 2

# Ensure the columns of food1c and food2c match before joining them.
  identical(colnames(food1c), colnames(food2c))
  
# Combine food1 and food2 as a longtable.
  food12c <- rbind(food1c, food2c)
  
# Pick up only the individuals listed in keepnames12.
  food12d <- food12c[food12c$SEQN %in% keepnames12, ]

    # Some checking -- to be removed.  
      length(unique(food12d$SEQN))
      sum(table(food12d$SEQN, food12d$Day)[,1] <2) # how many has <2 food entries? Should be zero. 
      sum(table(food12d$SEQN, food12d$Day)[,2] <2) # how many has <2 food entries? Should be zero.
      hhh = as.data.frame(table(food12d$SEQN, food12d$Day))
      head(hhh, 10)
      table(hhh$Var2)
      head(hhh[order(hhh$Freq), ], 10)
      tail(hhh[order(hhh$Freq), ], 10)
      subset(food12d, SEQN==86563)[, 'Day'] # Participant No. 86563 reported only 1 food/day. Should be nonexistent.

# Save the combined and QCed food items as a .txt file. #### THIS WILL BE A HUGE FILE ####
  write.table(food12d, "eg_data/NHANES/NHANES1516_items_d12_FC_QC.txt", sep="\t", quote=F, row.names=F)  
  head(food12d)
      
# Load food12d.
  food12d <- read.table("eg_data/NHANES/NHANES1516_items_d12_FC_QC.txt", sep="\t", header=T)
      
# ---------------------------------------------------------------------------------------------------------------
# You may also want to consider special diets that some participants are following: e.g. DASH diet, diabetic diet, etc.
# Depending on your research question, you may want to exclude those following special diets.
# The diet information is found in totals day 1.  

# ===============================================================================================================
# B-2: Calculate totals/day/participant with the food data of the selected SEQNs.
# ===============================================================================================================

# Calculate totals for day 1 and day 2, and combine the two datasets.
  TotalNHANES(food12d= food12d, 
              first.val= "GRMS", last.val= "A_DRINKS", 
              outfn = "eg_data/NHANES/NHANES1516_total_d12_FC.txt" )  

# Load the resultant total.
  total12d <- read.table("eg_data/NHANES/NHANES1516_total_d12_FC.txt", sep="\t", header=T)
  
# total12d has the sum of each variable (columns) for each day and participant. 
  head(total12d)
  
# ------- BY HAND ------------------------------ 
# # Calculate total for day 1. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Take only the Day 1 data
#   food12d_d1 <- subset(food12d, Day==1) 
#       
# # Sum nutrients and food categories; this will be total data calculated by hand.
# # First, specify the first and the last column (variable) names to calculate totals for. 
#   first.val <- "GRMS"
#   last.val <- "A_DRINKS"
#   
#   start_col_num <- match(first.val, names(food12d_d1))  # The number of column that matches the first variable specified.
#   end_col_num <-   match(last.val, names(food12d_d1)) # The number of column that matches the last variable specified.
#     
#   # Sum food items by SEQN from start through end columns.
#   total1 <- aggregate(food12d_d1[, start_col_num:end_col_num], 
#                       by=list(food12d_d1$SEQN), 
#                       FUN=sum)
#   
#   total1$Day <- 1
#   colnames(total1)[1] <- "SEQN"
# 
# # Create a vector of number of food items reported by each participant.
#   n_items1 <- as.data.frame(table(food12d_d1$SEQN))
#   colnames(n_items1) <- c("SEQN", "NoOfItems")
#   
#   # Add it to total1
#   total1b <- merge(x=total1, y=n_items1, by="SEQN", all.x=T)
#   
#     # Some checking
#     subset(total1b, NoOfItems<2) # should be zero.
#     # Look for any missing data
#     total1b[is.na(total1b$NoOfItems), ]
# 
# # Calculate total for day 2. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   food12d_d2 <- subset(food12d, Day==2) 
#   
#   # Sum nutrients.  
#   # First, speicify the first and the last column (variable) names to calculate totals for. 
#   first.val <- "GRMS"
#   last.val <- "A_DRINKS"
#   
#   start_col_num <- match(first.val, names(food12d_d2))  # The number of column that matches the first variable specified.
#   end_col_num <-   match(last.val, names(food12d_d2)) # The number of column that matches the last variable specified.
#   
#   # Sum food items by SEQN from start through end columns.
#   total2 <- aggregate(food12d_d2[, start_col_num:end_col_num], 
#                       by=list(food12d_d2$SEQN), 
#                       FUN=sum)
#   
#   total2$Day <- 2
#   colnames(total2)[1] <- "SEQN"
#   
#   # Create a vector of number of food items reported by each participant.
#   n_items2 <- as.data.frame(table(food12d_d2$SEQN))
#   colnames(n_items2) <- c("SEQN", "NoOfItems")
#   # Add it to total1
#   total2b <- merge(x=total2, y=n_items2, by="SEQN", all.x=T)
#   
#      # Some checking
#      subset(total2b, NoOfItems<2) # should be zero.
#      # Look for any missing data
#      total2b[is.na(total2b$NoOfItems), ]
#       
# # Merge totals day 1 and day 2
#   # Check if all the columnnames match.
#   identical(colnames(total1b), colnames(total2b))
#   
# # Merge
#   total12c <- rbind(total1b, total2b)
#   
# # Save the calculated totals of day 1 and 2 as a txt file.
#   write.table(total12c, "eg_data/NHANES/NHANES1516_total_d12_FC.txt", sep="\t", row.names=F, quote=F)

# ===============================================================================================================
# B-3: Calculate the mean of totals/participant. 
# ===============================================================================================================

# Calculate the mean of two days of the totals data per participant. 
  AverageTotalNHANES(food12d= food12d, 
                     first.val= "GRMS", last.val= "NoOfItems", 
                     outfn= "eg_data/NHANES/NHANES1516_total_d12_FC_mean.txt")  
  
# Load the mean total
  meantotal12b <- read.table("eg_data/NHANES/NHANES1516_total_d12_FC_mean.txt", sep="\t", header=T)

    
# ===============================================================================================================
# B-4: QC the mean total in the same way as ASA24. 
# ===============================================================================================================
  
# For individual food data, there is no code for cleaning.
# Outliers won't severely affect main analysis conclusions (ASA24 data cleaning doc)
# But, it's always a good idea to take a look at the distributions of variables of interest. 
# Could calculate totals by occasion, similar to ASA24 code.
  
# ---------------------------------------------------------------------------------------------------------------
# For totals, the same QC can be applied as ASA24 totals QC procedure.
  # Functions to clean ASA24 data.
  source("lib/load_clean_ASA24.R")
  
# Run all these QC steps in this order.  When asked, choose to remove the outliers
# that fall outside the specified range for each nutrient.

# Define the input data.  This dataframe will be modified after each filter.
  QCtotals <- meantotal12b
  
  # Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "KCAL", min = 600, max = 5700)
  
  # Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "PROT", min = 10, max = 240)
  
  # Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "TFAT", min = 15, max = 230)

  # Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "VC", min = 5, max = 400)
  
      # or show the outliers if too many.
      VCoutliers <- Outlier_rows[, c('SEQN', 'KCAL', 'VC')]
      # Show the first n rows of the outliers in a descending order. 
      head(VCoutliers[order(VCoutliers$VC, decreasing = T), ], n=10)

  # Do not include BCAR in the QC procedure - to be consistent with ASA24  analysis. 
  # # Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  # QCOutliers(input.data = QCtotals,  
  #            target.colname = "BCAR", min = 15, max = 8200)
  #   
  #     # or show the outliers if too many.
  #     bcaroutliers <- Outlier_rows[, c('SEQN', 'KCAL', 'BCAR')]
  #     # Show the first n rows of the outliers in a descending order. 
  #     head(bcaroutliers[order(bcaroutliers$BCAR, decreasing = T), ], n=10)
  #     
# Look at how many rows (observations) were kept after QC. 
  dim(QCtotals)
      
# ---------------------------------------------------------------------------------------------------------------
# Save QCtotal as a .txt file. 
  write.table(QCtotals, "eg_data/NHANES/Total_D12_FC_mean_QC.txt", sep="\t", quote=F, row.names=F)
  
# # ---------------------------------------------------------------------------------------------------------------
#   # Take n random samples of participants (SEQN).
#   RandomSample(data=QCtotal, n=100, out.fn="eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_100sampled.txt")
#       # This is the "input" file for the SaveInputAndPCs() function at the end of 23_PCA.R. 
#   
#   # Load the subsetted totals file. 
#   totals_QCed_sampled <- read.table(        "eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_100sampled.txt", sep="\t", header=T)
#   
# # ---------------------------------------------------------------------------------------------------------------
