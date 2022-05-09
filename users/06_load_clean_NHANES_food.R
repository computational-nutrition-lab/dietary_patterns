# ===============================================================================================================
# Load NHANES 2015-16 FOOD data, add food description, QC, and calculate total. 
# Version 1
# Created on 04/26/2022 by Rie Sadohara
# ===============================================================================================================

# Folder structure 
# 
#                          |----- eg_data -- NAHES -- Data -- Food Items and Totals files 
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
  # library(foreign)

# Set where the NHANES data and food code table are.
# it is not in the eg_data folder because it's too large to save in GitHub folder. 
  # setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")
  source("lib/prep_data_for_clustering.R")

# ---------------------------------------------------------------------------------------------------------------
# Prep the code table - replace special characters with "_" or "and"
  
  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "eg_data/NHANES/FoodCodes_DRXFCD_I.XPT", 
                       out.fn =              "eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted food code table.
  foodcodetable_f <- read.table("eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)

# ---------------------------------------------------------------------------------------------------------------
# Import items data Day 1, add food item descriptions, and save it as a txt file.
# LIKELY IT WILL BE A HUGE FILE.
  ImportNHANESFoodItems(data.name="E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Interview_IndFoods_Day1_DR1IFF_I.XPT", 
                        food.code.column = "DR1IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/Interview_IndFoods_Day1_DR1IFF_I_d.txt") # 'd' stands for food descriptions

# Load the saved food items file. 
  Food_D1 <- read.table("eg_data/NHANES/Interview_IndFoods_Day1_DR1IFF_I_d.txt", sep="\t", header=T)

# Import items data Day 2, add food item descriptions, and save it as a txt file.
  ImportNHANESFoodItems(data.name="E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Interview_IndFoods_Day2_DR2IFF_I.XPT", 
                        food.code.column = "DR2IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/Interview_IndFoods_Day2_DR2IFF_I_d.txt")
  
# Add food item description and save it as a txt file. 
  Food_D2 <- read.table("eg_data/NHANES/Interview_IndFoods_Day2_DR2IFF_I_d.txt", sep="\t", header=T)

# ===============================================================================================================
# QC the food data: filter by age, completeness, >1 food item reported/day, data exists on both days. 
# ===============================================================================================================

# Remove children.  
  # Load the demographics file, then filter by age > 18.
  demog <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  adults <- demog[demog$RIDAGEYR >= 18, ]
  
# Retain those with complete data (STZ==1)
  # DR1DRSTZ == 1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  # Code descriptions in Analytic notes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
  food1 <- subset(Food_D1, DR1DRSTZ == 1)
  food2 <- subset(Food_D2, DR2DRSTZ == 1)
  
  food1names <- unique(food1$SEQN)
  food2names <- unique(food2$SEQN)
  keepnames <- food1names[food1names %in% food2names]
  keepnames_adults <- keepnames[keepnames %in% adults$SEQN]
  
# Keep those who reported more than 1 food item per day.
  freqtable1 <- as.data.frame(table(food1$SEQN))
  freqtable1_m <- freqtable1[freqtable1$Freq > 1, ]
  colnames(freqtable1_m)[1] <- "SEQN"
  keepnames_adults_mult1 <- keepnames_adults[keepnames_adults %in% freqtable1_m$SEQN]
  # Take only the participants whose names are in keepnames_adults_mult1.
  food1b <- food1[food1$SEQN %in% keepnames_adults_mult1, ] 
  
  # Do the same for food2
  freqtable2 <- as.data.frame(table(food2$SEQN))
  freqtable2_m <- freqtable2[freqtable2$Freq > 1, ]
  colnames(freqtable2_m)[1] <- "SEQN"
  keepnames_adults_mult2 <- keepnames_adults[keepnames_adults %in% freqtable2_m$SEQN]
  food2b <- food2[food2$SEQN %in% keepnames_adults_mult2, ] 

# Create a vector of SEQN of those that have both day 1 and day 2 data.
  food1bnames <- unique(food1b$SEQN)
  food2bnames <- unique(food2b$SEQN)
  keepnames12 <- food1bnames[food1bnames %in% food2bnames]

# Combine day 1 and day 2 data.
  # Day 1
  # Import the list of variables to be picked up in Day 1. 
  day1variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_Day1.txt', header=F)
  # Which variables to pick up from the food data
  var_to_use1 <- names(food1b) %in% day1variables$V1
  # pick up only the specified variables 
  food1c <- food1b[, var_to_use1]
  # Remove "DR1T", "DR1" from the column names 
  colnames(food1c) <- gsub(colnames(food1c), pattern = "DR1I", replacement = "")
  colnames(food1c) <- gsub(colnames(food1c), pattern = "DR1",  replacement = "")
  # Check
  head(food1c, 1)
 
  # Do the same for Day 2  
  day2variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_Day2.txt', header=F)
  var_to_use2 <- names(food2b) %in% day2variables$V1
  food2c <- food2b[, var_to_use2]
  colnames(food2c) <- gsub(colnames(food2c), pattern = "DR2I", replacement = "")
  colnames(food2c) <- gsub(colnames(food2c), pattern = "DR2", replacement = "")
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
      aaa = as.data.frame(table(food12d$SEQN, food12d$Day))
      head(aaa, 10)
      table(aaa$Var2)
      head(aaa[order(aaa$Freq), ], 10)
      tail(aaa[order(aaa$Freq), ], 10)
      subset(food12d, SEQN==86563)[, 'Day'] # Participant No. 86563 reported only 1 food/day. Should be nonexistent.

# save the combined and QCed food items as a .txt file. (IT WILL BE A HUGE FILE.)
  write.table(food12d, "eg_data/NHANES/NHANES1516_items_d12_QC.txt", sep="\t", quote=F, row.names=F)  
      
# ---------------------------------------------------------------------------------------------------------------
# Take n random samples of participants (SEQN).
  RandomSample(data= food12d, n=500, out.fn="eg_data/NHANES/NHANES1516_items_d12_QC_500sampled.txt")

# ---------------------------------------------------------------------------------------------------------------
            
# You may also want to consider special diets that some participants are following: e.g. DASH diet, diabetic diet, etc.
# Depending on your research question, you may want to exclude those following special diets.
# The diet information is found in totals day 1.  

# ===============================================================================================================
# Calculate totals/day/participant with the food data of the selected SEQNs.
# ===============================================================================================================

# Calculate total for day 1. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Take only the Day 1 data
  food12d_d1 <- subset(food12d, Day==1) 
  colnames(food12d_d1)
      
# Sum nutrients; this is total data calculated by hand.
# First, speicfy the first and the last column (variable) names to calculate totals for. 
  first.val <- "GRMS"
  last.val <- "P226"
  
  start_col_num <- match(first.val, names(food12d_d1))  # The number of column that matches the first variable specified.
  end_col_num <-   match(last.val, names(food12d_d1)) # The number of column that matches the last variable specified.
    
  # Sum food items by SEQN from start through end columns.
  total1 <- aggregate(food12d_d1[, start_col_num:end_col_num], 
                      by=list(food12d_d1$SEQN), 
                      FUN=sum)

  total1$Day <- 1
  colnames(total1)[1] <- "SEQN"

# Create a vector of number of food items reported by each participant.
  n_items1 <- as.data.frame(table(food12d_d1$SEQN))
  colnames(n_items1) <- c("SEQN", "NoOfItems")
  
  # Add it to total1
  total1b <- merge(x=total1, y=n_items1, by="SEQN", all.x=T)
  
    # Some checking  
    subset(total1b, NoOfItems<2) # should be zero.
    # Look for any missing data
    total1b[is.na(total1b$NoOfItems), ]

# Calculate total for day 2. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  food12d_d2 <- subset(food12d, Day==2) 
  
  # Sum nutrients.  
  # First, speicfy the first and the last column (variable) names to calculate totals for. 
  first.val <- "GRMS"
  last.val <- "P226"
  
  start_col_num <- match(first.val, names(food12d_d2))  # The number of column that matches the first variable specified.
  end_col_num <-   match(last.val, names(food12d_d2)) # The number of column that matches the last variable specified.
  
  # Sum food items by SEQN from start through end columns.
  total2 <- aggregate(food12d_d2[, start_col_num:end_col_num], 
                      by=list(food12d_d2$SEQN), 
                      FUN=sum)
  
  total2$Day <- 2
  colnames(total2)[1] <- "SEQN"
  
  # Create a vector of number of food items reported by each participant.
  n_items2 <- as.data.frame(table(food12d_d2$SEQN))
  colnames(n_items2) <- c("SEQN", "NoOfItems")
  # Add it to total1
  total2b <- merge(x=total2, y=n_items2, by="SEQN", all.x=T)
  
     # Some checking
     subset(total2b, NoOfItems<2) # should be zero.
     # Look for any missing data
     total2b[is.na(total2b$NoOfItems), ]
      
# Merge the totals
  # Check if all the columnnames match.
  identical(colnames(total1b), colnames(total2b))
  # Merge
  total12c <- rbind(total1b, total2b)
  
# Save the calculated totals of day 1 and 2 as a txt file.
  write.table(total12c, "eg_data/NHANES/NHANES1516_total_d12.txt", sep="\t", row.names=F, quote=F)

# ===============================================================================================================
# Calculate the mean of totals/participant. 
# ===============================================================================================================
  
# Load the calculated totals.
  total12d <- read.table("eg_data/NHANES/NHANES1516_total_d12.txt", sep="\t", header=T)
  colnames(total12d)

# Take average of Day 1 and Day 2.
  # First, speicify the first and the last column (variable) names to calculate means for. 
  first.val <- "GRMS"
  last.val <- "NoOfItems"  #### Now you want the average of No of Items reported, too. 
  
  start_col_num <- match(first.val, names(total12d))  # The number of column that matches the first variable specified.
  end_col_num <-   match(last.val,  names(total12d)) # The number of column that matches the last variable specified.
  
  # Sum food items by SEQN from start through end columns.
  meantotal12a <- aggregate(total12d[, start_col_num:end_col_num], 
                            by=list(total12d$SEQN), 
                            FUN=mean) 

  # Remove the day, which is now all 1.5 (the average of 1 and 2.)
  meantotal12 <- meantotal12a[, !names(meantotal12a) %in% "Day"]
  # Change "Group.1" to "SEQN".
  colnames(meantotal12)[1] <- "SEQN"
  
# Save meantotals as a txt file.
  write.table(meantotal12, "eg_data/NHANES/NHANES1516_total_d12_mean.txt", sep="\t", row.names=F, quote=F)
  
# Load the mean total
  meantotal12b <- read.table("eg_data/NHANES/NHANES1516_total_d12_mean.txt", sep="\t", header=T)
  
# ===============================================================================================================
# QC the mean total in the same way as ASA24. 
# ===============================================================================================================
  
# For individual food data, there is no code for cleaning.
# Outliers won't severely affect main analysis conclusions (ASA24 data cleaning doc)
# But it's always a good idea to take a look at the distributions of variables of interest. 
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

  # Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "BCAR", min = 15, max = 8200)
    
      # or show the outliers if too many.
      bcaroutliers <- Outlier_rows[, c('SEQN', 'KCAL', 'BCAR')]
      # Show the first n rows of the outliers in a descending order. 
      head(bcaroutliers[order(bcaroutliers$BCAR, decreasing = T), ], n=10)
      

# ---------------------------------------------------------------------------------------------------------------
# Save QCtotals as a .txt file. 
  write.table(QCtotals, "eg_data/NHANES/NHANES1516_total_d12_mean_QC_2.txt", sep="\t", quote=F, row.names=F)  
  
# ---------------------------------------------------------------------------------------------------------------
  # Take n random samples of participants (SEQN).
  RandomSample(data=QCtotals, n=500, out.fn="eg_data/NHANES/NHANES1516_total_d12_mean_QC_2_500sampled.txt")
  
  # Load the subsetted totals file. 
  totals_QCed_sampled <- read.table(        "eg_data/NHANES/NHANES1516_total_d12_mean_QC_2_500sampled.txt", sep="\t", header=T)
  
# ---------------------------------------------------------------------------------------------------------------
