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
  library(foreign)

# Set where the NHANES data and food code table are.
# it is not in the eg_data folder because it's too large to save in GitHub folder. 
  # setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")
  setwd("~/GitHub/dietary_patterns")

# Load necessary functions.
  source("lib/load_clean_NHANES.R")

# ---------------------------------------------------------------------------------------------------------------
# Prep the code table - replace special characters with "_" or "and"
  
  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "eg_data/NHANES/FoodCodes_DRXFCD_I.XPT", 
                       out.fn =              "eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted food code table.
  foodcodetable_f <- read.table("eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)

# ---------------------------------------------------------------------------------------------------------------
# If analyzing both Day 1 and Day 2, save day 1 and day 2 with different names.

# Import items data Day 1, add food item descriptions, and save it as a txt file.
# LIKELY IT WILL BE A HUGE FILE.
  ImportNHANESFoodItems(data.name="E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Interview_IndFoods_Day1_DR1IFF_I.XPT", 
                        food.code.column = "DR1IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "eg_data/NHANES/Interview_IndFoods_Day1_DR1IFF_I_d.txt") # 'd' stands for food descriptions

# Load the saved food items file. 
  Food_D1 <- read.table("eg_data/NHANES/Interview_IndFoods_Day1_DR1IFF_I_d.txt", sep="\t", header=T)
  head(Food_D1,2)

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
  demo <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  adults <- demo[demo$RIDAGEYR >= 18, ]
  
# Retain those with complete data (STZ==1)
  # DR1DRSTZ == 1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  # Code descriptions in Analytic notes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
  food1 <- subset(Food_D1, DR1DRSTZ == 1)
  food2 <- subset(Food_D2, DR2DRSTZ == 1)
  
# Create a vector of SEQN of those who reported more than 1 food item per day.
  freqtable1 <- as.data.frame(table(food1$SEQN))
  freqtable1_m <- freqtable1[freqtable1$Freq > 1, ]
  colnames(freqtable1_m)[1] <- "SEQN"
  keepnames_adults_mult1 <- keepnames_adults[keepnames_adults %in% freqtable1_m$SEQN]
  # Take only those in keepnames_adults_mult1.
  food1 <- food1[food1$SEQN %in% keepnames_adults_mult1, ] 
  
  # Do the same for food2
  freqtable2 <- as.data.frame(table(food2$SEQN))
  freqtable2_m <- freqtable2[freqtable2$Freq > 1, ]
  colnames(freqtable2_m)[1] <- "SEQN"
  keepnames_adults_mult2 <- keepnames_adults[keepnames_adults %in% freqtable2_m$SEQN]
  food2 <- food2[food2$SEQN %in% keepnames_adults_mult2, ] 

# Create a vector of SEQN of those that have both day 1 and day 2 data.
  food1names <- unique(food1$SEQN)
  food2names <- unique(food2$SEQN)
  keepnames12 <- food1names[food1names %in% food2names]

# Combine day 1 and day 2 data.
  # Day 1
  # Import the list of variables present in Day 1. 
  day1variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_Day1.txt', header=F)
  # Which variables to pick up from the food data
  var_to_use1 <- names(food1) %in% day1variables$V1
  # pick up only the specified variables 
  food1 <- food1[, var_to_use1]
  # Remove "DR1T", "DR1" from the column names 
  colnames(food1) <- gsub(colnames(food1), pattern = "DR1I", replacement = "")
  colnames(food1) <- gsub(colnames(food1), pattern = "DR1", replacement = "")
  # Check
  head(food1, 1)
 
  # Do the same for Day 2  
  day2variables <- read.table('eg_data/NHANES/NHANES_Food_VarNames_Day2.txt', header=F)
  var_to_use2 <- names(food2) %in% day2variables$V1
  food2 <- food2[, var_to_use2]
  colnames(food2) <- gsub(colnames(food2), pattern = "DR2I", replacement = "")
  colnames(food2) <- gsub(colnames(food2), pattern = "DR2", replacement = "")
  head(food2, 1)
  
  # Do the columns of food1 and food2 match?
  identical(colnames(food1), colnames(food2))

  # Make a day variable before combining
  food1$Day = 1
  food2$Day = 2
  
  # Combine food1 and food2
  food12 <- rbind(food1, food2)
  
  # Pick up only those in keepnames12.
  food12 <- food12[food12$SEQN %in% keepnames12, ]
  
  
  sum(table(unique(food12$SEQN))[,1] <4) 
  sum(table(food12$SEQN, food12$Day)[,1] <2)
  sum(table(food12$SEQN, food12$Day)[,2] <2)
  aaa = as.data.frame(table(food12$SEQN, food12$Day))
  head(aaa[order(aaa$Freq), ], 10)
  table(food12$Day)
  
  subset(food12, SEQN==86563)[, 'Day']
  
  ######### STILL, SEQN THAT ONLY HAS DAY 1 DATA IS PICKED UP. WHY??????????????????
  
  #
  # dietary pattern/special diet map
  library(dplyr)
  diet_type_map <- nhanes_totals_1 %>% select(SEQN, DRQSDIET, DRQSDT1, DRQSDT2, DRQSDT3, DRQSDT4, DRQSDT5, DRQSDT6, DRQSDT7, DRQSDT8, DRQSDT9, DRQSDT10, DRQSDT11, DRQSDT12, DRQSDT91)
  diet_type_map <- diet_type_map[diet_type_map$SEQN %in% keepnames12, ]
   ############ THERE IS NO MATCH BETWEEN TOTALS$SEQN AND KEEPNAMES12????????????????????
  
  head(diet_type_map)
  head(keepnames12)
  tail(keepnames12)
  head(nhanes_totals_1$SEQN)
  tail(nhanes_totals_1$SEQN)
  head(Food_D1$SEQN)
  tail(Food_D1$SEQN)
  
  # food:  83732 - 93702 
  # total: 93704 - 102956
  
  
  
  
  
  
  
  #
  
  
  
  
  
  
  
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# Take n random samples of participants.
  RandomSample(data = nhanes_food_1, n=70, out.fn = "NHANES_foods_QCed_sampled.txt")

# Load the subsetted food items file. 
  food_sampled <- read.table("NHANES_foods_QCed_sampled.txt", sep="\t", header=T)
  
# ---------------------------------------------------------------------------------------------------------------
  # Check basic statistics of food_sampled
  
  colnames(food_sampled)
  # KCAL
  head(   food_sampled$DR1IKCAL)
  boxplot(food_sampled$DR1IKCAL)
  
  # only items file has GRMS (grams) data. 
  summary(food_sampled$DR1IGRMS)
  hist(   food_sampled$DR1IGRMS)
  boxplot(food_sampled$DR1IGRMS)
  
  # For individual food data, there is no code for cleaning.
  # Outliers won't severely affect main analysis conclusions (ASA24 data cleaning doc)
  # But it's always a good idea to take a look at the distributions of variables of interest. 
  # Could calculate totals by occasion, similar to ASA24 code.
  
  
  
# ===============================================================================================================
# Load and clean NHANES totals data. 
# ===============================================================================================================

# Load total nutrient intake of day 1 or day 2. Day 1 has more columns that can be used as metadata.
  nhanes1516_totals1 <- read.xport('E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Total_Nutrient_Day1_DR1TOT_J.XPT')
  nhanes1516_totals2 <- read.xport('E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Total_Nutrient_Day2_DR2TOT_J.XPT')
  
# Data documentation: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1IFF_I.htm#Appendix_3._List_of_Nutrients/Food_Components_(Unit)
# Descriptions of the columns are there.
  
  # e.g. only day 1 has Number of times mackerel eaten past 30 days
  hist(nhanes1516_totals1$DRD370HQ)
  
  # Intake of the day 
  hist(nhanes1516_totals1$DR1DAY)
  
  # On a special diet?
  table(nhanes1516_totals1$DRQSDIET)
  
  # KCAL on that day
  hist(nhanes1516_totals1$DR1TKCAL) 

  # Rename the dataset to work on.
  # nhanes1516 <- nhanes1516_totals1
  
  # How many participants are in the total dataset?
  length(unique(nhanes1516_totals1$SEQN))   # 8704 for totals day 1.
  length(unique(nhanes1516_totals2$SEQN))   # 8704 for totals day 2.
  
# ---------------------------------------------------------------------------------------------------------------
# Status code - Only retain complete entries.
  # Code descriptions in Analytic notes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
  #  1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  table(nhanes1516_totals1$DR1DRSTZ)
  table(nhanes1516_totals2$DR2DRSTZ)
  
  # Take only DR1DRSTZ = 1
  nhanes_totals_1 <- subset(nhanes1516_totals1, DR1DRSTZ == 1)
  nhanes_totals_2 <- subset(nhanes1516_totals2, DR2DRSTZ == 1)
  
  # How many participants selected?
  length(unique(nhanes_totals_1$SEQN)) 
  length(unique(nhanes_totals_2$SEQN)) 
  
  matchedall = merge(x=nhanes_totals_2, y=nhanes_totals_1, by = "SEQN")
  length(unique(matchedall$SEQN)) 
  # So, there are 6491 participants who have both day 1 and day 2 data. 
  
# ---------------------------------------------------------------------------------------------------------------
# Take average of day 1 and day 2 of totals 
  # Day 1
  # Import the list of variables present in Day 1. 
  day1variables <- read.table('eg_data/NHANES/NHANES_VarNames_Day1.txt', header=F)
  head(day1variables)
  
  # Which variables to pick up from the totals data
  names_to_use <- names(nhanes_totals_1) %in% day1variables$V1
  # pick up only the specified variables 
  nhanes_totals_1_subset <- nhanes_totals_1[, names_to_use]
  # Add a column that says "Day 1"
  nhanes_totals_1_subset$Day <- "Day1"
  # Remove "DR1T", "DR1" from the column names 
  colnames(nhanes_totals_1_subset) <- gsub(colnames(nhanes_totals_1_subset), pattern = "DR1T", replacement = "")
  colnames(nhanes_totals_1_subset) <- gsub(colnames(nhanes_totals_1_subset), pattern = "DR1", replacement = "")
  # Check
  head(nhanes_totals_1_subset, 1)
  
  # Do the same for Day 2.
  day2variables <- read.table('eg_data/NHANES/NHANES_VarNames_Day2.txt', header=F)
  head(day2variables)
  names_to_use <- names(nhanes_totals_2) %in% day2variables$V1
  nhanes_totals_2_subset <- nhanes_totals_2[, names_to_use]
  nhanes_totals_2_subset$Day <- "Day2"
  colnames(nhanes_totals_2_subset) <- gsub(colnames(nhanes_totals_2_subset), pattern = "DR2T", replacement = "")
  colnames(nhanes_totals_2_subset) <- gsub(colnames(nhanes_totals_2_subset), pattern = "DR2", replacement = "")
  head(nhanes_totals_2_subset, 1)
  
  # Make a data frame that has the column names of day 1 and day 2 totals, and ensure they match.
  colnames <- data.frame(day1=colnames(nhanes_totals_1_subset), day2=colnames(nhanes_totals_2_subset)) 
  head(colnames)
  identical(x=colnames$day1, y=colnames$day2) # they match if TRUE.
  
  # Create a long table
  bound <- rbind(nhanes_totals_1_subset, nhanes_totals_2_subset)
  
  # Create a frequency table of SEQN.  
  SEQNtable <- as.data.frame(table(bound$SEQN))
  table(SEQNtable$Freq)
  # So, there are 6491 SEQNs that have both Day 1 and Day 2.
  
  # Sort the freq table.
  orderedSEQNtable <- SEQNtable[order(SEQNtable$Freq, decreasing = T), ]
  head(orderedSEQNtable)
  
  # Pick up the SEQNs that are present in both days.
  orderedSEQNtable_2 <- subset(orderedSEQNtable, Freq==2)
  colnames(orderedSEQNtable_2)[1] <- "SEQN"
  
  # Select only the SEQN that are in orderedSEQNtable_2.
  twodays <- merge(x=orderedSEQNtable_2, y=bound, by="SEQN", all.x = T)
  head(twodays, 4)
  table(twodays$Day)

  # Then take average of Day 1 and Day 2.
  meantotals <- aggregate(twodays[, 3:67], by=list(twodays$SEQN), FUN=mean)
  dim(meantotals)
  head(meantotals)
  colnames(meantotals)[1] <- "SEQN"
  subset(meantotals, SEQN=="100001")
  
  # Save meantotals as a txt file.
  write.table(meantotals, "eg_data/NHANES/NHANES1516_totals_2daymean.txt", sep="\t", row.names=F, quote=F)
  
# Load the meantotals from next session --
  nhanes2days <- read.table("eg_data/NHANES/NHANES1516_totals_2daymean.txt", sep="\t", header=T)

# ---------------------------------------------------------------------------------------------------------------
# For totals, the same QC can be applied as ASA24 totals QC procedure.
  # Functions to clean ASA24 data.
  source("lib/load_clean_ASA24.R")
  
# Run all these QC steps in this order.  When asked, choose to remove the outliers
# that fall outside the specified range for each nutrient.
  
# Define the input data.  This will be modified after each filter.
  QCtotals <- nhanes2days
  
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
      VC_outlier_rows[, c('SEQN', 'KCAL', 'VC')]
  
  # Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "BCAR", min = 15, max = 8200)
    
      # or show the outliers if too many.
      bcaroutliers <- Outlier_rows[, c('SEQN', 'KCAL', 'BCAR')]
      # Show the first n rows of the outliers in a descending order. 
      head(bcaroutliers[order(bcaroutliers$BCAR, decreasing = T), ], n=10)
  

# ---------------------------------------------------------------------------------------------------------------
  # Save QCtotals as "Totals_QCed.txt" 
  write.table(QCtotals, "eg_data/NHANES/NHANES_totals_2days_QCed.txt", sep="\t", quote=F, row.names=F)  
  write.table(QCtotals, "eg_data/VVKAJ101-105/VVKAJ_2021-11-09_7963_Totals_QCed.txt", sep="\t", quote=F, row.names=F)  
  
# ---------------------------------------------------------------------------------------------------------------
  # Take n random samples of participants.
  RandomSample(data = QCtotals, n=1000, out.fn = "eg_data/NHANES/NHANES_2days_totals_QCed_1000sampled.txt")
  
  # Load the subsetted totals file. 
  totals_QCed_sampled <- read.table("eg_data/NHANES/NHANES_2days_totals_QCed_1000sampled.txt", sep="\t", header=T)
  
# ---------------------------------------------------------------------------------------------------------------
