# ===============================================================================================================
# Use NHANES 2015-16 data as an example.
# Version 1
# Created on 04/13/2022 by Rie Sadohara
# ===============================================================================================================

# First time only: install the packages you need.
  # install.packages("SASxport")
  # install.packages("foreign")

# ===============================================================================================================
# Load and clean NHANES "food items" data
# ===============================================================================================================

# Load necessary packages.
  library(SASxport)
  library(foreign)

# Load necessary functions.
  source("~/GitHub/dietary_patterns/lib/load_clean_NHANES.R")

# Set where the NHANES data and food code table are.
# it is not in the eg_data folder because it's too large to save in GitHub folder. 
  setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")


# ---------------------------------------------------------------------------------------------------------------
# Prep the code table - replace special characters with "_" or "and"
  
  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "FoodCodes_DRXFCD_I.XPT", 
                       out.fn = "FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted food code table.
  foodcodetable_f <- read.table("FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)

# ---------------------------------------------------------------------------------------------------------------
# If analyzing both Day 1 and Day 2, save day 1 and day 2 with different names.

# Import items data Day 1, add food item descriptions, and save it as a txt file.
# LIKELY IT WILL BE A HUGE FILE.
  ImportNHANESFoodItems(data.name="Interview_IndFoods_Day1_DR1IFF_I.XPT", 
                        food.code.column = "DR1IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "Food_D1_w_code.txt")

# Load the saved food items file. 
  Food_D1 <- read.table("Food_D1_w_code.txt", sep="\t", header=T)
 

# Import items data Day 2, add food item descriptions, and save it as a txt file.
  ImportNHANESFoodItems(data.name="Interview_IndFoods_Day2_DR2IFF_I.XPT", 
                        food.code.column = "DR2IFDCD", 
                        food.code.table = foodcodetable_f,
                        out.fn = "Food_D2_w_code.txt")
  
# Add food item description and save it as a txt file. 
  Food_D2 <- read.table("Food_D2_w_code.txt", sep="\t", header=T)

  
# ---------------------------------------------------------------------------------------------------------------
# Filter by Status code - Only retain complete entries. 
  #  1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  # Code descriptions in Analytic notes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
  
  # Define the dataset to work on. 
  nhanes_food <- Food_D1
  
    # FOR DAY 1, take only DR1DRSTZ = 1
    nhanes_food_1 <- subset(nhanes_food, DR1DRSTZ == 1)
    # FOR DAY 2, take only DR2DRSTZ = 1
    nhanes_food_1 <- subset(nhanes_food, DR2DRSTZ == 1)
  
  # Check how many participants were selected.
  length(unique(nhanes_food_1$SEQN)) 
  dim(nhanes_food_1)
  
# ---------------------------------------------------------------------------------------------------------------
# Take n random samples of participants.
  RandomSample(data = nhanes_food_1, n=30, out.fn = "NHANES_foods_QCed_30.txt")

# Load the subsetted food items file. 
  food_sampled <- read.table("NHANES_foods_QCed_30.txt", sep="\t", header=T)
  
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
  nhanes1516_totals1 <- read.xport('Total_Nutrient_Day1_DR1TOT_J.XPT')
  nhanes1516_totals2 <- read.xport('Total_Nutrient_Day2_DR2TOT_J.XPT')
  
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
  nhanes1516 <- nhanes1516_totals1
  dim(nhanes1516_totals1)
  # How many participants in the total dataset?
  length(unique(nhanes1516_totals1$SEQN))
  # 8704 for totals day 1.
  
# ---------------------------------------------------------------------------------------------------------------
# Status code - Only retain complete entries. 
  # Code descriptions in Analytic notes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
  #  1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  table(nhanes1516_totals1$DR1DRSTZ)
  table(nhanes1516_totals2$DR2DRSTZ)
  
  # Take only DR1DRSTZ = 1
  nhanes_totals_1 <- subset(nhanes1516_totals1, DR1DRSTZ == 1)
  nhanes_totals_2 <- subset(nhanes1516_totals2, DR2DRSTZ == 1)
  table(nhanes_totals_1$DR1DRSTZ)
  
  # How many participants selected?
  length(unique(nhanes_totals_1$SEQN)) 
  length(unique(nhanes_totals_2$SEQN)) 
  
# ---------------------------------------------------------------------------------------------------------------
  # For totals, the same QC can be applied as ASA24 totals QC procedure.
  # Functions to clean ASA24 data.
  source("~/GitHub/dietary_patterns/lib/load_clean_ASA24.R")
  
# Run all these QC steps in this order.  When asked, choose to remove the outliers
# that fall outside the specified range for each nutrient.
  
# Define the input data.  This will be modified after each filter.
  QCtotals <- nhanes_totals_1
  
  # Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "DR1TKCAL", min = 600, max = 5700)
  
  # Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "DR1TPROT", min = 10, max = 240)
  
  # Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals, 
             target.colname = "DR1TTFAT", min = 15, max = 230)

  # Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "DR1TVC", min = 5, max = 400)
  
      # or show the outliers if too many.
      VC_outlier_rows[, c('SEQN', 'DR1TKCAL', 'DR1TDR1TVC')]
  
  # Flag if BCAR (beta-carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  QCOutliers(input.data = QCtotals,  
             target.colname = "DR1TBCAR", min = 15, max = 8200)
    
      # or show the outliers if too many.
      bcaroutliers <- Outlier_rows[, c('SEQN', 'DR1TKCAL', 'DR1TBCAR')]
    # Show the first n rows of the outliers in a descending order. 
    head(bcaroutliers[order(bcaroutliers$DR1TBCAR, decreasing = T), ], n=10)
  

# ---------------------------------------------------------------------------------------------------------------
  # Save QCtotals as "Totals_QCed.txt" 
  write.table(QCtotals, "NHANES_totals_QCed.txt", sep="\t", quote=F, row.names=F)  
  write.table(QCtotals, "eg_data/VVKAJ101-105/VVKAJ_2021-11-09_7963_Totals_QCed.txt", sep="\t", quote=F, row.names=F)  
  
# ---------------------------------------------------------------------------------------------------------------
  # Take n random samples of participants.
  RandomSample(data = QCtotals, n=30, out.fn = "NHANES_totals_QCed_sampled.txt")
  
  # Load the subsetted totals file. 
  totals_QCed_1500 <- read.table("NHANES_totals_QCed_sampled.txt", sep="\t", header=T)
  
# ---------------------------------------------------------------------------------------------------------------
  
