# ========================================================================================
# Use NHANES 2015-16 data as an example.
# Version 1
# Created on 01/28/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import NHANES 2015-16 data. 
# ========================================================================================
# 
# Import NHANES data using the SASexport package.
  # install.packages("SASxport")
  library(SASxport)
  library(foreign)

# Set where the NHANES data are.   
  setwd("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16")

# Food items data.
  nhanes1516_raw1 <- read.xport("Interview_IndFoods_Day1_DR1IFF_I.XPT") 
  nhanes1516_raw2 <- read.xport("Interview_IndFoods_Day2_DR2IFF_I.XPT") 
  str(nhanes1516_raw1)
  dim(nhanes1516_raw1)
  dim(nhanes1516_raw2)
  tail(nhanes1516_raw1, 7)
  
# Add a sequential number to sort the rows after merging.
  nhanes1516_raw1$id <- 1:nrow(nhanes1516_raw1)

# Make a copy of the food code column as integer at the end of nhanes1516_raw. 
  nhanes1516_raw1$Food_code <- as.integer(nhanes1516_raw1$DR1IFDCD)
  str(nhanes1516_raw1)
  
# Make the food code as the first column for merging. 
  nhanes1516_raw1_s <- nhanes1516_raw1[, c(86, 1:85)]
 
  colnames(nhanes1516_raw1_s)
  str(nhanes1516_raw1_s)
  head(nhanes1516_raw1_s, 2)

# ---------------------------------------------------------------------------------------------------------------
# Totals data.
  
  
  
  

# ========================================================================================
# Add Food code description.   
# ========================================================================================
# Load the text file with food code and descriptions. 
  
  codetable <- read.xport("FoodCodes_DRXFCD_I.XPT")
  head(codetable)  
  colnames(codetable)[1] <- "Food_code"
  
# Make the Food code as integer here, too. 
  codetable$Food_codeint <- as.integer(codetable$Food_Code)  
  str(codetable)  
  ?as.integer
  
# Merge the NHANES data and codetable.
  nhanes1516 <- merge(x=nhanes1516_raw1_s, y=codetable, 
                      by = "Food_code", all.x=T)        # all.x=T matches all the rows in the 1st dataframe.
  dim(nhanes1516)
  colnames(nhanes1516)
  head(nhanes1516[, c(1, 2, 86)])
  
# Merging sorts the dataframe by Food_code, so   
# sort back by the id (the original order of nhanes1516_raw)
  nhanes1516 <- nhanes1516[order(nhanes1516$id), ]   
  head(nhanes1516)
  # Food description added! DRXFCSD is short descriptions, and DRXFCLD are long descriptions.
  
### Do not need to do it with the items file (ASA24) because it already has a column of food description 
### column at the end of the table!


# ========================================================================================
# Clean NHANES17-18 data
# ========================================================================================

# How many participants in the total dataset?
  length(unique(nhanes1516$SEQN))
  # 8505. 

# ---------------------------------------------------------------------------------------------------------------
# Status code. 
# Code desciptions in Analytic notes of: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm#Analytic_Notes
#  1: reliable and all relevant variables associated with the 24-hour dietary recall contain a value.
  table(nhanes1516$DR1DRSTZ)
  
# Take only DR1DRSTZ = 1
  nhanes1516_1 <- subset(nhanes1516, DR1DRSTZ == 1)
  table(nhanes1516_1$DR1DRSTZ)
  
# How many participants selected?
  length(unique(nhanes1516_1$SEQN))
  
# ---------------------------------------------------------------------------------------------------------------
    # Weights for both days (if you are analyzing two days)
    # Weights must be used when analyzing a subset of samples. 
      # e.g. 8506 persons provided Day 1 data, but only 7027 of them provided Day 2 data. 
      # if analyzing the 7207 only, weights nhanes1516$WTDR2D should be used.
      nhanes1516_1bothdays <- subset(nhanes1516_1, WTDR2D != 0)
      
      length(unique(nhanes1516_1bothdays$SEQN)) 
      # This is less than 7027 because of the removed individuals with DR1DRSTZ==1. 
      
      # A bit more about weight.... WTDRD1 is weight for day 1, and WTDR2D is for both day 1 and 2. 
      head(nhanes_sub1$WTDRD1)
      head(nhanes_sub1, 2)
      
      head(nhanes_sub1[, c("SEQN", "WTDRD1", "WTDR2D", "DR1_030Z")], 50)
      
      table(unique(nhanes_sub1$SEQN))
      table(unique(nhanes_sub1$WTDRD1))
      # each individual has the same weight values.
      
      How can I use weights??
      # But it seems like simple analysis of day 1 or day 2 of a single cycle of NHANES data
      # does not require weights, according to Table VIII. Most common survey sample weights 
      # and their appropriate use in NHANES Sample Design and Estimation Procedures.
      
# Day 1 and Day 2 are apart by different number of days for samples. 
# You may want to distinguish weekdays and weekend days.   

# ---------------------------------------------------------------------------------------------------------------
# Take a random sample.
  # Choose n random samples of participant ID
  subsetusers <- sample(unique(nhanes1516_1$SEQN), 1000)
  
  # Subset only those found in the chosen ID list.
  nhanes_sub1 <- nhanes1516_1[nhanes1516_1$SEQN %in% subsetusers, ]

  # Confirm how many participants were selected.
  length(unique(nhanes_sub1$SEQN))

  head(nhanes_sub1, 2)
  
# Check basic statistics
  summary(nhanes_sub1$DR1IGRMS)

# histogram
  hist(nhanes_sub1$DR1IGRMS)
  boxplot(nhanes_sub1$DR1IGRMS)
  
# For individual food data, no code for cleaning.
# Outliers won't severely affect main analysis conclusions (ASA24 data cleaning doc)
# But it's always a good idea to take a look at the distributions of variables of interest. 
# Could calculate totals by occasion, similar to ASA24 code.
 
  ### GOOD UNTIL HERE. 04/07/2022. #####
  
  
# ========================================================================================
# Use the prep_data, PCA, and k_means scripts to analyze this data!   
# ========================================================================================
# Load the necessary functions
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/prep_data_for_clustering.R")
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/PCA.R")
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/k-means.R")

# Take average of each user for each of the 64 nutrients.
# Nutrients analysis  --> start.col = "DR1IPROT",    end.col = "DR1IP226"
  AverageBy(data = nhanes_sub_b, by = "SEQN", start.col = "DR1IPROT", end.col = "DR1IP226")

# The column names should be the same as start.col-end.col. 
  colnames(meansbycategorydf)

# pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# --> No columns were removed.

# Collapse variables - cutoff R>0.75 
# --> collapsed from 64 to 30.

# The cleaned and averaged dataset is ?? x ?? dataframe.

# ---------------------------------------------------------------------------------------------------------------
# Calculate and plot %kcal of TFAT, PROT, and CARB. 
# Load necessary functions. 
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/percent_kcal.R")

# Plot %kcal of protein, fat, and carbs.
# need to rename the data so that they will be recognized by the functions.. 
  totals <- nhanes_sub_b
  totals$UserName <- totals$SEQN
  totals$KCAL <- totals$DR1IKCAL
  totals$PROT <- totals$DR1IPROT
  totals$TFAT <- totals$DR1ITFAT
  totals$CARB <- totals$DR1ICARB
  totals$SUGR <- totals$DR1ISUGR

# Is SUGR a part of CARB or is it separate???
  aaa =head(totals[, c("KCAL", "TFAT", "CARB", "PROT", "SUGR")])
  aaa$kcal_prot <- aaa$PROT*4  
  aaa$kcal_carb <- aaa$CARB*4  
  aaa$kcal_tfat <- aaa$TFAT*9  
  aaa$kcal_total <- aaa$kcal_prot + aaa$kcal_carb + aaa$kcal_tfat 
  aaa$diff <- aaa$kcal_total - aaa$KCAL
  totals$carb_sugr <- totals$CARB - totals$SUGR
  summary(totals$carb_sugr)
# For most cases, CARB > SUGR, so it's possible that CARB includes SUGR (should be...) 
# but not quite sure. 

  head(totals, 10)
  totals$PROT
  table(totals$UserName)
  totals[, c("UserName", "TFAT", "PROT")]
  str(totals)

# %kcal
  CalcKcal_user()
  NormalizedPercentKcal()
# This one works!

# If there was a factor(s) that could group participants, then SD will be meaningful. 
 NonNormalizedPercentKcal(show.sd = T)

# # Replace NaN and Inf with zero. *** Not really needed..
# totals[ is.na(totals)] <- 0 
# totals[ totals == Inf ] <- 0 
# totals[ totals == -Inf ] <- 0 
# head(totals, 10)
# ---------------------------------------------------------------------------------------------------------------
  
  
 
 
 
 
 
  
      
  
  
# ---------------------------------------------------------------------------------------------------------------
# Explore data 
  library(dplyr)
  # Gram weight of the food/individual component
  nhanes_sub %>% filter(SEQN == 93704) %>% nrow()  # 13 datapoints for this participant
  nhanes_sub %>% filter(SEQN == 93704) %>% summarize(protmean = mean(DR1IPROT))  
  # The mean protein is 3.967692.  Need to check the kcal_mean code. 
  
  nhanes_sub %>% filter(DR1IGRMS > 500) %>% nrow()  # 30
  nhanes_sub %>% filter(DR1IGRMS < 500) %>% nrow()/nrow(nhanes_sub)*100
  nhanes_sub %>% filter(DR1IGRMS > 3000)
  nhanes_sub %>% filter(DR1IFDCD == 94100100) %>% nrow()  # water intake record.
  water <- nhanes_sub %>% filter(DR1IFDCD == 94100100) # water intake record.
  summary(water$DR1IGRMS)
  water %>% filter(DR1IGRMS > 1000) %>% nrow() # 3 records are reporting > 1 L of water intake at one time. 
  water1L <- water %>% filter(DR1IGRMS > 1000)
  water1L[, c("SEQN", "DR1IGRMS")] # those are different people. hmm. 
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
  # remove records that report 500 g or higher intake.
  # 6.7% of the data will be removed and 93.3% of the data will still be preserved (for the n=450 subset). 
  nhanes_sub_a <- nhanes_sub %>% filter(DR1IGRMS < 500) 
  dim(nhanes_sub_a)
  
  # See the food codes of 400-500 g
  head(nhanes_sub_a[ order(nhanes_sub_a$DR1IGRMS, decreasing = T), ])
  # Those are sports drinks, orange juice, black tea, water, etc. Seems correct..
  
  # Use this dataset nhanes_sub_a. 
  # How many records/participants?
  user_freq <- as.data.frame(table(nhanes_sub_a$SEQN))
  hist(user_freq$Freq)
  min(user_freq$Freq)
  
  # Sort by the number of records/participant
  user_freq_s <- user_freq[order(user_freq$Freq, decreasing = F), ]
  head(user_freq_s, 10)
  
  # 93763 has only 2 records. what did they eat?
  nhanes_sub_a %>% filter(SEQN == 93763) 
  # Pizza and taco sauce.... Probably stopped recording after entering those 2 items. 
  
  # 93834 has only 4 records. what did they eat?
  nhanes_sub_a %>% filter(SEQN == 93834) 
  # Bread, butter, chicken, rice. Could be 2 meals per day.
  
  # 93724 has only 6 records. what did they eat?
  nhanes_sub_a %>% filter(SEQN == 93724) 
  
  # only keep participants who have at least 4 entries in this 24 h-record. 
  nhanes_sub_b <- nhanes_sub_a %>% filter(SEQN != 93763)
  
  # Intake day of the week frequency
  table(nhanes_sub_b$DR1DAY) 
  
  # Respondents' language 
  table(nhanes_sub_b$DR1LANG)  # language they spoke
  table(nhanes_sub_b$DR1CCMNM) # combination food number
  table(nhanes_sub_b$DR1CCMTX) # Combination food type
  table(nhanes_sub_b$DR1_040Z) # Did you eat this meal at home?
  
  foodcodes = read.table("clipboard", sep = "\t", header = T)
  head(foodcodes)

