# ========================================================================================
# Prepare NHANES food and items data for clustering analysis.
# 
# Version X
# Created on MM/DD/YYYY by xxxxxxxx
# ========================================================================================

# ========================================================================================
# Use the prep_data, PCA, and k_means scripts to analyze this data!   
# ========================================================================================
# Load the necessary functions
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/prep_data_for_clustering.R")
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/PCA.R")
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/k-means.R")

# ========================================================================================
# Load QC-ed NHANES items or totals    
# ========================================================================================

# Load the QC-ed and sampled totals file. 
  totals_QCed_1500 <- read.table("NHANES_totals_QCed_1500.txt", sep="\t", header=T)

  colnames(totals_QCed_1500)

# OR load the QC-ed and sampled food items file.
  foods_QCed_30 <- read.table("NHANES_foods_QCed_30.txt", sep="\t", header=T)
  

###### CHOOSE EITHER 1 OR 2 OF THE FOLLOWING: 1: WITHOUT AVEAGING; 2: WITH AVERAGING. #######

  # ---------------------------------------------------------------------------------------------------------------
  # 1. If using each dataponit as is WITHOUT AVERAGING, 
  # Subset totals or food items data.
  
    # FOOD ITEMS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Define the input data to be used.
       input_data <- foods_QCed_30
      # The columns specified as start.col, end.col, and all columns in between will be selected.
      # Items   --> start.col = "DR1IPROT",     end.col = "DR1IP226"
      SubsetColumns(data=input_data, start.col="DR1IPROT", end.col = "DR1IP226")
       
    # TOTALS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # Define the input data to be used.
       input_data <- totals_QCed_1500
      # The columns specified as start.col, end.col, and all columns in between will be selected.
      # Totals  --> start.col = "DR1TPROT",    end.col = "DR1TP226"
      SubsetColumns(data=input_data, start.col="DR1TPROT", end.col = "DR1TP226")

    # The output is a df called "subsetted".
    
    # pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
    # The removed columns will be shown if any.
    KeepNonZeroVarColumns(data = subsetted)
    # The out put is a df called "subsetted_non0var", to be used in the subsequent
    
    # collapse by correlation procedure.
    colnames(subsetted_non0var)
  # ---------------------------------------------------------------------------------------------------------------
    
### No. 2 below is not needed, I think, because the average is totals, and as is is food items.
###    since there is only 1 day of NHANES data. 
  # 
  # # ---------------------------------------------------------------------------------------------------------------
  # #  2. If taking average of the food items of each user for each of the nutrients.
  #   
  # # Define the input data to be used.
  #   input_data <- foods_QCed_30
  #   input_data <- totals_QCed_1500
  #   colnames(input_data)
  #   
  #   # Totals  --> start.col = "DR1TPROT",    end.col = "DR1TP226"
  #   SubsetColumns(data=input_data, start.col="DR1TPROT", end.col = "DR1TP226")
  #   # Items   --> start.col = "DR1IPROT",     end.col = "DR1IP226"
  #   SubsetColumns(data=input_data, start.col="DR1IPROT", end.col = "DR1IP226")
  #   # The output is a df called "subsetted".
  #   
  #   AverageBy(data = subsetted, by = "SEQN", start.col = "DR1TKCAL", end.col = "DR1TP226")
  #   
  #   # The column names should be the same as start.col - end.col. 
  #   colnames(meansbycategorydf)
  #   
  #   # pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  #   # The removed columns will be shown if any.
  #   KeepNonZeroVarColumns(data = subsetted)
  #   
  #   # "subsetted_non0var" is the dataframe to be used in the subsequent collapse by correlation procedure.
  #   
  # # ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
  cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                   min.cor = 0.75, 
                                   select.rep.fcn = 'mean', verbose = T)
  
  # Filter out highly correlated variables from the original dataset.  
  selected_variables <- subsetted_non0var[, cbc_res$reps]
  
  # ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***
  
  # Check to see the name of the original and filtered variables. 
  # Among the variables in the same group, the one with the highest variance is kept 
  #  (according to the explanation above.)
  # filtered
  head(selected_variables, 1)     
  dim(selected_variables)     
  
  # original
  head(subsetted_non0var, 1)
  dim(subsetted_non0var)
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
  # Save the correlation matrix for record in the results folder.
  # cc is the correlation matrix produced when variables are collapsed by correlation. 
  SaveCorrMatrix(x=cc, out.fn = "NHANEStotals_corr_matrix.txt")
# ---------------------------------------------------------------------------------------------------------------
  

  #
  
  
  
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# 2. If taking average of each user for each of the nutrients.
  
  # Items   --> start.col = "DR1IPROT",     end.col = "DR1IP226"
  # Totals  --> start.col = "DR1TPROT",    end.col = "DR1TP226"
  SubsetColumns(data=totals_QCed_1500, start.col="DR1TPROT", end.col = "DR1TP226")
  # The output is a df called "subsetted".
  
  AverageBy(data = subsetted, by = "SEQN", start.col = "DR1TKCAL", end.col = "DR1TP226")

# The column names should be the same as start.col - end.col. 
  colnames(meansbycategorydf)

# pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
  # The removed columns will be shown if any.
  KeepNonZeroVarColumns(data = subsetted)

# "subsetted_non0var" is the dataframe to be used in the subsequent
  # collapse by correlation procedure.
  colnames(subsetted_non0var)  
  
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
# need to rename the data so that they will be recognized by the functions. 
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
# For most cases, CARB > SUGR, so it's possible that CARB includes SUGR (it should...) 
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

