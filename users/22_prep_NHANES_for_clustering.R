# ========================================================================================
# The purpose of this script.
# Version X
# Created on MM/DD/YYYY by xxxxxxxx
# ========================================================================================

############################
JUST COPIED AND PASTED FROM MY NHANES2015_16.R. 
NEEDS SOME ORGANIZATION.
MAKE IT SIMILAR TO 20_PREP_ASA24_FOR_CLUSTERING
############################


# ========================================================================================
# Use the prep_data, PCA, and k_means scripts to analyze this data!   
# ========================================================================================
# Load the necessary functions
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/prep_data_for_clustering.R")
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/PCA.R")
source("C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/lib/k-means.R")

# Take average of each user for each of the nutrients.
# Nutrients analysis  --> start.col = "DR1IPROT",    end.col = "DR1IP226"
SubsetColumns(data=nhanes_sub1, start.col="", end.col)

AverageBy(data = nhanes_sub1, by = "SEQN", start.col = "DR1IPROT", end.col = "DR1IP226")

# The column names should be the same as start.col - end.col. 
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

