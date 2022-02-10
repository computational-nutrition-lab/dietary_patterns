# ========================================================================================
# The purpose of this script.
# Version 1
# Created on 02/04/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Load ASA24 data
# ========================================================================================
# 
# Load necessary scripts
# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")

# Abby's code: https://github.com/knights-lab/dietstudy_analyses/blob/master/data/diet/raw_and_preprocessed_ASA24_data/lib/Clean_diet_data.R

# ---------------------------------------------------------------------------------------------------------------
# Load your Items data 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  

# Load your Items data.
# Ensure there are no special characters that mess up loading:
#  "
#  '
#  #
#  &
  
  Items_raw <- read.csv("VVKAJ_2021-11-09_7963_Items_NoCommas.csv", sep = ",", header=T)
  dim(Items_raw)
  head(Items_raw, 2)
  Items_raw[, 130]

# ========================================================================================
# Calculate [total grams/eating occasion], grouped by individuals, day, eating occasion.
# ========================================================================================  
# 
  # dplyr version ==============================================================
  # library(dplyr)
  #  Items_by_User_Occ <- 
  #   Items_raw %>% 
  #   select(UserName, IntakeStartDateTime, Occ_No, 26:128) %>%  # Select necessary columns  
  #   group_by(UserName, IntakeStartDateTime, Occ_No) %>% 
  #   summarise_each(funs(sum))
  # # Add a unique ID for each line for merging.
  #  Items_by_User_Occ$uniqueid <- paste0(Items_by_User_Occ$UserName, "_", 
  #                                       Items_by_User_Occ$IntakeStartDateTime, "_",
  #                                       Items_by_User_Occ$Occ_No)
  #  colnames(Items_by_User_Occ)
  # =============================================================================
  
  # Base R version ==============================================================
  # Get the index number of "FoodAmt" in dataframe Items_raw.
  FoodAmt_Index_No <- grep("FoodAmt", colnames(Items_raw)) 
  A_DRINKS_Index_No <- grep("A_DRINKS", colnames(Items_raw)) 

  # Subset necessary columns.
  Items_raw2 <- subset(Items_raw, select = c(UserName, 
                                             IntakeStartDateTime, 
                                             Occ_No, 
                                             FoodAmt_Index_No:A_DRINKS_Index_No))
  
  head(Items_raw2)
  
  # Define variables to calculate means for.
  myvar <- names(Items_raw2[, -c(1,2,3)])
  myvar[1]
  # Create an empty list to store results.
  results <-  list()

  # Calculate totals of each variable for each combination of User x Date x Occasion. 
   for(i in 1:length(myvar)){
     if(i==1){
       subsetted <- Items_raw2[, c('UserName','IntakeStartDateTime', 'Occ_No', myvar[i])]
       # print(myvar[i])
       restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3], 
                             data=subsetted, FUN = mean)
       # print(myvar[i])
       colnames(restable) <- c('UserName', 'IntakeStartDateTime', 'Occ_No', paste(myvar[i]))
       # print(myvar[i])
       # print(restable)
       restable$User_Day_Occ <- paste(restable$UserName, restable$IntakeStartDateTime, restable$Occ_No, sep = "_")
       restable_sub <- restable[, c(5, 4)]  # take only User_Day_Occ and means.
       results[[i]] <- restable_sub 
       Items_by_User_Occ <<- restable_sub
     }else if(i>1){
       subsetted <- Items_raw2[, c('UserName','IntakeStartDateTime', 'Occ_No', myvar[i])]
       restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3], 
                             data=subsetted, FUN = mean)
       colnames(restable) <- c('UserName', 'IntakeStartDateTime', 'Occ_No', paste(myvar[i]))
       restable$User_Day_Occ <- paste(restable$UserName, restable$IntakeStartDateTime, restable$Occ_No, sep = "_")
       restable_sub <- restable[, c(5, 4)]  
       results[[i]] <- restable_sub 
       Items_by_User_Occ <<- merge(Items_by_User_Occ, results[[i]], by="User_Day_Occ", all=T) 
        # all=T takes care of missing data ... inserts NA for combinations not found
     }
   }
  
# ========================================================================================
# Get the type of occasion (breakfast, just a drink etc.) by user & by occasion.
# ========================================================================================  
# 
  # dplyr version ====================================================================
  #  Occ_Names <- 
  #    Items_raw %>% 
  #    select(UserName, IntakeStartDateTime, Occ_No, Occ_Name) %>% 
  #    group_by(UserName, IntakeStartDateTime, Occ_No) %>% 
  #    summarise_each(funs(mean)) 
  # write.table(Occ_Names_1, "clipboard", sep = '\t')
   
  # Base R version ====================================================================
  # Define variables to calculate means for.
  Occ_Names <- aggregate(Occ_Name ~ UserName + IntakeStartDateTime + Occ_No,
                         data=Items_raw,
                         FUN=mean)
   # Occasion Name will be the same for each occasion, so just taking the mean to 
   # pick up that Occasion Name per each occasion.

  # Add a unique ID for each line for merging.
   Occ_Names$uniqueid <- paste0(Occ_Names$UserName, "_", 
                                Occ_Names$IntakeStartDateTime, "_",
                                Occ_Names$Occ_No)
   head(Occ_Names)
   
  # Take only uniqueid and Occ_Name. 
   Occ_Names_2 <- Occ_Names[, c("uniqueid", "Occ_Name")]
   

# ========================================================================================
# Make a reference table that has Occ_Name and corresponding types.
# ========================================================================================  
# 
   Occ_Names_ref <- data.frame(Occ_Name=seq(1:8),
                               Occ_In_Words=c("Breakfast", "Brunch", 
                                              "Lunch",     "Dinner", 
                                              "Supper",    "Snack", 
                                              "Just a drink", "Just a supplement" ))
   Occ_Names_ref
   

# ========================================================================================
# Combine!
# ========================================================================================  
   
# Match the Occ_Nmes with the Occ_Names_ref (VLOOKUP)
  Occ_Names_and_Words <- merge(x=Occ_Names_2, y=Occ_Names_ref, by="Occ_Name", all.x=T)
  Occ_Names_and_Words

# Combine the 2 tables so that the sums of each occasion, occ numbers, and occ names in word 
#  will be in one table. 
  Totals_to_check <- merge(x=Items_by_User_Occ, y=Occ_Names_and_Words, by="uniqueid", all.x=T)   
  Totals_to_check
# ---------------------------------------------------------------------------------------------------------------
   

# ========================================================================================
# Check Totals_to_check for outliers.
# ========================================================================================  
  
# ---------------------------------------------------------------------------------------------------------------
# Show drinks (OCC_NAME=7: 'just drinks') 
#  larger than 1/2 gallon (1892 grams) 
  
  
  
# Show Snack foods (chips, nut, etc.; OCC_NAME=6: 'Snack') 
#  greater than or equal to 8 ounces by weight 
  

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Calculate your Totals.
# Totals: 1 row is 1 day of recall of 1 particiapnt. Sum by person, by day.
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find nutrient outliers in totals.
# Cut points are based on the 5th and 95th percentile of intakes from NHANES data.

# KCAL
  # Adult women (>= 12 yo) <600 or >4400
  # Adult   men (>= 12 yo) <650 or >5700

# Protein 
  # Adult women (>= 12 yo) <10 or >180
  # Adult   men (>= 12 yo) <25 or >240
  
# Fat 
  # Adult women (>= 12 yo) <15 or >185
  # Adult   men (>= 12 yo) <25 or >230

# Vitamin C 
  # Adult women (>= 12 yo) <5 or >350
  # Adult   men (>= 12 yo) <5 or >400
  
# Beta-carotene 
  # Adult women (>= 12 yo) <15 or >7100
  # Adult   men (>= 12 yo) <15 or >8200
  
# Save as "Totals_to_use.txt"
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Load your metadata files.  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
# SpecifyDataDirectory(directory.name = "eg_data/salt/")

# Load the totals.csv
  # totals <- read.table("Totals_to_use.txt", sep = "\t", header = T)
# If totals data is a csv:
# totals <- read.csv(list.files(pattern = '\\Totals.csv$'))
# Load the items.csv
  # items <- read.table("Items_to_use.txt", quote = "", sep = "\t", header = T)

# Load your metadata if you have one. 
  metadata_1 <- read.csv("Metadata_1.csv", header=T)
  metadata_2 <- read.csv("Food_map_txt_Metadata_2.csv", header=T)

# Come back to the main directory
  setwd(main.wd)
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to QC rows of 'totals' by Metadata
# Show which has "yes" in the "Remove" column, and remove them. 
  RemoveRows <- function(data=totals, metadata.file=metadata){
    toberemoved <<- subset(metadata.file, Remove=="yes")
    cat(nrow(toberemoved), "rows below are to be removed:", "\n")
    print(toberemoved)    
    # Merge the data and metadata.
    merged <<- merge(x=data, y=metadata.file, by="UserName", all.x=T)
    # Remove the rows that have "yes" in the "Remove" column.
    totals_selected <<- subset(merged, Remove!="yes")
    cat("The resulting file, totals_selected, has", 
        nrow(totals_selected), "rows and",
        ncol(totals_selected), "columns.")
  }
# ---------------------------------------------------------------------------------------------------------------

