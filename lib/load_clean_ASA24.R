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
  
# Calculate [total grams/eating occasion], grouped by individuals, day, eating occasion.
  library(dplyr)
   Items_by_User_Occ <- 
    Items_raw %>% 
    select(UserName, IntakeStartDateTime, Occ_No, 26:128) %>%  # Select necessary columns  
    group_by(UserName, IntakeStartDateTime, Occ_No) %>% 
    summarise_each(funs(sum))
  
  # Add a unique ID for each line for merging.
   Items_by_User_Occ$uniqueid <- paste0(Items_by_User_Occ$UserName, "_", 
                                        Items_by_User_Occ$IntakeStartDateTime, "_",
                                        Items_by_User_Occ$Occ_No)
   Items_by_User_Occ
   

# Get the type of occasion (breakfast, just a drink etc.) by user by occasion.
   Occ_Names <- 
     Items_raw %>% 
     select(UserName, IntakeStartDateTime, Occ_No, Occ_Name) %>% 
     group_by(UserName, IntakeStartDateTime, Occ_No) %>% 
     summarise_each(funs(mean))  
   # Occasion Name will be the same for each occasion, so just taking the mean to 
   # pick up that Occasion Name per each occasion.

  # Add a unique ID for each line for merging.
   Occ_Names$uniqueid <- paste0(Occ_Names$UserName, "_", 
                                Occ_Names$IntakeStartDateTime, "_",
                                Occ_Names$Occ_No)
   Occ_Names
   
  # Take only uniqueid and Occ_Name. 
   Occ_Names_2 <- Occ_Names[, c("uniqueid", "Occ_Name")]
   
   
# Make a reference table that has Occ_Name and corresponding types.
   Occ_Names_ref <- data.frame(Occ_Name=seq(1:8),
                               Occ_In_Words=c("Breakfast", "Brunch", 
                                              "Lunch",     "Dinner", 
                                              "Supper",    "Snack", 
                                              "Just a drink", "Just a supplement" ))
   Occ_Names_ref
   

# Match the Occ_Nmes with the Occ_Names_ref (VLOOKUP)
  Occ_Names_and_Words <- merge(x=Occ_Names_2, y=Occ_Names_ref, by="Occ_Name", all.x=T)
  Occ_Names_and_Words

  
# Combine the 2 tables so that the sums of each occasion, occ numbers, and occ names in word 
# will be in one table. 

  Totals_to_check <- merge(x=Items_by_User_Occ, y=Occ_Names_and_Words, by="uniqueid", all.x=T)   

# ---------------------------------------------------------------------------------------------------------------
   
# ---------------------------------------------------------------------------------------------------------------

# Show drinks larger than 1/2 gallon (1892 grams) of OCC_NAME=7: 'just drinks'
# Show drinks larger than 1/2 gallon (1892 grams) of OCC_NAME=7: 'just drinks'
  
  

# Save as "Items_to_use.txt"


# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Calculate your Totals.



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

