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

# ========================================================================================
# Calculate [total grams/eating occasion], grouped by individuals, day, eating occasion.
# ========================================================================================  
# 
  # dplyr version ==============================================================
  # library(dplyr)
  #  Items_by_User_Occ <-
  #   Items_raw %>%
  #   select(UserName, RecallNo, Occ_No, 26:128) %>%  # Select necessary columns
  #   group_by(UserName, RecallNo, Occ_No) %>%
  #   summarise_each(funs(sum))
  # # Add a unique ID for each line for merging.
  #  Items_by_User_Occ$uniqueid <- paste0(Items_by_User_Occ$UserName, "_",
  #                                       Items_by_User_Occ$RecallNo, "_",
  #                                       Items_by_User_Occ$Occ_No)
  #  colnames(Items_by_User_Occ)
  #  head(Items_by_User_Occ)
  # =============================================================================
  
  # Base R version ==============================================================
  
# This produces a dataframe called Items_by_User_Occ.
  SumByOccasion <- function(items.data, User.Name, 
                            Recall.No, Occ.No){
    
      # Get the index number of "FoodAmt" in dataframe Items_raw.
      FoodAmt_Index_No  <- grep("FoodAmt", colnames(items.data)) 
      A_DRINKS_Index_No <- grep("A_DRINKS", colnames(items.data)) 
      
      # Subset necessary columns.
      Items_raw2 <<- items.data[, c(User.Name, 
                                    Recall.No, 
                                    Occ.No, 
                                    names(items.data)[FoodAmt_Index_No:A_DRINKS_Index_No] )]
      
      # Change column names so that it will be recognized in the for loop below.
      colnames(Items_raw2)[1:3] <<- c('UserName','RecallNo', 'Occ_No')
  
      # Define variables to calculate means for.
      myvar <- names(Items_raw2[, -c(1,2,3)])
      myvar
      # Create an empty list to store results.
      results <- list()
  
      # Calculate totals of each variable for each combination of User x Date x Occasion.
      for(i in 1:length(myvar)){
        if(i==1){
          subsetted <- Items_raw2[, c('UserName','RecallNo', 'Occ_No', myvar[i])]
          restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3],
                                data=subsetted, FUN = sum)
          colnames(restable) <- c('UserName', 'RecallNo', 'Occ_No', paste(myvar[i]))
          restable$User_Day_OccNo <- paste(restable$UserName, restable$RecallNo, restable$Occ_No, sep = "_")
          results[[i]] <- restable
          Items_by_User_Occ <<- restable
        }else if(i>1){
          subsetted <- Items_raw2[, c('UserName','RecallNo', 'Occ_No', myvar[i])]
          restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3],
                                data=subsetted, FUN = sum)
          colnames(restable) <- c('UserName', 'RecallNo', 'Occ_No', paste(myvar[i]))
          restable$User_Day_OccNo <- paste(restable$UserName, restable$RecallNo, restable$Occ_No, sep = "_")
          restable_sub <- restable[, c(5, 4)]  # take only User_Day_OccNo and means.
          results[[i]] <- restable_sub
          Items_by_User_Occ <<- merge(Items_by_User_Occ, results[[i]], by="User_Day_OccNo", all=T)
          # all=T takes care of missing data ... inserts NA for combinations not found
        }
      }
  }
 
  
# -----------------------------------------------------------------------------------------  
#  A separate function to pick up UserName, Day, Occ_No, Occ_Name to match Occ No. and Occ_names_in_word.
  
  # Get the type of occasion (breakfast, just a drink etc.) by user & by occasion.
  AddOccNames <- function(items.data, User.Name='UserName', 
                          Recall.No='RecallNo', Occ.No='Occ_No', Occ.Name='Occ_Name'  ){
      
      subsetted2 <<- items.data[, c(User.Name,  Recall.No,  Occ.No,  Occ.Name)]
      
      # Get Occ_Name (which is numeric) for each occasion. Taking mean because Occ_Name is the same 
      # for the same occasion.
      Occ_Names <<- aggregate(subsetted2[, 4] ~ subsetted2[, 1] + subsetted2[, 2] +  subsetted2[, 3],
                              data=items.data,
                              FUN=mean)
      
      colnames(Occ_Names) <<- c('UserName', 'RecallNo', 'Occ_No', 'Occ_Name')
  
      # Add a unique ID for each line for merging.
      Occ_Names$User_Day_OccNo <<- paste0(Occ_Names$UserName, "_",
                                          Occ_Names$RecallNo, "_",
                                          Occ_Names$Occ_No)
      
      # Take only User_Day_OccNo and Occ_Name.
      Occ_Names_2 <<- Occ_Names[, c("User_Day_OccNo", "Occ_Name")]
      
      # Make a reference table that has Occ_Name and corresponding types.
      Occ_Names_ref <<- data.frame(Occ_Name=seq(1:8),
                                   Occ_In_Words=c("Breakfast", "Brunch",
                                                  "Lunch",     "Dinner",
                                                  "Supper",    "Snack",
                                                  "Just a drink", "Just a supplement" ))
     
       # Combine!
      # Match the Occ_Names with the Occ_Names_ref (~VLOOKUP)
      Occ_Names_and_Words <<- merge(x=Occ_Names_2, y=Occ_Names_ref, by="Occ_Name", all.x=T)
      
      # Combine the 2 tables so that the sums of each occasion, occ numbers, and occ names in word
      #  will be in one table.
      Sum_by_User_Day_Occ <<- merge(x=Items_by_User_Occ, y=Occ_Names_and_Words, by='User_Day_OccNo', all.x=T)
  }    
# -----------------------------------------------------------------------------------------  


  
###### This code below works as is, when not made into a function. ##########################################  
  # Get the index number of "FoodAmt" in dataframe Items_raw.
  # FoodAmt_Index_No  <- grep("FoodAmt", colnames(Items_raw)) 
  # A_DRINKS_Index_No <- grep("A_DRINKS", colnames(Items_raw)) 
  # 
  # # Subset necessary columns.
  # Items_raw2 <- subset(Items_raw, select = c(UserName, 
  #                                            RecallNo, 
  #                                            Occ_No, 
  #                                            FoodAmt_Index_No:A_DRINKS_Index_No))
  # 
  # head(Items_raw2[, 1:3], 10)
  
  # # Define variables to calculate means for.
  # myvar <- names(Items_raw2[, -c(1,2,3)])
  # myvar
  # # Create an empty list to store results.
  # results <- list()
  # 
  # # Calculate totals of each variable for each combination of User x Date x Occasion. 
  #  for(i in 1:length(myvar)){
  #    if(i==1){
  #      subsetted <- Items_raw2[, c('UserName','RecallNo', 'Occ_No', myvar[i])]
  #      restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3], 
  #                            data=subsetted, FUN = sum)
  #      colnames(restable) <- c('UserName', 'RecallNo', 'Occ_No', paste(myvar[i]))
  #      restable$User_Day_OccNo <- paste(restable$UserName, restable$RecallNo, restable$Occ_No, sep = "_")
  #      results[[i]] <- restable
  #      Items_by_User_Occ <<- restable
  #      # restable_sub <- restable[, c(5, 4)]  
  #      # results[[i]] <- restable_sub 
  #      # Items_by_User_Occ <<- restable_sub
  #    }else if(i>1){
  #      subsetted <- Items_raw2[, c('UserName','RecallNo', 'Occ_No', myvar[i])]
  #      restable <- aggregate(subsetted[, 4] ~ subsetted[, 1] + subsetted[, 2] + subsetted[, 3], 
  #                            data=subsetted, FUN = sum)
  #      colnames(restable) <- c('UserName', 'RecallNo', 'Occ_No', paste(myvar[i]))
  #      restable$User_Day_OccNo <- paste(restable$UserName, restable$RecallNo, restable$Occ_No, sep = "_")
  #      restable_sub <- restable[, c(5, 4)]  # take only User_Day_OccNo and means.
  #      results[[i]] <- restable_sub 
  #      Items_by_User_Occ <<- merge(Items_by_User_Occ, results[[i]], by="User_Day_OccNo", all=T) 
  #       # all=T takes care of missing data ... inserts NA for combinations not found
  #    }
  #  }
  
# # ========================================================================================
# # Get the type of occasion (breakfast, just a drink etc.) by user & by occasion.
# # ========================================================================================  
# # 
#   # dplyr version ====================================================================
#   #  Occ_Names <- 
#   #    Items_raw %>% 
#   #    select(UserName, RecallNo, Occ_No, Occ_Name) %>% 
#   #    group_by(UserName, RecallNo, Occ_No) %>% 
#   #    summarise_each(funs(mean)) 
#   # write.table(Occ_Names_1, "clipboard", sep = '\t')
#    
#   # Base R version ====================================================================
#   # Get the occasion names for each of the User x Day x Occasion combinations.
#   Occ_Names <- aggregate(Occ_Name ~ UserName + RecallNo + Occ_No,
#                          data=Items_raw,
#                          FUN=mean)
#    # Occasion Name will be the same for each occasion, so just taking the mean to 
#    # pick up that Occ_Name (which is numeric) per each occasion.
# 
#   # Add a unique ID for each line for merging.
#    Occ_Names$User_Day_OccNo <- paste0(Occ_Names$UserName, "_", 
#                                 Occ_Names$RecallNo, "_",
#                                 Occ_Names$Occ_No)
#    head(Occ_Names, 10)
#    
#   # Take only User_Day_OccNo and Occ_Name. 
#    Occ_Names_2 <- Occ_Names[, c("User_Day_OccNo", "Occ_Name")]
#    

# # ========================================================================================
# # Make a reference table that has Occ_Name and corresponding types.
# # ========================================================================================  
# # 
#    Occ_Names_ref <- data.frame(Occ_Name=seq(1:8),
#                                Occ_In_Words=c("Breakfast", "Brunch", 
#                                               "Lunch",     "Dinner", 
#                                               "Supper",    "Snack", 
#                                               "Just a drink", "Just a supplement" ))
#    Occ_Names_ref

# # ========================================================================================
# # Combine!
# # ========================================================================================  
#    
# # Match the Occ_Names with the Occ_Names_ref (VLOOKUP)
#   Occ_Names_and_Words <- merge(x=Occ_Names_2, y=Occ_Names_ref, by="Occ_Name", all.x=T)
#   Occ_Names_and_Words
# 
# # Combine the 2 tables so that the sums of each occasion, occ numbers, and occ names in word 
# #  will be in one table. 
#   Sum_by_User_Day_Occ <- merge(x=Items_by_User_Occ, y=Occ_Names_and_Words, by='User_Day_OccNo', all.x=T) 
#   head(Sum_by_User_Day_Occ)
  
  # # Save as a csv file.
  # write.csv(Sum_by_User_Day_Occ, 'Sum_by_User_Day_Occ.csv')
  # # This will be useful if a researcher wants to look at the sum of each eating occasion 
  # # per participant. (Because Totals file sums all the occasions in one day.)
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Calculate totals by hand if any correction was made in Items.
# ========================================================================================  

 ########## Need to make it into a function #############
  GenerateTotals <- function(items.data=Items_raw, User.Name='UserName', 
                             Recall.No='RecallNo'){
    
    # Get the index number of "FoodAmt" in dataframe items.data.
    FoodAmt_Index_No  <- grep("FoodAmt", colnames(items.data)) 
    A_DRINKS_Index_No <- grep("A_DRINKS", colnames(items.data)) 
    
    # Subset necessary columns.
    Items_raw3 <<- items.data[, c(User.Name, 
                                  Recall.No,
                                  names(items.data)[FoodAmt_Index_No:A_DRINKS_Index_No])]
    
    # Change column names to be recognized by the loop below. 
    colnames(Items_raw3)[1:2] <<- c('UserName', 'RecallNo') 
    
    # Define variables to calculate Totals for.
    myvar <<- names(Items_raw3[, -c(1,2)])
    myvar
    # Create an empty list to store results.
    results <<- list()
  
    # Calculate totals of each variable for each combination of User x Date x Occasion. 
    for(i in 1:length(myvar)){
      if(i==1){
        subsetted <- Items_raw3[, c('UserName','RecallNo', myvar[i])]
        restable <- aggregate(subsetted[, 3] ~ subsetted[, 1] + subsetted[, 2], 
                              data=subsetted, FUN = sum)
        colnames(restable) <- c('UserName', 'RecallNo', paste(myvar[i]))
        restable$User_Day <- paste(restable$UserName, restable$RecallNo, sep = "_")
        results[[i]] <- restable
        New_Totals <<- restable
      }else if(i>1){
        subsetted <- Items_raw3[, c('UserName','RecallNo', myvar[i])]
        restable <- aggregate(subsetted[, 3] ~ subsetted[, 1] + subsetted[, 2], 
                              data=subsetted, FUN = sum)
        colnames(restable) <- c('UserName', 'RecallNo', paste(myvar[i]))
        restable$User_Day <- paste(restable$UserName, restable$RecallNo, sep = "_")
        restable_sub <- restable[, c(4, 3)]  # take only User_Day and means.
        results[[i]] <- restable_sub 
        New_Totals <<- merge(New_Totals, results[[i]], by="User_Day", all=T) 
        # all=T takes care of missing data ... inserts NA for combinations not found
      }
    }
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find nutrient outliers in totals.
# Cut-points are based on the 5th and 95th percentile of intakes from NHANES data.

# KCAL
  # Adult women (>= 12 yo) <600 or >4400
  # Adult   men (>= 12 yo) <650 or >5700
  
  # Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
  KCALOutliers <- function(totals.data, min = 600, max = 5700){
    KCAL_outlier_rows <<- subset(totals.data, KCAL < min | KCAL > max)
    cat("There are", nrow(KCAL_outlier_rows), "observations with <", min, "kcal/day or >", max, "kcal/day. \n", sep = " ") 
    if(nrow(KCAL_outlier_rows) == 0){ cat("\n")}
    else{ 
          print(KCAL_outlier_rows)   # Show the outlier rows # Some totals have 'RecallNo', others 'SutdyDayNo'...  
          answer <- askYesNo("Remove?")
          if(answer==T){
            totals.data <- subset(totals.data, KCAL >= min & KCAL <= max)
          }
        }
  }
# ---------------------------------------------------------------------------------------------------------------

  
# ---------------------------------------------------------------------------------------------------------------
# Protein 
  # Adult women (>= 12 yo) <10 or >180
  # Adult   men (>= 12 yo) <25 or >240
  
  # Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
  PROTOutliers <- function(totals.data = New_Totals, min = 10, max = 240){
    PROT_outlier_rows <<- subset(totals.data, PROT < min | PROT > max)
    cat("There are", nrow(PROT_outlier_rows), "observations with <", min, "PROT/day or >", max, "PROT/day. \n", sep = " ") 
    if(nrow(PROT_outlier_rows) == 0){ cat("\n")}
    else{ 
            print(PROT_outlier_rows)   # Show the outlier rows
            answer <- askYesNo("Remove?")
            if(answer==T){
              totals.data <- subset(totals.data, PROT >= min & PROT <= max)
            }
        }
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Fat 
  # Adult women (>= 12 yo) <15 or >185
  # Adult   men (>= 12 yo) <25 or >230
  
  # Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
  TFATOutliers <- function(totals.data = New_Totals, min = 15, max = 230){
    TFAT_outlier_rows <<- subset(totals.data, TFAT < min | TFAT > max)
    cat("There are", nrow(TFAT_outlier_rows), "observations with <", min, "TFAT/day or >", max, "TFAT/day. \n", sep = " ") 
    if(nrow(TFAT_outlier_rows) == 0){ cat("\n")}
    else{ 
          print(TFAT_outlier_rows)   # Show the outlier rows
          answer <- askYesNo("Remove?")
          if(answer==T){
            totals.data <- subset(totals.data, TFAT >= min & TFAT <= max)
      }
    }
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Vitamin C 
  # Adult women (>= 12 yo) <5 or >350
  # Adult   men (>= 12 yo) <5 or >400

  # Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
  VCOutliers <- function(totals.data = New_Totals, min = 5, max = 400){
    VC_outlier_rows <<- subset(totals.data, VC < min | VC > max)
    cat("There are", nrow(VC_outlier_rows), "observations with <", min, "VC/day or >", max, "VC/day. \n", sep = " ") 
    if(nrow(VC_outlier_rows) == 0){ cat("\n")}
    else{ 
      print(VC_outlier_rows)   # Show the outlier rows
      answer <- askYesNo("Remove?")
      if(answer==T){
        totals.data <- subset(totals.data, VC >= min & VC <= max)
      }
    }
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Beta-carotene 
  # Adult women (>= 12 yo) <15 or >7100
  # Adult   men (>= 12 yo) <15 or >8200
  
  # Flag if BCAR (beta carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
  BCAROutliers <- function(totals.data = New_Totals, min = 15, max = 8200){
    BCAR_outlier_rows <<- subset(totals.data, BCAR < min | BCAR > max)
    cat("There are", nrow(BCAR_outlier_rows), "observations with <", min, "BCAR/day or >", max, "BCAR/day. \n", sep = " ") 
    if(nrow(BCAR_outlier_rows) == 0){ cat("\n")}
    else{ 
      print(BCAR_outlier_rows)   # Show the outlier rows
      answer <- askYesNo("Remove?")
      if(answer==T){
        totals.data <- subset(totals.data, BCAR >= min & BCAR <= max)
      }
    }
  }
 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to QC rows of 'totals' by Metadata
# Show which has "yes" in the "Remove" column, and remove them. 
  RemoveRows <- function(data=totals, metadata.file=metadata_1){
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

