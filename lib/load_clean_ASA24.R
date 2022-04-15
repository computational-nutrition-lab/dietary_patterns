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
  source("~/GitHub/dietary_patterns/lib/specify_dir_and_check_col.R")

# Abby's code: https://github.com/knights-lab/dietstudy_analyses/blob/master/data/diet/raw_and_preprocessed_ASA24_data/lib/Clean_diet_data.R

# ========================================================================================
# Calculate totals by hand if any correction was made in Items.
# ========================================================================================  

  GenerateTotals <- function(items.data=Items_raw, User.Name='UserName', 
                             Recall.No='RecallNo'){
    
    # Get the index number of "FoodAmt" in dataframe items.data.
    FoodAmt_Index_No  <- grep("FoodAmt", colnames(items.data)) 
    A_DRINKS_Index_No <- grep("A_DRINKS", colnames(items.data)) 
    
    # Subset the necessary columns.
    Items_raw3 <<- items.data[, c(User.Name, 
                                  Recall.No,
                                  names(items.data)[FoodAmt_Index_No:A_DRINKS_Index_No])]
    
    # Change column names to be recognized by the loop below. 
    colnames(Items_raw3)[1:2] <<- c('UserName', 'RecallNo') 
    
    # Define variables to calculate Totals for.
    myvar <<- names(Items_raw3[, -c(1,2)])
    myvar
    # Create an empty list to store results.
    results <- list()
  
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

# Flag outliers in the target column and min max values, and pop out a prompt asking whether to delete 
  # the outliers or not.
  # Whether the outliers are removed or not, the output will be a df called 'QCtotals'.
  QCOutliers <- function(input.data, target.colname, min, max){
    temp <- input.data
    
    # the nth column has the variable of interest.
    nth_column <- which(colnames(temp) == target.colname )
    
    # Extact rows that are NOT in the range of min-max.
    Outlier_rows <<- temp[ temp[, nth_column] < min | temp[, nth_column] > max  , ]
    
    # Report how many rows are outside the min-max range.
    cat("There are", nrow(Outlier_rows), "observations with <", min, "or >", max, ". \n", sep = " ") 
    
    if(nrow(Outlier_rows) == 0){ 
      QCtotals <<- temp
      cat("There are no outlier rows, but the input data was renamed as QCtotals.\n",
          nrow(QCtotals), "rows remained.\n")}
    else{
      answer <- askYesNo("Remove?")
      if(answer==T){
        # Save rows that are within the range of min-max as QCtotal.
        QCtotals <<- temp[ temp[, nth_column] >= min & temp[, nth_column] <= max , ]
        cat("Outlier rows were removed; the cleaned data is saved as an object called \"QCtotals\".\n",
            nrow(QCtotals), "rows remained.\n")
      }else{
        QCtotals <<- temp
        cat("Outlier rows were not removed, but the input data was renamed as QCtotals.\n",
            nrow(QCtotals), "rows remained.\n")}
    }
  }
  
# ================= TO BE DELETED ===========================================================================
# # KCAL
# # Adult women (>= 12 yo) <600 or >4400
# # Adult   men (>= 12 yo) <650 or >5700
#  # Flag if KCAL is <600 or >5700 --> ask remove or not --> if yes, remove those rows
#   KCALOutliers <- function(totals.data, target.column, min = 600, max = 5700){
#     temp <- totals.data
#     KCAL_outlier_rows <<- subset(temp, KCAL < min | KCAL > max)
#     cat("There are", nrow(KCAL_outlier_rows), "observations with <", min, "kcal/day or >", max, "kcal/day. \n", sep = " ") 
#     if(nrow(KCAL_outlier_rows) == 0){ cat("\n")}
#     else{ 
#           print(KCAL_outlier_rows)   # Show the outlier rows # Some totals have 'RecallNo', others 'SutdyDayNo'...  
#           answer <- askYesNo("Remove?")
#           if(answer==T){
#             QCtotals <<- subset(temp, KCAL >= min & KCAL <= max)
#           }
#         }
#   }
# # ---------------------------------------------------------------------------------------------------------------
# 
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Protein 
#   # Adult women (>= 12 yo) <10 or >180
#   # Adult   men (>= 12 yo) <25 or >240
#   
#   # Flag if PROT is <10 or >240 --> ask remove or not --> if yes, remove those rows
#   PROTOutliers <- function(totals.data, min = 10, max = 240){
#     temp <- totals.data
#     PROT_outlier_rows <<- subset(temp, PROT < min | PROT > max)
#     cat("There are", nrow(PROT_outlier_rows), "observations with <", min, "PROT/day or >", max, "PROT/day. \n", sep = " ") 
#     if(nrow(PROT_outlier_rows) == 0){ cat("\n")}
#     else{ 
#             print(PROT_outlier_rows)   # Show the outlier rows
#             answer <- askYesNo("Remove?")
#             if(answer==T){
#               QCtotals <<- subset(temp, PROT >= min & PROT <= max)
#             }
#         }
#   }
# # ---------------------------------------------------------------------------------------------------------------
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Fat 
#   # Adult women (>= 12 yo) <15 or >185
#   # Adult   men (>= 12 yo) <25 or >230
#   
#   # Flag if TFAT is <15 or >230 --> ask remove or not --> if yes, remove those rows
#   TFATOutliers <- function(totals.data, min = 15, max = 230){
#     temp <- totals.data
#     TFAT_outlier_rows <<- subset(temp, TFAT < min | TFAT > max)
#     cat("There are", nrow(TFAT_outlier_rows), "observations with <", min, "TFAT/day or >", max, "TFAT/day. \n", sep = " ") 
#     if(nrow(TFAT_outlier_rows) == 0){ cat("\n")}
#     else{ 
#           print(TFAT_outlier_rows)   # Show the outlier rows
#           answer <- askYesNo("Remove?")
#           if(answer==T){
#             QCtotals <<- subset(temp, TFAT >= min & TFAT <= max)
#       }
#     }
#   }
# # ---------------------------------------------------------------------------------------------------------------
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Vitamin C 
#   # Adult women (>= 12 yo) <5 or >350
#   # Adult   men (>= 12 yo) <5 or >400
# 
#   # Flag if VC (Vitamin C) is <5 or >400 --> ask remove or not --> if yes, remove those rows
#   VCOutliers <- function(totals.data, min = 5, max = 400){
#     temp <- totals.data
#     VC_outlier_rows <<- subset(temp, VC < min | VC > max)
#     cat("There are", nrow(VC_outlier_rows), "observations with <", min, "VC/day or >", max, "VC/day. \n", sep = " ") 
#     if(nrow(VC_outlier_rows) == 0){ cat("\n")}
#     else{ 
#       print(VC_outlier_rows)   # Show the outlier rows
#       answer <- askYesNo("Remove?")
#       if(answer==T){
#         QCtotals <<- subset(temp, VC >= min & VC <= max)
#       }
#     }
#   }
# # ---------------------------------------------------------------------------------------------------------------
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Beta-carotene 
#   # Adult women (>= 12 yo) <15 or >7100
#   # Adult   men (>= 12 yo) <15 or >8200
#   
#   # Flag if BCAR (beta carotene) is <15 or >8200 --> ask remove or not --> if yes, remove those rows
#   BCAROutliers <- function(totals.data, min = 15, max = 8200){
#     temp <- totals.data
#     BCAR_outlier_rows <<- subset(temp, BCAR < min | BCAR > max)
#     cat("There are", nrow(BCAR_outlier_rows), "observations with <", min, "BCAR/day or >", max, "BCAR/day. \n", sep = " ") 
#     if(nrow(BCAR_outlier_rows) == 0){ cat("\n")}
#     else{ 
#       print(BCAR_outlier_rows)   # Show the outlier rows
#       answer <- askYesNo("Remove?")
#       if(answer==T){
#         QCtotals <<- subset(temp, BCAR >= min & BCAR <= max)
#       }
#     }
#   }
 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to QC rows of 'totals' by Metadata
# Show which has "yes" in the "Remove" column, and remove them. 
  RemoveRows <- function(data, metadata.file, output.name){
    toberemoved <<- subset(metadata.file, Remove=="yes")
    
    cat(nrow(toberemoved), "row(s) below are to be removed:", "\n")
    print(toberemoved) 
    
    # Merge the data and metadata.
    merged <<- merge(x=data, y=metadata.file, by="UserName", all.x=T)
    
    # Remove the rows that have "yes" in the "Remove" column.
    selected_data <<- subset(merged, Remove!="yes")
    
    # Save externally.
    write.table(selected_data, output.name, sep="\t", row.names=F, quote=F)
  }
# ---------------------------------------------------------------------------------------------------------------

  