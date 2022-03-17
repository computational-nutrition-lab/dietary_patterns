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
# Calculate totals by hand if any correction was made in Items.
# ========================================================================================  

 ########## Need to make it into a function #############
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

  # Merge the data and metadata.
  merged <- merge(x=Items_raw, y=metadata_1, by="UserName", all.x=T)
  tail(merged, 2)
  
  selected_data <- subset(merged, Remove!="yes")
  head(selected_data,1)
  colnames(selected_data)
  dim(selected_data)

  write.table(selected_data, "selectedbyhand.txt", row.names=F, sep="\t", quote=F)
  aaa = read.table("selectedbyhand.txt", header=T, sep="\t")
  
  head(aaa, 2)
  head(Items_raw, 2)
  head(metadata_1, 2)
  colnames(Items_raw)
  toberemoved = subset(Items_raw, Remove=='yes')

  
  # create sample dataframe
  sample_data <- data.frame( name= c("Geeks1", "Geeks2", "Geeks3",
                                     "Geeks4", "Geeks5", "Geeks6"),
                             value= c( 11, 15, 10, 23, 32, 53 ) )
  colnames(sample_data)
  # write dataframe into a space separated text file
  write.table( sample_data, file='sample.txt', sep="\t", quote=F, row.names = F)
  read.table('sample.txt', sep="\t", header=T)
