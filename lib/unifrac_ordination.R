# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1
# Created on 03/08/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Load the packages and functions needed.
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Prep data behind the scene...
# prep food data.
  PrepFood <- function(data=food){
    
    # remove the taxonomy column
    food2 <- data[, !colnames(data) == "taxonomy"] 
    
    # transform to matrix, then a phyloseq object
    food_mat <<- as.matrix(food2)
    OTU <<- phyloseq::otu_table(food_mat, taxa_are_rows = TRUE)
  } 
  
# prep taxonomy (food names) data
  PrepTax <- function(data=tax){  # split taxonomy into 5 levels by default.  need to check if it works with other dataset than
    
    # Make the food description as row names
    row.names(data) <- data$Main.food.description 
    
    # remove the FoodID column
    woFoodID <<- data[, !colnames(data) == "FoodID"]

    # Split taxonomy L1, L2, L3 etc. by a semicolon, in lieu of tidyr::separate
    splittax <<- strsplit(as.character(tax$taxonomy), split=";")
    # How many levels were created after splitting? 
    max_n_levels <<- max(lengths(splittax))
    
    tax1 <<- woFoodID
    n <- 1
    
    if(max_n_levels==1){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        n <- n + 1
      }
    }else if(max_n_levels==2){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        n <- n + 1
      }
    }else if(max_n_levels==3){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        n <- n + 1
      }
    }else if(max_n_levels==4){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        n <- n + 1
      }
    }else if(max_n_levels==5){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        tax1[n, 'L5'] <- i[[5]]
        n <- n + 1
      }
    }else if(max_n_levels==6){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        tax1[n, 'L5'] <- i[[5]]
        tax1[n, 'L6'] <- i[[6]]
        n <- n + 1
      }
    }else if(max_n_levels==7){
      for(i in strsplit(as.character(tax$taxonomy), split=';')){
        tax1[n, 'L1'] <- i[[1]]
        tax1[n, 'L2'] <- i[[2]]
        tax1[n, 'L3'] <- i[[3]]
        tax1[n, 'L4'] <- i[[4]]
        tax1[n, 'L5'] <- i[[5]]
        tax1[n, 'L6'] <- i[[6]]
        tax1[n, 'L7'] <- i[[7]]
        n <- n + 1
      }
    }else{
      cat("The number of levels are beyond the range of 1-6. Please check your input taxonomy file.")
    }
        
    # remove Main.food.description column, because it's already made into row names.
    woFoodID2 <<- tax1[, !colnames(tax1) == "Main.food.description"]   

    # Transform to matrix, then to a tax_table object.
    tax_mat <<- as.matrix(woFoodID2)
    TAX <<- phyloseq::tax_table(tax_mat)
  }
  
# prep metadata.
  PrepMeta <- function(data=meta){

    #subset metadata to the correct samples
    meta2 <<- data[colnames(food), ]
    
    # Transform meta2 to sample_data object.
    SAMPLES <<- phyloseq::sample_data(meta2)
  }
    
# prep foodtree.
  PrepTree <- function(data=foodtree){
    
    TREE <<- data
    
    # Replace '_' with spaces in the tree object.
    taxa_names(TREE) <<- gsub('_', ' ', taxa_names(data))
  }

# ---------------------------------------------------------------------------------------------------------------
# Merge metadata and Axis values.
  MergeAxesAndMetadata <- function(ord.object, number.of.axes=4, meta=meta){
    
    # extract all the Axis vectors
    allvectors <- as.data.frame(ord.object["vectors"])
    
    # extract Axes 1 through the specified axis
    vectors <- allvectors[, 1:number.of.axes]
    
    # Sort by the Axis.1 values.
    sortedv <<- vectors[order(vectors$vectors.Axis.1, decreasing = T), ]
    
    # make rownames as a column for merging.
    meta3 <<- meta
    meta3$IDforMerging    <<- rownames(meta)
    sortedv1 <<- sortedv
    sortedv1$IDforMerging <<- rownames(sortedv)

    # Match IDforMerging (sample ID) and UserName.
    axes_and_meta <<- merge(sortedv1, meta3, by="IDforMerging", all.x=T)
  }
  
  
  
  
# ---------------------------------------------------------------------------------------------------------------
# 
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# 

# ---------------------------------------------------------------------------------------------------------------
  
