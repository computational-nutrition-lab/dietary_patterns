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

    # Split taxonomy L1, L2, L3 etc. by a semicolon, in lieu of
    # tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4", "L5"), sep = ";")
      n <- 1
      for(i in strsplit(as.character(woFoodID$taxonomy), split=';')){
        woFoodID[n, 'L1'] <<- i[[1]]
        woFoodID[n, 'L2'] <<- i[[2]]
        woFoodID[n, 'L3'] <<- i[[3]]
        woFoodID[n, 'L4'] <<- i[[4]]
        woFoodID[n, 'L5'] <<- i[[5]]
        n <- n + 1
      }
      
    # remove Main.food.description column, because it's already made into row names.
    woFoodID2 <<- woFoodID[, !colnames(woFoodID) == "Main.food.description"]   

    # Transform to matrix, then to a tax_table object.
    tax_mat <<- as.matrix(woFoodID2)
    TAX <<- phyloseq::tax_table(tax_mat)
  }
  
      # make a list of separated character strings in taxonomy. 
      #mysplit = strsplit(as.character(woFoodID$taxonomy), split=';' )
      #max(lengths(mysplit)) # what if the max is not 5, but 4 or something...??
  

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
  
