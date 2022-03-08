# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1
# Created on 03/08/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Load the packages and functions needed.
# ========================================================================================

# Prep data behind the scene...
# prep food data.
  PrepFood <- function(data=food){
    wotax <- data[, !colnames(data) == "taxonomy"] # remove the taxonomy column
    food2 <<- wotax} 
  

# taxonomy (food names) data
  colnames(tax)
  head(tax, 2)
  
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
  }
  
  colnames(tax)
  woFoodID = head(tax, 20)
  # make a list of separated character strings in taxonomy. 
  mysplit = strsplit(as.character(woFoodID$taxonomy), split=';' )
  max(lengths(mysplit)) # what if the max is not 5, but 4 or something...??
  
  ####### RESUME FROM HERE ##########
   
  
  
  
  
 
  
# Transform food and tax to matrix
  food_mat <- as.matrix(food)
  tax_mat <- as.matrix(tax)
  
# prep metadata.
  #subset meta to the correct samples
  meta <- meta[colnames(food), ]
  head(meta)
  
  #transform to phyloseq objects
  OTU <- phyloseq::otu_table(food_mat, taxa_are_rows = TRUE)
  TAX <- phyloseq::tax_table(tax_mat)
  samples <- phyloseq::sample_data(meta)
  
# prep foodtree.
  ggtree(foodtree, layout = "circular") # it works!
  is(foodtree)
  head(OTU, 1)
  
  head(taxa_names(OTU)) # 'Milk', "Milk cows fluid whole" etc.
  head(taxa_names(TAX)) # 'Milk', "Milk cows fluid whole" etc.
  head(taxa_names(foodtree)) # "Milk_cows_fluid_whole" etc. need to replace underscores with spaces.
  
  # Replace '_' with spaces in the tree object.
  taxa_names(foodtree) <- gsub('_', ' ', taxa_names(foodtree))
  head(taxa_names(foodtree)) 


  
  

# ---------------------------------------------------------------------------------------------------------------
# Header 2 -- explain the purpose of this subsection. 
# Operations....
  myfunction(arg1 = mydata)  # comment in lower case

  head(mydata_analysis)
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# A function or a chunk of code to do one operation. 
  MyFunctionToRun <- function(arg.one = my_object_1, arg.two = my_object_2){
    #dothis <<- basefunction()  # comment in lower case
  }

# ---------------------------------------------------------------------------------------------------------------
  
