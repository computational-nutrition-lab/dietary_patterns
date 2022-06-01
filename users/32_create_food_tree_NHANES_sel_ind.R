# ========================================================================================
# Create food tree with NHANES data. AFTER SELECTING INDIVIDUALS USED IN QCTOTAL.
# Version 1
# Created on 06/01/2022 by Rie Sadohara
# ========================================================================================

  setwd("~/GitHub/dietary_patterns")

# ========================================================================================
# Load source scripts
# ========================================================================================
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")

# After running Scenario A in 06_load_clean_NHANES_food.R, 'eg_data/NHANES/Food_D12_FC_cc_f.txt' should have been created.
# This has Day 1 AND Day 2 food and nutrient data and their Main.Food.Description of 
# individuals who are adults, reported >1 food items/day, and has complete record for both days.

# "Food_D12_FC_cc_f.txt" contains all the info. SEQN, FoodAmt, Main.food.description, FoodCode.
# This is too large as is, so use FilterDBByDietRecords() function to 
# remove unnecessary columns. All rows will be kept. 

# ========================================================================================
# Keep only the individuals in QC-ed total data (optional; if wish to be
#  consistent with clustering analysis using 'QCtotal') 
# ========================================================================================
# Load the calculated and QC-ed total (after Scenario B-4). 
  QCtotal_a <- read.table("eg_data/NHANES/Total_D12_FC_mean_QC_1.txt", sep="\t", header=T)
  selectedind <- unique(QCtotal_a$SEQN)

# Load the input file to be filtered.
  wwww <- read.table("eg_data/NHANES/Food_D12_FC_cc_f.txt", sep="\t", header=T)
  colnames(wwww)
# Select only the individuals listed in 'selectedind'. 
  ssss <- wwww[wwww$SEQN %in% selectedind, ]
  
# Confirm 
  identical(length(unique(ssss$SEQN)), length(selectedind))
  
# Save. This will be the input in the following procedures.
  write.table(ssss, "eg_data/NHANES/Food_D12_FC_cc_f_sel.txt", sep="\t", row.names=F, quote=F)
  
# ========================================================================================
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input) 
# ========================================================================================
# Keep only the foods reported in your study. This is already done, but need to run this 
#  so that the data will be formatted in a compatible way to create food tree.
  FilterDbByDietRecords(food_database_fn = "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
                        food_records_fn  = "eg_data/NHANES/Food_D12_FC_cc_f_sel.txt",   # output of FormatFoods.  
                        output_fn =        "eg_data/NHANES/Food_D12_FC_cc_f_sel_red.txt")
  
# CheckDB and ensure no food reported Food_D12_FC_cc_f.txt in is missing in the database.
  
  # check if there is any food item reported by people but are missing in the database. 
  check.db(food_database_fn = "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
           food_records_fn =  "eg_data/NHANES/Food_D12_FC_cc_f_sel_red.txt", 
           output_fn =        "eg_data/NHANES/Food_D12_FC_cc_f_sel_red_missing.txt")
  #  the output "eg_data/NHANES/Food_D12_FC_cc_f_missing.txt" dose not contain any data. 
  
# Does the output contain anything? 
  rrr = read.table("eg_data/NHANES/Food_D12_FC_cc_f_sel_red_missing.txt", sep="\t", header=T)
  head(rrr)
  # Has something ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty         ===> put NULL in addl_foods_fn argument of MakeFoodTree.

# Create food tree with the reduced dataset (only reported foods).   
  MakeFoodTree(nodes_fn="data/Food_tree_data/NodeLabelsMCT.txt", # can use this for now. 
               addl_foods_fn = NULL, 
               num.levels = 5,
               food_database_fn=    "eg_data/NHANES/Food_D12_FC_cc_f_sel_red.txt",  
               output_tree_fn=      "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.txt", 
               output_taxonomy_fn = "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.taxonomy.txt"
               ) 

# --------------------------------------------------------------------------------------------------------------
  # Viz food tree.  
  source("lib/viz_food_tree.r")
  
  # Load your ggtree object. 
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv2.txt")
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv3.txt")
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv4.txt")
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.txt")
  
  PrepFoodTreePlots(input.tree=tree)
  
  # Create a color-coded and annotated food tree with 9 L1 levels.
  # Choose either 'circular' or 'radial' for layout.
  # It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="radial")
  
  # Look at the color-coded and annotated food tree, saved as tree_an_hi_o_rt.
  tree_an_hi_o_rt

# --------------------------------------------------------------------------------------------------------------
# Generate OTU tables for downstream analyses; MAY TAKE SOME TIME
# It is OK to see the following warning message:
    # In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
    # appending column names to file

# Make the standard food otu table with data in gram weights of food.
  MakeFoodOtu(food_records_fn=  "eg_data/NHANES/Food_D12_FC_cc_f_sel.txt", # need to supply data that have 'FoodAmt' before applying FilterDBByDietRecords.  
              food_record_id =  "SEQN",                       # Specify the ID of your participants
              food_taxonomy_fn= "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.taxonomy.txt",  # Specify your taxonomy file produced by MakeFoodTree.
              output_fn =       "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.food.otu.txt")  # Name your output otu file.
  
# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "eg_data/NHANES/Food_D12_FC_cc_f_sel.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.taxonomy.txt", 
               output_fn=        "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal
  MakeDhydrtOtu(food_records_fn=  "eg_data/NHANES/Food_D12_FC_cc_f_sel.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.taxonomy.txt", 
                output_fn =       "results/Food_tree_NHANES/Food_D12_FC_cc_f_sel_red_Lv5.dhydrt.otu.txt")


# ========================================================================================
# OR you can create a food tree from all the possible food items.
# Same as in 32_create_food_tree_NHANES.R
# ========================================================================================
  MakeFoodTree(nodes_fn="data/Food_tree_data/NodeLabelsMCT.txt", # can use this for now. 
               addl_foods_fn =   NULL, 
               num.levels = 2,
               food_database_fn= "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
               output_tree_fn=      "results/Food_tree_NHANES/NHANESDatabase_Lv2.txt", 
               output_taxonomy_fn = "results/Food_tree_NHANES/NHANESDatabase_Lv2.taxonomy.txt"
               )
  
  # --------------------------------------------------------------------------------------------------------------
  # Viz food tree.  
  source("lib/viz_food_tree.r")
  
  # Load your ggtree object. 
  tree <- read.tree("results/Food_tree_NHANES/NHANESDatabase_Lv2.txt")
  PrepFoodTreePlots(input.tree=tree)
  VizFoodTree(input.tree=tree, layout="radial")
  tree_an_hi_o_rt
  