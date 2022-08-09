# ===============================================================================================================
# Generate food tree out of GLU - males in their 50s
# Version 1
# Created on 08/08/2022 by Rie Sadohara
# ===============================================================================================================

# use this working directory until this script is complete. 
  setwd("~/GitHub/dietary_patterns")

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Come back to the main directory
  setwd(main_wd)
  
# ===============================================================================================================
# Load source scripts
# ===============================================================================================================
  source("lib/specify_dir_and_check_col.R")
  
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")
  
# Modify 33_create_food_tree_NHANES_diffdiet9.R to:
  # 1. select individuals, and
  # 2. generate food tree. 

# Specify where the data is.
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data")
  
# Load the males50s people. Note this is a total data (1 row/person).
  glu_3_males50s <- read.table("QCtotalANDglu_body_meta_demo_males50s.txt", 
                               sep="\t", header=T)
  
# Make the individuals as a vector.   
  selectedind <- glu_3_males50s$SEQN
  
# Load the input file to be filtered.
  all.food.record <- read.table("../Food_D12_FC_cc_f.txt", sep="\t", header=T)
  colnames(all.food.record)

# Select only the individuals listed in 'selectedind'. 
  ssss <- all.food.record[all.food.record$SEQN %in% selectedind, ]
  
# Confirm 
  identical(length(unique(ssss$SEQN)), length(selectedind))
  
# Save. This will be the input in the following procedures.
  write.table(ssss, "Food_D12_FC_cc_f_males50s.txt", sep="\t", row.names=F, quote=F)  
  
  
# ===============================================================================================================
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input) 
# ===============================================================================================================
# Keep only the foods reported in your study. This is already done, but need to run this 
  #  so that the data will be formatted in a compatible way to create food tree.
  FilterDbByDietRecords(food_database_fn = "../../NHANES1516/processed/NHANESDatabase.txt", 
                        food_records_fn  = "Food_D12_FC_cc_f_males50s.txt",   # output of FormatFoods.  
                        output_fn =        "Food_D12_FC_cc_f_males50s_red.txt")
  
  # CheckDB and ensure no food reported Food_D12_FC_cc_f.txt in is missing in the database.
  
  # Check if there is any food item reported by people but are missing in the database. 
  check.db(food_database_fn = "../../NHANES1516/processed/NHANESDatabase.txt", 
           food_records_fn =  "Food_D12_FC_cc_f_males50s_red.txt", 
           output_fn =        "Food_D12_FC_cc_f_males50s_red_missing.txt")
  #  the output "eg_data/NHANES/Food_D12_FC_cc_f_missing.txt" does not contain any data. 
  
  # Does the output contain anything? 
  mmm = read.table("Food_D12_FC_cc_f_males50s_red_missing.txt", sep="\t", header=T)
  head(mmm)
  # Has something ===> put this missing.txt file in addl_foods_fn argument of MakeFoodTree.
  # Empty         ===> put NULL in addl_foods_fn argument of MakeFoodTree.
  
  # Create food tree with the reduced dataset (only reported foods).   
  MakeFoodTree(nodes_fn="../../../data/Food_tree_data/NodeLabelsMCT.txt", # can use this for now. 
               addl_foods_fn = NULL, 
               num.levels = 4,
               food_database_fn =   "Food_D12_FC_cc_f_males50s_red.txt",  
               output_tree_fn =     "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.nwk", 
               output_taxonomy_fn = "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt"
  ) 
  

# ===============================================================================================================
# Viz food tree.  
# ===============================================================================================================

# --------------------------------------------------------------------------------------------------------------
  source("../../../lib/viz_food_tree.r")
  
# Load your ggtree object. 
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv2.txt")
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv3.txt")
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv4.txt")
  tree <- read.tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.txt")
  
  tree <- read.tree("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.nwk")
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with 9 L1 levels.
# Choose either 'circular' or 'radial' for layout.
# It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="radial")  

  annotated_tree
  
# --------------------------------------------------------------------------------------------------------------
# Generate OTU tables for downstream analyses; MAY TAKE SOME TIME
# It is OK to see the following warning message:
# In write.table(fiber.otu, output_fn, sep = "\t", quote = F, append = TRUE) :
# appending column names to file
  
# Make the standard food otu table with data in gram weights of food.
  MakeFoodOtu(food_records_fn=  "Food_D12_FC_cc_f_males50s.txt", # need to supply data that have 'FoodAmt' before applying FilterDBByDietRecords.  
              food_record_id =  "SEQN",                       # Specify the ID of your participants
              food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt",  # Specify your taxonomy file produced by MakeFoodTree.
              output_fn =       "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.food.otu.txt")  # Name your output otu file.
  
# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "Food_D12_FC_cc_f_males50s.txt", 
               food_record_id=   "SEQN", 
               food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt", 
               output_fn=        "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal
  MakeDhydrtOtu(food_records_fn=  "Food_D12_FC_cc_f_males50s.txt", 
                food_record_id =  "SEQN", 
                food_taxonomy_fn= "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt", 
                output_fn =       "Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.dhydrt.otu.txt")  

# ---------------------------------------------------------------------------------------------------------------

  
# ---------------------------------------------------------------------------------------------------------------
  
