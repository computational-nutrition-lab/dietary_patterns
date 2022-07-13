# For USERS ==============================================================================

# ========================================================================================
# Generate a food tree from ASA24 data.
# Version 1
# Created on 02/17/2022 by Rie Sadohara
# ========================================================================================

# Before running the following code, one needs to have cleaned input data and obtained 
# 'New_totals' or 'totals', which are going to be used here. 

# The following code expects that input files are tab-delimited txt file. 
# Ensure your input files have no special characters that mess up loading:
#  "
#  '
#  #
#  &

  # use this working directory until this script is complete. 
  setwd("~/GitHub/dietary_patterns")

# Folder structure 
# 
#                          |----- data ---- Food_tree_data
#                          |
#                          |----- eg_data 
#                          |
#                          |----- lib --- source codes are here
#  Main -------------------|
#   (dietary_patterns)     |----- users --- this script is here
#                          |
#                          |----- results ---- Food_tree_results
#                          |
#                          |----- ...
#

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# Come back to the main directory
  setwd(main_wd)   
  
# ========================================================================================
# Load source scripts
# ========================================================================================
  source("lib/specify_dir_and_check_col.R")
  
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")

# ========================================================================================
# Prep data
# ========================================================================================

# Current ASA24 database doesn't have modcodes, so de-duplicate database file, 
# replace special characters with "_", and create a new FoodID out of foodcode and modcode.
# It leaves all other columns intact.
  FormatFoods(input_fn="data/Food_tree_data/all.food.desc.txt", output_fn="data/Food_tree_data/ASA24Database.txt")

# FoodCode and Main.food.description of additional foods not in ASA24. Format it for use.
  FormatFoods(input_fn="data/Food_tree_data/Soylent_codes.txt", output_fn="data/Food_tree_data/Soylent_codes_formatted.txt")
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/")
  
# Format your items data, and save the formatted items file to the "Foodtree" folder.
  FormatFoods(input_fn="VVKAJ_Items_f_s_m.txt", output_fn="Foodtree/VVKAJ_Items_f_s_m_ff.txt", dedupe=F)


# ---------------------------------------------------------------------------------------------------------------
# Generate a food tree with the whole ASA24 database. 
  
# Come back to the main directory for now.
  setwd(main_wd)   
  
# Generate a tree with the whole ASA24 food database first. 
  # if there are missing foods, then create new files to add them in below under addl_foods
  MakeFoodTree(nodes_fn=        "data/Food_tree_data/NodeLabelsMCT.txt", 
               food_database_fn="data/Food_tree_data/ASA24Database.txt", 
               addl_foods_fn= c("data/Food_tree_data/Soylent_codes_formatted.txt"), 
               num.levels = 4,  # How many levels of foods to be classified
               output_taxonomy_fn = "results/Food_tree_ASA24/ASA24_Lv4.taxonomy.txt",  # Name your output taxonomy file
               output_tree_fn=      "results/Food_tree_ASA24/ASA24_Lv4.tree.nwk"       # Name your output tree
               )

# ---------------------------------------------------------------------------------------------------------------
# Generate a food tree with your own items dataset. 
  
# Specify the directory where the data is again.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Foodtree")
  
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input)
  FilterDbByDietRecords(food_database_fn = "../../../data/Food_tree_data/ASA24Database.txt", 
                        food_records_fn  = "VVKAJ_Items_f_s_m_ff.txt",   # output of FormatFoods above.
                        output_fn        = "VVKAJ_Items_f_s_m_ff_database.txt")
  
# make a food tree with the reduced data.
  MakeFoodTree(nodes_fn=         "../../../data/Food_tree_data/NodeLabelsMCT.txt", 
               food_database_fn= "VVKAJ_Items_f_s_m_ff_database.txt",    # output for FilterDbByDietRecords above.
               addl_foods_fn   = NULL, 
               num.levels      = 3,
               output_taxonomy_fn = "VVKAJ_Items_f_s_m_ff_reduced_3Lv.tax.txt",
               output_tree_fn=      "VVKAJ_Items_f_s_m_ff_reduced_3Lv.tree.nwk" 
               )
  
# ---------------------------------------------------------------------------------------------------------------
# Generate standard, grams of fiber, and dehydrated grams per kcal OTU tables to be used later.
# Make the standard food otu table with data in gram weights of food.
  MakeFoodOtu(food_records_fn=  "VVKAJ_Items_f_s_m_ff.txt", 
              food_record_id =  "UserName",                       # Specify the ID of your participants
              food_taxonomy_fn= "VVKAJ_Items_f_s_m_ff_reduced_3Lv.tax.txt",  # Specify your taxonomy file produced by MakeFoodTree.
              output_fn =       "VVKAJ_Items_f_s_m_ff_reduced_3Lv.food.otu.txt")  # Name your output otu file.
  
# Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "VVKAJ_Items_f_s_m_ff.txt", 
               food_record_id=   "UserName", 
               food_taxonomy_fn= "VVKAJ_Items_f_s_m_ff_reduced_3Lv.tax.txt", 
               output_fn=        "VVKAJ_Items_f_s_m_ff_reduced_3Lv.fiber.otu.txt")
  
# Make a food otu table as dehydrated grams per kcal.
  MakeDhydrtOtu(food_records_fn=  "VVKAJ_Items_f_s_m_ff.txt", 
                food_record_id =  "UserName", 
                food_taxonomy_fn= "VVKAJ_Items_f_s_m_ff_reduced_3Lv.tax.txt", 
                output_fn =       "VVKAJ_Items_f_s_m_ff_reduced_3Lv.dhydrt.otu.txt")
  
# ---------------------------------------------------------------------------------------------------------------


  
  
  
  
  
  
  
  
  
  
  
    
########## BELOW IS OLD, MCT VERSEION OF IT. ############
  
  # ========================================================================================
  # Prep data
  # ========================================================================================
  
  # Current ASA24 database doesn't have modcodes, so de-duplicate database file, 
  # replace special characters with "_", and create a new FoodID out of foodcode and modcode.
  # It leaves all other columns intact.
  FormatFoods(input_fn="data/Food_tree_data/all.food.desc.txt", output_fn="data/Food_tree_data/ASA24Database.txt")
  
  # FoodCode and Main.food.description of additional foods not in ASA24. Format it for use.
  FormatFoods(input_fn="data/Food_tree_data/Soylent_codes.txt", output_fn="data/Food_tree_data/Soylent_codes_formatted.txt")
  
  # Format your items data. Output will be saved as dietrecords.txt.
  FormatFoods(input_fn="data/Food_tree_data/Items_to_use.txt", output_fn="data/Food_tree_data/dietrecords.txt", dedupe=F)
  
  
  # ---------------------------------------------------------------------------------------------------------------
  # Generate a tree with the whole ASA24 food database. 
  # if there are missing foods, then create new files to add them in below under addl_foods
  MakeFoodTree(nodes_fn=        "data/Food_tree_data/NodeLabelsMCT.txt", 
               food_database_fn="data/Food_tree_data/ASA24Database.txt", 
               addl_foods_fn= c("data/Food_tree_data/Soylent_codes_formatted.txt"), 
               num.levels = 2,  # How many levels of foods to be classified
               output_taxonomy_fn = "results/Food_tree_results/mct_Lv2.taxonomy.txt",  # Name your output taxonomy file
               output_tree_fn=      "results/Food_tree_results/mct_Lv2.tree.nwk"       # Name your output tree
  )
  
  # ---------------------------------------------------------------------------------------------------------------
  # Limit to just the foods reported in your study (formatted dietrecords.txt as the input)
  FilterDbByDietRecords(food_database_fn = "data/Food_tree_data/ASA24Database.txt", 
                        food_records_fn  = "data/Food_tree_data/dietrecords.txt",   # output of FormatFoods above.
                        output_fn = "data/Food_tree_data/MCTdatabase.txt")
  
  # make a food tree with the reduced data.
  MakeFoodTree(nodes_fn=         "data/Food_tree_data/NodeLabelsMCT.txt", 
               food_database_fn= "data/Food_tree_data/MCTdatabase.txt", 
               addl_foods_fn=    "data/Food_tree_data/Soylent_codes_formatted.txt", 
               num.levels = 1,
               output_taxonomy_fn = "results/Food_tree_results/mct.reduced_1Lv.taxonomy.txt",
               output_tree_fn=      "results/Food_tree_results/mct.reduced_1Lv.tree.nwk" 
  )
  
  # Make the standard food otu table with data in gram weights of food.
  MakeFoodOtu(food_records_fn=  "data/Food_tree_data/dietrecords.txt", 
              food_record_id =  "X.SampleID",                       # Specify the ID of your participants
              food_taxonomy_fn= "results/Food_tree_results/mct_Lv2.taxonomy.txt",  # Specify your taxonomy file produced by MakeFoodTree.
              output_fn =       "results/Food_tree_results/mct_Lv2.food.otu.txt")  # Name your output otu file.
  
  # Make a food otu table with data in grams of fiber per food
  MakeFiberOtu(food_records_fn=  "data/Food_tree_data/dietrecords.txt", 
               food_record_id=   "X.SampleID", 
               food_taxonomy_fn= "results/Food_tree_results/mct_Lv2.taxonomy.txt", 
               output_fn=        "results/Food_tree_results/mct_Lv2.fiber.otu.txt")
  
  # Make a food otu table as dehydrated grams per kcal
  MakeDhydrtOtu(food_records_fn=  "data/Food_tree_data/dietrecords.txt", 
                food_record_id =  "X.SampleID", 
                food_taxonomy_fn= "results/Food_tree_results/mct_Lv2.taxonomy.txt", 
                output_fn =       "results/Food_tree_results/mct_Lv2.dhydrt.otu.txt")
  
  # ---------------------------------------------------------------------------------------------------------------
  
  