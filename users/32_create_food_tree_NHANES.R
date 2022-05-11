# For USERS ==============================================================================

# ========================================================================================
# Generate a food tree from NHANES data.
# Version 1
# Created on 05/10/2022 by Rie Sadohara and Abby Johnson
# Abby's code https://github.com/knights-lab/Food_Tree/blob/master/R/lib/import_NHANES_data.R
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
  main.wd <- file.path(getwd())
  
  
  
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

### Abby's code
  library(Hmisc)
  library(dplyr)
  

  
  library(SASxport)
  # library(foreign)
  
 
  
  # read in SAS data for 2011-2012 dietary intake
  # food1 <- read.xport("data/NHANES/DR1IFF_G.XPT")
  food1 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Interview_IndFoods_Day1_DR1IFF_I.XPT")
  # food2 <- read.xport("data/NHANES/DR2IFF_G.XPT")
  food2 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Interview_IndFoods_Day2_DR2IFF_I.XPT")
  # tots1 <- read.xport("data/NHANES/DR1TOT_G.XPT")
  tots1 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Total_Nutrient_Day1_DR1TOT_J.XPT")
  # tots2 <- read.xport("data/NHANES/DR2TOT_G.XPT")
  tots2 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/Total_Nutrient_Day2_DR2TOT_J.XPT")
  
  # demo <- read.xport("data/NHANES/DEMO_G.XPT")
  demo <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  
  adults <- demo[demo$ridageyr >= 18,]
  
  # read in food description files
  # foodcodes <- sasxport.get("data/NHANES/DRXFCD_G.XPT")
  foodcodes <- read.xport("eg_data/NHANES/FoodCodes_DRXFCD_I.XPT")
     # There is no modcodes for NHANES1516...
     modcodes <-  sasxport.get("data/NHANES/DRXMCD_G.XPT")
  
# Quality filtering
# did the food recall meet the minimum criteria to be considered reliable?
  food1 <- food1[food1$dr1drstz == 1,] # this drops people with unreliable records and breastfed children
  food2 <- food2[food2$dr2drstz == 1,]
    
# Naming notes
# food codes: "dr1ifdcd" 
# mod codes: "dr1mc"
# weight in grams: "dr1igrms"
  
  # fix nameing for downstream food tree use
  # names(food1)[names(food1) == "dr1ifdcd"] <- "FoodCode"
  names(food1)[names(food1) == "DR1IFDCD"] <- "FoodCode"
  
  ### RESUME FROM HERE ###
  
  names(food2)[names(food2) == "dr2ifdcd"] <- "FoodCode"
  names(food1)[names(food1) == "dr1mc"] <- "ModCode"
  names(food2)[names(food2) == "dr2mc"] <- "ModCode"
  names(food1)[names(food1) == "dr1igrms"] <- "FoodAmt"
  names(food2)[names(food2) == "dr2igrms"] <- "FoodAmt"
  names(foodcodes)[names(foodcodes) == "drxfdcd"] <- "FoodCode"
  names(foodcodes)[names(foodcodes) == "drxfcld"] <- "Main.food.description"
  names(modcodes)[names(modcodes) == "drxmc"] <- "ModCode" 
  
  
  
  
#### ASA24 
  
  
# ========================================================================================
# Prep data
# ========================================================================================

# Current ASA24 database doesn't have modcodes, so de-duplicate database file, 
# replace special characters with _, and create a new FoodID out of foodcode and modcode.
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
