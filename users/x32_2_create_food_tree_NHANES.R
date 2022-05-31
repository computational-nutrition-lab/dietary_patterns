# ========================================================================================
# notes on 05/31/2022.
# Now that food tree codes are completed in another R script, this script may no longer 
# be needed.
# I keep this just in case, but making foodtree should work without this script.
# ========================================================================================

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
  
# ========================================================================================
# Import NHANES data
# https://github.com/knights-lab/Food_Tree/blob/master/R/lib/import_NHANES_data.R
# ========================================================================================

### Abby's code
  library(Hmisc)
  library(dplyr)
  
  library(SASxport)
  # library(foreign)
  
  # read in data for 2011-2012 dietary intake
  # food1 <- read.xport("data/NHANES/DR1IFF_G.XPT")
  food1 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1IFF_I.XPT")
  # food2 <- read.xport("data/NHANES/DR2IFF_G.XPT")
  food2 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR2IFF_I.XPT")
  # tots1 <- read.xport("data/NHANES/DR1TOT_G.XPT")
  tots1 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1TOT_I.XPT")
  # tots2 <- read.xport("data/NHANES/DR2TOT_G.XPT")
  tots2 <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR2TOT_I.XPT")
  
  # demo <- read.xport("data/NHANES/DEMO_G.XPT")
  demo <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  
  adults <- demo[demo$RIDAGEYR >= 18,]
  
  # read in food description files
  # foodcodes <- sasxport.get("data/NHANES/DRXFCD_G.XPT")
  foodcodes <- read.xport("eg_data/NHANES/FoodCodes_DRXFCD_I.XPT")
  head(foodcodes)
     # There is no modcodes for NHANES1516...
     modcodes <-  sasxport.get("data/NHANES/DRXMCD_G.XPT")
     
  
# Quality filtering
# did the food recall meet the minimum criteria to be considered reliable?
  food1 <- food1[food1$DR1DRSTZ == 1,] # this drops people with unreliable records and breastfed children
  food2 <- food2[food2$DR2DRSTZ == 1,]
    
# Naming notes
  # food codes: "DR1IFDCD" 
  # mod codes: "DR1MC" ?
  # weight in grams: "DR1IGRMS"
  
# fix naming for downstream food tree use
  # names(food1)[names(food1) == "dr1ifdcd"] <- "FoodCode"
  names(food1)[names(food1) == "DR1IFDCD"] <- "FoodCode"
  names(food2)[names(food2) == "DR2IFDCD"] <- "FoodCode"
  # names(food1)[names(food1) == "DR1MC"] <- "ModCode"  # No such column.
  # names(food2)[names(food2) == "DR2MC"] <- "ModCode"
  names(food1)[names(food1) == "DR1IGRMS"] <- "FoodAmt"
  names(food2)[names(food2) == "DR2IGRMS"] <- "FoodAmt"
  
  names(foodcodes)[names(foodcodes) == "DRXFDCD"] <- "FoodCode"
  names(foodcodes)[names(foodcodes) == "DRXFCLD"] <- "Main.food.description"
  # names(modcodes)[names(modcodes) == "DRXMC"] <- "ModCode" 

  
# Save foodcodes with modified columnnames so that it can be used further downstream.
  write.table(foodcodes, "eg_data/NHANES1516/nhanes1516.foodcodes.txt", sep="\t", quote=F, row.names=F)

  # Format the food table and save it as a .txt file.
  PrepareFoodCodeTable(raw.food.code.table = "eg_data/NHANES/FoodCodes_DRXFCD_I.XPT", 
                       out.fn =              "eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt")  
  
  # Load the formatted food code table.
  foodcodetable_f <- read.table("eg_data/NHANES/FoodCodes_DRXFCD_I_f.txt", sep="\t", header=T)
  head(foodcodetable_f)
  DRXFCSD
  
# Take care of spcecial characters.
  FormatFoods(input_fn = "eg_data/NHANES1516/nhanes1516.foodcodes.txt", output_fn = "eg_data/NHANES1516/nhanes1516.foodcodes_f.txt", dedupe=F)
  
# Load the formatted foods 
  formattedfoodcode <- read.table("eg_data/NHANES1516/nhanes1516.foodcodes_f.txt", sep="\t", header=T)
  #
  
  
  
  
  
  
  
  
  
  
  
  
  
    
## add main food description/or mod code to the raw data
# food1
  food1$FoodCode <- as.factor(food1$FoodCode)
  # food1$ModCode <- as.factor(food1$ModCode)
  head(food1, 1)
  
  # food1 <- left_join(food1, foodcodes, by = "FoodCode")
  food1 <- merge(x=food1, y=foodcodes, by="FoodCode", all.x=T) # Careful, this will sort it by FoodCode.

      # food1 <- left_join(food1, modcodes, by = "ModCode")
      # replace names
      # food1$Main.food.description <- ifelse(is.na(food1$drxmcd), food1$Main.food.description, food1$drxmcd)
  
# food2
  food2$FoodCode <- as.factor(food2$FoodCode)
  # food2$ModCode <- as.factor(food2$ModCode)
  # food2 <- left_join(food2, foodcodes, by = "FoodCode")
  food2 <- merge(x=food2, y=foodcodes, by="FoodCode", all.x=T) # Careful, this will sort it by FoodCode.
      # food2 <- left_join(food2, modcodes, by = "ModCode")
      # replace names
      # food2$Main.food.description <- ifelse(is.na(food2$drxmcd), food2$Main.food.description, food2$drxmcd)
  head(food2,2)
  
# subset to just people in both food1 and food2
  food1names <- unique(food1$SEQN)
  food2names <- unique(food2$SEQN)
  keepnames <- food1names[food1names %in% food2names]
  keepnames_adults <- keepnames[keepnames %in% adults$SEQN]


  
  
  
    
# subset to just the variables we need for food tree
  food1 <- food1 %>% select(SEQN, FoodCode, FoodAmt, Main.food.description)
  food2 <- food2 %>% select(SEQN, FoodCode, FoodAmt, Main.food.description)
  subset(food1, SEQN=="88925")
  subset(food2, SEQN=="88925")
  
# make a day varabile before we bind these together
  food1$Day = 1
  food2$Day = 2
    
  food12 <- rbind(food1, food2)
  subset(food12, SEQN=="88925")
  
  
# limit to just people who have records from day 1 and 2
  food12 <- food12[food12$SEQN %in% keepnames_adults,]
  
# look for people with no foods
  sum(table(food12$SEQN, food12$Day)[,1] == 1) # No people with only one food reported
  sum(table(food12$SEQN, food12$Day)[,2] == 1)  # 4 people with only one food reported
  
# dietary pattern/special diet map
  # diet_type_map <- tots1 %>% select(SEQN, drqsdiet, drqsdt1, drqsdt2, drqsdt3, drqsdt4, drqsdt5, drqsdt6, drqsdt7, drqsdt8, drqsdt9, drqsdt10, drqsdt11, drqsdt12, drqsdt91)
  diet_type_map <- tots1 %>% select(SEQN, DRQSDIET, DRQSDT1, DRQSDT2, DRQSDT3, DRQSDT4, DRQSDT5, DRQSDT6, DRQSDT7, DRQSDT8, DRQSDT9, DRQSDT10, DRQSDT11, DRQSDT12, DRQSDT91)
  diet_type_map <- diet_type_map[diet_type_map$SEQN %in% keepnames_adults,]
  
# demographics map
  demo <- demo[demo$SEQN %in% keepnames_adults, ]
  head(demo, 1)
  
# Need to format food12 so that it will be saved properly... replace special characters with '_'...
  # FormatFoods(input_fn="eg_data/NHANES1516/processed/foodcodes.txt", 
  #            output_fn="eg_data/NHANES1516/processed/NHANESDatabase.txt") 
  #  "
  #  '
  #  #
  #  &
  
  # food12
  table(food12$Day)
  dim(food12)
  food12$Day # there should not be any NAs in Day column... 
  
  tail(food12$Main.food.description)
  
  
  ### RESUME FROM HERE ###
  
  
  
  
  
  
  
  
  
  
# write to text file (tab separated)
  write.table(food1, file = "eg_data/NHANES1516/processed/foodday1.txt", sep = "\t", quote = F, row.names = F)
  write.table(food2, file = "eg_data/NHANES1516/processed/foodday2.txt", sep = "\t", quote = F, row.names = F)
  write.table(food12, file ="eg_data/NHANES1516/processed/foodday1and2.txt", sep = "\t", quote = F, row.names = F)
  
# database
  head(foodcodes)
  write.table(foodcodes, file = "eg_data/NHANES1516/processed/foodcodes.txt", sep = "\t", quote = F, row.names = F)
  
# write maps
  write.table(diet_type_map, file = "eg_data/NHANES1516/processed/diet_type_map.txt", sep = "\t", quote = F, row.names = F)
  write.table(demo, file = "eg_data/NHANES1516/processed/demo_map.txt", sep = "\t", quote = F, row.names = F)
    
  
# ---------------------------------------------------------------------------------------------------------------
  
# ========================================================================================
# Build food tree with NHANES data
# https://github.com/knights-lab/Food_Tree/blob/master/R/bin/make.nhanes.tree.r 
# ========================================================================================
  
  source("lib/Food_tree_scripts/newick.tree.r")
  source("lib/Food_tree_scripts/check.db.r")
  source("lib/Food_tree_scripts/format.foods.r")
  source("lib/Food_tree_scripts/filter.db.by.diet.records.r")
  source("lib/Food_tree_scripts/make.food.tree.r")
  source("lib/Food_tree_scripts/make.food.otu.r")
  source("lib/Food_tree_scripts/make.fiber.otu.r")
  source("lib/Food_tree_scripts/make.dhydrt.otu.r")
  
# clean up files so they work with the tree building downstream
  FormatFoods(input_fn="eg_data/NHANES1516/processed/foodcodes.txt", 
              output_fn="eg_data/NHANES1516/processed/NHANESDatabase.txt") # build database from main FoodCodes
 #format.foods(input_fn="data/NHANES/processed/individual.foods.from.deceased_NHANES_2007-2010.txt", output_fn="data/NHANES/dietrecords.txt", dedupe=F)

  # This is the input. food items file. 
  qqq = read.table("E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/processed/individual.foods.from.NHANES_2007-2010.txt", sep="\t", header=T)
  head(qqq,1)
  
  # ORIGINAL 
  FormatFoods(input_fn= "data/NHANES/processed/individual.foods.from.NHANES_2007-2010.txt", output_fn="data/NHANES/dietrecords_all.txt", dedupe=F)
  # Mine
  FormatFoods(input_fn="E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/processed/individual.foods.from.NHANES_2007-2010.txt", 
               output_fn="E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/processed/dietrecords_all_1.txt", dedupe=F)
  # What does the output look like..
  ppp = read.table("E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/processed/dietrecords_all_1.txt", sep="\t", header=T)
  head(ppp,1)
  # Old.Main.food.description and FoodID added.
  
  
  # check if there is any food item reported by people but are missing in the database. 
  check.db(food_database_fn = "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
           food_records_fn =  "E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/processed/dietrecords_all_1.txt", 
           output_fn = "E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/missing_1.txt")
  # What does the output look like?
  rrr = read.table("E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/missing_1.txt", sep="\t", header=T)
  head(rrr)
  
  MakeFoodTree(nodes_fn="data/Food_tree_data/NodeLabelsMCT.txt", 
                 addl_foods_fn =   "E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/missing_1.txt",
                 food_database_fn= "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
                 output_tree_fn=   "results/Food_tree_NHANES/NHANES1516_Lv5.txt", 
                 output_taxonomy_fn = "results/Food_tree_NHANES/NHANES1516_Lv5.taxonomy.txt",
                 num.levels=5)
  
# ---------------------------------------------------------------------------------------------------------------
# Limit to just the foods reported in your study (formatted dietrecords.txt as the input)
  FilterDbByDietRecords(food_database_fn = "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
                        food_records_fn  = "E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/processed/dietrecords_all_1.txt",   # output of FormatFoods above.
                        output_fn = "eg_data/NHANES1516/processed/NHANES1516FilteredDatabase.txt")
  
  
# make a food tree with the reduced data.
  MakeFoodTree(nodes_fn=         "data/Food_tree_data/NodeLabelsMCT.txt", 
               food_database_fn= "eg_data/NHANES1516/processed/NHANES1516FilteredDatabase.txt", 
               addl_foods_fn=    NULL, 
               num.levels =      5,
               output_taxonomy_fn = "results/Food_tree_NHANES/NHANES1516.reduced_Lv5.taxonomy.txt",
               output_tree_fn=      "results/Food_tree_NHANES/NHANES1516.reduced_Lv5.tree.nwk" 
  )
  
  
  
  #
  
  
  
  
  
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
