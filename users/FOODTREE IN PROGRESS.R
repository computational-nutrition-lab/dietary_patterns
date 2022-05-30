# Trial and error of food tree building using NHANES data.

# "eg_data/NHANES/Food_D12_FC_cc_f.txt" contains all the info.  SEQN, FoodAmt, Main.food.description, FoodCode.
# Can I use this to create food tree??

# CheckDB and ensure no food reported Food_D12_FC_cc_f.txt in is missing in the database.

# Below is food tree building code from elsewhere. Edit as necessary. 

#### RESUME FROM HERE #####


# check if there is any food item reported by people but are missing in the database. 
check.db(food_database_fn = "eg_data/NHANES1516/processed/NHANESDatabase.txt", 
         food_records_fn =  "eg_data/NHANES/Food_D1_FC_cc_f.txt", 
         output_fn = "eg_data/NHANES/Food_D1_FC_cc_f_missing.txt")
#  the output "eg_data/NHANES/Food_D1_FC_cc_f_missing.txt" dose not contain any data. 

# What does the output look like?
rrr = read.table("E:/MSU OneDrive 20210829/UMinn/Food_tree_unused/data/NHANES/missing_1.txt", sep="\t", header=T)
head(rrr)

MakeFoodTree(nodes_fn="data/Food_tree_data/NodeLabelsMCT.txt", # can use this for now. 
             addl_foods_fn =   NULL,
             food_database_fn= "eg_data/NHANES/Food_D1_FC_cc_f.txt", 
             output_tree_fn=      "results/Food_tree_NHANES/Food_D1_FC_cc_f_Lv2.txt", 
             output_taxonomy_fn = "results/Food_tree_NHANES/Food_D1_FC_cc_f_Lv2.taxonomy.txt",
             num.levels = 2
             )

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
