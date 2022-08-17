library(vegan)
data(dune.env)
dune.env
attach(dune.env)
head(dune)

ord_cca <- cca(dune ~ A1 + Management, data=dune.env)
ord_rda <- rda(dune ~ A1 + Management, data=dune.env)
summary(ord_cca)
summary(ord_rda)

plot(ord_rda)
anova(ord_rda)
anova(ord_rda, by="term")
anova(ord_rda, by="mer") # marginal effects (type III effects)
anova(ord_rda, by="axis")

# ----------------------------------------------------------------------------------------------------------
# Excerpt of Abby's R markdown code

# Not sure where the map files are coming from.  
  map <- read.table("data/maps/SampleID_map.txt", sep = "\t", header = T, comment = "")
  map_username <- read.table("data/maps/UserName_map.txt", sep = "\t", header = T, comment = "" )

  map_ord <- map_username[map_username$UserName %in% rownames(tax_t),]

# set food vectors to weighted or unweighted
  food_vectors <- food_vectors_w

# env
  env <- merge(food_vectors, map_ord, by = 0)

#### unconstrained
  un_rda <- rda(tax_t)

#### constrained
# note use formula notation to include factors (i.e. gender)

# just diet
  rrda_d <- rda(tax_t ~ Grains + Poultry + Alcohol + Fats + Cakes , data = env)

# diet + metadata
  rrda_a <- rda(tax_t ~ Age + Gender + BMI + Grains + Poultry + Alcohol + Fats + Cakes , data = env)

# just metadata
  rrda_m <- rda(tax_t ~ Age + Gender + BMI, data = env)

# But the point is to have 
# tax_t ... transformed TAX data; that's my Y matrix.
# env - X matrix. and it has dietary data. 
# and build RDA formula using the Y and X matrices.

# ----------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------
# Practice RDA with vegan
  library(vegan)

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")
  
# Load the necessary scripts.
  source("../../../lib/unifrac_ordination.R")
  source("../../../lib/unifrac_ordination.R")
  
# Set working dir
  # SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/")
  setwd("~/GitHub/dietary_patterns/eg_data/NHANES/Laboratory_data/")
  
# ---------------------------------------------------------------------------------------------------------------
  # Load the necessary files for creating a phyloseq object.  
  
  # Food
  # Load food OTU table - this is our food OTU data
  # food <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_red_Lv5.dhydrt.otu.txt", row.names = 1)
  # food <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.dhydrt.otu.txt", row.names = 1)
  food <- read.delim("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.dhydrt.otu.txt", row.names=1)
  # food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.dhydrt.otu.txt", row.names = 1)
  # Format the food file and create a otu_table called OTU.
  PrepFood(data=food)
  food[1:10, 1:10]
  
  # Taxonomy (tax)
  # tax <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_red_Lv5.taxonomy.txt")
  # tax <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.taxonomy.txt")
  tax <- read.delim("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt")
  # tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.taxonomy.txt")
  # tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.taxonomy.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  
  # Sample
  # MCT
  # meta <- read.csv( "~/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", row.names = 1, check.names = F)
  
  # NHANES
  demog <- read.xport("../DEMO_I.XPT")
  colnames(food)  # SEQN starts with 'X'
  
  # demog needs to have XSEQN as rownames in order to use it in PrepMeta function. 
  rownames(demog) <- paste("X", demog$SEQN, sep="") # Add 'X' at the beginning
  head(demog)
  
  PrepMeta_NHANES(data=demog)
  # Error in `[.data.frame`(data, , "SampleID") : undefined columns selected
  # Need to create PrepMeta for NHANES. because the current PrepMeta function uses
  # "SampleID", that is the combination of UserName and Day of ASA24, but  
  # NHANES data do not have such a naming scheme.
  
  PrepMeta_NHANES <- function(data=meta){
    
    # make UserName as rownames of meta.
    # rownames(data) <- data[, "SampleID"]
    
    # subset metadata to the correct samples.
    # colnames(food) has users.  
    meta2 <<- data[colnames(food), ]
    
    # Transform meta2 to sample_data object.
    SAMPLES <<- phyloseq::sample_data(meta2)
  }
  
  
  
