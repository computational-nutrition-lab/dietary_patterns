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
  food <- read.delim("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.dhydrt.otu.txt", row.names=1)
  PrepFood(data=food)
  food[1:10, 1:10]
  
  # Taxonomy (tax)
  tax <- read.delim("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  head(TAX,1) 
 
  
  head(food) 
  dim(food) 
  colnames(food)
  food_t <- t(food)
  colnames(food_t)
  head(food_t)[1:3,1:3]
  
 
  #### unconstrained
  un_rda <- rda(tax)
  
  
  # Sample
  # MCT
  # meta <- read.csv( "~/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", row.names = 1, check.names = F)
  
  # NHANES
  demog <- read.xport("../DEMO_I.XPT")
  colnames(food)  # SEQN starts with 'X'
  
  # demog needs to have XSEQN as rownames in order to use it in PrepMeta function. 
  rownames(demog) <- paste("X", demog$SEQN, sep="") # Add 'X' at the beginning
  head(demog)
  
# ----------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------
# chapter 2 Prepare for the workshop
  # Load the required packages
  library(vegan)
  library(labdsv)
  library(MASS)
  library(mvpart)
  library(ggplot2)
  
  # # Install the required packages
  # install.packages("vegan")
  # install.packages("labdsv")
  # install.packages("MASS")
  # install.packages("ggplot2")
  # 
  # # install mvpart from package archive file
  # install.packages("remotes")
  # remotes::install_url("https://cran.r-project.org/src/contrib/Archive/mvpart/mvpart_1.6-2.tar.gz")
  
# Chapter 4
# Make sure the files are in your working directory!  If R
# cannot find the dataset, set your working directory with
# setwd() to the folder in which your data is stored (e.g.
# setwd('~/Desktop/workshop10'))
  
  setwd("../../../RDA/")
  # Species community data frame (fish abundance)
  # spe <- read.csv("data/doubsspe.csv", row.names = 1)
  spe <- read.table("data/doubsspe.txt", sep=",", header=T, row.names = 1)
  spe <- spe[-8, ]  # Site number 8 contains no species, so we remove row 8 (site 8) 
  # Be careful to only run this command line once as you are
  # overwriting 'spe' each time!
  
  # Environmental data frame: "DoubsEnv.csv"
  # env <- read.csv("data/doubsenv.csv", row.names = 1)
  env <- read.table("data/doubsenv.txt", sep=",", header=T, row.names = 1)
  head(env)
  env <- env[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 
  head(env)
  # Again, be careful to only run the last line once.
  
# Count number of species frequencies in each abundance class
  ab <- table(unlist(spe))
# Plot distribution of species frequencies
  barplot(ab, las = 1, # make axis labels perpendicular to axis
          xlab = "Abundance class", ylab = "Frequency", # label axes
          col = grey(5:0/5)) # 5-colour gradient for the bars
  
  
  
  
# Chapter 6 Redundancy analysis
  # https://r.qcbs.ca/workshop10/book-en/redundancy-analysis.html
  
  # We'll use our standardized environmental data, but we
  # will remove 'das', which was correlated with many other
  # variables:
 
  # Make sure the files are in your working directory!  If R
  # cannot find the dataset, set your working directory with
  # setwd() to the folder in which your data is stored (e.g.
  # setwd('~/Desktop/workshop10'))
  
  # Species community data frame (fish abundance)
  spe <- read.csv("data/doubsspe.csv", row.names = 1)
  spe <- spe[-8, ]  # Site number 8 contains no species, so we remove row 8 (site 8) 
  # Be careful to only run this command line once as you are
  # overwriting 'spe' each time!
  
  # Environmental data frame: "DoubsEnv.csv"
  env <- read.csv("data/doubsenv.csv", row.names = 1)
  env <- env[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 
  # Again, be careful to only run the last line once.

  env.z <- decostand(env, method = "standardize")
  env.z <- subset(env.z, select = -das)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  