# For USERS ==============================================================================

# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1
# Created on 03/08/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Import necessary functions and data
# Folder structure 
# 
#                          |----- eg_data 
#                          |
#                          |----- lib --- source codes are here
#                          |
#                          |----- users --- this script is here
#  Main -------------------|
#  (dietary_patterns)      |----- results
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Install phyloseq package.
  # BiocManager::install("phyloseq")

# load the necessary packages.
  library(phyloseq)
  library(ggtree)
  library(ggplot2)

# Define ggplot2 arguments and themes first.
  theme_set(theme_bw())
  fontsize = 18L
  theme_update(axis.title.x = element_text(size=fontsize))
  theme_update(axis.title.y = element_text(size=fontsize))
  theme_update(plot.title   = element_text(size=fontsize+2))
  
# Load the necessary scripts.
  source()
  
# ---------------------------------------------------------------------------------------------------------------
# Load the necessary files for creating a phyloseq object.  
  
  # Food OTU table - this is our food OTU data
  food <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.dhydrt.otu.txt", row.names = 1)
  # Format the food file and save as food2.
  PrepFood(data=food)
  
  # Taxonomy - this is the taxonomy data from food tree code, but forced into a tabular format
  tax <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.taxonomy.txt")
  # Format the tax file and save as tax2.
  PrepTax(data=tax)
  
  ####### RESUME FROM HERE ##########
  
  
  

  # Tree file - output from make.tree. Be sure the levels of taxonomy and tree are the same. 
  foodtree <- read_tree("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.tree.nwk")
  
  # Samples - metadata file which has samples in rows and characteristics (BMI, Gender, treatment etc.) as columns 
  meta <- read.csv( "C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", 
                    row.names = 1, check.names = F)
  
  
  
  

# ---------------------------------------------------------------------------------------------------------------
# 
# ---------------------------------------------------------------------------------------------------------------

