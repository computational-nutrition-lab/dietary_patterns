# For USERS ==============================================================================

# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1
# Created on 03/08/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory and create phyloseq object.
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

# Set your working directory to the main directory.
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
  source("lib/unifrac_ordination.R")
  
# ---------------------------------------------------------------------------------------------------------------
# Load the necessary files for creating a phyloseq object.  
  
# Food
  # Load food OTU table - this is our food OTU data
  food <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.dhydrt.otu.txt", row.names = 1)
  # Format the food file and create a otu_table called OTU.
  PrepFood(data=food)
  
# Taxonomy (tax)
  # Load taxonomy file - this is the taxonomy data from food tree code, but forced into a tabular format
  tax <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.taxonomy.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  
# Sample
  # Load metadata file which has samples in rows and characteristics (BMI, Gender, treatment etc.) as columns 
  meta <- read.csv( "C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", 
                    row.names = 1, check.names = F)
  # Format the metafile and save it as 'SAMPLES'. 
  PrepMeta(data=meta)

# Food tree
  # Load tree file - output from make.tree. Be sure the levels of taxonomy and tree are the same. 
  foodtree <- read_tree("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.tree.nwk")
  # It is OK to see a message saying that
    # "Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'"
  # Format food tree and save it as 'TREE'. 
  PrepTree(data=foodtree)

# ---------------------------------------------------------------------------------------------------------------
# Make a phyloseq object with OTU, TAX, samples, and foodtree.
  phyfoods <- phyloseq(OTU, TAX, SAMPLES, TREE)
  # It is OK to see a message saying that
    # Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'

# Check your metadata
  head(sample_names(phyfoods), n=6) # Change n to adjust the number of rows to show.
  head(sample_data(phyfoods), n=2)
  sample_variables(phyfoods)

# Check the level 1 foods in your food tree 
  L1s = tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))
  
  L1sdf <- as.data.frame(L1s)
  head(L1sdf)
  library(dplyr)
  L1sdf %>% filter(!is.na(L1))

# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Use your phyloseq object and perform ordination 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated = phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=TRUE)  
  
# Make a plot to show the separation of taxa (foods) by level 1 on a PCo1-PCo2 plane. 
  p1 <- plot_ordination(phyfoods, ordinated, color="L1", type="taxa") +
    geom_point(size=2) + theme(aspect.ratio=1) + ggtitle("Foods at L1")
  p1
  
# Make a plot to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata
  p2 = plot_ordination(phyfoods, ordinated, type="samples", color="UserName") + 
    geom_point(size=2) + theme(aspect.ratio = 1) + ggtitle("Username")
  
  # Add ellipses at a desired confidence level. 
  p2 + stat_ellipse(level=0.95)
  
  # Add lines to connect samples
  p2 + geom_line()

  # make a polygon by UserName
  p2 + geom_polygon(aes(fill=UserName)) + geom_point(size=3) 
  # Could be messy with overlapping clusters and/or too many samples

# plot both foods (taxa) and samples (people)
  p3 = plot_ordination(phyfoods, ordinated, type="biplot", shape="L1", color="UserName") +
    scale_shape_manual(values=c(1:10)) + geom_point(size=2) + theme(aspect.ratio=1) + ggtitle("Biplot") +
    guides(colour=guide_legend(ncol=4)) # make the legend have 4 columns so they will fit
  p3
      # Gives a warning about 9 rows of missing values (geom_point)??? 
  
# ---------------------------------------------------------------------------------------------------------------
# Merge axes values and metadata and save as a dataframe called axes_and_meta.
  # This will sort Axis1 in an descending order.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=4, meta=meta)
  
  # Look at the first few samples that have the highest Axis 1 values.  
  head(axes_and_meta, n=6)  
  
  # Save as a .csv
  write.csv(x=axes_and_meta, "results/Ordination_Axis_Meta.csv")

   
# ---------------------------------------------------------------------------------------------------------------
# Perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
  # This may take a few minutes depending on your data size.
  # e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated = phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=FALSE)  
  
# ---------------------------------------------------------------------------------------------------------------
# With a small dataset,
# Perform Double Principal Coordinate Analysis (DPCoA) which takes into account both distance and weight. 
  # This may take a long time depending on your data size.
  ordinated = phyloseq::ordinate(phyfoods, method="DPCoA", distance="unifrac")  
  
 

  
  
  
  
  
  
  