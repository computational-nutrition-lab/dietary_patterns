# For USERS ==============================================================================

# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1 
# Created on 05/31/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory and create a phyloseq object.
# ========================================================================================

# Import necessary functions and data
# Folder structure 
# 
#                          |----- data ---- Food_tree_data
#                          |
#                          |----- eg_data 
#                          |
#                          |----- lib --- source codes are here
#                          |
#                          |----- users --- this script is here
#  Main -------------------|
#  (dietary_patterns)      |----- results ---- Food_tree_results
#                          |
#                          |----- ...
#
  setwd("~/GitHub/dietary_patterns")

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
  library(SASxport)

# Define ggplot2 arguments and themes first.
  theme1 <- 
    theme(axis.title.x=element_text(margin=margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(axis.title.y=element_text(margin=margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(aspect.ratio = 1)
  
# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")
  
# Load the necessary scripts.
  source("lib/unifrac_ordination.R")
  
# ---------------------------------------------------------------------------------------------------------------
# Load the necessary files for creating a phyloseq object.  
  
# Food
  # Load food OTU table - this is our food OTU data
  # food <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_red_Lv5.dhydrt.otu.txt", row.names = 1)
  food <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.dhydrt.otu.txt", row.names = 1)
  # food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.dhydrt.otu.txt", row.names = 1)
  # food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.dhydrt.otu.txt", row.names = 1)
  # Format the food file and create a otu_table called OTU.
  PrepFood(data=food)
  food[1:10, 1:10]
  
# Taxonomy (tax)
  # tax <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_red_Lv5.taxonomy.txt")
  tax <- read.delim("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.taxonomy.txt")
  # tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.taxonomy.txt")
  # tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.taxonomy.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  
# Sample
  # MCT
  # meta <- read.csv( "~/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", row.names = 1, check.names = F)

  # NHANES
  demog <- read.xport("eg_data/NHANES/DEMO_I.XPT")
  colnames(food)  # SEQN starts with 'X'
  
  # demog needs to have XSEQN as rownames in order to use it in PrepMeta function. 
  rownames(demog) <- paste("X", demog$SEQN, sep="") # Add 'X' at the beginning
  
  PrepMeta(data=demog)

  
# Food tree
  # foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.tree.nwk")
  # foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.tree.nwk")
  foodtree <- read_tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.txt")
  # It is OK to see a message that says:
    # "Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'"
  # Format food tree and save it as 'TREE'. 
  PrepTree(data=foodtree)
  # It is OK to see the same message as the previous line. 

# ---------------------------------------------------------------------------------------------------------------
# Make a phyloseq object with OTU, TAX, samples, and foodtree.
  phyfoods <- phyloseq(OTU, TAX, SAMPLES, TREE)
  # It is OK to see a message (or multiple of them) saying that
    # Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
    # Also defined by 'tidytree'

# Check your metadata
  # Show the sample names. Change n to adjust the number of rows to show.
  head(sample_names(phyfoods), n=6)  
  # Show metadata. 
  head(sample_data(phyfoods), n=2)
  # Show only the columns of metadata. 
  sample_variables(phyfoods)

# Check the level 1 foods in your food tree 
  L1s = tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))
  

# ========================================================================================
# Use your phyloseq object and perform ordination 
# ========================================================================================

# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=TRUE) 

      # If it gives a warning with Lv1 saying that:
      # In matrix(tree$edge[order(tree$edge[, 1]), ][, 2], byrow = TRUE,  :
      #             data length [1461] is not a sub-multiple or multiple of the number of rows [731]
      # A solution shared in GitHub discussion forum is to transform all multichotomies into dichotomies with 
      # branches with length zero: need the age package. 
      # (https://github.com/joey711/phyloseq/issues/936, see commnet by PandengWang on Dec 26, 2019) 
      new_tre <- ape::multi2di(foodtree)
      # Prep it again for making a unifrac object.
      PrepTree(data=new_tre)
      # With the newly created TREE, create a phyloseq object once again.
      phyfoods <- phyloseq(OTU, TAX, SAMPLES, TREE)
      # New object overwritten the old one. Then, run the ordinate function again.
      # The warning should disappear now. 

# Save the percent variance explained by the axes as a vector to use in plots.  
  eigen_percent <- ordinated$values$Relative_eig

# Save the percent variance explained as a txt file.
  Eigen(eigen.input = eigen_percent, 
        output.fn="results/Ordination_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5_ord_wtd_eigen.txt")
    

# ========================================================================================
# Plot your ordination results 
# ========================================================================================

# Merge the first n axes to the metadata and save it as a txt file. 
# The merged dataframe is named 'meta_usersdf'.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=10, meta.data= demog, 
     output.fn= "results/Ordination_NHANES/demog_Food_D12_FC_cc_f_diffdiet98_red_Lv5_ord_wtd_users.txt")

# Load the diffdiet 98 people (this has SEQN, nutrients, FC, diet name; 1 row/person)
  QCtotal_b <- read.table("eg_data/NHANES/NHANES1516_total_d12_FC_mean_QC_2_98diffdiet.txt", 
                          sep="\t", header=T)

# Create a dataframe that has only the SEQN and Diet.
  SEQN_diet <- QCtotal_b[, c("SEQN","Diet")]  
  
# Keep the 98 individuals only. Currently, meta_usersdf has ALL the individuals of NHANES15-16.
# Add the Axis values to the 98 people. 
  meta_usersdf_diet <- merge(x=SEQN_diet, y=meta_usersdf, by="SEQN", all.x=T)

# Save - this may be more useful with the diet info.
  write.table(meta_usersdf_diet, 
      "results/Ordination_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5_ord_wtd_meta_users_diet.txt",
      sep="\t", row.names=F)
  
# Plot Axis 1 and Axis 2 to show the separation of samples colored by Diet and factors the metadata
  p1 <- ggplot(meta_usersdf_diet, aes(x=Axis.1, y=Axis.2, color=Diet)) +
          geom_point(aes(color=Diet), size=2.5, alpha=0.8) + 
          scale_color_manual(values = distinct100colors) + # OR use viridis theme.
          # scale_color_viridis_d() +
          xlab( paste("Axis.1 (", paste(round(eigen_percent[1]*100, 1)), "%)", sep="") ) +
          ylab( paste("Axis.2 (", paste(round(eigen_percent[2]*100, 1)), "%)", sep="") ) +
    theme_bw(base_size = 14) +  
    theme(legend.position = "bottom") +
    theme1
  p1
  
# Save the plot
  ggsave("results/Ordination_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5_ord_wtd.tif",
         p1, device = "tiff", width=6, height=6)

# Add ellipses at a desired confidence level. 
  p1 + stat_ellipse(level=0.95) 
  
# Add lines to connect samples in order of the variable on the x axis.
  p1 + geom_line(aes(color = Diet))  
  
# Add lines to connect samples in the order in which they appear in the data.
  p1 + geom_path(aes(color = Diet))  
  
# make a polygon by UserName, as.character(SEQN), Diet etc.
  p1 + geom_polygon(aes(fill = Diet)) + geom_point(aes(color=Diet), size=3) + 
    scale_fill_manual(values=distinct100colors)  
  # Could be messy with overlapping clusters and/or too many samples
    
# Specify colors for specific user(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight one sample with others being grey.  
      select_point_1 <- subset(meta_usersdf, UserName=="MCTs11") 

      p1 + geom_point(size=2, color="grey") +  
        geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=2) 

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight multiple samples with others being grey.
      select_points <- subset(meta_usersdf, UserName=="MCTs11" | UserName=="MCTs12" )
    
      p1 + geom_point(data=select_points, aes(x=Axis.1, y=Axis.2, color=as.factor(UserName))) +
        scale_color_manual(values = c("MCTs11"="red", "MCTs12"="blue")) 
      # OK to see a message: "Scale for 'colour' is already present. 
      # Adding another scale for 'colour', which will replace the existing scale."
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight one sample; other points will retain their original colors. 
      select_point_1 <- subset(meta_usersdf, UserName=="MCTs11") 
    
      # Changing the shape sizes might help find the dots. Note that points may be overlapping
      p1 + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) 
        
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight multiple samples; other points will retain their original colors. 
      select_point_1 <- subset(meta_usersdf, UserName=="MCTs11") 
      select_point_2 <- subset(meta_usersdf, UserName=="MCTs12") 
  
      p1 + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
           geom_point(data=select_point_2, aes(x=Axis.1, y=Axis.2), color="green", size=4) 
        
# ========================================================================================
# Save distance matrices. 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Generate and save an unweighted unifrac distance matrix for use outside R.  type="samples" only. 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, output.fn = "results/Ordination_NHANES/unweighted_uni_dis.txt")        
    
  
# Generate and save an unweighted unifrac distance matrix for use outside R.  type="samples" only. 
  WeightedUnifracDis(input.phyloseq.obj = phyfoods, output.fn = "results/WEIGHTED_uni_dis.txt")        
  

  
# ========================================================================================
# Use other ordination methods 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
  # This may take a few minutes depending on your data size.
  # e.g. takes ~ 1 min to process a 7.9-MB phyloseq object . 
  ordinated = phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=FALSE)  
  
  # Use the same code above for creating plots.
  
# ---------------------------------------------------------------------------------------------------------------
# With a small dataset,
# Perform Double Principal Coordinate Analysis (DPCoA) which takes into account both distance and weight. 
  # This may take a long time depending on your data size.
  ordinated = phyloseq::ordinate(phyfoods, method="DPCoA", distance="unifrac")  
  
  
  
  
  
 

  
  
  