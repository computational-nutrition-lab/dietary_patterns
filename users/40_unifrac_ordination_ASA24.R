# For USERS ==============================================================================

# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 2 - cleaner veresion with just 'Users' plot.
# Created on 03/25/2022 by Rie Sadohara
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

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# ---------------------------------------------------------------------------------------------------------------
# Install phyloseq package.
  BiocManager::install("phyloseq")

# load the necessary packages.
  library(phyloseq)
  library(ggtree)
  library(ggplot2)

# Load ggplot formatting themes
  source("~/GitHub/dietary_patterns/lib/ggplot2themes.R")
  
  # # Define ggplot2 arguments and themes first.
  #   theme1 <- theme_bw(base_size = 16) +
  #     theme(axis.title.x=element_text(margin=margin(t = 10, r = 0, b = 0, l = 0))) +
  #     theme(axis.title.y=element_text(margin=margin(t = 0, r = 10, b = 0, l = 0))) +
  #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #     theme(aspect.ratio = 1)
  
# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")
  
# Load the necessary scripts.
  source("lib/unifrac_ordination.R")
  
# ---------------------------------------------------------------------------------------------------------------
# Load the necessary files for creating a phyloseq object.  
  
# Food
  # Load food OTU table - this is our food OTU data
  food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_ASA24/mct.reduced_4Lv.dhydrt.otu.txt", row.names = 1)
  # food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.dhydrt.otu.txt", row.names = 1)
  food <- read.delim("eg_data/VVKAJ/Foodtree/VVKAJ_Items_f_s_m_ff_reduced_4Lv.food.otu.txt", row.names = 1)
  head(food)
  # Format the food file and create a otu_table called OTU.
  PrepFood(data= food)
  
# Taxonomy (tax)
  tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_ASA24/mct.reduced_4Lv.taxonomy.txt")
  # tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.taxonomy.txt")
  tax <- read.delim("eg_data/VVKAJ/Foodtree/VVKAJ_Items_f_s_m_ff_reduced_4Lv.tax.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data= tax)
  
# Sample
  meta <- read.csv( "~/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv",
                    row.names = 1, check.names = F)
  meta <- read.table( "eg_data/VVKAJ/ind_metadata.txt", sep="\t", header=T)
  head(meta)
  
  # Format the metadata file and save it as 'SAMPLES'. 
  PrepMeta(data= meta)
  SAMPLES

# Food tree
  foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_ASA24/mct.reduced_4Lv.tree.nwk")
  # foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.tree.nwk")
  foodtree <- read_tree("eg_data/VVKAJ/Foodtree/VVKAJ_Items_f_s_m_ff_reduced_4Lv.tree.nwk")
  # It is OK to see a message saying that
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
  # Show the sample names. 
  sample_names(phyfoods)  
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
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=T) 

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
  Eigen(eigen.input = eigen_percent, output.fn="results/eigen_percent_mct.txt")
  Eigen(eigen.input = eigen_percent, output.fn="eg_data/VVKAJ/Unifrac/ordinated_weighted_eigen_percent.txt")
    

# ========================================================================================
# Plot your ordination results 
# ========================================================================================

# Merge the first n axes to the metadata and save it as a txt file. 
# The merged dataframe, 'meta_usersdf', will be used for plotting.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=10, meta.data= meta, 
                       output.fn= "results/ordinated_weighted_meta_users_MCT.txt")
                       # output.fn= "eg_data/VVKAJ/Unifrac/ordinated_Weighted_meta_users.txt")

# Read in the metadata and users' Axis values. 
  meta_usersdf_loaded <- read.table("results/ordinated_weighted_meta_users_MCT.txt", header=T)
  meta_usersdf_loaded <- read.table("eg_data/VVKAJ/Unifrac/ordinated_Weighted_meta_users.txt", header=T)
 
# Plot Axis 1 and Axis 2 to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata
  p1 <- ggplot(meta_usersdf, aes(x=Axis.1, y=Axis.2, color=UserName)) +
          geom_point(aes(color=UserName)) + 
          scale_color_manual(values = distinct100colors) + # OR use viridis theme.
          # scale_color_viridis_d() +
          xlab( paste("Axis.1 (", paste(round(eigen_percent[1]*100, 1)), "%)", sep="") ) +
          ylab( paste("Axis.2 (", paste(round(eigen_percent[2]*100, 1)), "%)", sep="") ) +
          no_grid + space_axes + theme(aspect.ratio = 1)
  p1
  
# Add ellipses at a desired confidence level. 
  p1 + stat_ellipse(level=0.95) 
  
# Add lines to connect samples in order of the variable on the x axis.
  p1 + geom_line(aes(color = UserName))  
  
# Add lines to connect samples in the order in which they appear in the data.
  p1 + geom_path(aes(color = UserName))  
  
# make a polygon by UserName
  p1 + geom_polygon(aes(fill = UserName)) + geom_point(aes(color=UserName), size=3) + 
    scale_fill_manual(values=distinct100colors)  
  # Could be messy with overlapping clusters and/or too many samples
    
# Specify colors for specific user(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight one sample with others being grey.  
      select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 

      p1 + geom_point(size=2, color="grey") +  
        geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=2) 

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight multiple samples with others being grey.
      select_points <- subset(meta_usersdf, UserName=="VVKAJ101" | UserName=="VVKAJ106" )
    
      p1 + geom_point(data=select_points, aes(x=Axis.1, y=Axis.2, color=as.factor(UserName))) +
        scale_color_manual(values = c("VVKAJ101"="red", "VVKAJ106"="blue")) 
      # It is OK to see a message: "Scale for 'colour' is already present. 
      # Adding another scale for 'colour', which will replace the existing scale."
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight one sample; other points will retain their original colors. 
      select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
    
      # Changing the shape sizes might help find the dots. Note that points may be overlapping
      p1 + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) 
        
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight multiple samples; other points will retain their original colors. 
      select_point_1 <- subset(meta_usersdf, UserName=="VVKAJ101") 
      select_point_2 <- subset(meta_usersdf, UserName=="VVKAJ106") 
  
      p1 + geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
           geom_point(data=select_point_2, aes(x=Axis.1, y=Axis.2), color="green", size=4) 
        
# ========================================================================================
# Save distance matrices. 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Generate and save an unweighted unifrac distance matrix for use outside R.  type="samples" only. 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, output.fn = "eg_data/VVKAJ/Unifrac/weighted_uni_dis.txt")        
    
  
# Generate and save an weighted unifrac distance matrix for use outside R.  type="samples" only. 
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
  
  
  
  
  
 

  
  
  