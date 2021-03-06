# For USERS ==============================================================================

# ========================================================================================
# Create a phyloseq object out of dietary and tree data and run ordination.
# Version 1
# Created on 03/08/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory and create a phyloseq object.
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
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
  
# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")
  
# Load the necessary scripts.
  source("lib/unifrac_ordination.R")
  
# ---------------------------------------------------------------------------------------------------------------
# Load the necessary files for creating a phyloseq object.  
  
# Food
  # Load food OTU table - this is our food OTU data
  # food <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.dhydrt.otu.txt", row.names = 1)
  food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.dhydrt.otu.txt", row.names = 1)
  food <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.dhydrt.otu.txt", row.names = 1)
  # Format the food file and create a otu_table called OTU.
  PrepFood(data=food)
  
# Taxonomy (tax)
  # Load taxonomy file - this is the taxonomy data from food tree code, but forced into a tabular format
  tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.taxonomy.txt")
  tax <- read.delim("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.taxonomy.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  
# Sample
  # Load metadata file which has samples in rows and characteristics (BMI, Gender, treatment etc.) as columns 
  meta <- read.csv( "~/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv",
                    row.names = 1, check.names = F)
  # Format the metafile and save it as 'SAMPLES'. 
  PrepMeta(data=meta)

# Food tree
  # Load tree file - output from make.tree. Be sure the levels of taxonomy and tree are the same. 
  foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.tree.nwk")
  foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.tree.nwk")
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
  # Show the sample names. Change n to adjust the number of rows to show.
  head(sample_names(phyfoods), n=6)  
  # Show metadata. 
  head(sample_data(phyfoods), n=2)
  # Show only the columns of metadata. 
  sample_variables(phyfoods)

# Check the level 1 foods in your food tree 
  L1s = tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Use your phyloseq object and perform ordination 
# ========================================================================================

# Perform Principal Coordinate Analysis (PCoA) with weighted unifrac distance of your food data.
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=T) 
  
  # Save the percent variance explained for the fist 3 axes.  
  eigen_percent <- head(ordinated$values$Relative_eig, 3)

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
  
  df = as.data.frame(ordinated$vectors[, 1:2])
  head(df)
  ggplot(df, aes(x=Axis.1, y=Axis.2)) + geom_point()
  
  
# ========================================================================================
# Plot your ordination results 
# ========================================================================================

# Make a plot to show the separation of taxa (foods) by level 1 on a PCo1-PCo2 plane. 
  p1 <- plot_ordination(phyfoods, ordinated, color="L1", type="taxa") +
    geom_point(size=2) + theme(aspect.ratio=1) + ggtitle("Foods") +
    scale_color_manual(values=distinct100colors)
    # scale_color_viridis_d()
  p1 
  ######## removed 9 rows containing missing values?  
  
  # Save the information necessary for ploting as a dataframe if preferred.
  p1df <- plot_ordination(phyfoods, ordinated, color="L1", type="taxa", justDF = T)
    colnames(p1df)
    dim(p1df)
    as.data.frame(table(p1df$L1))
    colSums(x=as.data.frame(table(p1df$L1))[2]  )
    ggplot(p1df, aes(x=Axis.1, y=Axis.2), color=) + geom_point()
  
  

# Make a plot to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata
  p2 = plot_ordination(phyfoods, ordinated, type="samples", color="UserName") + 
    geom_point(size=2) + theme(aspect.ratio = 1) + ggtitle("Users") +
    scale_color_manual(values=distinct100colors)
  p2

  p2 + scale_color_manual(values=distinct100colors)
  
    # Add ellipses at a desired confidence level. 
    p2 + stat_ellipse(level=0.95) + ggtitle("elipses confidence level=0.95") +
      theme(plot.title=element_text(size=16)) # Specify the font size of the title
    
    # Add lines to connect samples in order of the variable on the x axis.
    p2 + geom_line() + ggtitle("Users connected in the order of x axis") + 
      theme(plot.title=element_text(size=16)) 
    
    # Add lines to connect samples in the order in which they appear in the data.
    p2 + geom_path() + ggtitle("Users connected in the order of data") + 
      theme(plot.title=element_text(size=16))
    
    # make a polygon by UserName
    p2 + geom_polygon(aes(fill=UserName)) + geom_point(size=3) 
    # Could be messy with overlapping clusters and/or too many samples
    
  # Specify colors for specific user(s) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Save an ordination plot for filtering purposes. 
    p2df = plot_ordination(phyfoods, ordinated, type="samples", color="UserName", justDF=T) 
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight one sample with others being grey.  
      select_point_1 <- subset(p2df, UserName=="MCTs11") 
      
      p2_2 <- plot_ordination(phyfoods, ordinated, type="samples") +  
        geom_point(size=2, color="grey") + theme(aspect.ratio = 1) + ggtitle("MCTs11 (Shake drinker)") +
        geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=2) 
      p2_2
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight multiple samples with others being grey.
      select_points <- subset(p2df, UserName=="MCTs11" | UserName=="MCTs12" )
      head(select_points,2)
    
      p2 +  geom_point(data=select_points, aes(x=Axis.1, y=Axis.2, color=as.factor(UserName))) +
        ggtitle("MCTs11 & MCTs12 (Shake drinkers)") +
        theme(plot.title=element_text(size=16), aspect.ratio=1) +
        scale_color_manual(values = c("MCTs11"="red", "MCTs12"="blue")) +
        theme(aspect.ratio = 1)
      # OK to see a message: "Scale for 'colour' is already present. 
      # Adding another scale for 'colour', which will replace the existing scale."
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight one sample; other points will retain their original colors. 
      select_point_1 <- subset(p2df, UserName=="MCTs11") 
    
    # Changing the shape sizes might help find the dots. Note that points may be overlapping
      p2 +  geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
        ggtitle("MCTs11 (Shake drinker)") +
        theme(plot.title=element_text(size=16), aspect.ratio=1) 
        
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Highlight multiple samples; other points will retain their original colors. 
      select_point_1 <- subset(p2df, UserName=="MCTs11") 
      select_point_2 <- subset(p2df, UserName=="MCTs12") 
  
      p2 +  geom_point(data=select_point_1, aes(x=Axis.1, y=Axis.2), color="black", size=4) +
            geom_point(data=select_point_2, aes(x=Axis.1, y=Axis.2), color="green", size=4) +
            ggtitle("MCTs11 & MCTs12 (Shake drinkers)") +
            theme(plot.title=element_text(size=16), aspect.ratio=1)
        

# plot both foods (taxa) and samples (people)
  p3 = plot_ordination(phyfoods, ordinated, type="biplot", shape="L1", color="UserName") +
    scale_shape_manual(values=c(1:10)) + geom_point(size=2) + theme(aspect.ratio=1) + ggtitle("Biplot") +
    guides(colour=guide_legend(ncol=4)) # make the legend have 4 columns so they will fit
  p3
      # Gives a warning about 9 rows of missing values (geom_point)??? 
  
  
# ========================================================================================
# Save your results 
# ========================================================================================

# Merge axes values and metadata and save as a dataframe called axes_and_meta.
  # This will sort Axis1 in an descending order.
  MergeAxesAndMetadata(ord.object=ordinated, number.of.axes=4, meta=meta)
  
  # Look at the first few samples that have the highest Axis 1 values.  
  head(axes_and_meta, n=6)  
  
  # Save as a .csv
  write.csv(x=axes_and_meta, "results/Ordination_Axis_Meta.csv")

# ---------------------------------------------------------------------------------------------------------------
# Generate and save an unweighted unifrac distance matrix for use outside R.  type="samples" only. 
  unweighted_uni_dis <- as.matrix(distance(phyfoods, method="unifrac")) 
  
# Generate and save an unweighted unifrac distance matrix for use outside R.  type="samples" only. 
  weighted_uni_dis <- as.matrix(distance(phyfoods, method="wunifrac"))

# Save as a .csv or tab-delimited .txt 
  write.csv(x=unweighted_uni_dis, "~/GitHub/dietary_patterns/results/Food_tree_results/un-distance.csv")
  write.table(x=weighted_uni_dis, "~/GitHub/dietary_patterns/results/Food_tree_results/wt-distance.txt", sep="\t")

  
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
  
  
  
  
  
 

  
  
  