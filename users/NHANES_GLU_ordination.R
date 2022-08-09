# ===============================================================================================================
# Unifrac ordination of Males 50s GLU.
# Version 1
# Created on 08/09/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# 
# ===============================================================================================================

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

# Load the distinct 100 colors for use.   
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")

# Load the necessary scripts.
  source("lib/unifrac_ordination.R")
  source("lib/ggplot2themes.R")

# Set working dir
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/")

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
  
# Food tree
  # foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_4Lv.tree.nwk")
  # foodtree <- read_tree("~/GitHub/dietary_patterns/results/Food_tree_results/mct.reduced_1Lv.tree.nwk")
  # foodtree <- read_tree("results/Food_tree_NHANES/Food_D12_FC_cc_f_diffdiet98_red_Lv5.txt")
  foodtree <- read_tree("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.nwk")
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
  L1s <- tax_table(phyfoods)[, "L1"]
  as.vector(unique(L1s))
# 

# ========================================================================================
# Use your phyloseq object and perform ordination - WEIGHTED
# ========================================================================================
  
# Perform Principal Coordinate Analysis (PCoA) with WEIGHTED unifrac distance of your food data.
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
      phyfoods <- phyloseq(OTU, TAX, SAMPLES, new_tre)
      # New object overwritten the old one. Then, run the ordinate function again.
      # The warning should disappear now. 
  
  # Save the percent variance explained by the axes as a vector to use in plots.  
  eigen_percent <- ordinated$values$Relative_eig
  
  # Save the percent variance explained as a txt file.
  Eigen(eigen.input = eigen_percent, 
        output.fn="Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_eigen.txt")
  
# ===============================================================================================================
# Save unifrac distance (unweighted or weighted) matrices. 
# ===============================================================================================================
  
# Generate and save an UNweighted unifrac distance matrix of "Samples". 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, 
                       output.fn = "Ordination/Food_D12_FC_cc_f_males50s_red_Lv4_ord_UNweighted_uni_dis.txt")        
  
# ---------------------------------------------------------------------------------------------------------------
# Generate and save an WEIGHTED unifrac distance matrix of "Samples". 
  WeightedUnifracDis(input.phyloseq.obj = phyfoods, 
                     output.fn = "Ordination/Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_uni_dis.txt")        
  
  
# ===============================================================================================================
# Plot your ordination results 
# ===============================================================================================================

# Change to the folder called "Ordination" in your "VVKAJ" folder.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data/Ordination/")
  
# Need to define this function for NHANES. 
    MergeAxesAndMetadata_NHANES <- function(ord.object, number.of.axes, meta.data, output.fn){
      
      # extract all the Axis vectors
      allvectors <<- as.data.frame(ord.object["vectors"])
      
      # Remove the suffix 'vectors.' in the column names of 'allvectors'
      colnames(allvectors) <<- sub(pattern='vectors.', replacement='', x=colnames(allvectors))
      
      # Extract Axes 1 through the specified axis
      vectors <<- allvectors[, 1:number.of.axes]
      
      # make SampleID as rownames of meta. (to use SampleID in rownames for merging)
      # rownames(meta.data) <- meta.data[, "SampleID"]
      
      # Merge by the rownames (X89125 etc.).
      meta_usersdf <<- merge(x=meta.data, y=vectors, all.y=T, by="row.names", sort=FALSE)
      
      # Save as a txt file.
      write.table(x = meta_usersdf, file= output.fn, sep="\t", row.names= F)
    }
  
# ---------------------------------------------------------------------------------------------------------------
# Merge the first n axes to the metadata and save it as a txt file. 
# The merged dataframe, 'meta_usersdf', will be used for plotting.
  MergeAxesAndMetadata_NHANES(ord.object=ordinated, number.of.axes= 10, meta.data= demog, 
                              output.fn= "Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_meta_users.txt")

# Read in the metadata and users' Axis values. 
# meta_usersdf_loaded <- read.table("results/ordinated_weighted_axes_meta_MCT.txt", header=T)
  meta_usersdf_loaded <- read.table("Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_meta_users.txt", 
                                    header=T)
  
  head(meta_usersdf_loaded)
  
# Load a dataset that has the "GLU_index" information. 
  glu <- read.delim( file="../QCtotalANDglu_body_meta_demo.txt", sep= "\t", header= T )
  colnames(glu)

  # Take out only what you need.
  SEQN_GLU <- glu[, c("SEQN", "GLU_index")]
  
  # Put 'X' in front of the SEQN and    
  SEQN_GLU$Row.names <- paste("X", SEQN_GLU$SEQN, sep="")
  SEQN_GLU_2 <- SEQN_GLU[, c("Row.names", "GLU_index")] # Take out one column as a dataframe, not a vector.
  head(SEQN_GLU_2)
  
  # Add GLU_index.
  meta_usersdf_loaded_glu <- merge(x=meta_usersdf_loaded, y=SEQN_GLU_2, by="Row.names", all.x=T, sort=F) 
  head(meta_usersdf_loaded_glu)
  
# Save the resultant table.
  write.table(meta_usersdf_loaded_glu, 
              "Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_meta_users_glu.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
#### GOOD TILL HERE ####
  
# Load the XXX_meta_users_glu.txt again for plotting.
  loaded_glu <- read.table("Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_meta_users_glu.txt",
                           sep="\t", header=T)
  
# Take a look at meta_usersdf_loaded. 
  head(loaded_glu, 3)

# ---------------------------------------------------------------------------------------------------------------
# Plot Axis 1 and Axis 2 to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata.
  p1 <- ggplot(loaded_glu, aes(x=Axis.1, y=Axis.2, color=GLU_index)) +
    geom_point(aes(color= GLU_index), size=3) + 
    scale_color_manual( values= c("mediumvioletred",  "turquoise2",  "goldenrod3") ,
                       labels= c("Diabetic", "Normal",      "Prediabetic")) +
    # scale_color_viridis_d() +    # OR use viridis theme.
    xlab( paste("Axis.1 (", paste(round(eigen_percent[1]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.2 (", paste(round(eigen_percent[2]*100, 1)), "%)", sep="") ) +
    no_grid + space_axes + theme(aspect.ratio = 1)
  p1
  
  # Save p1 as a PDF. 
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_Axis12_p1.png", 
         p1, device="png", width=7, height=6.5, unit="in", dpi=300)
  
  # You can add ellipses at a desired confidence level; but with this 
  # example data, there are too few samples per user to draw them. 
  ellipses <- p1 + stat_ellipse(level=0.95) 
  ellipses
  
  # Save ellipses as a PDF. 
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_Axis12_ellipses.png", 
         ellipses, device="png", height=7, width=6.5, unit="in", dpi=300)
  
# Not applicable to NHANES. 
  # # Add lines to connect samples in the order in which they appear in the data. 
  # # Note that geom_line option, not geom_path, connects in the order of the variable 
  # # on the x axis, so it could be misleading.
  # pathconnected <- p1 + geom_path(aes(color = UserName))  
  # pathconnected
  
  # # Save pathconnected as a PDF. 
  # ggsave("4Lv_ordinated_Weighted_Axis12_pathconnected.pdf", 
  #        pathconnected, device="pdf", height=6, width=6, unit="in", dpi=300)
  
# Make a polygon by UserName
  # Could be messy with overlapping clusters and/or too many samples.
  polygons <- p1 + geom_polygon(aes(fill = GLU_index)) + 
    geom_point(aes(color=GLU_index), size=2) + 
    scale_fill_manual(values=distinct100colors)
  polygons
  
  # Save polygons as a PDF. 
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_Axis12_polygons.png", 
         polygons, device="png", height=7, width=6.5, unit="in", dpi=300)
  
  
# ---------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------
  
# ========================================================================================
# Use your phyloseq object and perform ordination - UNweighted
# ========================================================================================
  
# Perform Principal Coordinate Analysis (PCoA) with UNweighted unifrac distance of your food data.
# This may take a few minutes depending on your data size.
# e.g. a large phyloseq object (7.9 MB) takes ~ 1 min. 
  ordinated <- phyloseq::ordinate(phyfoods, method="PCoA", distance="unifrac", weighted=FALSE) 
  
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
      phyfoods <- phyloseq(OTU, TAX, SAMPLES, new_tre)
      # New object overwritten the old one. Then, run the ordinate function again.
      # The warning should disappear now. 
  
# Save the percent variance explained by the axes as a vector to use in plots.  
  eigen_percent <- ordinated$values$Relative_eig
  
  # Save the percent variance explained as a txt file.
  Eigen(eigen.input = eigen_percent, 
        output.fn="Food_D12_FC_cc_f_males50s_red_Lv4_ord_UNw_eigen.txt")
  
# ===============================================================================================================
# Save unifrac distance (unweighted or weighted) matrices. 
# ===============================================================================================================
  
# Generate and save an UNweighted unifrac distance matrix of "Samples". 
  UnweightedUnifracDis(input.phyloseq.obj = phyfoods, 
                       output.fn = "Ordination/Food_D12_FC_cc_f_males50s_red_Lv4_ord_UNweighted_uni_dis.txt")        
  
# ---------------------------------------------------------------------------------------------------------------
  # Generate and save an WEIGHTED unifrac distance matrix of "Samples". 
  WeightedUnifracDis(input.phyloseq.obj = phyfoods, 
                     output.fn = "Ordination/Food_D12_FC_cc_f_males50s_red_Lv4_ord_WEIGHTED_uni_dis.txt")        
  
# ===============================================================================================================
# Plot your ordination results - UNweighted. 
# ===============================================================================================================
  
# Change to the folder called "Ordination" in your "VVKAJ" folder.
  SpecifyDataDirectory(directory.name = "eg_data/NHANES/Laboratory_data/Ordination/")
  
# Need to define this function for NHANES. 
  MergeAxesAndMetadata_NHANES <- function(ord.object, number.of.axes, meta.data, output.fn){
    
    # extract all the Axis vectors
    allvectors <<- as.data.frame(ord.object["vectors"])
    
    # Remove the suffix 'vectors.' in the column names of 'allvectors'
    colnames(allvectors) <<- sub(pattern='vectors.', replacement='', x=colnames(allvectors))
    
    # Extract Axes 1 through the specified axis
    vectors <<- allvectors[, 1:number.of.axes]
    
    # make SampleID as rownames of meta. (to use SampleID in rownames for merging)
    # rownames(meta.data) <- meta.data[, "SampleID"]
    
    # Merge by the rownames (X89125 etc.).
    meta_usersdf <<- merge(x=meta.data, y=vectors, all.y=T, by="row.names", sort=FALSE)
    
    # Save as a txt file.
    write.table(x = meta_usersdf, file= output.fn, sep="\t", row.names= F)
  }
  
# ---------------------------------------------------------------------------------------------------------------
# Merge the first n axes to the metadata and save it as a txt file. 
# The merged dataframe, 'meta_usersdf', will be used for plotting.
  MergeAxesAndMetadata_NHANES(ord.object=ordinated, number.of.axes= 10, meta.data= demog, 
                              output.fn= "Food_D12_FC_cc_f_males50s_red_Lv4_ord_UNweighted_meta_users.txt")
  
  # Read in the metadata and users' Axis values. 
  # meta_usersdf_loaded <- read.table("results/ordinated_weighted_axes_meta_MCT.txt", header=T)
  meta_usersdf_loaded <- read.table("Food_D12_FC_cc_f_males50s_red_Lv4_ord_UNweighted_meta_users.txt", 
                                    header=T, sep="\t")
  
  # Load a dataset that has the "GLU_index" information. 
  glu <- read.delim( file="../QCtotalANDglu_body_meta_demo.txt", sep= "\t", header= T )
  colnames(glu)
  
  # Take out only what you need.
  SEQN_GLU <- glu[, c("SEQN", "GLU_index")]
  
  # Put 'X' in front of the SEQN and    
  SEQN_GLU$Row.names <- paste("X", SEQN_GLU$SEQN, sep="")
  SEQN_GLU_2 <- SEQN_GLU[, c("Row.names", "GLU_index")] # Take out one column as a dataframe, not a vector.
  head(SEQN_GLU_2)
  
  # Add GLU_index.
  meta_usersdf_loaded_glu <- merge(x=meta_usersdf_loaded, y=SEQN_GLU_2, by="Row.names", all.x=T, sort=F) 
  head(meta_usersdf_loaded_glu)
  
  # Save the resultant table.
  write.table(meta_usersdf_loaded_glu, 
              "Food_D12_FC_cc_f_males50s_red_Lv4_ord_unWEIGHTED_meta_users_glu.txt", 
              sep="\t", row.names=F, quote=F)
  
# ---------------------------------------------------------------------------------------------------------------
# Load the XXX_meta_users_glu.txt again for plotting.
  loaded_glu <- read.table("Food_D12_FC_cc_f_males50s_red_Lv4_ord_unWEIGHTED_meta_users_glu.txt",
                           sep="\t", header=T)
  
  # Take a look at meta_usersdf_loaded. 
  head(loaded_glu, 3)
  
# ---------------------------------------------------------------------------------------------------------------
# Plot Axis 1 and Axis 2 to show the separation of samples colored by UserName, gender, timing, etc. as in the metadata.
  p1 <- ggplot(loaded_glu, aes(x=Axis.1, y=Axis.2, color=GLU_index)) +
    geom_point(aes(color= GLU_index), size=3) + 
    scale_color_manual( values= c("mediumvioletred",  "turquoise2",  "goldenrod3") ,
                        labels= c("Diabetic",         "Normal",      "Prediabetic")) +
    # scale_color_viridis_d() +    # OR use viridis theme.
    xlab( paste("Axis.1 (", paste(round(eigen_percent[1]*100, 1)), "%)", sep="") ) +
    ylab( paste("Axis.2 (", paste(round(eigen_percent[2]*100, 1)), "%)", sep="") ) +
    no_grid + space_axes + theme(aspect.ratio = 1)
  p1
  
  # Save p1 as a PDF. 
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_ord_unWEIGHTED_Axis12_p1.png", 
         p1, device="png", width=7, height=6.5, unit="in", dpi=300)
  
  # You can add ellipses at a desired confidence level; but with this 
  # example data, there are too few samples per user to draw them. 
  ellipses <- p1 + stat_ellipse(level=0.95) 
  ellipses
  
  # Save ellipses as a PDF. 
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_ord_unWEIGHTED_Axis12_ellipses.png", 
         ellipses, device="png", height=7, width=6.5, unit="in", dpi=300)
  
  # Make a polygon by UserName
  # Could be messy with overlapping clusters and/or too many samples.
  polygons <- p1 + geom_polygon(aes(fill = GLU_index)) + 
    geom_point(aes(color=GLU_index), size=2)  
    # scale_fill_manual(values=distinct100colors)
  polygons
  
  # Save polygons as a PDF. 
  ggsave("Food_D12_FC_cc_f_males50s_red_Lv4_ord_unWEIGHTED_Axis12_polygons.png", 
         polygons, device="png", height=7, width=6.5, unit="in", dpi=300)
  
  
  
# ---------------------------------------------------------------------------------------------------------------
  
