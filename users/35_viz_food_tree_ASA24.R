# ===============================================================================================================
# Plot a food tree generated with dietary data.
# Version 2
# Created on 08/30/2022 by Rie Sadohara
# ===============================================================================================================

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.
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

# Name your main directory for future use. 
  main_wd <- file.path(getwd())
  
# Load the functions necessary to set directories.
  source("lib/specify_data_dir.R")


# ===============================================================================================================
# Load the "ggtree" package and source script for visualizing food trees.
# ===============================================================================================================
  
# If BiocManager has not installed yet, do so.
  if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  
# Then, use BiocManage to install the "ggree" package.
  BiocManager::install("ggtree")

# Load the source script as well.
  source("lib/viz_food_tree.r")
  
# ---------------------------------------------------------------------------------------------------------------
# Go to the "Foodtree" directory where the tree files are saved. 
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Foodtree")
  
# Load the generated food tree. This will load the .nwk file and save it as a tree object called "tree".
# It is OK to see a message saying: 
# Found more than one class "phylo" in cache; using the first, from namespace 'phyloseq'
# Also defined by 'tidytree'  
  tree <- read.tree("VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tree.nwk")
  tree
  
# Prepare node labels of L1 for plotting. It assumes that the tree file has 9 L1 levels.
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with 9 L1 levels.
  # Choose either 'circular' or 'radial' for layout.
  # It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="radial")
  
# Look at the color-coded and annotated food tree, saved as annotated_tree.
  annotated_tree
  
# Save the tree as a PDF file. 
  ggsave("VVKAJ_Items_f_id_s_m_ff_reduced_4Lv.tree.pdf", 
         annotated_tree, device="pdf", width=6, height=6, units="in", dpi=300)
  
  
  