# ========================================================================================
# Plot a food tree generated with dietary data.
# Version 1
# Created on 03/15/2022 by Rie Sadohara
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# use this working directory until this script is complete. 
  setwd("~/GitHub/dietary_patterns")

# Load the functions necessary to set directories.
  source("lib/specify_dir_and_check_col.R")

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

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

  
# ========================================================================================
# Load the "ggtree" package and source script for visualizing food trees.
# ========================================================================================
  
# If BiocManager has not installed yet, do so.
  if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  
# Then, use BiocManage to install the "ggree" package.
  BiocManager::install("ggtree")

# Load the source script as well.
  source("lib/viz_food_tree.r")
  
# ---------------------------------------------------------------------------------------------------------------
# Load your ggtree object. 
  #   tree <- read.tree("results/Food_tree_ASA24/mct.reduced_4Lv.tree.nwk")
  #   tree <- read.tree("results/Food_tree_ASA24/mct_Lv2.tree.nwk")
  #   tree <- read.tree("results/Food_tree_ASA24/mct.reduced_1Lv.tree.nwk")
  #   
  #   # VVKAJ
  #   tree <- read.tree("results/Food_tree_ASA24/VVKAJ.reduced_2Lv.tree.nwk")
  #   tree <- read.tree("results/Food_tree_ASA24/VVKAJ.reduced_1Lv.tree.nwk")
  #   
  # # VVKAJ_NEW!!
  
# Go to the "Foodtree" directory where the tree files are saved. 
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Foodtree")
  
# Load the generated food tree. This will load the .nwk file and save it as a tree object called "tree".
  tree <- read.tree("VVKAJ_Items_f_s_m_ff_reduced_4Lv.tree.nwk")
  tree
  
    # NHANES
    tree <- read.tree("results/Food_tree_NHANES/NHANES1516.reduced_Lv1.tree.nwk")

# Not quite useful with so many samples...
# # Use ggtree to plot the tree. It is critical to have 'ladderize=FALSE' argument to preserve your L1 order.
#   mytreeplot <- ggtree(tree, ladderize=FALSE, layout = 'radial') + 
#     geom_tiplab()
#   mytreeplot
#   
# # Without the tip labels.
#   mytreeplot <- ggtree(tree, ladderize = F, layout = 'radial') 
#   mytreeplot
#   
# # Show the node numbers only. 
#   mytreeplot <- ggtree(tree, ladderize = F, layout = 'radial') +
#                        geom_text(aes(label=node), hjust= -0.1) 
#   mytreeplot

# ---------------------------------------------------------------------------------------------------------------
# Prepare node labels of L1 for plotting. It assumes that the tree file has 9 L1 levels.
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with 9 L1 levels.
  # Choose either 'circular' or 'radial' for layout.
  # It is OK to see some warning messages about Coordinate system and scale for 'y' already being present.
  VizFoodTree(input.tree=tree, layout="radial")
  
# Look at the color-coded and annotated food tree, saved as annotated_tree.
  annotated_tree
  
# Save the tree as a PDF file. 
  ggsave("VVKAJ_Items_f_s_m_ff_reduced_4Lv.tree.pdf", annotated_tree, device="pdf", width=6, height=6, units="in", dpi=300)
  
# Or a png file.
  ggsave("VVKAJ_Items_f_s_m_ff_reduced_4Lv.tree.png", annotated_tree, device='png', width=6, height=6, dpi=300)

  
  