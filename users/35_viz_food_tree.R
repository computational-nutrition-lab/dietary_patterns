# ========================================================================================
# Plot a food tree generated with dietary data.
# Version 1
# Created on 03/15/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# 
# ========================================================================================
# 
# ---------------------------------------------------------------------------------------------------------------
    # use this working directory until this script is complete. 
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

# Set your working directory as the main directory (dietary_patterns)
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main.wd <- file.path(getwd())

  
# ========================================================================================
# Load source scripts
# ========================================================================================
  source("lib/viz_food_tree.r")
  source("lib/")
  
# ---------------------------------------------------------------------------------------------------------------
# Load your ggtree object. 
  tree <- read.tree("results/Food_tree_results/mct.reduced_4Lv.tree.nwk")
  tree <- read.tree("results/Food_tree_results/mct.reduced_1Lv.tree.nwk")
  
  # VVKAJ
  tree <- read.tree("output/VVKAJ.reduced_2Lv.tree.nwk")
  tree <- read.tree("output/VVKAJ.reduced_1Lv.tree.nwk")
  tree

# Use ggtree to plot the tree. It is critical to have ladderize=F argument to preserve your L1 order.
  mytreeplot <- ggtree(tree, ladderize=F, layout = 'radial') + # disable ladderizing (sorting by ggtree, CRITICAL!!!)
    geom_tiplab()
  
# Plot a circular tree. - hard to see with lots of samples
  mytreeplot <- ggtree(tree, ladderize = F, layout = 'circular') +
    geom_tiplab()
  mytreeplot
  
# Show the node numbers only. 
  ggtree(tree, ladderize = F, layout = 'circular') +
    geom_text(aes(label=node), hjust= -0.1) 
  
# Save a tree as a tiff file if you'd like. 
  ggsave("VVKAJ_tree.tif", mytreeplot, width=10, height=10, device='tiff', dpi=150)

# ---------------------------------------------------------------------------------------------------------------
# Prepare node labels of L1 for plotting. It assumes that there are 9 L1 levels.
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with 9 L1 levels.
  # Choose either 'circular' or 'radial' for layout.
  # It is OK to see some warning messages.
  VizFoodTree(input.tree=tree, layout="radial")
  
# Look at the color-coded and annotated food tree, saved as tree_an_hi_o_rt.
  tree_an_hi_o_rt
  
# Save the tree as a tiff file if you'd like. 
  ggsave("mytree.tif", tree_an_hi_o_rt, width=10, height=10, device='tiff', dpi=150)
  

  
  