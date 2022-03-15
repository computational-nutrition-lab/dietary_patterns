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
  
  # VVKAJ
  tree <- read.tree("output/VVKAJ.reduced_2Lv.tree.nwk")
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
  
# Save a tree as a tiff file. 
  ggsave("VVKAJ_tree.tif", mytreeplot, width=10, height=10, device='tiff', dpi=150)

# ---------------------------------------------------------------------------------------------------------------
# Prepare node labels of L1 for plotting.  
  PrepFoodTreePlots(input.tree=tree)
  
# Create a color-coded and annotated food tree with 9 L1 levels. 
  VizFoodTree(input.tree=tree, layout="circular")
  
  # Look at the color-coded and annotated food tree.
  tree_an_hi
  
  
  
# # Function to do some prep for plotting L1 labels.
#   # Do some preparation to highlight and annotate.
#   # Save the tip labels as a vector for processing.
#   PrepFoodTreePlots <- function(tree=tree){
# 
#     tiplabels <<- tree[["tip.label"]]
#     length(tiplabels)  # 134 food items = tips for VVKAJ.
#     
#     # Make the nodes (root, L1s and L2s and so on) into a dataframe.   
#     nodelabels <<- tree[["node.label"]]
#     length(nodelabels) # 42 = 1 root + 41 L1s&L2s.
#     nodelabelsdf <<- data.frame(nodelabels= nodelabels, 
#                                 level= substr(nodelabels, 1,2), # Take the 1st and 2nd characters from nodelabels. 
#                                 seqnum= seq(1:length(nodelabels)),   
#                                 nodenum= seq(1:length(nodelabels))+length(tiplabels)  # This corresponds to the node numbers in the plot.
#     )
#     
#     # Replace 'fo' to 'root'
#     nodelabelsdf[1, 2] <- 'root'
#     
#     # Take only the rows that are L1 and find their nodenumbers in order to annotate nodes.
#     L1s <<- subset(nodelabelsdf, level=='L1')  
#     # This should have 9 rows if all L1 levels are present in your dataset.
#     # if for some reason there are not 9, need to decrease the number of colors specified below. 
#     
#     # Make a reference table of the full node names and shorter node names.
#     L1sref <<- data.frame(nodelabels = L1s$nodelabels,
#                           shortnodelabels=c("Milks",                  "Meats",
#                                             "Eggs",                   "Legumes",
#                                             "Grains",                 "Fruits", 
#                                             "Vegetables",             "Fats", 
#                                             "Sweets&\nBeverages"),
#                           hilightcolors=  c('lightblue',              'firebrick1',        # Add 9 colors for highlighting each L1.
#                                             'orange',                 'royalblue',
#                                             'gold',                   'darkorchid',
#                                             'seagreen',               'lightgreen',
#                                             'rosybrown1'),
#                           L1labelcolors=  c('lightblue4',             'firebrick',         # Add 9 colors for annotating each L1.
#                                             'darkorange',            'darkblue',
#                                             'gold4',                 'darkorchid',
#                                             'seagreen',               'limegreen',
#                                             'mediumvioletred'))
#     
#     # Merge the shortnodelabels to L1s
#     merged1 <<- merge(x=L1s, y=L1sref, all.x=T, by='nodelabels') # all.x=T ignores items in y that is missing in x. 
#     
#     # merge() sorts rows automatically, so need to re-sort it to the original order - by seqnum.
#     # and sort the columns also so that nodelabels (column 1) and shortnodelabels (column 5) will be next to each other.
#     L1s <<- merged1[order(merged1$seqnum), c(1,5,2:4,6:7)]
#     
#     # Make vectors for plotting.
#     L1nodenum       <<- L1s$nodenum
#     L1nodelabels    <<- L1s$shortnodelabels
#     L1hilightcolors <<- L1s$hilightcolors
#     L1labelcolors   <<- L1s$L1labelcolors
#   }
# 
#   
#   
#   
#   
#   
#   
#   
#   
#   