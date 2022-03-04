# format the data parts
require(phyloseq)
require(tidyr)

# need OTU table - this is our food OTU data
# food <- read.delim("Dropbox/dietstudy/data/processed_food/dhydrt.txt", row.names = 1)
  food <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.dhydrt.otu.txt", row.names = 1)
  food <- food[, !colnames(food) == "taxonomy"] # remove taxonomy column

# Taxonomy - this is the taxonomy data from food tree code, but forced into a tabular format
# tax <- read.delim("Dropbox/Food_Tree/R/output/mct.taxonomy.txt")
  tax <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.taxonomy.txt")
  colnames(tax)
  head(tax, 2)
  row.names(tax) <- tax$Main.food.description # Make food description as the row names
  # tax1 <- tax[, -1] # bad behavior - remove FoodID
  tax <- tax[, !colnames(tax) == "FoodID"] # remove FoodID column

# need to spread out the taxonomy column, separate on the ; delimiter; then label columns L1, L2, L3, etc.
  # tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4", "L5", "L6"), sep = ";")
  # There are only L1-L5.
  tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4", "L5"), sep = ";")
  tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4"), sep = ";")
  head(tax,2)
  
# drop the last column (Main.food.description because it's already made into row names.) 
  tax <- tax[, -6]

# Samples - this is our meta data file
  # meta <- read.delim("Dropbox/dietstudy/data/maps/SampleID_map.txt", row.names = 1, check.names = F)
  meta <- read.csv( "C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", 
                   row.names = 1, check.names = F)

#subset meta to the correct samples
  meta <- meta[colnames(food), ]

#transform to matrix
  food_mat <- as.matrix(food)
  tax_mat <- as.matrix(tax)

#transform to phyloseq objects
  OTU <- phyloseq::otu_table(food_mat, taxa_are_rows = TRUE)
  TAX <- phyloseq::tax_table(tax_mat)
  samples <- phyloseq::sample_data(meta)

# Read a tree file
  mcttree1 <- read_tree("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.tree.nwk")
  # mcttree1 <- ggtree::read.tree("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.tree.nwk")
  
  ggtree(mcttree1, layout = "circular") # it works!
  is(mcttree1)
  head(OTU, 1)

  head(taxa_names(OTU)) # 'Milk', "Milk cows fluid whole" etc.
  head(taxa_names(TAX)) # 'Milk', "Milk cows fluid whole" etc.
  head(taxa_names(mcttree1)) # "Milk_cows_fluid_whole" etc. need to replace underscores with spaces.
  
# Replace '_' with spaces in the tree object.
  taxa_names(mcttree1) <- gsub('_', ' ', taxa_names(mcttree1))
  head(taxa_names(mcttree1)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# Make a phyloseq object with just OTU and mcttree1.
  phyfoods1 <- phyloseq(OTU, mcttree1) # works
  
  sample_names(phyfoods1)
  rank_names(phyfoods1)
  sample_variables(phyfoods1)
  
# Use the ordinate function to simultaneously perform weighted UniFrac and then perform a 
# Principal Coordinate Analysis on that distance matrix. 
  ordmct1 = phyloseq::ordinate(phyfoods1, method = "PCoA", distance = "unifrac", weighted=TRUE)  
  # plot.
  plot_ordination(phyfoods1, ordmct1)   
  # works!!!!

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# Make a phyloseq object with OTU, TAX, and mcttree1.
  phyfoods2 <- phyloseq(OTU, TAX, mcttree1)

  # Gives an error:
  # Error in eigen(delta1) : infinite or missing values in 'x'
  # In addition: Warning message:
  #   In matrix(tree$edge[order(tree$edge[, 1]), ][, 2], byrow = TRUE,  :
  #               data length [59] is not a sub-multiple or multiple of the number of rows [30]
  
# Use the ordinate function to simultaneously perform weighted UniFrac and then perform a 
  # Principal Coordinate Analysis on that distance matrix. 
  ordmct2 = phyloseq::ordinate(phyfoods2, method = "PCoA", distance = "unifrac", weighted=TRUE)  
  # plot.
  plot_ordination(phyfoods2, ordmct2)   
  # works!!!!

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# Make a phyloseq object with OTU, TAX, samples, and mcttree1.
  phyfoods3 <- phyloseq(OTU, TAX, samples, mcttree1)
  phyfoods3
  
  head(sample_data(phyfoods3))
  L1s = tax_table(phyfoods3)[, "L1"]
  is(L1s)
  unique(L1s)
    sample_names(phyfoods3)
  rank_names(phyfoods3)
  sample_variables(phyfoods3)

# Use the ordinate function to simultaneously perform weighted UniFrac and then perform a 
  # Principal Coordinate Analysis on that distance matrix. 
  ordmct3 = phyloseq::ordinate(phyfoods3, method = "PCoA", distance = "unifrac", weighted=TRUE)  
  # works!!!!
  
  # plot foods (taxa).
  p1 = plot_ordination(phyfoods3, ordmct3, color="L1", type="taxa", title="taxa(foods)")
  p1
  
  # plot samples (people's days)
  p2 = plot_ordination(phyfoods3, ordmct3, type="samples", color="Gender") 
  p2 = plot_ordination(phyfoods3, ordmct3, type="samples", color="Timing") 
  p2
  p2 + geom_polygon(aes(fill=Timing)) + geom_point(size=5) + ggtitle("samples") # messy because of poor clustering
  
  # plot both foods (taxa) and samples (people's days)
  p3 = plot_ordination(phyfoods3, ordmct3, type="biplot", shape="Gender", color="L1", title="biplot") 
  p3
  
  
# Now make the graphic look nicer with a few additional ggplot2 layers.
  p <- p + geom_point(size=1, alpha=0.75)
  p <- p + scale_colour_brewer(type="qual", palette="Set1")
  p + ggtitle("MDS/PCoA on weighted-UniFrac distance, GlobalPatterns")
  
  
  phyfoodsplustree <- phyloseq::merge_phyloseq(phyfoods, mcttree1)
  phyfoodsplustree
  phy_tree(phyfoodsplustree)
  


  
  
  