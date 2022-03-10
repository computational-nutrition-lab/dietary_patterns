# ========================================================================================
# Create a phylseq object from MCT diet data and perform 
# unifrac distance and PCA (multidimensional scaling) just like Qiime
# Version 1
# Created on 03/02/2022 by Abby Johnson and Rie Sadohara
# ======================================================================================== 
# format the data parts
  require(phyloseq)
  require(tidyr)
  require(ggplot2)
  theme_set(theme_bw())
  fontsize = 18L
  theme_update(axis.title.x = element_text(size=fontsize))
  theme_update(axis.title.y = element_text(size=fontsize))
  theme_update(plot.title   = element_text(size=fontsize+2))

# --------------------------------------------------------------------------------------------------------
# Load and format OTU, taxonomy, metadata, and tree files to create phyloseq objects.
# At least OTU and tree are necessary to create a phyloseq object.

# format OTU table - this is our food OTU data
# food <- read.delim("Dropbox/dietstudy/data/processed_food/dhydrt.txt", row.names = 1)
  food <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.dhydrt.otu.txt", row.names = 1)
  food <- food[, !colnames(food) == "taxonomy"] # remove taxonomy column
  
  #transform to matrix
  food_mat <- as.matrix(food)
  
  # transform to phyloseq objects
  OTU <- phyloseq::otu_table(food_mat, taxa_are_rows = TRUE)

# format Taxonomy - this is the taxonomy data from food tree code, but forced into a tabular format
# tax <- read.delim("Dropbox/Food_Tree/R/output/mct.taxonomy.txt")
  tax <- read.delim("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.taxonomy.txt")
  colnames(tax)
  head(tax, 2)
  row.names(tax) <- tax$Main.food.description # Make food description as the row names
  tax <- tax[, !colnames(tax) == "FoodID"] # remove FoodID column 

  # need to spread out the taxonomy column, separate on the ; delimiter; then label columns L1, L2, L3, etc.
  # tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4", "L5", "L6"), sep = ";")
  # There are only L1-L5.
  tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4", "L5"), sep = ";")
  tax <- tidyr::separate(tax, taxonomy, into = c("L1", "L2", "L3", "L4"), sep = ";")
  
  # drop the last column (Main.food.description because it's already made into row names.) 
  colnames(tax)
  tax <- tax[, -length(colnames(tax))]
  
  head(tax[, "L5"], 20)

  #transform to matrix
  tax_mat <- as.matrix(tax)

  # transform to phyloseq objects
  TAX <- phyloseq::tax_table(tax_mat)
  
# Samples - this is our meta data file
  # meta <- read.delim("Dropbox/dietstudy/data/maps/SampleID_map.txt", row.names = 1, check.names = F)
  meta <- read.csv( "C:/Users/sadoh/OneDrive/Documents/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", 
                   row.names = 1, check.names = F)

  #subset meta to the correct samples
  meta <- meta[colnames(food), ]
  head(meta)

  # transform to phyloseq objects
  samples <- phyloseq::sample_data(meta)

# Read a tree file
  mcttree1 <- read_tree("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.tree.nwk")
  # mcttree1 <- ggtree::read.tree("E:/MSU OneDrive 20210829/UMinn/Food_Tree-master/R/output/mct.reduced_4Lv.tree.nwk")
  
  ggtree(mcttree1, layout = "circular") # it works!
  is(mcttree1)
  head(OTU, 1)

  # Check if the food names are the same in OTU, TAX, and mcttree1.
  head(taxa_names(OTU)) # 'Milk', "Milk cows fluid whole" etc.
  head(taxa_names(TAX)) # 'Milk', "Milk cows fluid whole" etc.
  head(taxa_names(mcttree1)) # "Milk_cows_fluid_whole" etc. need to replace underscores with spaces.
  
  # Replace '_' with spaces in the tree object.
  taxa_names(mcttree1) <- gsub('_', ' ', taxa_names(mcttree1))
  head(taxa_names(mcttree1)) 

# --------------------------------------------------------------------------------------------------------
# Create phyloseq objects!!
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    # Make a phyloseq object with just OTU and mcttree1.
      phyfoods1 <- phyloseq(OTU, mcttree1) # works
      
      sample_names(phyfoods1)
      rank_names(phyfoods1)
      
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

# --------------------------------------------------------------------------------------------------------
# Use the ordinate function to perform weighted UniFrac and then perform a 
  # Principal Coordinate Analysis on that distance matrix. 
  ordmct3 = phyloseq::ordinate(phyfoods3, method="PCoA", distance="unifrac", weighted=TRUE)  
  
  # plot foods (taxa).
  p1 = plot_ordination(phyfoods3, ordmct3, color="L1", type="taxa", title="taxa(foods)")
  p1 + geom_point(size=2) + theme(aspect.ratio = 1)
  
  # plot samples (by gender, timing, people, etc as in the meta file)
  p2 = plot_ordination(phyfoods3, ordmct3, type="samples", color="Gender") 
  p2 = plot_ordination(phyfoods3, ordmct3, type="samples", color="UserName") # ordmct3(weighted) & UserName 
  p2 + geom_point(size=2) + theme(aspect.ratio = 1) + stat_ellipse()
  p2 + geom_point(size=2) + theme(aspect.ratio = 1) + geom_line()
  
  # make a polygon by UserName
  p2 + geom_polygon(aes(fill=UserName)) + geom_point(size=3) + theme(aspect.ratio=1) + ggtitle("Samples") 
  # messy because of poor clustering
  
  # plot both foods (taxa) and samples (people)
  p3 = plot_ordination(phyfoods3, ordmct3, type="biplot", shape="L1", color="UserName", title="Biplot") +
    scale_shape_manual(values = c(1:10)) + geom_point(size=2) + theme(aspect.ratio = 1) +
  p3

# --------------------------------------------------------------------------------------------------------  
  # What are the dark yellow points in samples plot by UserName that are kind of far away from other users?
  # extract vectors
  vectors <- as.data.frame(ordmct3[4])
  vectors <- vectors[, c("vectors.Axis.1", "vectors.Axis.2")]
  colnames(vectors)
  head(vectors)
  # Show points with the highest Axis.1 values.
  sortedv <- vectors[order(vectors$vectors.Axis.1, decreasing = T), ]
  sortedv1 <- subset(sortedv, vectors.Axis.1 > 0.05 & vectors.Axis.2 > 0.04)
  nrow(sortedv1)
  # Match UserName
  head(meta, 1)
  # make rownames as a column for merging.
  meta$MCTXXX     <- rownames(meta)
  sortedv1$MCTXXX <- rownames(sortedv1)
  # Match MCTXXX (sample ID) and UserName.  
  sortedv1name <- merge(sortedv1, meta, by="MCTXXX", all.x=T)
  head(sortedv1name)
  table(sortedv1name$UserName)
  sortedv1name[, 1:4]
  # The green points are MCTs08 (n=16) and MCTs10 (n=1). 
  # Where are the other MCTs10?
  dim(vectors)
  vectors$MCTXXX <- rownames(vectors)
  # merge full 
  vectorsplusmeta <- merge(vectors, meta, by="MCTXXX", all.x=T) 
  head(vectorsplusmeta)
  MCT11 <- subset(vectorsplusmeta, UserName=="MCTs11")
  MCT10[, 1:4]
  plot(MCT10$vectors.Axis.1, MCT10$vectors.Axis.2)
  
# --------------------------------------------------------------------------------------------------------
# unweighted UniFrac, to compare with Johnson et al. 2019 paper 
  ordmct4 = phyloseq::ordinate(phyfoods3, method = "PCoA", distance = "unifrac", weighted=F)  
  
  # plot foods (taxa).
  p1 = plot_ordination(phyfoods3, ordmct4, color="L1", type="taxa", title="taxa(foods)") + theme(aspect.ratio = 1)
  
  # plot samples (by gender, timing, people, etc as in the meta file)
  p2 = plot_ordination(phyfoods3, ordmct4, type="samples", color="UserName") # ordmct4(unweighted) & UserName
  p2 + geom_point(size=2) + theme(aspect.ratio = 1) + stat_ellipse()
  p2 + geom_point(size=2) + theme(aspect.ratio = 1) + geom_line()
  
  # make a polygon by UserName
  p2 + geom_polygon(aes(fill=UserName)) + geom_point(size=3) + theme(aspect.ratio=1) + ggtitle("samples") 
  # messy because of poor clustering
  
  # plot both foods (taxa) and samples (people)
  p3 = plot_ordination(phyfoods3, ordmct4, type="biplot", shape="L1", color="UserName", title="biplot") 
  p3
  
# --------------------------------------------------------------------------------------------------------  
# What are the purple points in taxa(foods) plot that are so far away from everyone else?
# -- Not sure, ordmct4 only has vectors for samples, not taxa(foods). So... not sure how it's 
  # making the taxa plot without the actual values.
  
# --------------------------------------------------------------------------------------------------------  
# What are the green points in samples plot by UserName that are far away from everyone else?
  # extract vectors
  vectors <- as.data.frame(ordmct4[4])
  vectors <- vectors[, c("vectors.Axis.1", "vectors.Axis.2")]
  colnames(vectors)
  head(vectors)
  # Show points with the highest Axis.1 values.
  sortedv <- vectors[order(vectors$vectors.Axis.1, decreasing = T), ]
  sortedv1 <- subset(sortedv, vectors.Axis.1 > 0.2)
  nrow(sortedv1)
  # Match UserName
  head(meta, 1)
  # make rownames as a column for merging.
  meta$MCTXXX     <- rownames(meta)
  sortedv1$MCTXXX <- rownames(sortedv1)
  # Match MCTXXX (sample ID) and UserName.  
  sortedv1name <- merge(sortedv1, meta, by="MCTXXX", all.x=T)
  sortedv1name
  table(sortedv1name$UserName)
# The green points are MCTs11 and MCTs12, who had meal replacements! 
  
# --------------------------------------------------------------------------------------------------------  
# Exclude MCTs11 and MCTs12, and run unweighted unifrac distance. 
  dim(sortedv1)
  head(sortedv1)
 
# --------------------------------------------------------------------------------------------------------
# Use the ordinate function to simultaneously perform weighted UniFrac and then perform a 
  # Double Principal Coordinate Analysis on that distance matrix. 
  ordmct5 = phyloseq::ordinate(phyfoods3, method = "DPCoA", distance="unifrac") #, weighted=TRUE)  
# Takes too much time!! More than 16 min with the MCT data and did not finish.
  
  # A quick eg of DPCoA with esophagus.
  # But esophagus has only 3 samples and no metadata, so cannot create color-coded plots. 
  data(esophagus)
  eso3 = phyloseq::ordinate(esophagus, method = "PCoA", distance="unifrac", weighted=T)
  eso4 = phyloseq::ordinate(esophagus, method = "PCoA", distance="unifrac", weighted=F)
  eso5 = phyloseq::ordinate(esophagus, method = "DPCoA", distance="unifrac")
  plot_ordination(esophagus, eso5, type="taxa", title="Taxa(esophagus)") + geom_point(size=2) + theme(aspect.ratio = 1)
  plot_ordination(esophagus, eso5, type="samples", title="Samples(esophagus)") +
    geom_point(size=2) + geom_line() + theme(aspect.ratio = 1)
  plot_ordination(esophagus, eso5, type="biplot", title="Biplot(esophagus)") + theme(aspect.ratio=1)
  plot_scree(eso5, title = "scree plot(esophagus)")
 
  # A quick eg of DPCoA with GP1.
  data(enterotype)
  GP1_3 = phyloseq::ordinate(GP1, method = "PCoA", distance="unifrac", weighted=T)
  GP1_4 = phyloseq::ordinate(GP1, method = "PCoA", distance="unifrac", weighted=F)
  GP1_5 = phyloseq::ordinate(GP1, method = "DPCoA", distance="unifrac")
  plot_ordination(GP1, GP1_5, type="taxa", title="Taxa(GP1)", color="Phylum") + geom_point(size=2) + theme(aspect.ratio = 1)
  plot_ordination(GP1, GP1_5, type="samples", title="Samples(GP1)", color="SampleType") +
    geom_point(size=2) + geom_line() + theme(aspect.ratio = 1)
  plot_ordination(GP1, GP1_5, type="biplot", title="Biplot(GP1)", color="SampleType") + theme(aspect.ratio=1)
  plot_scree(GP1_3, title="scree plot(GP1)")
  # method="DPCoA" runs fine with a small dataset like GP1, but takes too long with large datasets like MCT (7 MB)
  # If do not want to worry about dataset size, just use PCoA with weighted unifrac distance...
  # Or let users choose DPCoA if they want to try.
 
 


  
  
  