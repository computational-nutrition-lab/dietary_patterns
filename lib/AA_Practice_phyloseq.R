# Practice phyloseq to calculate unifrac distance.

  # BiocManager::install('phyloseq')
  library(phyloseq)
  
  library("phyloseq"); packageVersion("phyloseq")
  library("ggplot2"); packageVersion("ggplot2")
  
  set.seed(711)
  theme_set(theme_bw())
  fontsize = 18L
  theme_update(axis.title.x = element_text(size=fontsize))
  theme_update(axis.title.y = element_text(size=fontsize))
  theme_update(plot.title   = element_text(size=fontsize+2))

# Load a small example dataset
  data(esophagus)
  esophagus

  data(enterotype)
  enterotype
  enterotype <- subset_taxa(enterotype, Genus != "-1")
  head(otu_table(enterotype))

# These require a tree
#  dist_methods <- unlist(distanceMethodList)
#  dist_methods[(1:3)]

# Draw a tree
  library(ggtree)
  ggtree(phy_tree(esophagus), layout = 'circular')
  
# ---------------------------------------------------------------------------------------------------------------
# Compare unifrac distance argument options. 
# calc unifrac distance, weighted, normalized 
  UniFrac(esophagus, weighted=TRUE, normalized=TRUE, parallel=FALSE, fast=TRUE)
# this returns B-C 0.2035

# calc unifrac distance, weighted only
  UniFrac(esophagus, weighted=TRUE, normalized=F, parallel=FALSE, fast=TRUE)
# this returns B-C 0.1050

# calc unifrac distance, normalized only
  UniFrac(esophagus, weighted=F, normalized=TRUE, parallel=FALSE, fast=TRUE)
# this returns B-C 0.5175

# calc unifrac distance, not weighted, not normalized
  UniFrac(esophagus, weighted=F, normalized=F, parallel=FALSE, fast=TRUE)
# this returns B-C 0.5175

# ---------------------------------------------------------------------------------------------------------------
# Compare the methods in the distance() function in phyloseq. 
# calc unifrac distance with 'unifrac'- UNweighted unifrac. # Does the same as weighted=F  
  distance(esophagus, method='unifrac', phy_tree(esophagus))
# this returns B-C 0.5175. 

# calc unifrac distance with 'wunifrac'- weighted unifrac. # Does the same as weighted=TRUE and normalized=TRUE 
  distance(esophagus, method='wunifrac', phy_tree(esophagus))
# this returns B-C 0.2035. 

# use 'dpcoa' = sample-wise distance used in Double Principle Coordinate Analysis, DPCoA
  distance(esophagus, method='dpcoa', phy_tree(esophagus))
# this returns B-C 0.08700, different from all the distances calculated above. Hmmm
  
# ---------------------------------------------------------------------------------------------------------------
# Make a biplot anyway...
# Use ordinate function with 'DPCoA' as the method because I want to make a biplot.
  eso.ord <- ordinate(esophagus, method = 'DPCoA', distance = 'wunifrac')
  
# Plot the ordinate product...
  plot_ordination(esophagus, eso.ord, type="taxa", color="Phylum", title="taxa")

# No available covariate data to map on the points for this plot `type`
# Need a larger dataset to do it. 
  
# ---------------------------------------------------------------------------------------------------------------
# Borrow from the tutorial. 
  library("plyr"); packageVersion("plyr")
  
  data("GlobalPatterns")
  GP = GlobalPatterns
  # Remove OTUs that do not show appear more than 5 times in more than half the samples
  wh0 = genefilter_sample(GP, filterfun_sample(function(x) x > 5), A=0.5*nsamples(GP))
  GP1 = prune_taxa(wh0, GP)

  # Transform to even sampling depth.
  GP1 = transform_sample_counts(GP1, function(x) 1E6 * x/sum(x))
  
  # Keep only the most abundant five phyla.
  phylum.sum = tapply(taxa_sums(GP1), tax_table(GP1)[, "Phylum"], sum, na.rm=TRUE)
  top5phyla = names(sort(phylum.sum, TRUE))[1:5]
  GP1 = prune_taxa((tax_table(GP1)[, "Phylum"] %in% top5phyla), GP1)
  # That still leave 204 OTUs in the dataset, GP1.

  # calc unifrac distance, not weighted, not normalized
  UniFrac(GP1, weighted=F, normalized=F, parallel=FALSE, fast=TRUE)
  
  # Use ordinate function. NMDS=Non-metric multidimensional scaling
  GP.ord <- ordinate(GP1, method = "NMDS", distance = "bray")
  GP.ord <- ordinate(GP1, method = "PCoA", distance = "unifrac", weighted=TRUE)
  
  # plot
  p1 = plot_ordination(GP1, GP.ord, color="Phylum", type="taxa", title="taxa")
  p1
  p1 + facet_wrap(~Phylum, 3)
  
  p2 = plot_ordination(GP1, GP.ord, type="samples", color="SampleType", shape="human") 
  p2 + geom_polygon(aes(fill=SampleType)) + geom_point(size=5) + ggtitle("samples")
  
  p3 = plot_ordination(GP1, GP.ord, type="biplot", color="SampleType", shape="Phylum", title="biplot")
  p3
  
  # Some stuff to modify the automatic shape scale
  GP1.shape.names = get_taxa_unique(GP1, "Phylum")
  GP1.shape <- 15:(15 + length(GP1.shape.names) - 1)
  names(GP1.shape) <- GP1.shape.names
  GP1.shape["samples"] <- 16
  p3 + scale_shape_manual(values=GP1.shape)
  
  # We will want to investigate a major prior among the samples, which is that some are 
  #   human-associated microbiomes, and some are not. 
  # Define a human-associated versus non-human categorical variable in 2 steps:
  ?get_variable
  
  # 1. Test if the contents of the 'SampleType' variable are either "Feces", "Mock", "Skin", or "Tongue", and
  #   make a vector saving TRUE or FALSE for all the contents. 
  human = get_variable(GP1, "SampleType") %in% c("Feces", "Mock", "Skin", "Tongue")
  
  #2. Make human column in sample_data(GP1) as a factor.
  sample_data(GP1)$human <- factor(human)
  
  
# ---------------------------------------------------------------------------------------------------------------
# MDS ("PCoA") on Unifrac Distances. MDS=Multidimentional Scaling.   (I think this is what I want)
  # Use the ordinate function to simultaneously perform weighted UniFrac and then perform a 
  # Principal Coordinate Analysis on that distance matrix. 
  ordu = ordinate(GP1, method = "PCoA", distance = "unifrac", weighted=TRUE)
    # This one returns the same results as this line with "wunifrac":
    # ordu = ordinate(GP1, method = "PCoA", distance = "wunifrac")
  ?ordinate
  
  # Next pass that data and the ordination results to plot_ordination to create 
  # the ggplot2 output graphic with default ggplot2 settings.
  p <- plot_ordination(GP1, ordu, color="SampleType", shape="human")
  p
  
  # Now make the graphic look nicer with a few additional ggplot2 layers.
  p <- p + geom_point(size=7, alpha=0.75)
  p <- p + scale_colour_brewer(type="qual", palette="Set1")
  p + ggtitle("MDS/PCoA on weighted-UniFrac distance, GlobalPatterns")
  
  p = ggplot(df, aes(Axis.1, Axis.2, color=SeqTech, shape=Enterotype))
  
# ---------------------------------------------------------------------------------------------------------------
  # Explore examples
  data(GlobalPatterns)
  data(enterotype)
  data(esophagus)
  data(soilrep)
  example(enterotype, ask = F)
  example(esophagus, ask = F)
  ?esophagus
  ?GlobalPatterns
  ?enterotype
  phy_tree(esophagus)
  otu <- otu_table(GP1) # otu is an otu_table and a matrix.
  tax <- tax_table(GP1) # tax is a  tax_table and a matrix.
  sam <- sam_data(GP1)  # sam is a  tax_table and a matrix.
  tre <- phy_tree(GP1)  # tre is a phylogenetic tree.
  is(otu)
  is(tax)
  is(sam)
  is(tre)
  dim(otu)
  dim(tax)
  dim(sam)
  dim(tre)
  colnames(otu)
  colnames(tax)
  colnames(sam)
  colnames(tre)
  head(otu) # amount? 
  head(tax) # Kingdom, Phylum, Class, etc.
  head(sam) # Barcode sequence, sample type, description, human = T or F, etc. Info for plotting. 
  head(tre) # lots of info.... 
  length(taxa_names(otu))
  length(taxa_names(tax))
  
# ---------------------------------------------------------------------------------------------------------------
# What I want to do is to run MDS/PCoA on weighted-UniFrac distance between foods..
# esophagus is a small dataset, but can do ordination and plot.
  ordu = ordinate(esophagus, method = "PCoA", distance = "unifrac", weighted=TRUE)
  plot_ordination(esophagus, ordu) 
  # esophagus only has otu_table and phy_tree. (tax_table, sam_data and ref_seq are NULL.)
  # So, it seems possible to do ordinate with just otu_table and phy_tree. 
  otu <- otu_table(esophagus) # otu is an otu_table and a matrix.
  is(otu)
  dim(otu) # 58 taxa and 3 samples.
  head(otu, 10) 
  library(ggtree)
  ggtree(phy_tree(esophagus), layout = 'circular')
  
