# Practice phyloseq to calculate unifrac distance.

BiocManager::install('phyloseq')
library(phyloseq)

library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")

set.seed(711)
theme_set(theme_bw())
fontsize = 18L
theme_update(axis.title.x = element_text(size=fontsize))
theme_update(axis.title.y = element_text(size=fontsize))
theme_update(plot.title = element_text(size=fontsize+2))

# Load a small example dataset
data(esophagus)
esophagus

data(enterotype)
enterotype
enterotype <- subset_taxa(enterotype, Genus != "-1")
head(otu_table(enterotype))
# dist_methods <- unlist(distanceMethodList)
# print(dist_methods)

# These require tree
  dist_methods[(1:3)]

# Draw a tree
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
# Compare the methods in the distance() function 
# calc unifrac distance with 'unifrac'- UNweighted unifrac. # Does the same as weighted=F  
  distance(esophagus, method='unifrac', phy_tree(esophagus))
# this returns B-C 0.2035. 

# calc unifrac distance with 'wunifrac'- weighted unifrac. # Does the same as weighted=TRUE and normalized=TRUE 
  distance(esophagus, method='wunifrac', phy_tree(esophagus))
# this returns B-C 0.5175. 

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
  
  # Use ordinate function 
  GP.ord <- ordinate(GP1, method = "NMDS", distance = "bray")
  
  # plot
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
  # Define a human-associated versus non-human categorical variable:
  ?get_variable
  
  # Test if the contents of the 'SampleType' variable are either "Feces", "Mock", "Skin", or "Tongue", and
  #   make a vector saving TRUE or FALSE for all the contents. 
  human = get_variable(GP1, "SampleType") %in% c("Feces", "Mock", "Skin", "Tongue")
  
  # Make human column in sample_data(GP1) as a factor.
  sample_data(GP1)$human <- factor(human)
  
  
# ---------------------------------------------------------------------------------------------------------------
# MDS ("PCoA") on Unifrac Distances (I think this is what I want???)
  # Use the ordinate function to simultaneously perform weightd UniFrac and then perform a 
  # Principal Coordinate Analysis on that distance matrix (first line). 
  ordu = ordinate(GP1, method = "PCoA", distance = "unifrac", weighted=TRUE)
  
  # Next pass that data and the ordination results to plot_ordination to create 
  # the ggplot2 output graphic with default ggplot2 settings.
  p <- plot_ordination(GP1, ordu, color="SampleType", shape="human")
  p
  
  # Now make the graphic look nicer with a few additional ggplot2 layers.
  p = p + geom_point(size=7, alpha=0.75)
  p = p + scale_colour_brewer(type="qual", palette="Set1")
  p + ggtitle("MDS/PCoA on weighted-UniFrac distance, GlobalPatterns")
  
  
  p = ggplot(df, aes(Axis.1, Axis.2, color=SeqTech, shape=Enterotype))
  
  
?distance
?ordinate


