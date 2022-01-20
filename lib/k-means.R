# FUNCTIONS ==============================================================================

# ========================================================================================
# k-means clustering 
#  
# Version 1
# Created on 01.06.2022 by Rie Sadohara
# ========================================================================================

# Codes to build:
# Collapse variables by correlation.  --OK!
# option to average by participants or not. --> prep_data. -- OK!
# Find the optimum k. 

# ---------------------------------------------------------------------------------------------------------------
# Define your input file. Need to scale it to accomodate mesurements in different units.  
  colnames(selected_variables)  
  kmeans_input <- scale(selected_variables) # correlated variables removed.
  # kmeans_input <- subsetted_non0var  # before removing correlated variables.

# Set your ggplot2 theme.
  require(ggplot2)
  theme_set(theme_bw(base_size = 14))
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Find the ideal k
#  Modified code from https://uc-r.github.io/kmeans_clustering
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Function to find the ideal k: Elbow method
  set.seed(123)
  
  # function to compute total within-cluster sum of square 
  wss <- function(k, data) {
    kmeans(data, k, nstart = 25)$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:15
  
  # extract wss for 2-15 clusters
  wsstable <- data.frame(K=k.values, WithinClusterSS=NA)
  for(i in k.values){
    wssvalue <- wss(k.values[i], data = kmeans_input)
    wsstable[i, 2] <- wssvalue
  }
  wsstable
  
  # Plot the within-clusters SS for each K, and look for the elbow 
  # (= the first k at which the SS of k+1 is minimal)
  require(ggplot2)
  ggplot(wsstable, aes(x = K, y = WithinClusterSS)) + 
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = 1:nrow(wsstable)) +
    labs(x = "Number of clusters K",
         y = "Total within-clusters sum of squares") +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
    theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
    theme(aspect.ratio = 0.9)
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find the ideal k: the Silhouette method
  # need cluster package
  library(cluster)
 
  # function to compute average silhouette for k clusters
  avg_sil <- function(k) {
    km.res <- kmeans(kmeans_input, centers=k, nstart=25)
    ss <<- silhouette(km.res$cluster, dist(kmeans_input))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:15
  # extract avg silhouette for 2-15 clusters (using map_dbl function)
  avg_sil_values <- purrr::map_dbl(k.values, avg_sil)
  avg_sil_values
  
      # extract avg silhouette values for 2-15 clusters (using base R functions)
      siltable <- data.frame(K=k.values, Avg_Silhouette=NA)
      for(i in k.values){
        # silvalue <- avg_sil(k.values[i], data=kmeans_input )
        silvalue <- avg_sil(k.values[i])
        silvalue
      }
      avg_sil(k.values[15])
      # Hmm?? error...   
 
  # Create a data frame with the sil values for plotting.
  avg_sil_values_df <- data.frame(K=k.values, Avg_sil=avg_sil_values) 
  
  require(ggplot2)
  ggplot(avg_sil_values_df, aes(x = K, y = Avg_sil)) + 
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = 1:nrow(avg_sil_values_df)) +
    labs(x = "Number of clusters K",
         y = "Average Silhouettes") +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
    theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
    theme(aspect.ratio = 0.9)
  # The K with the max average Silhouette value is the ideal K. 
  
  # Or use factoextra package to use the silhouette method to identify the optimum K.
  factoextra::fviz_nbclust(kmeans_input, kmeans, method="silhouette")
  # This plots 'average Silhouette width' instead of 'average Silhouette' for some reason.
  # But the result (ideal K) is the same.
 
# ---------------------------------------------------------------------------------------------------------------
# Find the ideal k: Gap static method
  set.seed(123)
  k.values <- 1:15
  
  # Calculate the gap statistic.
  library(cluster)
  gap_stat <- clusGap(kmeans_input, FUN = kmeans, nstart = 25,
                      K.max=15, #k.values[length(k.values)], 
                      B=50) # B is the number of bootstrapping
  # Print the result.
  print(gap_stat, method = "firstmax")
  
  # Convert the table to a dataframe first.
  gap_stat_df <- as.data.frame(gap_stat[1])
  # Add the number of clusters as a new column.
  gap_stat_df$NumberofK <- k.values 
  
  # Plot the gap statistic with ggplot2
  require(ggplot2)
  ggplot(gap_stat_df, aes(x=NumberofK, y=Tab.gap)) + 
    geom_line() + 
    geom_point() +
    geom_errorbar(aes(ymin=Tab.gap-Tab.SE.sim, 
                      ymax=Tab.gap+Tab.SE.sim),  
                  width=0.2, 
                  position=position_dodge(0.05)) +
    scale_x_continuous(breaks = 1:nrow(gap_stat_df)) +
    labs(x = "Number of clusters K",
         y = "Gap stastistic") +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
    theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
    theme(aspect.ratio = 0.9)
  # The highest K is the optimum K. K=1??
  
  # or use factoextra package. 
  factoextra::fviz_gap_stat(gap_stat)
# ---------------------------------------------------------------------------------------------------------------
  
  
# ========================================================================================
# The optimum k should have been identified by now.
#  Do the k-means analysis with your optimum k. 
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Perform the k-means analysis, with the optimum number you found above as the 'centers'. 
  km.results <- kmeans(x=kmeans_input, centers = 15, nstart = 25)
# Calculate the means of each variable for each cluster. 
  aggregate(kmeans_input, by=list(cluster=km.results$cluster), mean)

# Add the cluster assignment to the original data. 
  totals_cl <- cbind(totals, cluster = km.results$cluster)

# Filter for a particular cluster.
  library(dplyr)
  mysubset <- as.data.frame(totals_cl) %>% filter(cluster==13)
# Let's see if they have something in common...
  table(mysubset$UserName)
  table(mysubset$X.SampleID)
  table(mysubset$StudyDayNo)
  
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Use factoextra package for now, but I could just use ggplot2.
  library(ggplot2)
  factoextra::fviz_cluster(km.results, 
                           data = kmeans_input, 
                           ellipse = T, ellipse.alpha = 0.1,  
                           ggtheme = theme_bw(base_size = 10),
                           repel = F, labelsize = 10)
# make a panel with the 4 charts.
  library(gridExtra)
  grid.arrange(k2, k3, k4, k5, nrow = 2)
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Loop through multiple Ks
  plots <- km.results <- list()
  myKs <- c(2, 3, 15) 
  myKs <- c(2, 3, 10, 15) 
  # Perform the k-means analysis, with the optimum number you found above as the 'centers'. 
  for(i in 1:length(myKs)){
    km.results[[i]] <- kmeans(x=kmeans_input, centers = myKs[i], nstart = 25)
    plots[[i]] = factoextra::fviz_cluster(km.results[[i]],
                                          data = kmeans_input,
                                          ellipse = T, ellipse.alpha = 0.1,
                                          ggtheme = theme_bw(base_size = 10),
                                          repel = F, labelsize = 10)
  }
  # Name each element and make a combined plot.
  names(plots) <- c("K2", "K3", "K15")
  grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = 2)
  
  names(plots) <- c("K2", "K3", "K10", "K15")
  grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2)

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

