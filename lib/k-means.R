# FUNCTIONS ==============================================================================

# ========================================================================================
# k-means clustering 
#  
# Version 1
# Created on 01.06.2022 by Rie Sadohara
# ========================================================================================


# ========================================================================================
# Find the ideal k
#  Modified code from https://uc-r.github.io/kmeans_clustering
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Function to do the Elbow method.
  ElbowMethod <- function(k.values=1:15){
    set.seed(123)
    
    # Define a function to compute total within-cluster sum of square 
    wss <- function(k, data) {
      kmeans(data, k, nstart = 25)$tot.withinss
    }

    # extract wss for 2-15 clusters
    wsstable <- data.frame(K=k.values, WithinClusterSS=NA)
    for(i in k.values){
      wssvalue <- wss(k.values[i], data = kmeans_input)
      wsstable[i, 2] <- wssvalue
    }
    
    # create a wss value plot 
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
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find the ideal k: the Silhouette method
 
 SilhouetteMethod <- function(k.values = 2:15){
   # need the cluster package
     library(cluster)

   # Define avg_sil function first.
     avg_sil <- function(number){
       km.res <- kmeans(kmeans_input, centers=number, nstart=25)
       ss <<- silhouette(km.res$cluster, dist(kmeans_input))
       mean(ss[, 3])
     }
     
   # Create a dataframe with k values.
     siltable3  <- data.frame(K=k.values)
   # Apply avg_sil function to each of the K and save results as a vector. 
     resultvec <- apply(siltable3, MARGIN=1, FUN = avg_sil)
   # Save the result vector as a new column of siltable.
     siltable3$Avg_Silhouette <- resultvec
  
   # Plot K and the Silhouette values.    
     ggplot(siltable3, aes(x = K, y = Avg_Silhouette)) + 
       geom_line() + 
       geom_point() +
       scale_x_continuous(breaks = siltable3$K) +
       labs(x = "Number of clusters K",
            y = "Average Silhouettes") +
       theme(panel.grid.major = element_blank()) +
       theme(panel.grid.minor = element_blank()) +
       theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
       theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
       theme(aspect.ratio = 0.9)
     # The K with the max average Silhouette value is the ideal K. 
   } 

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Find the ideal k: Gap statistic method

  GapMethod <- function(k.values=1:15){

    # Calculate the gap statistic.
    library(cluster)
    gap_stat <- clusGap(kmeans_input, FUN = kmeans, nstart = 25,
                        K.max = k.values[length(k.values)], 
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
    # The highest K is the optimum K. 
  }

  # Or use the factoextra package.
  
  FactoextraGapMethod <- function(k.values = 1:15){
    
    library(cluster)
    # Calculate Gap statistics first.
    gap_stat <- clusGap(kmeans_input, FUN = kmeans, nstart = 25,
                        K.max = k.values[length(k.values)], 
                        B=50) # B is the number of bootstrapping

    # Print the result.
    print(gap_stat, method = "firstmax")
    # Visualize. The best K is marked with a dotted line. 
    factoextra::fviz_gap_stat(gap_stat)
  }
# ---------------------------------------------------------------------------------------------------------------
  
  
# ========================================================================================
# The optimum k should have been identified by now.
#  Do the k-means analysis with your optimum k. 
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Perform the k-means analysis, with the optimum number you found above as the 'centers'. 
  km.results <- kmeans(x=kmeans_input, centers = 15, nstart = 25)
# Calculate the means of each variable for each cluster. 
  aggregate(kmeans_input, by=list(cluster = km.results$cluster), mean)

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
  plots <- list()
  km.results <- list()
  myKs <- c(2, 3, 15) 
  myKs <- c(2, 3, 6, 15) 
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
  gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = 2)
  
  names(plots) <- c("K2", "K3", "K6", "K15")
  gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2)

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Check the dependency packages of an R package
  pack <- available.packages()
  pack["ggplot2","Depends"]
  pack["factoextra","Depends"]
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

