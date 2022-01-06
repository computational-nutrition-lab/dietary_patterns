# FUNCTIONS ==============================================================================

# ========================================================================================
# k-means clustering 
#  
# Version 1
# Created on 01.06.2022 by Rie Sadohara
# ========================================================================================

data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
dim(df)
# View the firt 3 rows of the data
head(df, n = 3)

set.seed(123)
?set.seed
km.res <- kmeans(df, 4, nstart = 25)
km.res
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)
head(km.res$cluster, 8)
# install.packages('factoextra')
library(factoextra)
factoextra::fviz_cluster(km.res, data = df, ellipse = T,  ggtheme = theme_bw(base_size = 10),
                         ellipse.alpha = 0.1, repel = T, labelsize = 10)

# use dietary data.
# Calculate k-means, start with 4.
  km.results <- kmeans(x = subsetted_non0var, centers = 4, nstart = 25)
# Calculate means of each variable for each cluster. 
  aggregate(subsetted_non0var, by=list(cluster=km.results$cluster), mean)
# Add the cluster assignment to the original (subsetted) data. 
  dd <- cbind(subsetted_non0var, cluster = km.results$cluster)
# Take a look
  dd$cluster
# Use factoextra package for now, but I could just use ggplot2.
  factoextra::fviz_cluster(km.results, 
                           data = subsetted_non0var, 
                           ellipse = T, ellipse.alpha = 0.1,  
                           ggtheme = theme_bw(base_size = 10),
                           repel = F, labelsize = 10)
  
  # This clusters 580 datapoints: participants x days.  Should have average of each participant? 
  
  
# ---------------------------------------------------------------------------------------------------------------
# Function to 


# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to 
# ---------------------------------------------------------------------------------------------------------------

