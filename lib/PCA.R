# FUNCTIONS ==============================================================================

# ========================================================================================
# Principal Component Analysis (PCA)    
# Version 1
# Created on 12.17.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# PCA
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Function to create a scree plot.
# If there are 10 or less PCs in total, plot all, and else plot the first 10 PCs. 
  LineScreePlot <- function(pca.data = my_input, pca.result = scaled_pca){
    # Calculate the variance explained for each PC.
    var_explained_df <<- data.frame(PC = rep(1:length(colnames(pca.data))),
                                   var_explained = (pca.result$sdev)^2/sum((pca.result$sdev)^2))
    if(length(colnames(pca.data))<9){
      myPCs <<- var_explained_df
    }else{
      myPCs <<- var_explained_df[1:10, ]    # Subset the first 10 PCs
    }      
    # Create a scree plot.
    require(ggplot2)
    ggplot(myPCs, aes(x = PC, y = var_explained*100)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous(breaks = 1:nrow(myPCs)) +
      labs(x = "Number of PCs",
           y = "Variance explained by PCs (%)") +
      theme_bw(base_size = 13) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 0.9)
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals as black dots.
  BiplotDots <- function(pca.result = scaled_pca, pca.data = my_input){
    require(ggfortify) # Need ggfortify packge to use 'autoplot'.
    autoplot(object = pca.result, data = pca.data,
             loadings = T, loadings.label = T, loadings.colour = 'pink',
             loadings.label.size=3) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 1)
  } 

# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals labeled.
  BiplotLabeled <- function(pca.result = scaled_pca, pca.data = my_input, individuals.label = TRUE){
    require(ggfortify)
    autoplot(object = pca.result, data = pca.data, 
             label = individuals.label, label.size = 3, shape =FALSE,  
             loadings = T, loadings.label = T, loadings.colour = 'pink',
             loadings.label.size=3) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 1)
  } 
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to save the variance explained by each PC in the result folder.

  SaveVarExplained <- function(x = var_explained_df, out.fn){
    write.table(x, out.fn, sep = "\t", row.names = F)
  }   
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Function to calculate loadings of each PC to the variables and save it as a txt file
  
  SaveLoadings <- function(pca.result = scaled_pca, out.fn){
    # Calculate the loadings.  sweep function is similar to apply.
    fc.l <- sweep(pca.result$rotation, MARGIN = 2, pca.result$sdev, FUN = "*") 
    # Convert the matrix to a dataframe. 
    fc.l.df <- as.data.frame(fc.l)
    # Save it as a txt file.
    write.table(fc.l.df, out.fn, sep = "\t", row.names = F)
  }
# ---------------------------------------------------------------------------------------------------------------
# Function to obtain PC values and save as a txt file
  SaveInputAndPCs <- function(pca.result, out.fn){
    
    PCs <- as.data.frame(pca.result[["x"]])
    
    write.table(PCs, out.fn, sep="\t", row.names = F)
  }
# ---------------------------------------------------------------------------------------------------------------
  
  
 