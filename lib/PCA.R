# FUNCTIONS ==============================================================================

# ========================================================================================
# Principal component analysis (PCA)  
#  
# Version 1
# Created on 12.17.2021 by Rie Sadohara
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Function to subset specific data from the totals.

  SubsetColumns <- function(data, start.col, end.col){
    # Column Variables of "totals" dataframe.
    colvars <- names(data)
    # Get the first ID
    start.col <- match(start.col, colvars)
    # Get the second ID
    end.col <- match(end.col, colvars)
    # Subset range
    subsetted <<- data[, start.col:end.col]
    # Print what was loaded.
    cat("'subsetted' contains the following", length(colnames(subsetted)), "columns.", "\n")
    print(colnames(subsetted))
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Keep only the columns with non-zero variance in order to perform PCA.
  KeepNonZeroVarColumns <- function(data = subsetted){
    subsetted_non0var <<- subsetted[, which(apply(subsetted, 2, var) != 0)] 
      # Print which column(s) were removed.
      if(ncol(subsetted) == ncol(subsetted_non0var)){
        cat("No columns were removed.", "\n")
      }
      if(ncol(subsetted) != ncol(subsetted_non0var)){
        cat("The following column(s) in ", deparse(substitute(data)), " had zero variance and were removed.", "\n")
        print(which(apply(subsetted, 2, var) == 0))
      }
    }
  
   
# ---------------------------------------------------------------------------------------------------------------
# Function to create a scree plot.

  LineScreePlot <- function(pca.result = scaled_pca){
    # Calculate the variance explained for each PC.
    var_explained_df <- data.frame(PC = rep(1:length(colnames(subsetted_non0var))),
                                   var_explained = (pca.result$sdev)^2/sum((pca.result$sdev)^2))
    # Subset the first 10 PCs
    first10PCs <- var_explained_df[1:10, ]
    # Create a scree plot.
    require(ggplot2)
    ggplot(first10PCs, aes(x = PC, y = var_explained*100)) + 
      geom_line() + 
      geom_point() +
      scale_x_continuous(breaks = 1:10) +
      labs(x = "Number of PCs",
           y = "Variance explained by PCs (%)") +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 0.9)
  }
# ---------------------------------------------------------------------------------------------------------------

  
# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals as black dots.
  BiplotDots <- function(pca.result = scaled_pca, pca.data = subsetted_non0var){
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
  BiplotLabeled <- function(pca.result = scaled_pca, pca.data = subsetted_non0var, individuals.label = TRUE){
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
# Function to calculate loadings of each PC to the variables and save it as a csv file
#  in the results folder.
  
  SaveLoadings <- function(pca.result = scaled_pca, name){
    # Calculate the loadings.  sweep function is similar to apply.
    fc.l <<- sweep(pca.result$rotation, MARGIN = 2, pca.result$sdev, FUN = "*") 
    # Convert the matrix to a dataframe. 
    fc.l.df <<- as.data.frame(fc.l)
    # Save it as a csv file.
    write.csv(fc.l.df, paste("results/", name, ".csv", sep = ""))
  }
# ---------------------------------------------------------------------------------------------------------------
  
 
  
  
  
  
  
  
  
  
  
  
  

