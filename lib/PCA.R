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
  LineScreePlot <- function(pca.data = pca_input, pca.result = scaled_pca){

    # Extract the importance of the PCs
    pca_summary <- summary(pca.result)
    
    # # Extract the Proportion of Variance
    var_explained_values <- pca_summary[["importance"]][2, ]
    
    # Create a dataframe that has the PCs and their importance (var explained by each PC)
    var_explained_df <<- data.frame(PC = seq(1:length(var_explained_values)),
                                    var_explained = var_explained_values)

    # if there are only 9 or fewer variables, plot them all; otherwise plot the first 10 PCs.
    if(length(colnames(pca.data))<10){
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
  # BiplotDots <- function(pca.result = scaled_pca, pca.data = pca_input){
  #   s <- summary(pca.result)
  #   PCs <<- pca.result[["x"]]
  #       # PCs
  #       # print(s)
  #       # print(head(PCs)[,1])
  #   ggplot(PCs, aes(x=PC1, y=PC2)) +
  #     geom_point(alpha=0.3) +
  #     theme(panel.grid.major = element_blank()) +
  #     theme(panel.grid.minor = element_blank()) +
  #     theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
  #     theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
  #     theme(aspect.ratio = 1)
  # } 
  

# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals as black dots.
  BiplotDots <- function(pca.result = scaled_pca, pca.data = pca_input, alpha=1){
    require(ggfortify) # Need ggfortify packge to use 'autoplot'.
    autoplot(object = pca.result, data = pca.data,
             loadings = T, loadings.label = T, loadings.colour = 'pink',
             loadings.label.size=3, alpha=alpha) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 1)
  }

# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals labeled and without the variables' arrows.
  BiplotLabeledwoArrows <- function(pca.result = scaled_pca, pca.data = pca_input, 
                        individuals.label = T){
    require(ggfortify)
    autoplot(object = pca.result, data = pca.data, 
             label = individuals.label, label.size = 3, shape =FALSE,  
             loadings = F, loadings.label = F, alpha=alpha) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 10, r = 0, b = 0, l = 0) ) ) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) +
      theme(aspect.ratio = 1)
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# Function to create a biplot with the individuals labeled.
  BiplotLabeled <- function(pca.result = scaled_pca, pca.data = pca_input, individuals.label = TRUE){
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
# Plot the contribution of each variable to PC1.  
  LoadingsPlot <- function(pca.result,  whichPC, positive.color="green2", negative.color="grey70", labels.aligned=c(TRUE, FALSE)){
    
    p <- pca.result[["rotation"]]
    variables <- rownames(p)
    # create a numeric object ("n.pc1") containing values which will position text underneath the bars.
    # i.e. shows the starting points for the label of the variables. 
    n.PCx <- ifelse(p[, whichPC] > 0, yes= -0.01, no= p[, whichPC]-0.01)

    # if loadings is positive, color it green, if not, color it red.
    c.PCx <- ifelse(p[, whichPC] > 0, yes=positive.color, no=negative.color)  
    
    if(labels.aligned==TRUE){
      par(mar=c(8,3,2,1)) # Set margins
      b1 <- barplot(p[, whichPC], main=paste(whichPC, "Loadings Plot"), col=c.PCx, las=2, axisnames=T)
      abline(h=0) # Add horizontal line
      
    }else if(labels.aligned==FALSE){
      # Plot again 
      par(mar=c(8,3,2,1)) # Set margins
      b1 <- barplot(p[, whichPC], main=paste(whichPC, "Loadings Plot"), col=c.PCx, las=2, axisnames=FALSE)
      abline(h=0) # Add horizontal line
      # 
      text(x=b1, y=n.PCx, labels= names(n.PCx), adj=1, srt=90, xpd= TRUE) # Add variable names
      # xpd=TRUE tells R that it can plot the text outside the plot region.
    }
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to save the variance explained by each PC in the result folder.

  SaveVarExplained <- function(pca.data=pca_input, pca.result=scaled_pca, out.fn){
    # Extract the importance of the PCs
    pca_summary <- summary(pca.result)
    
    # # Extract the Proportion of Variance
    var_explained_values <- pca_summary[["importance"]][2, ]
    
    # Create a dataframe that has the PCs and their importance (var explained by each PC)
    var_explained_df <<- data.frame(PC = seq(1:length(var_explained_values)),
                                   var_explained = var_explained_values)
    write.table(var_explained_df, out.fn, sep = "\t", row.names = F, quote = F)
  }   
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to calculate loadings of each PC to the variables and save it as a txt file
  
  SaveLoadings <- function(pca.result = scaled_pca, out.fn){
    
    p <- pca.result[["rotation"]]
    p <- as.data.frame(scaled_pca[["rotation"]])
    
    # make a variable column. 
    variables <- rownames(p)
    p$Var <- variables

    # Sort the columns so that the rownames (variable names) come first
    sortedp <- p[, c(length(colnames(p)), 1:length(colnames(p))-1)]
    
    write.table(sortedp, out.fn, sep = "\t", row.names = F, quote = F)
  }
# ---------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------
# Function to obtain PC values and save as a txt file
  SaveInputAndPCs <- function(input, pca.results, out.fn){
    
    # Define your food input file from which you derived the input for the PCA. 
    pca_input <- read.table(input, sep="\t", header=T)
    # This has food codes and food names.
    
    # extract the PCs
    PCs <- as.data.frame(pca.results[["x"]]) 
    
    # These should have the same number of rows, so their difference should be zero.  
    diff <- nrow(pca_input) - nrow(PCs)

    # Gives an error message if the input and pca.result have a different number of rows.
    if(diff != 0){
      cat("Error: The input and the PCA results should have the same number of samples.")
    }else{
      
      # Add columns
      Input_PCs <<-  cbind(pca_input, PCs)
      
      # Save as a txt file.
      write.table(Input_PCs, out.fn, sep="\t", row.names = F, quote = F)
    }
  }
  
# ---------------------------------------------------------------------------------------------------------------
  
  
 