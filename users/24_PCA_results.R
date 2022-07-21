# ========================================================================================
# Look at the PCA results in detail
# Created on 07/15/2022 by Rie Sadohara
# Version 1
# ========================================================================================
# 
# Define ggplot2 themes
  library(ggplot2)

# Theme black and white, with the base font size 14: change if necessary.
  theme_set(theme_bw(base_size = 14))

# No gridlines inside charts
  no_grid <- theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())

# Insert some space between axes and axes labels. 
  space_axes <- theme(axis.title.x = element_text(margin=margin(t = 8, r = 0, b = 0, l = 0) ),
                      axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) 

    
# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ/Nut_asis_PCA")

# Load the PCA result
  pcares <- read.table("VVKAJ_Nut_asis_PCs.txt", sep="\t", header=T)
  head(pcares,1)

# Load the variance explained by each PC.
  PC_var_exp <- read.table("VVKAJ_Nut_asis_PC_var_explained.txt", sep="\t", header=T)
  head(PC_var_exp)
  colnames(PC_var_exp)

  
# ---------------------------------------------------------------------------------------------------------------
# Generate a biplot with the users colored by their diets.
  ggplot(pcares, aes(x=PC1, y=PC2, color=Diet, fill=Diet)) + 
    geom_point(size=3) +
    no_grid + space_axes +
    scale_fill_manual( values = distinct100colors) +
    scale_color_manual(values = distinct100colors) +
    scale_x_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of X.
    scale_y_continuous(expand = expansion(mult=c(0.1, 0.1))) + # give some space on the lower and the upper limits of Y.
    labs(x = paste("PC1 (", round(PC_var_exp[1,2]*100, 2), "%)", sep=""),  
         y = paste("PC2 (", round(PC_var_exp[2,2]*100, 2), "%)", sep=""))
  

# ---------------------------------------------------------------------------------------------------------------
  

  
  
  