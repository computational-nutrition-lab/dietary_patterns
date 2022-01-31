# FUNCTIONS ==============================================================================

# ========================================================================================
# Analyze the kcal from carbohydrate, protein, and fat in the totals file of  
# ASA24 output (data) 
# Version 1
# Created on 12.16.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Calculation for plotting 
# ========================================================================================
# ---------------------------------------------------------------------------------------------------------------
# Calculate the mean kcal from carb/protein/fat per participant, no other factors.
# Since this is an average of all the food items they reported, SD doesn't really make sense. 
# (because SD will be the variability of kcal among the food items one person reported.)
  CalcKcal_user <- function(){
    # Get means (g) for each user and save as a separate dataframe. 
    PROTsum <- aggregate(totals$PROT, by = list(totals$UserName), FUN = sum)
    TFATsum <- aggregate(totals$TFAT, by = list(totals$UserName), FUN = sum)
    CARBsum <- aggregate(totals$CARB, by = list(totals$UserName), FUN = sum)
    colnames(PROTsum) <- c("UserName", "PROT_sum_g")  
    colnames(TFATsum) <- c("UserName", "TFAT_sum_g")  
    colnames(CARBsum) <- c("UserName", "CARB_sum_g")  
    
    # Calculate the calories for each macronutrient. 
    PROTsum$PROT_sum_kcal <- PROTsum$PROT_sum * 4
    TFATsum$TFAT_sum_kcal <- TFATsum$TFAT_sum * 9
    CARBsum$CARB_sum_kcal <- CARBsum$CARB_sum * 4
    
    # Combine the three tables
    temp1 <- merge(PROTsum, TFATsum, all = T)
    macronutr.sum <- merge(temp1, CARBsum, all = T)
    
    # Add a column of mean total calories per item/user. 
    macronutr.sum$total_kcal <- macronutr.sum$PROT_sum_kcal +  
      macronutr.sum$TFAT_sum_kcal +
      macronutr.sum$CARB_sum_kcal 
    
    # Add a column of percentage of kcal/macronutrient
    macronutr.sum$PROT_pk <- macronutr.sum$PROT_sum_kcal / macronutr.sum$total_kcal *100
    macronutr.sum$TFAT_pk <- macronutr.sum$TFAT_sum_kcal / macronutr.sum$total_kcal *100
    macronutr.sum$CARB_pk <- macronutr.sum$CARB_sum_kcal / macronutr.sum$total_kcal *100
    
    # Modify the dataframe structure for plotting.
    mean.p <- macronutr.sum[, c("UserName",  "PROT_pk")]
    mean.t <- macronutr.sum[, c("UserName",  "TFAT_pk")]
    mean.c <- macronutr.sum[, c("UserName",  "CARB_pk")]
    
    # Add a column of macronutrients
    mean.p$macronutrient <- "PROT"
    mean.t$macronutrient <- "TFAT"
    mean.c$macronutrient <- "CARB"
    
    # Change XXXX_pk to "value"
    colnames(mean.p)[2] <- colnames(mean.t)[2] <- colnames(mean.c)[2] <- "value"
    
    # Bind the 3 datasets
    bound <- rbind(mean.p, mean.t, mean.c)
    macronutr.mean.l <- bound[, c(1,3,2)] # sort columns
    
    # Check the dimention of the macronutr.mean.l (for programmers)
    # dim(macronutr.mean.l)  # l means a long table.
  }
# ---------------------------------------------------------------------------------------------------------------

# ========================================================================================
# Plot stacked barcharts 
# ========================================================================================

# ---------------------------------------------------------------------------------------------------------------
# Plot the mean kcal from carbs, protein, and fat by participant (normalized)
  NormalizedPercentKcal <- function(){
    
    cat("Showing a normalized stacked barchart.", "\n")
    
    library(ggplot2)
    bwoe <<- ggplot(macronutr.mean.l,
                    aes(x = factor(UserName), y = value, fill = macronutrient)) + 
      geom_bar(position = "fill", stat = "identity", colour = "black", width = 0.7) +
      theme_bw(base_size = 10) +
      # scale_fill_manual(values = my15colors ) +
      # labels=c("Protein", "Fat", "Carbohydrates")) +
      labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "Macronutrients") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
      theme(aspect.ratio = 0.4)
    bwoe
  }
# ---------------------------------------------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------------------------------------------
# If there are factor(s) that can group participants, SD will be meaningful.
# Plot the mean kcal from carbs, protein, and fat by participant (non-normalized)
  NonNormalizedPercentKcal <- function(show.sd = TRUE){
    
    if(show.sd == TRUE){ # default
    cat("Showing a non-normalized stacked barchart with SD as error bars.", "\n")
    
    # Rearrange the macronutr.mean table
    # Merge mean and SD for PROT
    temp3 <<- merge(PROTmeans, PROTsd, all = T)
    colnames(temp3) <<- c("UserName", "Means", "SD")
    temp3$Macronutrient <<- "PROT"
    
    # Merge mean and SD for CARB
    temp4 <<- merge(CARBmeans, CARBsd, all = T)
    colnames(temp4) <<- c("UserName", "Means", "SD")
    temp4$Macronutrient <<- "CARB"
    
    # Merge mean and SD for TFAT
    temp5 <<- merge(TFATmeans, TFATsd, all = T)
    colnames(temp5) <<- c("UserName", "Means", "SD")
    temp5$Macronutrient <<- "TFAT"
    
    # Bind temp3-5
    pfc.mean.sd <<- rbind(temp3, temp4, temp5)
    pfc.mean.sd <<- pfc.mean.sd[, c(1,4,2,3)] # Sort columns
    
    # Calculate the cumulative value for creating a stacked chart with error bars.
    library(dplyr)
    pfc.mean.sd2 <<- pfc.mean.sd %>% 
      arrange(desc(Macronutrient)) %>% group_by(UserName) %>% mutate(cumsum_Means=cumsum(Means))  
    
    # Stacked barchart with SD error bars.
    library(ggplot2)
    bwe <<- ggplot(pfc.mean.sd2, aes(x = UserName, y = Means, fill = Macronutrient)) +
      geom_col(color = "black", width = 0.6, position = position_stack(vjust=1)) +
      geom_errorbar(inherit.aes = FALSE, 
                    aes(x = UserName, ymin = cumsum_Means, ymax = cumsum_Means + SD), 
                    width = 0.2) +
      theme_bw(base_size = 10) +
      # scale_fill_manual(values = my15colors ) +
      # scale_fill_manual(values=c("lightgoldenrod2", "steelblue", "palevioletred2"))+
      #labels=c("Carbohydrates", "Protein", "Fat")) +
      labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "variable") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
      theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
      theme(aspect.ratio = 0.4)
    bwe
    }
    
    else if(show.sd == FALSE){
      
      cat("Showing a non-normalized stacked barchart without error bars.", "\n")
      
      # Not normalized
      library(ggplot2)
      nonnormal <<- ggplot(macronutr.mean.l, 
                          aes(x = UserName, y = value, fill = macronutrient)) + 
        geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
        theme_bw(base_size = 10) +
        # scale_fill_manual(values = my15colors, # ) +
        # labels=c( "Carbohydrates", "Protein", "Total fat")) +
        labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "Macronutrients") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
        theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
        theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        theme(aspect.ratio = 0.4)
      nonnormal
    }  
  
  }
# --------------------------------------------------------------------------------------------------------------- 

