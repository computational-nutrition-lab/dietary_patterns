# For USERS ==============================================================================

# ========================================================================================
# Visualize the mean values of %kcal from carbohydrate, protein, and total fat.
# Version 1
# Created on 12.16.2021 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Import data from your data directory 
# ========================================================================================
# 
# Folder structure 
# 
#                          |----- eg_data 
#                          |
#                          |----- lib
#                          |
#                          |----- users
#  Main -------------------|
#  (dietary_patterns)      |----- 
#                          |
#                          |----- ...
#

# Set your working directory as to the main directory.
# Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")
  source("lib/percent_kcal.R")

# Load example totals data =============================================================== 
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/dietstudy/")
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")
  
# Load the totals.csv
  totals <- read.table("Totals_to_use.txt",  sep = "\t", header = T)
  totals <- read.table("VVKAJ_2021-11-09_7963_Tot_m_QCed.txt",  sep = "\t", header = T)

# Come back to the main directory
  setwd(main_wd)
  
# ========================================================================================  
# Plot the mean %protein/fat/carb of each participant
# ========================================================================================

# # Calculate the mean kcal from carb/protein/fat per participant
#   CalcKcal_user() 
# 
# # Show normalized stacked barchart per participant
#   NormalizedPercentKcal()
# 
# # show a stacked barchart per participant with standard deviations as error bars.
#   NonNormalizedPercentKcal()
#   NonNormalizedPercentKcal(show.sd = TRUE)
#  
# # show a stacked barchart per participant without error bars.
#   NonNormalizedPercentKcal(show.sd = FALSE)


##################### update on July 1, 2022 from below ##################################
# create theme and simplify the ggplot2 code. then, done!!!!! 


# Calculate the mean and SD of CARB, PROT, and TFAT.
 CPTgramsPerUser(inputfn= totals, user.name = "UserName", recall.no = "RecallNo",
                 outfn='VVKAJ_2021-11-09_7963_Tot_m_QCed_CPT_g.txt')
# Not used in the visualization below, but you may want to take a look at it.

 
# Calculate the mean % of energy intake (kcal) and SD of CARB, PROT, and TFAT.
 CPTpctKcalPerUser(inputfn=totals, user.name='UserName', recall.no='RecallNo', 
                   outfn="VVKAJ_2021-11-09_7963_Tot_m_QCed_CPT_kcal.txt")
 
# Load the %kcal 
 CPT_kcal <- read.table("VVKAJ_2021-11-09_7963_Tot_m_QCed_CPT_kcal.txt", sep="\t", header=T)
 CPT_kcal

# --------------------------------------------------------------------------------------------------------------
 
# Plot without SD first...
  library(ggplot2)
  ggplot(CPT_kcal, aes(x = UserName, y = mean, fill = macronutrient)) + 
    geom_bar(position = "stack", stat = "identity", colour = "black", width = 0.7) +
    theme_bw(base_size = 10) +
    # scale_fill_manual(values = my15colors, # ) +
    # labels=c( "Carbohydrates", "Protein", "Total fat")) +
    labs(x = element_blank(), y = "Percentages of total kcal intake", fill = "Macronutrients") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
    theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  
 
# --------------------------------------------------------------------------------------------------------------
 # Plot "dodge"-type of barchart 
  # Different bars for carb, prot, and tfat. NOT STACKED.
  ggplot(CPT_kcal, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
    geom_bar(stat = "identity", position = "dodge", color="black")  +
    geom_errorbar(aes(ymin= mean, ymax= mean+sd), position = position_dodge(0.9), width = 0.25,
                  color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
    theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  
  # # stacked bars of carb, prot, and tfat.
  # ggplot(CPT_kcal, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
  #   geom_bar(stat = "identity", position = "stack")  +
  #   geom_errorbar(aes(ymin= mean-sd, ymax= mean+sd), position = position_dodge(0.9), width = 0.25) +
  #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #   theme(axis.title.x = element_text(margin=margin(t = 5, r = 0, b = 0, l = 0))) +
  #   theme(axis.title.y = element_text(margin=margin(t = 0, r = 5, b = 0, l = 0))) + 
  #   theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1)) 
  
  # # practice with just one user - VVKAJ102
  # user1 <- subset(CPT_kcal, UserName=="VVKAJ103")
  # 
  # CARBmeanval <- subset(user1, macronutrient=="CARB_kcal_pct")[, "mean"]
  # PROTmeanval <- subset(user1, macronutrient=="PROT_kcal_pct")[, "mean"]
  # TFATmeanval <- subset(user1, macronutrient=="TFAT_kcal_pct")[, "mean"]
  # CARBsdval <- subset(user1, macronutrient=="CARB_kcal_pct")[, "sd"]
  # PROTsdval <- subset(user1, macronutrient=="PROT_kcal_pct")[, "sd"]
  # TFATsdval <- subset(user1, macronutrient=="TFAT_kcal_pct")[, "sd"]
  # 
  # # sd values for stacked barchart. 
  # user1$sd_base    <- c(TFATmeanval+PROTmeanval,  # carb, on top of the stacked barchart.
  #                       TFATmeanval,              # prot, in the middle.
  #                       0)                          # tfat, on the bottom.
  # user1$sd_stacked <-  c(CARBsdval+PROTmeanval+TFATmeanval,      # carb, on top of the stacked barchart.
  #                        PROTsdval+TFATmeanval,                  # prot, in the middle.
  #                        TFATsdval)                            # tfat, on the bottom.
  
  # # stacked bars of carb, prot, and tfat.
  # ggplot(user1, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
  #   geom_bar(stat = "identity", position = "stack")  +
  #   geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.25 , color=c('black', "blue", "red")) 
  # 
  # user1and2 <- rbind(user12, user1)
  # 
  # ggplot(user1and2, aes(x = factor(UserName), y = mean, fill = macronutrient, colour = macronutrient)) + 
  #   geom_bar(stat = "identity", position = "stack")  +
  #   geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.25, color="grey10") 
  # 
  # # Create add these sd_stacked and sd_base for all the Users, and bind rows..
  
  
# --------------------------------------------------------------------------------------------------------------
  # Using CPT_kcal, create a stacked barchart.  
  CPT_kcal
  
  # Create a vector that contains all the users (individuals). 
  individuals <- unique(CPT_kcal$UserName)

  # Generate a dataframe to save data.
  CPT_kcal_forstacked <- data.frame(matrix(NA, nrow=length(individuals)*3, ncol=7)) 

  # Specify its colnames.
  colnames(CPT_kcal_forstacked) <- c("UserName", "macronutrient", "n", "mean", "sd", "sd_base", "sd_stacked")
  
  # Calculate sd_base and sd_forstacked for stacked barchart.
  CalcStackedSD(input.df = CPT_kcal, out.fn = "CPT_kcal_forstacked.txt")
  
    # Load the saved file that has SD for stacked barchart.
  CPT_kcal_forstacked_read <- read.table("CPT_kcal_forstacked.txt", sep="\t", header=T)
  CPT_kcal_forstacked_read
  
  # Stacked barchart with SD as error bars.
  ggplot(CPT_kcal_forstacked_read, aes(x = UserName, y = mean, fill=macronutrient, colour=macronutrient)) + 
    geom_bar(stat = "identity", position = "stack", colour = "black")  +
    geom_errorbar(aes(ymin= mean+sd_base, ymax= mean+sd_stacked), width = 0.15, color="grey10") 
  
 
  #
  
  
  
