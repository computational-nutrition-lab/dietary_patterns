# ========================================================================================
# Take a overview of ASA24 items and totals data.
# Version 1
# Created on 06/22/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Set working directory 
# ========================================================================================

# Set your working directory to the main directory.
  Session --> Set working directory --> Choose directory.

# Name your main directory for future use. 
  main_wd <- file.path(getwd())

# Import source code to run the analyses to follow.
  source("lib/specify_dir_and_check_col.R")  
  source("lib/data_overview.R")  
  # source("lib/load_clean_ASA24.R")
  # source("lib/format.file.R")

# Call color palette.
  distinct100colors <- readRDS("lib/distinct100colors.rda")

# You can come back to the main directory by:
  setwd(main_wd)

# ========================================================================================
# Load and analyze (QC-ed) ASA24 items data
# ========================================================================================

# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  

# Load your items data to be analyzed.
  items_f_s_m <- read.table("VVKAJ_2021-11-09_7963_Items_f_s_m.txt", sep="\t", header=T)
  head(items_f_s_m)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable

# View min, quantiles, mean, etc. for a variable in your dataset. 
  summary(items_f_s_m$KCAL)

# Summary statistics of all the variables
# Calculate minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe and save as a .txt file. 
  SummaryStats(inputdf = items_f_s_m, 
               outfn = "VVKAJ_2021-11-09_7963_Items_f_s_m_summ.txt")
  
# ---------------------------------------------------------------------------------------------------------------
  # Define ggplot1 themes
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
# Boxplot
# Generate a boxplot to view data distribution.
  
# Boxplot of KCAL by users.
  ggplot(items_f_s_m, aes(x=UserName, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes

# Boxplot of KCAL by gender.
  ggplot(items_f_s_m, aes(x=Gender, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes

# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scaterplot of two numeric variables: TFAT and KCAL. 
  ggplot(items_f_s_m, aes(x=TFAT, y=KCAL)) +
    geom_point() + no_grid + space_axes

# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
  cor.test(x=items_f_s_m$TFAT, y=items_f_s_m$KCAL, method="pearson")

  
# ===============================================================================================================
# Load and analyze (QC-ed) ASA24 totals data
# ===============================================================================================================
  
# Specify the directory where the data is.
  SpecifyDataDirectory(directory.name = "eg_data/VVKAJ101-105/")  
  
# Load your QC-ed totals data to be analyzed.
  tot_m_QCed <- read.table("VVKAJ_2021-11-09_7963_Tot_m_QCed.txt", sep="\t", header=T)
  
# Note that each row is a total dietary intake of each user on each day. 
  head(tot_m_QCed)

# ---------------------------------------------------------------------------------------------------------------
# Summary statistics
  
# Summary statistics of one variable
  SummaryStats(inputdf = tot_m_QCed, 
               outfn = "VVKAJ_2021-11-09_7963_Tot_m_QCed_summ.txt")
  
# View min, quantiles, mean, etc. for a variable in your dataset. 
  summary(tot_m_QCed$KCAL)
  
# ---------------------------------------------------------------------------------------------------------------
# Load ggplot2 package and define theme if you have not done so.
  library(ggplot2)
  
# Define ggpplot theme - white background, no inner grid.
  theme_set(theme_bw(base_size = 14))
  no_grid <- theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())
  space_axes <- theme(axis.title.x = element_text(margin=margin(t = 8, r = 0, b = 0, l = 0) ),
                      axis.title.y = element_text(margin=margin(t = 0, r = 10, b = 0, l = 0) ) ) 
  
# ---------------------------------------------------------------------------------------------------------------
# Boxplot
# Generate a boxplot to view data distribution.

# Boxplot of KCAL by users. This is a variation of the days, and note that
# some users may have less number of days due to the QC process or missing data. 
  ggplot(tot_m_QCed, aes(x=UserName, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes
  
# Boxplot of KCAL by gender.
  ggplot(tot_m_QCed, aes(x=Gender, y=KCAL)) +
    geom_boxplot() + no_grid + space_axes
  
# ---------------------------------------------------------------------------------------------------------------
# Scatterplot
  
# Scaterplot of two variables. 
  ggplot(tot_m_QCed, aes(x=TFAT, y=KCAL)) +
    geom_point() + no_grid + space_axes
  
# Test if the two variables are correlated.
# The output should show p-value and R correlation coefficient
  cor.test(x=tot_m_QCed$TFAT, y=tot_m_QCed$KCAL, method="pearson")

# ---------------------------------------------------------------------------------------------------------------
# Lineplot 
  
# Prepare your totals dataset for line plot - insert NA to missing combinations of UserName and RecallNo (day), 
# and separate rows into NA's and no NAs. 
  PrepLinePlot(inputdf= tot_m_QCed, day="RecallNo", username="UserName", 
               all.fn=           "VVKAJ_2021-11-09_7963_Tot_m_QCed_wNA.txt",
               full.days.only.fn="VVKAJ_2021-11-09_7963_Tot_m_QCed_fullonly.txt",
               partial.days.only.fn="VVKAJ_2021-11-09_7963_Tot_m_QCed_partialonly.txt")
  
# Load the files.
  tot_m_QCed_w_NA <- read.table("VVKAJ_2021-11-09_7963_Tot_m_QCed_wNA.txt", sep="\t", header=T)
  tot_m_QCed_fullonly <- read.table("VVKAJ_2021-11-09_7963_Tot_m_QCed_fullonly.txt", sep="\t", header=T)
  tot_m_QCed_partialonly <- read.table("VVKAJ_2021-11-09_7963_Tot_m_QCed_partialonly.txt", sep="\t", header=T)

# Make RecallNo (day) as a factor.
  tot_m_QCed$RecallNo <- as.factor(tot_m_QCed$RecallNo)
  tot_m_QCed_w_NA$RecallNo <- as.factor(tot_m_QCed_w_NA$RecallNo)
  tot_m_QCed_fullonly$RecallNo <- as.factor(tot_m_QCed_fullonly$RecallNo)
  tot_m_QCed_partialonly$RecallNo <- as.factor(tot_m_QCed_partialonly$RecallNo)
  
  
# Plot points and lines separately.  Specify your "y" twice.
  ggplot() +
    geom_point(tot_m_QCed,         mapping = aes(x=RecallNo, y=TFAT, group=UserName, color=UserName)) +
    geom_line(tot_m_QCed_fullonly, mapping = aes(x=RecallNo, y=TFAT, group=UserName, color=UserName), 
              linetype="dashed") + no_grid


  
