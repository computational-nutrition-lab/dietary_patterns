# ===============================================================================================================
# Look at the fasting glucose, and group individuals if possible. 
# Then select only for males in their 60s plus.
# Version 1
# Created on 10/14/2022 by Rie Sadohara
# ===============================================================================================================

# ===============================================================================================================
# Load NHANES15-16totals with demographic data
# ===============================================================================================================
# Load necessary packages.
library(SASxport)

# Set your working directory to the main directory.
Session --> Set working directory --> Choose directory.
setwd("~/GitHub/dietarry_patterns")

# Name your main directory for future use. 
main_wd <- file.path(getwd())  

# Load necessary functions.
source("lib/specify_data_dir.R")
source("lib/load_clean_NHANES.R")
source("lib/prep_data_for_clustering.R")
source("lib/ggplot2themes.R") 

# Load the distinct 100 colors for use.   
distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")

# ---------------------------------------------------------------------------------------------------------------
# Specify the directory where the data is.
SpecifyDataDirectory(directory.name = "eg_data/NHANES")  

# # Load the QC-ed total (with food categories), filtered for KCAL, PROT, TFAT, VC. 4207 people.
#   QCtotals_d <- read.table("Total_D12_FC_QC_mean_QC_d.txt", sep="\t", header=T) 
# 
# # Check the number of participants in the QCtotals - should be 4,207 people.   
#   length(unique(QCtotals_d$SEQN))
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Load the blood glucose data and see.
#   glu <- read.xport("Raw_data/GLU_I.XPT")
# 
# # glu has LBXGLU - Fasting Glucose (mg/dL). 
#   head(glu)
#   
# # Count the number of rows with no missing data.
# # 2972 individuals have glucose data.
#   sum(complete.cases(glu))
#   
# # Take out only the rows with no missing data in LBXGLU.
#   glu_comp <- glu[!is.na(glu$LBXGLU), ]
# 
# # Take a quick look at the distribution of LBXGLU.
#   hist(glu_comp$LBXGLU)
#   
# # Use default of merge to only keep SEQNs found in both datasets.
#   QCtotal_d_glu <- merge(x=QCtotals_d, y=glu_comp, by="SEQN")
#   
# # Check the dimension of QCtotal_d_glu - should be 1,943 rows.
#   dim(QCtotal_d_glu)      
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Load the body measure data.
#   bodymea <- read.xport("Raw_data/BMX_I.XPT")
#   
# # Explanation of variables can be found here: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.htm
# # Relevant variables here include:
#   # BMDSTATS - Body Measures Component Status Code: 1	== Complete data for age group. 
#   #            2 ==	Partial: Only height and weight obtained
#   # BMXHT - Standing Height (cm) 
#   # BMIHT - Standing Height Comment
#   # BMXBMI - Body Mass Index (kg/m**2)
#   # BMXWAIST - Waist Circumference (cm)
#   
# # Add body measure to QCtotal_d_glu
#   QCtotal_d_glu_body <- merge(x=QCtotal_d_glu, y=bodymea, by="SEQN")
# 
# # ---------------------------------------------------------------------------------------------------------------
# # Load the metadata of people, which is in Total Day 1.
#   metadata_raw <- read.xport("E:/MSU OneDrive 20210829/UMinn/20_NHANES/2015-16/Data/DR1TOT_I.XPT")
#   
# # Total Day 1 has "dietary data for day 1" and "metadata", but we only need the metadata; thus, take out 
# # only the metadata columns (variable) and exclude the day 1 data.
# # Column names' descriptions can be found here: https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.htm#DRQSPREP
#   
# # First, specify the first and the last column names to select. 
# # Look for the column number that matches the first and last variable specified.
#   sta_col_num_a <- match("DBQ095Z"  , names(metadata_raw))  # Salt-related questions
#   end_col_num_a <- match("DRQSPREP" , names(metadata_raw)) 
#   sta_col_num_b <- match("DRQSDIET" , names(metadata_raw))  # Diet-related questions 
#   end_col_num_b <- match("DRQSDT91" , names(metadata_raw)) 
#   sta_col_num_c <- match("DRD340"   , names(metadata_raw))  # Fish-related questions
#   end_col_num_c <- match("DRD370V"  , names(metadata_raw)) 
#   
# # Only select the metadata variables and SEQN, which is in column 1.
#   metadata_only <- metadata_raw[, c(1,    
#                                     sta_col_num_a:end_col_num_a, 
#                                     sta_col_num_b:end_col_num_b, 
#                                     sta_col_num_c:end_col_num_c 
#                                     )]
#   
# # Check that this has only the SEQN and metadata columns.
#   head(metadata_only, 1)
#     
# # Add meatadata to QCtotal_d_glu_body
#   QCtotal_d_glu_body_meta <- merge(x=QCtotal_d_glu_body, y=metadata_only, by="SEQN")
# 
# ### In summary, individuals were kept who were in the QCtotal_d AND also had glucose test measurements. 
#   # bodymeasures and metadata were added in a way that only individuals present in all the datasets
#   # will be kept. 1943 individuals were kept.
# 
# # ===============================================================================================================
# # Use QCtotal_d_glu_body_meta dataframe for further analysis. 
# # ===============================================================================================================
#   
# # Add index according to their glucose level: Normal, Prediabetic, and Diabetic. 
#   # Norm: 99 mg/dL or lower 
#   # Pred: 100 to 125 mg/dL 
#   # Diab: 126 mg/dL or higher 
# 
# # Create an empty column to insert glucose level index.
#   QCtotal_d_glu_body_meta$GLU_index <- NA
#   
# # Add glucose level index.
#   for(i in 1: nrow(QCtotal_d_glu_body_meta)){
#     if(     QCtotal_d_glu_body_meta$LBXGLU[i] < 100){ QCtotal_d_glu_body_meta$GLU_index[i] <- "Normal" }
#     else if(QCtotal_d_glu_body_meta$LBXGLU[i] < 126){ QCtotal_d_glu_body_meta$GLU_index[i] <- "Prediabetic" }
#     else{                                             QCtotal_d_glu_body_meta$GLU_index[i] <- "Diabetic" }
#   }
#   
# # Check the first 10 rows of glucose and GLU_index columns in QCtotal_d_glu_body_meta. 
#   QCtotal_d_glu_body_meta[1:10, c("LBXGLU", "GLU_index")]
#   
# # Look at the frequency of GLU_index.
#   table(QCtotal_d_glu_body_meta$GLU_index)
#   
# # Save the dataset as a .txt file. 
#   write.table(QCtotal_d_glu_body_meta, file="Laboratory_data/QCtotal_d_glu_body_meta.txt", 
#               sep= "\t", row.names=F, quote= F)
#   
# ---------------------------------------------------------------------------------------------------------------
# Load the data of those to be used in the diabetes status analysis. 
glu <- read.delim( file="Laboratory_data/QCtotal_d_glu_body_meta.txt", sep= "\t", header= T )

# Make GLU_index as a factor for plotting.
glu$GLU_index <- factor(glu$GLU_index, levels = c("Normal", "Prediabetic", "Diabetic"))

# Exclude those who are following special diets.   
# Look at the number of individuals who are following any specific diet (DRQSDIET==1).
table(glu$DRQSDIET)

# DRQSDIET==1 is following a special diet, so select only rows with DRQSDIET==2. 
glu_2 <- subset(glu, DRQSDIET == 2)

# How many people remained? -- 1625 remained.
table(glu_2$DRQSDIET)

# Check the sample size of each category.
table(glu_2$GLU_index)

# Normal Prediabetic    Diabetic 
# 684         730         211 

# # ---------------------------------------------------------------------------------------------------------------
# # Look at the BMI frequency of each group.   
# # The columnname for BMI is BMXBMI.
# 
# # Check the summary data - this will also show the number of missing data if any.
#   summary(glu_2$BMXBMI)
# 
# # 14 are missing BMI and has NA's. You can also see that by counting the number of NAs
# # in specified rows.
#   colSums(is.na(glu_2[, c("SEQN", "BMXBMI")]))
#   
# # Create a density plot of BMI by GLU_index type. 
#   BMIfreq <- ggplot(data=glu_2, aes(x=BMXBMI, group=GLU_index, fill=GLU_index)) +
#     geom_density(adjust=1.5, alpha=.4) + space_axes + no_grid +
#     scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") ) +
#     labs(x="BMI", y="Density") 
#   BMIfreq
#   
# # If there are missing data, it will give a Warning message:
# # "Removed 14 rows containing non-finite values (stat_density)." 
# 
# # Save the chart as .pdf. n = 1625 - 14 missing = 1611. 
#   ggsave("Laboratory_data/QCtotal_d_glu_body_meta_demo_n1611_BMI_by_GLU_index.pdf", 
#          BMIfreq, device="pdf", width=5.3, height=4.5)
#   
# #### The diabetic population had higher BMI than the prediabetic, and the lowest BMI was
#   # the normal population.
#   
# # ---------------------------------------------------------------------------------------------------------------
# # The columnname for bodyweight is BMXWT 
#   
# # Check the summary data - this will show the number of missing data if any.
#   summary(glu_2$BMXWT)
#   colSums(is.na(glu_2[, c("SEQN", "BMXWT")]))
# # 12 are missing body weight and has NA's.
# 
# # Show histogram of body weight.
#   hist(glu_2$BMXWT)
#   
# # Create a density plot of body weight by GLU_index type. 
#   weightfreq <- ggplot(data=glu_2, aes(x=BMXWT, group=GLU_index, fill=GLU_index)) +
#     geom_density(adjust=1.5, alpha=.4) + space_axes + no_grid +
#     scale_fill_manual(values= c("steelblue3", "yellow", "hotpink") 
#     ) +
#     labs(x="Body weight (kg)", y="Density") 
#   weightfreq
#   
# # Save the chart as .pdf. n = 1625 - 12 missing = 1613. 
#   ggsave("Laboratory_data/QCtotal_d_glu_body_meta_demo_n1613_weight_by_GLU_index.pdf", 
#          weightfreq, device="pdf", width=5.3, height=4.5)
# 
#   
# # ---------------------------------------------------------------------------------------------------------------
# # Look at the KCAL frequency of each group.   
# # Check the summary data - this will show the number of missing data if any.
#   summary(glu_2$KCAL)
# # There is no missing data for KCAL.
#   
# # Create a line chart of the KCAL frequency of each group.   
#   KCALfreq <- ggplot(data=glu_2, aes(x=KCAL, group=GLU_index, color=GLU_index)) +
#     geom_density(adjust=1.5, alpha=.4, size=1.2, linetype="longdash") + space_axes + no_grid +
#     scale_color_manual(values= c("steelblue3", "gold3", "hotpink") ) +
#     labs(x="KCAL", y="Density") +
#     scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
#   KCALfreq
#   
# # Save the chart as .pdf.
#   ggsave("Laboratory_data/QCtotal_d_glu_body_meta_demo_n1625_KCAL_by_GLU_index_line.pdf", 
#          KCALfreq, device="pdf", width=5.3, height=4.5)

# ===============================================================================================================
# Build a stacked bar chart of diabetics by age and gender.
# ===============================================================================================================

# Add age group label.
  glu_2$agegroup <- NA
  
  for(i in 1: nrow(glu_2)){
    if(     glu_2$RIDAGEYR[i] < 30){ glu_2$agegroup[i] <- "29_and_below" }
    else if(glu_2$RIDAGEYR[i] < 40){ glu_2$agegroup[i] <- "30s" }
    else if(glu_2$RIDAGEYR[i] < 50){ glu_2$agegroup[i] <- "40s" }
    else if(glu_2$RIDAGEYR[i] < 60){ glu_2$agegroup[i] <- "50s" }
    else{                            glu_2$agegroup[i] <- "60s_and_over" }
  }
  table(glu_2$agegroup)

# ---------------------------------------------------------------------------------------------------------------
# male
# Select males 
  glu_2_males <- subset(glu_2, RIAGENDR == 1) 

# Check the dimension of the selected data - 774 rows.
  dim(glu_2_males)

# Create summary for plotting a barchart.  
  library(dplyr)

  mycolors = c("aquamarine3", "lightgoldenrod3", "lightpink1")
  
  perc <- ggplot(glu_2_males %>% count(agegroup, GLU_index) %>%    
           group_by(agegroup) %>% 
           mutate(pct=n/sum(n)),              
         aes(agegroup, n, fill=GLU_index)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), 
              position=position_stack(vjust=0.5)) +
    scale_fill_manual(values = mycolors) +
    labs(x="Age Group (Male)") +
    rotate_X_labels + space_axes + no_grid
  perc
  ggsave("Laboratory_data/GLU_index_by_age_male.png", 
         perc, device="png", width=5.3, height=4.5)
  

# WITHOUT dplyr
# Change the color order --> blue, yellow, red.

  countbar <- ggplot(glu_2_males, aes(agegroup)) +
    geom_bar(aes(fill=GLU_index)) + 
    scale_fill_manual(values = mycolors) +
    labs(x="Age Group") +
    rotate_X_labels + space_axes + no_grid 
  countbar
  
# percentage 
  stacked <- ggplot(glu_2_males, aes(agegroup)) +
    geom_bar(aes(fill= GLU_index), position = "fill") + 
    scale_fill_manual(values = mycolors) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = paste0(agegroup*100,"%")), 
              position = position_stack(vjust = 0.5), size = 2) +    
    rotate_X_labels + space_axes + no_grid 
  stacked
# hmmmm 
  
# ---------------------------------------------------------------------------------------------------------------
# FEmale
# Select FEmales 
  glu_2_fems <- subset(glu_2, RIAGENDR == 2) 
  
  # Check the dimension of the selected data - 774 rows.
  dim(glu_2_fems)
  
  # Create summary for plotting a barchart.  
  library(dplyr)
  
  mycolors = c("aquamarine3", "lightgoldenrod3", "lightpink1")
  
  perc <- ggplot(glu_2_fems %>% count(agegroup, GLU_index) %>%    
                   group_by(agegroup) %>% 
                   mutate(pct=n/sum(n)),              
                 aes(agegroup, n, fill=GLU_index)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%")), 
              position=position_stack(vjust=0.5)) +
    scale_fill_manual(values = mycolors) +
    labs(x="Age Group (Female)") +
    rotate_X_labels + space_axes + no_grid
  perc
  ggsave("Laboratory_data/GLU_index_by_age_female.png", 
         perc, device="png", width=5.3, height=4.5)
  
# Show HEX code to use in Excel
  gplots::col2hex("aquamarine3")
  gplots::col2hex("lightgoldenrod3")
  gplots::col2hex("lightpink1")
  