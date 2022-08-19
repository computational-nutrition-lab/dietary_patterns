library(vegan)
data(dune.env)
head(dune.env)
attach(dune.env)
head(dune)

  
# ----------------------------------------------------------------------------------------------------------
# Study the RDA tutorial
# ----------------------------------------------------------------------------------------------------------
# chapter 2 Prepare for the workshop
  # Load the required packages
  library(vegan)
  library(labdsv)
  library(MASS)
  library(mvpart)
  library(ggplot2)
  
  # # Install the required packages
  # install.packages("vegan")
  # install.packages("labdsv")
  # install.packages("MASS")
  # install.packages("ggplot2")
  # 
  # # install mvpart from package archive file
  # install.packages("remotes")
  # remotes::install_url("https://cran.r-project.org/src/contrib/Archive/mvpart/mvpart_1.6-2.tar.gz")
  
# Chapter 4
# Make sure the files are in your working directory!  If R
# cannot find the dataset, set your working directory with
# setwd() to the folder in which your data is stored (e.g.
# setwd('~/Desktop/workshop10'))
  
  setwd("~/GitHub/dietary_patterns/RDA/")
  # Species community data frame (fish abundance)
  # spe <- read.csv("data/doubsspe.csv", row.names = 1)
  spe_raw <- read.table("data/doubsspe.txt", sep=",", header=T, row.names = 1)
  spe <- spe_raw[-8, ]  # Site number 8 contains no species, so we remove row 8 (site 8) 
  head(spe)
  
  # Environmental data frame: "DoubsEnv.csv"
  # env <- read.csv("data/doubsenv.csv", row.names = 1)
  env_raw <- read.table("data/doubsenv.txt", sep=",", header=T, row.names = 1)
  head(env_raw)
  env <- env_raw[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 
  head(env)
  
# Count number of species frequencies in each abundance class
  ab <- table(unlist(spe))
# Plot distribution of species frequencies
  barplot(ab, las = 1, # make axis labels perpendicular to axis
          xlab = "Abundance class", ylab = "Frequency", # label axes
          col = grey(5:0/5)) # 5-colour gradient for the bars
  
  # Count the number of zeros in the dataset
  sum(spe == 0)
  
  # Calculate proportion of zeros in the dataset
  sum(spe == 0)/(nrow(spe) * ncol(spe))
  
  # The Hellinger transformation expresses abundances as the square-root of their relative 
  # abundance at each site (Borcard, Gillet, and Legendre 2011), solving the issue 
  # with double zeros. We will apply this transformation to the fish abundance dataset.
  
  # Apply Hellinger transformation to correct for the double zero problem
  spe.hel <- decostand(spe, method = "hellinger")
  
# We can visually look for correlations between variables:
  heatmap(abs(cor(env)), 
          # Compute pearson correlation (note they are absolute values)
          col = rev(heat.colors(6)), 
          Colv = NA, Rowv = NA)
  legend("topright", 
         title = "Absolute Pearson R",
         legend =  round(seq(0,1, length.out = 6),1),
         y.intersp = 0.7, bty = "n",
         fill = rev(heat.colors(6)))  
  
# 4.2.2 Standardizing the environmental variables

  # Scale and center variables
  env.z <- decostand(env, method = "standardize")
  head(env.z)
  
  # Variables are now centered around a mean of 0
  round(apply(env.z, 2, mean), 1)
  
  # and scaled to have a standard deviation of 1
  apply(env.z, 2, sd)

# Chapter 6 Redundancy analysis
# https://r.qcbs.ca/workshop10/book-en/redundancy-analysis.html
  
# We'll use our standardized environmental data, but we
# will remove 'das', which was correlated with many other
# variables:
 
  # Make sure the files are in your working directory!  If R
  # cannot find the dataset, set your working directory with
  # setwd() to the folder in which your data is stored (e.g.
  # setwd('~/Desktop/workshop10'))
  
  # We'll use our standardized environmental data, but we
  # will remove 'das', which was correlated with many other
  # variables:
  env.z <- subset(env.z, select = -das)
  head(env.z)
  
# Run the RDA.
  # Model the effect of all environmental variables on fish
  # community composition
  spe.rda <- rda(spe.hel ~ ., data = env.z)
    
  aaa <- summary(spe.rda)
  head(aaa)
  # importance of components.
  summary(spe.rda)[11]
  RsquareAdj(spe.rda)
  
  summary(spe.rda)[6]
  summary(spe.rda)[5]
  # biplot
  summary(spe.rda)[4]
  summary(spe.rda)[3]
  
  
# 6.2.1 Selecting variables
  
# Here, we will be performing forward selection on our 11 environmental variables. 
  
  # Forward selection of variables:
  fwd.sel <- ordiR2step(rda(spe.hel ~ 1, data = env.z), # lower model limit (simple!)
                        scope = formula(spe.rda), # upper model limit (the "full" model)
                        direction = "forward",
                        R2scope = TRUE, # can't surpass the "full" model's R2
                        pstep = 1000,
                        trace = FALSE) # change to TRUE to see the selection process!
  
  # Check the new model with forward-selected variables
  fwd.sel$call
   
  # What is the adjusted R2 of the RDA with the selected variables?
  # Write our new model
  spe.rda.signif <- rda(spe.hel ~ alt + oxy + dbo, data = env.z)
  # check the adjusted R2 (corrected for the number of
  # explanatory variables)
  RsquareAdj(spe.rda.signif)
  
# 6.2.2 Significance testing
    
  anova.cca(spe.rda.signif, step = 1000)

  anova.cca(spe.rda.signif, step = 1000, by = "term")
  
  # You can also test the significance of each canonical axis with by = "axis". 
  # Recall that these axes represent the variation in explanatory variables in fewer dimensions.
  
  anova.cca(spe.rda.signif, step = 1000, by = "axis")   
  
# 6.2.3 RDA plot 
    
  # Type 1 scaling
  ordiplot(spe.rda.signif, scaling = 1, type = "text")
  # Type 2 scaling
  ordiplot(spe.rda.signif, scaling = 2, type = "text")
  
  
# ----------------------------------------------------------------------------------------------------------
# Excerpt of Abby's R markdown code
# ----------------------------------------------------------------------------------------------------------
# # Practice RDA with vegan
#   library(vegan)
#   
#   tax <- read.delim("data/processed_UN_tax/UN_taxonomy_clr_f.txt", sep = "\t", row.names = 1)
#   
#   # Drop soylent
#   tax <- tax[,!colnames(tax) == "MCTs11"]
#   tax <- tax[,!colnames(tax) == "MCTs12"]
#   
#   # make sure most prevalent taxa are ordered first
#   tax <- tax[order(rowMeans(tax), decreasing = T),]
#   
#   # transpose
#   tax_t <- as.data.frame(t(tax))
# 
# # Not sure where the map files are coming from.  
#   
#   map <- read.table("data/maps/SampleID_map.txt", sep = "\t", header = T, comment = "")
#   map_username <- read.table("data/maps/UserName_map.txt", sep = "\t", header = T, comment = "" )
#   
# # Take only the records whose UserName is in tax_t.
#   map_ord <- map_username[map_username$UserName %in% rownames(tax_t),]
#   
# # set food vectors to weighted or unweighted
#   food_vectors <- food_vectors_w
#   
# # rename for easy plotting
#   colnames(food_vectors) <- c("Grains", "Poultry", "Alcohol", "Fats", "Cakes")
#   
# # env
#   env <- merge(food_vectors, map_ord, by = 0)
  
# ----------------------------------------------------------------------------------------------------------
# RDA model building

#### unconstrained
  un_rda <- rda(tax_t)
  
#### constrained
  # note use formula notation to include factors (i.e. gender)
  
  # just diet
  rrda_d <- rda(tax_t ~ Grains + Poultry + Alcohol + Fats + Cakes , data = env)
  
  # diet + metadata
  rrda_a <- rda(tax_t ~ Age + Gender + BMI + Grains + Poultry + Alcohol + Fats + Cakes , data = env)
  
  # just metadata
  rrda_m <- rda(tax_t ~ Age + Gender + BMI, data = env)
  
  # But the point is to have 
  # tax_t ... transformed TAX data; that's my Y matrix.
  # env - X matrix. and it has dietary data. 
  # and build RDA formula using the Y and X matrices.
  
# ----------------------------------------------------------------------------------------------------------
  # Sample
  # MCT
  # meta <- read.csv( "~/GitHub/dietary_patterns/eg_data/dietstudy/food_map_txt_Metadata_2.csv", 
  # row.names = 1, check.names = F)
  
  
# ----------------------------------------------------------------------------------------------------------  
# Try with dietstudy.  
# ----------------------------------------------------------------------------------------------------------  
# Set working dir
  SpecifyDataDirectory("eg_data/NHANES/Laboratory_data/")
  setwd("~/GitHub/dietary_patterns/eg_data/NHANES/Laboratory_data/")

# Load the distinct 100 colors for use.
  distinct100colors <- readRDS("~/GitHub/R_Toolbox/distinct100colors.rda")

# Load the necessary scripts.
  source("../../../lib/unifrac_ordination.R")

# Food
  # Load food OTU table - this is our food OTU data
  food <- read.delim("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.dhydrt.otu.txt", row.names=1)
  PrepFood(data=food)
  food[1:10, 1:10]
  # "food" has taxonomy data already.
  food[1:10, 128:129]
  
# Taxonomy (tax)
  tax <- read.delim("Foodtree/Food_D12_FC_cc_f_males50s_red_Lv4.taxonomy.txt")
  # Format the tax file and create a taxonomy table called TAX.
  PrepTax(data=tax)
  head(TAX,1) 
  
# Some safety check
  head(food,1) 
  dim(food) 
  colnames(food)
  food_t <- t(food)
  colnames(food_t)
  head(food_t)[1:3,1:3]
  

# I think food is my response matrix....

# Demographic data - the explanatory variables.  
# NHANES
  library(SASxport)
  mea <- read.xport("../BodyMeasures/BMX_I.XPT")
  demog <- read.xport("../DEMO_I.XPT")
  glu <- read.xport("GLU_I.XPT")
  head(mea,1)
  head(demog,1)
  head(glu,1)
  
  colnames(food)  # SEQN starts with 'X'
  
# demog needs to have XSEQN as rownames in order to use it in PrepMeta function. 
  rownames(demog) <- paste("X", demog$SEQN, sep="") # Add 'X' at the beginning
  demog$XSEQN <-     paste("X", demog$SEQN, sep="") # Add 'X' at the beginning
  head(demog, 1)
  
  mea$XSEQN <-     paste("X", mea$SEQN, sep="") # Add 'X' at the beginning
  head(mea, 1)
  
  glu$XSEQN <-     paste("X", glu$SEQN, sep="") # Add 'X' at the beginning
  head(glu, 1)

# Pick up what I need
  mea_1 <- mea[, c("XSEQN", "BMXBMI", "BMXWAIST" , 'BMDAVSAD')]  
  # demog_1 <- glu[, c("XSEQN", "INDHHIN2" )] # annual household income (just to see)
  demog_1 <- glu[, c("XSEQN", "LBXGLU" )]   # fasting glucose (mg/dL)
  
  mea_demog_1 <- merge(mea_1, demog_1, by="XSEQN") 
  head(mea_demog_1)
  dim(mea_demog_1)

# Take only the XSEQN that are in food
  head(food, 1)
  mea_demog_1_s <- mea_demog_1[mea_demog_1$XSEQN %in% colnames(food), ]
  head(mea_demog_1_s)
  dim(mea_demog_1_s)
  # This is my "env" matrix....
  env_1 <- mea_demog_1_s 

# Need to remove any missing data in order to run RDA. really??
  library(naniar)
  vis_miss(env_1)
  
# Need to remove those with missing metadata... uuugh 
  env_2 <- na.omit(env_1)
  vis_miss(env_2)

# Add a column that has the XSEQN. 
  food_t$XSEQN <- row.names(food_t)

# From "food_t", Pick up only the SEQN in env_2.
  food_t_s <- food_t[food_t$XSEQN %in% env_2$XSEQN, ]
  
# This is my food (response matrix)  

# ---------------------------------------------------------------------------------------------------------------
# Scale and center variables

# Make XSEQN as the rownames and delete that column.  
  head(env_2)
  row.names(env_2) <- env_2$XSEQN
  env_3 <- env_2[, -1]
  
  env.z <- decostand(env_3, method = "standardize")
  head(env.z)
  
  # Variables are now centered around a mean of 0
  round(apply(env.z, 2, mean), 1)
  
  # and scaled to have a standard deviation of 1
  apply(env.z, 2, sd)
  
# ---------------------------------------------------------------------------------------------------------------
# Hellinger transformation (to base ordination on presence similarity, not absence similarity)
 # Need to have the variables (foods) in the columns, in order to filter for them. 
#   is(food)
#   is(food_t)
#   food_t <- data.frame(t(food))
#   head(food,1)
#   head(food_t, 1)
#   str(food_t)
#   is(food_t)
#   food_t[1:2, 1:2]
#   
# # change to numeric 
#   food_t
#   df1 <- lapply(food_t, as.numeric)
#   dim(df1)
#   
#   dim(food_t)
#   dim(food)
#   
#   # Look at the min values of each column. except for the last column which is taxonomy.
#   apply(food[, -129], 2, min)
#   colnames(food_t) <- NA
#   apply(food_t[-129, ], 1, min)
#   # For all the columns (food), the min is zero, and no columns contains negative values.  
#   food_t <- t(food)
#   
#   # Hellinger transformation. 
#   food_t.hel <- decostand(food_t, method = "hellinger")
#   
#   food.hel <- decostand(food[, -129], method = "hellinger")
#   head(food.hel,2)
#   
#   food_t.hel <- decostand(food_t, method = "hellinger")
#   head(food_t.hel)
  
  # Cannot do it... I think the columnnames (food names) are too long of the food_t dataframe. 
  # proceed for now without transformation.
  
  
# ---------------------------------------------------------------------------------------------------------------
# RDA
  # Run the RDA.
  # Model the effect of the body measure and demographic variables on dietary patterns.
  head(food_t_s)
  food_t_s[1:2, 1191:1192]
  # Need to remove the XSEQN column.
  food_t_s_2 <- food_t_s[, -1192] 
  food_t_s_2[1:2, 1190:1191]
  str(food_t_s_2)
  
  # convert characters to numeric. 
  food_t_s_3 <-  sapply( food_t_s_2, as.numeric )
  str(food_t_s_3)
  
# RDA
  food.rda <- rda(food_t_s_3 ~ ., data = env.z)
  
  summary(food.rda)
  # The second element (Partitioning of variance) shown should say:
  # 
  #               Inertia Proportion
  # Total          111778    1.00000
  # Constrained      3294    0.02947
  # Unconstrained  108484    0.97053
  # This is to be interpreted as: 
  # "The included environmental variables explain 2.947% of the variation in dietary pattern composition across users."
  
  RsquareAdj(food.rda)
  # But adjusted R square is -0.002882421.
  
  anova.cca(food.rda, step = 1000)
  
# 6.2.1 Selecting variables
  
# Here, we will be performing forward selection on our environmental variables. 
  
# Forward selection of variables:
  fwd.sel <- ordiR2step(rda(food_t_s_3 ~ 1, data = env.z), # lower model limit (simple!)
                        scope = formula(food.rda), # upper model limit (the "full" model)
                        direction = "forward",
                        R2scope = TRUE, # can't surpass the "full" model's R2
                        pstep = 1000,
                        trace = FALSE) # change to TRUE to see the selection process!
  
  # Check the new model with forward-selected variables
  fwd.sel$call
  
  # What is the adjusted R2 of the RDA with the selected variables?
  # Write our new model
  food_t_s_3.rda.signif <- rda(food_t_s_3 ~ 1, data = env.z)
  # check the adjusted R2 (corrected for the number of
  # explanatory variables)
  RsquareAdj(food_t_s_3.rda.signif)
  
  # 6.2.2 Significance testing
  
  anova.cca(food_t_s_3.rda.signif, step = 1000)
  
  anova.cca(food_t_s_3.rda.signif, step = 1000, by = "term")
  
  # You can also test the significance of each canonical axis with by = "axis". 
  # Recall that these axes represent the variation in explanatory variables in fewer dimensions.
  
  anova.cca(food_t_s_3.rda.signif, step = 1000, by = "axis")   
  
  
  
  
  
  
  
  