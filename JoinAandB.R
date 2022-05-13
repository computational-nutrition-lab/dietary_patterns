# --------------------------------------------------------------------------------------------------------
# How to speed up a loop when going over 100K+ rows.
# 05/11/2022  Rie Sadohara 
# --------------------------------------------------------------------------------------------------------


# Load data.
  # Table A = Fdcd_GRMS has the food code and consumption amount in grams. 
  Fdcd_GRMS <- read.table("TableA.txt", sep="\t", header=T)
  
  # Table B = FPED has the references of food groups and their serving weight/100 g.
  FPED <-      read.table("TableB.txt", sep="\t", header=T)
  
# i=1 --------------------------------------------------------------------------------- 

# Select only one row at a time.
  pickedrow <- Fdcd_GRMS[1, ]

# Pick up a row in FPED that contains the food_code in pickedrow.
  pickedFPED <- FPED[FPED$Food_code == pickedrow$Food_code, ] 

# GRMS x each food category --> cup or servings of that food in that particular amount. 
  cup_oz <- pickedrow[, "DR2IGRMS"] * pickedFPED[, -1]/100  # "-1" is to exclude food_code from multiplication.

# Join pickedrow and cup_oz, which is the categorized food items converted to cup or oz. 
  Food_Cat <- cbind(pickedrow, cup_oz)

# Create a dataframe to save results; has the same ncol as Food_Cat.
  result_v2 <- data.frame(matrix(NA, nrow=1000, ncol=ncol(Food_Cat)))  # My goal is to have nrow = nrow(Fdcd_GRMS)
  
# Match the column names of result_v2 with those of Food_Cat. 
  colnames(result_v2) <- colnames(Food_Cat)

# Put Food_Cat to the corresponding row.
  result_v2[1, ] <- Food_Cat[1, ]

# i=1 is done.

# i=2 and onward ---------------------------------------------------------------------

  # create a progressbar object outside the loop
  pb = txtProgressBar(min = 0, max = nrow(result_v2), initial = 0)
  
  # Run a loop to go through all the rows in Fdcd_GRMS. 
  # My goal is to do for(i in 2:nrow(Fdcd_GRMS)), but it takes so long that R stops working.
  for(i in 2:1000){   
    
    # Select only one row at a time.
    pickedrow <- Fdcd_GRMS[i, ]
    
    pickedfoodcode <- pickedrow$Food_code 
    
    # Look for a row in FPED that contains the food_code in pickedrow.
    pickedFPED <- FPED[FPED$Food_code == pickedfoodcode, ] 
    
    # GRMS
    quantity <- pickedrow[, "DR2IGRMS"]
    
    # Make a vector of pickedFPED[, -1][1,]/100) 
    vec <-as.numeric(as.vector(pickedFPED[, -1][1,]/100)) # "-1" is to exclude food_code from multiplication.
    
    # GRMS x each food category --> cup or servings of that food in that particular amount.
    cup_oz_vec <- quantity * vec  
    
    # Join pickedrow and cup_oz, which is the categorized food items converted to cup or oz.
    tempnewrow <- c(pickedfoodcode, quantity, cup_oz_vec)
    
    # Put Food_Cat to the corresponding row.
    result_v2[i, ] <- tempnewrow

    # inside you need to update with every iteration
    setTxtProgressBar(pb, i)
    
    # close progress bar at the end of your loop:
    close(pb)
    
  }
  
# This has the result! 
  result_v2
  
  
