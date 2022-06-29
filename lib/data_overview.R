# ========================================================================================
# Functions used in data overview.
# Version 1
# Created on 06/23/2022 by Rie Sadohara
# ========================================================================================

# ========================================================================================
# Generate a summary of each variable in a dataframe 
# ========================================================================================

# Calculate minimum, 1st quantile, median, mean, 3rd quantile, max, and standard deviation
# for each variable in the input dataframe.   

  SummaryStats <- function(inputdf, outfn){
    # Create an empty table to save results.
    summarybox <- data.frame(Variables=names(inputdf), 
                             Min=NA, FirstQu=NA, Median=NA, Mean=NA, ThirdQu=NA, Max=NA, SD=NA)
    # Define input.
    input <- inputdf
    
    # For each column (variable), calculate summary statistics. If not numeric, indicate so.
    for(i in 1:ncol(input)){
      
      ith_col <- input[, i]
      
      # if numeric, calculate summary stats.
      if(is.numeric(ith_col)){
        summarybox[i, 2] <- min(ith_col)
        summarybox[i, 3] <- quantile(ith_col, 0.25)
        summarybox[i, 4] <- median(ith_col)
        summarybox[i, 5] <- mean(ith_col)
        summarybox[i, 6] <- quantile(ith_col, 0.75)
        summarybox[i, 7] <- max(ith_col)
        summarybox[i, 8] <- sd(ith_col)
        
      }else{
        
        # if not numeric, say not numeric.
        summarybox[i, 2:8] <- "not_numeric"
      }  
    }
    # Save the summary information.
    write.table(summarybox, file=outfn, sep="\t", row.names=F, quote=F)
  }

# ---------------------------------------------------------------------------------------------------------------
# Generate a line plot with only individuals with all days of data are connected with dots.
# Or do not connect where there is missing data.
# merge out table and extracted variable table

  df <- tot_m_QCed # (contains all data)
  colnames(df)
  # df$RecallNo = factor(df$RecallNo, levels= 1:3)
  
  out <- with(df, tapply(KCAL, list(UserName,  RecallNo), sum) )
  out2 <- as.data.frame(out)
  out2$UserName <- row.names(out2)
  row.names(out2) <- NULL
  out2 
  library(reshape2)
  out3 <- melt(out2)
  out3
  # pick up UserName that has NA values
  rows_w_NA <- out3[is.na(out3$value), ]
  
  # vector of partial users
  partial_users <- unique(rows_w_NA$UserName)
  
  # full_df <- df %>% 
  #   filter(!UserName %in% partial_users)
  
  full_df <- subset(df, !UserName %in% partial_users) # pick up df records of users who are not partical.
  full_users <- unique(full_df$UserName) # Not needed for ggplot, but FYI...

  PrepLinePlot <- function(inputdf, day, username, variable){
   df <- inputdf

   df[, day]  <-  factor(df[, day])
   
   # make a table that has NA where values are missing for each user-day combo 
   out <- with(df, tapply(KCAL, list(UserName, RecallNo), sum) )
   # out2 <- as.data.frame(out)
   # out2$un <- row.names(out2)
   # row.names(out2) <- NULL
   
   print(out)
   print(username)
   print(variable)
   
  } 
  out2
  PrepLinePlot(inputdf=df, day="RecallNo", username="UserName", variable="KCAL")
  
  str(df)
  aaa = subset(df, select = "RecallNo")
  as.factor(aaa)
    
  ggplot() +
    geom_point(df, mapping = aes(x=RecallNo, y=KCAL, group=UserName, color=UserName)) +
    geom_line(full_df, mapping = aes(x=RecallNo, y=KCAL, group=UserName, color=UserName), linetype="dashed")
  
  
# This one works too - not a function yet ========================================================================
  # Define users and days in your df
  users <- unique(df$UserName)
  days <- order(unique(df$RecallNo), decreasing = F)
  
  df$RecallNo <- as.factor(df$RecallNo)
  
  # Create a character vector to be "users"
   usersvector = rep(users, length(days))
   
  # Create a character vector to be "days"
   daysvector <- character(0)

   k = length(users) 
   
   # Repeat each day as many as there are users.  
   for(i in 1:length(days)){
     if(i==1){daysvector = rep(days[i], k)}else{daysvector = c(daysvector, rep(days[i], k) ) }
   }
   daysvector
  
  full_user_day = data.frame(UserName = usersvector, RecallNo = daysvector)
  # Add a column of user_day
  full_user_day$User_Day = paste(full_user_day$UserName, full_user_day$RecallNo, sep="_")
  # Change the colnames slightly so that .x or .y won't be inserted after merging.
  # Leave "User_Day" as is, because this is needed for merging.
  colnames(full_user_day) <- c("UserName_a", "RecallNo_a", "User_Day")
  
  # Also add a column of user_day to df.
  # df may already have User_Day, but it won't hurt to make it again.
  df$User_Day = paste(df$UserName, df$RecallNo, sep="_")
  head(colnames(df))
  # Merge, so that the rows absent in full_user_day will be NA.
  df_w_NA <- merge(x=full_user_day, y=df, by="User_Day", all.x=TRUE)
  head(colnames(df_w_NA))

  # pick up UserName that has NA values
  rows_w_NA <- df_w_NA[is.na(df_w_NA$UserName), ]
  
  # vector of partial users
  partial_users <- unique(rows_w_NA$UserName_a) # UserName_a has actual UserNames.  
  
  # full_df <- df %>% 
  #   filter(!UserName %in% partial_users)
  
  full_df <- subset(df, !UserName %in% partial_users) # pick up df records of users who are not partial.
  full_users <- unique(full_df$UserName) # Not needed for ggplot, but FYI...
  
  # Plot points and lines separately.   
  ggplot() +
    geom_point(df, mapping = aes(x=RecallNo, y=KCAL, group=UserName, color=UserName)) +
    geom_line(full_df, mapping = aes(x=RecallNo, y=KCAL, group=UserName, color=UserName), linetype="dashed") +
    no_grid
  # only those that have all days of data are connected with lines. 

# ========================================================================
#  make this into function    
  PrepLinePlot <- function(inputdf, day, username, all.fn, full.days.only.fn){
    df <- inputdf
    
    # Define users and days in your df
    users <- unique(df[, username])
    days  <- order(unique(df[, day]), decreasing = F)
    
    # Make day as a factor.
    df[, day]  <-  factor(df[, day])
    
    # Create a character vector to be "users"
    usersvector = rep(users, length(days))
    
    # Create a character vector to be "days"
    daysvector <- character(0)
    
    k = length(users) 
    
    # Repeat each day as many as there are users.  
    for(i in 1:length(days)){
      if(i==1){daysvector = rep(days[i], k)}else{daysvector = c(daysvector, rep(days[i], k) ) }
    }
    
    full_user_day = data.frame(UserName = usersvector, RecallNo = daysvector)
    # Add a column of user_day
    full_user_day$User_Day = paste(full_user_day$UserName, full_user_day$RecallNo, sep="_")
    # Change the colnames slightly so that .x or .y won't be inserted after merging.
    # Leave "User_Day" as is, because this is needed for merging.
    colnames(full_user_day) <- c("UserName_a", "RecallNo_a", "User_Day")
    
    # Also add a column of user_day to df.
    # df may already have User_Day, but it won't hurt to make it again.
    # df$User_Day = paste(df$UserName, df$RecallNo, sep="_")
    df$User_Day = paste(df[, username], df[, day], sep="_")
    
    # Merge, so that the rows absent in full_user_day will be NA.
    df_w_NA <- merge(x=full_user_day, y=df, by="User_Day", all.x=TRUE)
    
    # pick up UserName that has NA values
    rows_w_NA <- df_w_NA[is.na(df_w_NA[, "UserName"]), ]  # UserName could eb RecallNo or KCAL...  
    
    # vector of partial users
    partial_users <- unique(rows_w_NA$UserName_a) # UserName_a has actual UserNames. 
    
    # pick up df records of users who are not partial.
    full_df <<- subset(df, !UserName %in% partial_users)     
    full_users <- unique(full_df[, "UserName"]) # Not needed for ggplot, but FYI... 
    
    # all data with NA inserted
    write.table( x=df_w_NA, file = all.fn, sep="\t", row.names = F, quote=F) 
    # Save users' rows with full data only 
    write.table( x=full_df, file = full.days.only.fn, sep="\t", row.names = F, quote=F) 

  } 

  PrepLinePlot(inputdf=df, day="RecallNo", username="UserName", 
                all.fn=           "VVKAJ_2021-11-09_7963_Tot_m_QCed_wNA.txt",
                full.days.only.fn="VVKAJ_2021-11-09_7963_Tot_m_QCed_fullonly.txt")
 # OK!!!!
 # NEXT, check to see if this works with different UserName and RecallNo names. 
  

  # This just connects existing data automatically 
  ggplot(df, aes(x=as.factor(RecallNo), y=TFAT, group=UserName)) +
    geom_line(linetype="dashed", aes(color=UserName)) +
    geom_point(aes(color=UserName)) + 
    # xlab("Day") + # Re-annotate the X-axis label. 
    no_grid
  
  
  
  
  

  
    
# ---------------------------------------------------------------------------------------------------------------
  