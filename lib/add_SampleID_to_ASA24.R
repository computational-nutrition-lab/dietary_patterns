# Create SampleID (Username x RecallDayNo) combination 
# to use in foodtree and ordination... 

# Load your raw items data.
  items_raw <- read.csv("VVKAJ_Items.csv", sep = ",", header=T) 

# Create a combination of user and day for merging. 
  items_raw$userxday <- paste(items_raw$UserName, items_raw$RecallNo, sep="_")
  
  head(items_raw)
  
# Create a dataframe that has UserName and RecallNo (Day).
  user_recallno <- items_raw[, c("UserName", "RecallNo", "userxday")]

  head(user_recallno)
  
# Remove duplicates 
  sampleIDtable <- user_recallno[!duplicated(user_recallno), ]
  head(sampleIDtable)
  dim(sampleIDtable)

# Sort by Username then day. 
  sampleIDtable_s <- sampleIDtable[order(sampleIDtable$UserName, sampleIDtable$RecallNo) , ]
  
# Create a SampleID column that shows the combination of user x day.
  # sampleIDtable$SampleID <- paste(sampleIDtable$UserName, sampleIDtable$RecallNo, sep = ".")
  sampleIDtable_s$SampleID <- paste("vvkaj.", 
                                    formatC(seq(from=1, to=nrow(sampleIDtable)), width=5,flag="0" ),
                                    sep="")
  head(sampleIDtable_s, 12)
  
# Add SampleID to Items data.
  items_raw_ID <- merge(x=items_raw, y=sampleIDtable_s[, c("SampleID", "userxday")], 
                        all.x=T, by="userxday" )
  
  # Remove the "userxday" column (which is in the first column) as not necessary.   
  items_raw_ID_2 <- items_raw_ID[, 2: ncol(items_raw_ID)]
  
# Bring SampleID (currently in the last column) to the first column. 
  items_raw_ID_3 <- items_raw_ID_2[, c(ncol(items_raw_ID_2), 1: (ncol(items_raw_ID_2)-1) ) ]
  
  head(items_raw_ID_3, 1)
  
# Save it as a .txt file for further processing.
  write.table(items_raw_ID_3, "VVKAJ_Items_ID.txt", sep="\t", row.names=F)

  
  
  