####
# This file imports and cleans FOMC minutes from txt files
####

setwd("~/Documents/GitHub/intro-text-analysis-econ")
rm(list = ls())

require(readtext)
require(stringr)
require(tidyverse)
require(lubridate)


############### Minutes (pre 2014) ############### 
fedminutes_df <- read.csv("data/fedmins_pre2014.csv", stringsAsFactors = FALSE)

########### Add post 2014 data ################
extra_files <- dir("data/minutes_2014onward/")
extra_dates <- as.Date(str_remove_all(extra_files, "mins_|.txt"), format = "%d-%m-%Y")
extra_files <- extra_files[order(extra_dates)]

pb = txtProgressBar(min = 1, max = length(extra_files), initial = 1) 
for (ii in 1:length(extra_files)){
  
  docid <- extra_files[ii]
  import_filename <- paste0("data/minutes_2014onward/", docid)
  meeting_txt <- readtext(import_filename, encoding = "utf-8")
  
  paras <- str_split(meeting_txt$text, "\n\n")[[1]]
  paras <- str_squish(paras)
  paras <- paras[which(nchar(paras) > 1)]
  # Create empty dataframe to fill
  temp.df <- data.frame(matrix(NA,length(paras),ncol(fedminutes_df)))
  names(temp.df) <- names(fedminutes_df)
  
  # Save the text itself
  temp.df$paragraph <- paras
  temp.df$document <- docid
  
  date <- str_split(docid, "-")[[1]]
  temp.df$date <- paste(str_remove(date[3], ".txt"), date[2], "01", sep = "-")
  # Append to dataframe for that file
  fedminutes_df <- rbind(fedminutes_df, temp.df)
  setTxtProgressBar(pb,ii)
}
# Add a unique paragraph identifier
fedminutes_df$unique_id <- paste0("FEDp_", 1:nrow(fedminutes_df))
# Add a quarter variable
fedminutes_df$quarter <- floor_date(as.Date(fedminutes_df$date), "quarter")


########### A bit more cleaning ########### 
# Remove very short paragraphs
fedminutes_df <- fedminutes_df %>%
  mutate(nchar = nchar(paragraph)) %>%
  subset(nchar > 10)

# Remove administrative paragraphs
# For example:
fedminutes_df[which((str_detect(fedminutes_df$paragraph, "Votes against"))), "paragraph"]

remove_phrases <- c("Votes against", "Vote against", "Votes for", "Voting against",
                    "Voting for", "Absent and", "^The vote encompassed approval", 
                    "^The votes encompassed approval", "^The vote also encompassed approval",
                    "voted as alternate member", "voted as the alternate",
                    "voted as an alternate", "Return to top")
for (rr in remove_phrases){
  fedminutes_df <- fedminutes_df[which(!str_detect(fedminutes_df$paragraph, rr)),]
}


########### Export clean data ########### 
write.csv(fedminutes_df, "data/fedminutes_clean.csv", row.names = FALSE)

