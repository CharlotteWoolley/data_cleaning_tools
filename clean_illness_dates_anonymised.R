#FILENAME: clean_illness_dates_anonymised.R
#AUTHOR: Charlotte Woolley
#DESCRIPTION: This code was originally written in order to clean owner reported start and end 
#dates for illnesses in dogs enrolled in the Dogslife project
#it has been anonymised and adapted to be used on an example dataset 
#PACKAGES NEEDED:
        library(plyr)
        library(dplyr)
        library(lubridate)
        library(data.table)

#read in data and format 

        dat <- read.csv('example_dates_data.csv')
        dat2 <- dat[,2:7] 
        dat2 <- dat2 %>%
                mutate(start_date_new = start_date,
                       end_date_new = end_date,
                       visit_date_new = visit_date)
        
#create columns to identify duplications in the ID and to identify whether the 
#entire data entry has been completely duplicated
        
        dat2 <- dat2 %>%
                mutate(duplications = (duplicated(dups_ID) | 
                                               duplicated(dups_ID, 
                                                          fromLast = TRUE)),
                       complete_dups_ID = group_indices_(dat2, 
                                .dots=c("ID", "start_date_new", 
                                        "end_date_new", "visit_date_new")),
                       complete_duplications = (duplicated(complete_dups_ID) | 
                                                duplicated(complete_dups_ID, 
                                                           fromLast = TRUE)))
        
        
        sum(dat2$duplications) #2392 duplications
        sum(dat2$complete_duplications) #808 complete duplications

