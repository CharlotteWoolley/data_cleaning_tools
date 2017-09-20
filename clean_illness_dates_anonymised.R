#FILENAME: clean_illness_dates_anonymised.R
#AUTHOR: Charlotte Woolley
#DESCRIPTION: This code was originally written in order to clean owner reported start and end 
#dates for illnesses in dogs enrolled in the Dogslife project
#it has been anonymised and adapted to be used on an example dataset 
#PACKAGES NEEDED:

        library(tidyverse)
        library(data.table)
        library(lubridate)

#PRELIMINARY DATA ORGANISATION/EXPLORATION
        
        #read in data and format 
        dat <- read_csv('example_dates_data.csv')
        dat2 <- dat[,2:7] 
        dat2 <- dat2 %>%
                mutate(row_no = 1:nrow(dat2),
                       ID = as.factor(ID),
                       start_date_new = start_date,
                       end_date_new = end_date,
                       visit_date_new = visit_date)
        
        #function that create columns to identify duplications in the ID and to
        #identify whether the entire data entry has been completely duplicated
        get_duplications <- function(X) {
                X <- X %>%
                        mutate(duplications = (duplicated(ID) | duplicated(ID, 
                                                                           fromLast = TRUE)),
                               complete_dups_ID = group_indices_(X, 
                                                                 .dots=c("ID", "start_date_new", 
                                                                         "end_date_new", "visit_date_new")),
                               complete_duplications = (duplicated(complete_dups_ID) | 
                                                                duplicated(complete_dups_ID, 
                                                                           fromLast = TRUE)))
                return(X)
        }
        
        dat2 <- get_duplications(dat2)
        sum(dat2$duplications) #2392 duplications
        sum(dat2$complete_duplications) #808 complete duplications
        
        #seperate the subsets of duplicated and non-duplicated data so that 
        #duplicates can be cleaned first
        dups <- subset(dat2, dat2$duplications == TRUE)  #2392 duplications
        not_dups <- subset(dat2, dat2$duplications == FALSE) #9026 not duplicated

       
#STEP 1 OF CLEANING – REMOVE THE COMPLETE DUPLICATIONS  
        
        #get_complete_dup_info finds the last complete duplicate in the data 
        #(i.e the most recently entered entry)
        get_complete_dup_info <- function(X){
                X <- X[order(X$complete_dups_ID), ]
                observation_comp <- 1:nrow(X) 
                last_obs_num_comp <- tail(observation_comp, 1)
                X$last_observation_comp <- last_obs_num_comp == observation_comp
                return(X)
        }
        #Apply this function to each group of complete duplicates
        dups <- dups %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_complete_dup_info(.)) %>%
                dplyr::ungroup()
        
        #Make a new dataset that contains only the most recent entry from complete 
        #duplicates and deletes all older complete duplicates        
        dups2 <- subset(dups, (dups$complete_duplications == FALSE) |
                                (dups$complete_duplications == TRUE & dups$last_observation_comp == TRUE))
        length(dups$ID) - length(dups2$ID) #This removes 435 data entries 
        
        #recreate duplicate columns to identify changes in the duplications
        dups2 <- get_duplications(dups2)
        sum(dups2$duplications) #1698 duplications
        sum(dups2$complete_duplications) #0 complete duplications

#STEP 2 OF CLEANING – REPLACE MISSING DATA WITH DATA IN OTHER DUPLICATE ROWS 
        
        #get_complete_dup_info finds the last duplicate in the data 
        #(i.e the most recently entered entry) and also finds the number of
        #entries in each group of duplicates
        get_dup_info <- function(X){
                X <- X[order(X$ID), ]
                observation <- 1:nrow(X)
                X$last_obs_num <- tail(observation, 1)
                X$last_observation <- X$last_obs_num == observation
                return(X)
        }
        #Apply this function to each group of duplicates
        dups3 <- dups2 %>%
                dplyr::group_by(ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()
        
        n <- (max(dups3$last_obs_num))-1 
        #There are a maximum of n(10) duplicates of every data entry

        #get_lags leads generates n (10) columns with the previous and following 
        #data entries for the start dates, end dates and visit dates
        get_lags_leads <- function(X, n) {
                X <- setDT(X)[, paste("SD_lag", 1:n, sep = "") 
                              := shift(start_date_new, 1:n)][]
                X <- setDT(X)[, paste("SD_lead", 1:n, sep = "") 
                              := shift(start_date_new, 1:n, type = "lead")][]
                X <- setDT(X)[, paste("ED_lag", 1:n, sep = "") 
                              := shift(end_date_new, 1:n)][]
                X <- setDT(X)[, paste("ED_lead", 1:n, sep = "") 
                              := shift(end_date_new, 1:n, type = "lead")][]
                X <- setDT(X)[, paste("VD_lag", 1:n, sep= "") 
                              := shift(visit_date_new, 1:n)][]
                X <- setDT(X)[, paste("VD_lead", 1:n, sep = "") 
                              := shift(visit_date_new, 1:n, type = "lead")][]
                return(X)
        }
        
        #Apply this function to each group of duplicates
        dups3 <- dups3 %>%
                dplyr::group_by(ID) %>%
                dplyr::do(get_lags_leads(., n)) %>%
                dplyr::ungroup()
        
        #get_missing_data makes binary columns for whether the columns contain 
        #missing data or not
        get_missing_data <- function (X,a,b,c,y) {
                X <- setDT(X)[, paste("start_date_miss", y, sep = "") 
                              := is.na(X[[a]])][]
                X <- setDT(X)[, paste("end_date_miss", y, sep = "") 
                              := is.na(X[[b]])][]
                X <- setDT(X)[, paste("visit_date_miss", y, sep = "") 
                              := is.na(X[[c]])][]
                return(X)
        }
        
        dups3 <- get_missing_data(dups3, a = "start_date_new", b = "end_date_new", 
                                  c = "visit_date_new", y = "")
        
        get_lags_leads_missing_data <- function(X, n) {
                for(i in 1:n) {
                        X <- get_missing_data(X, a = paste("SD_lag", i, sep=""), 
                                              b = paste("ED_lag", i, sep=""), 
                                              c = paste("VD_lag", i, sep=""), 
                                              y = paste("_lag", i, sep=""))
                        X <- get_missing_data(X, a = paste("SD_lead", i, sep=""), 
                                              b = paste("ED_lead", i, sep=""), 
                                              c = paste("VD_lead", i, sep=""), 
                                              y = paste("_lead", i, sep=""))
                }
                return(X)
        }
        
        dups3 <- ddply(dups3, 1, get_lags_leads_missing_data, n)
        
        replace_missing_data <- function(X, n) {
                for(i in 1:n) {
                #replace missing data in the start dates
                X$start_date_new <- ifelse(X$start_date_miss == FALSE |
                                           is.na(X$start_date_miss), 
                                        X$start_date_new, 
                                    ifelse(X$start_date_miss == TRUE & 
                                                  (X[, paste("start_date_miss_lag", 
                                                             i, sep="")] == FALSE), 
                                          X[, paste("SD_lag", i, sep="")], 
                                    ifelse(X$start_date_miss == TRUE & 
                                                 (X[, paste("start_date_miss_lead", 
                                                            i, sep="")] == FALSE), 
                                          X[, paste("SD_lead", i, sep="")], 
                                          X$start_date_new))) 
                #replace missing data in the end dates
                X$end_date_new <- ifelse(X$end_date_miss == FALSE |
                                                 is.na(X$end_date_miss), 
                                         X$end_date_new, 
                                  ifelse(X$end_date_miss == TRUE & 
                                                (X[, paste("end_date_miss_lag", 
                                                           i, sep="")] == FALSE), 
                                        X[, paste("ED_lag", i, sep="")], 
                                  ifelse(X$end_date_miss == TRUE & 
                                               (X[, paste("end_date_miss_lead", 
                                                          i, sep="")] == FALSE), 
                                        X[, paste("ED_lead", i, sep="")], 
                                        X$end_date_new))) 
                #replace missing data in the visit dates
                X$visit_date_new <- ifelse(X$visit_date_miss == FALSE |
                                                 is.na(X$visit_date_miss), 
                                         X$visit_date_new, 
                                    ifelse(X$visit_date_miss == TRUE & 
                                                (X[, paste("visit_date_miss_lag", 
                                                           i, sep="")] == FALSE), 
                                         X[, paste("VD_lag", i, sep="")], 
                                    ifelse(X$visit_date_miss == TRUE & 
                                               (X[, paste("visit_date_miss_lead", 
                                                          i, sep="")] == FALSE), 
                                         X[, paste("VD_lead", i, sep="")], 
                                         X$visit_date_new))) 
                X <- get_missing_data(X, a = "start_date_new", b = "end_date_new", 
                                      c = "visit_date_new", y = "")
                }
                return(X)
        }
        
        dups6 <- replace_missing_data(dups5, n)
        
        #Transform the date columns that have been edited back to human-readable format
        dups4$end_date_new <- as.Date(dups4$end_date_new, origin="1970-01-01")
        dups4$start_date_new <- as.Date(dups4$start_date_new, origin="1970-01-01")
        dups4$visit_date_new <- as.Date(dups4$visit_date_new, origin="1970-01-01")
        
        #START DATES
        sum(is.na(dups3$start_date)) #214 NAs
        sum(is.na(dups4$start_date_new)) #15 NAs - 199 mods
        #END DATES
        sum(is.na(dups3$end_date)) #206 NAs
        sum(is.na(dups4$end_date_new)) #7 NAs - 199 mods
        #VISITED VET DATES
        sum(is.na(dups3$visit_date)) #250 NAs
        sum(is.na(dups4$visit_date_new)) #18 NAs - 232 mods
        
        #recreate duplicate columns to identify changes in the duplications
        dups4 <- get_duplications(dups4)
        sum(dups4$duplications) #1698 duplications
        sum(dups4$complete_duplications) #731 complete duplications have appeared 
        #now because of added missing data
        
        dups4 <- dups4 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_complete_dup_info(.)) %>%
                dplyr::ungroup()
        
        dups5 <- subset(dups4, (dups4$complete_duplications == FALSE) |
                                (dups4$complete_duplications == TRUE & dups4$last_observation_comp == TRUE))
        length(dups4$ID) - length(dups5$ID) #This removes 375 data entries 
        
        #recreate duplicate columns to identify changes in the duplications
        dups5 <- get_duplications(dups5)
        sum(dups5$duplications) #1037 duplications
        sum(dups5$complete_duplications) #0 complete duplications 
        
#STEP 3 OF CLEANING – REPLACE ERRORS IN THE DATA FROM THE DUPLICATIONS 
        
        get_errors <- function (X,a,b,c,d) {
                X <- setDT(X)[, paste("SD_UP_error", d, sep = "") := (X[[a]] > X$date_recorded)][]
                X <- setDT(X)[, paste("SD_DOB_error", d, sep = "") := (X[[a]] < X$date_of_birth)][]
                X <- setDT(X)[, paste("ED_UP_error", d, sep = "") := (X[[b]] > X$date_recorded)][]
                X <- setDT(X)[, paste("ED_DOB_error", d, sep = "") := (X[[b]] < X$date_of_birth)][]
                X <- setDT(X)[, paste("VD_DOB_error", d, sep = "") := (X[[c]] < X$date_of_birth)][]
                X <- setDT(X)[, paste("SD_VD_error", d, sep = "") := (X[[a]] > X[[b]]) & (X[[a]] > X[[c]])][]
                X <- setDT(X)[, paste("ED_VD_error", d, sep = "") := (X[[b]] < X[[a]]) &  (X[[b]] < X[[c]])][]
                return(X)
        }
        
        dups5 <- get_errors(dups5, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
        
        get_lags_leads_errors <- function(X, n) {
                #SDs
                for(i in 1:n) {
                        X <- get_errors(X, a = paste("SD_lag", i, sep=""), 
                                        b = paste("end_date_new", i, sep=""), 
                                        c = paste("visit_date_new", i, sep=""), 
                                        d = paste("_SD_lag", i, sep=""))
                }
                for(i in 1:n) {
                        X <- get_errors(X, a = paste("SD_lead", i, sep=""), 
                                        b = paste("end_date_new", i, sep=""), 
                                        c = paste("visit_date_new", i, sep=""), 
                                        d = paste("_SD_lead", i, sep=""))
                }
                #EDs
                for(i in 1:n) {
                        X <- get_errors(X, a = paste("start_date_new", i, sep=""), 
                                        b = paste("ED_lag", i, sep=""), 
                                        c = paste("visit_date_new", i, sep=""), 
                                        d = paste("_ED_lag", i, sep=""))
                }
                for(i in 1:n) {
                        X <- get_errors(X, a = paste("start_date_new", i, sep=""), 
                                        b = paste("ED_lead", i, sep=""), 
                                        c = paste("visit_date_new", i, sep=""), 
                                        d = paste("_ED_lead", i, sep=""))
                }
                #VDs
                for(i in 1:n) {
                        X <- get_errors(X, a = paste("start_date_new", i, sep=""), 
                                        b = paste("end_date_new", i, sep=""), 
                                        c = paste("VD_lag", i, sep=""), 
                                        d = paste("_VD_lag", i, sep=""))
                }
                for(i in 1:n) {
                        X <- get_errors(X, a = paste("start_date_new", i, sep=""), 
                                        b = paste("end_date_new", i, sep=""), 
                                        c = paste("VD_lead", i, sep=""), 
                                        d = paste("_VD_lead", i, sep=""))
                }
                return(X)
        }
        
        
        dups5 <- ddply(dups5, 1, get_lags_leads_errors, n)
        
        replace_errors <- function(X, n) {
        for(i in 1:n) { 
        #replace SD_UP errors
        X$start_date_new <- ifelse(X$SD_UP_error == FALSE |
                                           is.na(X$SD_UP_error), 
                                   X$start_date_new, 
                                   ifelse(X$SD_UP_error == TRUE & 
                                                  (X[, paste("SD_UP_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_UP_error_SD_lag", i, sep="")])) &
                                                  (X[, paste("SD_DOB_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_DOB_error_SD_lag", i, sep="")])), 
                                          X[, paste("SD_lag", i, sep="")], 
                                          ifelse(X$SD_UP_error == TRUE & 
                                                         (X[, paste("SD_UP_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_UP_error_SD_lead", i, sep="")])) &
                                                         (X[, paste("SD_DOB_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_DOB_error_SD_lead", i, sep="")])),  
                                                 X[, paste("SD_lead", i, sep="")], X$start_date_new))) 
        }
        for(i in 1:n) { 
        #replace SD_DOB ERRORS
        X$start_date_new <- ifelse(X$SD_DOB_error == FALSE |
                                           is.na(X$SD_DOB_error), 
                                   X$start_date_new, 
                                   ifelse(X$SD_DOB_error == TRUE & 
                                                  (X[, paste("SD_DOB_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_DOB_error_SD_lag", i, sep="")])) & 
                                                  (X[, paste("SD_UP_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_UP_error_SD_lag", i, sep="")])), 
                                          X[, paste("SD_lag", i, sep="")], 
                                          ifelse(X$SD_DOB_error == TRUE & 
                                                         (X[, paste("SD_DOB_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_DOB_error_SD_lead", i, sep="")])) & 
                                                         (X[, paste("SD_UP_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_UP_error_SD_lead", i, sep="")])),
                                                 X[, paste("SD_lead", i, sep="")], X$start_date_new)))
        }
        for(i in 1:n) { 
        #replace ED_UP ERRORS
        X$end_date_new <- ifelse(X$ED_UP_error == FALSE |
                                         is.na(X$ED_UP_error), 
                                 X$end_date_new, 
                                 ifelse(X$ED_UP_error == TRUE & 
                                                (X[, paste("ED_UP_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_UP_error_ED_lag", i, sep="")])) & 
                                                (X[, paste("ED_DOB_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_DOB_error_ED_lag", i, sep="")])),  
                                        X[, paste("ED_lag", i, sep="")], 
                                        ifelse(X$ED_UP_error == TRUE & 
                                                       (X[, paste("ED_UP_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_UP_error_ED_lead", i, sep="")])) & 
                                                       (X[, paste("ED_DOB_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_DOB_error_ED_lead", i, sep="")])),  
                                               X[, paste("ED_lead", i, sep="")], X$end_date_new))) 
        }
        for(i in 1:n) { 
        #replace ED_DOB ERRORS
        X$end_date_new <- ifelse(X$ED_DOB_error == FALSE |
                                         is.na(X$ED_DOB_error), 
                                 X$end_date_new, 
                                 ifelse(X$ED_DOB_error == TRUE & 
                                                (X[, paste("ED_DOB_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_DOB_error_ED_lag", i, sep="")])) & 
                                                (X[, paste("ED_UP_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_UP_error_ED_lag", i, sep="")])), 
                                        X[, paste("ED_lag", i, sep="")], 
                                        ifelse(X$ED_DOB_error == TRUE & 
                                                       (X[, paste("ED_DOB_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_DOB_error_ED_lead", i, sep="")])) & 
                                                       (X[, paste("ED_UP_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_UP_error_ED_lead", i, sep="")])), 
                                               X[, paste("ED_lead", i, sep="")], X$end_date_new))) 
        }
        for(i in 1:n) { 
        #replace VVD_DOB ERRORS
        X$visit_date_new <- ifelse(X$VD_DOB_error == FALSE |
                                                 is.na(X$VD_DOB_error), 
                                         X$visit_date_new, 
                                         ifelse(X$VD_DOB_error == TRUE & 
                                                        (X[, paste("VD_DOB_error_VD_lag", i, sep="")] == FALSE &
                                                                 !is.na(X[, paste("VD_DOB_error_VD_lag", i, sep="")])), 
                                                X[, paste("VD_lag", i, sep="")], 
                                                ifelse(X$VD_DOB_error == TRUE & 
                                                               (X[, paste("VD_DOB_error_VD_lead", i, sep="")] == FALSE &
                                                                        !is.na(X[, paste("VD_DOB_error_VD_lead", i, sep="")])), 
                                                       X[, paste("VD_lead", i, sep="")], X$visit_date_new))) 
        }
        for(i in 1:n) { 
        #replace SD_VVD ERRORS
        X$start_date_new <- ifelse(X$SD_VD_error == FALSE |
                                           is.na(X$SD_VD_error), 
                                   X$start_date_new, 
                                   ifelse(X$SD_VD_error == TRUE & 
                                                  (X[, paste("SD_VD_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_VD_error_SD_lag", i, sep="")])) & 
                                                  (X[, paste("SD_UP_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_UP_error_SD_lag", i, sep="")])) &
                                                  (X[, paste("SD_DOB_error_SD_lag", i, sep="")] == FALSE &
                                                           !is.na(X[, paste("SD_DOB_error_SD_lag", i, sep="")])), 
                                          X[, paste("SD_lag", i, sep="")], 
                                          ifelse(X$SD_VD_error == TRUE & 
                                                         (X[, paste("SD_VD_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_VD_error_SD_lead", i, sep="")])) & 
                                                         (X[, paste("SD_UP_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_UP_error_SD_lead", i, sep="")])) &
                                                         (X[, paste("SD_DOB_error_SD_lead", i, sep="")] == FALSE &
                                                                  !is.na(X[, paste("SD_DOB_error_SD_lead", i, sep="")])), 
                                                 X[, paste("SD_lead", i, sep="")], X$start_date_new))) 
        }
        for(i in 1:n) { 
        #replace ED_VVD ERRORS
        X$end_date_new <- ifelse(X$ED_VD_error == FALSE |
                                         is.na(X$ED_VD_error), 
                                 X$end_date_new, 
                                 ifelse(X$ED_VD_error == TRUE & 
                                                (X[, paste("ED_VD_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_VD_error_ED_lag", i, sep="")])) & 
                                                (X[, paste("ED_UP_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_UP_error_ED_lag", i, sep="")])) &
                                                (X[, paste("ED_DOB_error_ED_lag", i, sep="")] == FALSE &
                                                         !is.na(X[, paste("ED_DOB_error_ED_lag", i, sep="")])), 
                                        X[, paste("ED_lag", i, sep="")], 
                                        ifelse(X$ED_VD_error == TRUE & 
                                                       (X[, paste("ED_VD_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_VD_error_ED_lead", i, sep="")])) &
                                                       (X[, paste("ED_UP_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_UP_error_ED_lead", i, sep="")])) &
                                                       (X[, paste("ED_DOB_error_ED_lead", i, sep="")] == FALSE &
                                                                !is.na(X[, paste("ED_DOB_error_ED_lead", i, sep="")])), 
                                               X[, paste("ED_lead", i, sep="")], X$end_date_new))) 
                        } 
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                return(X)
        }
        
        dups6 <- replace_errors(dups5, n)
        
        
        #Transform the date columns that have been edited back to human-readable format
        dups6$end_date_new <- as.Date(dups6$end_date_new, origin="1970-01-01")
        dups6$start_date_new <- as.Date(dups6$start_date_new, origin="1970-01-01")
        dups6$visit_date_new <- as.Date(dups6$visit_date_new, origin="1970-01-01")
        
        #find out numbers replaced
        sum(dups5$SD_UP_error, na.rm = TRUE) - sum(dups6$SD_UP_error, na.rm = TRUE) #40 replaced
        sum(dups5$SD_DOB_error, na.rm = TRUE) - sum(dups6$SD_DOB_error, na.rm = TRUE) #51 replaced
        sum(dups5$ED_UP_error, na.rm = TRUE) - sum(dups6$ED_UP_error, na.rm = TRUE) # 38 replaced
        sum(dups5$ED_DOB_error, na.rm = TRUE) - sum(dups6$ED_DOB_error, na.rm = TRUE) #108 replaced
        sum(dups5$VD_DOB_error, na.rm = TRUE) - sum(dups6$VD_DOB_error, na.rm = TRUE) #87 replaced
        sum(dups5$SD_VD_error, na.rm = TRUE) - sum(dups6$SD_VD_error, na.rm = TRUE) #13 replaced
        sum(dups5$ED_VD_error, na.rm = TRUE) - sum(dups6$ED_VD_error, na.rm = TRUE) #96 replaced
        
        #recreate duplicate columns to identify changes in the duplications
        dups6 <- get_duplications(dups6)
        sum(dups6$duplications) #1037 duplications
        sum(dups6$complete_duplications) #463 complete duplications have appeared 
        #now because of added missing data
        
        dups6 <- dups6 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_complete_dup_info(.)) %>%
                dplyr::ungroup()
        
        dups7 <- subset(dups6, (dups6$complete_duplications == FALSE) |
                                (dups6$complete_duplications == TRUE & dups6$last_observation_comp == TRUE))
        length(dups6$ID) - length(dups7$ID) #This removes 250 data entries 
        
        #recreate duplicate columns to identify changes in the duplications
        dups7 <- get_duplications(dups7)
        sum(dups7$duplications) #603 duplications
        sum(dups7$complete_duplications) #0 complete duplications 
        
        
#STEP 4 – CORRECT ANY ERRORS IN THE DUPLICATIONS
        
        get_alternative_dates <- function (X) {
                #work out the alternative dates for the SDs
                X$start_date_plus_day <-  as.Date(X$start_date_new) %m+% days(1)
                X$start_date_minus_day <-  as.Date(X$start_date_new) %m+% days(-1)
                X$start_date_plus_week <-  as.Date(X$start_date_new) %m+% weeks(1)
                X$start_date_minus_week <-  as.Date(X$start_date_new) %m+% weeks(-1)
                X$start_date_plus_month <- as.Date(X$start_date_new) %m+% months(1)
                X$start_date_minus_month <- as.Date(X$start_date_new) %m+% months(-1)
                X$start_date_plus_year <- as.Date(X$start_date_new) %m+% years(1)
                X$start_date_minus_year <- as.Date(X$start_date_new) %m+% years(-1)       
                #work out the alternative dates for the EDs
                X$end_date_plus_day <-  as.Date(X$end_date_new) %m+% days(1)
                X$end_date_minus_day <-  as.Date(X$end_date_new) %m+% days(-1)
                X$end_date_plus_week <-  as.Date(X$end_date_new) %m+% weeks(1)
                X$end_date_minus_week <-  as.Date(X$end_date_new) %m+% weeks(-1)
                X$end_date_plus_month <- as.Date(X$end_date_new) %m+% months(1)
                X$end_date_minus_month <- as.Date(X$end_date_new) %m+% months(-1)
                X$end_date_plus_year <- as.Date(X$end_date_new) %m+% years(1)
                X$end_date_minus_year <- as.Date(X$end_date_new) %m+% years(-1)
                #work out the alternative dates for the VDs
                X$visit_date_plus_day <-  as.Date(X$visit_date_new) %m+% days(1)
                X$visit_date_minus_day <-  as.Date(X$visit_date_new) %m+% days(-1)
                X$visit_date_plus_week <-  as.Date(X$visit_date_new) %m+% weeks(1)
                X$visit_date_minus_week <-  as.Date(X$visit_date_new) %m+% weeks(-1)
                X$visit_date_plus_month <- as.Date(X$visit_date_new) %m+% months(1)
                X$visit_date_minus_month <- as.Date(X$visit_date_new) %m+% months(-1)
                X$visit_date_plus_year <- as.Date(X$visit_date_new) %m+% years(1)
                X$visit_date_minus_year <- as.Date(X$visit_date_new) %m+% years(-1)
                return(X)
        }
        
        dups8 <- subset(dups7, select = 1:(which("last_observation" == colnames(dups7))-1))
        dups8 <- get_alternative_dates(dups8)
        
        get_alternative_dates_errors <- function(X){
                #start_dates
                X <- get_errors(X, a = "start_date_minus_day", b = "end_date_new", c = "visit_date_new", d = "_SD_minus_day")
                X <- get_errors(X, a = "start_date_minus_week", b = "end_date_new", c = "visit_date_new",  d = "_SD_minus_week")
                X <- get_errors(X, a = "start_date_minus_month", b = "end_date_new", c = "visit_date_new",  d = "_SD_minus_month")
                X <- get_errors(X, a = "start_date_minus_year", b = "end_date_new", c = "visit_date_new",  d = "_SD_minus_year")
                X <- get_errors(X, a = "start_date_plus_day", b = "end_date_new", c = "visit_date_new",  d = "_SD_plus_day")
                X <- get_errors(X, a = "start_date_plus_week", b = "end_date_new", c = "visit_date_new",  d = "_SD_plus_week")
                X <- get_errors(X, a = "start_date_plus_month", b = "end_date_new", c = "visit_date_new",  d = "_SD_plus_month")
                X <- get_errors(X, a = "start_date_plus_year", b = "end_date_new", c = "visit_date_new",  d = "_SD_plus_year")
                #end_dates
                X <- get_errors(X, a = "start_date_new", b = "end_date_minus_day", c = "visit_date_new", d = "_ED_minus_day")
                X <- get_errors(X, a = "start_date_new", b = "end_date_minus_week", c = "visit_date_new",  d = "_ED_minus_week")
                X <- get_errors(X, a = "start_date_new", b = "end_date_minus_month", c = "visit_date_new",  d = "_ED_minus_month")
                X <- get_errors(X, a = "start_date_new", b = "end_date_minus_year", c = "visit_date_new",  d = "_ED_minus_year")
                X <- get_errors(X, a = "start_date_new", b = "end_date_plus_day", c = "visit_date_new",  d = "_ED_plus_day")
                X <- get_errors(X, a = "start_date_new", b = "end_date_plus_week", c = "visit_date_new",  d = "_ED_plus_week")
                X <- get_errors(X, a = "start_date_new", b = "end_date_plus_month", c = "visit_date_new",  d = "_ED_plus_month")
                X <- get_errors(X, a = "start_date_new", b = "end_date_plus_year", c = "visit_date_new",  d = "_ED_plus_year")
                #visit_dates
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_minus_day", d = "_VD_minus_day")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_minus_week",  d = "_VD_minus_week")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_minus_month",  d = "_VD_minus_month")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_minus_year",  d = "_VD_minus_year")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_plus_day",  d = "_VD_plus_day")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_plus_week",  d = "_VD_plus_week")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_plus_month",  d = "_VD_plus_month")
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_plus_year",  d = "_VD_plus_year")
        }
        dups8 <- get_errors(dups8, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
        dups8 <- get_alternative_dates_errors(dups8)
        
        #SD_UP_errors
        correct_SD_UP_errors <- function (X) {
                print("Total SD_UP_errors")
                print(sum(X$SD_UP_error, na.rm = TRUE)) 
                X$start_date_new <- ifelse((X$SD_UP_error == TRUE & !is.na(X$SD_UP_error)) &
                                                   (X$SD_UP_error_SD_minus_day == FALSE & !is.na(X$SD_UP_error_SD_minus_day)) &
                                                   (X$SD_DOB_error_SD_minus_day !=TRUE),
                                           X$start_date_minus_day, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_UP_errors after corrections by day")
                print(sum(X$SD_UP_error, na.rm = TRUE))  
                X$start_date_new <- ifelse((X$SD_UP_error == TRUE & !is.na(X$SD_UP_error)) &
                                                   (X$SD_UP_error_SD_minus_week == FALSE & !is.na(X$SD_UP_error_SD_minus_week)) &
                                                   (X$SD_DOB_error_SD_minus_week !=TRUE),
                                           X$start_date_minus_week, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_UP_errors after corrections by week")
                print(sum(X$SD_UP_error, na.rm = TRUE))  
                X$start_date_new <- ifelse((X$SD_UP_error == TRUE & !is.na(X$SD_UP_error)) &
                                                   (X$SD_UP_error_SD_minus_month == FALSE & !is.na(X$SD_UP_error_SD_minus_month)) &
                                                   (X$SD_DOB_error_SD_minus_month !=TRUE),
                                           X$start_date_minus_month, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_UP_errors after corrections by month")
                print(sum(X$SD_UP_error, na.rm = TRUE)) 
                X$start_date_new <- ifelse((X$SD_UP_error == TRUE & !is.na(X$SD_UP_error)) &
                                                   (X$SD_UP_error_SD_minus_year == FALSE & !is.na(X$SD_UP_error_SD_minus_year)) &
                                                   (X$SD_DOB_error_SD_minus_year !=TRUE),
                                           X$start_date_minus_year, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_UP_errors after corrections by year")
                print(sum(X$SD_UP_error, na.rm = TRUE))     
                X$start_date_new <- ifelse(X$SD_UP_error == TRUE & !is.na(X$SD_UP_error), NA, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_UP_errors after deletions")
                print(sum(X$SD_UP_error, na.rm = TRUE))    
                return(X)
        }
        
        #SD_DOB_errors
        correct_SD_DOB_errors <- function (X) {
                print("Total SD_DOB_errors")
                print(sum(X$SD_DOB_error, na.rm = TRUE)) 
                X$start_date_new <- ifelse((X$SD_DOB_error == TRUE & !is.na(X$SD_DOB_error)) & 
                                                   (X$SD_DOB_error_SD_plus_day == FALSE & !is.na(X$SD_DOB_error_SD_plus_day)) &
                                                   (X$SD_UP_error_SD_plus_day != TRUE),
                                           X$start_date_plus_day, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_DOB_errors after corrections by day")
                print(sum(X$SD_DOB_error, na.rm = TRUE))  
                X$start_date_new <- ifelse((X$SD_DOB_error == TRUE & !is.na(X$SD_DOB_error)) & 
                                                   (X$SD_DOB_error_SD_plus_week == FALSE & !is.na(X$SD_DOB_error_SD_plus_week)) &
                                                   (X$SD_UP_error_SD_plus_week != TRUE),
                                           X$start_date_plus_week, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_DOB_errors after corrections by week")
                print(sum(X$SD_DOB_error, na.rm = TRUE))  
                X$start_date_new <- ifelse((X$SD_DOB_error == TRUE & !is.na(X$SD_DOB_error)) & 
                                                   (X$SD_DOB_error_SD_plus_month == FALSE & !is.na(X$SD_DOB_error_SD_plus_month)) &
                                                   (X$SD_UP_error_SD_plus_month !=TRUE),
                                           X$start_date_plus_month, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_DOB_errors after corrections by month")
                print(sum(X$SD_DOB_error, na.rm = TRUE)) 
                X$start_date_new <- ifelse((X$SD_DOB_error == TRUE & !is.na(X$SD_DOB_error)) & 
                                                   (X$SD_DOB_error_SD_plus_year == FALSE & !is.na(X$SD_DOB_error_SD_plus_year)) &
                                                   (X$SD_UP_error_SD_plus_year !=TRUE),
                                           X$start_date_plus_year, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_DOB_errors after corrections by year")
                print(sum(X$SD_DOB_error, na.rm = TRUE))     
                X$start_date_new <- ifelse(X$SD_DOB_error == TRUE & !is.na(X$SD_DOB_error), NA, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_DOB_errors after deletions")
                print(sum(X$SD_DOB_error, na.rm = TRUE))    
                return(X)
        }
        
        #ED_UP_errors
        correct_ED_UP_errors <- function (X) {
                print("Total ED_UP_errors")
                print(sum(X$ED_UP_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse((X$ED_UP_error == TRUE & !is.na(X$ED_UP_error)) &
                                                 (X$ED_UP_error_ED_minus_day == FALSE & !is.na(X$ED_UP_error_ED_minus_day)) &
                                                 (X$ED_DOB_error_ED_minus_day !=TRUE),
                                         X$end_date_minus_day, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_UP_errors after corrections by day")
                print(sum(X$ED_UP_error, na.rm = TRUE))  
                X$end_date_new <- ifelse((X$ED_UP_error == TRUE & !is.na(X$ED_UP_error)) &
                                                 (X$ED_UP_error_ED_minus_week == FALSE & !is.na(X$ED_UP_error_ED_minus_week)) &
                                                 (X$ED_DOB_error_ED_minus_week !=TRUE),
                                         X$end_date_minus_week, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_UP_errors after corrections by week")
                print(sum(X$ED_UP_error, na.rm = TRUE))  
                X$end_date_new <- ifelse((X$ED_UP_error == TRUE & !is.na(X$ED_UP_error)) &
                                                 (X$ED_UP_error_ED_minus_month == FALSE & !is.na(X$ED_UP_error_ED_minus_month)) &
                                                 (X$ED_DOB_error_ED_minus_month !=TRUE),
                                         X$end_date_minus_month, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_UP_errors after corrections by month")
                print(sum(X$ED_UP_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse((X$ED_UP_error == TRUE & !is.na(X$ED_UP_error)) &
                                                 (X$ED_UP_error_ED_minus_year == FALSE & !is.na(X$ED_UP_error_ED_minus_year)) &
                                                 (X$ED_DOB_error_ED_minus_year !=TRUE),
                                         X$end_date_minus_year, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_UP_errors after corrections by year")
                print(sum(X$ED_UP_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse(X$ED_UP_error == TRUE & !is.na(X$ED_UP_error), NA, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_UP_errors after deletions")
                print(sum(X$ED_UP_error, na.rm = TRUE))    
                return(X)
        }
        
        
        #ED_DOB_errors
        correct_ED_DOB_errors <- function (X) {
                print("Total ED_DOB_errors")
                print(sum(X$ED_DOB_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse((X$ED_DOB_error == TRUE & !is.na(X$ED_DOB_error)) & 
                                                 (X$ED_DOB_error_ED_plus_day == FALSE & !is.na(X$ED_DOB_error_ED_plus_day)) &
                                                 (X$ED_UP_error_ED_plus_day !=TRUE),
                                         X$end_date_plus_day, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_DOB_errors after corrections by day")
                print(sum(X$ED_DOB_error, na.rm = TRUE))  
                X$end_date_new <- ifelse((X$ED_DOB_error == TRUE & !is.na(X$ED_DOB_error)) & 
                                                 (X$ED_DOB_error_ED_plus_week == FALSE & !is.na(X$ED_DOB_error_ED_plus_week)) &
                                                 (X$ED_UP_error_ED_plus_week !=TRUE),
                                         X$end_date_plus_week, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_DOB_errors after corrections by week")
                print(sum(X$ED_DOB_error, na.rm = TRUE))  
                X$end_date_new <- ifelse((X$ED_DOB_error == TRUE & !is.na(X$ED_DOB_error)) & 
                                                 (X$ED_DOB_error_ED_plus_month == FALSE & !is.na(X$ED_DOB_error_ED_plus_month)) &
                                                 (X$ED_UP_error_ED_plus_month !=TRUE),
                                         X$end_date_plus_month, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_DOB_errors after corrections by month")
                print(sum(X$ED_DOB_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse((X$ED_DOB_error == TRUE & !is.na(X$ED_DOB_error)) & 
                                                 (X$ED_DOB_error_ED_plus_year == FALSE & !is.na(X$ED_DOB_error_ED_plus_year)) &
                                                 (X$ED_UP_error_ED_plus_year !=TRUE),
                                         X$end_date_plus_year, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_DOB_errors after corrections by year")
                print(sum(X$ED_DOB_error, na.rm = TRUE))     
                #delete errors that can't be corrected
                X$end_date_new <- ifelse(X$ED_DOB_error == TRUE & !is.na(X$ED_DOB_error), NA, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_DOB_errors after deletions")
                print(sum(X$ED_DOB_error, na.rm = TRUE))    
                return(X)
        }
        
        #VD_DOB_errors
        correct_VD_DOB_errors <- function (X) {
                print("Total VD_DOB_errors")
                print(sum(X$VD_DOB_error, na.rm = TRUE)) 
                X$visit_date_new <- ifelse((X$VD_DOB_error == TRUE & !is.na(X$VD_DOB_error)) & 
                                                         (X$VD_DOB_error_VD_plus_day == FALSE & !is.na(X$VD_DOB_error_VD_plus_day)),
                                                 X$visit_date_plus_day, X$visit_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("VD_DOB_errors after corrections by day")
                print(sum(X$VD_DOB_error, na.rm = TRUE))  
                X$visit_date_new <- ifelse((X$VD_DOB_error == TRUE & !is.na(X$VD_DOB_error)) & 
                                                         (X$VD_DOB_error_VD_plus_week == FALSE & !is.na(X$VD_DOB_error_VD_plus_week)),
                                                 X$visit_date_plus_week, X$visit_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("VD_DOB_errors after corrections by week")
                print(sum(X$VD_DOB_error, na.rm = TRUE))  
                X$visit_date_new <- ifelse((X$VD_DOB_error == TRUE & !is.na(X$VD_DOB_error)) & 
                                                         (X$VD_DOB_error_VD_plus_month == FALSE & !is.na(X$VD_DOB_error_VD_plus_month)),
                                                 X$visit_date_plus_month, X$visit_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("VD_DOB_errors after corrections by month")
                print(sum(X$VD_DOB_error, na.rm = TRUE)) 
                
                X$visit_date_new <- ifelse((X$VD_DOB_error == TRUE & !is.na(X$VD_DOB_error)) & 
                                                         (X$VD_DOB_error_VD_plus_year == FALSE & !is.na(X$VD_DOB_error_VD_plus_year)),
                                                 X$visit_date_plus_year, X$visit_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("VD_DOB_errors after corrections by year")
                print(sum(X$VD_DOB_error, na.rm = TRUE))     
                
                #delete errors that can't be corrected
                X$visit_date_new <- ifelse(X$VD_DOB_error == TRUE & !is.na(X$VD_DOB_error), NA, X$visit_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("VD_DOB_errors after deletions")
                print(sum(X$VD_DOB_error, na.rm = TRUE))    
                return(X)
        }
        
        #SD_VD_errors
        correct_SD_VD_errors <- function (X) {
                print("Total SD_VD_errors")
                print(sum(X$SD_VD_error, na.rm = TRUE)) 
                X$start_date_new <- ifelse((X$SD_VD_error == TRUE & !is.na(X$SD_VD_error)) & 
                                                   (X$SD_VD_error_SD_minus_day == FALSE & !is.na(X$SD_VD_error_SD_minus_day)) &
                                                   (X$SD_UP_error_SD_minus_day !=TRUE) &
                                                   (X$SD_DOB_error_SD_minus_day !=TRUE),
                                           X$start_date_minus_day, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_VD_errors after corrections by day")
                print(sum(X$SD_VD_error, na.rm = TRUE))  
                X$start_date_new <- ifelse((X$SD_VD_error == TRUE & !is.na(X$SD_VD_error)) & 
                                                   (X$SD_VD_error_SD_minus_week == FALSE & !is.na(X$SD_VD_error_SD_minus_week)) &
                                                   (X$SD_UP_error_SD_minus_week !=TRUE) &
                                                   (X$SD_DOB_error_SD_minus_week !=TRUE),
                                           X$start_date_minus_week, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_VD_errors after corrections by week")
                print(sum(X$SD_VD_error, na.rm = TRUE))  
                X$start_date_new <- ifelse((X$SD_VD_error == TRUE & !is.na(X$SD_VD_error)) & 
                                                   (X$SD_VD_error_SD_minus_month == FALSE & !is.na(X$SD_VD_error_SD_minus_month)) &
                                                   (X$SD_UP_error_SD_minus_month !=TRUE) &
                                                   (X$SD_DOB_error_SD_minus_month !=TRUE),
                                           X$start_date_minus_month, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_VD_errors after corrections by month")
                print(sum(X$SD_VD_error, na.rm = TRUE)) 
                X$start_date_new <- ifelse((X$SD_VD_error == TRUE & !is.na(X$SD_VD_error)) & 
                                                   (X$SD_VD_error_SD_minus_year == FALSE & !is.na(X$SD_VD_error_SD_minus_year)) &
                                                   (X$SD_UP_error_SD_minus_year !=TRUE) &
                                                   (X$SD_DOB_error_SD_minus_year !=TRUE),
                                           X$start_date_minus_year, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_VD_errors after corrections by year")
                print(sum(X$SD_VD_error, na.rm = TRUE))     
                X$start_date_new <- ifelse(X$SD_VD_error == TRUE & !is.na(X$SD_VD_error), NA, X$start_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("SD_VD_errors after deletions")
                print(sum(X$SD_VD_error, na.rm = TRUE))    
                return(X)
        }
        
        #ED_VD_errors
        correct_ED_VD_errors <- function (X) {
                print("Total ED_VD_errors")
                print(sum(X$ED_VD_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse((X$ED_VD_error == TRUE & !is.na(X$ED_VD_error)) &  
                                                 (X$ED_VD_error_ED_plus_day = FALSE & !is.na(X$ED_VD_error_ED_plus_day)) &
                                                 (X$ED_UP_error_ED_plus_day !=TRUE) &
                                                 (X$ED_DOB_error_ED_plus_day !=TRUE),
                                         X$end_date_plus_day, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_VD_errors after corrections by day")
                print(sum(X$ED_VD_error, na.rm = TRUE))  
                X$end_date_new <- ifelse((X$ED_VD_error == TRUE & !is.na(X$ED_VD_error)) & 
                                                 (X$ED_VD_error_ED_plus_week = FALSE & !is.na(X$ED_VD_error_ED_plus_week)) &
                                                 (X$ED_UP_error_ED_plus_week !=TRUE) &
                                                 (X$ED_DOB_error_ED_plus_week !=TRUE),
                                         X$end_date_plus_week, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_VD_errors after corrections by week")
                print(sum(X$ED_VD_error, na.rm = TRUE))  
                X$end_date_new <- ifelse((X$ED_VD_error == TRUE & !is.na(X$ED_VD_error)) & 
                                                 (X$ED_VD_error_ED_plus_month = FALSE & !is.na(X$ED_UP_error_ED_plus_month)) &
                                                 (X$ED_UP_error_ED_plus_month !=TRUE) &
                                                 (X$ED_DOB_error_ED_plus_month !=TRUE),
                                         X$end_date_plus_month, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_VD_errors after corrections by month")
                print(sum(X$ED_VD_error, na.rm = TRUE)) 
                X$end_date_new <- ifelse((X$ED_VD_error == TRUE & !is.na(X$ED_VD_error)) & 
                                                 (X$ED_UP_error_ED_plus_year = FALSE & !is.na(X$ED_UP_error_ED_plus_year)) &
                                                 (X$ED_UP_error_ED_plus_year !=TRUE) &
                                                 (X$ED_DOB_error_ED_plus_year !=TRUE),
                                         X$end_date_plus_year, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_VD_errors after corrections by year")
                print(sum(X$ED_VD_error, na.rm = TRUE))     
                X$end_date_new <- ifelse(X$ED_VD_error == TRUE & !is.na(X$ED_VD_error), NA, X$end_date_new)
                X <- get_errors(X, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
                X <- get_alternative_dates_errors(X)
                print("ED_VD_errors after deletions")
                print(sum(X$ED_VD_error, na.rm = TRUE))    
                return(X)
        }
        
        dups8 <- correct_SD_UP_errors(dups8)
        dups8 <- correct_SD_DOB_errors(dups8)
        dups8 <- correct_ED_UP_errors(dups8)
        dups8 <- correct_ED_DOB_errors(dups8)
        dups8 <- correct_VD_DOB_errors(dups8)
        dups8 <- correct_SD_VD_errors(dups8)
        dups8 <- correct_ED_VD_errors(dups8)
        
        dups8 <- subset(dups8, select = 1:(which("start_date_plus_day" == colnames(dups8))-1))
        
        #Transform the date columns that have been edited back to human-readable format
        dups8$end_date_new <- as.Date(dups8$end_date_new, origin="1970-01-01")
        dups8$start_date_new <- as.Date(dups8$start_date_new, origin="1970-01-01")
        dups8$visited_vet_date_new <- as.Date(dups8$visit_date_new, origin="1970-01-01")
        
        #recreate duplicate columns to identify changes in the duplications
        dups8 <- get_duplications(dups8)
        sum(dups8$duplications) #603 duplications
        sum(dups8$complete_duplications) #246 complete duplications have appeared 
        #now because of added missing data
        
        dups8 <- dups8 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_complete_dup_info(.)) %>%
                dplyr::ungroup()
        
        dups9 <- subset(dups8, (dups8$complete_duplications == FALSE) |
                                (dups8$complete_duplications == TRUE & dups8$last_observation_comp == TRUE))
        length(dups8$ID) - length(dups9$ID) #This removes 157 data entries 
        
        #recreate duplicate columns to identify changes in the duplications
        dups9 <- get_duplications(dups9)
        sum(dups9$duplications) #370 duplications
        sum(dups9$complete_duplications) #0 complete duplications 
        
        
#STEP 5 – WHEN THERE ARE DUPLICATIONS THAT ARE NOT ERRORS/MISSING KEEP THE MOST RECENT DATA ENTRY
        
        get_most_recent_dups <- function(X){
                X <- X[order(X$ID), ]
                X$most_recent_start_date <- tail(X$start_date_new, 1)
                X$most_recent_end_date <- tail(X$end_date_new, 1)
                X$most_recent_visit_date <- tail(X$visit_date_new, 1)
                X$any_duplications <- any(X$duplications == TRUE)
                return(X)
        }
        
        dups8 <- dups8 %>%
                dplyr::group_by(ID) %>%
                dplyr::do(get_most_recent_dups(.)) %>%
                dplyr::ungroup()
        
        replace_duplicate_data <- function(X, n) {
                #replace duplicate data in the start dates
                X$start_date_new <- ifelse(X$any_duplications == TRUE,
                                           X$most_recent_start_date, X$start_date_new)
                #replace duplicate data in the end dates
                X$end_date_new <- ifelse(X$any_duplications == TRUE,
                                         X$most_recent_end_date, X$end_date_new)
                #replace duplicate data in the visited vet dates
                X$visit_date_new <- ifelse(X$any_duplications == TRUE,
                                                 X$most_recent_visit_date, X$visit_date_new)
                return(X)
        }
        
        dups9 <- replace_duplicate_data(dups8)
        dups9$end_date_new <- as.Date(dups9$end_date_new, origin="1970-01-01")
        dups9$start_date_new <- as.Date(dups9$start_date_new, origin="1970-01-01")
        dups9$visited_vet_date_new <- as.Date(dups9$visited_vet_date_new, origin="1970-01-01")
        
        #recreate duplicate columns to identify changes in the duplications
        dups9 <- get_duplications(dups9)
        sum(dups9$duplications) #603 duplications
        sum(dups9$complete_duplications) #603 complete duplications have appeared 
        #now because of added missing data
        
        dups9 <- dups9 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_complete_dup_info(.)) %>%
                dplyr::ungroup()
        
        dups10 <- subset(dups9, (dups9$complete_duplications == FALSE) |
                                (dups9$complete_duplications == TRUE & dups9$last_observation_comp == TRUE))
        length(dups9$ID) - length(dups10$ID) #This removes 358 data entries 
        
        #recreate duplicate columns to identify changes in the duplications
        dups10 <- get_duplications(dups10)
        sum(dups10$duplications) #0 duplications
        sum(dups10$complete_duplications) #0 complete duplications 
       
        #THIS IS THE FULLY CLEANED DUPLICATE DATA!
        
#STEP 7 OF CLEANING – CORRECT OR DELETE ANY ERRORS IN THE DATA THAT IS NOT DUPLICATED          
        
        not_dups2 <- get_alternative_dates(not_dups)
        not_dups2 <- get_errors(not_dups2, a = "start_date_new", b = "end_date_new", c = "visit_date_new", d = "")
        not_dups2 <- get_alternative_dates_errors(not_dups2)
        not_dups2 <- correct_SD_UP_errors(not_dups2)
        not_dups2 <- correct_SD_DOB_errors(not_dups2)
        not_dups2 <- correct_ED_UP_errors(not_dups2)
        not_dups2 <- correct_ED_DOB_errors(not_dups2)
        not_dups2 <- correct_VD_DOB_errors(not_dups2)
        not_dups2 <- correct_SD_VD_errors(not_dups2)
        not_dups2 <- correct_ED_VD_errors(not_dups2)
        
        not_dups2 <- subset(not_dups2, select = 1:(which("start_date_plus_day" == colnames(not_dups2))-1))
        
        #Transform the date columns that have been edited back to human-readable format
        not_dups2$end_date_new <- as.Date(not_dups2$end_date_new, origin="1970-01-01")
        not_dups2$start_date_new <- as.Date(not_dups2$start_date_new, origin="1970-01-01")
        not_dups2$visit_date_new <- as.Date(not_dups2$visit_date_new, origin="1970-01-01")
        #START DATES
        sum(is.na(not_dups$start_date_new)) #
        sum(is.na(not_dups2$start_date_new)) #
        #END DATES
        sum(is.na(not_dups$end_date_new)) #
        sum(is.na(not_dups2$end_date_new)) #
        #VISITED VET DATES
        sum(is.na(not_dups$visit_date_new)) #
        sum(is.na(not_dups2$visit_date_new)) #   
        