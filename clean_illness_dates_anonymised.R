#FILENAME: clean_illness_dates_anonymised.R
#AUTHOR: Charlotte Woolley
#DESCRIPTION: This code was originally written in order to clean owner reported start and end 
#dates for illnesses in dogs enrolled in the Dogslife project
#it has been anonymised and adapted to be used on an example dataset 
#PACKAGES NEEDED:

        library(tidyverse)
        library(data.table)

#PRELIMINARY DATA ORGANISATION/EXPLORATION
        
        #read in data and format 
        dat <- read_csv('example_dates_data.csv')
        dat2 <- dat[,2:7] 
        dat2 <- dat2 %>%
                mutate(ID = as.factor(ID),
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
        
        
        dups4 <- replace_missing_data(dups3, n) 
        
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
        sum(dups4$complete_duplications) #732 complete duplications have appeared 
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
                #VVDs
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
        #START DATES
        sum(dups5$SD_UP_error, na.rm = TRUE) #
        sum(dups6$SD_UP_error, na.rm = TRUE) #
        sum(dups5$SD_DOB_error, na.rm = TRUE) #
        sum(dups6$SD_DOB_error, na.rm = TRUE) #
        sum(dups5$SD_VD_error, na.rm = TRUE) #
        sum(dups6$SD_VD_error, na.rm = TRUE) #
        #END DATES
        sum(dups5$ED_UP_error, na.rm = TRUE) #
        sum(dups6$ED_UP_error, na.rm = TRUE) #
        sum(dups5$ED_DOB_error, na.rm = TRUE) #
        sum(dups6$ED_DOB_error, na.rm = TRUE) #
        sum(dups5$ED_VD_error, na.rm = TRUE) #
        sum(dups6$ED_VD_error, na.rm = TRUE) #
        #VISITED VET DATES
        sum(dups5$VD_DOB_error, na.rm = TRUE) #
        sum(dups6$VD_DOB_error, na.rm = TRUE) #
        