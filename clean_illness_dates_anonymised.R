#FILENAME: clean_illness_dates_anonymised.R
#AUTHOR: Charlotte Woolley
#DESCRIPTION: This code was originally written in order to clean owner reported
#start and end dates for illnesses and for veterinary visit dates for dogs 
#enrolled in the Dogslife project
#it has been anonymised, generalised and adapted to be used on an example dataset 
#The example dataset can be generated using the file "example_data_generation"
#or can be imported directly from "example_dates_data.csv"

#PACKAGES NEEDED:

        library(tidyverse)
        library(lubridate)

#PRELIMINARY DATA ORGANISATION/EXPLORATION

#read in data and format 

        dat <- read_csv('example_dates_data.csv')
        dat2 <- dat[,2:7] #removes the 'X1' column
        dat2 <- dat2 %>%
                mutate(ID = as.factor(ID),
                       start_date_new = start_date,
                       end_date_new = end_date,
                       visit_date_new = visit_date) 
        dat2 <- dat2[order(dat2$ID), ]
        
#function that creates columns to identify duplications in the ID and to
#identify whether the entire data entry has been completely duplicated. It
#Then prints how may of each type of duplications there are
        get_duplications <- function(X) {
                X <- X %>%
                mutate(duplications = (duplicated(ID) | duplicated(ID, 
                                                           fromLast = TRUE)),
                               complete_dups_ID = group_indices_(X, 
                .dots=c("ID", "start_date_new", "end_date_new", "visit_date_new")),
               complete_duplications = (duplicated(complete_dups_ID) | 
                                                duplicated(complete_dups_ID, 
                                                           fromLast = TRUE)))
                print(c("Duplications", sum(X$duplications)))
                print(c("Complete Duplications", sum(X$complete_duplications)))
                return(X)
        }

        dat2 <- get_duplications(dat2)

#seperate the subsets of duplicated and non-duplicated data so that 
#duplicates can be cleaned first
        dups <- subset(dat2, dat2$duplications == TRUE)  #2392 duplications
        not_dups <- subset(dat2, dat2$duplications == FALSE) #9026 not duplicated

#STEP 1 OF CLEANING – REMOVE THE COMPLETE DUPLICATIONS  

#get_dup_info finds the last duplicate in the data (i.e the most recently entered 
#entry)
        get_dup_info <- function(X){
                X <- X[order(X$ID), ]
                observation <- 1:nrow(X) 
                X$last_obs_num <- tail(observation, 1)
                X$last_observation <- X$last_obs_num == observation
                return(X)
        }
#Apply this function to each group of complete duplicates
        dups <- dups %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()

#Make a new dataset that contains only the most recent entry from complete 
#duplicates and deletes all older complete duplicates        
        dups2 <- subset(dups, (dups$complete_duplications == FALSE) |
                                (dups$complete_duplications == TRUE & 
                                         dups$last_observation == TRUE))
        length(dups$ID) - length(dups2$ID) #This removes 435 data entries 

#recreate duplicate columns to identify changes in the duplications
        dups2 <- get_duplications(dups2)
        
#STEP 2 OF CLEANING – REPLACE MISSING DATA WITH DATA IN OTHER DUPLICATE ROWS 

        dups3 <- dups2 %>%
                dplyr::group_by(ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()

        n <- (max(dups3$last_obs_num))-1 # Find the mmaximum number of duplicates
        n #There are a maximum of n(10) duplicates

#get_lags leads generates n (10) number of columns with the previous and following 
#data entries for the duplicated start dates, end dates and visit dates.
#Default date columns and names for lagging/leading columns can be changed

        get_lags_leads <- function(X, n, var1 = "start_date_new", 
                           var2 = "end_date_new", var3 = "visit_date_new", 
                           var1_lag_name = "SD_lag", var2_lag_name = "ED_lag", 
                           var3_lag_name = "VD_lag", var1_lead_name = "SD_lead",
                           var2_lead_name = "ED_lead", var3_lead_name = "VD_lead") {
                for(i in 1:n) {
                        X[, paste(var1_lag_name, i, sep = "")] <- lag(X[[var1]], i)
                        X[, paste(var2_lag_name, i, sep = "")] <- lag(X[[var2]], i)
                        X[, paste(var3_lag_name, i, sep = "")] <- lag(X[[var3]], i)
                        X[, paste(var1_lead_name, i, sep = "")] <- lead(X[[var1]], i)
                        X[, paste(var2_lead_name, i, sep = "")] <- lead(X[[var2]], i)
                        X[, paste(var3_lead_name, i, sep = "")] <- lead(X[[var3]], i)
                }
                return(X)
        }

#Apply this function to each group of duplicates
        dups3 <- dups3 %>%
                dplyr::group_by(ID) %>%
                dplyr::do(get_lags_leads(., n)) %>%
                dplyr::ungroup()
        
#replace_missing_dates replaces missing data (NAs) with the previous and following 
#data entries for the duplicated start dates, end dates and visit dates.
#var 1 is the original column, var 2 is the lagging column and var 3 is the leading 
#column. The function iterates over each number of n to sensure that each lagging 
#and leading column is checked, starting at the closest column to the missing data
#It chooses the nearest non-missing value to replace the missing value. Lagging 
#values are chosen before leding values.
#E.g if there was a missing value in the 3rd row of a set of 5 duplicates,
#the 2nd row would be checked first, then the 4th, the 1st and finally the 5th.
#Print results allows you to print out the changes as they are made at each step
        replace_missing_dates <- function(X, n, var1, var2, var3, 
                                          print_results = TRUE) {
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(var1, " NAs", 
                                     sep =""), "before replacements"))
                        a <- (sum(is.na(X[[var1]]))) 
                        print(a) }
                for(i in 1:n) {
                        X[[var1]] <- ifelse(!is.na(X[[var1]]), X[[var1]], 
                                    ifelse(is.na(X[[var1]]) & 
                                           !is.na(X[[paste(var2, i, sep="")]]), 
                                   X[[paste(var2, i, sep="")]],
                                   ifelse(is.na(X[[var1]]) & 
                                          !is.na(X[[paste(var3, i, sep="")]]), 
                                  X[[paste(var3, i, sep="")]], X[[var1]])))
                #This prints out results after each iteration of 'n' so
                        #it can be clearly visualised where the corrections are made
                        if(print_results == TRUE) {
                        print(paste("Total number of ", paste(var1, " NAs", 
                                      sep =""), " after n = ", i, sep=""))
                                print(sum(is.na(X[[var1]]))) 
                        }
                }
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(var1, " NAs", 
                                     sep =""), "replaced"))
                        b <- (sum(is.na(X[[var1]]))) 
                        print(a-b)
                }
                return(X)
        }
        
        #Apply the function to each date column as needed
        dups4 <- replace_missing_dates(dups3, n, "start_date_new", "SD_lag", 
                                       "SD_lead")
        dups4 <- replace_missing_dates(dups4, n, "end_date_new", "ED_lag", 
                                       "ED_lead", print_results = FALSE)
        dups4 <- replace_missing_dates(dups4, n, "visit_date_new", "VD_lag", 
                                       "VD_lead", print_results = FALSE)
        
#Transform the date columns that have been edited back to human-readable format
        dups4$end_date_new <- as.Date(dups4$end_date_new, origin="1970-01-01")
        dups4$start_date_new <- as.Date(dups4$start_date_new, origin="1970-01-01")
        dups4$visit_date_new <- as.Date(dups4$visit_date_new, origin="1970-01-01")

#recreate duplicate columns to identify changes in the duplications
        dups4 <- get_duplications(dups4)
        
#reapply get_dup_info to find the last complete duplicates in the data  
        dups4 <- dups4 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()
        
#Make a new dataset that contains only the most recent entry from complete 
#duplicates and deletes all older complete duplicates        
        dups5 <- subset(dups4, (dups4$complete_duplications == FALSE) |
                                (dups4$complete_duplications == TRUE & 
                                         dups4$last_observation == TRUE))
        length(dups4$ID) - length(dups5$ID) #This removes 385 data entries 

#recreate duplicate columns to identify changes in the duplications
        dups5 <- get_duplications(dups5)

#STEP 3 OF CLEANING – REPLACE ERRORS IN THE DATA FROM THE DUPLICATIONS

#get_errors identifies common errors in original date columns, e.g
#errors where a certain date is later than an upper limit (e.g the date when the
#data was entered, a deadline) or earlier than a lower limit (e.g date of birth,
#the beginning of time) or where a date is later/earlier than 2 other dates
#and that does not seem to be logicially correct (e.g if a start date is
#after an end date and a visit date or if an end date is before a start date
#and a visit date).
#get_errors can be editied to generated to identify the type of errors 
#encountered in the data. E.g. VD_UP_error is not included in this analysis due to 
#the fact it wasn't appropriate for the original data, but it is acknowledged 
#that this may be an error (and there may be lots of similar
#errors) in other scenarios.
        get_errors <- function (X, var1 = "start_date_new", var2 = "end_date_new", 
                        var3 = "visit_date_new", upper_limit = "date_recorded", 
                        lower_limit = "date_of_birth", error_name1 = "SD_UP_error", 
                        error_name2 = "ED_UP_error", error_name3 = "SD_LOW_error", 
                        error_name4 = "ED_LOW_error", error_name5 = "VD_LOW_error", 
                        error_name6 = "SD_LATE_error", error_name7 = "ED_EARLY_error", 
                        any_errors = "any_errors", spacer = "") {
                X[, paste(error_name1, spacer, sep = "")] <-  
                        X[[var1]] > X[[upper_limit]]
                X[, paste(error_name2, spacer, sep = "")] <-  
                        X[[var2]] > X[[upper_limit]]
                X[, paste(error_name3, spacer, sep = "")] <-  
                        X[[var1]] < X[[lower_limit]]
                X[, paste(error_name4, spacer, sep = "")] <- 
                        X[[var2]] < X[[lower_limit]]
                X[, paste(error_name5, spacer, sep = "")] <-  
                        X[[var3]] < X[[lower_limit]]
                X[, paste(error_name6, spacer, sep = "")] <-  
                        (X[[var1]] > X[[var2]]) & (X[[var1]] > X[[var3]])
                X[, paste(error_name7, spacer, sep = "")] <-  
                        (X[[var2]] < X[[var1]]) & (X[[var2]] < X[[var3]])
                #Column to identify whether any errors exist within that row
                X[, paste(any_errors, spacer, sep = "")] <- 
                        X[, paste(error_name1, spacer, sep = "")] == TRUE |
                        X[, paste(error_name2, spacer, sep = "")] == TRUE | 
                        X[, paste(error_name3, spacer, sep = "")] == TRUE |
                        X[, paste(error_name4, spacer, sep = "")] == TRUE | 
                        X[, paste(error_name5, spacer, sep = "")] == TRUE |
                        X[, paste(error_name6, spacer, sep = "")] == TRUE | 
                        X[, paste(error_name7, spacer, sep = "")] == TRUE
                X[[paste(any_errors, spacer, sep = "")]][
                        is.na(X[[paste(any_errors, spacer, sep = "")]])] <- FALSE
                return(X)
        }
        
        #Apply the function with defaults
        dups5 <- get_errors(X = dups5)
        
#get_lags_leads_errors identifies errors in lagging and leading columns, similarly
#to get_errors. The function iterates over each number of n to ensure that errors
#for all lagging/leading columns are generated. Default names for lagging/leading
# columns can be changed.         
        get_lags_leads_errors <- function(X, n, col1 = "SD_lag", col2 = "SD_lead", 
                                          col3 = "ED_lag", col4 = "ED_lead", 
                                          col5 = "VD_lag", col6 = "VD_lead") {
                for(i in 1:n) {
                        X <- get_errors(X, var1 = paste(col1, i, sep=""),
                                        spacer = paste("_", col1, i, sep=""))
                        X <- get_errors(X, var1 = paste(col2, i, sep=""),
                                        spacer = paste("_", col2, i, sep=""))
                        X <- get_errors(X, var2 = paste(col3, i, sep=""),
                                        spacer = paste("_", col3, i, sep=""))
                        X <- get_errors(X, var2 = paste(col4, i, sep=""),
                                        spacer = paste("_", col4, i, sep=""))
                        X <- get_errors(X, var3 = paste(col5, i, sep=""),
                                        spacer = paste("_", col5, i, sep=""))
                        X <- get_errors(X, var3 = paste(col6, i, sep=""),
                                        spacer = paste("_", col6, i, sep=""))
                }
                return(X)
        }
        #Apply the function with defaults
        dups5 <- get_lags_leads_errors(dups5, n)
#replace_error replaces errors with the previous and following 
#data entries for the duplicated start dates, end dates and visit dates that are 
#not errors.The functiion works in exactly the same way as replace_missing_data, 
#except that instead of replacing NAs with non-missing data, it replaces errors 
#with non-errors
#Print results allows you to print out the changes as they are made at each step
        replace_errors <- function(X, n, error_name, var1, var2, var3, 
                                   print_results = TRUE) {
                if(print_results == TRUE) {
                print(paste("Total number of", paste(error_name, "s", 
                             sep =""), "before corrections"))
                a <- sum(X[[paste(error_name)]], na.rm = TRUE)
                print(a) }
                for(i in 1:n) { 
                        X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                            is.na(X[[error_name]]), X[[var1]], 
                                    ifelse(X[[error_name]] == TRUE & 
                                           X[[paste("any_errors_", var2, i, 
                                                    sep="")]] == FALSE &
                                           !is.na(X[[paste(var2, i, sep="")]]), 
                                   X[[paste(var2, i, sep="")]],
                                   ifelse(X[[error_name]] == TRUE & 
                                          X[[paste("any_errors_", var3, i, 
                                                   sep="")]] == FALSE &
                                          !is.na(X[[paste(var3, i, sep="")]]), 
                                  X[[paste(var3, i, sep="")]], X[[var1]])))
                X <- get_errors(X)
                        X <- get_lags_leads_errors(X, n)
                        #This prints out results after each iteration of 'n' so
                        #it can be clearly visualised where the corretions are made
                        if(print_results == TRUE) {
                        print(paste("Total number of ", paste(error_name, "s", 
                                      sep =""), " after n = ", i, sep=""))
                        print(sum(X[[paste(error_name)]], na.rm = TRUE))
                        }
                }
                if(print_results == TRUE) {
                print(paste("Total number of", paste(error_name, "s", sep =""), 
                            "corrected"))
                b <- sum(X[[paste(error_name)]], na.rm = TRUE)
                print(a-b)
                }
                return(X)
        }
        
        #Apply the function to each date column for each error as needed
        dups6 <- replace_errors(dups5, n, "SD_UP_error", "start_date_new", 
                                "SD_lag", "SD_lead")
        dups6 <- replace_errors(dups6, n, "ED_UP_error", "end_date_new", 
                                "ED_lag", "ED_lead", print_results = FALSE)
        dups6 <- replace_errors(dups6, n, "SD_LOW_error", "start_date_new", 
                                "SD_lag", "SD_lead", print_results = FALSE)
        dups6 <- replace_errors(dups6, n, "ED_LOW_error", "end_date_new", 
                                "ED_lag", "ED_lead", print_results = FALSE)
        dups6 <- replace_errors(dups6, n, "VD_LOW_error", "visit_date_new", 
                                "VD_lag", "VD_lead", print_results = FALSE)
        dups6 <- replace_errors(dups6, n, "SD_LATE_error", "start_date_new", 
                                "SD_lag", "SD_lead", print_results = FALSE)
        dups6 <- replace_errors(dups6, n, "ED_EARLY_error", "end_date_new", 
                                "ED_lag", "ED_lead", print_results = FALSE)

#Transform the date columns that have been edited back to human-readable format
        dups6$end_date_new <- as.Date(dups6$end_date_new, origin="1970-01-01")
        dups6$start_date_new <- as.Date(dups6$start_date_new, origin="1970-01-01")
        dups6$visit_date_new <- as.Date(dups6$visit_date_new, origin="1970-01-01")

#recreate duplicate columns to identify changes in the duplications
        dups6 <- get_duplications(dups6)
        
#reapply get_dup_info to find the last complete duplicates in the data        
        dups6 <- dups6 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()
        
#Make a new dataset that contains only the most recent entry from complete 
#duplicates and deletes all older complete duplicates   
        dups7 <- subset(dups6, (dups6$complete_duplications == FALSE) |
                                (dups6$complete_duplications == TRUE & 
                                         dups6$last_observation == TRUE))
        length(dups6$ID) - length(dups7$ID) #This removes 264 data entries 

#recreate duplicate columns to identify changes in the duplications
        dups7 <- get_duplications(dups7)
        
        #Removes any irrelevant columns
        dups8 <- subset(dups7, select = 1:(which("last_observation" == 
                                                         colnames(dups7))-1))

#STEP 4 – CORRECT ANY ERRORS IN THE DUPLICATIONS

#get_alernative_dates generates columns with time added to or substracted from the 
#original start dates, end dates and visit dates. In this example, a day, week,
#month and year is added to or substracted from the original dates (because these
#are common errors that people make when recording dates). However, it is 
#acknowledged that in other scenarios there might be other common errors and this
#function can be edited to account for this e.g decade errors ar not included
        get_alternative_dates <- function (X, var1) {
                X[, paste(var1, "_plus_day", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% days(1)
                X[, paste(var1, "_minus_day", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% days(-1)
                X[, paste(var1, "_plus_week", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% weeks(1)
                X[, paste(var1, "_minus_week", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% weeks(-1)
                X[, paste(var1, "_plus_month", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% months(1)
                X[, paste(var1, "_minus_month", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% months(-1)
                X[, paste(var1, "_plus_year", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% years(1)
                X[, paste(var1, "_minus_year", sep = "")] <-  
                        as.Date(X[[var1]]) %m+% years(-1)
                return(X)
        }
        
        #Apply function to each date column as needed
        dups8 <- get_alternative_dates(dups8, "start_date_new")
        dups8 <- get_alternative_dates(dups8, "end_date_new")
        dups8 <- get_alternative_dates(dups8, "visit_date_new")

#get_alternative_dates_errors identifies errors in alternative dates columns, 
#similarly to get_errors. Errors are identified for the specific time difference 
#given in the arguements (e.g tim_diff = plus_day")
#Default names for original dates columns can be changed.  
        get_alternative_dates_errors <- function(X, col1 = "start_date_new", 
                                                 col2 = "end_date_new", 
                                                 col3 = "visit_date_new", time_diff) {
                X <- get_errors(X, var1 = paste(col1, "_", time_diff, sep=""),
                                spacer = paste("_", col1, "_", time_diff, sep=""))
                X <- get_errors(X, var2 = paste(col2, "_", time_diff, sep=""),
                                spacer = paste("_", col2, "_", time_diff, sep=""))
                X <- get_errors(X, var3 = paste(col3, "_", time_diff, sep=""),
                                spacer = paste("_", col3, "_", time_diff, sep=""))
                return(X)
        }

        #Reapply get_errors to ensure they are up to date
        dups8 <- get_errors(dups8)
        #Apply function to each time differnce as needed
        dups8 <- get_alternative_dates(dups8, "start_date_new")
        dups8 <- get_alternative_dates(dups8, "end_date_new")
        dups8 <- get_alternative_dates(dups8, "visit_date_new")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "plus_day")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "plus_week")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "plus_month")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "plus_year")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "minus_day")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "minus_week")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "minus_month")
        dups8 <- get_alternative_dates_errors(dups8, time_diff = "minus_year")

#correct_errors replaces errors with the alternative start dates, end dates and 
#visit dates that are not errors. It checks if there are any errors first in 
#differences in days, then weeks, the months, then years. It checks if adding
#a time difference is an error before checking if substracting a time difference
#is an error. For example, it would check adding a day, substracting a day,
#adding a week, subtracting a week etc, in that order. Default time differences 
#can be changed if needed.
#Print results allows you to print out the changes as they are made at each step
        correct_errors <- function(X, error_name, var1, time_diff1 = "plus_day", 
                           time_diff2 = "minus_day", time_diff3 = "plus_week", 
                           time_diff4 = "minus_week", time_diff5 = "plus_month", 
                           time_diff6 = "minus_month", time_diff7 = "plus_year", 
                           time_diff8 = "minus_year", print_results = TRUE) {
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "before corrections"))
                        a <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(a) }
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE & 
                                   X[[paste("any_errors_", var1, "_", time_diff1, 
                                            sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff1, 
                                                   sep="")]]), 
                           X[[paste(var1, "_", time_diff1, sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff1)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                          sep =""), "corrected with", time_diff1))
                        b <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(a-b)
                }
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE & 
                                   X[[paste("any_errors_", var1, "_", time_diff2, 
                                            sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff2, 
                                                   sep="")]]), 
                           X[[paste(var1, "_", time_diff2, sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff2)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff2))
                        c <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(b-c)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE 
                           & X[[paste("any_errors_", var1, "_", time_diff3, 
                                                              sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff3, 
                                                   sep="")]]), 
                           X[[paste(var1, "_", time_diff3, sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff3)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff3))
                        d <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(c-d)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE & 
                                   X[[paste("any_errors_", var1, "_", time_diff4, 
                                                              sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff4, sep="")]]), 
                           X[[paste(var1, "_", time_diff4, sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff4)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff4))
                        e <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(d-e)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                            is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE & 
                                   X[[paste("any_errors_", var1, "_", time_diff5, 
                                                              sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff5, 
                                                   sep="")]]), 
                           X[[paste(var1, "_", time_diff5, sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff5)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff5))
                        f <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(e-f)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE & 
                                   X[[paste("any_errors_", var1, "_", time_diff6, 
                                                              sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff6, sep="")]]), 
                           X[[paste(var1, "_", time_diff6, 
                                    sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff6)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff6))
                        g <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(f-g)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE & 
                                   X[[paste("any_errors_", var1, "_", time_diff7, 
                                                              sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff7, 
                                                   sep="")]]), 
                           X[[paste(var1, "_", time_diff7, sep="")]], X[[var1]]))
                X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff7)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff7))
                        h <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(g-h)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                    is.na(X[[error_name]]), X[[var1]], 
                            ifelse(X[[error_name]] == TRUE &
                                   X[[paste("any_errors_", var1, "_", time_diff8, 
                                                              sep="")]] == FALSE &
                                   !is.na(X[[paste(var1, "_", time_diff8, 
                                                   sep="")]]), 
                           X[[paste(var1, "_", time_diff8, sep="")]], X[[var1]]))
        X <- get_errors(X)
                X <- get_alternative_dates_errors(X, n, time_diff = time_diff8)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                     sep =""), "corrected with", time_diff8))
                        I <- sum(X[[paste(error_name)]], na.rm = TRUE)
                        print(h-I)
                }       
                X[[var1]] <- ifelse(X[[error_name]] == FALSE | 
                                            is.na(X[[error_name]]), X[[var1]], 
                                    ifelse(X[[error_name]] == TRUE, NA, X[[var1]]))
                X <- get_errors(X)
                if(print_results == TRUE) {
                        print(paste("Total number of", paste(error_name, "s", 
                                             sep =""), "deleted"))
                        print(I)
                }
                return(X)
        }
        
        #Apply the function to each date column for each error as needed
        dups8 <- correct_errors(dups8, "SD_UP_error", "start_date_new")
        dups8 <- correct_errors(dups8, "ED_UP_error", "end_date_new")
        dups8 <- correct_errors(dups8, "SD_LOW_error", "start_date_new")
        dups8 <- correct_errors(dups8, "ED_LOW_error", "end_date_new")
        dups8 <- correct_errors(dups8, "VD_LOW_error", "visit_date_new")
        dups8 <- correct_errors(dups8, "SD_LATE_error", "start_date_new")
        dups8 <- correct_errors(dups8, "ED_EARLY_error", "end_date_new")
        
        #Remove any unneccesary columns
        dups8 <- subset(dups8, select = 1:(which("start_date_new_plus_day" ==
                                         colnames(dups8))-1))

#Transform the date columns that have been edited back to human-readable format
        dups8$end_date_new <- as.Date(dups8$end_date_new, origin="1970-01-01")
        dups8$start_date_new <- as.Date(dups8$start_date_new, origin="1970-01-01")
        dups8$visit_date_new <- as.Date(dups8$visit_date_new, origin="1970-01-01")

#recreate duplicate columns to identify changes in the duplications
        dups8 <- get_duplications(dups8)
#reapply get_dup_info to find the last complete duplicates in the data        
        dups8 <- dups8 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()
#Make a new dataset that contains only the most recent entry from complete 
#duplicates and deletes all older complete duplicates           
        dups9 <- subset(dups8, (dups8$complete_duplications == FALSE) |
                                (dups8$complete_duplications == TRUE &
                                         dups8$last_observation == TRUE))
        length(dups8$ID) - length(dups9$ID) #This removes 130 data entries 

#recreate duplicate columns to identify changes in the duplications
        dups9 <- get_duplications(dups9)

#STEP 5 – WHEN THERE ARE DUPLICATIONS THAT ARE NOT ERRORS/MISSING KEEP THE MOST
#RECENT DATA ENTRY

#This function gets the most recent data entry within each set of duplicates
        get_most_recent_dups <- function(X, var1 = "start_date_new", 
                         var2 = "end_date_new", var3 = "visit_date_new", 
                         spacer = "most_recent"){
                X <- X[order(X$ID), ]
                X[, paste(var1, "_", spacer, sep = "")] <- tail(X[[var1]], 1)
                X[, paste(var2, "_", spacer, sep = "")] <- tail(X[[var2]], 1)
                X[, paste(var3, "_", spacer, sep = "")] <- tail(X[[var3]], 1)
                return(X)
        }
        
#Apply function to each set of duplicates       
        dups9 <- dups8 %>%
                dplyr::group_by(ID) %>%
                dplyr::do(get_most_recent_dups(.)) %>%
                dplyr::ungroup()

#replace dates with the most recent data entry within that group of duplicates
        dups9$start_date_new <- ifelse(dups9$duplications == TRUE, 
                       dups9$start_date_new_most_recent, dups9$start_date_new)
        dups9$end_date_new <- ifelse(dups9$duplications == TRUE, 
                       dups9$end_date_new_most_recent, dups9$end_date_new)
        dups9$visit_date_new <- ifelse(dups9$duplications == TRUE, 
                       dups9$visit_date_new_most_recent, dups9$visit_date_new)
                                       
#Transform the date columns that have been edited back to human-readable format        
        dups9$end_date_new <- as.Date(dups9$end_date_new, origin="1970-01-01")
        dups9$start_date_new <- as.Date(dups9$start_date_new, origin="1970-01-01")
        dups9$visit_date_new <- as.Date(dups9$visit_date_new, origin="1970-01-01")

#recreate duplicate columns to identify changes in the duplications
        dups9 <- get_duplications(dups9)

#reapply get_dup_info to find the last complete duplicates in the data  
        dups9 <- dups9 %>%
                dplyr::group_by(complete_dups_ID) %>%
                dplyr::do(get_dup_info(.)) %>%
                dplyr::ungroup()
        
#Make a new dataset that contains only the most recent entry from complete 
#duplicates and deletes all older complete duplicates          
        dups10 <- subset(dups9, (dups9$complete_duplications == FALSE) |
                                 (dups9$complete_duplications == TRUE & 
                                          dups9$last_observation == TRUE))
        length(dups9$ID) - length(dups10$ID) #This removes 334 data entries 

#recreate duplicate columns to identify changes in the duplications
        dups10 <- get_duplications(dups10)

#remove unnecessary columns        
        dups10 <- subset(dups10, select = 1:(which("duplications" == 
                                                   colnames(dups10))-1))

#THIS IS THE FULLY CLEANED DUPLICATE DATA!

#STEP 6 OF CLEANING – CORRECT/DELETE ANY ERRORS IN THE DATA THAT IS NOT DUPLICATED          

#This following code re-applies all of the above STEP 4 method and functions on 
#the data that is not duplicated        
       not_dups2 <- get_alternative_dates(not_dups, "start_date_new")
       not_dups2 <- get_alternative_dates(not_dups2, "end_date_new")
       not_dups2 <- get_alternative_dates(not_dups2, "visit_date_new")
       
       not_dups2 <- get_errors(not_dups2)
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "plus_day")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "plus_week")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "plus_month")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "plus_year")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "minus_day")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "minus_week")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "minus_month")
       not_dups2 <- get_alternative_dates_errors(not_dups2, time_diff = "minus_year")
       
       not_dups2 <- correct_errors(not_dups2, "SD_UP_error", "start_date_new")
       not_dups2 <- correct_errors(not_dups2, "ED_UP_error", "end_date_new")
       not_dups2 <- correct_errors(not_dups2, "SD_LOW_error", "start_date_new")
       not_dups2 <- correct_errors(not_dups2, "ED_LOW_error", "end_date_new")
       not_dups2 <- correct_errors(not_dups2, "VD_LOW_error", "visit_date_new")
       not_dups2 <- correct_errors(not_dups2, "SD_LATE_error", "start_date_new")
       not_dups2 <- correct_errors(not_dups2, "ED_EARLY_error", "end_date_new")
       
       not_dups2 <- subset(not_dups2, select = 1:(which("duplications" == 
                                        colnames(not_dups2))-1))

#Transform the date columns that have been edited back to human-readable format
        not_dups2$end_date_new <- as.Date(not_dups2$end_date_new, origin="1970-01-01")
        not_dups2$start_date_new <- as.Date(not_dups2$start_date_new, origin="1970-01-01")
        not_dups2$visit_date_new <- as.Date(not_dups2$visit_date_new, origin="1970-01-01")

#THIS IS THE FULLY CLEANED NOT-DUPLICATE DATA!

#REJOIN THE SUBSETS AND SUMMARISE CLEANING PROCESS     

        dat2 <- rbind(not_dups2, dups10)
        
        #SUMMARY
        sum(length(dat2$ID)) - sum(length(dat$ID)) #1418 duplicated rows deleted
        sum(is.na(dat2$start_date)) - sum(is.na(dat2$start_date_new)) #504 start dates deleted
        sum(dat2$start_date != dat2$start_date_new, na.rm = TRUE) #125 start dates modified
        sum(is.na(dat2$end_date)) - sum(is.na(dat2$end_date_new)) #507 end dates deleted
        sum(dat2$end_date != dat2$end_date_new, na.rm = TRUE) #111 end dates modified
        sum(is.na(dat2$visit_date)) - sum(is.na(dat2$visit_date_new)) #213 visit dates deleted
        sum(dat2$visit_date != dat2$visit_date_new, na.rm = TRUE) #83 visit dates modified
        

