#FILENAME: example_data_generation.R
#AUTHOR: Charlotte Woolley
#DESCRIPTION: This code documents how an example dataset was generated for 
#testing a data cleaning method for dates in illness records
#PACKAGES NEEDED:
        library(plyr)
        library(dplyr)

#Make an ID variable for each simulated individual
        ID <- as.factor(1:10000)
        
#Set the start of time and end of time for the data period generated        
        st <- '2000/01/01'
        et <- '2010/01/01'
        
#Make a randomly generated date of birth variable for each simulated individual,
#that assumes all individuals were born between within the start and end of time

        set.seed(777)
        date_of_birth <- sample(seq(as.Date(st), as.Date(et), 
                                    by="day"), 10000, replace = TRUE)
#Make a dataframe 
        dat <- data.frame(ID, date_of_birth)

#Function that generates random dates for when the incidents were recorded and 
#for start, end and visit dates

        get_dates <- function(X, min_diff=0) {
                #date_recorded should be after the date_of_birth but before the
                #et. mid_diff allows a minimum difference between the date of birth 
                #and the recorded date, with the default set to 0
                set.seed(778)
                X$date_recorded <- (sample(seq(as.Date(X$date_of_birth), 
                               as.Date(et), by="day"), 1)) + min_diff
                #start_date should be after the date_of birth but before the date
                #recorded
                set.seed(779)
                X$start_date <- (sample(seq(as.Date(X$date_of_birth), 
                                               as.Date(X$date_recorded), by="day"), 1))
                #end_date should be after the start_date but before the et
                set.seed(780)
                X$end_date <- (sample(seq(as.Date(X$start_date), 
                                            as.Date(X$date_recorded), by="day"), 1))
                #visit date should be between the start and end dates
                set.seed(781)
                X$visit_date <- (sample(seq(as.Date(X$start_date), 
                                          as.Date(X$end_date), by="day"), 1))
                return(X)
        }
        
#This applies the function to each row, with a minimum difference between
#the date of birth and date recorded set at 28 days
        dat <- ddply(dat, 1, get_dates, min_diff = 28)
        
#randomly select data to add errors to

        p <- 0.1 #percentage of data that may contain errors
        
        #start dates
        set.seed(782)
        sub1 <- dat[sample(nrow(dat), (max(dat$ID)*p)), ]
        sub1$SD_error_gen <- TRUE
        dat <- join(dat,sub1)
        set.seed(783)
        dat$random_SDs <- sample(seq(as.Date(st), as.Date(et), 
                                     by="day"), 10000, replace = TRUE)
        dat$start_date <- ifelse(dat$SD_error_gen == TRUE & !is.na(dat$SD_error_gen), 
                                  dat$random_SDs, dat$start_date)
        dat$start_date <- as.Date(dat$start_date, origin="1970-01-01")

        #end dates
        set.seed(784)
        sub2 <- dat[sample(nrow(dat), (max(dat$ID)*p)), ]
        sub2$ED_error_gen <- TRUE
        dat <- join(dat,sub2)
        set.seed(785)
        dat$random_EDs <- sample(seq(as.Date(st), as.Date(et), 
                                     by="day"), 10000, replace = TRUE)
        dat$end_date <- ifelse(dat$ED_error_gen == TRUE & !is.na(dat$ED_error_gen), 
                               dat$random_EDs, dat$end_date)
        dat$end_date <- as.Date(dat$end_date, origin="1970-01-01")
        
        #visited vet dates
        set.seed(786)
        sub3 <- dat[sample(nrow(dat), (max(dat$ID)*p)), ]
        sub3$VD_error_gen <- TRUE
        dat <- join(dat,sub3)
        set.seed(787)
        dat$random_VDs <- sample(seq(as.Date(st), as.Date(et), 
                                     by="day"), 10000, replace = TRUE)
        dat$visit_date <- ifelse(dat$VD_error_gen == TRUE & !is.na(dat$VD_error_gen),
                                 dat$random_VDs, dat$visit_date)
        dat$visit_date <- as.Date(dat$visit_date, origin="1970-01-01")
        
        
#randomly select data to add missing data to
        
        q <- 0.05 #percentage of data that contains missing data       
        
        #start dates
        set.seed(788)
        sub4 <- dat[sample(nrow(dat), (max(dat$ID)*q)), ]
        sub4$SD_error_gen2 <- TRUE
        dat <- join(dat,sub4)
        set.seed(789)
        dat$start_date <- ifelse(dat$SD_error_gen2 == TRUE & !is.na(dat$SD_error_gen2), 
                                 NA, dat$start_date)
        dat$start_date <- as.Date(dat$start_date, origin="1970-01-01")
        
        #end dates
        set.seed(790)
        sub5 <- dat[sample(nrow(dat), (max(dat$ID)*q)), ]
        sub5$ED_error_gen2 <- TRUE
        dat <- join(dat,sub5)
        set.seed(791)
        dat$end_date <- ifelse(dat$ED_error_gen2 == TRUE & !is.na(dat$ED_error_gen2), 
                               NA, dat$end_date)
        dat$end_date <- as.Date(dat$end_date, origin="1970-01-01")
        
        #visited vet dates
        set.seed(792)
        sub6 <- dat[sample(nrow(dat), (max(dat$ID)*q)), ]
        sub6$VD_error_gen2 <- TRUE
        dat <- join(dat,sub6)
        set.seed(793)
        dat$visit_date <- ifelse(dat$VD_error_gen2 == TRUE & !is.na(dat$VD_error_gen2),
                                 NA, dat$visit_date)
        dat$visit_date <- as.Date(dat$visit_date, origin="1970-01-01")
        
        
#save final dataset and write data to a csv

        dat <- dat[, 1:6]        
        write.csv(dat, 'example_dates_data.csv')
        
        
        

