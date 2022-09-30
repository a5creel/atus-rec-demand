# Intial data exploration / Andie Creel / Started Sept 15 2022

# -----------------------------------------------------------------------------
# loading packages 


# The 2003-21 Activity Summary file contains data for the total number of minutes that each respondent spent doing each 6-digit activity.  
# Each record corresponds to a unique respondent, as indicated by a unique value of the variable TUCASEID. 
# -----------------------------------------------------------------------------
library(vroom)
library(dplyr)
library(data.table)



# -----------------------------------------------------------------------------
# reading in data 
# -----------------------------------------------------------------------------

# The 2021 Activity Summary file contains data for the total number of minutes 
# that each respondent spent doing each 6-digit activity.  Each record 
# corresponds to a unique respondent, as indicated by a unique value of the 
# variable TUCASEID. 

# -----------------------------------------------------------------------------
# Reading in summary file for each year
# -----------------------------------------------------------------------------

# List to store all summary files ing 
list_df <- list() 

# 2003 - 2017
for (k in 3:17){
  file_name <- paste0("curl https://www.bls.gov/tus/special.requests/atussum_20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  list_df[[k]] <- fread(file_name)
}

# 2018 - 2021
for (k in 18:21){
  file_name <- paste0("curl https://www.bls.gov/tus/special.requests/atussum-20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  list_df[[k]] <- fread(file_name)
}


#NEED TO REDO THIS FOR DIFFERENT YEARS 

myDataClean <- function(df, year){
  
  # if year is XXX then variables are XXXX 
  
  myW <- df %>%
    rename(race = PTDTRACE) %>%
    rename(week_earning = TRERNWA) %>%
    #rename(household_id = TUCASEID) %>%
    rename(sample_weight = TUFINLWGT) %>%
    select(household_id, race, week_earning, sample_weight, starts_with("t1301")) %>% #Sports, Exercise, & Recreation
    rowwise() %>% mutate(minutes_recreation = sum(c_across(cols = starts_with('t1301'))))
  
  myW
}

list_df_clear <- list()
list_df_clear <- for (k in 3:21) {
  list_df_clear[[k]] <-myDataClean(list_df[[k]])
}

myWorking_21 <- myOG_21 %>%
  rename(race = PTDTRACE) %>%
  rename(week_earning = TRERNWA) %>%
  rename(household_id = TUCASEID) %>%
  rename(sample_weight = TUFINLWGT) %>%
  select(household_id, race, week_earning, sample_weight, starts_with("t1301")) %>% #Sports, Exercise, & Recreation
  rowwise() %>% mutate(minutes_recreation = sum(c_across(cols = starts_with('t1301'))))

# average 
avg_min_rec_21 = sum(myWorking_21$sample_weight*myWorking_21$minutes_recreation)/sum(myWorking_21$sample_weight)

# -----------------------------------------------------------------------------
# 2003
# -----------------------------------------------------------------------------

myOG_03 <- vroom('raw_data/2003/atussum_2003/atussum_2003.dat')  

myWorking_03 <- myOG_03 %>%
  rename(race = PTDTRACE) %>%
  rename(week_earning = TRERNWA) %>%
  rename(household_id = TUCASEID) %>%
  rename(sample_weight = TUFINLWGT) %>%
  select(household_id, race, week_earning, sample_weight, starts_with("t1301")) %>% #Sports, Exercise, & Recreation
  rowwise() %>% mutate(minutes_recreation = sum(c_across(cols = starts_with('t1301'))))

# average 
avg_min_rec_03 = sum(myWorking_03$sample_weight*myWorking_03$minutes_recreation)/sum(myWorking_03$sample_weight)

unzip("https://www.bls.gov/tus/special.requests/atussum_2003.zip", list=TRUE)$Name

library(data.table)

list_df <- list() # creates a list
for (k in 3:17){
  file_name <- paste0("curl https://www.bls.gov/tus/special.requests/atussum_20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  list_df[[k]] <- fread(file_name)
}

for (k in 18:21){
  file_name <- paste0("curl https://www.bls.gov/tus/special.requests/atussum-20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  list_df[[k]] <- fread(file_name)
}




