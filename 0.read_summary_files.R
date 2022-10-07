# Reading and cleaning activity summary file for every year /
# Andie Creel / Started Sept 15 2022, Finished Oct. 6th 

# -----------------------------------------------------------------------------
# loading packages 
# -----------------------------------------------------------------------------
rm(list = ls())
library(vroom)
library(dplyr)
library(data.table)



# -----------------------------------------------------------------------------
# reading in data 
# -----------------------------------------------------------------------------

# The year's Activity Summary file contains data for the total number of minutes 
# that each respondent spent doing each 6-digit activity.  Each record 
# corresponds to a unique respondent, as indicated by a unique value of the 
# variable TUCASEID. 

#All year's microdata: https://www.bls.gov/tus/#data 
#One year specifically: https://www.bls.gov/tus/datafiles-2021.htm 
# ATUS 2021 Activity summary file (zip) is the data set in this r script

# -----------------------------------------------------------------------------
# Reading in activity summary file for each year
# -----------------------------------------------------------------------------

# List to store all summary files  
list_df <- list() 

# 2003 - 2017
for (k in 3:17){
  file_name <- paste0("curl https://www.bls.gov/tus/special.requests/atussum_20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  list_df[[k]] <- fread(file_name)
  
  #write data to raw data folder so I have it 
  vroom_write(list_df[[k]], paste0("raw_data/ATUS_2003-2021_Raw/", k,".csv"))
}

# 2018 - 2021
for (k in 18:21){
  file_name <- paste0("curl https://www.bls.gov/tus/special.requests/atussum-20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  list_df[[k]] <- fread(file_name)
  
  #write data to raw data folder so I have it 
  vroom_write(list_df[[k]], paste0("raw_data/ATUS_2003-2021_Raw/", k,".csv"))
}

# -----------------------------------------------------------------------------
# functions
# -----------------------------------------------------------------------------


mySum_func <- function(df) {  
  df[1,] %>% 
    sum(c_across(cols = starts_with('t1301')))
}

myDataClean <- function(df, year){
  # year = 3
  # df <- list_df[[year]] 
  
  
  #capitalizes all column names 
  setnames(df, toupper(names(df)))
  
  #2020's sampling weight is different 
  if (year != 20){
    df <- df %>%
      rename(sample_weight = TUFINLWGT)
      
  } else { 
    df <- df %>%
      rename(sample_weight = TU20FWGT)
  }
 
  myW <- df %>%
    rename(race = PTDTRACE) %>%
    rename(week_earning = TRERNWA) %>%
    rename(household_id = TUCASEID) %>%
    select(household_id, race, week_earning, sample_weight, starts_with("t1301")) #Sports, Exercise, & Recreation
  
  #creating sum of all recreation minutes
  myW$total_min_rec <-  apply(myW[,select(myW, starts_with("t1301"))], 1, sum) 
  
  #return
  myW
}


# -----------------------------------------------------------------------------
# 1) creating a list of that holds a clean data frame for each year
# 2) store each clean dataset as csv
# -----------------------------------------------------------------------------


list_df_clean <- list()
for (y in 3:21) {
  list_df_clean[[y]] <-myDataClean(df = list_df[[y]], year = y)
  
  myFile_name <- if_else(y < 10, 
                         paste0("clean_data/ATUS_2003-2021/200", y,".csv"),
                         paste0("clean_data/ATUS_2003-2021/20", y,".csv"))
  
  vroom_write(list_df_clean[[y]], myFile_name)
}




