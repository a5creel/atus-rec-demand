# Reading and cleaning activity summary file for every year /
# Andie Creel / Started Sept 15 2022, Finished Oct. 6th 

# -----------------------------------------------------------------------------
# loading packages 
# -----------------------------------------------------------------------------
rm(list = ls())
library(vroom)
library(dplyr)
library(data.table)
library(blscrapeR) # pulls consumer price index


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
  # k <- 3
  file_name <- paste0("curl https://www.bls.gov/tus/datafiles/atussum_20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")

  list_df[[k]] <- fread(cmd=file_name)
  
  #write data to raw data folder so I have it 
  vroom_write(list_df[[k]], paste0("raw_data/ATUS_2003-2021_Raw/", k,".csv"))
}

# 2018 - 2021
for (k in 18:21){
  file_name <- paste0("curl https://www.bls.gov/tus/datafiles/atussum-20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
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
    rename(week_earning = TRERNWA) %>% # Weekly earnings (2 implied decimals) (found in summary do file)
    mutate(week_earning = week_earning / 100) %>% # dealing with 2 implied decimals
    rename(household_id = TUCASEID) %>%
    select(household_id, race, week_earning, sample_weight, starts_with("t1301"), starts_with("t1813")) #Sports, Exercise, & Recreation
  
  #creating sum of all recreation minutes
  myW$total_min_rec <-  apply(myW[,select(myW, starts_with("t1301"))], 1, sum) 
  myW$total_min_travel <- apply(myW[,select(myW, starts_with("t1813"))], 1, sum) 
  
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


# -----------------------------------------------------------------------------
# pulling in replicate weights to get standard errors
# -----------------------------------------------------------------------------


# List to store all replicate weight files 
replicate_list <- list() 

#2003 is weird
file_name <- paste0("curl https://www.bls.gov/tus/datafiles/atuswgts06wt_2003.zip | tar -xf- --to-stdout *.dat")

replicate_list[[3]] <- fread(cmd=file_name)

#write data to raw data folder so I have it 
vroom_write(replicate_list[[3]], paste0("raw_data/ATUS_2003-2021_Replicate_weights/3.csv"))



# 2004 - 2017
for (k in 4:17){

  file_name <- paste0("curl https://www.bls.gov/tus/datafiles/atuswgts_20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  replicate_list[[k]] <- fread(cmd=file_name)
  
  #write data to raw data folder so I have it 
  vroom_write(replicate_list[[k]], paste0("raw_data/ATUS_2003-2021_Replicate_weights/", k,".csv"))
}


# 2018 - 2021
for (k in 18:21){

  file_name <- paste0("curl https://www.bls.gov/tus/datafiles/atuswgts-20",if_else(k<10, paste0("0", k), paste0(k)),".zip | tar -xf- --to-stdout *.dat")
  
  replicate_list[[k]] <- fread(cmd=file_name)
  
  #write data to raw data folder so I have it 
  vroom_write(replicate_list[[k]], paste0("raw_data/ATUS_2003-2021_Replicate_weights/", k,".csv"))
  
}

# -----------------------------------------------------------------------------
# inflation adjustment
# -----------------------------------------------------------------------------

# Consumer price index, uses package blscrapeR : https://github.com/keberwein/blscrapeR
inflationStuff <- inflation_adjust(2021)
vroom_write(inflationStuff, "clean_data/inflation_rates.csv")

# -----------------------------------------------------------------------------
# US population by year
# -----------------------------------------------------------------------------

myPop <-vroom("raw_data/world_bank_us_pop/API_SP.POP.TOTL_DS2_en_csv_v2_4760264.csv") %>%
  filter(`Country Code` == "USA") %>%
  pivot_longer(cols = c(starts_with("19") |starts_with("20")), names_to = "year", values_to = "population") %>%
  mutate(country = `Country Code`) %>%
  select(country, year, population)

vroom_write(myPop, "clean_data/USA_pop.csv")

