# Functions to support the first pass of result 
# Andie Creel 
# Started on Dec. 1, 2022 


# -----------------------------------------------------------------------------
#  variable selection from activity file
# -----------------------------------------------------------------------------

clean_activity <- function(df) {
  # activity file: used the atusact_2021.do file for variable names 
  # Location: --------
  # label define labeltewhere
  # -1 "Blank"
  # -2 "Don't Know"
  # -3 "Refused"
  # 1 "Respondent's home or yard"
  # 2 "Respondent's workplace"
  # 3 "Someone else's home"
  # 4 "Restaurant or bar"
  # 5 "Place of worship"
  # 6 "Grocery store"
  # 7 "Other store/mall"
  # 8 "School"
  # 9 "Outdoors away from home"
  # 10 "Library"
  # 11 "Other place"
  # 12 "Car, truck, or motorcycle (driver)"
  # 13 "Car, truck, or motorcycle (passenger)"
  # 14 "Walking"
  # 15 "Bus"
  # 16 "Subway/train"
  # 17 "Bicycle"
  # 18 "Boat/ferry"
  # 19 "Taxi/limousine service"
  # 20 "Airplane"
  # 21 "Other mode of transportation"
  # 30 "Bank"
  # 31 "Gym/health club"
  # 32 "Post Office"
  # 89 "Unspecified place"
  # 99 "Unspecified mode of transportation"
  # ------
  df <- df %>%
    select(TUCASEID, TEWHERE, TRCODE, TUACTDUR24, TUCUMDUR24, TUACTIVITY_N) %>%
    rename(act_number = TUACTIVITY_N) %>% # Activity line number
    rename(id = TUCASEID) %>% #ATUS Case ID (14-digit identifier)
    rename(location = TEWHERE) %>% # where were you during the activity?
    rename(act_code = TRCODE) %>% # Six digit activity code
    rename(duration = TUACTDUR24) %>% # Duration of activity in minutes (last activity truncated at 4:00 a.m.)
    rename(duration_cum = TUCUMDUR24) %>%  #Cumulative duration of activity lengths in minutes; last activity truncated at 4:00am or 1440 minutes (cumulative total of TUACT";
    mutate(act_code_short = str_sub(act_code, 1,4)) %>%
    filter(act_code_short == "1813" | act_code_short == "1301") %>% #travel to recreation and recreation
    filter(location != 1) # location is not at your own home 

  
  return(df)
}

# -----------------------------------------------------------------------------
# calculate duration of travel and recreation
# -----------------------------------------------------------------------------

get_duration <- function(df) {

  #pivoting wider to inspect activity order (want to check for travel on both sides of recreation)
  myDF_pivot <- df %>%
  # test <- myAct %>%
    select(id, duration, act_number, act_code_short) %>%
    pivot_wider(id_cols =  id, names_from = act_code_short, values_from = c(act_number, duration)) %>%
    rename(act_number_travel = act_number_1813) %>%
    rename(act_number_rec = act_number_1301) %>%
    rename(duration_travel = duration_1813) %>%
    rename(duration_rec = duration_1301) %>%
    filter(act_number_travel != "NULL") %>% 
    filter(act_number_rec != "NULL")    
    
  # (a visual inspection shows that most activities are "sandwiched" (travel comes before or after activity))
  # relies on dropping the nulls, an individual needs to participate in the recreation I'm interested in and travel to it to be included in the dataset
  
  # sum duration spent traveling and duration recreating for each person 
  # travel there and back are summed, different kind of recreation are summed 
  myDF_pivot$duration_travel <- sapply(myDF_pivot$duration_travel, sum)  
  myDF_pivot$duration_rec <- sapply(myDF_pivot$duration_rec, sum) 
  
  return(myDF_pivot)
}

# -----------------------------------------------------------------------------
# clean CPS to relevant variables
# -----------------------------------------------------------------------------

#RACE, INCOME FIP
# Race: --------  
#   label define labelptdtrace
# -1 "Blank"
# -2 "Don't Know"
# -3 "Refused"
# 1 "White only"
# 2 "Black only"
# 3 "American Indian, Alaskan Native only"
# 4 "Asian only"
# 5 "Hawaiian/Pacific Islander only"
# 6 "White-Black"
# 7 "White-American Indian"
# 8 "White-Asian"
# 9 "White-Hawaiian"
# 10 "Black-American Indian"
# 11 "Black-Asian"
# 12 "Black-Hawaiian"
# 13 "American Indian-Asian"
# 14 "American Indian-Hawaiian"
# 15 "Asian-Hawaiian"
# 16 "White-Black-American Indian"
# 17 "White-Black-Asian"
# 18 "White-Black-Hawaiian"
# 19 "White-American Indian-Asian"
# 20 "White-American Indian-Hawaiian"
# 21 "White-Asian-Hawaiian"
# 22 "Black-American Indian-Asian"
# 23 "White-Black-American Indian-Asian"
# 24 "White-American Indian-Asian-Hawaiian"
# 25 "Other 3 race combinations"
# 26 "Other 4 and 5 race combinations"
# ----

# Income: ----
# -1 "Blank"
# -2 "Don't Know"
# -3 "Refused"
# 1 "Less than $5,000"
# 2 "$5,000 to $7,499"
# 3 "$7,500 to $9,999"
# 4 "$10,000 to $12,499"
# 5 "$12,500 to $14,999"
# 6 "$15,000 to $19,999"
# 7 "$20,000 to $24,999"
# 8 "$25,000 to $29,999"
# 9 "$30,000 to $34,999"
# 10 "$35,000 to $39,999"
# 11 "$40,000 to $49,999"
# 12 "$50,000 to $59,999"
# 13 "$60,000 to $74,999"
# 14 "$75,000 to $99,999"
# 15 "$100,000 to $149,999"
# 16 "$150,000 and over"
# ----

clean_cps <- function(df){
  df <- df %>%
    select(TUCASEID, PTDTRACE, HEFAMINC, GESTFIPS, GTCO) %>%
    rename(id = TUCASEID) %>% # ATUS Case ID (14-digit identifier)
    rename(race = PTDTRACE) %>% # Race (topcoded)
    rename(family_income = HEFAMINC) %>% # Edited: Family Income
    rename(fips_st = GESTFIPS) %>% # Federal Processing Information Standards (FIPS) state code
    rename(fips_co = GTCO) %>% # Federal Processing Standards (FIPS) county code
    distinct() %>%
    mutate(fips_st = str_pad(fips_st, width = 2, pad ="0")) # adding zero in for fips
  
  return(df)
}


# -----------------------------------------------------------------------------
# Cleaning respondent files variables
# gets weights
# -----------------------------------------------------------------------------

clean_resp <- function(df){
  
  df <- df %>%
    select(TUCASEID, TUFINLWGT) %>% 
    rename(id = TUCASEID) %>% # ATUS Case ID (14-digit identifier)
    rename(weight = TUFINLWGT) # ATUS final weight
  
  return(df)
}

# -----------------------------------------------------------------------------
# get income from the binned levels
# top bin is entered as 150000 which is always an underestimate
# -----------------------------------------------------------------------------

get_income <- function(df){
  
  # middle income
  df <- df %>% 
    mutate(family_income_mid = ifelse(family_income == -1, NA, family_income)) %>%
    mutate(family_income_mid = ifelse(family_income == -2, NA, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == -3, NA, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 1, 2500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 2, 6250, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 3, 8750, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 4, 11250, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 5, 13750, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 6, 17500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 7, 22500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 8, 27500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 9, 32500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 10, 37500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 11, 45000, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 12, 55000, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 13, 67500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 14, 87500, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 15, 125000, family_income_mid)) %>%
    mutate(family_income_mid = ifelse(family_income == 16, 150000, family_income_mid)) %>% # top one is entered as 150000 which is always an underestiamte
  
  # lower bound -- guarantees this is an underestimate 
    mutate(family_income_low = ifelse(family_income == -1, NA, family_income)) %>%
    mutate(family_income_low = ifelse(family_income == -2, NA, family_income_low)) %>%
    mutate(family_income_low = ifelse(family_income == -3, NA, family_income_low)) %>%
    mutate(family_income_low = ifelse(family_income == 1, 1, family_income_low)) %>%
    mutate(family_income_low = ifelse(family_income == 2, 5000, family_income_low)) %>%
    mutate(family_income_low = ifelse(family_income == 3, 7500, family_income_low)) %>%
    mutate(family_income_low = ifelse(family_income == 4, 10000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 5, 12500, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 6, 15000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 7, 20000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 8, 25000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 9, 30000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 10, 35000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 11, 40000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 12, 50000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 13, 60000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 14, 75000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 15, 100000, family_income_low)) %>%
    mutate(family_income_low = if_else(family_income == 16, 150000, family_income_low)) %>% # top one is entered as 150000 which is always an underestiamte
  
  # upper bound -- guarantees this is an underestimate 
    mutate(family_income_up = ifelse(family_income == -1, NA, family_income)) %>%
    mutate(family_income_up = ifelse(family_income == -2, NA, family_income_up)) %>%
    mutate(family_income_up = ifelse(family_income == -3, NA, family_income_up)) %>%
    mutate(family_income_up = ifelse(family_income == 1, 5000, family_income_up)) %>%
    mutate(family_income_up = ifelse(family_income == 2, 7500, family_income_up)) %>%
    mutate(family_income_up = ifelse(family_income == 3, 10000, family_income_up)) %>%
    mutate(family_income_up = ifelse(family_income == 4, 12500, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 5, 15000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 6, 20000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 7, 25000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 8, 30000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 9, 35000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 10, 40000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 11, 50000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 12, 60000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 13, 75000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 14, 100000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 15, 150000, family_income_up)) %>%
    mutate(family_income_up = if_else(family_income == 16, 150000, family_income_up)) # top one is entered as 150000 which is always an underestiamte
  
  return(df)
}


