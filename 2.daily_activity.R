# Getting daily activity /
# Andie Creel / Started Nov. 23rd, 2022

# -----------------------------------------------------------------------------
# loading packages 
# -----------------------------------------------------------------------------
rm(list = ls())
library(vroom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


# -----------------------------------------------------------------------------
# reading in data
# -----------------------------------------------------------------------------

#read in activity file (daily diary)
myAct_21_og <-vroom("raw_data/atusact-2021/atusact_2021.dat")

# read in respondent file (has the weights)
myResp_21_og <- vroom("raw_data/atusresp-2021/atusresp_2021.dat")

# read in cps file (has state, fips info)
myCPS_21_og <- vroom("raw_data/atuscps-2021/atuscps_2021.dat")

# -----------------------------------------------------------------------------
# variable selection 
# -----------------------------------------------------------------------------

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
myAct_21 <- myAct_21_og %>%
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


# -----------------------------------------------------------------------------
# Trying to pivot so I can regress minutes recreating on minutes traveled 
# -----------------------------------------------------------------------------

#pivoting wider to inspect activity order (want to check for travel on both sides of recreation)
myAct_21_pivot <- myAct_21 %>%
  select(id, duration, act_number, act_code_short) %>%
  pivot_wider(id_cols =  id, names_from = act_code_short, values_from = c(act_number, duration)) %>%
  filter(act_number_1813 != "NULL") %>% 
  filter(act_number_1301 != "NULL") 
# (a visual inspection shows that most activities are "sandwiched" (travel comes before or after activity))
# relies on dropping the nulls, an individual needs to participate in the recreation I'm interested in and travel to it to be included in the dataset
  
# sum duration spent traveling and duration recreating for each person 
# travel there and back are summed, different kind of recreation are summed 
myAct_21_pivot$duration_travel <- sapply(myAct_21_pivot$duration_1813, sum)  
myAct_21_pivot$duration_rec <- sapply(myAct_21_pivot$duration_1301, sum)  


# -----------------------------------------------------------------------------
# mergin in other variables  
# -----------------------------------------------------------------------------

# RACE, INCOME FIPS -----------------------

myCPS <- myCPS_21_og %>%
  select(TUCASEID, PTDTRACE, HEFAMINC, GESTFIPS, GTCO) %>%
  rename(id = TUCASEID) %>% # ATUS Case ID (14-digit identifier)
  rename(race = PTDTRACE) %>% # Race (topcoded)
  rename(family_income = HEFAMINC) %>% # Edited: Family Income
  rename(fips_st = GESTFIPS) %>% # Federal Processing Information Standards (FIPS) state code
  rename(fips_co = GTCO) %>% # Federal Processing Standards (FIPS) county code
  distinct()

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

# merge 
myWorking_21 <- left_join(myAct_21_pivot, myCPS, by = "id")
  
# WEIGHTS -----------------------

myResp <- myResp_21_og %>%
  select(TUCASEID, TUFINLWGT) %>% 
  rename(id = TUCASEID) %>% # ATUS Case ID (14-digit identifier)
  rename(weight = TUFINLWGT) # ATUS final weight
  
myWorking_21 <- left_join(myWorking_21, myResp, by = "id")


# NEXT STEPS: 
# - FIGURE OUT WHAT TO DO WITH WEIGHTS
# - FIGURE OUT HOW TO BRING IN MORE YEARS
# - RUN INITIAL REGRESSIONS?? 





