# Intial data exploration / Andie Creel / Started Sept 15 2022

# -----------------------------------------------------------------------------
# loading packages 


# The 2003-21 Activity Summary file contains data for the total number of minutes that each respondent spent doing each 6-digit activity.  
# Each record corresponds to a unique respondent, as indicated by a unique value of the variable TUCASEID. 
# -----------------------------------------------------------------------------
library(vroom)
library(dplyr)


# -----------------------------------------------------------------------------
# reading in data 

# The 2003-21 Activity Summary file contains data for the total number of minutes that each respondent spent doing each 6-digit activity.  
# Each record corresponds to a unique respondent, as indicated by a unique value of the variable TUCASEID.  

# The variable names identify activities by their 6-digit codes.  The letter "t" precedes the 6-digit codes.  The first and second digits 
# of the 6-digit code correspond to the variable trtier1p; the first, second, third and fourth digits of the 6-digit code correspond to the 
# variable trtier2p; and, all six digits of the 6-digit code correspond to the variable trcodep.  For example, the variable "t040102" 
# refers to the activity with TRTIER1P=04, TRTIER2P=0401, and TRCODEP=040102.  As documented in the ATUS 2003-21 Coding Lexicon 
# (https://www.bls.gov/tus/lexiconnoex0321.pdf), activity code "040102" corresponds to "Caring for and helping nonhousehold members/
# Caring for and helping nonhousehold children/Reading to or with nonhousehold children." See Chapter 5 of the ATUS User's Guide 
# (https://www.bls.gov/tus/atususersguide.pdf) for more information about the ATUS Coding Lexicon. 
# 
# In addition to the activity code variables (t010101...t509999), the file atussum_0321.dat includes selected variables (such as age, sex, 
# and labor force status of the respondent) from the Respondent and ATUS-CPS files.  The definitions for variables that begin with the letter 
# "P" or "G" appear in the 2003-21 ATUS-CPS Data Dictionary, available at https://www.bls.gov/tus/atuscpscodebk0321.pdf; the definitions 
# for variables that begin with the letter "T" (other than t010101...t509999) appear in the 2003-21 ATUS Interview Data Dictionary, available at 
# https://www.bls.gov/tus/atusintcodebk0321.pdf.  
# -----------------------------------------------------------------------------

myOG <- vroom('raw_data/atussum-0321/atussum_0321.dat')  

myWorking <- myOG %>%
  rename(race = PTDTRACE) %>%
  rename(week_earning = TRERNWA) %>%
  rename(household_id = TUCASEID) %>%
  select(household_id, race, week_earning, starts_with("t1301")) #Sports, Exercise, & Recreation


