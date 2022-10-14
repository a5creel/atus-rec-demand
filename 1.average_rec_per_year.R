# Gretting average number of minutes spent recreating every year /
# Andie Creel / Started Oct. 6th, 2022

# -----------------------------------------------------------------------------
# loading packages 
# -----------------------------------------------------------------------------
rm(list = ls())
library(vroom)
library(dplyr)
library(ggplot2)
library(tidyr)


# -----------------------------------------------------------------------------
# reading in data
# -----------------------------------------------------------------------------
myList <- list()

for (k in 3:21){

  myFile <- paste0("clean_data/ATUS_2003-2021/", 2000 + k, ".csv")
  myList[[k]] <- vroom(myFile)
  
}

# -----------------------------------------------------------------------------
# function
# -----------------------------------------------------------------------------

# Where I got the caluation for average take spent recreating per day
# In zotero: “American Time Use Survey User’s Guide,” 2022, 125.

get_avg_rec_min <- function(df){
  sum(df$sample_weight*df$total_min_rec)/sum(df$sample_weight)
}

# -----------------------------------------------------------------------------
# make dataframe with averages
# -----------------------------------------------------------------------------

myYear <- 2003:2021
myAverages <- data.frame(year = myYear, avg_min_per_day = NA)

for (y in 3:21) {
  myAverages$avg_min_per_day[y-2] <- get_avg_rec_min(myList[[y]])
  
  # -----------------------------------------------------------------------------
  # break down by race
  # 1 White only
  # 2 Black only
  # 3 American Indian, Alaskan Native only
  # 4 Asian only
  # 5 Hawaiian/Pacific Islander only 
  # https://www.bls.gov/tus/atuscpscodebk14.pdf
  
  myRaceList <- list()
  for (r in 1:5) {
    myRaceList[[r]] <- myList[[y]] %>%
      select(household_id, race, week_earning, sample_weight, total_min_rec) %>% 
      filter(race == r)
    
    if (r == 1){
      myAverages$avg_min_per_day_white[y-2] <- get_avg_rec_min(myRaceList[[r]])
      
    } else if (r == 2){
      myAverages$avg_min_per_day_black[y-2] <- get_avg_rec_min(myRaceList[[r]])
      
    } else if (r == 3){
      myAverages$avg_min_per_day_native[y-2] <- get_avg_rec_min(myRaceList[[r]])
    } else if (r == 4){
      myAverages$avg_min_per_day_asian[y-2] <- get_avg_rec_min(myRaceList[[r]])
    } else if (r == 5){
      myAverages$avg_min_per_day_island[y-2] <- get_avg_rec_min(myRaceList[[r]])
    }
    
  }
  # -----------------------------------------------------------------------------
  
  
}

# -----------------------------------------------------------------------------
# plots
# -----------------------------------------------------------------------------

# Country Averages  -------------------

myAverages %>%
  ggplot(aes(x = year, y = avg_min_per_day)) +
  geom_point(colour = "black", size = 3) +
  geom_smooth(aes(y = avg_min_per_day), method = "lm", fill = "grey", color = "darkred", size = 1.5) +
  ggtitle("Time recreating may be increasing", "Recreation increased ~7% from 2003-2021") +
  ylab("Minutes") +
  xlab("Year")+
  labs(caption = "Data Source: ATUS Microdata Activity Summary files, 2003-2021")+
  theme_bw() + 
  scale_colour_manual("", 
                      breaks = c("Population", "White", "Black"),
                      values = c("darkred", "darkblue", "darkgreen")) +
  theme(text = element_text(size=30))
 
ggsave(width = 11, height = 8, filename = "figures/country-average.jpeg")

# Race -------------------

#pivot long
myAverages_thin <- myAverages %>%
  select(year, avg_min_per_day, avg_min_per_day_black, avg_min_per_day_white)

myAverages_long <- myAverages %>%
  select(year, avg_min_per_day, avg_min_per_day_black, avg_min_per_day_white) %>%
  rename(All = avg_min_per_day) %>%
  rename(Black = avg_min_per_day_black) %>%
  rename(White = avg_min_per_day_white) %>%
  pivot_longer(cols = c(All, White, Black), values_to = "avg_min") %>%
  select(year, name, avg_min) %>%
  rename(Race = name)

ggplot(myAverages_long, aes(x = year, y = avg_min, colour = Race)) +
  geom_smooth(method = "lm", fill = "Grey", size = 1.5) +
  ggtitle("Time recreating varries by race") +
  ylab("Minutes") +
  xlab("Year")+
  labs(caption = "Data Source: ATUS Microdata Activity Summary files, 2003-2021")+
  theme_bw() + 
  theme(text = element_text(size=30))

ggsave(width = 13, height = 8, filename = "figures/race-average.jpeg")

