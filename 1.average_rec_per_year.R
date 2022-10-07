# Gretting average number of minutes spent recreating every year /
# Andie Creel / Started Oct. 6th, 2022

# -----------------------------------------------------------------------------
# loading packages 
# -----------------------------------------------------------------------------
rm(list = ls())
library(vroom)
library(dplyr)
library(ggplot2)


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
# plot
# -----------------------------------------------------------------------------

myAverages %>%
  ggplot(aes(x = year)) +
  # geom_point(colour = "black") +
  geom_smooth(aes(y = avg_min_per_day), method = "lm", fill = "grey", color = "darkred") +
  geom_smooth(aes(y = avg_min_per_day_white), method = "lm", fill = NA, color = "darkblue") +
  geom_smooth(aes(y = avg_min_per_day_black), method = "lm", fill = NA, color = "darkgreen") +
  # geom_smooth(aes(y = avg_min_per_day_native), method = "lm", fill = NA, color = "purple") +
  # geom_smooth(aes(y = avg_min_per_day_island), method = "lm", fill = NA, color = "coral") +
  ggtitle("Time spent recreating varries by race", "The avg. American saw a ~7% increase in outdoor recreation") +
  ylab("avg min recreating outside per day") +
  xlab("Year")+
  labs(caption = "Data Source: ATUS Microdata Activity Summary files, 2003-2021")+
  theme_bw() + 
  scale_colour_manual("", 
                      breaks = c("Population", "White", "Black"),
                      values = c("darkred", "darkblue", "darkgreen"))
# 
summary(myAverages$avg_min_per_day)



#NEED TO LABEL WHITE AND BLACK LINES!
