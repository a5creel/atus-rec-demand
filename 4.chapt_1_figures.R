# Get summary stats for prospectus chapters
# Andie Creel / Started on Nov 5th 2023

library(vroom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(fixest)
source('3.functions.R')
library(vroom)

# -----------------------------------------------------------------------------
# reading in data
# -----------------------------------------------------------------------------
mySummary_files <- list()

for (k in 3:21){
  
  myFile <- paste0("clean_data/ATUS_2003-2021/", 2000 + k, ".csv")
  mySummary_files[[k]] <- vroom(myFile) %>%
    mutate(year = 2000+k) # add in year
  
  #adjust for inflation, in 2021 dollars
  myInf <- vroom("clean_data/inflation_rates.csv")
  
  mySummary_files[[k]] <- left_join(mySummary_files[[k]], myInf, by = "year") %>%
    mutate(week_earning_nominal = week_earning) %>%
    mutate(week_earning = week_earning_nominal/adj_value)
}


# -----------------------------------------------------------------------------
# make data frame with averages
# -----------------------------------------------------------------------------

myYear <- 2003:2021
myMinutes <- data.frame(year = myYear, avg_min_per_day = NA)

# -----------------------------------------------------------------------------
# break down by race
# -----------------------------------------------------------------------------

for (y in 3:21) {
  myMinutes$avg_min_per_day[y-2] <- get_avg_rec_min(mySummary_files[[y]])
  
  # break down by race
  # 1 White only
  # 2 Black only
  # 3 American Indian, Alaskan Native only
  # 4 Asian only
  # 5 Hawaiian/Pacific Islander only 
  # https://www.bls.gov/tus/atuscpscodebk14.pdf
  
  myRaceList <- list()
  for (r in 1:5) {
    myRaceList[[r]] <- mySummary_files[[y]] %>%
      select(household_id, race, week_earning, sample_weight, total_min_rec) %>% 
      filter(race == r)
    
    if (r == 1){
      myMinutes$avg_min_per_day_white[y-2] <- get_avg_rec_min(myRaceList[[r]])
      
    } else if (r == 2){
      myMinutes$avg_min_per_day_black[y-2] <- get_avg_rec_min(myRaceList[[r]])
      
    } else if (r == 3){
      myMinutes$avg_min_per_day_native[y-2] <- get_avg_rec_min(myRaceList[[r]])
    } else if (r == 4){
      myMinutes$avg_min_per_day_asian[y-2] <- get_avg_rec_min(myRaceList[[r]])
    } else if (r == 5){
      myMinutes$avg_min_per_day_island[y-2] <- get_avg_rec_min(myRaceList[[r]])
    }
  }
}

# -----------------------------------------------------------------------------
# Country averages
# -----------------------------------------------------------------------------

myMinutes %>%
  ggplot(aes(x = year, y = avg_min_per_day)) +
  geom_point(colour = "black", size = 3, alpha = .5) +
  geom_smooth(aes(y = avg_min_per_day), method = "lm", fill = 'grey', color = "black", size = 1.5) +
  ggtitle("Americans recreate outside for 18-19 min per day") +
  ylab("Minutes") +
  xlab("Year")+
  labs(caption = "Data Source: ATUS Microdata Activity Summary files, '03-'21")+
  theme_bw() + 
  theme(text = element_text(size=17))
# coord_cartesian(ylim = c(0, 30))  #zoom


ggsave(width = 11, height = 8, filename = "figures/fall_2023_figs/minutes.jpeg")

# -----------------------------------------------------------------------------
# Plots: race
# -----------------------------------------------------------------------------
# pivot long to make graphs
myMinutes_long <- myMinutes %>%
  rename(Nation = avg_min_per_day) %>%
  rename(Black = avg_min_per_day_black) %>%
  rename(White = avg_min_per_day_white) %>%
  rename(Native = avg_min_per_day_native) %>%
  rename(Asian = avg_min_per_day_asian) %>%
  rename(Islander = avg_min_per_day_island) %>%
  pivot_longer(cols = c(Nation, White, Black,Native, Islander, Asian), values_to = "avg_min") %>%
  select(year, name, avg_min) %>%
  rename(Race = name)

#native and Islander
myMinutes_long %>%
  filter(Race == 'Islander' | Race == 'Nation' | Race == "Native" ) %>%
  ggplot(aes(x = year, y = avg_min, colour = Race)) +
  geom_smooth(method = "lm", fill = 'grey', size = 1.5) +
  geom_point(size = 2, alpha = .25) +
  ggtitle("Average daily time recreating outdoors") +
  ylab("Minutes") +
  xlab("Year")+
  # labs(caption = "Data Source: ATUS Microdata Activity Summary files, '03-'21")+
  theme_minimal() + 
  theme(text = element_text(size=20)) +
  scale_color_manual(values=c('#00BFC4',  'black', '#FF61CC'))

ggsave(width = 11, height = 8, filename = "figures/fall_2023_figs/minutes-islander-native.jpeg")

# White, asian, black
myMinutes_long %>%
  filter(Race == "Nation" | Race == 'White' | Race == 'Asian' | Race == 'Black') %>%
  ggplot(aes(x = year, y = avg_min, colour = Race)) +
  geom_smooth(method = "lm", fill = 'grey', size = 1.5) +
  geom_point(size = 2, alpha = .25) +
  ggtitle("Average daily time recreating outdoors") +
  ylab("Minutes") +
  xlab("Year")+
  theme_minimal() + 
  theme(text = element_text(size=20)) + 
   scale_color_manual(values=c('#F8766D', '#A3A500', 'black', '#00A9FF'))

ggsave(width = 11, height = 8, filename = "figures/fall_2023_figs/minutes-white-black-asian.jpeg")
