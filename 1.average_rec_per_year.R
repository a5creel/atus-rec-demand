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

for (k in 3:21) {
  myAverages$avg_min_per_day[k-2] <- get_avg_rec_min(myList[[k]])
  
}

myAverages %>%
  ggplot(aes(x = year, y = avg_min_per_day)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", fill = "grey", color = "darkred") +
  ggtitle("Time spent outdoors may be increasing", "The avg. American saw a ~7% increase in outdoor recreation") +
  ylab("avg min recreating outside per day") +
  xlab("Year")+
  theme_bw()

summary(myAverages$avg_min_per_day)


