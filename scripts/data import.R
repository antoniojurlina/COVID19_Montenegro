#-------- packages --------
library(readr)
library(dplyr)
library(lubridate)

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

covid_data <- read_csv("montenegro_covid19.csv") %>%
  mutate(date = mdy(date))

# save the data frame for further analysis
save(covid_data, file = "montenegro_covid19.RData")
