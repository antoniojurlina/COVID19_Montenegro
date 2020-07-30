#-------- packages --------
library(tidyverse)
library(rebus)
library(tidylog)

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

covid_data <- read_csv("montenegro_covid19.csv") %>%
  mutate(date = lubridate::mdy(date),
         `pop/km_sq` = population / km_sq,
         `pop/km_sq_mne` = population_mne / km_sq_mne) %>%
  filter(!is.na(date), 
         date <= Sys.Date())


# save the data frame for further analysis
save(covid_data, file = "montenegro_covid19.RData")


