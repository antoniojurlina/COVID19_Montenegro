#-------- packages --------
library(tidyverse)

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

covid_data <- read_csv("montenegro_covid19.csv") %>%
  mutate(date = lubridate::mdy(date))

# save the data frame for further analysis
save(covid_data, file = "montenegro_covid19.RData")
