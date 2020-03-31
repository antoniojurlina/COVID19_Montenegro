#-------- packages --------
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(tidyr)

#-------- data and directory --------
directory <- paste0(here::here(), "/data")
setwd(directory)

load("montenegro_covid19.RData")

#-------- plots --------
directory <- paste0(here::here(), "/output")
setwd(directory)

plot <- covid_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>% 
  pivot_longer(cols = 2:3, names_to = "type", values_to = "cases") %>%
  mutate(type = recode(type, "confirmed" = "Total cases", "deaths" = "Deaths")) %>% 
  ggplot(aes(x = date, y = cases, 
             color = factor(type, levels = c("Total cases", "Deaths")), 
             fill = factor(type, levels = c("Total cases", "Deaths")))) +
  geom_col() +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Cases in Montenegro",
       subtitle = paste0("as of ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("COVID-19 Cases in Montenegro.jpeg", plot, height = 8, width = 7)




