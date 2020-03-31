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
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih", "deaths" = "Preminuli")) %>% 
  ungroup() %>%
  ggplot(aes(x = factor(date), y = cases, 
             color = factor(type, levels = c("Ukupno zaraženih", "Preminuli")), 
             fill = factor(type, levels = c("Ukupno zaraženih", "Preminuli")))) +
  geom_col() +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Broj zaraženih u Crnoj Gori",
       subtitle = paste0("od ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("COVID-19 Cases in Montenegro.jpeg", plot, height = 8, width = 7)

plot <- covid_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>% 
  mutate(confirmed_diff = c(2, diff(confirmed, lag = 1)),
         deaths_diff = c(0, diff(deaths, lag = 1))) %>% 
  ungroup() %>%
  select(date, confirmed = confirmed_diff, deaths = deaths_diff) %>%
  pivot_longer(cols = 2:3, names_to = "type", values_to = "cases") %>%
  mutate(type = recode(type, "confirmed" = "Novi slučajevi", "deaths" = "Preminuli")) %>% 
  ggplot(aes(x = factor(date), y = cases, 
             color = factor(type, levels = c("Novi slučajevi", "Preminuli")), 
             fill = factor(type, levels = c("Novi slučajevi", "Preminuli")))) +
  geom_col(width = 0.5) +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Dnevno stanje u Crnoj Gori",
       subtitle = paste0("od ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom")

ggsave("COVID-19 New Cases in Montenegro.jpeg", plot, height = 8, width = 7)


max_y <- covid_data %>%
               group_by(date) %>%
               summarize(confirmed = sum(confirmed, na.rm = TRUE)) %>%
               select(confirmed) %>%
               max() + 10

plot <- covid_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>% 
  mutate(thirty = cumprod(c(2, rep(1.33, length(confirmed)-1)))) %>% 
  ungroup() %>%
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>% 
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih", 
                             "deaths" = "Preminuli",
                             "thirty" = "Dnevni rast od 33%")) %>% 
  ggplot(aes(x = factor(date), y = cases, color = type, fill = type, group = type)) +
  geom_line() +
  geom_vline(aes(xintercept = 14), alpha = 0.3) +
  geom_vline(aes(xintercept = 3), alpha = 0.3) +
  geom_point(size = 0.3) +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Rast u Crnoj Gori",
       subtitle = paste0("do ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom")

plot <- plot + annotate("text", x = 14, y = max_y+5, label = "Policijski čas", color = "gray20", size = 4)
plot <- plot + annotate("text", x = 3, y = max_y+5, label = "Vojna pomoć", color = "gray20", size = 4)

ggsave("COVID-19 Growth in Montenegro.jpeg", plot, height = 8, width = 10)
 
  



