#-------- packages --------
library(tidyverse)
library(tidylog)
library(scales)
library(ggthemes)

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
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE)) %>% 
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>%
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih",
                             "recovered" = "Oporavljeni",
                             "deaths" = "Preminuli")) %>% 
  ungroup() %>%
  ggplot(aes(x = factor(date), y = cases, 
             color = factor(type, levels = c("Ukupno zaraženih", "Oporavljeni", "Preminuli")), 
             fill = factor(type, levels = c("Ukupno zaraženih", "Oporavljeni", "Preminuli")))) +
  geom_col() +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Broj zaraženih u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
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
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE)) %>% 
  mutate(confirmed_diff = c(2, diff(confirmed, lag = 1)),
         deaths_diff = c(0, diff(deaths, lag = 1)),
         recovered_diff = c(0, diff(recovered, lag = 1))) %>% 
  ungroup() %>% 
  select(date, confirmed = confirmed_diff, deaths = deaths_diff, recovered = recovered_diff) %>%
  pivot_longer(cols = 2:3, names_to = "type", values_to = "cases") %>% 
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih",
                             "recovered" = "Oporavljeni",
                             "deaths" = "Preminuli")) %>% 
  ggplot(aes(x = factor(date), y = cases, 
             color = factor(type, levels = c("Ukupno zaraženih", "Oporavljeni", "Preminuli")), 
             fill = factor(type, levels = c("Ukupno zaraženih", "Oporavljeni", "Preminuli")))) +
  geom_col(width = 0.5) +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Dnevno stanje u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
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
  mutate(thirty = cumprod(c(2, rep(1.15, length(confirmed)-1)))) %>% 
  ungroup() %>%
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>% 
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih", 
                             "deaths" = "Preminuli",
                             "thirty" = "Dnevni rast od 15%")) %>% 
  ggplot(aes(x = factor(date), y = cases, color = type, fill = type, group = type)) +
  geom_line() +
  geom_vline(aes(xintercept = 14), alpha = 0.3) +
  geom_vline(aes(xintercept = 3), alpha = 0.3) +
  geom_point(size = 0.3) +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Rast u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom")

plot <- plot + annotate("text", x = 14, y = max_y+5, label = "Policijski čas", color = "gray20", size = 4)
plot <- plot + annotate("text", x = 3, y = max_y+5, label = "Vojna pomoć", color = "gray20", size = 4)

ggsave("COVID-19 Growth in Montenegro.jpeg", plot, height = 8, width = 11)

plot <- covid_data %>%
  group_by(date, municipality) %>%
  summarize(confirmed = mean(confirmed, na.rm = TRUE),
            recovered = mean(recovered, na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(date == last(date)) %>% 
  arrange(desc(confirmed)) %>% 
  ggplot(aes(x = reorder(municipality, -confirmed), y = confirmed, label = confirmed)) +
  geom_col(fill = "#004488", colour = "#004488") +
  geom_col(aes(x = reorder(municipality, -confirmed), y = recovered),
           fill = "#DDAA33", colour = "#DDAA33") +
  coord_flip() +
  geom_text(nudge_y = 2.4) +
  theme_par() +
  ylab("Ukupno zaraženih i oporavljenih") +
  labs(title = "Broj zaraženih i oporavljenih po opštini u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.y = element_blank())

ggsave("COVID-19 Municipality Data in Montenegro.jpeg", plot, height = 8, width = 10)

plot <- map_data("world") %>% 
  filter(region == "Montenegro") %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "#DDAA33", color = "#004488", size = 0.3) +
  coord_fixed(ratio = 1.5) +
  theme_map()

plot <- plot + geom_point(data = covid_data %>%
                  filter(date == last(date)), 
                  aes(x = long, y = lat, size = confirmed),
                  show.legend = F, alpha = 0.7, color = "#004488")

plot <- plot + geom_point(data = covid_data %>%
                            filter(date == last(date)), 
                          aes(x = long, y = lat, size = active),
                          show.legend = F, alpha = 0.7, color = "#BB5566")

plot <- plot + scale_size(range = c(2, 35))

ggsave("COVID-19 Montenegro Map.jpeg", plot, height = 8, width = 8)

plot <- covid_data %>%
  mutate(`confirmed/5000 people` = 5000 * confirmed / population,
         `recovered/5000 people` = 5000 * recovered / population) %>% 
  group_by(date, municipality) %>%
  summarize(confirmed = round(mean(`confirmed/5000 people`, na.rm = TRUE), 2),
            recovered = round(mean(`recovered/5000 people`, na.rm = TRUE), 2)) %>% 
  ungroup() %>%
  filter(date == last(date)) %>% 
  arrange(desc(confirmed)) %>% 
  ggplot(aes(x = reorder(municipality, -confirmed), y = confirmed, label = confirmed)) +
  geom_col(show.legend = F, fill = "#004488", colour = "#004488") +
  geom_col(aes(x = reorder(municipality, -confirmed), y = recovered),
           fill = "#DDAA33", colour = "#DDAA33") +
  coord_flip() +
  geom_text(nudge_y = 0.6) +
  theme_par() +
  ylab("Ukupno zaraženih i oporavljenih (na svakih 5000 stanovnika)") +
  labs(title = "Broj zaraženih i oporavljenih (na svakih 5000 stanovnika) po opštini u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.y = element_blank())

ggsave("COVID-19 Municipality Data in Montenegro 2.jpeg", plot, height = 8, width = 10)

plot <- covid_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE),
            population_mne = mean(population_mne, na.rm = TRUE)) %>% 
  mutate(confirmed = 100000 * confirmed / population_mne,
         deaths = 100000 * deaths / population_mne,
         recovered = 100000 * recovered / population_mne) %>% 
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>%
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih", 
                             "deaths" = "Preminuli", 
                             "recovered" = "Oporavljeni")) %>% 
  ungroup() %>%
  ggplot(aes(x = factor(date), y = cases, 
             color = factor(type, levels = c("Ukupno zaraženih", "Oporavljeni", "Preminuli")), 
             fill = factor(type, levels = c("Ukupno zaraženih", "Oporavljeni", "Preminuli")))) +
  geom_col() +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_par() +
  labs(title = "COVID-19 Broj zaraženih (na svakih 100,000 stanovnika) u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("COVID-19 Cases in Montenegro 2.jpeg", plot, height = 8, width = 7)
 


  
 
  



