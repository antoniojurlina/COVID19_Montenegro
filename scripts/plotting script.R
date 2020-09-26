#-------- packages --------
library(tidyverse)
library(tidylog)
library(scales)
library(ggthemes)

#-------- data and directory --------
paste0(here::here(), "/data") %>% setwd()

load("montenegro_covid19.RData")

#-------- plots --------
paste0(here::here(), "/output") %>% setwd()

plot <- covid19_data %>%
  ggplot(aes(x = date, y = confirmed, color = municipality, group = municipality)) +
  geom_line() +
  theme_wsj()

ggsave("municipalities.jpeg", plot, height = 7, width = 9)

plot <- covid19_data %>% 
  group_by(date) %>%
  summarize(pcr = first(pcr),
            confirmed = sum(confirmed, na.rm = TRUE)) %>% 
  mutate(confirmed_diff = c(2, diff(confirmed, lag = 1))) %>%
  ungroup() %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = confirmed_diff), width = 0.5, color = "#CC3311") +
  geom_line(aes(y = pcr), size = 0.2) +
  geom_vline(aes(xintercept = covid19_data$date[2600]), color = "#0077BB", size = 5, alpha = 0.3) +
  annotate("text", x = covid19_data$date[2450], y = 780, label = "Election", color = "black", size = 4) +
  theme_wsj() 

ggsave("election.jpg", plot, height = 7, width = 9)

plot <- covid19_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE)) %>% 
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>%
  mutate(type = recode(type, "confirmed" = "Confirmed",
                             "recovered" = "Recovered",
                             "deaths" = "Deaths")) %>% 
  ungroup() %>%
  ggplot(aes(x = date, y = cases, 
             color = factor(type, levels = c("Confirmed", "Recovered", "Deaths")), 
             fill = factor(type, levels = c("Confirmed", "Recovered", "Deaths")))) +
  geom_area(position = "dodge") +
  scale_color_wsj() +
  scale_fill_wsj() +
  theme_wsj() +
  scale_y_continuous(label = comma) +
  labs(title = "COVID-19 Cases in Montenegro",
       subtitle = paste0("updated ", Sys.Date())) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("COVID-19 Cases in Montenegro.jpeg", plot, height = 7, width = 9)

plot <- covid19_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE),
            pcr = first(pcr)) %>% 
  mutate(confirmed_diff = c(2, diff(confirmed, lag = 1)),
         deaths_diff = c(0, diff(deaths, lag = 1)),
         recovered_diff = c(0, diff(recovered, lag = 1))) %>% 
  ungroup() %>% 
  select(date, 
         confirmed = confirmed_diff, 
         deaths = deaths_diff, 
         recovered = recovered_diff,
        `# of RT-PCR test` = pcr) %>%
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>% 
  mutate(type = recode(type, "confirmed" = "confirmed",
                             "recovered" = "recovered",
                             "deaths" = "deaths")) %>% 
  ggplot(aes(x = date, y = cases, 
             color = factor(type, levels = c("confirmed", "recovered", "deaths")), 
             fill = factor(type, levels = c("confirmed", "recovered", "deaths")))) +
  geom_col(width = 0.5) +
  geom_line(aes(y = `# of RT-PCR test`), color = "black", size = 0.3, alpha = 0.5) +
  scale_color_wsj() +
  scale_fill_wsj() +
  theme_wsj() +
  labs(title = "COVID-19 Daily New Cases",
       subtitle = paste0("updated ", Sys.Date())) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "bottom")

ggsave("COVID-19 New Cases in Montenegro.jpeg", plot, height = 6, width = 9)


max_y <- covid19_data %>%
               group_by(date) %>%
               summarize(confirmed = sum(confirmed, na.rm = TRUE)) %>%
               select(confirmed) %>%
               max() + 10

plot <- covid19_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>% 
  mutate(thirty = cumprod(c(2, rep(1.15, length(confirmed)-1)))) %>% 
  ungroup() %>%
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>% 
  mutate(type = recode(type, "confirmed" = "Ukupno zaraženih", 
                             "deaths" = "Preminuli",
                             "thirty" = "Dnevni rast od 15%")) %>% 
  ggplot(aes(x = date, y = cases, color = type, fill = type, group = type)) +
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

plot <- covid19_data %>%
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
  geom_text(nudge_y = 30) +
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

plot <- plot + geom_point(data = covid19_data %>%
                  filter(date == last(date)), 
                  aes(x = long, y = lat, size = confirmed),
                  show.legend = F, alpha = 0.7, color = "#004488")

plot <- plot + geom_point(data = covid19_data %>%
                            filter(date == last(date)), 
                          aes(x = long, y = lat, size = active),
                          show.legend = F, alpha = 0.7, color = "#BB5566")

plot <- plot + scale_size(range = c(2, 35))

ggsave("COVID-19 Montenegro Map.jpeg", plot, height = 8, width = 8)

plot <- covid19_data %>%
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
  geom_text(nudge_y = 1.8) +
  theme_par() +
  ylab("Ukupno zaraženih i oporavljenih (na svakih 5000 stanovnika)") +
  labs(title = "Broj zaraženih i oporavljenih (na svakih 5000 stanovnika) po opštini u Crnoj Gori",
       subtitle = paste0("ažurirano ", Sys.Date()),
       caption = "https://www.ijzcg.me/me/ncov") +
  theme(axis.title.y = element_blank())

ggsave("COVID-19 Municipality Data in Montenegro 2.jpeg", plot, height = 8, width = 10)

plot <- covid19_data %>%
  group_by(date) %>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE),
            recovered = sum(recovered, na.rm = TRUE),
            population_mne = mean(population_mne, na.rm = TRUE)) %>% 
  mutate(confirmed = 100000 * confirmed / population_mne,
         deaths = 100000 * deaths / population_mne,
         recovered = 100000 * recovered / population_mne) %>% 
  pivot_longer(cols = 2:4, names_to = "type", values_to = "cases") %>%
  mutate(type = recode(type, "confirmed" = "Confirmed",
                             "recovered" = "Recovered",
                             "deaths" = "Deaths")) %>% 
  ungroup() %>%
  ggplot(aes(x = date, y = cases, 
             color = factor(type, levels = c("Confirmed", "Recovered", "Deaths")), 
             fill = factor(type, levels = c("Confirmed", "Recovered", "Deaths")))) +
  geom_area(position = "dodge") +
  scale_color_wsj() +
  scale_fill_wsj() +
  scale_y_continuous(label = comma) +
  theme_wsj() +
  labs(title = "per 100,000.jpeg",
       subtitle = paste0("updated ", Sys.Date())) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("COVID-19 Cases in Montenegro 2.jpeg", plot, height = 7, width = 9)
 


  
 
  



