####library####

library(tidyverse)
library(janitor)
library(patchwork)
library(paletteer)


#### loading in data ####

episode_data <- read.csv("./drwho_episodes.csv")

#### cleaning data ####

data_clean <- episode_data |>
  rename(episode = "episode_number",
         season = "season_number",
         viewers = "uk_viewers")|>
  select(type, season, episode, viewers, rating, episode_title) |>
  mutate(index = row_number()) |>
  mutate(doctor = case_when(index > 0 & index <= 13 ~ "Ninth Doctor",
                            index > 13 & index <= 60 ~ "Tenth Doctor",
                            index > 60 & index <= 104 ~ "Eleventh Doctor",
                            index > 104 & index <= 144 ~ "Twelfth Doctor",
                            index > 144 & index <= 175 ~ "Thirteenth Doctor",
                            TRUE ~ "Other")) |>
  mutate(doctor = factor(doctor,
                         levels = c("Ninth Doctor", "Tenth Doctor", "Eleventh Doctor", "Twelfth Doctor", "Thirteenth Doctor")))|>
  mutate(adj_rating = rating / 10)


####part 1 graph 1####

overall_decline_graph <- ggplot(data_clean, aes(x = index, y = viewers)) +
  geom_smooth(se = TRUE, colour = "black") +
  geom_point(alpha = 0.5, colour = "black") +
  labs(title = "UK Viewership across the Modern Era of Doctor Who",
       caption = "Figure 1: Relationship between chronological episode number (n = 175) and UK viewership. Each point represents the number of \nUK viewers for an individual episode. The data has been fitted with a LOESS smoothed estimate of the relationship between \nviewership and episode number. The grey band shows the 95% confidence interval for the estimate.")+
  xlab("Chronological Episode Number")+
  ylab("Number of UK Viewers (Millions)") +
  scale_y_continuous(limits = c(0,14),
                     breaks = seq(0, 14, by = 1),
                     minor_breaks = NULL)+
  scale_x_continuous(breaks = seq(0, 175, by = 20))+
  theme_minimal() +
  theme(
    plot.caption = element_text(
      hjust = 0,
      colour = "black"))

overall_decline_graph


####part 1 graph 2####


doctor_trends <- ggplot(data_clean, aes(x = index, y = viewers))+
  geom_smooth(se = TRUE, aes(group = factor(doctor)), colour = "black", fill = "grey80")+
  geom_point(alpha = 0.5) +
  labs(title = "UK Viewership Trends by Doctor",
       caption = "Figure 2. Relationship between chronological episode number and UK viewership, split across iterations of the doctor. Each point represents the number of \nUK viewers for an individual episode. For each iteration of the doctor, the data has been fitted with a LOESS smoothed estimate of the relationship \nbetween viewership and episode number. The grey band shows the 95% confidence interval for the estimate.")+
  xlab("Chronological Episode Number")+
  ylab("Number of UK Viewers (Millions)")+
  geom_text(aes(x = 2, y = 2, label = "Ninth \n Doctor \n (Ep 1-13)"), colour = "black")+
  geom_text(aes(x = 37, y = 2, label = "Tenth \n Doctor \n (Ep 14-60)"), colour = "black") +
  geom_text(aes(x = 82.5, y = 2, label = "Eleventh \n Doctor \n (Ep 61-104)"), colour = "black") +
  geom_text(aes(x = 124.5, y = 2, label = "Twelfth \n Doctor \n (Ep 105-144)"), colour = "black")+
  geom_text(aes(x = 164.5, y = 2, label = "Thirteenth \n Doctor \n (Ep 145-175)"), colour = "black") +
  scale_y_continuous(limits = c(0,14),
                     breaks = seq(0, 14, by = 1),
                     minor_breaks = NULL) +
  scale_x_continuous(limits = c(0, 175),
                     breaks = seq(0, 175, by = 20))+
  theme_minimal() +
  theme(
    plot.caption = element_text(
      hjust = 0,
      colour = "black"))

doctor_trends



####part 2 graph 1####

season_summary <- data_clean |>
  filter(type == "episode") |>
  group_by(season)|>
  summarise(avg_view = mean(viewers),
            avg_rate = mean(rating),
            avg_adj_rate = mean(adj_rating),
            doctor = first(doctor))|>
  ungroup()

season_graph <- ggplot(season_summary, aes(x = factor(season), y = avg_view))+
  geom_col(aes(fill = factor(season)))+
  labs(title = "Doctor Who's Popularity is Decreasing")+
  xlab("Season Number")+
  ylab("Average Number of UK Viewers per Episode (Millions)")+
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,10)) 

season_graph

####part 2 graph 2####

season_summary <- data_clean |>
  filter(type == "episode") |>
  group_by(season)|>
  summarise(avg_view = mean(viewers),
            avg_rate = mean(rating),
            avg_adj_rate = mean(adj_rating),
            doctor = first(doctor))|>
  ungroup()

season_by_doctor_graph <- ggplot(season_summary, aes(x = factor(season), y = avg_view))+
  geom_col(aes(fill = factor(doctor)))+
  labs(title = "The Popularity of Doctor Who across Regenerations")+
  xlab("Season Number")+
  ylab("Average Number of UK Viewers per Episode (Millions)") +
  scale_y_continuous(limits = c(0,10)) +
  scale_fill_discrete(
    name = "Regeneration")
season_by_doctor_graph

####part 2 graph 3

summary_by_doctor <- data_clean |>
  group_by(doctor) |>
  summarise(doctor_rate = mean(rating),
            doctor_view = mean(viewers))

doctor_summary <- ggplot(summary_by_doctor, aes(x = doctor))+
  geom_col(aes(y = doctor_view, fill = factor(doctor))) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(title = "Overall Popularity across Regenerations")+
  xlab("Regeneration") +
  ylab("Average Number of UK Viewers per Episode (Millions)") +
  theme_minimal() +
  theme(legend.position = "none") 
doctor_summary
