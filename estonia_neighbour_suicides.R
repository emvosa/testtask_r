# Title     : Suicides in Estonia and its bordering countries
# Objective : Filter and process the correct data to create a graph
# Created by: Emma Võsa

library(tidyverse)
library(gganimate)

data <- read_csv("master.csv")  # Read data into memory using relative path

# Suicides per 100 000 people grouped by countries throughout the years (per year)
suicides_per_country_year <- data %>%
  filter(year >= 1995, year <= 2015) %>%
  group_by(year, country) %>%
  summarise(suicides = sum(suicides_no), pop = sum(population))

# A collection of Estonia and its neighbouring countries
neighbours <- c("Estonia", "Sweden", "Finland", "Latvia", "Russian Federation")

# Filtering Estonia's and neighbouring countries' suicides per 100 000 people throughout 20 years
neighbour_suicides <- suicides_per_country_year %>%
  group_by(country, year) %>%
  filter(country %in% neighbours) %>%
  summarise(suicides = sum(suicides), pop = sum(pop))
neighbour_suicides <- as_tibble(
  cbind(neighbour_suicides, per100k = (neighbour_suicides$suicides / neighbour_suicides$pop * 100000))
) %>% arrange(year)

# Animation of number of suicides per 100 000 people per country
neighbour_suicides  %>%
  ggplot(aes(x = country, y = per100k, fill = country)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Country",
       y = "Suicides (per 100k)",
       title = "Suicides in Estonia and its neighbouring countries",
       subtitle = "Suicides per 100 000 people over 20 years",
       tag = "Year: {closest_state}",
       fill = "Country") +
  transition_states(year, transition_length = 2, state_length = 2)

anim_save("estonia_neighbour_suicides.gif")
