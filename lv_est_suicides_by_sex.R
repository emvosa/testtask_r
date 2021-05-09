# Title     : Suicides in Latvia and Estonia throughout the years by sex
# Objective : Filter and process the correct data to create an animated graph
# Created by: Emma Võsa

library(tidyverse)
library(gganimate)

data <- read_csv("master.csv")  # Read data into memory using relative path

estonia <- data %>% filter(country == "Estonia")  # Filter Estonian data
latvia <- data %>% filter(country == "Latvia")  # Filter Latvian data

# A collection of Estonia and Latvia
ee_lv <- c("Estonia", "Latvia")

# Get data for Estonian and Latvian suicides by year and sex
by_year_sex_ee_lv <- data %>%
  filter(country %in% ee_lv) %>%
  group_by(sex, year, country) %>%
  summarise(suicides = sum(suicides_no))

# Animation of total number of suicides of Latvian and Estonian women and men separately throughout the years
by_year_sex_ee_lv %>%
  ggplot(aes()) +
  layer(geom = "path",
        data = by_year_sex_ee_lv %>% filter(sex == "female" & country == "Estonia"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, suicides, color = "Estonian women")) +
  layer(geom = "path",
        data = by_year_sex_ee_lv %>% filter(sex == "male" & country == "Estonia"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, suicides, color = "Estonian men")) +
  layer(geom = "path",
        data = by_year_sex_ee_lv %>% filter(sex == "female" & country == "Latvia"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, suicides, color = "Latvian women")) +
  layer(geom = "path",
        data = by_year_sex_ee_lv %>% filter(sex == "male" & country == "Latvia"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, suicides, color = "Latvian men")) +
  labs(x = "Year",
       y = "Suicides (total number)",
       color = "Demographic",
       title = "Suicides of Latvians and Estonians by sex") +
  transition_reveal(year)

# Render animation to gif
anim_save("lv_est_suicides_by_sex.gif")
