# Title     : Estonian suicides throughout the years
# Objective : Filter and process the correct data to create an animated graph
# Created by: Emma Võsa

library(tidyverse)
library(gganimate)

data <- read_csv("master.csv")  # Read data into memory using relative path

estonia <- data %>% filter(country == "Estonia")  # Filter Estonian data

# Animation of Estonian suicides through the years
estonia %>%
  group_by(year) %>%
  summarise(Suicides = sum(suicides_no)) %>%
  ggplot(aes(year, Suicides)) +
  geom_line() +
  labs(x = "Year", y = "Suicides (total number)", title = "Suicides in Estonia") +
  theme(axis.title.x = element_text(color = "#000000", size = 14, face = "bold"),
        axis.title.y = element_text(color = "#0000FF", size = 14, face = "bold")) +
  geom_point(colour = 'blue', size = 3) +
  transition_reveal(year)

# Render animation to gif
anim_save("estonian_suicides.gif")
