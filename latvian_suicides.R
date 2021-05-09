# Title     : Latvian suicides throughout the years
# Objective : Filter and process the correct data to create an animated graph
# Created by: Emma Võsa

library(tidyverse)
library(gganimate)

data <- read_csv("master.csv")  # Read data into memory using relative path

latvia <- data %>% filter(country == "Latvia")  # Filter Latvian data

# Animation of Latvian suicides throughout the years
latvia %>%
  group_by(year) %>%
  summarise(Suicides = sum(suicides_no)) %>%
  ggplot(aes(year, Suicides)) +
  geom_line() +
  labs(x = "Year", y = "Suicides (total number)", title = "Suicides in Latvia") +
  theme(axis.title.x = element_text(color = "#000000", size = 14, face = "bold"),
        axis.title.y = element_text(color = "#FF0000", size = 14, face = "bold")) +
  geom_point(colour = 'red', size = 3) +
  transition_reveal(year)

# Render animation to gif
anim_save("latvian_suicides.gif")
