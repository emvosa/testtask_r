# Title     : Suicides in Eastern Europe throughout the years per 100k population
# Objective : Filter and process the correct data to create a graph
# Created by: Emma Võsa

library(tidyverse)

data <- read_csv("master.csv")  # Read data into memory using relative path

# Suicides per 100 000 people grouped by countries throughout the years (per year)
suicides_per_country_year <- data %>%
  filter(year >= 1995, year <= 2015) %>%
  group_by(year, country) %>%
  summarise(suicides = sum(suicides_no), pop = sum(population))

# Graph of suicides per 100 000 people per country throughout the years
suicides_per_country_year <- suicides_per_country_year %>% ggplot(aes(color = country)) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country=="Estonia"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Russian Federation"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Latvia"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Lithuania"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Belarus"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Romania"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Ukraine"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Bulgaria"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  layer(geom = "line",
        data = suicides_per_country_year %>% filter(country == "Poland"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, (suicides/pop * 100000))) +
  labs(x = "Year",
       y = "Suicides (per 100k)",
       color = "Country",
       title = "Suicides in Eastern Europe")

# Function to render to png
save_graph <- function(plot, filename) {
  png(filename)
  print(plot)
  dev.off()
}

# Call function to render
save_graph(suicides_per_country_year, "suicides_in_eastern_europe.png")
