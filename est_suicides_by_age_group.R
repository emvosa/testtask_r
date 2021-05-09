# Title     : Suicides in Estonia throughout the years by age groups
# Objective : Filter and process the correct data to create a graph
# Created by: Emma Võsa

library(tidyverse)

data <- read_csv("master.csv")  # Read data into memory using relative path

estonia <- data %>% filter(country == "Estonia")  # Filter Estonian data

# Get data for Estonian suicides by age groups
age_groups <- estonia %>%
  group_by(age, year) %>%
  summarise(suicides = sum(suicides_no), total_pop = sum(population)) %>%
  arrange(year)

# Get per 100k for each age group
# Original data has per 100k for each gender in each age group
age_groups <- as_tibble(cbind(age_groups, per100k = (age_groups$suicides / age_groups$total_pop * 100000)))

# Graph of suicides per 100 000 people per age group
age_groups <- age_groups %>%
  ggplot(aes()) +
  layer(geom = "path",
        data = age_groups %>% filter(age == "15-24 years"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, per100k, color = "15-24 years")) +
  layer(geom = "path",
        data = age_groups %>% filter(age == "25-34 years"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, per100k, color = "25-34 years")) +
  layer(geom = "path",
        data = age_groups %>% filter(age == "35-54 years"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, per100k, color = "35-54 years")) +
  layer(geom = "path",
        data = age_groups %>% filter(age == "5-14 years"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, per100k, color = " 5-14 years")) +
  layer(geom = "path",
        data = age_groups %>% filter(age == "55-74 years"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, per100k, color = "55-74 years")) +
  layer(geom = "path",
        data = age_groups %>% filter(age == "75+ years"),
        stat = "identity",
        position = "identity",
        mapping = aes(year, per100k, color = "75+ years")) +
  labs(x = "Year",
       y = "Suicides (per 100k)",
       color = "Demographic",
       title = "Suicides by age groups in Estonia")

# Function to render to png
save_graph <- function(plot, filename) {
  png(filename)
  print(plot)
  dev.off()
}

# Call function to render
save_graph(age_groups, "est_suicides_by_age_group.png")
