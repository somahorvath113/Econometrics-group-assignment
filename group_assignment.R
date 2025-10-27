rm(list = ls())

library(readxl)
library(tidyverse)
library(dplyr)
setwd("D://Egyetem//Econometrics//Group_assignment")
data <- read_excel("data.xlsx")


new_names <- c("date", "city", "comp", "address", "diesel", "gasoline")

data <- data %>% rename_with(~ new_names)

# creating the averages 

data_avg <- data %>%
  group_by(address) %>%
  summarise(
    avg_gasoline = mean(gasoline, na.rm = TRUE),
    avg_diesel   = mean(diesel, na.rm = TRUE),
    comp = first(comp),
    city = first(city)
  )

# number of gas stations in the city variable

n_station <- data %>%
  group_by(city) %>%
  summarise(
    n_stations = n_distinct(address)
  )

print(n_station)

# add it to the data

data_avg <- data_avg %>%
  group_by(city) %>%
  mutate(n_stations = n_distinct(address)) %>%
  ungroup()
