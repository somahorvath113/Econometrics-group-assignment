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


# second database, rename etc
data_rest <- read_excel("data2.xlsx")

new_names2 <- c("city", "hcso", "legal_stat", "county", "dist_code", "dist_name",
                "dist_seat", "office_code", "council_seat", "area", "pop", "dwellings", 
                "area_size", "bulgarian", "greek", "croatian", "polish", "german", 
                "armenian", "roma", "romanian", "ruthenian", "serbian", "slovakian", 
                "slovenian", "ukrainian")

data_rest <- data_rest %>% rename_with(~ new_names2)

# merge the two datasets
merged_data <- inner_join(data_avg, data_rest, by = "city")

# new variable, area/gas station

merged_data$areapstatiom <- merged_data$area/merged_data$n_stations

model1 <- lm(avg_gasoline ~ avg_diesel  + dwellings + pop + areapstatiom, data = merged_data)
summary(model1)

# income data
county_data <- read_excel("data3.xlsx")
new_names3 <- c("kod", "county", "region", "inc")
county_data <- county_data %>% rename_with(~ new_names3)

merged_data <- inner_join(merged_data, county_data, by = "county")

model2 <- lm(avg_gasoline ~ avg_diesel  + dwellings + pop + areapstatiom + inc, data = merged_data)
summary(model2)

# cars data

cars_data <- read_excel("cars_data.xlsx")
new_names4 <- c("kod", "county", "region", "cars")
cars_data <- cars_data %>% rename_with(~ new_names4)

merged_data <- inner_join(merged_data, cars_data, by = "county")

is.numeric(merged_data$cars)

merged_data$cars <- as.numeric(merged_data$cars)

model3 <- lm(avg_gasoline ~ avg_diesel  + dwellings + pop + areapstatiom + inc + cars, data = merged_data)
summary(model3)

