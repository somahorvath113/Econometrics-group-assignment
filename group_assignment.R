rm(list = ls())

library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
setwd("C:/Bator/1felev/Econometrics/empirical assignment")
data <- read_excel("data.xlsx")


new_names <- c("date", "city", "comp", "address", "diesel", "gasoline", "main/highway")

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

#mutate Budapest in the same form
data_avg$city <- gsub("kerület", "ker.", data_avg$city, perl = TRUE)

# merge the two datasets
merged_data <- inner_join(data_avg, data_rest, by = "city")

#check for the missing rows
library(tibble)
missing_from_rest <- anti_join(data_avg, data_rest, by = "city")
head(missing_from_rest$city, 30)


# new variable, area/gas station

merged_data$areapstation <- merged_data$area/merged_data$n_stations

#cleaning
merged_data <- merged_data[, -c(7,10,11,13,14,18)]

# income data
income_data <- read_excel("data3.xlsx")
new_names3 <- c("code", "county", "region", "inc")
income_data <- income_data %>% rename_with(~ new_names3)
income_data <- income_data[, -1]

#merge them with special care of Budapest 
merged_data <- left_join(merged_data, income_data, by = "county")
merged_data$region <- replace_na(merged_data$region, "Budapest")
merged_data$inc <- replace_na(merged_data$inc, 450.097)

# add cars data
cars_data <- read_excel("cars_data.xlsx")
new_names4 <- c("code", "county", "region", "cars")
cars_data <- cars_data %>% rename_with(~ new_names4)
cars_data <- cars_data[,-c(1,3)]

merged_data <- inner_join(merged_data, cars_data, by = "county")

#do it numeric
is.numeric(merged_data$cars)
merged_data$cars <- as.numeric(merged_data$cars)

# creating highway dummy
merged_data <- merged_data %>%
  mutate(highway = stringr::str_extract(address, "M\\d+"))
merged_data <- merged_data %>%
  mutate(highway_dummy = ifelse(str_detect(address, "M\\d+"), 1, 0))

#some statistics
summary(merged_data)

#change type of parameters
merged_data$comp <- as.factor(merged_data$comp)
merged_data$city <- as.factor(merged_data$city)
merged_data$county <- as.factor(merged_data$county)
merged_data$dist_seat <- as.factor(merged_data$dist_seat)
merged_data$legal_stat <- as.factor(merged_data$legal_stat)
#merged_data[,13-25] <- replace_na(merged_data[,13-25], 0)

#regressions for gasoline
gas1 <- lm(avg_gasoline ~ avg_diesel, data = merged_data)
summary(gas1)

gas2<- lm(avg_gasoline ~ avg_diesel  + dwellings + pop + areapstation + inc + cars,
             data = merged_data)
summary(gas2)

gas3<- lm(avg_gasoline ~ dwellings + pop + areapstation + inc + cars +
            region + highway_dummy,  data = merged_data)
summary(gas3)

#just the 4 biggest company stays
library(forcats)
keep <- c("Mol","Shell","Omv","Orlen")   
merged_data <- merged_data %>%
  mutate(comp = fct_other(comp, keep = keep, other_level = "other"))
merged_data$comp <- relevel(merged_data$comp, ref = "other")

gas4 <- lm(avg_gasoline ~ pop + inc + cars + region + highway_dummy 
           + n_stations + comp,  data = merged_data)
summary(gas4)

#ez egyelőre nem jó, de ki kéne javítani
corrplot::corrplot(merged_data, method = "color", type = "full", 
                   col = c(2,3,6,11,12,26,28,29,31))
