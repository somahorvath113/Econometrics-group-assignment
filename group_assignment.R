rm(list = ls())

library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
setwd("D://Egyetem//Econometrics//Group_assignment")
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

merged_data <- left_join(merged_data, cars_data, by = "county")
merged_data$cars <- as.numeric(merged_data$cars) #budapest
merged_data$cars <- replace_na(merged_data$cars, 390.9)
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
merged_data$region <- as.factor(merged_data$region)
merged_data$region <- relevel(merged_data$region, ref = "Budapest")
merged_data <- merged_data %>%
  mutate(Budapest = fct_other(region, keep = "Budapest", other_level = "other"))
merged_data$Budapest <- relevel(merged_data$Budapest, ref = "other")
#merged_data[,13-25] <- replace_na(merged_data[,13-25], 0)

#just the 4 biggest company needed
library(forcats)
keep <- c("Mol","Shell","Omv","Orlen")   
merged_data <- merged_data %>%
  mutate(comp = fct_other(comp, keep = keep, other_level = "other"))
merged_data$comp <- relevel(merged_data$comp, ref = "other")


#regressions for gasoline
gas1 <- lm(avg_gasoline ~ n_stations + region
           + highway_dummy + comp,  data = merged_data)
summary(gas1)

gas2<- lm(avg_gasoline ~ pop + Budapest
          + highway_dummy + comp,  data = merged_data)
summary(gas2)

gas3<- lm(avg_gasoline ~ n_stations + areapstation + inc + cars +
            region + highway_dummy,  data = merged_data)
summary(gas3)

gas4 <- lm(avg_gasoline ~ pop + areapstation +inc + cars + region
           + highway_dummy + comp,  data = merged_data)
summary(gas4)

gas5 <- lm(avg_gasoline ~ n_stations + areapstation +inc + cars + region
           + highway_dummy + comp,  data = merged_data)
summary(gas5)

gas6 <- lm(avg_gasoline ~ n_stations + areapstation +inc + cars + region
           + highway_dummy + comp + legal_stat,  data = merged_data)
summary(gas6)

AIC(gas1, gas2, gas3, gas4, gas5, gas6)
BIC(gas1, gas2, gas3, gas4, gas5, gas6) #gas2  a legjobb

#regressions for diesel

diesel1 <- lm(avg_diesel ~ n_stations + region
           + highway_dummy + comp,  data = merged_data)
summary(diesel1)

diesel2 <- lm(avg_diesel ~ pop + region
              + highway_dummy + comp,  data = merged_data)
summary(diesel2)

diesel3 <- lm(avg_diesel ~ n_stations + Budapest
              + highway_dummy + comp,  data = merged_data)
summary(diesel3)

AIC(diesel1, diesel2, diesel3)
BIC(diesel1, diesel2, diesel3) #diesel 3 - ugyanaz mint gas2


#correlaton matrix
selected_cols <- c("avg_gasoline", "avg_diesel", "n_stations", "pop", "dwellings", "areapstation", "inc", "cars", "highway_dummy")
corrplot_data <- merged_data[selected_cols]
cor_matrix <- cor(corrplot_data, use = "pairwise.complete.obs")
corrplot::corrplot(cor_matrix, method = "number", type = "full")
#pop, dwellings, n_station -> just one of them

# descriptive statistics
#install.packages("skimr")
library(skimr)
#install.packages("gt")
library(gt)
vis_data <- merged_data
vis_data <- vis_data[, c(2, 3, 6, 11, 26, 28, 29 )]
new_names5 <- c("Gasoline", "Diesel", "Number of stations", "Population", "Area per Station", "Income", "Cars")
vis_data <- vis_data %>% rename_with(~ new_names5)
vis1 <- skim(vis_data)
vis1 <- vis1[, -c(1,3, 4, 12)]

# visulalisation 
gt_table <- vis1 %>%
  gt() %>%
  tab_header(title = "Descriptive Statistics") %>%
  fmt_number(
    columns = vars(numeric.mean, numeric.sd, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100),
    decimals = 2) %>%
  cols_label(skim_variable = "Variable",
    numeric.mean = "Average",
    numeric.sd = "Standard deviation",
    numeric.p0 = "Min",
    numeric.p25 = "25. Percentile", numeric.p50 = "Median",
    numeric.p75 = "75. Percentile",
    numeric.p100 = "Max") %>%
  tab_options(table.font.names = "Arial",
    table.background.color = "white")
print(gt_table)

library(ggplot2)
hist_gas <- ggplot(merged_data, aes(x = avg_gasoline)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white",alpha = 0.8)+              
  labs(title = "Averge Price of Gasoline in Hungary",
    x = "Average price of gasoline",
    y = "Frequency") +
   theme_minimal()
hist_gas
hist_diesel <- ggplot(merged_data, aes(x = avg_diesel)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Averge Price of Diesel in Hungary",
       x = "Average price of diesel", y = "Frequency") +
  theme_minimal()
hist_diesel



hist_pop <- ggplot(merged_data, aes(x = pop)) +
  geom_histogram(
    bins = 30,                
    fill = "steelblue",       
    color = "white",          
    alpha = 0.8               
  ) +
  labs(
    title = "Population of settlements in Hungary",
    x = "Population",
    y = "Frequency"
  ) +
  theme_minimal()

hist_pop

hist_dwe <- ggplot(merged_data, aes(x = dwellings)) +
  geom_histogram(
    bins = 30,                
    fill = "steelblue",       
    color = "white",          
    alpha = 0.8               
  ) +
  labs(
    title = "Number of Dwellings in Settlements in Hungary",
    x = "Number of Dwellings",
    y = "Frequency"
  ) +
  theme_minimal()

hist_dwe


gas_station_scat <- ggplot(merged_data, aes(x = dwellings, y = avg_gasoline)) +
  geom_point(
    color = "steelblue",  
    alpha = 0.7,          
    size = 3              
  ) +
  geom_smooth(
    method = "lm",        
    color = "darkred",    
    se = TRUE             
  ) +
  labs(
    title = "Scatterplot with Linear Regression Line",
    x = "Number of Dwellings",
    y = "Averge Price of Gasoline"
  ) +
  theme_minimal()

gas_station_scat



#visualization

library(stargazer)

aic_values <- c(AIC(gas1), AIC(gas2), AIC(gas5))
bic_values <- c(BIC(gas1), BIC(gas2), BIC(gas5))

extra_lines <- list(
  c("AIC", sprintf("%.2f", aic_values)),
  c("BIC", sprintf("%.2f", bic_values))
)



html_file <- "group_regression_table.html"
sink(html_file)
stargazer(
  gas1, gas2, gas5,
  type = "html",
  title = "Regression Results",
  add.lines = extra_lines,
  digits = 2
)
sink()

webshot(html_file, file = "groupregression_table.png", zoom = 2, vwidth = 1200)


aic_values2 <- c(AIC(diesel1), AIC(diesel2), AIC(diesel3))
bic_values2 <- c(BIC(diesel1), BIC(diesel2), BIC(diesel3))


extra_lines2 <- list(
  c("AIC", sprintf("%.2f", aic_values2)),
  c("BIC", sprintf("%.2f", bic_values2))
)

html_file2 <- "group_regression_table2.html"
sink(html_file2)
stargazer(
  diesel1, diesel2, diesel3,
  type = "html",
  title = "Regression Results",
  add.lines = extra_lines2,
  digits = 2
)
sink()

webshot(html_file2, file = "groupregression_table2.png", zoom = 2, vwidth = 1200)
