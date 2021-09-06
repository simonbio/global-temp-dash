library('tidyverse')
library(highcharter)

country <- read_csv('GlobalLandTemperaturesByCountry.csv') %>% na.omit()
#glabal <- read_csv('GlobalTemperatures.csv')


mapdata <- get_data_from_map(download_map_data("custom/world-eckert3-highres"))

glimpse(mapdata)

country$dt <- format(country$dt, format="%Y")

mean_temp_yr <- country %>% group_by(Country, dt) %>%  summarise(average_temp = mean(AverageTemperature)) %>% rename(name = Country)

#mean_temp <- country %>% group_by(Country) %>%  summarise(average_temp = mean(AverageTemperature)) %>% rename(name = Country)
mean_temp_yr$name <- str_replace_all(mean_temp_yr$name, 'United States', 'United States of America')
mean_temp_yr$name <- str_replace_all(mean_temp_yr$name, '^Congo.\\(.*', 'Democratic Republic of the Congo')
mean_temp_yr$name <- str_replace_all(mean_temp_yr$name, '^Congo', 'Republic of Congo')
mean_temp_yr$name <- str_replace_all(mean_temp_yr$name, 'Tanzania', 'United Republic of Tanzania')
# remove Denmark measurement containing Greenland temps
mean_temp_yr <- mean_temp_yr[!mean_temp_yr$name == 'Denmark',]
mean_temp_yr$name <- str_replace_all(mean_temp_yr$name, 'Denmark.*', 'Denmark')



test <- mean_temp_yr %>% filter(name %in% c('Italy', 'Greece'))

test %>% 
  hchart(
    'line', 
    hcaes(x = dt, 
          y = average_temp, 
          group = name)
  )



hcmap(
  map = "custom/world-eckert3-highres",
  data = data,
  value = "average_temp",
  joinBy = c("name", "name"),
  name = "Temperature (Celsius) ",
  #dataLabels = list(enabled = TRUE, format = "{point.name}"),
  # borderColor = "#FAFAFA",
  # borderWidth = 0.1,
  tooltip = list(
    valueDecimals = 2,
    valueSuffix = "°C"
  )
) %>% hc_title(text ="Average World Temperature ")


#### Time-series


country <- read_csv('GlobalLandTemperaturesByCountry.csv') %>% na.omit()



  # String manipulation
  country$Country <- str_replace_all(country$Country, 'United States', 'United States of America')
  country$Country <- str_replace_all(country$Country, '^Congo.\\(.*', 'Democratic Republic of the Congo')
  country$Country <- str_replace_all(country$Country, '^Congo', 'Republic of Congo')
  country$Country <- str_replace_all(country$Country, 'Tanzania', 'United Republic of Tanzania')
  # remove Denmark measurement containing Greenland temps
  country <- country[!country$Country == 'Denmark',]
  country$Country <- str_replace_all(country$Country, 'Denmark.*', 'Denmark')
  
  country$dt <- format(country$dt, format="%Y")
  
  test <- country %>% filter(dt >= 1900 & dt <= 2000 & Country %in% vars) %>%  group_by(Country, dt) %>% summarise(AverageTemperature = mean(AverageTemperature)) %>%
    arrange(dt)
  

vars <- c('Germany', 'Spain', 'Peru')
test %>% 
  hchart(., 
         type = "line", 
         hcaes(x = dt, 
               y = AverageTemperature, 
               group = 'Country'))




test %>% 
  filter(Country %in% vars & dt >= min(.$dt))

names_test <- unique(test$Country)



  country %>% filter(dt >= 1900 & dt <= 2000 & Country %in% vars) %>%
    group_by(Country, dt) %>% summarise('Average Temp' = mean(AverageTemperature)) %>%
    arrange(dt)

test <- country %>% filter(Country == 'Germany' & dt == 1900) 

mean(test$AverageTemperature)


##### test

country$dt <- format(country$dt, format="%Y")

country <- country  %>%
  group_by(name, dt) %>% summarise('Average Temp' = mean(AverageTemperature)) %>%
  arrange(dt)

hcmap(
  map = "custom/world-eckert3-highres",
  data = country,
  value = 'Average Temp',
  joinBy = c("name", "name"),
  name = "Temperature (Celsius) ",
  #dataLabels = list(enabled = TRUE, format = "{point.name}"),
  # borderColor = "#FAFAFA",
  # borderWidth = 0.1,
  tooltip = list(
    valueDecimals = 2,
    valueSuffix = "°C"
  )
) %>% hc_title(text ="Average World Temperature ")


test1 <- country %>%  filter( name == 'Afghanistan') %>%
  group_by(name, dt) %>%
  summarise(average_temp = mean(AverageTemperature)) %>%
  arrange(dt)
