library(here)
library(tidyverse)
library(janitor)
library(stringr)
library(assertr)
getwd()

meteorite_data <- read.csv(here("data/meteorite_landings.csv"))
glimpse(meteorite_data)
names(meteorite_data)
dim(meteorite_data)

meteorite_data <- meteorite_data %>% 
  clean_names()

meteorite_data <- meteorite_data %>% 
  separate(geo_location, c("latitude", "longitude"), sep = ",")

meteorite_data <- meteorite_data %>% 
  mutate(latitude = str_replace_all(latitude, "\\(|\\)", "")) %>% 
  mutate(longitude = str_replace_all(longitude, "\\(|\\)", ""))
  
meteorite_data <- mutate_at(meteorite_data, c("latitude", "longitude"), ~replace(., is.na(.), 0))
meteorite_data


meteorite_data %>% 
  select(mass_g) %>% 
  filter(mass_g < 1000) %>% 
  count()

dim(meteorite_data)


meteorite_data <- meteorite_data %>% 
  arrange(year)


meteor_data <- meteorite_data %>% 
  mutate(latitude = as.numeric(latitude))

meteor_data_clean <- meteor_data %>% 
  mutate(longitude = as.numeric(longitude))

dim(meteor_data_clean)
glimpse(meteor_data_clean)

meteor_data_clean %>% 
  stopifnot(latitude >= -90 & latitude <= 90) %>% 
  stopifnot(longitude >= -180 & longitude <= 180)



meteor_data_clean$latitude
meteor_data_clean$longitude

meteor_data_clean %>% 
  summarise(n_nas = sum(is.na(latitude)))

meteor_data_clean %>% 
  summarise(n_nas = sum(is.na(longitude)))

meteor_data_clean <- mutate_at(meteor_data_clean, "longitude", ~replace(., is.na(.), 0))
meteor_data_clean <- mutate_at(meteor_data_clean, "latitude", ~replace(., is.na(.), 0))

meteor_data_clean <- meteor_data_clean %>% 
  filter(mass_g > 1000) %>% 
  arrange(year)


dim(meteor_data_clean)

save(meteor_data_clean, file = "meteor_data_clean.csv")
