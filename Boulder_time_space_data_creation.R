# create tine + space data
library(sf)
library(tidyverse)
library(mapview)
library(viridis)

m <- mapview
g <- glimpse

dat <- read_sf("./Boulder_students_4326.geojson") 
dat_test <- read_sf("./Boulder_instructor_4326.geojson") 
boulder_county <- read_sf("./County_geojson/County_Boundary.geojson")

dat <- dat %>% 
  mutate(Bedroom_class = case_when(
    nbrBedRoom <= 2 ~ "0-2",
    nbrBedRoom > 2 & nbrBedRoom <= 4 ~ "3-4",
    nbrBedRoom > 4 ~ "5+"
  ))

dat_test <- dat_test %>% 
  mutate(Bedroom_class = case_when(
    nbrBedRoom <= 2 ~ "0-2",
    nbrBedRoom > 2 & nbrBedRoom <= 4 ~ "3-4",
    nbrBedRoom > 4 ~ "5+"
  ))

m(dat)

## price + time + coords only model
dat_train_coords <- dat %>% 
  select(price, saleDate)
dat_test_coords <- dat_test %>% 
  select(price, saleDate)

# Will DR complain given the geojson has 'no features'? what about during inference
st_write(dat_train_coords, "./Boulder_Train_coords_time_4326.geojson")
st_write(dat_test_coords, "./Boulder_Test_coords_time_4326.geojson")

## price + time + coords + ??? model
dat_train_coords_fea <- dat %>% 
  select(price, saleDate, Bedroom_class)
dat_test_coords_fea <- dat_test %>% 
  select(price, saleDate, Bedroom_class)

# Will DR complain given the geojson has 'no features'? what about during inference
st_write(dat_train_coords_fea, "./Boulder_Train_coords_time_BDR_4326.geojson")
st_write(dat_test_coords_fea, "./Boulder_Test_coords_time_BDR_4326.geojson")
###