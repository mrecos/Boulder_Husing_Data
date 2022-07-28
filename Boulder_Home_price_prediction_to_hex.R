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

dat_UTM <- dat %>% 
  st_transform(crs = "epsg:32613")

m(dat)

## price + coords only model
dat_train_coords <- dat %>% 
  select(price)
dat_test_coords <- dat_test %>% 
  select(price)

# Will DR complain given the geojson has 'no features'? what about during inference
st_write(dat_train_coords, "./Boulder_Train_coords_only_4326.geojson")
st_write(dat_test_coords, "./Boulder_Test_coords_only_4326.geojson")
###


## price + coords+ Bedrooms model
dat_train_coords_fea <- dat %>% 
  select(price, saleDate, Bedroom_class)
dat_test_coords_fea <- dat_test %>% 
  select(price, saleDate, Bedroom_class)

# Will DR complain given the geojson has 'no features'? what about during inference
st_write(dat_train_coords_fea, "./Boulder_Train_coords_BDR_4326.geojson")
st_write(dat_test_coords_fea, "./Boulder_Test_coords_BDR_4326.geojson")
###

# fit on individual points
# aggregate attributes to cell centroid
# predict to centroids/points
# join to polygons

net <- st_make_grid(dat_UTM, cellsize = 1000, square = FALSE ) %>% 
  st_sf() %>% 
  mutate(cell_id = 1:n())

dat_net <- dat %>% 
  st_join(net)

# Net preds 
net_coords <- st_centroid(net)
st_write(net_coords, "./Boulder_Test_NET_coords_only_4326.geojson")
## returned preds 
preds_net <- read_csv("preds/Boulder_Train_coords_only_4326.geojson_ENET_Blender_(33+32+35+39+34+38+37+36)_(83)_63.99_620fe18241aee02b3642ddd0_39460362-27af-4b51-8042-a_Boulder_Test_NET_coords_only_4326.csv")
net_preds_sf <- cbind(net, preds_net) %>% 
  st_join(st_transform(boulder_county, crs = "epsg:32613"), by = st_intersects()) %>% 
  filter(!is.na(OBJECTID))

m(net_preds_sf, zcol = "Prediction", alpha.regions = 0.65, col.regions = inferno(5))
