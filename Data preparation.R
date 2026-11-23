## Data prep for DeepSSF model



# Individuals are from years 2008–2011. 
# MS-NFI (VMI) data is used for canopy cover and age of the forests
# To limit information loss, we will use both 2009 and 2011 MS-NFI datasets by calculating their mean.
# For agricultural land: https://ckan.ymparisto.fi/dataset/kaytosta-poistunut-maatalousmaa-2000-2020


library(tidyverse)
library(patchwork)
library(sf)
library(amt)
library(terra)
library(tidyterra)

setwd("PROJECT DIRECTORY")

# GPS Data ####
data <- read.csv("Data/GPS data/all_dat.csv") %>% 
mutate(date=paste(GMT_Date, GMT_time, sep="_")) %>% 
  mutate(date=lubridate::dmy_hms(date)) %>% 
  distinct()

data <- data %>% filter(!Object_ID %in% c("cc_fi_10_002", "cc_fi_10_004")) |> 
  st_as_sf(coords=c("X","Y"), crs=2393) |> 
  st_transform(crs=3067)

data$X = st_coordinates(data)[,1]
data$Y = st_coordinates(data)[,2]

roedeer <- data %>% filter(SPECIES=="cc") %>%
  mutate(month = month(date), 
         season = case_when(month %in% c(12,1,2,3) ~ "winter", 
         month %in% 5:9 ~ "summer")) |> 
  st_drop_geometry()

wtdeer <- data %>% filter(SPECIES=="ov") %>%
      mutate(month = month(date), 
         season = case_when(month %in% c(12,1,2,3) ~ "winter", 
         month %in% 5:9 ~ "summer")) |> 
  st_drop_geometry()

roedeer %>% make_track(.x=X,.y=Y, .t = date, 
                       id=Object_ID, 
                       sex=sex, 
                       crs=3067) %>%
  summarize_sampling_rate_many(cols = "id", time_unit="hour") %>%
  print(n=100)


wtdeer %>% make_track(.x=X,.y=Y, .t = date, 
                                      id=Object_ID, 
                                      sex=sex, 
                                      crs=3067) %>%
  summarize_sampling_rate_many(cols = "id", time_unit="hour") %>%
  print(n=100)

# DeepSSF does not require fixed time-interval
# It might even benefit from irregular sampling, so the model is not trained only on fixed interval

roedeer_trk <- roedeer |> nest(data = -"Object_ID") |> 
  mutate(track = map(data, function(d) d |> make_track(.x=X,.y=Y, .t = date, sex=sex, crs=3067)),
         steps = map(track, function(d) d |> 
           steps(diff_time_units = "hours"))
)
  
wtdeer_trk <- wtdeer |> nest(data = -"Object_ID") |> 
  mutate(track = map(data, function(d) d |> make_track(.x=X,.y=Y, .t = date, sex=sex, crs=3067)),
         steps = map(track, function(d) d |> 
           steps(diff_time_units = "hours"))
)

roedeer_steps <- roedeer_trk %>% 
  amt::select(Object_ID, steps) %>% 
  amt::unnest(cols = steps)

wtdeer_steps <- wtdeer_trk %>% 
  amt::select(Object_ID, steps) %>% 
  amt::unnest(cols = steps)

roedeer_steps |> 
  ggplot() +
  geom_density(aes(x = sl_), #, bins = 50
                 fill = "skyblue", colour = "skyblue", alpha = 0.75) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000),
  labels = c("0.1", "1", "10", "100", "1000", "10000")) +
  theme_classic()

wtdeer_steps |> 
  ggplot() +
  geom_density(aes(x = sl_), #, bins = 50
                 fill = "skyblue", colour = "skyblue", alpha = 0.75) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000),
  labels = c("0.1", "1", "10", "100", "1000", "10000")) +
  theme_classic()

# https://swforrest.github.io/deepSSF/deepSSF_data_prep_id.html


# Spatial layers ####


stand_age <- c(rast("Data/Habitat layers/puustoika_2009.tif"),
               rast("Data/Habitat layers/puustoika_2011.tif"))

canopy <- c(rast("Data/Habitat layers/latvuspeitto_2009.tif"),
            rast("Data/Habitat layers/latvuspeitto_2011.tif"))

broadleaved_biomass <- c(rast("Data/Habitat layers/biomass_broadleaved_foliage_2009.tif"),
                         rast("Data/Habitat layers/biomass_broadleaved_foliage_2011.tif"))

plot(stand_age)
plot(canopy)
plot(broadleaved_biomass)

gps_sample <- filter(data, Object_ID %in% sample(unique(data$Object_ID), 1))

summary(stand_age)
# Stand age is continuous raster with 32766 being value for missing values
stand_age <- subst(stand_age, from=32766, to=NA)

ggplot() +
  geom_spatraster(data=crop(stand_age, gps_sample)) +
  geom_sf(data=gps_sample, size=1) +
  facet_wrap(~lyr) +
  scale_fill_distiller(palette="Greens", direction=1)

stand_age <- app(stand_age, fun="mean", na.rm=TRUE)

plot(stand_age)
names(stand_age) <- "stand_age"



summary(canopy)
# Canopy is continuous raster with 32766 being value for missing values
canopy <- subst(canopy, from=32766, to=NA)

ggplot() +
  geom_spatraster(data=crop(canopy, gps_sample)) +
  geom_sf(data=gps_sample, size=1) +
  facet_wrap(~lyr) +
  scale_fill_distiller(palette="Greens", direction=1)

canopy <- app(canopy, fun="mean", na.rm=TRUE)
plot(canopy)
names(canopy) <- "canopy"

summary(broadleaved_biomass)
# Biomass of broad-leaved foliage is continuous raster with 32766 being value for missing values
broadleaved_biomass <- subst(broadleaved_biomass, from=32766, to=NA)

ggplot() +
  geom_spatraster(data=crop(broadleaved_biomass, gps_sample)) +
  geom_sf(data=gps_sample, size=1) +
  facet_wrap(~lyr) +
  scale_fill_distiller(palette="Greens", direction=1)

broadleaved_biomass <- app(broadleaved_biomass, fun="mean", na.rm=TRUE)
plot(broadleaved_biomass)
names(broadleaved_biomass) <- "broadleaved_biomass"

crs(broadleaved_biomass)

stand_age
canopy
broadleaved_biomass


# Corine landcover 2012
corine <- rast("Data/Habitat layers/Corine 2012/clc2012_fi20m.tif")
crs(corine)
corine <- project(corine, canopy)

corine
activeCat(corine) <- "LEVEL1"
cats(corine)[[1]]

ggplot() +
  geom_spatraster(data=crop(corine, gps_sample)) +
  geom_sf(data=gps_sample, size=1) +
  scale_fill_brewer(palette = "Set1")


clc_class <- function(r, level1) {
  ids <- with(cats(r)[[1]], VALUE[LEVEL1 == level1])
  classify(as.numeric(r), cbind(ids, 1), others = 0)
}

activeCat(corine) <- "VALUE" # activate relevant category
corine_urban   <- clc_class(corine, 1)
corine_agri   <- clc_class(corine, 2)
corine_wetlands   <- clc_class(corine, 4)
corine_water  <- clc_class(corine, 5)


names(corine_urban) <- "urban"
names(corine_agri) <- "agriculture"
names(corine_wetlands) <- "wetlands"
names(corine_water) <- "water"

ggplot() +
  geom_spatraster(data=crop(corine_agri, gps_sample)) +
  geom_sf(data=gps_sample, size=1)

ggplot() +
  geom_spatraster(data=crop(corine_urban, gps_sample)) +
  geom_sf(data=gps_sample, size=1)

ggplot() +
  geom_spatraster(data=crop(corine_water, gps_sample)) +
  geom_sf(data=gps_sample, size=1)

ggplot() +
  geom_spatraster(data=crop(corine_wetlands, gps_sample)) +
  geom_sf(data=gps_sample, size=1)


# Agriculture
agri_parcels <- st_read("Data/Agriculture/kaytpoistmaatalousmaa2000_2020.shp")
agri_parcels2025 <- st_read("Data/Agriculture/maatalousmaa2025.gpkg")


agri_parcels_combined <- rbind(st_transform(transmute(agri_parcels, col=1), 3067) |> rename(geom = geometry),
                               transmute(agri_parcels2025, col=2))

r_agri_parcels <- rasterize(agri_parcels_combined, canopy, background=0, touches=TRUE)

# combine the rasterized parcel vectors with the corine agriculture raster
compareGeom(corine_agri, r_agri_parcels, stopOnError=FALSE)

r_agri <- max(corine_agri, r_agri_parcels, na.rm = TRUE)

plot(r_agri)

agriculture <- mask(r_agri, mask=canopy, maskvalues=NA, updatevalue=0, inverse=TRUE)

ggplot() +
  geom_spatraster(data=crop(agriculture, gps_sample)) +
  geom_sf(data=gps_sample, size=1) +
  scale_fill_viridis_c()

# Apply majority filter to the agriculture raster
agriculture <- focal(agriculture, w=3, fun="modal")

ggplot() +
  geom_spatraster(data=crop(agriculture, gps_sample)) +
  geom_sf(data=gps_sample, size=1) +
  scale_fill_viridis_c()

plot(agriculture)

names(agriculture) <- "agriculture"

habitat_stack <- c(corine_urban, corine_water, corine_wetlands, canopy, stand_age, broadleaved_biomass, agriculture)
names(habitat_stack)

writeRaster(habitat_stack, "Data/Habitat stack/Habitat_stack.tif", overwrite=TRUE)


plot(habitat_stack)



roedeer_steps |> 
  ggplot() +
  geom_density(aes(x = sl_), #, bins = 50
                 fill = "skyblue", colour = "skyblue", alpha = 0.75) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000),
  labels = c("0.1", "1", "10", "100", "1000", "10000")) +
  theme_classic()

wtdeer_steps |> 
  ggplot() +
  geom_density(aes(x = sl_), #, bins = 50
                 fill = "skyblue", colour = "skyblue", alpha = 0.75) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000),
  labels = c("0.1", "1", "10", "100", "1000", "10000")) +
  theme_classic()

# Resolution of our spatial rasters is 20 x 20
res = 20
distance <- 1400

distance %% res == 0

sum(roedeer_steps$sl_ < distance) / nrow(roedeer_steps)
sum(wtdeer_steps$sl_ < distance) / nrow(wtdeer_steps)


buffer <- distance + (res/2)

nxn_cells <- buffer*2/res
print(paste0("Number of cells in each axis: ", nxn_cells))

prop_steps <- roedeer_steps %>% filter(x2_ - x1_ > -buffer & 
                                             x2_ - x1_ < buffer &
                                             y2_ - y1_ > -buffer & 
                                             y2_ - y1_ < buffer) %>% 
  nrow() / roedeer_steps %>% nrow()

print(paste0("Proportion of steps less than ", buffer, "m in x or y direction: ", round(prop_steps, 4)))

max_dist <- sqrt(buffer^2 + buffer^2)
print(paste0("Maximum distance at corner: ", round(max_dist, 2)))

# Save raw GPS fixes for use in python script
setwd("PROJECT DIRECTORY")

roedeer_data <- roedeer_trk |> 
  amt::select(Object_ID, track) |> 
  amt::unnest(cols = track) |> 
  transmute(id = Object_ID, x = x_, y = y_, time = t_)

wtdeer_data <- wtdeer_trk |> 
  amt::select(Object_ID, track) |> 
  amt::unnest(cols = track) |> 
  transmute(id = Object_ID, x = x_, y = y_, time = t_)

write_csv(roedeer_data,"Processed data/Movement data/roedeer_data.csv")
write_csv(wtdeer_data,"Processed data/Movement data/wtdeer_data.csv")


habitat_stack <- readRDS("Habitat stack/Habitat_stack.rds")

st_crs(data)
crs(habitat_stack)

current_extent <- ext(vect(data))

new_extent <- ext(
  current_extent[1] - buffer*2,  # xmin
  current_extent[2] + buffer*2,  # xmax
  current_extent[3] - buffer*2,  # ymin
  current_extent[4] + buffer*2   # ymax
)

new_extent

cropped_stack <- crop(habitat_stack, new_extent)

ggplot() +
  geom_spatraster(data=cropped_stack[["canopy"]]) +
  geom_sf(data=data) +
  scale_fill_distiller(palette="Greens", direction=1)

plot(cropped_stack)

names(cropped_stack)

cropped_stack[["canopy"]] |> 
writeRaster("Processed data/Environmental layers/canopy.tif", overwrite = T)

cropped_stack[["stand_age"]] |> 
writeRaster("Processed data/Environmental layers/forest_age.tif", overwrite = T)

cropped_stack[["broadleaved_biomass"]] |> 
writeRaster("Processed data/Environmental layers/broadleaved_biomass.tif", overwrite = T)

cropped_stack[["agriculture"]] |> 
writeRaster("Processed data/Environmental layers/agriculture.tif", overwrite = T)

cropped_stack[["water"]] |> 
writeRaster("Processed data/Environmental layers/water.tif", overwrite = T)

cropped_stack[["wetlands"]] |> 
writeRaster("Processed data/Environmental layers/wetlands.tif", overwrite = T)

cropped_stack[["urban"]] |> 
writeRaster("Processed data/Environmental layers/urban.tif", overwrite = T)






