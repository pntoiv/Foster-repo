# Setup ####

library(sf)
library(tidyverse)
library(terra)
library(tidyterra)
library(amt)
library(patchwork)
library(suncalc)
library(ggnewscale)

theme_set(theme_bw())
setwd("PROJECT DIRECTORY")


wtdeer_obs <- read.csv("Data/Processed data/Movement data/wtdeer_data.csv")
roedeer_obs <- read.csv("Data/Processed data/Movement data/roedeer_data.csv")

roedeer_obs$time <- as_datetime(roedeer_obs$time, tz="UTC")
roedeer_obs$time <- with_tz(roedeer_obs$time, "Europe/Helsinki")

wtdeer_obs$time <- as_datetime(wtdeer_obs$time, tz="UTC")
wtdeer_obs$time <- with_tz(wtdeer_obs$time, "Europe/Helsinki")

roedeer_sf <- st_as_sf(roedeer_obs, coords=c("x","y"), crs=3067, remove=FALSE)
wtdeer_sf <- st_as_sf(wtdeer_obs, coords=c("x","y"), crs=3067, remove=FALSE)


roedeer_sim_data <- read.csv("outputs/Roe deer/simulated_trajectories.csv") |> 
  # Conversion to Finnish time
  mutate(date = as.POSIXct(paste(as.Date(yday-1, origin="2010-01-01"), hour), format = "%Y-%m-%d %H", tz="UTC"),
         date_fin = with_tz(date, "Europe/Helsinki"),
         yday = yday(date_fin),
         hour = hour(date_fin)) |> 
  select(-date, -date_fin)

wtdeer_sim_data <- read.csv("outputs/White-tailed deer/simulated_trajectories.csv") |> 
  # Conversion to Finnish time
  mutate(date = as.POSIXct(paste(as.Date(yday-1, origin="2010-01-01"), hour), format = "%Y-%m-%d %H", tz="UTC"),
         date_fin = with_tz(date, "Europe/Helsinki"),
         yday = yday(date_fin),
         hour = hour(date_fin)) |> 
  select(-date, -date_fin)

habitat_stack <- rast("Data/Habitat stack/Habitat_stack.tif")


wtdeer_obs |> 
  mutate(yday = yday(time)) |> 
  ggplot() +
  geom_line(aes(y=id, x=yday))

roedeer_obs |> 
  mutate(yday = yday(time)) |> 
  ggplot() +
  geom_line(aes(y=id, x=yday))

wtdeer_obs |> 
  mutate(hour = hour(time)) |> 
  ggplot() +
  geom_line(aes(y=id, x=hour)) +
  scale_x_continuous(limits=c(0,23), breaks=seq(0,23, by=1))

roedeer_obs |> 
  mutate(hour = hour(time)) |> 
  ggplot() +
  geom_line(aes(y=id, x=hour)) +
  scale_x_continuous(limits=c(0,23), breaks=seq(0,23, by=1))


# Night layers for plots

days <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by = "day"),
                   lat = 61, lon = 24)

sun <- getSunlightTimes(data = days, keep = c("sunrise", "sunset"),
                        tz = "Europe/Helsinki")

night_df <- sun |>
  mutate(season = case_when(month(date) %in% c(12, 1, 2) ~ "Winter",
                            month(date) %in% 3:5  ~ "Spring",
                            month(date) %in% 6:8  ~ "Summer",
                            month(date) %in% 9:11 ~ "Autumn"),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
         rise = hour(sunrise) + minute(sunrise) / 60,
         set  = hour(sunset)  + minute(sunset)  / 60) |>
  group_by(season) |>
  summarize(rise_min = min(rise), rise_max = max(rise),
            set_min  = min(set),  set_max  = max(set),
            .groups = "drop")

night_layer <- list(
  # dark on every day of the season (darker grey)
  geom_rect(data = night_df, aes(xmin = -1, xmax = rise_min, ymin = -Inf, ymax = Inf),
            fill = "grey30", alpha = 0.15, inherit.aes = FALSE),
  geom_rect(data = night_df, aes(xmin = set_max, xmax = 25, ymin = -Inf, ymax = Inf),
            fill = "grey30", alpha = 0.15, inherit.aes = FALSE),
  # dark on some days only (lighter grey)
  geom_rect(data = night_df, aes(xmin = rise_min, xmax = rise_max, ymin = -Inf, ymax = Inf),
            fill = "grey30", alpha = 0.07, inherit.aes = FALSE),
  geom_rect(data = night_df, aes(xmin = set_min, xmax = set_max, ymin = -Inf, ymax = Inf),
            fill = "grey30", alpha = 0.07, inherit.aes = FALSE)
)

# Optimal hours for the outputs would be 
#data.frame(date = c(as.Date("2010-01-01"), as.Date("2010-06-30")),
#                   lat = 61, lon = 24) |> 
#  getSunlightTimes(data=_, keep = c("sunrise", "sunset"),
#                   tz = "Europe/Helsinki")

## from EEST/EET to UTC:
with_tz(as.POSIXct("2010-01-01 06:00", tz="Europe/Helsinki"), "UTC")
with_tz(as.POSIXct("2010-01-01 18:00", tz="Europe/Helsinki"), "UTC")

with_tz(as.POSIXct("2010-06-30 05:00", tz="Europe/Helsinki"), "UTC")
with_tz(as.POSIXct("2010-06-30 20:00", tz="Europe/Helsinki"), "UTC")

# UTC to EEST/EET
with_tz(as.POSIXct("2010-06-30 17:00", tz="UTC"), "Europe/Helsinki")
with_tz(as.POSIXct("2010-01-01 17:00", tz="UTC"), "Europe/Helsinki")

# Season layers for plots and helper function to convert yday to date
ref_year = 2010
yday_to_date <- function(yday) as.Date(yday - 1, origin = paste0(ref_year, "-01-01"))

season_df <- data.frame(
  season = factor(c("Winter", "Spring", "Summer", "Autumn", "Winter"),
                  levels = c("Winter", "Spring", "Summer", "Autumn")),
  start  = as.Date(paste0(ref_year, c("-01-01", "-03-01", "-06-01", "-09-01", "-12-01"))),
  end    = as.Date(paste0(c(rep(ref_year, 4), ref_year + 1),
                          c("-03-01", "-06-01", "-09-01", "-12-01", "-01-01")))
)

season_layer <- list(
  geom_rect(data = season_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = season),
            alpha = 0.2, inherit.aes = FALSE),
  scale_fill_manual(values = c(Winter = "lightblue", Spring = "palegreen3",
                               Summer = "gold", Autumn = "tan3"),
                    name = "Season")
)



# Movement process ####

# Prepare step data
hourly_lag <- 1

roedeer <- roedeer_obs |> 
  group_by(id) |> 
  arrange(time) |> 
  mutate(
    id = id,
    x1 = x,
    y1 = y,
    x2 = lead(x1, n = hourly_lag, default = NA),
    y2 = lead(y1, n = hourly_lag, default = NA),
    t1 = time,
    t2 = lead(t1, n = hourly_lag, default = NA),
    dt = as.numeric(difftime(t2, t1, units="hours")), 
    hour_t1 = hour(time),
    hour_t2 = lead(hour_t1, n = hourly_lag, default = NA),
    yday_t1 = yday(time), # day of the year
    yday_t2 = lead(yday_t1, n = hourly_lag, default = NA),
    sl = c(sqrt(diff(y)^2 + diff(x)^2), NA), # step lengths
    log_sl = log(sl),
    bearing = c(atan2(diff(y), diff(x)), NA),
    ta = c(NA, ifelse(
      diff(bearing) > pi, diff(bearing)-(2*pi), ifelse(
        diff(bearing) < -pi, diff(bearing)+(2*pi), diff(bearing)))), # turning angles
    cos_ta = cos(ta),
    
    .keep = "none"
  ) |> 
  select(-t1, -t2)

roedeer_sim <- roedeer_sim_data |> 
  mutate(sim_id_new = paste0(sim_id,"_",replicate)) |> 
  group_by(id, sim_id_new) |> 
  mutate(
    x1 = x,
    y1 = y,
    x2 = lead(x1, n = hourly_lag, default = NA),
    y2 = lead(y1, n = hourly_lag, default = NA),
    dt = 1,
    yday_t1 = yday,
    yday_t2 = lead(yday_t1, n = hourly_lag, default = NA),
    hour_t1 = hour,
    hour_t2 = lead(hour_t1, n = hourly_lag, default = NA),
    sl = c(sqrt(diff(y)^2 + diff(x)^2), NA),  # step lengths
    log_sl = log(sl),
    bearing = c(atan2(diff(y), diff(x)), NA),
    ta = c(NA, ifelse(
      diff(bearing) > pi, diff(bearing) - (2 * pi),
      ifelse(diff(bearing) < -pi, diff(bearing) + (2 * pi), diff(bearing))
    )),
    cos_ta = cos(ta),
    
    .keep = "none"
  )


roedeer_all <- bind_rows(mutate(roedeer, class = "Observed", sim_id_new=NA), 
                         mutate(roedeer_sim, class = "Simulated")) |> 
  mutate(season = case_when(yday_t1 %in% c(335:366, 1:60) ~ "Winter", 
                            yday_t1 %in% 61:151 ~ "Spring", 
                            yday_t1 %in% 152:243 ~ "Summer", 
                            yday_t1 %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))


wtdeer <- wtdeer_obs |> 
  group_by(id) |> 
  arrange(time) |> 
  mutate(
    id = id,
    x1 = x,
    y1 = y,
    x2 = lead(x1, n = hourly_lag, default = NA),
    y2 = lead(y1, n = hourly_lag, default = NA),
    t1 = time,
    t2 = lead(t1, n = hourly_lag, default = NA),
    dt = as.numeric(difftime(t2, t1, units="hours")), 
    hour_t1 = hour(time),
    hour_t2 = lead(hour_t1, n = hourly_lag, default = NA),
    yday_t1 = yday(time), # day of the year
    yday_t2 = lead(yday_t1, n = hourly_lag, default = NA),
    sl = c(sqrt(diff(y)^2 + diff(x)^2), NA), # step lengths
    log_sl = log(sl),
    bearing = c(atan2(diff(y), diff(x)), NA),
    ta = c(NA, ifelse(
      diff(bearing) > pi, diff(bearing)-(2*pi), ifelse(
        diff(bearing) < -pi, diff(bearing)+(2*pi), diff(bearing)))), # turning angles
    cos_ta = cos(ta),
    
    .keep = "none"
  ) |> 
  select(-t1, -t2)

wtdeer_sim <- wtdeer_sim_data |> 
  mutate(sim_id_new = paste0(sim_id,"_",replicate)) |> 
  group_by(id, sim_id_new) |> 
  mutate(
    x1 = x,
    y1 = y,
    x2 = lead(x1, n = hourly_lag, default = NA),
    y2 = lead(y1, n = hourly_lag, default = NA),
    dt = 1,
    yday_t1 = yday,
    yday_t2 = lead(yday_t1, n = hourly_lag, default = NA),
    hour_t1 = hour,
    hour_t2 = lead(hour_t1, n = hourly_lag, default = NA),
    sl = c(sqrt(diff(y)^2 + diff(x)^2), NA),  # step lengths
    log_sl = log(sl),
    bearing = c(atan2(diff(y), diff(x)), NA),
    ta = c(NA, ifelse(
      diff(bearing) > pi, diff(bearing) - (2 * pi),
      ifelse(diff(bearing) < -pi, diff(bearing) + (2 * pi), diff(bearing))
    )),
    cos_ta = cos(ta),
    
    .keep = "none"
  )


wtdeer_all <- bind_rows(mutate(wtdeer, class = "Observed", sim_id_new=NA), 
                         mutate(wtdeer_sim, class = "Simulated")) |> 
  mutate(season = case_when(yday_t1 %in% c(335:366, 1:60) ~ "Winter", 
                            yday_t1 %in% 61:151 ~ "Spring", 
                            yday_t1 %in% 152:243 ~ "Summer", 
                            yday_t1 %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))




ggplot() +
  geom_density(data = roedeer_all %>% filter(class == "Observed"),
               aes(x = sl),
               fill = "skyblue", colour = "skyblue", alpha = 0.5) +
  geom_density(data = roedeer_all %>% filter(class == "Simulated"),
               aes(x = sl),
               fill = "orange", colour = "orange", alpha = 0.5) +
  scale_x_continuous("Step length (m)", limits = c(0, 1500)) +
  scale_y_continuous("Density") +
  labs(title="Roe deer") +
  theme_bw() +
  
ggplot() +
  geom_density(data = wtdeer_all %>% filter(class == "Observed"),
               aes(x = sl),
               fill = "skyblue", colour = "skyblue", alpha = 0.5) +
  geom_density(data = wtdeer_all %>% filter(class == "Simulated"),
               aes(x = sl),
               fill = "orange", colour = "orange", alpha = 0.5) +
  scale_x_continuous("Step length (m)", limits = c(0, 1500)) +
  scale_y_continuous("Density") +
  labs(title="White-tailed deer") +
  theme_bw()




ggplot() +
  stat_summary(data = filter(roedeer_all, class == "Observed", !is.na(season)) , aes(x=hour_t1, y=sl/as.numeric(dt)), fun="mean", geom="bar", fill = "gray", width = 1) +
  stat_summary(data = filter(roedeer_all, class == "Simulated", !is.na(season)) , aes(x=hour_t1, y=sl/as.numeric(dt)), fun="mean", geom="line", color="red", linewidth=1) +
  night_layer +
  facet_wrap(~season) +
  labs(title="a) Roe deer") +
ggplot() +
  stat_summary(data = filter(wtdeer_all, class == "Observed", !is.na(season)) , aes(x=hour_t1, y=sl/as.numeric(dt)), fun="mean", geom="bar", fill = "gray", width = 1) +
  stat_summary(data = filter(wtdeer_all, class == "Simulated", !is.na(season)) , aes(x=hour_t1, y=sl/as.numeric(dt)), fun="mean", geom="line", color="red", linewidth=1) +
  night_layer +
  facet_wrap(~season) +
  labs(title="b) White-tailed deer") +
  plot_layout(axis="collect") &
  theme_bw() &
  scale_x_continuous("Hour (Europe/Helsinki)", breaks=seq(0,25, by=2), expand=c(0,0)) &
  scale_y_continuous("Movement rate (m/hour)", expand=c(0,0.1)) &
  coord_cartesian(ylim=c(0, 350))


ggsave(filename="Figures/movement_rates.png",
       width=29, height=15, units="cm", dpi=600)



turning_angles_stationary <- ggplot() +
  geom_density(data = roedeer_all %>% filter(class == "Observed"),
               aes(x = ta),
               fill = "skyblue", colour = "skyblue", alpha = 0.5) +
  geom_density(data = roedeer_all %>% filter(class == "Simulated"),
               aes(x = ta),
               fill = "orange", colour = "orange", alpha = 0.5) +
  scale_x_continuous("Turning angle (radians)") +
  scale_y_continuous("Density") +
  theme_bw()

turning_angles_stationary



# Habitat use ####

# Roe deer
habitat_roedeer_obs <- roedeer_obs |> 
  cbind(terra::extract(habitat_stack, vect(roedeer_obs, geom=c("x","y"), crs="EPSG:3067"))) |> 
  mutate(yday=yday(time), hour=hour(time))

habitat_roedeer_sim <- roedeer_sim_data |> 
  cbind(terra::extract(habitat_stack, vect(roedeer_sim_data, geom=c("x","y"), crs="EPSG:3067")))

# White-tailed deer
habitat_wtdeer_obs <- wtdeer_obs |> 
  cbind(terra::extract(habitat_stack, vect(wtdeer_obs, geom=c("x","y"), crs="EPSG:3067"))) |> 
  mutate(yday=yday(time), hour=hour(time))

habitat_wtdeer_sim <- wtdeer_sim_data |> 
  cbind(terra::extract(habitat_stack, vect(wtdeer_sim_data, geom=c("x","y"), crs="EPSG:3067")))

# Create library of habitat use data
habitat_use_data <- list("obs" = list(), "sim" = list())

habitat_use_data[["obs"]][["wtdeer"]] <- habitat_wtdeer_obs
habitat_use_data[["obs"]][["roedeer"]] <- habitat_roedeer_obs
habitat_use_data[["sim"]][["wtdeer"]] <- habitat_wtdeer_sim
habitat_use_data[["sim"]][["roedeer"]] <- habitat_roedeer_sim


# Produce data for plotting

yday_summary <- list("obs" = list(), "sim" = list())

for (i in c("wtdeer", "roedeer")) {
  
  yday_summary[["sim"]][[i]] <- habitat_use_data[["sim"]][[i]] |> 
    mutate(hour = round(hour, 0), yday = round(yday, 0), 
           forest = case_when(is.na(canopy) ~ 0, .default=1)) |> 
    group_by(yday, id) |> 
    summarize(canopy = mean(canopy, na.rm=TRUE),
              forest = mean(forest, na.rm=TRUE),
              urban = mean(urban, na.rm=TRUE),
              broadleaved_mass = mean(broadleaved_biomass, na.rm=TRUE),
              age = mean(stand_age, na.rm=TRUE),
              agriculture = mean(agriculture, na.rm=TRUE)) |> 
    group_by(yday) |> 
    summarize(canopy_q05 = quantile(canopy, 0.05, na.rm=TRUE),
              canopy_q25 = quantile(canopy, 0.25, na.rm=TRUE),
              canopy_q50 = quantile(canopy, 0.5, na.rm=TRUE),
              canopy_q75 = quantile(canopy, 0.75, na.rm=TRUE),
              canopy_q95 = quantile(canopy, 0.95, na.rm=TRUE),
              forest_q05 = quantile(forest, 0.05, na.rm=TRUE),
              forest_q25 = quantile(forest, 0.25, na.rm=TRUE),
              forest_q50 = quantile(forest, 0.5, na.rm=TRUE),
              forest_q75 = quantile(forest, 0.75, na.rm=TRUE),
              forest_q95 = quantile(forest, 0.95, na.rm=TRUE),
              agriculture_q05 = quantile(agriculture, 0.05, na.rm=TRUE),
              agriculture_q25 = quantile(agriculture, 0.25, na.rm=TRUE),
              agriculture_q50 = quantile(agriculture, 0.50, na.rm=TRUE),
              agriculture_q75 = quantile(agriculture, 0.75, na.rm=TRUE),
              agriculture_q95 = quantile(agriculture, 0.95, na.rm=TRUE),
              age_q05 = quantile(age, 0.05, na.rm=TRUE),
              age_q25 = quantile(age, 0.25, na.rm=TRUE),
              age_q50 = quantile(age, 0.5, na.rm=TRUE),
              age_q75 = quantile(age, 0.75, na.rm=TRUE),
              age_q95 = quantile(age, 0.95, na.rm=TRUE),
              urban_q05 = quantile(urban, 0.05, na.rm=TRUE),
              urban_q25 = quantile(urban, 0.25, na.rm=TRUE),
              urban_q50 = quantile(urban, 0.5, na.rm=TRUE),
              urban_q75 = quantile(urban, 0.75, na.rm=TRUE),
              urban_q95 = quantile(urban, 0.95, na.rm=TRUE),            
              broadleaved_q05 = quantile(broadleaved_mass, 0.05, na.rm=TRUE),
              broadleaved_q25 = quantile(broadleaved_mass, 0.25, na.rm=TRUE),
              broadleaved_q50 = quantile(broadleaved_mass, 0.5, na.rm=TRUE),
              broadleaved_q75 = quantile(broadleaved_mass, 0.75, na.rm=TRUE),
              broadleaved_q95 = quantile(broadleaved_mass, 0.95, na.rm=TRUE)) |> 
    ungroup()
  
  yday_summary[["obs"]][[i]] <- habitat_use_data[["obs"]][[i]] |> 
    mutate(hour = round(hour, 0), yday = round(yday, 0), 
           forest = case_when(is.na(canopy) ~ 0, .default=1)) |> 
    group_by(yday, id) |> 
    summarize(canopy = mean(canopy, na.rm=TRUE),
              forest = mean(forest, na.rm=TRUE),
              age = mean(stand_age, na.rm=TRUE),
              broadleaved_mass = mean(broadleaved_biomass, na.rm=TRUE),
              urban = mean(urban, na.rm=TRUE),
              agriculture = mean(agriculture, na.rm=TRUE)) |> 
    group_by(yday) |> 
    summarize(canopy_q05 = quantile(canopy, 0.05, na.rm=TRUE),
              canopy_q25 = quantile(canopy, 0.25, na.rm=TRUE),
              canopy_q50 = quantile(canopy, 0.5, na.rm=TRUE),
              canopy_q75 = quantile(canopy, 0.75, na.rm=TRUE),
              canopy_q95 = quantile(canopy, 0.95, na.rm=TRUE),
              forest_q05 = quantile(forest, 0.05, na.rm=TRUE),
              forest_q25 = quantile(forest, 0.25, na.rm=TRUE),
              forest_q50 = quantile(forest, 0.5, na.rm=TRUE),
              forest_q75 = quantile(forest, 0.75, na.rm=TRUE),
              forest_q95 = quantile(forest, 0.95, na.rm=TRUE),
              agriculture_q05 = quantile(agriculture, 0.05, na.rm=TRUE),
              agriculture_q25 = quantile(agriculture, 0.25, na.rm=TRUE),
              agriculture_q50 = quantile(agriculture, 0.50, na.rm=TRUE),
              agriculture_q75 = quantile(agriculture, 0.75, na.rm=TRUE),
              agriculture_q95 = quantile(agriculture, 0.95, na.rm=TRUE),
              age_q05 = quantile(age, 0.05, na.rm=TRUE),
              age_q25 = quantile(age, 0.25, na.rm=TRUE),
              age_q50 = quantile(age, 0.5, na.rm=TRUE),
              age_q75 = quantile(age, 0.75, na.rm=TRUE),
              age_q95 = quantile(age, 0.95, na.rm=TRUE),
              urban_q05 = quantile(urban, 0.05, na.rm=TRUE),
              urban_q25 = quantile(urban, 0.25, na.rm=TRUE),
              urban_q50 = quantile(urban, 0.5, na.rm=TRUE),
              urban_q75 = quantile(urban, 0.75, na.rm=TRUE),
              urban_q95 = quantile(urban, 0.95, na.rm=TRUE),            
              broadleaved_q05 = quantile(broadleaved_mass, 0.05, na.rm=TRUE),
              broadleaved_q25 = quantile(broadleaved_mass, 0.25, na.rm=TRUE),
              broadleaved_q50 = quantile(broadleaved_mass, 0.5, na.rm=TRUE),
              broadleaved_q75 = quantile(broadleaved_mass, 0.75, na.rm=TRUE),
              broadleaved_q95 = quantile(broadleaved_mass, 0.95, na.rm=TRUE)) |> 
    ungroup()

  
}


yday_summary


hourly_summary <- list("obs" = list(), "sim" = list())

for (i in c("wtdeer", "roedeer")) {
  
  hourly_summary[["sim"]][[i]] <- habitat_use_data[["sim"]][[i]] |> 
    mutate(hour = round(hour, 0), yday = round(yday, 0), 
           forest = case_when(is.na(canopy) ~ 0, .default=1)) |> 
    group_by(hour, id) |> 
    summarize(canopy = mean(canopy, na.rm=TRUE),
              forest = mean(forest, na.rm=TRUE),
              agriculture = mean(agriculture, na.rm=TRUE),
              broadleaved_mass = mean(broadleaved_biomass, na.rm=TRUE),
              urban = mean(urban, na.rm=TRUE),
              age = mean(stand_age, na.rm=TRUE)) |> 
    group_by(hour) |> 
    summarize(canopy_q05 = quantile(canopy, 0.05, na.rm=TRUE),
              canopy_q25 = quantile(canopy, 0.25, na.rm=TRUE),
              canopy_q50 = quantile(canopy, 0.5, na.rm=TRUE),
              canopy_q75 = quantile(canopy, 0.75, na.rm=TRUE),
              canopy_q95 = quantile(canopy, 0.95, na.rm=TRUE),
              forest_q05 = quantile(forest, 0.05, na.rm=TRUE),
              forest_q25 = quantile(forest, 0.25, na.rm=TRUE),
              forest_q50 = quantile(forest, 0.5, na.rm=TRUE),
              forest_q75 = quantile(forest, 0.75, na.rm=TRUE),
              forest_q95 = quantile(forest, 0.95, na.rm=TRUE),
              agriculture_q05 = quantile(agriculture, 0.05, na.rm=TRUE),
              agriculture_q25 = quantile(agriculture, 0.25, na.rm=TRUE),
              agriculture_q50 = quantile(agriculture, 0.50, na.rm=TRUE),
              agriculture_q75 = quantile(agriculture, 0.75, na.rm=TRUE),
              agriculture_q95 = quantile(agriculture, 0.95, na.rm=TRUE),
              age_q05 = quantile(age, 0.05, na.rm=TRUE),
              age_q25 = quantile(age, 0.25, na.rm=TRUE),
              age_q50 = quantile(age, 0.5, na.rm=TRUE),
              age_q75 = quantile(age, 0.75, na.rm=TRUE),
              age_q95 = quantile(age, 0.95, na.rm=TRUE),
              urban_q05 = quantile(urban, 0.05, na.rm=TRUE),
              urban_q25 = quantile(urban, 0.25, na.rm=TRUE),
              urban_q50 = quantile(urban, 0.5, na.rm=TRUE),
              urban_q75 = quantile(urban, 0.75, na.rm=TRUE),
              urban_q95 = quantile(urban, 0.95, na.rm=TRUE),            
              broadleaved_q05 = quantile(broadleaved_mass, 0.05, na.rm=TRUE),
              broadleaved_q25 = quantile(broadleaved_mass, 0.25, na.rm=TRUE),
              broadleaved_q50 = quantile(broadleaved_mass, 0.5, na.rm=TRUE),
              broadleaved_q75 = quantile(broadleaved_mass, 0.75, na.rm=TRUE),
              broadleaved_q95 = quantile(broadleaved_mass, 0.95, na.rm=TRUE)) |> 
    ungroup()
  
  hourly_summary[["obs"]][[i]] <- habitat_use_data[["obs"]][[i]] |> 
    mutate(hour = round(hour, 0), yday = round(yday, 0), 
           forest = case_when(is.na(canopy) ~ 0, .default=1)) |> 
    group_by(hour, id) |> 
    summarize(canopy = mean(canopy, na.rm=TRUE),
              forest = mean(forest, na.rm=TRUE),
              broadleaved_mass = mean(broadleaved_biomass, na.rm=TRUE),
              agriculture = mean(agriculture, na.rm=TRUE),
              urban = mean(urban, na.rm=TRUE),
              age = mean(stand_age, na.rm=TRUE)) |> 
    group_by(hour) |> 
    summarize(canopy_q05 = quantile(canopy, 0.05, na.rm=TRUE),
              canopy_q25 = quantile(canopy, 0.25, na.rm=TRUE),
              canopy_q50 = quantile(canopy, 0.5, na.rm=TRUE),
              canopy_q75 = quantile(canopy, 0.75, na.rm=TRUE),
              canopy_q95 = quantile(canopy, 0.95, na.rm=TRUE),
              forest_q05 = quantile(forest, 0.05, na.rm=TRUE),
              forest_q25 = quantile(forest, 0.25, na.rm=TRUE),
              forest_q50 = quantile(forest, 0.5, na.rm=TRUE),
              forest_q75 = quantile(forest, 0.75, na.rm=TRUE),
              forest_q95 = quantile(forest, 0.95, na.rm=TRUE),
              agriculture_q05 = quantile(agriculture, 0.05, na.rm=TRUE),
              agriculture_q25 = quantile(agriculture, 0.25, na.rm=TRUE),
              agriculture_q50 = quantile(agriculture, 0.50, na.rm=TRUE),
              agriculture_q75 = quantile(agriculture, 0.75, na.rm=TRUE),
              agriculture_q95 = quantile(agriculture, 0.95, na.rm=TRUE),
              age_q05 = quantile(age, 0.05, na.rm=TRUE),
              age_q25 = quantile(age, 0.25, na.rm=TRUE),
              age_q50 = quantile(age, 0.5, na.rm=TRUE),
              age_q75 = quantile(age, 0.75, na.rm=TRUE),
              age_q95 = quantile(age, 0.95, na.rm=TRUE),
              urban_q05 = quantile(urban, 0.05, na.rm=TRUE),
              urban_q25 = quantile(urban, 0.25, na.rm=TRUE),
              urban_q50 = quantile(urban, 0.5, na.rm=TRUE),
              urban_q75 = quantile(urban, 0.75, na.rm=TRUE),
              urban_q95 = quantile(urban, 0.95, na.rm=TRUE),            
              broadleaved_q05 = quantile(broadleaved_mass, 0.05, na.rm=TRUE),
              broadleaved_q25 = quantile(broadleaved_mass, 0.25, na.rm=TRUE),
              broadleaved_q50 = quantile(broadleaved_mass, 0.5, na.rm=TRUE),
              broadleaved_q75 = quantile(broadleaved_mass, 0.75, na.rm=TRUE),
              broadleaved_q95 = quantile(broadleaved_mass, 0.95, na.rm=TRUE)) |> 
    ungroup()
  
  
}


hourly_summary


seasonal_summary <- list()

for (i in c("wtdeer", "roedeer")) {
  
  seasonal_summary[[i]] <- habitat_use_data[["sim"]][[i]] |> 
    mutate(hour = round(hour, 0), yday = round(yday, 0), 
           forest = case_when(is.na(canopy) ~ 0, .default=1),
           season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                              yday %in% 61:151 ~ "Spring", 
                              yday %in% 152:243 ~ "Summer", 
                              yday %in% 244:334 ~ "Autumn", 
                              .default=NA),
           season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |> 
    filter(!is.na(season)) |> 
    group_by(hour, season, id) |> 
    summarize(canopy = mean(canopy, na.rm=TRUE),
              forest = mean(forest, na.rm=TRUE),
              agriculture = mean(agriculture, na.rm=TRUE),
              urban = mean(urban, na.rm=TRUE),
              broadleaved_mass = mean(broadleaved_biomass, na.rm=TRUE),
              age = mean(stand_age, na.rm=TRUE)) |> 
    group_by(hour, season) |> 
    summarize(canopy_q05 = quantile(canopy, 0.05, na.rm=TRUE),
              canopy_q25 = quantile(canopy, 0.25, na.rm=TRUE),
              canopy_q50 = quantile(canopy, 0.5, na.rm=TRUE),
              canopy_q75 = quantile(canopy, 0.75, na.rm=TRUE),
              canopy_q95 = quantile(canopy, 0.95, na.rm=TRUE),
              forest_q05 = quantile(forest, 0.05, na.rm=TRUE),
              forest_q25 = quantile(forest, 0.25, na.rm=TRUE),
              forest_q50 = quantile(forest, 0.5, na.rm=TRUE),
              forest_q75 = quantile(forest, 0.75, na.rm=TRUE),
              forest_q95 = quantile(forest, 0.95, na.rm=TRUE),
              agriculture_q05 = quantile(agriculture, 0.05, na.rm=TRUE),
              agriculture_q25 = quantile(agriculture, 0.25, na.rm=TRUE),
              agriculture_q50 = quantile(agriculture, 0.50, na.rm=TRUE),
              agriculture_q75 = quantile(agriculture, 0.75, na.rm=TRUE),
              agriculture_q95 = quantile(agriculture, 0.95, na.rm=TRUE),
              urban_q05 = quantile(urban, 0.05, na.rm=TRUE),
              urban_q25 = quantile(urban, 0.25, na.rm=TRUE),
              urban_q50 = quantile(urban, 0.5, na.rm=TRUE),
              urban_q75 = quantile(urban, 0.75, na.rm=TRUE),
              urban_q95 = quantile(urban, 0.95, na.rm=TRUE), 
              age_q05 = quantile(age, 0.05, na.rm=TRUE),
              age_q25 = quantile(age, 0.25, na.rm=TRUE),
              age_q50 = quantile(age, 0.5, na.rm=TRUE),
              age_q75 = quantile(age, 0.75, na.rm=TRUE),
              age_q95 = quantile(age, 0.95, na.rm=TRUE),
              broadleaved_q05 = quantile(broadleaved_mass, 0.05, na.rm=TRUE),
              broadleaved_q25 = quantile(broadleaved_mass, 0.25, na.rm=TRUE),
              broadleaved_q50 = quantile(broadleaved_mass, 0.5, na.rm=TRUE),
              broadleaved_q75 = quantile(broadleaved_mass, 0.75, na.rm=TRUE),
              broadleaved_q95 = quantile(broadleaved_mass, 0.95, na.rm=TRUE)) |> 
    ungroup()
  
  
}

seasonal_summary



## Time of the year (yday) plots ####

### Observed and simulated ####

# Canopy cover
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Canopy - simulated") +

ggplot(data=yday_summary[["obs"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Canopy - observed") +
  
ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Canopy - simulated") +
  
ggplot(data=yday_summary[["obs"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Canopy - observed") +
  plot_layout(axis="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month") &
  labs(y="Canopy cover (%)")

# Broadleaved biomass
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Broadleaved biomass - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Broadleaved biomass - observed") +
  
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Broadleaved biomass - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Broadleaved biomass - observed") +
  plot_layout(axis="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month") &
  labs(y="Broadleaved biomass 10 kg/ha")
  

# Agriculture
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Agriculture - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Agriculture - observed") +
  
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Agriculture - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Agriculture - observed") +
  plot_layout(axis="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month") &
  labs(y="Agricultural land use (0-1)")

# Forest
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest - observed") +
  
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest - observed") +
  plot_layout(axis="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month") &
  labs(y="Forest use (0-1)")

# Stand age
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest age - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest age - observed") +
  
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest age - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest age - observed") +
  plot_layout(axis="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month") &
  labs(y="Forest age")

# Urban
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Urbanity - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Urbanity - observed") +
  
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Urbanity - simulated") +
  
  ggplot(data=yday_summary[["obs"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Urbanity - observed") +
  plot_layout(axis="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month") &
  labs(y="Urban land use (0-1)")


### Simulated only ####


# Roe deer 

## Canopy cover
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Canopy cover", y="Canopy cover (%)") +
  
  
## Stand age
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(subtitle="Forest stand age", y="Forest age") +


## Agriculture
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(subtitle="Agricultural land", y="Use of agricultural land (0-1)") +
  
## Forest
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(subtitle="Forest", y="Forest use (0-1)") +
  
## Broadleaved biomass
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(subtitle="Broadleaved biomass", y="Broadleaved biomass (10 kg / ha)")  +

## Urban
ggplot(data=yday_summary[["sim"]][["roedeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(subtitle="Urban", y="Use of urban areas (0-1)") + 
  
guide_area() +
plot_layout(ncol=2, guides="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month", expand=0) &
  theme(legend.direction = "horizontal") &
  guides(fill = guide_legend(nrow = 1, title.position = "top"))


ggsave(filename="Figures/roedeer_yday_habitat_use.png",
       width=25, height=30, units="cm", dpi=600)


# White-tailed deer

## Canopy cover
ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Canopy cover", y="Canopy cover (%)") +
  
  
  ## Stand age
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(subtitle="Forest stand age", y="Forest age") +
  
  
  ## Agriculture
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(subtitle="Agricultural land", y="Use of agricultural land (0-1)") +
  
  ## Forest
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(subtitle="Forest", y="Forest use (0-1)") +
  
  ## Broadleaved biomass
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(subtitle="Broadleaved biomass", y="Broadleaved biomass (10 kg / ha)")  +
  
  ## Urban
  ggplot(data=yday_summary[["sim"]][["wtdeer"]], aes(x=yday_to_date(yday))) +
  season_layer +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="black", fill="black", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="black", fill="black") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(subtitle="Urban", y="Use of urban areas (0-1)") + 
  
  guide_area() +
  plot_layout(ncol=2, guides="collect") &
  scale_x_date(date_breaks = "1 month", date_labels = "%m", name="Month", expand=0) &
  theme(legend.direction = "horizontal") &
  guides(fill = guide_legend(nrow = 1, title.position = "top"))


ggsave(filename="Figures/wtdeer_yday_habitat_use.png",
       width=25, height=30, units="cm", dpi=600)


## Hourly plots ####



### Observed and simulated ####

# Canopy cover
ggplot(data=hourly_summary[["sim"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Canopy - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Canopy - observed") +
  
  ggplot(data=hourly_summary[["sim"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Canopy - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Canopy - observed") +
  plot_layout(axis="collect") &
  scale_x_continuous(breaks = seq(0, 25, by=2), name="Hour") &
  labs(y="Canopy cover (%)")

# Broadleaved biomass
ggplot(data=hourly_summary[["sim"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Broadleaved biomass - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Broadleaved biomass - observed") +
  
  ggplot(data=hourly_summary[["sim"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Broadleaved biomass - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Broadleaved biomass - observed") +
  plot_layout(axis="collect") &
  scale_x_continuous(breaks = seq(0, 25, by=2), name="Hour") &
  labs(y="Broadleaved biomass 10 kg/ha")


# Agriculture
ggplot(data=hourly_summary[["sim"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Agriculture - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Agriculture - observed") +
  
  ggplot(data=hourly_summary[["sim"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Agriculture - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Agriculture - observed") +
  plot_layout(axis="collect") &
  scale_x_continuous(breaks = seq(0, 25, by=2), name="Hour") &
  labs(y="Agricultural land use (0-1)")

# Forest
ggplot(data=hourly_summary[["sim"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest - observed") +
  
  ggplot(data=hourly_summary[["sim"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest - observed") +
  plot_layout(axis="collect") &
  scale_x_continuous(breaks = seq(0, 25, by=2), name="Hour") &
  labs(y="Forest use (0-1)")

# Stand age
ggplot(data=hourly_summary[["sim"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest age - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Forest age - observed") +
  
  ggplot(data=hourly_summary[["sim"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest age - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Forest age - observed") +
  plot_layout(axis="collect") &
  scale_x_continuous(breaks = seq(0, 25, by=2), name="Hour") &
  labs(y="Forest age")

# Urban
ggplot(data=hourly_summary[["sim"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Urbanity - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["roedeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="Roe deer", subtitle="Urbanity - observed") +
  
  ggplot(data=hourly_summary[["sim"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="orange", fill="orange", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="orange", fill="orange") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Urbanity - simulated") +
  
  ggplot(data=hourly_summary[["obs"]][["wtdeer"]], aes(x=hour)) +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, color="blue", fill="blue", linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1, color="blue", fill="blue") +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(title="White-tailed deer", subtitle="Urbanity - observed") +
  plot_layout(axis="collect") &
  scale_x_continuous(breaks = seq(0, 25, by=2), name="Hour") &
  labs(y="Urban land use (0-1)")


## Seasonal, hourly habitat use ####


season_cols <- c(Winter = "steelblue", Spring = "palegreen4",
                 Summer = "goldenrod", Autumn = "tan4")

# Roe deer
seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1) +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(subtitle="Forest age", y="Forest stand age") +
  facet_wrap(~season) +
  
  seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1) +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(subtitle="Forest canopy cover", y="Canopy cover (%)") +
  facet_wrap(~season) +
  
  seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1) +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(subtitle="Broadleaved biomass", y="Broadleaved biomass, foliage (10 kg/ha)") +
  facet_wrap(~season) +
  plot_layout(guides="collect", nrow=3) &
  scale_fill_manual(values = season_cols,
                    name = "Season") &
  scale_color_manual(values = season_cols,
                     name = "Season") &
  scale_x_continuous(breaks=seq(0,23, by=2), expand=c(0,0)) &
  coord_cartesian(xlim=c(0,23))

ggsave(filename="Figures/roedeer_hourly_forest_features.png",
       width=25, height=30, units="cm", dpi=600)



seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1) +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(subtitle="Agriculture", y="Use of agricultural land (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1) +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(subtitle="Forest", y="Forest use (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1) +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(subtitle="Urban", y="Use of urban areas (0-1)") +
  facet_wrap(~season) +
  plot_layout(guides="collect", nrow=3) &
  scale_fill_manual(values = season_cols,
                    name = "Season") &
  scale_color_manual(values = season_cols,
                     name = "Season") &
  scale_x_continuous(breaks=seq(0,23, by=2), expand=c(0,0)) &
  coord_cartesian(xlim=c(0,23))

ggsave(filename="Figures/roedeer_hourly_habitat_use.png",
       width=25, height=30, units="cm", dpi=600)


# White-tailed deer
seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=age_q05, ymax=age_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=age_q25, ymax=age_q75), alpha=0.1) +
  geom_line(aes(y=age_q50), linewidth=1) +
  labs(subtitle="Forest age", y="Forest stand age") +
  facet_wrap(~season) +
  
  seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=canopy_q05, ymax=canopy_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=canopy_q25, ymax=canopy_q75), alpha=0.1) +
  geom_line(aes(y=canopy_q50), linewidth=1) +
  labs(subtitle="Forest canopy cover", y="Canopy cover (%)") +
  facet_wrap(~season) +
  
  seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=broadleaved_q05, ymax=broadleaved_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=broadleaved_q25, ymax=broadleaved_q75), alpha=0.1) +
  geom_line(aes(y=broadleaved_q50), linewidth=1) +
  labs(subtitle="Broadleaved biomass", y="Broadleaved biomass, foliage (10 kg/ha)") +
  facet_wrap(~season) +
  plot_layout(guides="collect", nrow=3) &
  scale_fill_manual(values = season_cols,
                    name = "Season") &
  scale_color_manual(values = season_cols,
                     name = "Season") &
  scale_x_continuous(breaks=seq(0,23, by=2), expand=c(0,0)) &
  coord_cartesian(xlim=c(0,23))

ggsave(filename="Figures/wtdeer_hourly_forest_features.png",
       width=25, height=30, units="cm", dpi=600)



seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1) +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(subtitle="Agriculture", y="Use of agricultural land (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1) +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(subtitle="Forest", y="Forest use (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1) +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(subtitle="Urban", y="Use of urban areas (0-1)") +
  facet_wrap(~season) +
  plot_layout(guides="collect", nrow=3) &
  scale_fill_manual(values = season_cols,
                    name = "Season") &
  scale_color_manual(values = season_cols,
                     name = "Season") &
  scale_x_continuous(breaks=seq(0,23, by=2), expand=c(0,0)) &
  coord_cartesian(xlim=c(0,23))

ggsave(filename="Figures/wtdeer_hourly_habitat_use.png",
       width=25, height=30, units="cm", dpi=600)


## Both species hourly habitat use together

seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1) +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title = "a) Roe deer", subtitle="Agriculture", y="Use of agricultural land (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1) +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(subtitle="Forest", y="Forest use (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["roedeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1) +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(subtitle="Urban", y="Use of urban areas (0-1)") +
  facet_wrap(~season) +

seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=agriculture_q05, ymax=agriculture_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=agriculture_q25, ymax=agriculture_q75), alpha=0.1) +
  geom_line(aes(y=agriculture_q50), linewidth=1) +
  labs(title="b) White-tailed deer", subtitle="Agriculture", y="Use of agricultural land (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=forest_q05, ymax=forest_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=forest_q25, ymax=forest_q75), alpha=0.1) +
  geom_line(aes(y=forest_q50), linewidth=1) +
  labs(subtitle="Forest", y="Forest use (0-1)") +
  facet_wrap(~season) +
  
  seasonal_summary[["wtdeer"]] |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=urban_q05, ymax=urban_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=urban_q25, ymax=urban_q75), alpha=0.1) +
  geom_line(aes(y=urban_q50), linewidth=1) +
  labs(subtitle="Urban", y="Use of urban areas (0-1)") +
  facet_wrap(~season) +
  plot_layout(guides="collect", ncol=2, axis="collect", design="AD
                                                                BE
                                                                CF") &
  scale_fill_manual(values = season_cols,
                    name = "Season") &
  scale_color_manual(values = season_cols,
                     name = "Season") &
  scale_x_continuous(breaks=seq(0,23, by=2), expand=c(0,0), name="Hour (Helsinki/Finland)") &
  coord_cartesian(xlim=c(0,23))

ggsave(filename="Figures/hourly_habitat_use.png",
       width=25, height=30, units="cm", dpi=600)


## Use of youngest forest stands

habitat_use_data[["sim"]][["roedeer"]] |> 
  mutate(hour = round(hour, 0), yday = round(yday, 0), 
         young_forest = case_when(stand_age <= 10 ~ 1, stand_age > 10 ~ 0, .default=NA),
         season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                            yday %in% 61:151 ~ "Spring", 
                            yday %in% 152:243 ~ "Summer", 
                            yday %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |> 
  filter(!is.na(season)) |> 
  group_by(hour, season, id) |> 
  summarize(young_forest = mean(young_forest, na.rm=TRUE)) |> 
  group_by(hour, season) |> 
  summarize(young_forest_q05 = quantile(young_forest, 0.05, na.rm=TRUE),
            young_forest_q25 = quantile(young_forest, 0.25, na.rm=TRUE),
            young_forest_q50 = quantile(young_forest, 0.5, na.rm=TRUE),
            young_forest_q75 = quantile(young_forest, 0.75, na.rm=TRUE),
            young_forest_q95 = quantile(young_forest, 0.95, na.rm=TRUE)) |> 
  ungroup() |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=young_forest_q05, ymax=young_forest_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=young_forest_q25, ymax=young_forest_q75), alpha=0.1) +
  geom_line(aes(y=young_forest_q50), linewidth=1) +
  labs(title="Use of forests < 10 years old", subtitle="Roe deer", y="Usage (0-1)") +
  facet_wrap(~season) +

habitat_use_data[["sim"]][["wtdeer"]] |> 
  mutate(hour = round(hour, 0), yday = round(yday, 0), 
         young_forest = case_when(stand_age <= 10 ~ 1, stand_age > 10 ~ 0, .default=NA),
         season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                            yday %in% 61:151 ~ "Spring", 
                            yday %in% 152:243 ~ "Summer", 
                            yday %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |> 
  filter(!is.na(season)) |> 
  group_by(hour, season, id) |> 
  summarize(young_forest = mean(young_forest, na.rm=TRUE)) |> 
  group_by(hour, season) |> 
  summarize(young_forest_q05 = quantile(young_forest, 0.05, na.rm=TRUE),
            young_forest_q25 = quantile(young_forest, 0.25, na.rm=TRUE),
            young_forest_q50 = quantile(young_forest, 0.5, na.rm=TRUE),
            young_forest_q75 = quantile(young_forest, 0.75, na.rm=TRUE),
            young_forest_q95 = quantile(young_forest, 0.95, na.rm=TRUE)) |> 
  ungroup() |> 
  ggplot(aes(x=hour, fill=season, color=season)) +
  night_layer +
  geom_ribbon(aes(ymin=young_forest_q05, ymax=young_forest_q95), alpha=0.1, linetype=2) +
  geom_ribbon(aes(ymin=young_forest_q25, ymax=young_forest_q75), alpha=0.1) +
  geom_line(aes(y=young_forest_q50), linewidth=1) +
  labs(subtitle="White-tailed deer", y="Usage (0-1)") +
  facet_wrap(~season) +
  plot_layout(guides="collect", axis="collect") &
  scale_x_continuous(breaks=seq(0,23, by=2), expand=c(0,0), name= "Hour (Europe/Helsinki)") &
  coord_cartesian(xlim=c(0,23)) &
  scale_fill_manual(values = season_cols,
                    name = "Season") &
  scale_color_manual(values = season_cols,
                     name = "Season")

ggsave(filename="Figures/young_forest_util.png",
       width=29, height=15, units="cm", dpi=600)


# Habitat selection ####

season_cols <- c(Winter = "steelblue", Spring = "palegreen4",
                 Summer = "goldenrod", Autumn = "tan4")



# Habitat preference for forest stand age
roedeer_forest_age <- read.csv("outputs/Roe deer/roedeer_yday_forest_suitability_by_age_hour4.csv")
wtdeer_forest_age <- read.csv("outputs/White-tailed deer/wtdeer_yday_forest_suitability_by_age_hour4.csv")

roedeer_forest_age |> 
  distinct(forest_age, n_cells) |> 
  ggplot() +
  geom_bar(aes(x=forest_age, y=n_cells), stat="identity") +
  coord_cartesian(xlim=c(0,150))


roedeer_season_forest_age <- roedeer_forest_age |> 
  mutate(season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                            yday %in% 61:151 ~ "Spring", 
                            yday %in% 152:243 ~ "Summer", 
                            yday %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |> 
  filter(forest_age <= 150, forest_age > 0) |> 
  group_by(season, forest_age) |> 
  summarize(habitat_prob = mean(mean_prediction, na.rm=TRUE)) |> 
  ungroup()


wtdeer_season_forest_age <- wtdeer_forest_age |> 
  mutate(season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                            yday %in% 61:151 ~ "Spring", 
                            yday %in% 152:243 ~ "Summer", 
                            yday %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |> 
  filter(forest_age <= 150, forest_age > 0) |> 
  group_by(season, forest_age) |> 
  summarize(habitat_prob = mean(mean_prediction, na.rm=TRUE)) |> 
  ungroup()


# Use of forests by age (all seasons)
habitat_roedeer_sim %>%
  mutate(yday = round(yday),
         hour = round(hour, 0),
         season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                            yday %in% 61:151 ~ "Spring", 
                            yday %in% 152:243 ~ "Summer", 
                            yday %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>%
  filter(!is.na(season), !is.na(stand_age), hour %in% c(5,6)) %>%
  ggplot(aes(x = stand_age, fill = season)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2,
                 position = "dodge", boundary = 0, alpha=0.5) +
  facet_wrap(~season) +
  labs(y = "Density", x="Forest stand age", fill="Season", title="a) Roe deer", subtitle="Density of use") +
  theme(legend.position = "inside", legend.position.inside = c(0.88, 0.80)) +
  
  habitat_wtdeer_sim %>%
  mutate(yday = round(yday),
         hour = round(hour, 0),
         season = case_when(yday %in% c(335:366, 1:60) ~ "Winter", 
                            yday %in% 61:151 ~ "Spring", 
                            yday %in% 152:243 ~ "Summer", 
                            yday %in% 244:334 ~ "Autumn", 
                            .default=NA),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>%
  filter(!is.na(season), !is.na(stand_age), hour %in% c(5,6)) %>%
  ggplot(aes(x = stand_age, fill = season)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2,
                 position = "dodge", boundary = 0, alpha=0.5) +
  labs(y = "Density", x="Forest stand age", fill="Season", title="b) White-tailed deer", subtitle="Density of use") +
  theme(legend.position = "inside", legend.position.inside = c(0.88, 0.80)) +
  facet_wrap(~season) +
  
  
  ggplot(data=roedeer_season_forest_age, aes(x=forest_age, y=habitat_prob, fill=season)) +
  stat_summary(fun="mean", geom="bar", position='dodge', alpha=0.5) +
  labs(x="Forest stand age", y="Mean habitat probability", subtitle="Habitat preference") +
  facet_wrap(~season) +
  
  ggplot(data=wtdeer_season_forest_age, aes(x=forest_age, y=habitat_prob, fill=season)) +
  stat_summary(fun="mean", geom="bar", position='dodge', alpha=0.5) +
  labs(x="Forest stand age", y="Mean habitat probability", subtitle="Habitat preference") +
  facet_wrap(~season) +
  plot_layout(guides="collect", nrow=2, design="AB
                                                CD") &
  scale_x_continuous(breaks=c(0, 10, 30, 50, 70, 90, 110, 130, 150), limits=c(0,150)) &
  scale_fill_manual(values = season_cols,
                    name = "Season") 

ggsave(filename="Figures/forest_age_season.png",
       width=25, height=20, units="cm", dpi=600)


## Prediction maps for AOIs ####
library(ggnewscale)

roedeer_winter.r <- rast("outputs/Roe deer/habitat_landscape_yday1_hourly.tif")
names(roedeer_winter.r) <- c("January 04:00", "January 16:00")

roedeer_summer.r <- rast("outputs/Roe deer/habitat_landscape_yday180_hourly.tif")
names(roedeer_summer.r) <- c("Summer 02:00", "Summer 17:00")


wtdeer_winter.r <- rast("outputs/White-tailed deer/habitat_landscape_yday1_hourly.tif")
names(wtdeer_winter.r) <- c("January 04:00", "January 16:00")

wtdeer_summer.r <- rast("outputs/White-tailed deer/habitat_landscape_yday180_hourly.tif")
names(wtdeer_summer.r) <- c("Summer 02:00", "Summer 17:00")

wtdeer_pred.r <- c(wtdeer_winter.r, wtdeer_summer.r)
roedeer_pred.r <- c(roedeer_winter.r, roedeer_summer.r)

names(roedeer_pred.r)
names(wtdeer_pred.r)

raster_names <- setNames(rep("", length(names(roedeer_pred.r))), names(roedeer_pred.r))

raster_names
# Model is trained on UTC time, so let's see what the time in Finland would be:
with_tz(as.POSIXct("2010-01-01 04:00", tz="UTC"), "Europe/Helsinki")
with_tz(as.POSIXct("2010-01-01 16:00", tz="UTC"), "Europe/Helsinki")
with_tz(as.POSIXct("2010-06-30 02:00", tz="UTC"), "Europe/Helsinki")
with_tz(as.POSIXct("2010-06-30 17:00", tz="UTC"), "Europe/Helsinki")

raster_names[[1]] = "January 1st, 06:00 EET"
raster_names[[2]] = "January 1st, 18:00 EET"
raster_names[[3]] = "June 30th, 05:00 EEST"
raster_names[[4]] = "June 30th, 20:00 EEST"

raster_names


aoi1 <- vect("Data/aoi1.gpkg")
aoi2 <- vect("Data/aoi2.gpkg")

habitat_aoi1 <- crop(habitat_stack, aoi1)
habitat_aoi2 <- crop(habitat_stack, aoi2)


# Roe deer
# Selected AOIs: aoi5, aoi6 and aoi7
aoi5 <- vect("Data/aoi5.gpkg")
aoi6 <- vect("Data/aoi6.gpkg")
aoi7 <- vect("Data/aoi7.gpkg")

habitat_aoi5 <- crop(habitat_stack, aoi5)
habitat_aoi6 <- crop(habitat_stack, aoi6)
habitat_aoi7 <- crop(habitat_stack, aoi7)


## AOI 5 
(ggplot() +
  geom_spatraster(data=crop(roedeer_pred.r, aoi5)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +

ggplot() +
  geom_spatraster(data=habitat_aoi5[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["water"]]) +
  geom_spatraster(data=habitat_aoi5[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi5[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["water"]]) +
  geom_spatraster(data=habitat_aoi5[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi5[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi5[["water"]]) +
  geom_spatraster(data=habitat_aoi5[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )) +
plot_annotation(title="Roe deer", subtitle="AOI A", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/roedeer_predmap_aoi5.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)


## AOI 6 
(ggplot() +
  geom_spatraster(data=crop(roedeer_pred.r, aoi6)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi6[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["water"]]) +
  geom_spatraster(data=habitat_aoi6[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi6[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["water"]]) +
  geom_spatraster(data=habitat_aoi6[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi6[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi6[["water"]]) +
  geom_spatraster(data=habitat_aoi6[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))  +
plot_annotation(title="Roe deer", subtitle="AOI B", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/roedeer_predmap_aoi6.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)


## AOI 7
(ggplot() +
  geom_spatraster(data=crop(roedeer_pred.r, aoi7)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi7[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["water"]]) +
  geom_spatraster(data=habitat_aoi7[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi7[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["water"]]) +
  geom_spatraster(data=habitat_aoi7[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi7[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi7[["water"]]) +
  geom_spatraster(data=habitat_aoi7[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))  +
plot_annotation(title="Roe deer", subtitle="AOI C", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/roedeer_predmap_aoi7.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)


# White-tailed deer
# Selected AOIs: AOI 8, AOI 9, AOI 10, AOI 11
aoi8 <- vect("Data/aoi8.gpkg")
aoi9 <- vect("Data/aoi9.gpkg")
aoi10 <- vect("Data/aoi10.gpkg")
aoi11 <- vect("Data/aoi11.gpkg")

habitat_aoi8 <- crop(habitat_stack, aoi8)
habitat_aoi9 <- crop(habitat_stack, aoi9)
habitat_aoi10 <- crop(habitat_stack, aoi10)
habitat_aoi11 <- crop(habitat_stack, aoi11)

## AOI 8
(ggplot() +
  geom_spatraster(data=crop(wtdeer_pred.r, aoi8)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi8[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["water"]]) +
  geom_spatraster(data=habitat_aoi8[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi8[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["water"]]) +
  geom_spatraster(data=habitat_aoi8[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi8[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi8[["water"]]) +
  geom_spatraster(data=habitat_aoi8[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))  +
plot_annotation(title="White-tailed deer", subtitle="AOI D", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/wtdeer_predmap_aoi8.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)


## AOI 9
(ggplot() +
  geom_spatraster(data=crop(wtdeer_pred.r, aoi9)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi9[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["water"]]) +
  geom_spatraster(data=habitat_aoi9[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi9[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["water"]]) +
  geom_spatraster(data=habitat_aoi9[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi9[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi9[["water"]]) +
  geom_spatraster(data=habitat_aoi9[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))   +
plot_annotation(title="White-tailed deer", subtitle="AOI E", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/wtdeer_predmap_aoi9.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)


## AOI 10
(ggplot() +
  geom_spatraster(data=crop(wtdeer_pred.r, aoi10)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi10[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["water"]]) +
  geom_spatraster(data=habitat_aoi10[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi10[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["water"]]) +
  geom_spatraster(data=habitat_aoi10[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi10[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi10[["water"]]) +
  geom_spatraster(data=habitat_aoi10[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )) +
plot_annotation(title="White-tailed deer", subtitle="AOI F", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))


ggsave("Figures/wtdeer_predmap_aoi10.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)


## AOI 11
(ggplot() +
  geom_spatraster(data=crop(wtdeer_pred.r, aoi11)) +
  scale_fill_viridis_c() +
  facet_wrap(~lyr, labeller = as_labeller(raster_names)) +
  labs(fill="log(Habitat probability)") +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi11[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["water"]]) +
  geom_spatraster(data=habitat_aoi11[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Canopy cover")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi11[["stand_age"]]) +
  scale_fill_steps(name="Stand age (years)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["water"]]) +
  geom_spatraster(data=habitat_aoi11[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Stand age")) +
  
  ggplot() +
  geom_spatraster(data=habitat_aoi11[["broadleaved_biomass"]]) +
  scale_fill_steps(name="Broadleaved biomass (10 kg / ha)", low="white", high="darkgreen",
                   limits = c(0, 150),
                   breaks = c(10, 20, 40, 60, 80, 100),
                   oob = scales::squish, 
                   na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=habitat_aoi11[["water"]]) +
  geom_spatraster(data=habitat_aoi11[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  facet_wrap(vars("Broadleaved biomass")) +
  
  guide_area() +
  
  plot_layout(nrow=1, design="AA
                              AA
                              BC
                              DE",  guides="collect") &
  coord_sf(expand = FALSE, datum = NA) &
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.box           = "vertical",   # legends on top of each other
    legend.box.just      = "left",       # left-align the stack
    legend.justification = "left",
    legend.title.position = "top",
    legend.title         = element_text(size = 7),
    legend.text          = element_text(size = 6),
    legend.key.width     = unit(1, "cm"),
    legend.key.height    = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.0, "cm"),
    legend.margin        = margin(1, 1, 1, 1),
    legend.box.margin    = margin(1, 1, 10, 30),
    plot.margin          = margin(2, 2, 2, 2),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))  +
  plot_annotation(title="White-tailed deer", subtitle="AOI G", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/wtdeer_predmap_aoi11.pdf",
       width = 19, height = 27, units = "cm", dpi = 600)



## Combine PDFs into one file 

# Only use after every PDF have been generated

library(qpdf)
library(stringr)

files <- list.files(
  path = "Figures",
  pattern = "predmap_aoi[0-9]+\\.pdf$",
  full.names = TRUE
)

files_ordered <- files[c(1,2,3,6,7,4,5)]

qpdf::pdf_combine(input = files_ordered, output = "Figures/prediction_booklet.pdf")



# Test individuals ####

roedeer_test <- read.csv("outputs/Roe deer/next_step_probs.csv") |> 
  mutate(season = case_when(yday_t1 %in% c(335:366, 1:60) ~ "Winter", 
                            yday_t1 %in% 61:151 ~ "Spring", 
                            yday_t1 %in% 152:243 ~ "Summer", 
                            yday_t1 %in% 244:334 ~ "Autumn", 
                              .default=NA))

wtdeer_test <- read.csv("outputs/White-tailed deer/next_step_probs.csv") |> 
  mutate(season = case_when(yday_t1 %in% c(335:366, 1:60) ~ "Winter", 
                            yday_t1 %in% 61:151 ~ "Spring", 
                            yday_t1 %in% 152:243 ~ "Summer", 
                            yday_t1 %in% 244:334 ~ "Autumn", 
                              .default=NA))

head(roedeer_test)

ggplot(data=roedeer_test) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  facet_wrap(~id) +
ggplot(data=roedeer_test) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  plot_layout(nrow=2)+
  plot_annotation(title="Roe deer")

ggplot(data=wtdeer_test) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  facet_wrap(~id) +
ggplot(data=wtdeer_test) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  plot_layout(nrow=2) +
  plot_annotation(title="White-tailed deer")


roedeer_test |> 
  group_by(season) |> 
  summarize(mean = mean(habitat_prob), 
            "25%" = quantile(habitat_prob, 0.25),
            "50%" = quantile(habitat_prob, 0.50),
            "75%" = quantile(habitat_prob, 0.75),
            uniform = 1/141^2)

wtdeer_test |> 
  group_by(season) |> 
  summarize(mean = mean(habitat_prob), 
            "25%" = quantile(habitat_prob, 0.25),
            "50%" = quantile(habitat_prob, 0.50),
            "75%" = quantile(habitat_prob, 0.75),
            uniform = 1/141^2)

# Calculate what is the role of habitat selection in predicting next step location
d <- roedeer_test %>%
  filter(next_step_prob > 0, move_prob > 0) %>%   # row 1 has zeros: drop it (first step / outside window?)
  mutate(
    ll_full = log(next_step_prob),
    ll_move = log(move_prob),
    ll_unif = log(1 / 141^2),
    delta   = ll_full - ll_move,     # >0: habitat helps beyond movement
    day     = as.Date(t1_)
  )

# per-individual summary (nats/step)
d %>% group_by(id) %>%
  summarise(n = n(),
            nll_full = -mean(ll_full),
            nll_move = -mean(ll_move),
            nll_unif = -mean(ll_unif),
            delta    = mean(delta),
            gain     = mean(ll_full - ll_move),
            frac_pos = mean((ll_full - ll_move) > 0))

d <- wtdeer_test %>%
  filter(next_step_prob > 0, move_prob > 0) %>%   # row 1 has zeros: drop it (first step / outside window?)
  mutate(
    ll_full = log(next_step_prob),
    ll_move = log(move_prob),
    ll_unif = log(1 / 141^2),
    delta   = ll_full - ll_move,     # >0: habitat helps beyond movement
    day     = as.Date(t1_)
  )

# per-individual summary (nats/step)
d %>% group_by(id) %>%
  summarise(n = n(),
            nll_full = -mean(ll_full),
            nll_move = -mean(ll_move),
            nll_unif = -mean(ll_unif),
            delta    = mean(delta),
            gain     = mean(ll_full - ll_move),
            frac_pos = mean((ll_full - ll_move) > 0))


roedeer_test_sf <-   st_as_sf(roedeer_test, coords=c("x1_","y1_"), crs=3067, remove=FALSE)
wtdeer_test_sf <-   st_as_sf(wtdeer_test, coords=c("x1_","y1_"), crs=3067, remove=FALSE)

plot_layer <- function(rast, sf_data) {

  r = crop(rast, sf_data)

  ggplot() +
  geom_spatraster(data=r[["canopy"]]) +
  scale_fill_distiller(name="Canopy cover (%)", palette="Greens", direction=1, 
                       limits=c(0,100), 
                       breaks=c(0,25,50,75,100), 
                       na.value="white") +
  new_scale_fill() +
  geom_spatraster(data=r[["agriculture"]]) +
  scale_fill_gradient(name="Agriculture", low = NA, high = "wheat", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=r[["urban"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "red", na.value = NA, guide="none") +
  new_scale_fill() +
  geom_spatraster(data=r[["water"]]) +
  geom_spatraster(data=r[["wetlands"]]) +
  scale_fill_gradient(name="Urban", low = NA, high = "midnightblue", na.value = NA, guide="none") +
  geom_sf(data=sf_data, size=0.2, alpha=0.2) +
  scale_color_viridis_c() +
  theme_minimal() &
  coord_sf(expand = FALSE, datum = NA)
}

unique(roedeer_test_sf$id)

(ggplot(data=filter(roedeer_test_sf, id =="cc_fi_09_002")) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
ggplot(data=filter(roedeer_test_sf, id =="cc_fi_09_007")) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  annotate("text", x=1/141^2, y=12000, label="uniform distribution", hjust = -0.05, alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
ggplot(data=filter(roedeer_test_sf, id =="cc_fi_10_003")) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
plot_layer(habitat_stack, filter(roedeer_test_sf, id =="cc_fi_09_002")) +
plot_layer(habitat_stack, filter(roedeer_test_sf, id =="cc_fi_09_007")) +
plot_layer(habitat_stack, filter(roedeer_test_sf, id =="cc_fi_10_003")) +
plot_layout(guides="collect", nrow=2) &
  theme(
    legend.position = "none",
    plot.margin = margin(0, 5, 2, 5),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))  +
  plot_annotation(title="Roe deer", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/roedeer_testing.png",
      width=20, height=15, units="cm", dpi=600)

unique(wtdeer_test_sf$id)

(ggplot(data=filter(wtdeer_test_sf, id =="ov_fi_09_005")) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
ggplot(data=filter(wtdeer_test_sf, id =="ov_fi_10_001")) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  annotate("text", x=1/141^2, y=6000, label="uniform distribution", hjust = -0.05, alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
ggplot(data=filter(wtdeer_test_sf, id =="ov_fi_12_001")) +
  geom_density(aes(x=habitat_prob), color="black", fill="cyan", alpha=0.5) +
  geom_vline(xintercept=1/141^2, linetype=2) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
plot_layer(habitat_stack, filter(wtdeer_test_sf, id =="ov_fi_09_005")) +
plot_layer(habitat_stack, filter(wtdeer_test_sf, id =="ov_fi_10_001")) +
plot_layer(habitat_stack, filter(wtdeer_test_sf, id =="ov_fi_12_001")) +
plot_layout(guides="collect", nrow=2) &
  theme(
    legend.position = "none",
    plot.margin = margin(0, 5, 2, 5),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ))  +
  plot_annotation(title="White-tailed deer", 
                  theme = theme(plot.margin = margin(t = 20, r = 10, b = 30, l = 10, unit = "pt")))

ggsave("Figures/wtdeer_testing.png",
      width=20, height=12, units="cm", dpi=600)








