rm(list = ls())
library("pacman")
pacman::p_load(
  DBI, RSQLite, readr, dplyr, janitor, stringr, ggplot2, tidyr, sf,
  ggspatial,DoubleML, grf, fixest, stringr, broom, fixest, purrr, future, forecast, haven
)

DA_cleaned <-read.csv("Data/cleaned DA/DA_2021_cleaned.csv")
DA_shape <- read_sf("Data/DA shapefile/2021 DA shape/lda_000b21a_e.shp")
CSD_shape <- read_sf("Data/CSD shapefile/Census_2021_geo/Census_2021.shp")

#what is the coordinate system of the shapes
st_crs(DA_shape)
st_crs(CSD_shape)

#join
DA_shape  <- st_make_valid(DA_shape)
CSD_shape <- st_make_valid(CSD_shape)

csd_key <- CSD_shape %>% select(CSDUID)

DA_with_CSDUID <- DA_shape %>%
  mutate(.pt = st_point_on_surface(geometry)) %>%  # always inside polygon
  st_set_geometry(".pt") %>%                       # use the point for join
  st_join(csd_key, join = st_within, left = TRUE) %>%
  st_set_geometry("geometry") %>%                  # restore DA polygons
  select(-.pt)

#attach the population column to DA_with_CSDUID
DA_cleaned <- DA_cleaned %>%
  mutate(ALT_GEO_CODE = as.character(ALT_GEO_CODE))



DA_with_CSDUID <- DA_with_CSDUID %>%
  left_join(DA_cleaned %>% select(ALT_GEO_CODE, pop2021), by = c("DAUID" = "ALT_GEO_CODE"))

#create population weighted centroids for each CSD
DA_pts <- DA_with_CSDUID %>%
  mutate(pt = st_point_on_surface(geometry))

coords <- st_coordinates(st_geometry(DA_pts$pt))

DA_pts <- DA_pts %>%
  mutate(
    x = coords[,1],
    y = coords[,2]
  ) %>%
  st_drop_geometry()

## 
# 2) pop-weighted centroid per CSD (will be NA where pop_sum==0)
csd_w <- DA_pts %>%
  group_by(CSDUID) %>%
  summarise(
    pop_sum = sum(pop2021, na.rm = TRUE),
    x_w = ifelse(pop_sum > 0, weighted.mean(x, w = pop2021, na.rm = TRUE), NA_real_),
    y_w = ifelse(pop_sum > 0, weighted.mean(y, w = pop2021, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# 3) geometric fallback centroid from CSD polygons
csd_g <- CSD_shape %>%
  st_centroid() %>%
  mutate(
    x_g = st_coordinates(geometry)[,1],
    y_g = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(CSDUID, x_g, y_g)

# 4) combine: pop-weighted if available, else geometric
CSD_centroids_full <- csd_w %>%
  left_join(csd_g, by = "CSDUID") %>%
  mutate(
    x = ifelse(!is.na(x_w), x_w, x_g),
    y = ifelse(!is.na(y_w), y_w, y_g),
    centroid_type = ifelse(!is.na(x_w), "pop_weighted", "geometric_fallback")
  ) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(CSD_shape))

# checks
stopifnot(nrow(CSD_centroids_full) == 5161)
table(CSD_centroids_full$centroid_type)

#save
st_write(CSD_centroids_full, "Data/Cleaned commuting file/CSD_centroids_full.shp", delete_dsn = TRUE)

commuting_data_cleaned <- read_csv("Data/Cleaned commuting file/commuting_data_cleaned.csv")

#create a res_csduid and work_csduid column
CSDUID_DGUID<- CSD_shape %>%
  st_drop_geometry() %>%
  select(CSDUID, DGUID)


commuting_data_cleaned <- commuting_data_cleaned %>%
  left_join(CSDUID_DGUID, by = c("res_dguid" = "DGUID")) 

#rename
commuting_data_cleaned <- commuting_data_cleaned %>%
  rename(res_csduid = CSDUID)

#join for place of work
commuting_data_cleaned <- commuting_data_cleaned %>%
  left_join(CSDUID_DGUID, by = c("pow_dguid" = "DGUID")) %>% 
  rename(pow_csduid = CSDUID)
#reorder
commuting_data_cleaned <- commuting_data_cleaned %>%
  select(REF_DATE, GEO, res_dguid, res_csduid, `Gender (3):Total - Gender[1]`, pow_dguid, pow_csduid, Coordinate)
#save
write.csv(commuting_data_cleaned, "Data/Cleaned commuting file/commuting_data_final.csv", row.names = FALSE)

#creating coordinates
centroids_xy <- CSD_centroids_full %>%
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(CSDUID, x, y)

commute_xy <- commuting_data_cleaned %>%
  left_join(centroids_xy, by = c("res_csduid" = "CSDUID")) %>%
  rename(x_o = x, y_o = y) %>%
  left_join(centroids_xy, by = c("pow_csduid" = "CSDUID")) %>%
  rename(x_d = x, y_d = y)

commute_xy <- commute_xy %>%
  mutate(
    dist_km = sqrt((x_o - x_d)^2 + (y_o - y_d)^2) / 1000
  )

commute_xy <- commute_xy %>%
  mutate(dist_km = ifelse(res_csduid == pow_csduid, 0.5, dist_km))
#rename colum
commute_xy <- commute_xy %>%
  rename(commuters = `Gender (3):Total - Gender[1]`)
#save
write.csv(commute_xy, "Data/Cleaned commuting file/commuting_data_gravity.csv", row.names = FALSE)

gravity<- read_csv("Data/Cleaned commuting file/commuting_data_gravity.csv")
