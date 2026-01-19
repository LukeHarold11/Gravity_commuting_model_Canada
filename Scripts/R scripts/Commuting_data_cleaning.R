rm(list = ls())
library("pacman")
pacman::p_load(
  DBI, RSQLite, readr, dplyr, janitor, stringr, ggplot2, tidyr, sf,
  ggspatial,DoubleML, grf, fixest, stringr, broom, fixest, purrr, future, forecast, haven
)

commuting_data<- read_csv("Data/98100459-eng/98100459.csv")

commuting_data<-commuting_data %>% 
  mutate(Coordinate = as.character(Coordinate))

smaller_commuting<- commuting_data %>% 
  select(REF_DATE, GEO, DGUID, `Place of work`, Coordinate, `Gender (3):Total - Gender[1]`)

rm(commuting_data)
rm(coordinates)
#find the order of the geo
coordinates<- smaller_commuting %>% 
  select(DGUID) %>% 
  distinct()

#create a place of work column dguid
smaller_commuting<- smaller_commuting %>% 
  mutate(pow_dguid = 0)

#populate the place of work dguid column following with the entries from coordinates 

# vector of POW DGUIDs in correct order
pow_vec <- coordinates$DGUID

# sanity check
stopifnot(nrow(coordinates) == 5161)

# populate POW DGUID by cycling every 5161 rows
smaller_commuting <- smaller_commuting %>%
  mutate(pow_dguid = rep(pow_vec, times = 5161))

#check unique
stopifnot(nrow(smaller_commuting) == length(rep(pow_vec, times = 5161)))
smaller_commuting %>%
  slice(c(1, 5161, 5162, 10322)) %>%
  select(pow_dguid)

smaller_commuting <- smaller_commuting %>%
  rename(res_dguid = DGUID)

write.csv(smaller_commuting, "Data/Cleaned commuting file/commuting_data_cleaned.csv", row.names = FALSE)

smaller_commuting<- read_csv("Data/Cleaned commuting file/commuting_data_cleaned.csv")
