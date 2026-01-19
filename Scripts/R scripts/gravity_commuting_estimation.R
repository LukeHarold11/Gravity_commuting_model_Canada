rm(list = ls())
library("pacman")
pacman::p_load(
  DBI, RSQLite, readr, dplyr, janitor, stringr, ggplot2, tidyr, sf,
  ggspatial,DoubleML, grf, fixest, stringr, broom, fixest, purrr, future, forecast, haven
)

grav_commuting <- read_csv("Data/Cleaned commuting file/commuting_data_gravity.csv")

#are commuters always zero if res_csduid == pow_csduid?
# grav_commuting %>%
#   filter(res_csduid == pow_csduid) %>%
#   summarise(total_commuters = sum(commuters)) %>%
#   pull(total_commuters)


grav_commuting <- grav_commuting %>%
  mutate(
    ldist  = log(dist_km),
    res_id = as.integer(factor(res_csduid)),
    pow_id = as.integer(factor(pow_csduid))
  )

grav_ppml <- fepois(
  commuters ~ ldist | res_id + pow_id,
  data = grav_commuting
)

summary(grav_ppml)
etable(grav_ppml)

#graph
df_pos <- grav_commuting %>% filter(commuters > 0)

# Residualize log(commuters) and log(distance) on origin+destination FE
rY <- resid(feols(log(commuters) ~ 1 | res_id + pow_id, data = df_pos))
rD <- resid(feols(ldist          ~ 1 | res_id + pow_id, data = df_pos))

plot_df <- tibble(rD = rD, rY = rY)

ggplot(plot_df, aes(rD, rY)) +
  geom_point(alpha = 0.05, size = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Residual log distance (net of origin & destination FE)",
    y = "Residual log commuters (net of origin & destination FE)",
    title = "Distance decay in commuting flows (residualized)"
  )
