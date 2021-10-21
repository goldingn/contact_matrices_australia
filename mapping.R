# library(remotes)
# remotes::install_github("wfmackey/absmapsdata")

library(readr)
library(ggplot2)
library(sf)
library(dplyr)
library(absmapsdata)
library(conmat)

#areas <- sf::st_read("data/ASGS_2021_MAIN_STRUCTURE_GPKG_GDA2020/ASGS_2021_Main_Structure_GDA2020.gpkg")

# sh_aus <- read_sf("data/AUS_2021_AUST_SHP_GDA2020/")
# plot(sh_aus$geometry)
# 
# sh_state <- read_sf("data/STE_2021_AUST_SHP_GDA2020/")
# plot(sh_state$geometry)
# 
# sh_city <- read_sf("data/GCCSA_2021_AUST_SHP_GDA2020/")
# plot(sh_city$geometry)
# 
# sh_sa4 <- read_sf("data/SA4_2021_AUST_SHP_GDA2020/")
# plot(sh_sa4$geometry)
# 
# 
# sh_sa3 <- read_sf("data/SA3_2021_AUST_SHP_GDA2020/")
# plot(sh_sa3$geometry)
# 
# sh_sa2 <- read_sf("data/SA2_2021_AUST_SHP_GDA2020/")
# plot(sh_sa2$geometry)
# 
# sh_mb <-read_sf("data/MB_2021_AUST_SHP_GDA2020/")


wfh_index <- read_csv(
  file = "data/WorkFromHomeability_by_SA216_LGA19_reduced.csv"
)

tp_reduction <- read_csv(
  file = "data/tp_lga_withreductions.csv"
)


# subset metropolitain areas

metro <- gcc2021 %>%
  filter(
    gcc_name_2021 %in% c(
      "Greater Sydney",
      "Greater Melbourne",
      "Greater Brisbane",
      "Greater Adelaide",
      "Greater Perth",
      "Greater Hobart",
      "Greater Darwin",
      "Australian Capital Territory" 
    )
  ) %>%
  rename(
    city = gcc_name_2021
  ) %>%
  as_tibble

# calculate area of LGAs intersecting with metropolitain areas
metro_area <- lga2018 %>%
  filter(
    apply(
      X = st_intersects(., st_as_sf(metro), sparse = FALSE),
      MARGIN = 1,
      FUN = any
    )
  ) %>%
  mutate(
    lga_area = st_area(.) %>% as.numeric
  )

metro_lgas <- metro_area %>%
  as.data.frame %>%
  select(lga_name_2018, state_name_2016) %>%
  as_tibble

# saveRDS(metro_lgas, file = "outputs/metro_lgas.RDS")

# calculate area of intersection of LGAs intersecting with metropolitain areas
intersection_area <- st_intersection(lga2018, st_as_sf(metro)) %>%
  mutate(
    intersection_area = st_area(.) %>% as.numeric
  ) %>%
  as.data.frame %>%
  select(lga_name_2018, intersection_area) %>%
  as_tibble


metro_lgas <- metro_area %>%
  left_join(
    intersection_area,
    by = c("lga_name_2018")
  ) %>%
  mutate(
    pc_metro = intersection_area/lga_area * 100
  ) %>% tibble



metro_lga_list <- abs_lga_lookup %>%
  right_join(
    metro_lgas %>%
      filter(pc_metro > 5),
    by = c("lga" = "lga_name_2018")
  ) %>%
  select(state, lga)


saveRDS(
  object = metro_lga_list,
  file = "outputs/metro_lga_list.RDS"
)

# check if sensible to include in metro and adjust filter based on % metro

# VIC

ggplot(
  metro_lgas %>%
    filter(state_name_2016 == "Victoria")
) +
  geom_sf(
    aes(
      geometry = geometry,
      fill = pc_metro
    )
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0.2
  )

vic_metro_lgas <- metro_lgas %>%
  filter(
    state_name_2016 == "Victoria",
    pc_metro > 5
  )

ggplot(
  vic_metro_lgas
) +
  geom_sf(
    aes(
      geometry = geometry,
      fill = pc_metro
    )
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0.2
  )


# NSW

ggplot(
  metro_lgas %>%
    filter(state_name_2016 == "New South Wales")
) +
  geom_sf(
    aes(
      geometry = geometry,
      fill = pc_metro
    )
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0.2
  )

nsw_metro_lgas <- metro_lgas %>%
  filter(
    state_name_2016 == "New South Wales",
    pc_metro > 5
  )

ggplot(
  nsw_metro_lgas
) +
  geom_sf(
    aes(
      geometry = geometry,
      fill = pc_metro
    )
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0.2
  )


## plots of tp reduction
dpi <- 200




# Vic
tp_reduction %>%
  right_join(
    vic_metro_lgas,
    by = c("lga" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = tp
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "C"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 1
  ) +
  labs(
    title = "Baseline transmission potential",
    subtitle = "TP adjusted for household size and age structure in each local government area",
    fill = "Baseline\nTransmission\nPotential"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/baseline_tp_vic.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


tp_reduction %>%
  right_join(
    vic_metro_lgas,
    by = c("lga" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = tp_percent_reduction
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "C",
    direction = -1
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 1
  ) +
  labs(
    title = "Percentage reduction in TP",
    subtitle = "Percentage reduction in transmission potential due to vaccination",
    fill = "Transmission\nPotential\n% Reduction"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/pc_reduction_tp_vic.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


tp_reduction %>%
  right_join(
    vic_metro_lgas,
    by = c("lga" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = tp_vax
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "C"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 1
  ) +
  labs(
    title = "Post vaccination TP",
    subtitle = "Transmission potential after 80% vaccination threshold",
    fill = "Transmission\nPotential"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/tp_vax_vic.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


wfh_lga_summary %>%
  right_join(
    vic_metro_lgas,
    by = c("LGA_NAME_2018" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = wfh_mean
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "D"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Index of ability to work from home",
    subtitle = "Population-weighted mean index of ability to work from home",
    fill = "Index\nscore"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/wfh_index_mean_vic.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)

wfh_lga_summary %>%
  right_join(
    vic_metro_lgas,
    by = c("LGA_NAME_2018" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = wfh_var
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "D"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Melbourne"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Variation in ability to work from home",
    subtitle = "Variance of population-weighted index of ability to work from home",
    fill = "Variance"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/wfh_index_var_vic.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


# NSW

tp_reduction %>%
  right_join(
    nsw_metro_lgas,
    by = c("lga" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = tp
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "C"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Baseline transmission potential",
    subtitle = "TP adjusted for household size and age structure in each local government area",
    fill = "TP"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/baseline_tp_nsw.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


tp_reduction %>%
  # mutate(
  #   state_name_2016 = case_when(
  #     state == "NSW" ~ "New South Wales",
  #     TRUE ~ state
  #   )
  # ) %>%
  right_join(
    nsw_metro_lgas,
    by = c(
      "lga" = "lga_name_2018"#,
      #"state_name_2016"
    )
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = tp_percent_reduction
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "C",
    direction = -1
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Percentage reduction in TP",
    subtitle = "Percentage reduction in transmission potential due to vaccination",
    fill = "Transmission\nPotential\n% Reduction"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/pc_reduction_tp_nsw.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


tp_reduction %>%
  right_join(
    nsw_metro_lgas,
    by = c("lga" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = tp_vax
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "C"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Post vaccination TP",
    subtitle = "Transmission potential after 80% vaccination threshold",
    fill = "Transmission\nPotential"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/tp_vax_nsw.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


wfh_lga_summary %>%
  right_join(
    nsw_metro_lgas,
    by = c("LGA_NAME_2018" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = wfh_mean
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "D"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Index of ability to work from home",
    subtitle = "Population-weighted mean index of ability to work from home",
    fill = "Index\nscore"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/wfh_index_mean_nsw.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)

wfh_lga_summary %>%
  right_join(
    nsw_metro_lgas,
    by = c("LGA_NAME_2018" = "lga_name_2018")
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = wfh_var
    ),
    colour = "white",
    size = 0.5
  ) +
  scale_fill_viridis_c(
    begin = 0.1,
    end = 1,
    option = "D"
  ) +
  geom_sf(
    data = metro %>%
      filter(city == "Greater Sydney"),
    aes(geometry = geometry),
    colour = "black",
    alpha = 0,
    size = 0.7
  ) +
  labs(
    title = "Variation in ability to work from home",
    subtitle = "Variance of population-weighted index of ability to work from home",
    fill = "Variance"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/wfh_index_var_nsw.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)

# wfh index stuff - messy 
wfh_lga <- readRDS("data/wfh_ability_joined.RDS")

wfh_lga_summary <- wfh_lga  %>%
  mutate(state = STE_NAME16) %>%
  group_by(LGA_NAME_2018) %>%
  mutate(LGA_pop = sum(population * ratio, na.rm = TRUE)) %>%
  summarise(wfh_mean = Hmisc::wtd.mean(SA2_WFH, weights = population / LGA_pop * ratio, normwt = TRUE),
            wfh_var = Hmisc::wtd.var(SA2_WFH, weights = population / LGA_pop * ratio, normwt = TRUE),
            state = first(state))

saveRDS(wfh_lga_summary, "outputs/wfh_lga_summary.RDS")

wfh_index %>%
  left_join(
    tibble(sa22016) %>%
      mutate(SA2_MAIN16 = as.numeric(sa2_code_2016)),
    by = c("SA2_MAIN16")
  ) %>%
    filter(state_name_2016 == "New South Wales") %>%
    ggplot() +
    geom_sf(
      aes(
        fill = SA2_WFH,
        geometry = geometry
      ),
      colour = NA
    ) +
  scale_fill_viridis_c(option = "B", limits = c(0, 1))

nsw_lga_ix <- wfh_index %>% 
  filter(STE_NAME16 == "New South Wales") %>%
  select(LGA_NAME19)

anti_join(
  lga2018 %>%
    filter(state_name_2016 == "New South Wales"),
  wfh_index %>% 
    filter(STE_NAME16 == "New South Wales"),
  by = c("lga_name_2018" = "LGA_NAME19")
) %>%
  as_tibble %>%
  ggplot() +
  geom_sf(aes(geometry = geometry))


## wfh




lga_wfh <- wfh_index %>%
  group_by(
    STE_NAME16,
    LGA_NAME19,
    GCC_NAME16,
  ) %>%
  summarise(
    lga_wfh = mean(SA2_WFH, na.rm = TRUE)
  ) %>%
  left_join(
    lga2018,
    by = c("LGA_NAME19" = "lga_name_2018")
  )

lga_wfh %>%
  filter(STE_NAME16 == "New South Wales") %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = lga_wfh
    ),
    colour = NA
  ) +
  scale_fill_viridis_c(option = "B", limits = c(0, 1))

 lga_wfh %>%
  filter(
    STE_NAME16 == "New South Wales",
    GCC_NAME16 == "Greater Sydney"
  ) %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = lga_wfh
    )
  ) +
  scale_fill_viridis_c(option = "B", limits = c(0, 1))

lga_wfh %>%
  filter(
    STE_NAME16 == "Victoria"
  ) %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = lga_wfh
    )
  ) + 
  scale_fill_viridis_c(option = "B", limits = c(0, 1))



lga_wfh %>%
  filter(
    STE_NAME16 == "Victoria",
    GCC_NAME16 == "Greater Melbourne"
  ) %>%
  ggplot() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = lga_wfh
    )
  ) +
  scale_fill_viridis_c(option = "B", limits = c(0, 1))
