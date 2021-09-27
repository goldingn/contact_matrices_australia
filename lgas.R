# construct lga-specific contact amtrices, accounting for population age
# distributions and household sizes

age_breaks_5y <- c(seq(0, 80, by = 5), Inf)

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# get a national NGM, calibrated to TP = 3.6, and retain calibration
australia_ngm_unscaled <- get_australia_ngm_unscaled(
  model = model,
  age_breaks = age_breaks_5y
)
m <- find_m(3.6, australia_ngm_unscaled)
australia_ngm <- australia_ngm_unscaled * m

# get state-level next generation matrices, calibrated to the national estimate
state_ngms_unscaled <- get_state_ngms_unscaled(
  model = model,
  age_breaks = age_breaks_5y
)
state_ngms <- lapply(state_ngms_unscaled, `*`, m)

# # get lga names
# abs_pop_age_lga_2020 %>%
#   filter(state == "VIC") %>%
#   distinct(lga) %>%
#   pull(lga)

# lgas we are interested in
lgas <- c(
  # NSW SEIFA 1
  "Fairfield (C)",
  # VIC SEIFA 1
  "Greater Dandenong (C)",
  # NSW SEIFA 5
  "Northern Beaches (A)",
  # VIC SEIFA 5
  "Bayside (C)",
  #SA
  "Playford (C)",
  "Burnside (C)",
  "Alice Springs (T)"
)

lgairsad <- fread("data/lga_irsad.csv") %>% janitor::clean_names()
lgas <- unique(lgairsad$lga)
lgas <- lgas[lgas %in% abs_lga_lookup$lga][!lgas %like% "Unincorp"]

lga_ngms_unscaled <- tibble(
  lga = lgas
) %>%
  rowwise() %>%
  mutate(
    household_size = get_mean_household_size(
      lga = lga
    ),
    population = list(
      abs_age_lga(lga)
    ),
    setting_matrices = list(
      predict_setting_contacts(
        contact_model = model,
        population = population,
        age_breaks = age_breaks_5y
      )
    )
  ) %>%
  mutate(
    setting_matrices = list(
      adjust_household_contact_matrix(
        setting_matrices = setting_matrices,
        household_size = household_size,
        population = population
      )
    ),
    contact_matrix = list(
      pluck(setting_matrices, "all")
    ),
    ngm_unscaled = list(
      apply_age_contribution(
        contact_matrix
      )
    )
  ) %>%
  select(
    lga, ngm_unscaled
  ) %>%
  pivot_wider(
    names_from = lga,
    values_from = ngm_unscaled
  ) %>%
  as.list() %>%
  lapply(`[[`, 1)

# apply calibration to all LGA NGMs
lga_ngms <- lapply(lga_ngms_unscaled, `*`, m)

lga_ngms_filter <- lga_ngms[!(lga_ngms %>% sapply(function(x) any(is.nan(x))))]

# get equivalent TPs for different LGAs
lga_TPs <- lga_ngms_filter %>%
  vapply(
    get_R,
    FUN.VALUE = numeric(1)
  )

# clear sign of difference by SEIFA status though!

# we can do vaccination effects as per remote communities

# need to pull in code to make upside-down TP barplots as per the first national
# plan modelling report

tpframe <- data.table(tp=lga_TPs, lga=names(lga_TPs))
lgairsad_simplified <- lgairsad[count > 0 & (irsad_deciles_at_lga_level_area != "Total" &
                        irsad_deciles_at_lga_level_area != "Not applicable" &
                        lga != "Total"), .(lga, decile=as.numeric(gsub("Decile ", "", irsad_deciles_at_lga_level_area)))]

tpirsad <- merge(tpframe, lgairsad_simplified, by='lga', all.x=T)

ggplot(tpirsad, aes(x=decile, y=tp)) + geom_point()

lgas_care_about <- c(
  # NSW SEIFA 1
  "Fairfield (C)",
  # VIC SEIFA 1
  "Greater Dandenong (C)",
  # NSW SEIFA 5
  "Northern Beaches (A)",
  # VIC SEIFA 5
  "Bayside (C)",
  #SA
  "Playford (C)",
  "Burnside (C)",
  "Alice Springs (T)"
)
