# construct lga-specific contact amtrices, accounting for population age
# distributions and household sizes

age_breaks_5y <- c(seq(0, 80, by = 5), Inf)

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  survey_population = get_polymod_population()
)

# get lga names
abs_pop_age_lga_2020 %>%
  filter(state == "VIC") %>%
  distinct(lga) %>%
  pull(lga)

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

# loop through named lgas getting setting-specific synthetic contact matrices,
# accounting for population age distributions and household sizes
contact_matrices <- lgas %>%
  set_names(
    lgas
  ) %>%
  # getting population age distributions from ABS
  lapply(
    FUN = abs_age_lga
  ) %>%
  # getting contact matrices
  lapply(
    FUN = predict_setting_contacts,
    X = .,
    contact_model = model,
    age_breaks = age_breaks_5y
  ) %>%
  # adjusting the number of household contacts for the ABS household sizes for
  # that LGA
  adjust_household_contacts()

# # plot these
# plot_setting_matrices(matrices$`Northern Beaches (A)`)
# plot_setting_matrices(matrices$`Fairfield (C)`)
# plot_matrix(matrices$`Northern Beaches (A)`$all)
# plot_matrix(matrices$`Fairfield (C)`$all)

# get the 'all contacts' matrices for each
contact_matrices_all <- contact_matrices %>%
  lapply(
    pluck,
    "all"
  )

# get unscaled (to R) next generation matrices for these, by adjusting for
# age-specific relative susceptibility and onward transmission
lga_ngms_unscaled <- contact_matrices_all %>%
  lapply(
    apply_age_contribution
  )

# get an Australia-wide unscaled NGM
australia_ngm_unscaled <- conmat::abs_pop_age_lga_2020 %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  predict_setting_contacts(
    contact_model = model,
    age_breaks = age_breaks_5y
  ) %>%
  pluck(
    "all"
  ) %>%
  apply_age_contribution()
  
# get calibration factor against a TP of 3.6 (partial TTIQ, baseline PHSM)
m <- find_m(
  R_target = 3.6,
  transition_matrix = australia_ngm_unscaled
)

australia_ngm <- australia_ngm_unscaled * m

# apply calibration to all LGA NGMs
lga_ngms <- lga_ngms_unscaled %>%
  lapply(
    `*`,
    m
  )

lga_ngms_filter <- lga_ngms[!(lga_ngms %>% sapply(function(x) any(is.nan(x))))]

# get equivalent TPs for different LGAs
lga_TPs <- lga_ngms_filter %>%
  vapply(
    get_R,
    FUN.VALUE = numeric(1)
  )

# these are all a bit lower than national - we need to adjust the Australia-specifc
# matrix for the Australia-wide mean household size

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
