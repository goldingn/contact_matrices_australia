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
lgas <- lgas[lgas %in% abs_lga_lookup$lga]
lgas <- lgas[!lgas %like% "Unincorp"]

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
  "Alice Springs (T)",
  "Maribyrnong (C)",
  "Canterbury-Bankstown (A)",
  "Brimbank (C)"
)



# apply vaccination

vacc_reductions <- lapply(seq_along(lga_ngms_filter), function(i) {
  l <- lga_ngms_filter[[i]]
  readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
    group_by(vacc_coverage, vacc_schoolkids) %>%
    summarise(
      tp_reduction=vacc_tp_reduction(vacc_effect, l)  
    ) %>%
    pivot_longer(
      ends_with("reduction"),
      names_to = "population_group",
      values_to = "tp_multiplier"
    ) %>%
    mutate(
      population_group = str_remove(population_group, "_tp_reduction"),
      tp_percent_reduction = 100 * (1 - tp_multiplier)
    ) %>%
    arrange(
      vacc_schoolkids,
      vacc_coverage,
      population_group
    ) %>%
    filter(
      vacc_coverage == 0.8 & vacc_schoolkids
    ) %>%
    mutate(
      lga=names(lga_ngms_filter)[i]
    )
}) %>% rbindlist()


tps_vax <- tpframe %>% 
  left_join(vacc_reductions, by='lga') %>%
  mutate(tp_vax = tp * tp_multiplier) %>%
  left_join(lgairsad_simplified, by='lga') %>%
  left_join(conmat::abs_lga_lookup, by='lga')

tps_vax[, label := paste0(state, "-", decile)]

ggplot(tps_vax[lga %in% lgas_care_about], aes(y=label))+
  geom_segment(aes(x=tp, xend=tp_vax, yend=label), arrow=arrow(length=unit(0.1, "inches"))) +
  geom_point(aes(x=tp), colour="blue") +
  geom_point(aes(x=tp_vax), colour="green3") +
  #scale_y_discrete(labels=tps_vax[lga %in% lgas_care_about, label]) +
  labs(x="Transmission potential", y="LGA") +
  theme_bw() +
  xlim(c(0, NA)) +
  geom_vline(aes(xintercept=1), linetype="dashed") +
  coord_flip()

cowplot::save_plot(filename="plots/specific_lgas.png", last_plot())
