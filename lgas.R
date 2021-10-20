# construct lga-specific contact amtrices, accounting for population age
# distributions and household sizes

source("packages.R")

age_breaks_5y <- c(seq(0, 80, by = 5), Inf)

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)


vaccination_effects <- expand_grid(
  vacc_coverage = seq(0.5, 1, by = 0.1),
  age_band_5y = fraction_eligible_lookup()$age_band_5y
) %>%
  left_join(
    fraction_eligible_lookup(),
    by = c("age_band_5y")
  ) %>%
  # add on average efficacy on transmission, given an assumed AZ/Pfizer mix, based on age groups
  mutate(
    proportion_vaccinated = vacc_coverage * fraction_eligible,
    ve_susceptibility = ve("susceptibility", fraction_pfizer = 1, fraction_dose_2 = 1),
    ve_onward = ve("onward", fraction_pfizer = 1, fraction_dose_2 = 1),
    across(
      starts_with("ve"),
      .fns = list(multiplier = ~1 - proportion_vaccinated * .x)
    )
  )


optimal_ttiq_baseline <- 2.93
partial_ttiq_baseline <- 3.62


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

aus_ngms <- c(
  list(
    "AUS" = australia_ngm
  ),
  state_ngms
)

aus_TPs <- aus_ngms %>%
  vapply(
    get_R,
    FUN.VALUE = numeric(1)
  )


aus_tpframe <- tibble(
  tp = aus_TPs,
  state = names(aus_TPs)
)


# Australia and state-wide reductions

aus_reduction_partial <- lapply(
  seq_along(aus_ngms),
  function(i) {
    l <- aus_ngms[[i]]
    
    vaccination_effects %>%
      group_by(vacc_coverage) %>%
      summarise(
        vaccination_effect_matrix = list(
          outer(
            ve_susceptibility_multiplier,
            ve_onward_multiplier,
            FUN = "*"
          )
        )
      ) %>%
      rowwise %>%
      mutate(
        tp_multiplier = get_R(l * vaccination_effect_matrix) / get_R(l),
        tp_percent_reduction = 100 * (1 - tp_multiplier)
      ) %>%
      arrange(
        vacc_coverage
      ) %>%
      mutate(
        state = names(aus_ngms)[i]
      )
  }
) %>%
  rbindlist() %>%
  left_join(
    aus_tpframe,
    by = "state"
  ) %>%
  as_tibble %>%
  rename(tp_baseline = tp) %>%
  mutate(
    r0 = 8 * tp_baseline / partial_ttiq_baseline,
    .before = tp_baseline
  ) %>%
  mutate(
    post_vacc_tp = tp_baseline * tp_multiplier
  ) %>%
  mutate(
    scenario = factor(
      x = state,
      levels = c("AUS", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
    )
  )


aus_reduction_optimal <- aus_reduction_partial %>%
  mutate(
    across(
      c(tp_baseline, post_vacc_tp),
      ~ . * optimal_ttiq_baseline / partial_ttiq_baseline
    ),
    tp_percent_reduction = 100 * (1 - post_vacc_tp / tp_baseline),
  )


aus_tp <- bind_rows(
  optimal = aus_reduction_optimal,
  partial = aus_reduction_partial,
  .id = "ttiq"
) %>%
  select(-tp_percent_reduction, -vaccination_effect_matrix)

## LGAs

# # get lga names
# abs_pop_age_lga_2020 %>%
#   filter(state == "VIC") %>%
#   distinct(lga) %>%
#   pull(lga)

# lgas we are interested in
# lgas <- c(
#   # NSW SEIFA 1
#   "Fairfield (C)",
#   # VIC SEIFA 1
#   "Greater Dandenong (C)",
#   # NSW SEIFA 5
#   "Northern Beaches (A)",
#   # VIC SEIFA 5
#   "Bayside (C)",
#   #SA
#   "Playford (C)",
#   "Burnside (C)",
#   "Alice Springs (T)"
# )

# lgairsad <- fread("data/lga_irsad.csv") %>% janitor::clean_names()
# lgas <- unique(lgairsad$lga)
# lgas <- lgas[lgas %in% abs_lga_lookup$lga]
# lgas <- lgas[!lgas %like% "Unincorp"]

# lgairsad_simplified <- lgairsad %>%
#   filter(
#     count != 0,
#     irsad_deciles_at_lga_level_area != "Total",
#     irsad_deciles_at_lga_level_area != "Not applicable"
#   ) %>%
#   mutate(
#     decile = sub(
#       "Decile ",
#       "",
#       irsad_deciles_at_lga_level_area
#     )
#   ) %>%
#   dplyr::select(lga, decile)

lgas <- metro_lgas %>%
  filter(
    state_name_2016 %in% c("New South Wales", "Victoria"),
    lga_name_2018 %in% abs_lga_lookup$lga
  ) %>%
  pull(lga_name_2018)


#lgas <- lgas[1:3]


transmission_matrices <- get_setting_transmission_matrices(
  age_breaks = age_limits_5y
)

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
    contact_matrices = list(
      setting_matrices[c("home", "school", "work", "other")]
    ),
    ngm_unscaled = list(
      all = get_unscaled_ngm(
        contact_matrices = contact_matrices,
        transmission_matrices = transmission_matrices
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

# saveRDS(
#   lga_ngms_unscaled,
#   "outputs/lga_ngms_unscaled_subset.RDS"
# )
# 
# lga_ngms_unscaled <- readRDS("outputs/lga_ngms_unscaled_subset.RDS")
# 
# saveRDS(
#   lga_ngms_unscaled_all,
#   "outputs/lga_ngms_unscaled_all.RDS"
# )
# lga_ngms_unscaled <- readRDS("outputs/lga_ngms_unscaled_all.RDS")

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

tpframe <- tibble(
  tp = lga_TPs,
  lga = names(lga_TPs)
)

# 
# 
# lgas_care_about <- c(
#   # NSW SEIFA 1
#   "Fairfield (C)",
#   # VIC SEIFA 1
#   "Greater Dandenong (C)",
#   # NSW SEIFA 5
#   "Northern Beaches (A)",
#   # VIC SEIFA 5
#   "Bayside (C)",
#   #SA
#   "Playford (C)",
#   "Burnside (C)",
#   "Alice Springs (T)",
#   "Maribyrnong (C)",
#   "Canterbury-Bankstown (A)",
#   "Brimbank (C)"
# )
# 
# 
# 
# # apply vaccination
# 
# vacc_reductions <- lapply(seq_along(lga_ngms_filter), function(i) {
#   l <- lga_ngms_filter[[i]]
#   readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
#     group_by(vacc_coverage, vacc_schoolkids) %>%
#     summarise(
#       tp_reduction=vacc_tp_reduction(vacc_effect, l)  
#     ) %>%
#     pivot_longer(
#       ends_with("reduction"),
#       names_to = "population_group",
#       values_to = "tp_multiplier"
#     ) %>%
#     mutate(
#       population_group = str_remove(population_group, "_tp_reduction"),
#       tp_percent_reduction = 100 * (1 - tp_multiplier)
#     ) %>%
#     arrange(
#       vacc_schoolkids,
#       vacc_coverage,
#       population_group
#     ) %>%
#     filter(
#       vacc_coverage == 0.8 & vacc_schoolkids
#     ) %>%
#     mutate(
#       lga=names(lga_ngms_filter)[i]
#     )
# }) %>% rbindlist()
# 
# 
# tps_vax <- tpframe %>% 
#   left_join(vacc_reductions, by='lga') %>%
#   mutate(tp_vax = tp * tp_multiplier) %>%
#   #left_join(lgairsad_simplified, by='lga') %>%
#   left_join(conmat::abs_lga_lookup, by='lga')# %>%
#   # mutate(
#   #   label = paste0(state, "-", decile)
#   # )

#tps_vax[, label := paste0(state, "-", decile)] breaking for GR, added mutate above


# ggplot(tps_vax[lga %in% lgas_care_about], aes(y=label))+
#   geom_segment(aes(x=tp, xend=tp_vax, yend=label), arrow=arrow(length=unit(0.1, "inches"))) +
#   geom_point(aes(x=tp), colour="blue") +
#   geom_point(aes(x=tp_vax), colour="green3") +
#   #scale_y_discrete(labels=tps_vax[lga %in% lgas_care_about, label]) +
#   labs(x="Transmission potential", y="LGA") +
#   theme_bw() +
#   xlim(c(0, NA)) +
#   geom_vline(aes(xintercept=1), linetype="dashed") +
#   coord_flip()
# 
# cowplot::save_plot(filename="plots/specific_lgas.png", last_plot())


# national_plan_vaccination <- readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
#   ungroup() %>%
#   filter(
#     vacc_schoolkids
#   ) %>%
#   select(
#     -vacc_scenario,
#     -vacc_schoolkids,
#     -vacc_relative_efficacy
#   )


lga_reduction_partial <- lapply(
  seq_along(lga_ngms_filter),
  function(i) {
    l <- lga_ngms_filter[[i]]
    
    vaccination_effects %>%
      group_by(vacc_coverage) %>%
      summarise(
        vaccination_effect_matrix = list(
          outer(
            ve_susceptibility_multiplier,
            ve_onward_multiplier,
            FUN = "*"
          )
        )
      ) %>%
      rowwise %>%
      mutate(
        tp_multiplier = get_R(l * vaccination_effect_matrix) / get_R(l),
        tp_percent_reduction = 100 * (1 - tp_multiplier)
      ) %>%
      arrange(
        vacc_coverage
      ) %>%
      mutate(
        lga = names(lga_ngms_filter)[i]
      )
  }
) %>%
  rbindlist() %>%
  left_join(
    tpframe,
    by = "lga"
  ) %>%
  as_tibble %>%
  rename(tp_baseline = tp) %>%
  mutate(
    r0 = 8 * tp_baseline / partial_ttiq_baseline,
    .before = tp_baseline
  ) %>%
  mutate(
    post_vacc_tp = tp_baseline * tp_multiplier
  ) %>%
  select(
    -tp_multiplier
  ) %>%
  left_join(abs_lga_lookup) %>%
  mutate(
    lga_short = sub(
      pattern = " \\(.*",
      replacement = "",
      x = lga
    ) %>%
      sub(
        pattern = " ",
        replacement = "\n",
        x = .
      ),
    scenario = sprintf(
      "%s\n%s",
      lga_short,
      state
    ) %>%
      as.factor
  )



lga_reduction_optimal <- lga_reduction_partial %>%
  mutate(
    across(
      c(tp_baseline, post_vacc_tp),
      ~ . * optimal_ttiq_baseline / partial_ttiq_baseline
    ),
    tp_percent_reduction = 100 * (1 - post_vacc_tp / tp_baseline),
  )


lga_tp <- bind_rows(
  optimal = lga_reduction_optimal,
  partial = lga_reduction_partial,
  .id = "ttiq"
) %>%
  select(-tp_percent_reduction, -vaccination_effect_matrix)


# saveRDS(
#   lga_tp,
#   file = "outputs/lga_tp.RDS"
# )
# 
# lga_tp <- readRDS(
#   file = "outputs/lga_tp.RDS"
# )

lga_metro_tp <- inner_join(
  lga_tp,
  metro_lga_list,
  by = c("lga", "state")
)


lga_metro_tp_vic <- lga_metro_tp %>%
  filter(state == "VIC")

lga_metro_tp_nsw <- lga_metro_tp %>%
  filter(state == "NSW")





## Example TP data

atp <- aus_tp %>%
  filter(state == "AUS", ttiq == "partial") %>%
  select(-state) %>%
  mutate(scenario = "All\nAustralia")


incr_baseline_tp <- atp %>%
  mutate(
    tp_baseline = tp_baseline * 1.25,
    scenario = "Increased\nbaseline\nTP"
  )

incr_baseline_incr_vacc_tp <- atp %>%
  mutate(
    tp_baseline = tp_baseline * 1.25,
    tp_multiplier = tp_multiplier*0.8,
    scenario = "Increased\nbaseline\nTP and\nincreased\nvaccination\neffect"
  )

decr_baseline_decr_vacc_tp <- atp %>%
  mutate(
    tp_baseline = tp_baseline / 1.2,
    tp_multiplier = tp_multiplier / 0.7,
    scenario = "Decreased\nbaseline\nTP and\ndecreased\nvaccination\neffect"
  )


decr_baseline_tp <- atp %>%
  mutate(
    tp_baseline = tp_baseline / 1.2,
    scenario = "Decreased\nbaseline\nTP"
  )

example_tp_partial <- bind_rows(
  atp,
  incr_baseline_tp,
  incr_baseline_incr_vacc_tp,
  decr_baseline_tp,
  decr_baseline_decr_vacc_tp
) %>%
  mutate(
    r0 = 8 * tp_baseline / partial_ttiq_baseline,
    .before = tp_baseline
  ) %>%
  mutate(
    post_vacc_tp = tp_baseline * tp_multiplier,
    .after = tp_multiplier
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "All\nAustralia",
        "Increased\nbaseline\nTP",
        "Increased\nbaseline\nTP and\nincreased\nvaccination\neffect",
        "Decreased\nbaseline\nTP",
        "Decreased\nbaseline\nTP and\ndecreased\nvaccination\neffect"
      )
    )
  )


example_tp_optimal <- example_tp_partial %>%
  mutate(
    across(
      c(tp_baseline, post_vacc_tp),
      ~ . * optimal_ttiq_baseline / partial_ttiq_baseline
    )
  )


example_tp <- bind_rows(
  optimal = example_tp_optimal,
  partial = example_tp_partial,
  .id = "ttiq"
) %>%
  filter(vacc_coverage <= 0.8)

## Images

# examples

save_dancing_boxplots(
  df = example_tp %>%
    select(-tp_multiplier),
  label = "example_tp"
)

# Aus and states
save_dancing_boxplots(
  df = aus_tp %>%
    select(-state, -tp_multiplier),
  label = "national_tp",
  width = 250
)

# vic metro examples

vm_r0max <- lga_metro_tp_vic %>%
  filter(lga == .$lga[which.max(.$r0)])

vm_r0min <- lga_metro_tp_vic %>%
  filter(lga == .$lga[which.min(.$r0)])

vm_rangemax <- lga_metro_tp_vic %>%
  filter(lga_short != "Melbourne") %>% # melbourne already represented
  mutate(range = r0 - post_vacc_tp) %>%
  filter(lga == .$lga[which.max(.$range)]) %>%
  select(-range)

vm_rangemin <- lga_metro_tp_vic %>%
  filter(lga_short != "Murrindindi") %>% # Murrindindi already represented
  mutate(range = r0 - post_vacc_tp) %>%
  filter(lga == .$lga[which.min(.$range)]) %>%
  select(-range)

vm_pvmax <- lga_metro_tp_vic %>%
  filter(lga == .$lga[which.max(.$post_vacc_tp)])

vm_pvmin <- lga_metro_tp_vic %>%
  filter(lga_short != "Melbourne", lga_short != "Yarra") %>% # melbourne & Yarra already represented
  filter(lga == .$lga[which.min(.$post_vacc_tp)])

vic_metro_example <- 
  bind_rows(
    vm_r0max,
    vm_r0min,
    vm_rangemax,
    vm_rangemin,
    vm_pvmax,
    vm_pvmin
  ) %>%
  mutate(scenario = as.factor(lga_short))

save_dancing_boxplots(
  df = vic_metro_example,
  label = "vic_metro_tp",
  width = 250
)

# vic metro examples

nm_r0max <- lga_metro_tp_nsw %>%
  filter(lga == .$lga[which.max(.$r0)])

nm_r0min <- lga_metro_tp_nsw %>%
  filter(lga == .$lga[which.min(.$r0)])

nm_rangemax <- lga_metro_tp_nsw %>%
  filter(lga_short != "Sydney") %>% # Sydney already represented
  mutate(range = r0 - post_vacc_tp) %>%
  filter(lga == .$lga[which.max(.$range)]) %>%
  select(-range)

nm_rangemin <- lga_metro_tp_nsw %>%
  filter(lga_short != "Oberon") %>% # Murrindindi already represented
  mutate(range = r0 - post_vacc_tp) %>%
  filter(lga == .$lga[which.min(.$range)]) %>%
  select(-range)

nm_pnmax <- lga_metro_tp_nsw %>%
  filter(lga == .$lga[which.max(.$post_vacc_tp)])

nm_pnmin <- lga_metro_tp_nsw %>%
  filter(lga_short != "Sydney") %>% # Sydney already represented
  filter(lga == .$lga[which.min(.$post_vacc_tp)])

nsw_metro_example <- 
  bind_rows(
    nm_r0max,
    nm_r0min,
    nm_rangemax,
    nm_rangemin,
    nm_pnmax,
    nm_pnmin
  ) %>%
  mutate(scenario = as.factor(lga_short))


save_dancing_boxplots(
  df = nsw_metro_example,
  label = "nsw_metro_tp",
  width = 250
)

