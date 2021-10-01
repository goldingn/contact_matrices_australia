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

lgairsad_simplified <- lgairsad %>%
  filter(
    count != 0,
    irsad_deciles_at_lga_level_area != "Total",
    irsad_deciles_at_lga_level_area != "Not applicable"
  ) %>%
  mutate(
    decile = sub(
      "Decile ",
      "",
      irsad_deciles_at_lga_level_area
    )
  ) %>%
  dplyr::select(lga, decile)

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
  left_join(conmat::abs_lga_lookup, by='lga') %>%
  mutate(
    label = paste0(state, "-", decile)
  )

#tps_vax[, label := paste0(state, "-", decile)] breaking for GR, added mutate above


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


national_plan_vaccination <- readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
  ungroup() %>%
  filter(
    vacc_schoolkids
  ) %>%
  select(
    -vacc_scenario,
    -vacc_schoolkids,
    -vacc_relative_efficacy
  )


aspirational_vaccination <- expand_grid(
  vacc_coverage = c(0.9, 1),
  age_band_5y = unique(national_plan_vaccination$age_band_5y)
) %>%
  left_join(
    fraction_eligible_lookup,
    by = c("age_band_5y")
  ) %>%
  # add on average efficacy on transmission, given an assumed AZ/Pfizer mix, based on age groups
  mutate(
    fraction_pfizer = case_when(
      age_band_5y %in% c("60-64", "65-69", "70-74", "75-79", "80+") ~ 1,
      TRUE ~ 1
    ),
    average_efficacy_transmission = fraction_pfizer * efficacy_pf_2_dose + (1 - fraction_pfizer) * efficacy_az_2_dose,
    proportion_vaccinated = vacc_coverage * fraction_eligible,
    vacc_effect = 1 - proportion_vaccinated * average_efficacy_transmission
  ) %>%
  select(
    -fraction_eligible,
    -fraction_pfizer
  )

vaccination_coverages <- bind_rows(
  national_plan_vaccination,
  aspirational_vaccination
) %>%
  filter(
    vacc_coverage %in% c(0.7, 0.8, 0.9, 1)
  )


optimal_ttiq_baseline <- 2.93
partial_ttiq_baseline <- 3.62


lga_reduction_partial <- lapply(seq_along(lga_ngms_filter), function(i) {
    l <- lga_ngms_filter[[i]]
    vaccination_coverages %>%
      group_by(vacc_coverage) %>%
      summarise(
        tp_reduction = vacc_tp_reduction(vacc_effect, l)  
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
        vacc_coverage,
        population_group
      ) %>%
      
      mutate(
        lga=names(lga_ngms_filter)[i]
      )
  }) %>% rbindlist() %>%
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
    )
  )
  
  

lga_reduction_optimal <- lga_reduction_partial %>%
  mutate(
    across(
      c(tp_baseline, post_vacc_tp),
      ~ . * optimal_ttiq_baseline / partial_ttiq_baseline
    ),
    tp_percent_reduction = 100 * (1 - post_vacc_tp / tp_baseline),
  )


lga_tp_reduction <- bind_rows(
  optimal = lga_reduction_optimal,
  partial = lga_reduction_partial,
  .id = "ttiq"
)


saveRDS(
  lga_tp_reduction,
  file = "outputs/lga_tp.RDS"
)

lga_tp <- readRDS(
  file = "outputs/lga_tp.RDS"
)
  


colours <- RColorBrewer::brewer.pal(3, "Set2")

baseline_colour <- washout(colours[2], 0.8)
vaccine_colours <- washout(colours[3], c(0.7, 0.5, 0.25, 0.1))

border_colour <- grey(0.6)
r0_colour <- grey(0.5)
label_colour <- grey(0.3)
text_size <- 2.5

for (ttiq_plot in c("partial", "optimal")) {
  
  first_scenario <- levels(lga_tp$scenario)[1]
  lga_tp %>%
    filter(
      ttiq == ttiq_plot
    ) %>%
    select(
      -tp_percent_reduction
    ) %>%
    pivot_wider(
      names_from = vacc_coverage,
      values_from = post_vacc_tp,
      names_prefix = "tp_coverage_"
    ) %>%
    control_base_plot() %>%
    add_context_hline(
      label = "Control",
      at = 1,
      linetype = 2,
      text_size = text_size * 1.3
    ) %>%
    add_context_hline(
      label = "Delta R0 (for Australia)",
      at = 8,
      linetype = 2,
      text_size = text_size * 1.3
    ) %>%
    # add the vaccination + ttiq effect as a box
    add_single_box(
      top = r0,
      bottom = tp_baseline,
      box_colour = baseline_colour,
      only_scenarios = first_scenario,
      text_main = paste0(
        "baseline\nPHSM\n&\n",
        ttiq_plot,
        "\nTTIQ"
      )
    ) %>%
    add_single_box(
      top = tp_baseline,
      bottom = tp_coverage_0.7,
      box_colour = vaccine_colours[1],
      text_main = "70%\nvaccination\ncoverage",
      only_scenarios = first_scenario
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.7,
      bottom = tp_coverage_0.8,
      reference = tp_baseline_vacc,
      text_main = "80%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[2]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.8,
      bottom = tp_coverage_0.9,
      reference = tp_baseline_vacc,
      text_main = "90%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[3]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.9,
      bottom = tp_coverage_1,
      reference = tp_baseline_vacc,
      text_main = "100%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[4]
    ) %>%
    add_arrow(8) +
    theme(
      axis.text.x = element_text(
        size = 10,
        colour = grey(0.1)
      )
    )
  
  ggsave(
    sprintf("outputs/lga_tp_figure_%s.png", ttiq_plot),
    bg = "white",
    width = 8,
    height = 6
  )
  
}

