# synthetic contact matrices from First nations remote communities

# 1. use conmat to extrapolate age-specific contacts in remote communities in
# NT, from Polymod surveys, based on local population age distributions

# 2. re-calibrate household contacts based on household age structures with
# broad age groups from Vino et al.

# 3. check school, work, and other contacts relative to contact
# surveys/Australia

# 4. compute R0, TP, and vaccination effects in these communities

# 5. plot these a la national plan figure

source("packages.R")

# get population distribution for health districts in NT
age_limits_5y <- c(seq(0, 80, by = 5), Inf)

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# extrapolate (naive) setting-specific synthetic contact matrices from polymod
# to these population age distributions
remote_matrix_naive <- conmat::predict_setting_contacts(
  contact_model = model,
  population = get_nt_remote_aboriginal_pop(),
  age_breaks = age_limits_5y
)

urban_matrix_naive <- conmat::predict_setting_contacts(
  contact_model = model,
  population = get_nt_urban_aboriginal_pop(),
  age_breaks = age_limits_5y
)

# 2. re-calibrate household contacts with data from Vino et al.

# make the household contacts match the number of people who slept there last
# night. Note that for polymod (and LGA level analyses) the mean  number of
# contacts at home is generally larger than the mean number of other household
# members, however given the more fluid nature of household structure in
# aboriginal communities (and hence the household definition in Vino et al.
# being people who slept in the house the previous night), we do not apply a
# correction factor to increase the number of contacts at home.

# get household correction factors, at 5y level
remote_household_correction_factor_5y <- aboriginal_household_correction_factor_5y(
  contact_model = model, 
  age_breaks = age_limits_5y,
  remote = TRUE
)

urban_household_correction_factor_5y <- aboriginal_household_correction_factor_5y(
  contact_model = model,
  age_breaks = age_limits_5y,
  remote = FALSE
)
  
# 3. check school, work, and other contacts relative to contact
# surveys/Australia

# compare the number of social (non-household, non-work, non-school) contacts of
# at least 1h from the CAMP-remote study (median 2) to a comparable subset
# (women aged 17-37) of the polymod survey population

# female polymod respondents aged 17-37 (comparable to CAMP-remote)
polymod_comparable_ids <- polymod$participants %>%
  filter(
    part_gender == "F",
    between(part_age, 17, 37)
  ) %>% 
  pull(part_id)

# pull the mean and median number of social contacts ov longer than 1h
polymod$contacts %>%
  filter(
    part_id %in% polymod_comparable_ids,
    cnt_home == 0 & cnt_work == 0 & cnt_school == 0
  ) %>%
  group_by(part_id) %>%
  summarise(
    contacts_1h = sum(duration_multi >= 4)
  ) %>%
  ungroup() %>%
  summarise(
    across(
      starts_with("contacts"),
      .fns = list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE)
      )
    )
  )

# mean 2.09, median 1. Not obviously different to CAMP-remote, so leave the 'other' matrix it as-is

# adjust work contacts
employment_ratio <- conmat::abs_employ_age_lga %>%
  filter(
    year == max(year),
    age_group != "total"
  ) %>%
  mutate(
    population_group = case_when(
      state %in% c("NSW") ~ "NSW",
      lga %in% c(
        "Central Desert (R)",
        "MacDonnell (R)",
        "Barkly (R)",
        "East Arnhem (R)"
      ) ~ "remote aboriginal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(population_group)
  ) %>%
  group_by(population_group, age_group) %>%
  summarise(
    across(
      starts_with("total"),
      sum
    ),
    .groups = "drop"
  ) %>%
  mutate(
    work_fraction = if_else(
      total > 0,
      total_employed / total,
      0
    )
  ) %>%
  filter(total > 1000) %>%
  select(
    population_group, age_group, work_fraction, total,
  ) %>%
  pivot_wider(
    names_from = population_group,
    values_from = c(work_fraction, total)
  ) %>%
  mutate(
    ratio = `work_fraction_remote aboriginal` / work_fraction_NSW
  ) %>%
  summarise(
    ratio = weighted.mean(
      ratio,
      `total_remote aboriginal` + total_NSW,
      na.rm = TRUE)
  ) %>%
  pull(ratio)

nsw_pop_fun <- conmat::abs_state_age %>%
  filter(
    state == "NSW"
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  get_age_population_function()


nt_aboriginal_pop_fun <- get_nt_aboriginal_pop_function(remote = FALSE)

# adjust school contacts
school_ratio <- conmat::abs_education_state %>%
  filter(
    year == max(year)
  ) %>%
  mutate(
    population_group = case_when(
      state %in% c("NSW") ~ "NSW",
      state == "NT" & aboriginal_and_torres_strait_islander_status == "Aboriginal and Torres Strait Islander" ~ "NT aboriginal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(population_group)
  ) %>%
  group_by(population_group, age) %>%
  summarise(
    school_population = sum(n_full_and_part_time),
    .groups = "drop"
  ) %>%
  mutate(
    population = case_when(
      population_group == "NT aboriginal" ~ nt_aboriginal_pop_fun(age),
      TRUE ~ nsw_pop_fun(age)
    ),
    school_fraction = if_else(
      population > 0,
      school_population / population,
      0
    )
  ) %>%
  filter(
    school_population > 1000
  ) %>%
  select(
    population_group, age, school_fraction, population
  ) %>%
  pivot_wider(
    names_from = population_group,
    values_from = c(school_fraction, population)
  ) %>%
  mutate(
    ratio = `school_fraction_NT aboriginal` / school_fraction_NSW
  ) %>%
  summarise(
    ratio = weighted.mean(
      ratio,
      population_NSW + `population_NT aboriginal`,
      na.rm = TRUE
    )
  ) %>%
  pull(ratio)

# apply the household contact correction matrix
remote_matrix_updated <- remote_matrix_naive
remote_matrix_updated$home <- remote_matrix_naive$home * remote_household_correction_factor_5y

urban_matrix_updated <- urban_matrix_naive
urban_matrix_updated$home <- urban_matrix_naive$home * urban_household_correction_factor_5y

# employment rate in remote NT is approximately 47% of the NSW population that
# is broadly representative of polymod
employment_ratio

# school attendance rate in aboriginal NT is approximately 89% of NSW
# population, which is broadly representative of polymod
school_ratio

# however work/school contacts are likely compensated for by increased social
# and culutral contacts during those hours (possibly with similar age
# structures), so in the absence of data on these other contacts, leave the work
# and school contacts as they are

# update the 'all settings' matrix
remote_matrix_updated$all <- with(remote_matrix_updated,
                                  home + school + work + other)

urban_matrix_updated$all <- with(urban_matrix_updated,
                                 home + school + work + other)

# plot setting-sepcific matrices before and after updating
plot_setting_matrices(remote_matrix_naive)
plot_setting_matrices(remote_matrix_updated)

plot_setting_matrices(urban_matrix_naive)
plot_setting_matrices(urban_matrix_updated)

# plot overall contact matrices
plot_matrix(remote_matrix_naive$all) +
  ggtitle("Polymod extrapolated") +
  plot_matrix(remote_matrix_updated$all) +
  ggtitle("Polymod extrapolated\nhousehold corrected")

# get national and NT NGM and calibration
australia_ngm_unscaled <- get_australia_ngm_unscaled(
  model,
  age_breaks = age_limits_5y
)

state_ngms_unscaled <- get_state_ngms_unscaled(
  model,
  age_breaks = age_limits_5y
)

transmission_matrices <- get_setting_transmission_matrices(
  age_breaks = age_limits_5y
)
names(transmission_matrices)

base_matrices <- c("home", "school", "work", "other")

remote_nt_ngm_unscaled <- get_unscaled_ngm(
  contact_matrices = remote_matrix_updated[base_matrices],
  transmission_matrices = transmission_matrices[base_matrices]
)


urban_nt_ngm_unscaled <- get_unscaled_ngm(
  contact_matrices = urban_matrix_updated[base_matrices],
  transmission_matrices = transmission_matrices[base_matrices]
)

optimal_ttiq_baseline <- 2.93
partial_ttiq_baseline <- 3.62

m <- find_m(
  R_target = partial_ttiq_baseline,
  transition_matrix = australia_ngm_unscaled
)

australia_ngm <- australia_ngm_unscaled * m
# get_R(australia_ngm)

state_ngms <- lapply(state_ngms_unscaled, `*`, m)

nt_ngm <- state_ngms$NT

remote_nt_ngm <- remote_nt_ngm_unscaled * m

urban_nt_ngm <- urban_nt_ngm_unscaled * m


# 4. compute R0, TP, and vaccination effects in these communities


# define aspirational coverage assumptions: 90%, 100%, with and without kids, with full double dose coverage
australia_pop_fun <- abs_state_age %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  conmat::get_age_population_function()

nt_pop_fun <- abs_state_age %>%
  filter(
    state == "NT"
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  conmat::get_age_population_function()

nt_remote_aboriginal_pop_fun <- get_nt_remote_aboriginal_pop() %>%
  get_age_population_function()

nt_urban_aboriginal_pop_fun <- get_nt_urban_aboriginal_pop() %>%
  get_age_population_function()


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

# apply vaccination
aboriginal_tp_partial <- vaccination_effects %>%
  # create vaccination effect matrix
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
  # add on different ngms
  rowwise() %>%
  mutate(
    australia = list(australia_ngm),
    nt = list(nt_ngm),
    nt_remote_indigenous = list(remote_nt_ngm),
    nt_urban_indigenous = list(urban_nt_ngm),
    across(
      -starts_with("vacc"),
      .fns = list(
        tp_reduction = ~ get_R(.x * vaccination_effect_matrix) / get_R(.x)
      )
    )
  ) %>%
  select(
    vacc_coverage,
    ends_with("tp_reduction")
  ) %>%
  pivot_longer(
    ends_with("tp_reduction"),
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
    tp_baseline = case_when(
      population_group == "australia" ~ get_R(australia_ngm),
      population_group == "nt" ~ get_R(nt_ngm),
      population_group == "nt_remote_indigenous" ~ get_R(remote_nt_ngm),
      population_group == "nt_urban_indigenous" ~ get_R(urban_nt_ngm)
    ),
    .before = tp_multiplier
  ) %>%
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
  mutate(
    scenario = case_when(
      population_group == "australia" ~ "All\nAustralia",
      population_group == "nt" ~ "All\nNT",
      population_group == "nt_remote_indigenous" ~ "Remote\nIndigenous",
      population_group == "nt_urban_indigenous" ~ "Urban\nIndigenous"
    ),
    scenario = factor(
      scenario,
      levels = c(
        "All\nAustralia",
        "All\nNT",
        "Remote\nIndigenous",
        "Urban\nIndigenous"
      )
    )
  )

aboriginal_tp_optimal <- aboriginal_tp_partial %>%
   mutate(
     across(
       c(tp_baseline, post_vacc_tp),
       ~ . * optimal_ttiq_baseline / partial_ttiq_baseline
     ),
     tp_percent_reduction = 100 * (1 - post_vacc_tp / tp_baseline),
   )

aboriginal_tp <- bind_rows(
  optimal = aboriginal_tp_optimal,
  partial = aboriginal_tp_partial,
  .id = "ttiq"
)

saveRDS(
  aboriginal_tp,
  file = "outputs/aboriginal_tp.RDS"
)

aboriginal_tp <- readRDS(
  file = "outputs/aboriginal_tp.RDS"
)



# 5. plot these a la national plan figure

colours <- RColorBrewer::brewer.pal(3, "Set2")

baseline_colour <- washout(colours[2], 0.8)
vaccine_colours <- washout(colours[3], c(0.7, 0.65, 0.5, 0.35, 0.2, 0.1))

border_colour <- grey(0.6)
r0_colour <- grey(0.5)
label_colour <- grey(0.3)
text_size <- 2.5

for (ttiq_plot in c("partial", "optimal")) {
  
  first_scenario <- levels(aboriginal_tp$scenario)[1]
  aboriginal_tp %>%
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
      bottom = tp_coverage_0.5,
      box_colour = vaccine_colours[1],
      text_main = "50%\nvaccination\ncoverage",
      only_scenarios = first_scenario
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.5,
      bottom = tp_coverage_0.6,
      reference = tp_baseline_vacc,
      text_main = "60%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[2]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.6,
      bottom = tp_coverage_0.7,
      reference = tp_baseline_vacc,
      text_main = "70%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[3]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.7,
      bottom = tp_coverage_0.8,
      reference = tp_baseline_vacc,
      text_main = "80%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[4]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.8,
      bottom = tp_coverage_0.9,
      reference = tp_baseline_vacc,
      text_main = "90%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[5]
    ) %>%
    add_stacked_box(
      top = tp_coverage_0.9,
      bottom = tp_coverage_1,
      reference = tp_baseline_vacc,
      text_main = "100%",
      only_scenarios = first_scenario,
      box_colour = vaccine_colours[6]
    ) %>%
    add_arrow(8) +
    theme(
      axis.text.x = element_text(
        size = 10,
        colour = grey(0.1)
      )
    )
  
  ggsave(
    sprintf("outputs/aboriginal_tp_figure_%s.png", ttiq_plot),
    bg = "white",
    width = 8,
    height = 6
  )
  
}
