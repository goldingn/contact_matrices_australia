# synthetic contact matrices from First nations remote communities

# 1. use conmat to extrapolate age-specific contacts in remote communities in
# NT, from Polymod surveys, based on local population age distributions

# 2. re-calibrate household contacts based on household age structures with
# broad age groups from Vino et al.

# 3. re-calibrate workplace contacts based on employment in NT remote regions
# (vs NSW as proxy for polymod)

# 4. re-calibrate school contacts based on school attendance in NT ATSI
# populations (vs NSW as proxy for polymod)

# 5. re-calibrate other non-household contacts based on CAMP-remote study (vs
# polymod equivaent age/gender group)

# get population distribution for health districts in NT
age_limits_5y <- c(seq(0, 80, by = 5), Inf)

# get aboriginal populations in NT health districts
nt_lhd_aboriginal_pop <- get_nt_lhd_aboriginal_pop()

# look at age distributions
nt_lhd_aboriginal_pop %>%
  ggplot(
    aes(
      x = lower.age.limit,
      y = population
    )
  ) +
  geom_col() +
  facet_wrap(
    ~district,
    scales = "free_y"
  ) +
  theme_minimal()


# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# extrapolate (naive) setting-specific synthetic contact matrices from polymod
# to these population age distributions
remote_matrix_naive <- conmat::predict_setting_contacts(
  contact_model = model,
  population = nt_remote_aboriginal_pop,
  age_breaks = age_limits_5y
)

urban_matrix_naive <- conmat::predict_setting_contacts(
  contact_model = model,
  population = alice_urban_aboriginal_pop,
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

get_R(urban_matrix_naive$all)
get_R(urban_matrix_updated$all)

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
m <- find_m(
  R_target = 3.6,
  transition_matrix = australia_ngm_unscaled
)
australia_ngm <- australia_ngm_unscaled * m

state_ngms_unscaled <- get_state_ngms_unscaled(
  model,
  age_breaks = age_limits_5y
)
state_ngms <- lapply(state_ngms_unscaled, `*`, m)

nt_ngm <- state_ngms$NT

remote_nt_ngm_unscaled <- apply_age_contribution(remote_matrix_updated$all)
remote_nt_ngm <- remote_nt_ngm_unscaled * m

urban_nt_ngm_unscaled <- apply_age_contribution(urban_matrix_updated$all)
urban_nt_ngm <- urban_nt_ngm_unscaled * m

# apply vaccination
readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
  group_by(vacc_coverage, vacc_schoolkids) %>%
  summarise(
    australia_tp_reduction = vacc_tp_reduction(vacc_effect, australia_ngm),
    nt_tp_reduction = vacc_tp_reduction(vacc_effect, nt_ngm),
    remote_aboriginal_tp_reduction = vacc_tp_reduction(vacc_effect, remote_nt_ngm),
    urban_aboriginal_tp_reduction = vacc_tp_reduction(vacc_effect, urban_nt_ngm),
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
  mutate(
    starting_tp = case_when(
      population_group == "australia" ~ get_R(australia_ngm),
      population_group == "nt" ~ get_R(nt_ngm),
      population_group == "remote_aboriginal" ~ get_R(remote_nt_ngm),
      population_group == "urban_aboriginal" ~ get_R(urban_nt_ngm)
    ),
    .before = tp_multiplier
  ) %>%
  mutate(
    R0 = 8 * starting_tp / 3.6,
    .before = starting_tp
  ) %>%
  mutate(
    post_vacc_tp = starting_tp * tp_multiplier
  ) %>%
  select(-tp_multiplier) %>%
  print(n = Inf)
  


