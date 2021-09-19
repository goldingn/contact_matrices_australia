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

# 1. use conmat to estimate contacts in remote communities in NT

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

# extrapolate a matrix to all remote districts
nt_remote_aboriginal_pop <- nt_lhd_aboriginal_pop %>%
  filter(
    !(district %in% c("Alice Springs Urban", "Darwin/Casuarina"))
  ) %>%
  group_by(
    lower.age.limit
  ) %>%
  summarise(
    population = sum(population)
  )

# extrapolate (naive) setting-specific synthetic contact matrices from polymod
# to this population age distribution
remote_matrix_naive <- nt_remote_aboriginal_pop %>%
  conmat::extrapolate_polymod(
    age_breaks = age_limits_5y
  )

# 2. re-calibrate household contacts with data from Vino et al.

age_limits_broad <- c(0, 5, 16, Inf)

# first, get the extrapolated household matrix in these broad age groups
remote_matrix_naive_broad <- nt_remote_aboriginal_pop %>%
  conmat::extrapolate_polymod(
    age_breaks = age_limits_broad
  )

# load Vino et al. household data for remote communities, and aggregate sexes
remote_households <- vino_household_sizes() %>%
  group_by(
    idnum, 
    shire,
    rem_urb,
    respondent_age,
    age
  ) %>%
  summarise(
    count = sum(count),
    across(
      c(overall_no, n_rooms),
      first
    ),
    .groups = "drop"
  ) %>%
  filter(
    rem_urb == "ABC remote"
  ) %>%
  select(
    -rem_urb
  ) %>%
  # drop the Tiwi islands - economically quite different and seemingly a saller
  # household size than the mainland
  filter(
    shire != "Tiwi Islands"
  )

# # 1511 people in 185 households
# sum(remote_households$count)
# n_distinct(remote_households$idnum)
# 
# # comparable household sizes (7-8) and densities (~3 per room)
# remote_households %>%
#   group_by(idnum, shire) %>%
#   summarise(
#     count = sum(count),
#     n_rooms = first(n_rooms),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     density = count / n_rooms
#   ) %>%
#   group_by(
#     shire
#   ) %>%
#   summarise(
#     across(
#       c(count, n_rooms, density),
#       mean
#     )
#   )

# matrix in original broad groups
remote_household_matrix_broad <- remote_households %>%
  select(
    -shire,
    -n_rooms,
    -overall_no
  ) %>%
  rename(
    age_to = age
  ) %>%
  right_join(
    expand_grid(
      age_from = c("baby", "child", "adult"),
      age_to = c("baby", "child", "adult")
    ),
    by = "age_to"
  ) %>%
  relocate(idnum, age_from, age_to, count) %>%
  arrange(idnum, age_from, age_to) %>%
  mutate(
    count = ifelse(
      age_from == age_to,
      pmax(0, count - 1),
      count
    )
  ) %>%
  mutate(
    across(
      c(age_from, age_to),
      ~factor(.x, levels = c("baby", "child", "adult"))
    )
  ) %>%
  group_by(
    age_from, age_to
  ) %>%
  summarise(
    count = mean(count),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = age_to,
    values_from = count
  ) %>%
  tibble::column_to_rownames(
    "age_from"
  ) %>%
  as.matrix()

# plot these two against one another (not different scales and different broad
# mixing patterns)
plot_matrix(remote_matrix_naive_broad$home) +
  ggtitle("polymod extrapolated") +
plot_matrix(remote_household_matrix_broad) +
  ggtitle("Vino et al")

# lookup from broad age groups to integer years, to reaggregate correction
# factors at 5y intervals
age_group_lookup_broad <- tibble(
  age =  0:100
) %>%
  mutate(
    age_group = case_when(
      age < 5 ~ "baby",
      age < 16 ~ "child",
      TRUE ~ "adult"
    )
  )

# population age distribution function for aboriginal
nt_remote_aboriginal_pop_fun <- get_nt_aboriginal_pop_function(remote = TRUE)

# get household correction factors, at 5y level
household_correction_factor_broad <- remote_household_matrix_broad / remote_matrix_naive_broad$home
household_correction_factor_5y <- household_correction_factor_broad %>%
  conmat::matrix_to_predictions() %>%
  rename(
    correction = contacts
  ) %>%
  right_join(
    age_group_lookup_broad,
    by = c(age_group_from = "age_group")
  ) %>%
  rename(
    age_from = age
  ) %>%
  right_join(
    age_group_lookup_broad,
    by = c(age_group_to = "age_group")
  ) %>%
  rename(
    age_to = age
  ) %>%
  select(
    -starts_with("age_group")
  ) %>%
  mutate(
    age_group_to = cut(
      pmax(0.1, age_to),
      age_limits_5y,
      right = FALSE
    ),
    age_group_from = cut(
      pmax(0.1, age_from),
      age_limits_5y,
      right = FALSE
    ),
    population_from = nt_remote_aboriginal_pop_fun(age_from),
    population_to = nt_remote_aboriginal_pop_fun(age_to),
    population_interaction = population_from + population_to
  ) %>%
  group_by(age_group_to, age_group_from) %>%
  summarise(
    correction = weighted.mean(
      x = correction,
      w = population_interaction
    ),
    .groups = "drop"
  ) %>%
  rename(
    contacts = correction
  ) %>%
  conmat::predictions_to_matrix()
  
# compare the number of social (non-household, non-work, non-school) contacts of
# at lleast 1h from the CAMP-remote study (median 2) to a comparable subset
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
  )

nsw_pop_fun <- conmat::abs_state_age %>%
  filter(
    state == "NSW"
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  get_age_population_function()

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
      population_group == "NT aboriginal" ~ nt_aboriginal_pop_fun(age, remote = FALSE),
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
  )


# apply the household contact correction matrix
remote_matrix_updated <- remote_matrix_naive
remote_matrix_updated$home <- remote_matrix_naive$home * household_correction_factor_5y

# employment rate in remote NT is approximately 46% of the urbanised, culturally European
# Australian population that is representative of polymod, so scle down workplace contacts
remote_matrix_updated$work <- remote_matrix_naive$work * employment_ratio

# school attendance rate in aboriginal NT is approximately 89% of NSW
# population, which is broaadlys representative of polymod, so scale down school contacts
remote_matrix_updated$school <- remote_matrix_naive$school * school_ratio

# update the 'all settings' matrix
remote_matrix_updated$all <- with(remote_matrix_updated,
                                  home + school + work + other)

plot_setting_matrices(remote_matrix_naive)
plot_setting_matrices(remote_matrix_updated)

colSums(remote_matrix_updated$home) / colSums(remote_matrix_naive$home)

plot_matrix(remote_matrix_naive$all) +
  ggtitle("Polymod extrapolated") +
  plot_matrix(remote_matrix_updated$all) +
  ggtitle("Polymod extrapolated\nhousehold corrected")


