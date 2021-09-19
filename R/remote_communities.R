# synthetic contact matrices from First nations remote communities

# 1. use conmat to estimate contacts in remote communities in NT
# 2. replace household contacts with an interpolated estimate from Vino et al.
# 3. calibrate household to non-household contact ratio from CAMP-remote study

# 1. use conmat to estimate contacts in remote communities in NT

# get population distribution for health districts in NT
age_limits_5y <- c(seq(0, 80, by = 5), Inf)
age_limits_broad <- c(0, 5, 16, Inf)

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

# extrapolate synthetic contact matrices from polymod to this population age
# distribution
remote_matrix <- nt_remote_aboriginal_pop %>%
  conmat::extrapolate_polymod(
    age_breaks = age_limits_5y
  )

# plot these
plot_setting_matrices(
  remote_matrix,
  "NT remote aboriginal populations (extrapolated from polymod)"
)

# to do: add household structure, workforce participation, school participation,
# and calibrate non-household vs. household based on surveys

# diagnose why school is wonky

# compute calibration factors for household contacts, based on broad groupings
# from Vino et al. study

# 2. replace household contacts with an interpolated estimate from Vino et al.

# first, get the extrapolated household matrix in these broad age groups
remote_matrix_broad <- nt_remote_aboriginal_pop %>%
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

plot_matrix(remote_matrix_broad$home) +
  ggtitle("polymod extrapolated") +
plot_matrix(remote_household_matrix_broad) +
  ggtitle("Vino et al")

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

age_group_lookup_5y <- tibble(
  age =  0:100
) %>%
  mutate(
    age_group = case_when(
      age < 5 ~ "baby",
      age < 16 ~ "child",
      TRUE ~ "adult"
    )
  )

nt_aboriginal_pop_fun <- get_nt_aboriginal_pop_function()

# get household correction factors, at 5y level
household_correction_factor_broad <- remote_household_matrix_broad / remote_matrix_broad$home
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
    population_from = nt_aboriginal_pop_fun(age_from),
    population_to = nt_aboriginal_pop_fun(age_to),
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
  
# apply the correction
remote_matrix_updated <- remote_matrix
remote_matrix_updated$home <- remote_matrix_updated$home * household_correction_factor_5y
remote_matrix_updated$all <- with(remote_matrix_updated,
                          home + school + work + other)
plot_setting_matrices(remote_matrix)
plot_setting_matrices(remote_matrix_updated)
  
colSums(remote_matrix_updated$home) / colSums(remote_matrix$home)

# # impute ages (in 5 year bins) from distributions, compute contact matrices, and average:
# remote_household_minimal <- remote_households %>%
#   mutate(
#     contact_count = case_when(
#       age == "adult" ~ count - 1,
#       TRUE ~ count
#     )
#   ) %>%
#   select(
#     -shire,
#     -overall_no,
#     -n_rooms,
#     -count
#   )
# 
# 
# # improve this imputation by only imputing the *other* ages, and keeping the
# # respondent age known
# 
# # create a tibble with simulation IDs and integer ages for each age bracket
# sims <- expand_grid(
#   sim = 1:50,
#   household = unique(remote_households$idnum),
#   age = 0:100
# ) %>%
#   # fill in the population sizes, and append the age brackets
#   mutate(
#     population = nt_aboriginal_pop_fun(age),
#     age_group = case_when(
#       age < 5 ~ "baby",
#       age < 16 ~ "child",
#       TRUE ~ "adult"
#     )
#   ) %>%
#   # join on the counts by household and age bracket
#   left_join(
#     remote_household_minimal,
#     by = c(
#       age_group = "age",
#       household = "idnum"
#     )
#   ) %>%
#   # track the respondent age, and remove the respondent from the rest of the count
#   mutate(
#     respondent = as.numeric(respondent_age == age),
#   )
#   # within each simulation, household ID, and age bracket, do a multinomial draw
#   # to impute the household size
#   group_by(
#     sim, household, age_group
#   ) %>%
#   mutate(
#     contacts = rmultinom(
#       n = 1,
#       size = first(count),
#       prob = population / sum(population)
#     )[, 1],
#     contacts = as.numeric(contacts)
#   ) %>%
#   ungroup() %>%
#   select(
#     -age_group,
#     -count,
#     -population
#   )
# 
# # for each simulation and household, build the age-structured matrix in long form
# contact_matrices_per_household <- expand_grid(
#   sim = unique(sims$sim),
#   household = unique(sims$household),
#   age_from = unique(sims$age),
#   age_to = unique(sims$age)
# ) %>%
#   # get the number of household members of each age ('to') for hypothetical respondents
#   # of each age ('from'), including themselves
#   left_join(
#     sims,
#     by = c(
#       "sim",
#       "household",
#       age_to = "age"
#     )
#   ) %>%
#   # remove the respondent from the count, to get the number of people they would
#   # come into contact with
#   mutate(
#     respondent = as.numeric(age_from == age_to),
#     contacts = contacts - respondent,
#     contacts = pmax(0, contacts)
#   ) %>%
#   # then average these across all simulations to get an imputed matrix for each
#   # household
#   group_by(
#     household, age_from, age_to
#   ) %>%
#   summarise(
#     contacts = mean(contacts),
#     .groups = "drop"
#   )
# 
# household_contact_matrix <- contact_matrices_per_household %>%
#   group_by(age_from, age_to) %>%
#   summarise(
#     contacts = mean(contacts),
#     .groups = "drop"
#   ) %>%
#   tidyr::pivot_wider(
#     names_from = age_to,
#     values_from = contacts
#   ) %>%
#   tibble::column_to_rownames(
#     "age_from"
#   ) %>%
#   as.matrix()
# image(household_contact_matrix)

# nt_aboriginal_pop <- nt_lhd_aboriginal_pop %>%
#   group_by(
#     lower.age.limit
#   ) %>%
#   summarise(
#     across(
#       population,
#       sum
#     )
#   )
# 
# household_contact_matrix_5y <- contact_matrices_per_household %>%
#   group_by(age_from, age_to) %>%
#   summarise(
#     contacts = mean(contacts),
#     .groups = "drop"
#   ) %>%
#   conmat:::aggregate_predicted_contacts(
#     population = nt_aboriginal_pop,
#     age_breaks = age_limits_5y
#   ) %>%
#   conmat::predictions_to_matrix()
# 
# conmat::plot_matrix(household_contact_matrix_5y)
# colSums(household_contact_matrix_5y)
# colSums(barkly$home)
# 
# tibble(
#   age = 0:100
# ) %>%
#   mutate(
#     population = nt_aboriginal_pop_fun(age),
#     age_group = case_when(
#       age < 5 ~ "baby",
#       age < 16 ~ "child",
#       TRUE ~ "adult"
#     )
#   ) %>%
#   group_by(age_group) %>%
#   summarise(
#     population = sum(population)
#   ) %>%
#   mutate(
#     population_fraction = population / sum(population)
#   )
#   
# 
# remote_household_age_distribution %>%
#   mutate(
#     population_fraction = count /  sum(count)
#   )
# 
# # then average across all households to get overall
# # (or join on the household size, and summarise by household size)
# 
# 
# 
# 
# 
# # summarise
# 
# 
# 
# 
# nt_aboriginal_pop_fun <- get_nt_aboriginal_pop_function()
# 
# nt_aboriginal_school <- get_nt_aboriginal_school_participation()
# 
# nt_district_work <- get_nt_district_work()
# 
# 
# 
# 
# 
# nt_district_work %>%
#   ggplot(
#     aes(
#       x = age,
#       y = work_fraction,
#       color = district
#     )
#   ) +
#   geom_line()
# 
# nt_aboriginal_school %>%
#   ggplot(
#     aes(
#       x = age,
#       y = school_fraction
#     )
#   ) +
#   geom_line()
# 
# nt_district_work_school <- nt_district_work %>%
#   left_join(
#     nt_aboriginal_school,
#     by = "age"
#   )
