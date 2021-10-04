# do some checks on fit of the Davies and Eyre age-specific transmission
# probability assumptions

# define age limits
age_limits_5y <- c(seq(0, 80, by = 5), Inf)

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# get setting- and age-specific transmission probabilities in 5y bins
setting_transmission_5y <- read_csv(
  "outputs/eyre_setting_transmission_probabilities.csv",
  col_types = cols(
    setting = col_character(),
    case_age = col_double(),
    contact_age = col_double(),
    case_age_5y = col_character(),
    contact_age_5y = col_character(),
    probability = col_double()
  )
) %>%
  mutate(
    across(
      ends_with("age_5y"),
      ~factor(
        .x,
        levels = str_sort(
          unique(.x),
          numeric = TRUE
        ))
    )
  ) %>%
  group_by(
    setting,
    case_age_5y,
    contact_age_5y
  ) %>%
  summarise(
    across(
      probability,
      mean
    ),
    .groups = "drop"
  ) %>%
  rename(
    case_age = case_age_5y,
    contact_age = contact_age_5y
  )

# convert to a list of matrices
setting_transmission_matrices <- setting_transmission_5y %>%
  nest(
    data = c(case_age, contact_age, probability)
  ) %>%
  mutate(
    data = lapply(
      data,
      pivot_wider,
      names_from = contact_age,
      values_from = probability
    ),
    data = lapply(data, column_to_rownames, "case_age"),
    data = lapply(data, as.matrix)
  ) %>%
  pivot_wider(
    names_from = setting,
    values_from = data
  ) %>%
  as.list() %>%
  lapply(pluck, 1)

setting_contact_matrices <- abs_pop_age_lga_2020 %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  mutate(
    country = "Australia"
  ) %>%
  nest(
    population = -country
  ) %>%
  rowwise() %>%
  mutate(
    household_size = get_mean_household_size(),
    setting_matrices = list(
      predict_setting_contacts(
        contact_model = model,
        population = population,
        age_breaks = age_limits_5y
      )
    ),
    setting_matrices = list(
      adjust_household_contact_matrix(
        setting_matrices = setting_matrices,
        household_size = household_size,
        population = population
      )
    )
  ) %>%
  select(
    setting_matrices
  ) %>%
  unnest(
    cols = c(setting_matrices)
  ) %>%
  pull(setting_matrices) %>%
  `[`(1:4)

# weight the settings to represent NSW lockdown
setting_weights <- list(
  home = 1,
  school = 0.1,
  work = 0.1,
  other = 0.1
)

# apply age effects from Davies and Eyre
australia_ngm_unscaled_davies <- apply_age_contribution(
  setting_contact_matrices$home * setting_weights$home +
    setting_contact_matrices$school * setting_weights$school +
    setting_contact_matrices$work * setting_weights$work +
    setting_contact_matrices$other * setting_weights$other
)

australia_ngm_unscaled_eyre <-
  setting_contact_matrices$home * setting_weights$home * setting_transmission_matrices$household +
  setting_contact_matrices$school * setting_weights$school * setting_transmission_matrices$household +
  setting_contact_matrices$work * setting_weights$work * setting_transmission_matrices$household +
  setting_contact_matrices$other * setting_weights$other * setting_transmission_matrices$household

# calibrate both to R = 3.6
australia_ngm_davies <- australia_ngm_unscaled_davies * find_m(3.6, australia_ngm_unscaled_davies)
australia_ngm_eyre <- australia_ngm_unscaled_eyre * find_m(3.6, australia_ngm_unscaled_eyre)

# check
get_R(australia_ngm_davies)
get_R(australia_ngm_eyre)

stable_age <- function(matrix) {
  ages <- Re(eigen(matrix)$vectors[, 1])
  stable <- ages / sum(ages)
  names(stable) <- colnames(matrix)
  stable
}

# before-vaccination age distribution
barplot(stable_age(australia_ngm_davies))
barplot(stable_age(australia_ngm_eyre))

# apply vaccination and check age distribution and effect on TP

national_plan_vaccination <- readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
  ungroup() %>%
  filter(
    !vacc_schoolkids,
    vacc_coverage == 0.5
  ) %>%
  select(
    -vacc_scenario,
    -vacc_coverage,
    -vacc_schoolkids,
    -vacc_relative_efficacy
  )

australia_ngm_davies_vaccinated <- sweep(australia_ngm_davies, 1, national_plan_vaccination$vacc_effect, FUN = "*")
australia_ngm_eyre_vaccinated <- sweep(australia_ngm_eyre, 1, national_plan_vaccination$vacc_effect, FUN = "*")

1 - get_R(australia_ngm_davies_vaccinated) / get_R(australia_ngm_davies)
1 - get_R(australia_ngm_eyre_vaccinated) / get_R(australia_ngm_eyre)

stable_5y_davies <- stable_age(australia_ngm_davies_vaccinated)
stable_5y_eyre <- stable_age(australia_ngm_eyre_vaccinated)
barplot(stable_5y_davies)
barplot(stable_5y_eyre)
# rel <- rel / sum(rel)
# barplot(rel, names.arg = colnames(australia_ngm_vaccinated))

stable_5y_to_10y <- function(stable_5y) {
  stable_10y <- c(stable_5y[seq(1, 15, by = 2)] + stable_5y[seq(2, 16, by = 2)], stable_5y[17])
  stable_10y <- stable_10y / sum(stable_10y)
  lower <- seq(0, 80, by = 10)
  upper <- paste0("-", lower + 9)
  upper[length(upper)] <- "+"
  names(stable_10y) <- paste0(lower, upper)
  stable_10y
}

stable_10y_davies <- stable_5y_to_10y(stable_5y_davies)
stable_10y_eyre <- stable_5y_to_10y(stable_5y_eyre)

par(mfrow = c(1, 2))
barplot(stable_10y_davies, main = "Davies")
barplot(stable_10y_eyre, main = "Eyre")




national_plan_vaccination <- readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
  ungroup() %>%
  filter(
    vacc_schoolkids,
    vacc_coverage == 0.8
  ) %>%
  select(
    -vacc_scenario,
    -vacc_coverage,
    -vacc_schoolkids,
    -vacc_relative_efficacy
  )

australia_ngm_davies_vaccinated <- sweep(australia_ngm_davies, 1, national_plan_vaccination$vacc_effect, FUN = "*")
australia_ngm_eyre_vaccinated <- sweep(australia_ngm_eyre, 1, national_plan_vaccination$vacc_effect, FUN = "*")

round(1 - get_R(australia_ngm_davies_vaccinated) / get_R(australia_ngm_davies), 4)
round(1 - get_R(australia_ngm_eyre_vaccinated) / get_R(australia_ngm_eyre), 4)

