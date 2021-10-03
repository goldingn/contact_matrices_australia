# calibrate setting weights against case age distribution data from england over
# time, with Delta, post-reopening and pre-vaccination of 12-15 year olds
england_infections <- get_england_infections()

# compute vaccine efficacy against onward infection and onward transmission,
# assuming mean of pfizer and AZ assumptions from national plan phase 1
# modelling
efficacy <- list(
  infection = list(
    dose_1_only = 0.25,
    dose_2 = 0.7
  ),
  onward_transmission = list(
    dose_1_only = 0.47,
    dose_2 = 0.65
  )
)

england_vaccination_effect <- get_england_vaccination_coverage() %>%
  mutate(
    dose_1_only = dose_1 - dose_2,
    infection_multiplier = 1 - (dose_1_only * efficacy$infection$dose_1_only +
      dose_2 * efficacy$infection$dose_2),
    onward_transmission_multiplier = 1 - (dose_1_only * efficacy$onward_transmission$dose_1_only +
      dose_2 * efficacy$onward_transmission$dose_2)
  ) %>%
  select(
    age,
    infection_multiplier,
    onward_transmission_multiplier
  )
  
# get england infection age distribution and vaccination effects for ages 2-85
england_data <- england_infections %>%
  group_by(age) %>%
  summarise(
    across(
      infections,
      mean
    )
  ) %>%
  left_join(
    england_vaccination_effect,
    by = "age"
  )

# build England-specific contact matrices

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# define age limits (integer years from 2-85, as per prevalence data)
age_breaks <- seq(2, 86, by = 1)

age_lookup <- get_age_group_lookup(
  age_breaks
)

# get setting- and age-specific transmission probabilities in 5y bins
setting_transmission <- read_csv(
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
  left_join(
    age_lookup,
    by = c(case_age = "age")
  ) %>%
  select(
    -case_age
  ) %>%
  rename(
    case_age = age_group, 
  ) %>%
  left_join(
    age_lookup,
    by = c(contact_age = "age")
  ) %>%
  select(
    -contact_age
  ) %>%
  rename(
    contact_age = age_group
  ) %>%
  mutate(
    across(
      ends_with("age"),
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
    case_age,
    contact_age
  ) %>%
  summarise(
    across(
      probability,
      mean
    ),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(case_age),
    !is.na(contact_age)
  )

# convert to a list of matrices
setting_transmission_matrices <- setting_transmission %>%
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

# number of households of each size in UK in 2020 from:
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/families/datasets/familiesandhouseholdsfamiliesandhouseholds
# convert to population-average (not household average) household size, assuming
# households of 7+ all have size 7
uk_household_sizes <- tribble(
  ~household_size, ~n_households,
  1, 7898,
  2, 9675,
  3, 4337,
  4, 4095,
  5, 1246,
  6, 377,
  7, 163
) %>%
  mutate(
    n_people = household_size * n_households,
    fraction = n_people / sum(n_people)
  ) %>%
  summarise(
    mean_household_size = sum(household_size * fraction)
  )

england_population <- get_england_population() %>%
  rename(
    lower.age.limit = age
  )

# return a list of setting-specific contact matrices for England states,
# accounting for population age distributions and household sizes
setting_contact_matrices <- tibble(
  household_size = uk_household_sizes$mean_household_size,
  population = list(england_population)
) %>%
  rowwise() %>%
  mutate(
    setting_matrices = list(
      predict_setting_contacts(
        contact_model = model,
        population = population,
        age_breaks = age_breaks
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
  ) %>%
  select(
    setting_matrices
  ) %>%
  unnest(
    cols = c(setting_matrices)
  ) %>%
  pull(setting_matrices) %>%
  `[`(1:4)

# get a matrix of the effects of vaccines
vaccine_effect <- outer(
  england_data$infection_multiplier,
  england_data$onward_transmission_multiplier,
  FUN = "*"
)




library(greta.dynamics)

# compute the set of weights that best fit the VIC case data
# make them sum to 1, since the stable age distribution is invariant to the  
weights <- simplex_variable(4)

setting_weights <- list(
  home = weights[1],
  school = weights[2],
  work = weights[3],
  other = weights[4]
)

setting_weighted_contacts <- mapply(
  `*`,
  setting_contact_matrices,
  setting_weights,
  SIMPLIFY = FALSE
)

# get next generation matrix, pre-vaccination
ngm_pre_vacc <- Reduce("+", setting_weighted_contacts) * setting_transmission_matrices$household

# apply vaccination effects
ngm <- ngm_pre_vacc * vaccine_effect

# use better initial age distribution for faster sampling
ngm_unscaled  <- Reduce("+", setting_contact_matrices) * setting_transmission_matrices$household * vaccine_effect
stable_age <- Re(eigen(ngm_unscaled)$vectors[, 1])
initial <- stable_age / sum(stable_age)

solutions <- greta.dynamics::iterate_matrix(ngm, initial_state = initial)
stable_age_distribution <- solutions$stable_distribution
infections <- round(england_data$infections)

counts <- as_data(t(infections))

# define the likelihood
distribution(counts) <- multinomial(
  size = sum(infections),
  prob = t(stable_age_distribution)
)

# fit the model by HMC
m <- model(weights)
draws <- mcmc(m)

# check sampling
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
bayesplot::mcmc_trace(draws)
ages <- colnames(setting_contact_matrices$home) %>%
  str_replace(",", "-") %>%
  parse_number()
# plot posterior mean distribution and infections
stable_sims <- calculate(stable_age_distribution, values = draws, nsim = 1000)[[1]]
stable_sims_mean <- colMeans(stable_sims[, , 1])
stable_sims_mean <- stable_sims_mean / sum(stable_sims_mean)
par(mfrow = c(2, 1), mar = c(2, 2, 2, 1) + 0.1)
bp <- barplot(stable_sims_mean,
              main = "England modelled",
              xlab = "case ages",
              names.arg = ages)
abline(v = bp[ages == 12], lty = 2)
barplot(infections,
        main = "England observed",
        xlab = "case ages",
        names.arg = ages)
abline(v = bp[ages == 12], lty = 2)

# also plot prediction with posterior estimate of weights

