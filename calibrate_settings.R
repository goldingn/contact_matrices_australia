# calibrate setting weights against case age distribution data from england over
# time, with Delta, post-reopening and pre-vaccination of 12-15 year olds

# define model age limits (integer years from 2-85, as per prevalence data extents)
age_breaks <- c(seq(0, 80, by = 1), Inf)

age_lookup <- get_age_group_lookup(
  age_breaks
)

# get age-distribution of infections in England
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
  ) %>%
  left_join(
    age_lookup,
    by = "age"
  ) %>%
  group_by(
    age_group
  ) %>%
  summarise(
    across(
      c(infection_multiplier, onward_transmission_multiplier),
      mean
    ),
    .groups = "drop"
  )

# build England-specific contact matrices

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# get setting- and age-specific transmission probabilities in these bins
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

# get a matrix of the effects of vaccination
vaccine_effect <- outer(
  england_vaccination_effect$infection_multiplier,
  england_vaccination_effect$onward_transmission_multiplier,
  FUN = "*"
)

# compute marginals of household transmission paramters and recombine, as a way
# of factoring out household-specific mixing effects (like higher-risk
# inter-couple, parent-child, grandparent-child contact). Want to weight over
# the most commonly observed age pairs (to downwieight areas computed from
# little data), but without the age data, we have toi assume this is
# proportional to the contact rate estimates

home_contacts <- conmat::matrix_to_predictions(setting_contact_matrices$home)
household_transmissions <- conmat::matrix_to_predictions(setting_transmission_matrices$household)
home_contact_transmissions <- home_contacts %>%
  mutate(
    probability = household_transmissions$contacts
  )

relative_susceptibility <- home_contact_transmissions %>%
  group_by(age_group_to) %>%
  summarise(
    probability = weighted.mean(probability, contacts),
    .groups = "drop"
  ) %>%
  mutate(
    probability = probability / max(probability)
  )
  

relative_infectiousness <- home_contact_transmissions %>%
  group_by(age_group_from) %>%
  summarise(
    probability = weighted.mean(probability, contacts),
    .groups = "drop"
  ) %>%
  mutate(
    probability = probability / max(probability)
  )

relative_transmission_matrix <- outer(
  relative_infectiousness$probability,
  relative_susceptibility$probability,
  FUN = "*"
)

# plot_matrix(setting_transmission_matrices$household)
# plot_matrix(relative_transmission_matrix)

library(greta.dynamics)

# compute the set of weights that best fit the VIC case data
# make them sum to 1, since the stable age distribution is invariant to the  
weights <- simplex_variable(4)

setting_weights <- list(
  weights[1],
  weights[2],
  weights[3],
  weights[4]
)

names(setting_weights) <- names(setting_contact_matrices)

setting_weighted_contacts <- mapply(
  `*`,
  setting_contact_matrices,
  setting_weights,
  SIMPLIFY = FALSE
)

# get next generation matrix, pre-vaccination, by weighting these matrices
ngm_pre_vacc <- setting_weighted_contacts$home * setting_transmission_matrices$household +
  setting_weighted_contacts$school * setting_transmission_matrices$work_education +
  setting_weighted_contacts$work * setting_transmission_matrices$work_education +
  setting_weighted_contacts$other * setting_transmission_matrices$events_activities

# apply vaccination effects, with parameter for reduced effectiveness of
# vaccines relative to assumed
ngm <- ngm_pre_vacc * vaccine_effect

# use better initial age distribution for faster sampling
ngm_unscaled  <- Reduce("+", setting_contact_matrices) * setting_transmission_matrices$household * vaccine_effect
stable_age <- Re(eigen(ngm_unscaled)$vectors[, 1])
initial <- stable_age / sum(stable_age)

solutions <- greta.dynamics::iterate_matrix(ngm, initial_state = initial)
stable_age_distribution <- solutions$stable_distribution

ons_index <- get_ons_age_group_lookup() %>%
  filter(
    age <= max(age_breaks[is.finite(age_breaks)])
  ) %>%
  mutate(
    index = as.numeric(age_group),
    n_index = max(index, na.rm = TRUE),
    index = replace_na(index, n_index[1] + 1)
  )
  
  # age_lookup %>%
  # rename(
  #   age_group_model = age_group
  # ) %>%
  # # colnames(setting_transmission_matrices$household) %>%
  # left_join(
  #   get_ons_age_group_lookup(),
  #   by = "age"
  # ) %>%
  # filter(
  #   !is.na(age_group)
  # ) %>%
  # select(
  #   age_group_model,
  #   age_group
  # ) %>%
  # distinct()
  

# aggregate to match age groups
stable_age_distribution_grouped_unscaled <- tapply(
  X = stable_age_distribution,
  INDEX = ons_index$index,
  FUN = "sum"
)[seq_len(ons_index$n_index[1])]

stable_age_distribution_grouped <- stable_age_distribution_grouped_unscaled / sum(stable_age_distribution_grouped_unscaled)

# relevel infections so that the sample size represents the approximately 150K samples in each fortnight of estimates
infections <- round(england_infections$infections * 150000 / sum(england_infections$infections))

counts <- as_data(t(infections))

# define the likelihood
distribution(counts) <- multinomial(
  size = sum(infections),
  prob = t(stable_age_distribution_grouped)
)

# fit the model by HMC
m <- model(weights) # , vaccine_multiplier)
draws <- mcmc(m)

# check sampling
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
bayesplot::mcmc_trace(draws)

# get posterior mean of weights parameter (ensuring it sums to 1)
weights_posterior_mean <- summary(draws)$statistics[, "Mean"]
weights_posterior_mean <- weights_posterior_mean / sum(weights_posterior_mean)
names(weights_posterior_mean) <- names(setting_weights)

# and compute stable age distribution based on the posterior mean
stable_age_post_grouped <- calculate(
  stable_age_distribution_grouped,
  values = list(weights = weights_posterior_mean)
)[[1]][, 1]

# compare outputs
par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
barplot(stable_age_post_grouped,
        main = "England modelled",
        xlab = "case ages",
        ylab = "fraction of infections",
        names.arg = england_infections$age_group)
barplot(infections,
        main = "England observed",
        xlab = "case ages",
        ylab = "number of infections",
        names.arg = england_infections$age_group)

# switch to modelling with all years, not just 2-85
# read in ons data

