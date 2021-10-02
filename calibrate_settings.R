# calibrate wetting weights against 10y case age distribution data from VIC

# load VIC case age data in 10y age bins
cases <- read_csv(
  "data/vic_cases_by_age_group_20211002.csv",
  col_types = cols(
    diagnosis_date = col_date(format = ""),
    agegroup = col_character()
  )
) %>%
  rename(
    age_group = agegroup
  ) %>%
  filter(
    diagnosis_date > as.Date("2021-09-01"),
    age_group != "Age unknown"
  ) %>%
  mutate(
    age_group = case_when(
      age_group %in% c("80-89", "90+") ~ "80+",
      age_group == "10-19_" ~ "10-19",
      TRUE ~ age_group
    )
  ) %>%
  group_by(age_group) %>%
  summarise(
    count = n()
  )

# define age limits
age_breaks <- c(seq(0, 80, by = 10), Inf)

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

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
  rename(
    case_age = case_age,
    contact_age = contact_age
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

# return a list of setting-specific contact matrices for NSW states, accounting
# for population age distributions and household sizes
setting_contact_matrices <- tibble(
  state = "VIC"
) %>%
  rowwise() %>%
  mutate(
    household_size = get_mean_household_size(
      state = state
    ),
    population = list(
      abs_age_state(state)
    ),
    setting_matrices = list(
      predict_setting_contacts(
        contact_model = model,
        population = abs_age_state(state),
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

# need to load age-specific vaccination coverages too, and do multiple
# observations with different coverages to get more age-specific information
stop("incorporate vaccination")

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

ngm <- Reduce("+", setting_weighted_contacts) * setting_transmission_matrices$household

# use better initial age distribution for faster sampling
ngm_unscaled  <- Reduce("+", setting_contact_matrices) * setting_transmission_matrices$household
stable_age <- Re(eigen(ngm_unscaled)$vectors[, 1])
initial <- stable_age / sum(stable_age)

solutions <- greta.dynamics::iterate_matrix(ngm, initial_state = initial)
stable_age_distribution <- solutions$stable_distribution

counts <- as_data(t(cases$count))
# define the likelihood
distribution(counts) <- multinomial(
  size = sum(cases$count),
  prob = t(stable_age_distribution)
)

# fit the model by HMC
m <- model(weights)
draws <- mcmc(m)

# check sampling
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
bayesplot::mcmc_trace(draws)

stable_sims <- calculate(stable_age_distribution, values = draws, nsim = 1000)[[1]]
stable_sims_mean <- colMeans(stable_sims[, , 1])
stable_sims_mean <- stable_sims_mean / sum(stable_sims_mean)
barplot(stable_sims_mean)
barplot(cases$count)

# after accounting for vaccination, try modelling the weights as a function of
# google metrics and school/work closures?
