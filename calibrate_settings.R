# calibrate setting weights against case age distribution data from england over
# time, with Delta, post-reopening and pre-vaccination of 12-15 year olds

# define model age limits (integer years from 2-85, as per prevalence data extents)
age_breaks <- c(seq(0, 80, by = 1), Inf)

age_lookup <- get_age_group_lookup(
  age_breaks
)

# set the order of the setting-specific matrices, to ensure they are always aligned
setting_order <- c("home", "school", "work", "other")

# get age-distribution of infections in England
england_infections <- get_england_infections()

# compute vaccine efficacy against onward infection and onward transmission,
# assume pfizer efficacy from latest parameters doc
efficacy <- list(
  infection = list(
    dose_1_only = 0.57,
    dose_2 = 0.8
  ),
  onward_transmission = list(
    dose_1_only = 0.13,
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


google_spec <- cols(
  country_region_code = col_character(),
  country_region = col_character(),
  sub_region_1 = col_character(),
  sub_region_2 = col_character(),
  metro_area = col_logical(),
  iso_3166_2_code = col_character(),
  census_fips_code = col_logical(),
  place_id = col_character(),
  date = col_date(format = ""),
  retail_and_recreation_percent_change_from_baseline = col_double(),
  grocery_and_pharmacy_percent_change_from_baseline = col_double(),
  parks_percent_change_from_baseline = col_double(),
  transit_stations_percent_change_from_baseline = col_double(),
  workplaces_percent_change_from_baseline = col_double(),
  residential_percent_change_from_baseline = col_double()
)

# compute the percentage reduction in non-household contacts based on Google
# mobility and Australia-calibrated mobility model
uk_mobility_effect <- bind_rows(
  read_csv(
    "data/2020_GB_Region_Mobility_Report.csv",
    col_types = google_spec
  ),
  read_csv(
    "data/2021_GB_Region_Mobility_Report.csv",
    col_types = google_spec
  )
) %>%
  # get all-UK estimate
  filter(
    is.na(sub_region_1)
  ) %>%
  # convert to long-form with metrics and dates
  select(
    date,
    ends_with("_percent_change_from_baseline")
  ) %>%
  bind_rows(
    tibble(
      date = as.Date("2019-12-31"),
      retail_and_recreation_percent_change_from_baseline = 0,
      grocery_and_pharmacy_percent_change_from_baseline = 0,
      parks_percent_change_from_baseline = 0,
      transit_stations_percent_change_from_baseline = 0,
      workplaces_percent_change_from_baseline = 0,
      residential_percent_change_from_baseline = 0
    )
  ) %>%
  mutate(
    intercept_percent_change_from_baseline = NA
  ) %>%
  pivot_longer(
    cols = ends_with("_percent_change_from_baseline"),
    names_to = "setting",
    values_to = "percent_change"
  ) %>%
  # transform metrics for modelling
  mutate(
    across(
      setting,
      ~str_remove(.x, "_percent_change_from_baseline")
    ),
    covariate = case_when(
      setting == "intercept" ~ 1,
      TRUE ~ log(1 + percent_change / 100)
    )
  ) %>%
  filter(
    setting != "grocery_and_pharmacy"
  ) %>%
  # add on model coefficients
  mutate(
    coefficient = case_when(
      setting == "intercept" ~ 2.440755,
      setting == "parks" ~ 0.007651,
      setting == "residential" ~ -2.253547,
      setting == "retail_and_recreation" ~ 0.684079,
      setting == "transit_stations" ~ 0.024566,
      setting == "workplaces" ~ 0.572682,
    ),
    term = coefficient * covariate
  ) %>%
  group_by(
    date
  ) %>%
  summarise(
    non_household_contacts = exp(sum(term)),
    .groups = "drop"
  ) %>%
  mutate(
    period = case_when(
      date == as.Date("2019-12-31") ~ "baseline",
      date >= as.Date("2021-09-01") & date >= as.Date("2021-09-22") ~ "study",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(period)
  ) %>%
  group_by(
    period
  ) %>%
  summarise(
    non_household_contacts = mean(non_household_contacts)
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = non_household_contacts
  ) %>%
  mutate(
    ratio = study / baseline
  ) %>%
  pull(ratio)

# build England-specific contact matrices

# fit contact model to polymod
model <- fit_setting_contacts(
  get_polymod_setting_data(),
  population = get_polymod_population()
)

# get setting- and age-specific transmission probabilities in these bins
transmission <- read_csv(
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
transmission_matrices <- transmission %>%
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
uk_per_capita_household_size <- tribble(
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
    per_capita_household_size = sum(household_size * fraction)
  ) %>%
  pull(
    per_capita_household_size
  )

england_population <- get_england_population() %>%
  rename(
    lower.age.limit = age
  )

# get setting-specific contact matrices in the right order
setting_contact_matrices <- predict_setting_contacts(
  contact_model = model,
  age_breaks = age_breaks,
  population = england_population,
  per_capita_household_size = uk_per_capita_household_size
)[setting_order]

# get a matrix of the effects of vaccination
vaccine_effect_raw <- outer(
  england_vaccination_effect$infection_multiplier,
  england_vaccination_effect$onward_transmission_multiplier,
  FUN = "*"
)
 
# aggregate population over age breaks
pop_vec <- england_population %>%
  left_join(
    age_lookup,
    by = c(lower.age.limit = "age")
  ) %>%
  group_by(
    age_group
  ) %>%
  summarise(
    across(
      population,
      sum
    ),
    .groups = "drop"
  ) %>%
  pull(
    population
  )

# compute marginals of household transmission parameters and recombine, as a way
# of factoring out household-specific mixing effects (like higher-risk
# inter-couple, parent-child, grandparent-child contact). Want to weight over
# the most commonly observed age pairs (to downwieight areas computed from
# little data), but without the age data, we have toi assume this is
# proportional to the contact rate estimates

home_contacts <- conmat::matrix_to_predictions(setting_contact_matrices$home)
household_transmissions <- conmat::matrix_to_predictions(transmission_matrices$household)
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

# choose baseline transmission probability matrices and ensure they are in the
# correct order
setting_transmission_matrices <- list(
    home = transmission_matrices$household,
    school = transmission_matrices$work_education,
    work = transmission_matrices$work_education,
    other = transmission_matrices$events_activities
  )[setting_order]


# start fitting the model
library(greta.dynamics)

# all parameters
parameters <- list(
  
  # scaling parameters for transmission probabilities
  home_scaling = variable(lower = 0),
  school_scaling = variable(lower = 0),
  work_scaling = variable(lower = 0),
  other_scaling = variable(lower = 0),
  
  # a microdistancing effect to reduce non-household contact rates - reflecting
  # voluntary measures to reduce non-household transmission probabilities
  microdistancing = variable(lower = 0, upper = 1)
  
  # # a correction factor for the effect of vaccination, since VE estimates are
  # # uncertain
  # vaccine_effect_scaling = normal(1, 0.05, truncation = c(0, Inf))
  
)

# scale each of the transmission probability matrices
# home is scaled binimially as contacts saturate
setting_transmission_matrices_scaled <- 
  list(
    home = 1 - (1 - setting_transmission_matrices$home) ^ parameters$home_scaling,
    school = setting_transmission_matrices$school * parameters$school_scaling,
    work = setting_transmission_matrices$work * parameters$work_scaling,
    other = setting_transmission_matrices$other * parameters$other_scaling
  )[setting_order]

# create NGMs in the absence of vaccination, mobility., or microdistancing
setting_ngms <- mapply(
  "*",
  setting_contact_matrices,
  setting_transmission_matrices_scaled,
  SIMPLIFY = FALSE
)

vaccine_effect <- vaccine_effect_raw # * parameters$vaccine_effect_scaling

# and create NGMs with of vaccination, mobility, and microdistancing
setting_ngms_study_period <- list(
  home = setting_ngms$home * vaccine_effect,
  school = setting_ngms$school *
    uk_mobility_effect * parameters$microdistancing * vaccine_effect,
  work = setting_ngms$work *
    uk_mobility_effect * parameters$microdistancing * vaccine_effect,
  other = setting_ngms$other *
    uk_mobility_effect * parameters$microdistancing * vaccine_effect
)[setting_order]

# get next generation matrix, pre-vaccination, by summing these matrices
ngm_pre_pandemic <- Reduce("+", setting_ngms)

# apply vaccination effects and non-household distancing effects
ngm <- Reduce("+", setting_ngms_study_period)

# use better initial age distribution for faster sampling
ngm_unscaled  <- Reduce("+", setting_contact_matrices) * setting_transmission_matrices$home * vaccine_effect_raw
stable_age <- Re(eigen(ngm_unscaled)$vectors[, 1])
initial <- stable_age / sum(stable_age)

# calculate the stable solutions in the face of vaccination
rt_solutions <- greta.dynamics::iterate_matrix(ngm, initial_state = initial)
stable_age_distribution <- rt_solutions$stable_distribution
rt <- rt_solutions$lambda

# do the same for pre-vaccination to get R0 for Delta
r0_solutions <- greta.dynamics::iterate_matrix(
  ngm_pre_pandemic,
  initial_state = initial
)
r0 <- r0_solutions$lambda

# get index to aggregate stable state and match age distribution of infections
ons_index <- get_ons_age_group_lookup() %>%
  filter(
    age <= max(age_breaks[is.finite(age_breaks)])
  ) %>%
  mutate(
    index = as.numeric(age_group),
    n_index = max(index, na.rm = TRUE),
    index = replace_na(index, n_index[1] + 1)
  )

# aggregate to match age groups
stable_age_distribution_grouped_unscaled <- tapply(
  X = stable_age_distribution,
  INDEX = ons_index$index,
  FUN = "sum"
)[seq_len(ons_index$n_index[1])]

stable_age_distribution_grouped <- stable_age_distribution_grouped_unscaled / sum(stable_age_distribution_grouped_unscaled)

# relevel infections to prevent it from overwhelming the likelihood
infections <- round(england_infections$infections * 1000 / sum(england_infections$infections))

counts <- as_data(t(infections))

# define the likelihood for the age distribution of infections
distribution(counts) <- multinomial(
  size = sum(infections),
  prob = t(stable_age_distribution_grouped)
)

# define the likelihood for Rt

# Calibrate against Rt for England during this period - from UK modelling consensus estimate:
# https://www.gov.uk/guidance/the-r-value-and-growth-rate#history
# assuming the interval roughly corresponds to 95% CIs, and using 0.9-1.1 as the mean
# 24 September 0.8 to 1.0
# 17 September 0.9 to 1.1
# 10 September 0.9 to 1.1
# 3 September 0.9 to 1.1

Rt_mean <- 1
Rt_interval <- c(0.9, 1.1)
Rt_sd <- mean(abs(Rt_mean - Rt_interval)) / qnorm(0.995, 0, 1)
distribution(Rt_mean) <- normal(rt, sd = Rt_sd, truncation = c(0, Inf))

# use a fairly wide estimate for Delta R0 (95% interval 7.5-8.5)
R0_mean <- 8
R0_interval <- c(7.5, 8.5)
R0_sd <- mean(abs(R0_mean - R0_interval)) / qnorm(0.995, 0, 1)
distribution(R0_mean) <- normal(r0, sd = R0_sd, truncation = c(0, Inf))

# compute the hSAR and define a likelihood for that
hSAR <- mean_attack_rate(
  transmission_probability_matrix = setting_transmission_matrices_scaled$home,
  contact_matrix = setting_contact_matrices$home,
  population = pop_vec
)

hSAR_interval <- c(0.2, 0.35)
hSAR_mean <- mean(hSAR_interval)
hSAR_sd <- mean(abs(hSAR_mean - hSAR_interval)) / qnorm(0.975, 0, 1)
distribution(hSAR_mean) <- normal(hSAR, hSAR_sd, truncation = c(0, 1))

# add a likelihood term for the recent SAR in secondary schools UK estimate
# gives 2%, as per https://doi.org/10.1016/s0140-6736(21)01908-5 so use lower,
# since 2% is likely an overestimate for our contact definition of 9-10 contacts
# rather than 2-3
ss_index <- ons_index$age %in% 11:16
ssSAR <- mean(setting_transmission_matrices_scaled$school[ss_index, ss_index])
ssSAR_mean <- 0.01
ssSAR_interval <- c(0, 0.02)
ssSAR_sd <- mean(abs(ssSAR_mean - ssSAR_interval)) / qnorm(0.975, 0, 1)
# qnorm(0.975, ssSAR_mean, ssSAR_sd)
distribution(ssSAR_mean) <- normal(ssSAR, ssSAR_sd, truncation = c(0, 1))

# fit the model by HMC
attach(parameters)
m <- model(
  home_scaling,
  school_scaling,
  work_scaling,
  other_scaling,
  microdistancing
  # vaccine_effect_scaling
)
draws <- mcmc(
  m,
  chains = 10,
  warmup = 1000
)

# check sampling
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
bayesplot::mcmc_trace(draws)

# get posterior means of parameters
estimates <- do.call(
  calculate,
  c(parameters,
    list(
      values = draws,
      nsim = 4e3
    )
  )
) %>%
  lapply(mean)

# and compute stable age distribution based on the posterior mean
stable_age_post_grouped <- calculate(
  stable_age_distribution_grouped,
  values = estimates
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

# check the reproduction number estimate matches England estimates
plot(
  calculate(
    rt,
    r0,
    hSAR,
    ssSAR,
    values = draws
  )
)

estimated_calibration_stats <- calculate(
  rt,
  r0,
  hSAR,
  ssSAR,
  values = estimates
) %>%
  lapply(c)

sSAR <- mean_attack_rate(
  transmission_probability_matrix = setting_transmission_matrices_scaled$school,
  contact_matrix = setting_contact_matrices$school,
  population = pop_vec
)
wSAR <- mean_attack_rate(
  transmission_probability_matrix = setting_transmission_matrices_scaled$work,
  contact_matrix = setting_contact_matrices$work,
  population = pop_vec
)
oSAR <- mean_attack_rate(
  transmission_probability_matrix = setting_transmission_matrices_scaled$other,
  contact_matrix = setting_contact_matrices$other,
  population = pop_vec
)

# compute attack rates in each setting for a pair of 40 year olds
estimated_attack_rates <- calculate(
  hSAR,
  sSAR,
  wSAR,
  oSAR,
  values = estimates
) %>%
  lapply(c)

estimated_calibration_stats
estimated_attack_rates


do.call(c, estimates[1:4])
# output scalings and change conmat to use them

# compare with infection age distribution in UK pre-Delta and pre-vaccination (pre-lockdown?)



