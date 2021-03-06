# calibrate setting weights against case age distribution data from england over
# time, with Delta, post-reopening and pre-vaccination of 12-15 year olds


# to do:

# add Rt and R0 estimates back in as likelihoods - DONE

# roll forward to most recent UK infection age distribution data (includes spike
# in 12-15s) - DONE

# still learn household/non-household weights - DONE

# use smoothed Davies estimates for susceptibility and clinical fraction - DONE

# try learning the scaling on asymptomatic onwards transmission

# ditch Eyre, and infer susceptibility profile as a GP with Davies estimate as
# a prior mean


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

# Use coverage data from immediately before comencement of 12-15 vacciantion
# program. This is a pragmatic decision because UK does not yet report 12-15
# vaccination coverage separately from 16-17 year olds in a machine-readable
# format. Unlike 12-15 year olds, 16-17 year olds have been eligible for a long
# period and have likely considerably higher coverage. Whilst this incorrectly
# assumes no vaccine protection of 12-15 year olds. However coverage of this age
# group as at October 15 was low (https://www.bbc.com/news/health-55274833),
# first dose only, and given the lag to immunity will likely have minimal effect
# on transmission. This would also only bias estimates of susceptibility of
# 12-15s, relative to e.g. under-12s, *down*, rather than up.
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
  ) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = str_sort(
        unique(age_group),
        numeric = TRUE
      )
    )
  ) %>%
  arrange(
    age_group
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


mobility_ratios <- bind_rows(
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
    ratio = 1 + percent_change / 100
  ) %>%
  filter(
    date >= as.Date("2021-09-01"),
    date >= as.Date("2021-09-22"),
  ) %>%
  mutate(
    setting = case_when(
      setting == "residential" ~ "home",
      setting %in% c("retail_and_recreation", "grocery_and_pharmacy") ~ "other",
      setting == "workplaces" ~ "work",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(
    setting
  ) %>%
  summarise(
    across(
      ratio,
      mean
    )
  ) %>%
  filter(
    !is.na(setting)
  ) %>%
  pivot_wider(
    names_from = setting,
    values_from = ratio
  )

mobility_ratios

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


# pull out Davies et al transmission parameters
davies_estimates <- read_csv(
  "data/susceptibility_clinical_fraction_age_Davies.csv",
  col_types = cols(
    age_group = col_character(),
    rel_susceptibility_mean = col_double(),
    rel_susceptibility_median = col_double(),
    clinical_fraction_mean = col_double(),
    clinical_fraction_median = col_double()
  )
)

age_effect_smooths_davies <- davies_estimates %>%
  mutate(
    age_lower = readr::parse_number(age_group),
    age_upper = age_lower + 9,
  ) %>%
  group_by(age_group) %>%
  summarise(
    age = age_lower:age_upper,
    across(
      ends_with("mean"),
      first
    ),
    .groups = "drop"
  ) %>%
  summarise(
    across(
      ends_with("mean"),
      ~list(smooth.spline(age, .x, df = 10))
    )
  ) %>%
  as.list() %>%
  lapply(pluck, 1)

# convert to integer ages
davies_trends <- age_lookup %>%
  mutate(
    age_extrapolate = pmin(age, 80),
    clinical_fraction = predict(age_effect_smooths_davies$clinical_fraction_mean, age_extrapolate)$y,
    rel_susceptibility = predict(age_effect_smooths_davies$rel_susceptibility_mean, age_extrapolate)$y
  ) %>%
  group_by(
    age_group
  ) %>%
  summarise(
    across(
      c(clinical_fraction, rel_susceptibility),
      mean
    )
  ) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = str_sort(
        unique(age_group),
        numeric = TRUE
      )
    )
  ) %>%
  arrange(
    age_group
  )

davies_trends %>%
  pivot_longer(
    cols = -age_group,
    names_to = "parameter",
    values_to = "value"
  ) %>%
  ggplot(
    aes(
      x = age_group,
      y = value,
      fill = parameter
    )
  ) +
  geom_col(
    position = "dodge"
  ) +
  theme_minimal()

# start fitting the model
library(greta.dynamics)
library(greta.gp)

# all parameters
parameters <- list(
  
  # scaling parameters for transmission probabilities
  home_scaling = variable(lower = 0),
  non_household_scaling = variable(lower = 0),
  
  # a microdistancing effect to reduce non-household contact rates - reflecting
  # voluntary measures to reduce non-household transmission probabilities
  microdistancing = variable(lower = 0, upper = 1),
  
  # a correction factor for the effect of vaccination, since VE estimates are
  # uncertain and UK has lots of prior immunity
  vaccine_effect_scaling = normal(1, 0.1, truncation = c(0, 1))
  
)

# re-estimate susceptibility for younger ages in a manner similar to Davies.
# Note that due to high levels (and uncertain effects) of vaccination on the
# older age groups, re-estimation of the whole curve is not possible from these
# data, but they will provide details information on the younger age groups

prior_susceptibility <- davies_trends$rel_susceptibility
cutoff_age <- 16

# minimum age in the UK data
min_age <- 2

# get the integer ages for age groups
age_lower <- age_lookup %>%
  group_by(age_group) %>%
  summarise(
    age_lower = min(age)
  ) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = str_sort(
        unique(age_group),
        numeric = TRUE
      )
    )
  ) %>%
  arrange(age_group) %>%
  pull(age_lower)

age_weight <- tibble(
  age = age_lower
) %>%
  mutate(
    weight = pmin(cutoff_age, age),
    weight = weight - min(weight),
    weight = weight / max(weight),
    weight = 1 - weight
  ) %>%
  pull(weight)

# make susceptibility a monotone increasing function for the younger ages,
# matching the Davies estimates at the cutoff value

# define a GP for the vector of younger ages, and transform it to monotone
# decreasing starting at 1, the transform it to monotone decreasing converging
# on the cutoff
ages_to_infer <- age_lower < cutoff_age & age_lower >= min_age

n <- sum(ages_to_infer)

x <- seq(0, 1, length.out = n)
variance <- normal(0, 0.5, truncation = c(0, Inf))
lengthscale <- lognormal(-2, 1)
kernel <- rbf(lengthscales = lengthscale, variance = variance)
gp_logit_diff <- gp(x = x, kernel = kernel)


gp_logit_diff_centred <- gp_logit_diff + rev(qlogis(prior_susceptibility[1:n]))

# get the susceptibility at the cutoff
susceptibility_young_max <- prior_susceptibility[age_lookup$age == cutoff_age]

# get convert the GP to integer year differences in susceptibility relative ot
# the Davies cutoff, in reverse
gp_diffs_rev <- c(
  zeros(length(prior_susceptibility) - n - min_age),
  ilogit(gp_logit_diff_centred)
)

terminal <- uniform(0, 1)

# reverse and scale to increase to 1 at the cutoff, then stay at 1 thereafter
# gp_increasing <- 1 - rev(cumsum(gp_diffs_rev)) / n
gp_increasing <- 1 - terminal * rev(cumsum(gp_diffs_rev)) / sum(gp_diffs_rev)

# extend to repeat the final value for ages below the minimum age
gp_increasing_extended <- c(rep(gp_increasing[1], min_age), gp_increasing)


# multiply by a clamped version of the Davies susceptibility estimates, set to a
# constant value below the cutoff, to obtain a function increasing
# monotonically to converge at the cutoff
prior_susceptibility_clamped <- prior_susceptibility
prior_susceptibility_clamped[age_lower < cutoff_age] <- susceptibility_young_max
susceptibility <- gp_increasing_extended * prior_susceptibility_clamped

susceptibility_prior_sims <- calculate(susceptibility, nsim = 30)[[1]][, , 1]
par(mfrow = c(1, 1))
plot(
  susceptibility_prior_sims[1, ] ~ age_lower,
  type = "n",
  ylim = c(0, 1)
  # ylim = range(susceptibility_prior_sims)
)
for (i in 1:30) {
  lines(
    susceptibility_prior_sims[i, ] ~ age_lower,
    col = grey(0.4)
  )
}
lines(prior_susceptibility ~ age_lower, lwd = 3)

asymptomatic_relative_infectiousness <- 0.5

relative_infectiousness <- davies_trends$clinical_fraction + (1 - davies_trends$clinical_fraction) * asymptomatic_relative_infectiousness

relative_transmission_matrix <- kronecker(
  susceptibility,
  as_data(t(relative_infectiousness)),
  FUN = "*"
)

# choose baseline transmission probability matrices and ensure they are in the
# correct order
setting_transmission_matrices <- list(
  home = relative_transmission_matrix,
  school = relative_transmission_matrix,
  work = relative_transmission_matrix,
  other = relative_transmission_matrix
)[setting_order]

# scale each of the transmission probability matrices
# home is scaled binimially as contacts saturate
setting_transmission_matrices_scaled <- 
  list(
    # scaling home transmission by correction factor and by increased duration at home
    home = 1 - (1 - setting_transmission_matrices$home) ^ parameters$home_scaling,
    school = setting_transmission_matrices$school * parameters$non_household_scaling,
    work = setting_transmission_matrices$work * parameters$non_household_scaling,
    other = setting_transmission_matrices$other * parameters$non_household_scaling
  )[setting_order]

# create NGMs in the absence of vaccination, mobility., or microdistancing
setting_ngms <- mapply(
  "*",
  setting_contact_matrices,
  setting_transmission_matrices_scaled,
  SIMPLIFY = FALSE
)

vaccine_effect <- vaccine_effect_raw * parameters$vaccine_effect_scaling

# and create NGMs with of vaccination, mobility, and microdistancing
setting_ngms_study_period <- list(
  home = setting_ngms$home * mobility_ratios$home * vaccine_effect,
  school = setting_ngms$school * vaccine_effect,
  work = setting_ngms$work * mobility_ratios$work *
    parameters$microdistancing * vaccine_effect,
  other = setting_ngms$other * mobility_ratios$other *
    parameters$microdistancing * vaccine_effect
)[setting_order]

# get next generation matrix, pre-vaccination, by summing these matrices
ngm_pre_pandemic <- Reduce("+", setting_ngms)

# apply vaccination effects and non-household distancing effects
ngm <- Reduce("+", setting_ngms_study_period)

# use reasonable initial values for faster sampling
initial_state <- c(0.005, 0.006, 0.007, 0.009, 0.011, 0.015, 0.019, 0.025, 0.032, 
                   0.041, 0.05, 0.06, 0.066, 0.07, 0.069, 0.059, 0.023, 0.017, 0.011, 
                   0.01, 0.009, 0.009, 0.009, 0.009, 0.009, 0.009, 0.009, 0.01, 
                   0.01, 0.011, 0.01, 0.011, 0.012, 0.012, 0.013, 0.013, 0.013, 
                   0.013, 0.014, 0.014, 0.012, 0.011, 0.011, 0.011, 0.01, 0.009, 
                   0.009, 0.008, 0.008, 0.007, 0.006, 0.005, 0.005, 0.005, 0.004, 
                   0.004, 0.004, 0.003, 0.003, 0.003, 0.003, 0.003, 0.002, 0.002, 
                   0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.001, 
                   0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.004)

# calculate the stable solutions in the face of vaccination
rt_solutions <- greta.dynamics::iterate_matrix(
  ngm,
  initial_state = initial_state
)
stable_age_distribution <- rt_solutions$stable_distribution
rt <- rt_solutions$lambda

# do the same for pre-vaccination to get R0 for Delta
r0_solutions <- greta.dynamics::iterate_matrix(
  ngm_pre_pandemic,
  initial_state = initial_state
)
r0 <- r0_solutions$lambda

# define the likelihood for Rt

# Calibrate against Rt for England during this period - from UK modelling consensus estimate:
# https://www.gov.uk/guidance/the-r-value-and-growth-rate#history
# assuming the interval roughly corresponds to 95% CIs, and using 0.9-1.1 as the mean
# 15 October 0.9 to 1.1
# 8 October 0.9 to 1.1
# 1 October 0.8 to 1.1
# 24 September 0.8 to 1.0
# 17 September 0.9 to 1.1
# 10 September 0.9 to 1.1
# 3 September 0.9 to 1.1
Rt_mean <- 1
Rt_interval <- c(0.9, 1.1)
Rt_sd <- mean(abs(Rt_mean - Rt_interval)) / qnorm(0.995, 0, 1)
distribution(Rt_mean) <- normal(rt, sd = Rt_sd, truncation = c(0, Inf))

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

# relevel infections to prevent it from overwhelming the likelihood (not really as many datapoints as the whole population)
infections <- round(england_infections$infections * 1000 / sum(england_infections$infections))

counts <- as_data(t(infections))

# define the likelihood for the age distribution of infections
distribution(counts) <- multinomial(
  size = sum(infections),
  prob = t(stable_age_distribution_grouped)
)

# compute the hSAR and define a likelihood for that
hSAR <- mean_attack_rate(
  transmission_probability_matrix = setting_transmission_matrices_scaled$home,
  contact_matrix = setting_contact_matrices$home,
  population = pop_vec
)

# based on parameters doc reasonable range for Delta hSAR (corresponds to final
# attack rates above 70%)
hSAR_interval <- c(0.2, 0.35)
hSAR_mean <- mean(hSAR_interval)
hSAR_sd <- mean(abs(hSAR_mean - hSAR_interval)) / qnorm(0.975, 0, 1)
distribution(hSAR_mean) <- normal(hSAR, hSAR_sd, truncation = c(0, 1))

# add likelihoods for reasonable SARs in school, work and other (assuming lower
# than home)
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

SAR_interval <- c(0, 0.1)
SAR_mean <- mean(SAR_interval)
SAR_sd <- mean(abs(SAR_mean - SAR_interval)) / qnorm(0.975, 0, 1)

# distribution(SAR_mean) <- normal(sSAR, SAR_sd, truncation = c(0, 1))
# distribution(SAR_mean) <- normal(wSAR, SAR_sd, truncation = c(0, 1))
# distribution(SAR_mean) <- normal(oSAR, SAR_sd, truncation = c(0, 1))

# fit the model by HMC
attach(parameters)
m <- model(
  home_scaling,
  non_household_scaling,
  microdistancing,
  vaccine_effect_scaling
)

draws <- mcmc(
  m,
  chains = 10,
  warmup = 500,
  n_samples = 300,
  one_by_one = TRUE
)

# check sampling
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
coda::effectiveSize(draws)
bayesplot::mcmc_trace(draws)

# summarise susceptibility trend posterior
susceptibility_posterior_sims <- calculate(susceptibility, values = draws, nsim = 3000)[[1]][, , 1]

susceptibility_posterior_mean <- colMeans(susceptibility_posterior_sims)
susceptibility_posterior_intervals <- apply(susceptibility_posterior_sims,
                                           2,
                                           quantile,
                                           c(0.025, 0.975))

davies_step_data <- davies_estimates %>%
  mutate(
    age_lower = readr::parse_number(age_group),
    age_upper = age_lower + 10
  ) %>%
  rowwise() %>%
  summarise(
    age = age_lower:age_upper,
    susceptibility = first(rel_susceptibility_mean)
  )

png("outputs/rel_susceptibility.png",
    width = 1200,
    height = 1000,
    pointsize = 30)

par(
  mfrow = c(1, 1),
  mar = c(5, 4, 4, 2) + 0.1,
  las = 1
)
# plot the susceptibility curve
plot(
  x = age_lower,
  y = susceptibility_posterior_mean,
  type = "n",
  lwd = 2,
  ylim = c(0, 1),
  xlab = "age",
  ylab = "relative susceptibility",
  main = "Updated estimates of relative susceptibility by age\nNew estimate (black line: mean, grey region: 95% CI);\nOriginal estimate (dotted/dashed lines)"
)

lines(
  susceptibility ~ age,
  data = davies_step_data,
  lty = 3
)

polygon(
  x = c(age_lower,
        rev(age_lower)),
  y = c(susceptibility_posterior_intervals[1, ],
        rev(susceptibility_posterior_intervals[2, ])),
  col = grey(0.8),
  lty = 0
)
lines(
  susceptibility_posterior_intervals[1, ] ~ age_lower,
  col = grey(0.4)
)
lines(
  susceptibility_posterior_intervals[2, ] ~ age_lower,
  col = grey(0.4)
)
lines(
  prior_susceptibility ~ age_lower,
  lty = 2
)
lines(
  susceptibility_posterior_mean ~ age_lower,
  lwd = 2,
  lend = 2
)

dev.off()


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
  lapply(mean) %>%
  c(
    list(
      susceptibility = susceptibility_posterior_mean
    )    
  )


# and compute stable age distribution based on the posterior means
stable_age_post_grouped <- calculate(
  stable_age_distribution_grouped,
  values = estimates
)[[1]][, 1]

# and based on posterior means for other parameters, but Davies susceptibility
# assumptions
step_susceptibility <- davies_step_data %>%
  group_by(
    age
  ) %>%
  summarise(
    across(
      susceptibility,
      last
    )
  ) %>%
  pull(susceptibility)

estimates_davies <- estimates
estimates_davies$susceptibility <- step_susceptibility
stable_age_davies_grouped <- calculate(
  stable_age_distribution_grouped,
  values = estimates_davies
)[[1]][, 1]




png(
  "outputs/age_distribution.png",
  width = 1000,
  height = 1300,
  pointsize = 30
)
# compare outputs
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
barplot(stable_age_davies_grouped,
        main = "Original susceptibility estimate",
        xlab = "case ages",
        ylab = "fraction of infections",
        names.arg = england_infections$age_group)
barplot(stable_age_post_grouped,
        main = "New susceptibility estimate",
        xlab = "case ages",
        ylab = "fraction of infections",
        names.arg = england_infections$age_group)
barplot(england_infections$infections / 1000,
        main = "Infections observed in England",
        xlab = "case ages",
        ylab = "'000s of infections",
        names.arg = england_infections$age_group)

dev.off()

# check the other parameters are vaguely reasonable
plot(
  calculate(
    r0,
    rt,
    hSAR,
    sSAR,
    wSAR,
    oSAR,
    values = draws
  )
)

estimated_calibration_stats <- calculate(
  r0,
  rt,
  hSAR,
  sSAR,
  wSAR,
  oSAR,
  values = estimates
) %>%
  lapply(c)

estimated_calibration_stats


dput(
  c(
    home = estimates$home_scaling,
    school = estimates$non_household_scaling,
    work = estimates$non_household_scaling,
    other = estimates$non_household_scaling
  )
)


# combine the clinical fraction and age estimates
davies_smoothed_extended <- davies_trends %>%
  bind_cols(
    davies_updated = susceptibility_posterior_mean
  ) %>%
  rename(
    davies_original = rel_susceptibility
  ) %>%
  mutate(
    age_lower = readr::parse_number(as.character(age_group)),
    age_upper = case_when(
      age_lower == 80 ~ 100,
      TRUE ~ age_lower
    )
  ) %>%
  rowwise() %>%
  summarise(
    age = age_lower:age_upper,
    across(
      c(clinical_fraction,
        davies_original,
        davies_updated),
      first
    )
  )

datapasta::dpasta(davies_smoothed_extended)




