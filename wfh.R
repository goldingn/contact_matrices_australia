# wfh model implementation

library(greta)


# macro_model <- readRDS("/home/unimelb.edu.au/ryange/covid_19_australia_interventions/outputs/fitted_macro_model.RDS")
# 
# contacts <- tibble(
#   state = macro_model$data$contacts$state,
#   date = macro_model$data$contacts$date,
#   contact_number = macro_model$data$contacts$contact_num
# )
# 
# saveRDS(
#   object = contacts,
#   file = "data/contacts.RDS"
# )

## prepare data

# contacts from survey data
contacts <- readRDS(file = "data/contacts.RDS")

# wfh by SA2
wfh_index <- read_csv(
  file = "data/WorkFromHomeability_by_SA216_LGA19_reduced.csv"
)

# convert SA2 to state level by weighting for population
sa2_population <- read_csv(
  file = "data/ABS_ANNUAL_ERP_ASGS2016_20102021122013645.csv"
) %>%
  filter(
    REGIONTYPE == "SA2",
    TIME == "2020"
  ) %>%
  mutate(
    ASGS_2016 = as.numeric(ASGS_2016)
  )

wfh_state <- full_join(
    wfh_index,
    sa2_population,
    by = c("SA2_MAIN16" = "ASGS_2016")
  ) %>%
  group_by(STE_NAME16) %>%
  mutate(
    weight = Value / sum(Value),
    weighted_wfh = SA2_WFH * weight
  ) %>% 
  summarise(
    wfh = sum(weighted_wfh)
  ) %>%
  rename(
    state_names = STE_NAME16
  ) %>%
  filter(
    state_names != "Other Territories"
  ) %>%
  mutate(
    state = case_when(
      state_names %in% c("Australian Capital Territory", "ACT") ~ "ACT",
      state_names %in% c("New South Wales", "NSW") ~ "NSW",
      state_names %in% c("Northern Territory", "NT") ~ "NT",
      state_names %in% c("Queensland", "QLD") ~ "QLD",
      state_names %in% c("South Australia", "SA") ~ "SA",
      state_names %in% c("Tasmania", "TAS") ~ "TAS",
      state_names %in% c("Victoria", "VIC") ~ "VIC",
      state_names %in% c("Western Australia", "WA") ~ "WA"
    )
  ) %>%
  select(state, wfh)

# compile data
data <- contacts %>%
  left_join(
    lockdown_dates(),
    by = c("date", "state")
  ) %>%
  filter(state == "VIC" | state == "NSW") %>%
  mutate(
    state = as.factor(state),
    state_index = as.numeric(state)
  ) %>%
  left_join(
    wfh_state,
    by = "state"
  )


# model

alpha <- normal(0, 10)
gamma_i <- normal(0, 10, dim = c(2,1))
beta_t <- normal(0, 10)


OC_ti <- data$contact_number
L_ti <- data$lockdown
W_i <- data$wfh
state_index <- data$state_index

lambda_ti <- exp(alpha + gamma_i[state_index] + L_ti * W_i * beta_t)

distribution(OC_ti) <- poisson(lambda_ti)

mod <- model(alpha, gamma_i, beta_t)

# plot(mod)

draws <- mcmc(mod, n_samples = 2000, chains = 10)

mcmc_trace(draws)

summary(draws)
