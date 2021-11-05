# wfh model implementation

#library(greta)


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

# # wfh by SA2
# wfh_index <- read_csv(
#   file = "data/WorkFromHomeability_by_SA216_LGA19_reduced.csv"
# )
# 
# # convert SA2 to state level by weighting for population
# sa2_population <- read_csv(
#   file = "data/ABS_ANNUAL_ERP_ASGS2016_20102021122013645.csv"
# ) %>%
#   filter(
#     REGIONTYPE == "SA2",
#     TIME == "2020"
#   ) %>%
#   mutate(
#     ASGS_2016 = as.numeric(ASGS_2016)
#   )

postcode_lga <- read_excel(
  path = "data/CA_POSTCODE_2018_LGA_2018.xlsx",
  sheet = "Table 3",
  skip = 5
) %>%
  select(
    postcode = POSTCODE_2018...1,
    lga = LGA_NAME_2018,
    weight = RATIO
  ) %>%
  filter(
    !is.na(postcode),
    postcode %in% contacts$postcode
  )


contacts_lga <- inner_join(
  contacts,
  postcode_lga,
  by = "postcode"
)

wfh_lga_summary <- readRDS("outputs/wfh_lga_summary.RDS") %>%
  rename(lga = LGA_NAME_2018)

# wfh_state <- full_join(
#     wfh_index,
#     sa2_population,
#     by = c("SA2_MAIN16" = "ASGS_2016")
#   ) %>%
#   group_by(STE_NAME16) %>%
#   mutate(
#     weight = Value / sum(Value),
#     weighted_wfh = SA2_WFH * weight
#   ) %>% 
#   summarise(
#     wfh = sum(weighted_wfh)
#   ) %>%
#   rename(
#     state_names = STE_NAME16
#   ) %>%
#   filter(
#     state_names != "Other Territories"
#   ) %>%
#   mutate(
#     state = case_when(
#       state_names %in% c("Australian Capital Territory", "ACT") ~ "ACT",
#       state_names %in% c("New South Wales", "NSW") ~ "NSW",
#       state_names %in% c("Northern Territory", "NT") ~ "NT",
#       state_names %in% c("Queensland", "QLD") ~ "QLD",
#       state_names %in% c("South Australia", "SA") ~ "SA",
#       state_names %in% c("Tasmania", "TAS") ~ "TAS",
#       state_names %in% c("Victoria", "VIC") ~ "VIC",
#       state_names %in% c("Western Australia", "WA") ~ "WA"
#     )
#   ) %>%
#   select(state, wfh)

# compile data
# data <- contacts %>%
#   left_join(
#     lockdown_dates(),
#     by = c("date", "state")
#   ) %>%
#   filter(state == "VIC" | state == "NSW") %>%
#   mutate(
#     state = as.factor(state),
#     state_index = as.numeric(state)
#   ) %>%
#   left_join(
#     wfh_state,
#     by = "state"
#   )


# model

# #alpha <- normal(0, 10)
# gamma_i <- normal(0, 10, dim = c(2,1))
# beta_t <- normal(0, 10)
# 
# 
# OC_ti <- data$contact_number
# L_ti <- data$lockdown
# W_i <- data$wfh
# state_index <- data$state_index
# state <- data$state
# 
# lambda_ti <- exp(gamma_i[state_index] + L_ti * W_i * beta_t)
# 
# distribution(OC_ti) <- poisson(lambda_ti)
# 
# mod <- model(gamma_i, beta_t)
# 
# # plot(mod)
# 
# draws <- mcmc(mod, n_samples = 1000, chains = 10)
# 
# library("bayesplot")
# mcmc_trace(draws)
# 
# summary(draws)
# 
# glm(
#  formula = contact_number ~ state + I(lockdown*wfh),
#  data = data,
#  family = stats::poisson(link = "log")
# )

data <- contacts_lga %>%
  left_join(
    lockdown_dates(),
    by = c("date", "state")
  ) %>%
  filter(state == "VIC" | state == "NSW") %>%
  mutate(
    state = as.factor(state)
  ) %>%
  left_join(
    wfh_lga_summary %>%
      select(lga, wfh = wfh_mean),
    by = "lga"
  ) %>%
  group_by(lga) %>%
  mutate(ndata = n()) %>%
  filter(ndata >= 20) %>%
  ungroup %>%
  select(-ndata) %>%
  mutate(contact_num = ifelse(contact_num > 100, 100, contact_num))

data %>%
  filter(!is.na(contact_num)) %>%
  group_by(lockdown, state) %>%
  mutate(contact_num = ifelse(contact_num > 100, 100, contact_num)) %>%
  summarise(mean = mean(contact_num))


# wfh_glm <- glm(
#   formula = contact_num ~ I(lockdown*wfh) + lga,
#   data = data,
#   weights = weight,
#   family = stats::poisson(link = "log")
# )

library(lme4)
wfh_glmr <- glmer(
  formula = contact_num ~ I(lockdown*wfh) + (1|lga),
  data = data,
  weights = weight,
  family = stats::poisson(link = "log")
)

contact_effect_estimates <- fixef(wfh_glmr)

non_hh_contacts_mean <- exp(contact_effect_estimates[1])

wfh_effect <- contact_effect_estimates[2]


wfh_lockdown_effect <- wfh_lga_summary %>%
  mutate(
    lockdown_non_hh_contacts = non_hh_contacts_mean * exp(wfh_mean * wfh_effect),
    tp_multiplier = lockdown_non_hh_contacts/non_hh_contacts_mean
  )

saveRDS(
  wfh_lockdown_effect,
  "outputs/wfh_lockdown_effect.RDS"
)


