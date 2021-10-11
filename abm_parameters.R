# output various matrices and things for Eamon and Nic


library(conmat)
library(tidyverse)


# define age breaks for prediction
age_breaks <- c(seq(0, 80, by = 5), Inf)


# fit polymod model
setting_models <- fit_setting_contacts(
  contact_data_list = get_polymod_setting_data(),
  population = get_polymod_population()
)

australia_pop <- abs_pop_age_lga_2020 %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  mutate(
    country = "Australia"
  )

# get Australia-wide setting-specific contact matrices
# with adjustment to the household size
australia_setting_contact_matrices_5y <- predict_setting_contacts(
  contact_model = setting_models,
  population = australia_pop,
  per_capita_household_size = get_per_capita_household_size(),
  age_breaks = age_breaks
)

# get the setting-specific contact matrices
setting_transmission_matrices_5y <- get_setting_transmission_matrices(
  age_breaks = age_breaks
)

# reweight the setting transmission matrices to be relative to the maximum value
# max_transmission_prob <- max(vapply(setting_transmission_matrices_5y, max, FUN.VALUE = numeric(1)))
# relative_setting_transmission_matrices_5y <- lapply(setting_transmission_matrices_5y, "/", max_transmission_prob)

relative_setting_transmission_matrices_5y <- setting_transmission_matrices_5y

settings <- names(relative_setting_transmission_matrices_5y)

# setting specific unscaled NGMs
australia_setting_ngms_5y <- mapply(
  "*",
  australia_setting_contact_matrices_5y[settings],
  relative_setting_transmission_matrices_5y[settings],
  SIMPLIFY = FALSE
)

# overall NGM
australia_ngm_5y <- Reduce("+", australia_setting_ngms_5y)

# overall contacts
australia_contact_matrix_5y <- Reduce("+", australia_setting_contact_matrices_5y[settings])

# get ratio of these to get all-setting transmission probabilities
australia_transmission_matrix_5y <- australia_ngm_5y / australia_contact_matrix_5y



# make the marginals relative to the max, make the overall transmission effect
# relative to the max, and the divide one by the other to get the multiplicative
# remainder (to multiply onto the effective contact matrix)




# compute (contact-weighted) marginals of the average transmission probabilities
# by integer age.

# compute average transmission surfaces, based on the overall mean number of contacts in each setting
# age-specific contact-weighted means break up the modelled structure of the surface?
setting_mean_contacts <- vapply(australia_setting_contact_matrices_5y[settings],
                                mean,
                                FUN.VALUE = numeric(1)) 
setting_contact_weights <- setting_mean_contacts / sum(setting_mean_contacts)
average_transmission_matrix_5y <- Reduce("+",
       mapply(
         "*",
         relative_setting_transmission_matrices_5y,
         as.list(setting_contact_weights),
         SIMPLIFY = FALSE
       )
)

contacts <- conmat::matrix_to_predictions(australia_contact_matrix_5y)
transmissions <- conmat::matrix_to_predictions(average_transmission_matrix_5y)

contact_transmissions <- contacts %>%
  mutate(
    probability = transmissions$contacts
  )

relative_susceptibility <- contact_transmissions %>%
  group_by(age_group_to) %>%
  summarise(
    susceptibility = weighted.mean(probability, contacts),
    .groups = "drop"
  ) %>%
  mutate(
    relative_susceptibility = susceptibility / max(susceptibility)
  ) %>%
  select(
    age_group = age_group_to,
    relative_susceptibility
  )

relative_infectiousness <- contact_transmissions %>%
  group_by(age_group_from) %>%
  summarise(
    infectiousness = weighted.mean(probability, contacts),
    .groups = "drop"
  ) %>%
  mutate(
    relative_infectiousness = infectiousness / max(infectiousness)
  ) %>%
  select(
    age_group = age_group_from,
    relative_infectiousness
  )

relative_age_contribution <- relative_infectiousness %>%
  left_join(
    relative_susceptibility,
    by = "age_group"
  )

# relative_age_contribution %>%
#   pivot_longer(
#     cols = starts_with("relative"),
#     names_to = "parameter",
#     values_to = "ratio"
#   ) %>%
#   ggplot(
#     aes(
#       x = age_group,
#       y = ratio,
#       color = parameter
#     )
#   ) +
#   geom_point() +
#   theme_minimal()

# matrix of just the marginal effects
relative_transmission_matrix <- outer(
  relative_age_contribution$relative_infectiousness,
  relative_age_contribution$relative_susceptibility,
  FUN = "*"
) %>%
  `rownames<-`(relative_age_contribution$age_group) %>% 
  `colnames<-`(relative_age_contribution$age_group)

# divide out the marginal effects from the overall, to get the interactions
# australia_transmission_matrix_5y = relative_transmission_matrix * interaction_transmission_matrix
interaction_transmission_matrix <- australia_transmission_matrix_5y / relative_transmission_matrix

plot_matrix(interaction_transmission_matrix)

# check this
max(
  abs(
    australia_transmission_matrix_5y - relative_transmission_matrix * interaction_transmission_matrix
  )
) < 1e-6

# effective contacts for Eamon - incorporating age-interaction effects from the transmission probabilities that are not included in the marginals
australia_effective_contact_matrix_5y <- australia_contact_matrix_5y * interaction_transmission_matrix

# adjust so that the mean is the same as for the basic contacts one (the
# probability gets rescaled relative to this, but the number of contacts might
# affect the degree of dispersion?)
ratio <- mean(australia_contact_matrix_5y) / mean(australia_effective_contact_matrix_5y)
australia_effective_contact_matrix_5y <- australia_effective_contact_matrix_5y * ratio

# save these things for Eamon
australia_effective_contact_matrix_5y %>%
  as_tibble() %>%
  mutate(
    age_group_from = rownames(australia_effective_contact_matrix_5y),
    .before = everything()
  ) %>%
  write_csv(
    "outputs/effective_contact_matrix_for_eamon.csv"
  )

relative_age_contribution %>%
  write_csv(
    "outputs/relative_age_contribution_for_eamon.csv"
  )

plot_matrix(australia_effective_contact_matrix_5y)
plot_matrix(australia_contact_matrix_5y)

mean(australia_contact_matrix_5y)
mean(australia_effective_contact_matrix_5y)

# plot_matrix(australia_effective_contact_matrix_5y)
# plot_matrix(australia_contact_matrix_5y)

# get Eamon an Australia-specific contact matrix, and vectors of age-specific
# susceptibility and onward transmissability, adding the residual transmission
# probabbility interaction to the contact matrix


# output household/ average non-household transmission probabilities for Bec
nonhousehold_settings <- settings[-1]
australia_nonhousehold_ngm_5y <- Reduce("+", australia_setting_ngms_5y[nonhousehold_settings])
australia_nonhousehold_contacts_5y <- Reduce("+", australia_setting_contact_matrices_5y[nonhousehold_settings])
transmission_household_5y <- australia_setting_ngms_5y$home / australia_setting_contact_matrices_5y$home
transmission_nonhousehold_5y <- australia_nonhousehold_ngm_5y / australia_nonhousehold_contacts_5y

setting_transmission_probabilities_for_bec <- bind_rows(
  household = matrix_to_predictions(transmission_household_5y),
  nonhousehold = matrix_to_predictions(transmission_nonhousehold_5y),
  .id = "setting"
) %>%
  rename(
    relative_transmission_probability = contacts
  )

write_csv(setting_transmission_probabilities_for_bec,
          file = "outputs/setting_transmission_probabilities_for_bec.csv")
  
nt_first_nations_contact_matrices <- readRDS("nt_first_nations_contact_matrices_updated.RDS")
dir.create("outputs/bec_contact_matrices", showWarnings = FALSE)
for (population in names(nt_first_nations_contact_matrices)) {
  
  pop_settings <- nt_first_nations_contact_matrices[[population]]
  
  for(setting in names(pop_settings)) {
    
    matrix <- pop_settings[[setting]]
    
    matrix %>%
      as_tibble() %>%
      mutate(
        age_group_from = rownames(matrix),
        .before = everything()
      ) %>%
      write_csv(
        file = sprintf(
          "outputs/bec_contact_matrices/%s_%s_contact_matrix.csv",
          population, setting
        )
      )
  }
}



# output contact matrices for some NSW LGAs for Nic R

# get population data for all NSW LGAs
nsw_lga_pops <- abs_lga_lookup %>%
  filter(
    state == "NSW"
  ) %>%
  rowwise() %>%
  mutate(
    population = list(
      abs_age_lga(lga)
    )
  ) %>%
  select(
    state,
    lga,
    population
  )

# nsw_lga_pops$lga[!nsw_lga_pops$lga %in%nsw_lga_household_sizes$lga]
# sort(unique(nsw_lga_pops$lga))
# 
# unique(nsw_lga_household_sizes$lga)

# "Botany Bay (C)" to "Bayside (A)"
# "Gundagai (A)" to "Cootamundra-Gundagai Regional (A)"
# "Nambucca (A)" to "Nambucca Valley (A)"  
# "Western Plains Regional (A)" to "Dubbo Regional (A)"
# get household data for all LGAs using the outdated LGA definition
nsw_lga_household_sizes <- abs_household_lga %>%
  filter(
    state == "NSW",
    n_persons_usually_resident != "total"
  ) %>%
  mutate(
    size = readr::parse_number(n_persons_usually_resident), 
    n_people = n_households * size
  ) %>%
  group_by(
    lga
  ) %>%
  mutate(
    fraction = n_people/sum(n_people)
  ) %>%
  summarise(
    per_capita_household_size = sum(size * fraction),
    .groups = "drop"
  ) %>%
  # these were seemingly computed using an interim set of LGAs, not
  # corresponding to the ASGS LGA 2014 or LGA 2016 (there is no 2015), so
  # relabel 4 LGAs
  mutate(
    lga = case_when(
      lga == "Botany Bay (C)" ~ "Bayside (A)",
      lga == "Gundagai (A)" ~ "Cootamundra-Gundagai Regional (A)",
      lga == "Nambucca (A)" ~ "Nambucca Valley (A)",
      lga == "Western Plains Regional (A)" ~ "Dubbo Regional (A)",
      TRUE ~ lga
    )
  )

# LGAs Nic R is considering
lgas_keep <- read_csv(
  "data/nic_r_nsw_lgas.csv",
  col_types = cols(
    LGA_CODE_2019 = col_double(),
    LGA_NAME_2019 = col_character(),
    LGA = col_double()
  )
)

# all integer ages up to 99, then 99 and above
age_breaks_1y <- c(0:89, Inf)

nsw_contact_matrices <- nsw_lga_pops %>%
  left_join(
    nsw_lga_household_sizes,
    by = "lga"
  ) %>%
  filter(
    lga %in% lgas_keep$LGA_NAME_2019
  ) %>%
  mutate(
    setting_contact_matrices = list(
      predict_setting_contacts(
        population = population,
        contact_model = setting_models,
        per_capita_household_size = per_capita_household_size,
        age_breaks = age_breaks_1y
      )
    )
  )
    
# unpack these, turn them into a big dataframe, and save to disk
nsw_setting_contacts <- nsw_contact_matrices %>%
  select(
    lga,
    setting_contact_matrices
  ) %>%
  unnest(
    cols = c(setting_contact_matrices)
  ) %>%
  mutate(
    setting = names(setting_contact_matrices),
    .after = lga
  ) %>%
  filter(
    setting != "all"
  ) %>%
  mutate(
    setting_contact_matrices = lapply(
      setting_contact_matrices,
      matrix_to_predictions
    )
  ) %>%
  unnest(
    cols = c(setting_contact_matrices)
  )


write_csv(
  x = nsw_setting_contacts,
  file = "outputs/nsw_setting_contacts_for_nic_r.csv"
)

# get setting-specific contact matrices for the same age breaks, convert into a
# dataframe, and save
setting_tranmission_list_1y <- get_setting_transmission_matrices(
  age_breaks = age_breaks_1y
) %>%
  lapply(
    matrix_to_predictions
  )

setting_transmission_1y <- do.call(
    bind_rows,
    args = c(
      setting_tranmission_list_1y,
      list(.id = "setting")
    )
  ) %>%
  rename(
    relative_probability = contacts
  )

write_csv(
  x = setting_transmission_1y,
  file = "outputs/setting_transmission_probabilities_for_nic_r.csv"
)

nsw_setting_contacts %>%
  filter(
    setting == "home"
  ) %>%
  group_by(
    lga,
    age_group_from
  ) %>%
  summarise(
    `household contacts` = sum(contacts),
    .groups = "drop"
  ) %>%
  group_by(
    lga,
  ) %>%
  summarise(
    `mean household contacts` = mean(`household contacts`),
    .groups = "drop"
  ) %>%
  arrange(
    `mean household contacts`
  ) %>%
  mutate(
    lga = factor(
      lga,
      levels = unique(lga)
    )
  ) %>%
  ggplot(
    aes(
      x = lga,
      y = `mean household contacts`
    )
  ) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_minimal()

