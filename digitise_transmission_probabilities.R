# digitise setting- and age-specific transmission probability matrices

eyre_transmission_probabilities <- bind_rows(
  household = digitise_eyre_matrix(
    matrix_file = "data/eyre_transmission_household.png"
  ),
  household_visitor = digitise_eyre_matrix(
    matrix_file = "data/eyre_transmission_household_visitor.png"
  ),
  work_education = digitise_eyre_matrix(
    matrix_file = "data/eyre_transmission_work_education.png"
  ),
  events_activities = digitise_eyre_matrix(
    matrix_file = "data/eyre_transmission_events_activities.png"
  ),
  .id = "setting"
)

# save version with 5y lookup
age_lookup <- get_age_group_lookup(
  age_breaks = c(seq(0, 80, by = 5), Inf)
)

eyre_transmission_probabilities_with_5y <- eyre_transmission_probabilities %>%
  left_join(
    rename(
      age_lookup,
      case_age = age,
      case_age_5y = age_group
    ),
    by = "case_age"
  ) %>%
  left_join(
    rename(
      age_lookup,
      contact_age = age,
      contact_age_5y = age_group
    ),
    by = "contact_age"
  ) %>%
  relocate(
    case_age_5y,
    contact_age_5y,
    .before = probability
  )
  
write_csv(
  eyre_transmission_probabilities_with_5y,
  file = "outputs/eyre_setting_transmission_probabilities.csv"
)

# plot this
eyre_transmission_probabilities_with_5y %>%
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
  ) %>%
  mutate(
    across(
      ends_with("age"),
      ~ factor(.x,
               levels = str_sort(
                 unique(.x),
                 numeric = TRUE
               )
      )
    )
  ) %>%
  ggplot(
    aes(
      x = case_age,
      y = contact_age,
      fill = probability
    )
  ) +
  facet_wrap(~setting) +
  geom_tile() +
  scale_fill_viridis() +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text = element_text(angle = 45, hjust = 1)
  )




