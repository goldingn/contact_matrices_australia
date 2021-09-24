# get an state-wide unscaled NGM
states <- unique(conmat::abs_pop_age_lga_2020$state)
state_matrices <- lapply(states, function(s) {
  conmat::abs_pop_age_lga_2020 %>%
    filter(state == s) %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    predict_setting_contacts(
      contact_model = model,
      age_breaks = age_breaks_5y
    ) %>%
    pluck(
      "all"
    ) %>%
    apply_age_contribution()
})

names(state_matrices) <- states

state_ngm <- state_matrices %>%
  lapply(
  `*`,
  m
)

state_tps <- state_ngm %>%
  vapply(
    get_R,
    FUN.VALUE = numeric(1)
  )
state_tps <- data.table(state_tp=state_tps, state=names(state_tps))

merged <- tpirsad %>% left_join(conmat::abs_pop_age_lga_2020 %>% select(state, lga) %>% unique(), by='lga') %>%
  left_join(state_tps, by='state') %>% as.data.table()
