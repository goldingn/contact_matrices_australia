# everything else is in lgas.R now, I wasn't sure where these fitted

state_tps <- state_ngms %>%
  vapply(
    get_R,
    FUN.VALUE = numeric(1)
  )

state_tps <- tibble(
  state_tp = state_tps,
  state = names(state_tps)
)

merged <- tpirsad %>% left_join(conmat::abs_pop_age_lga_2020 %>% select(state, lga) %>% unique(), by='lga') %>%
  left_join(state_tps, by='state') %>% as.data.table()
