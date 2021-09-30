# everything else is in lgas.R now, I wasn't sure where these fitted

lapply(list.files("R/*.R", full.names = T), source)

state_tps <- state_ngm %>%
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


ggplot(merged, aes(x=decile, y=relative_diff)) +
  geom_point() + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  facet_wrap(~state) +
  theme_bw() +
  labs(x="SEIFA Decile", y="Transmission Potential")

cowplot::save_plot(plot=last_plot(),
                   filename="plots/tp_lga_by_state.png")


merged2 <- merged %>% left_join(b[, .(lga=lga_name_2018, is_metro)], by='lga')

labels <- c("Metro", "Non-Metro",
            state_tps$state)
names(labels) <- c("TRUE", "FALSE", state_tps$state)

ggplot(merged2[!is.na(is_metro)], aes(x=decile, y=tp)) +
  geom_point() + 
  geom_hline(aes(yintercept=state_tp), linetype="dashed") + 
  geom_hline(aes(yintercept=3.6), linetype="dotted", colour="red") +
  facet_grid(state~is_metro, labeller = as_labeller(labels)) +
  theme_bw() +
  labs(x="SEIFA Decile", y="Transmission Potential") +
  geom_text_repel(data=merged2[lga %in% lgas_care_about,], aes(label=lga)) +
  geom_point(data=merged2[lga %in% lgas_care_about,], colour="red")
