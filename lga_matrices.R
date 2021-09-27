source("packages.R")

age_limits_5y <- c(seq(0, 80, by = 5), Inf)

test_lgas <- c(
  # NSW
  "Blacktown (C)",
  "Canterbury-Bankstown (A)",
  "Liverpool (C)",
  "Sydney (C)",
  "Waverley (A)",
  "Walgett (A)",
  # VIC
  "Brimbank (C)",
  "Greater Dandenong (C)",
  "Greater Shepparton (C)",
  "Melbourne (C)",
  "Stonnington (C)",
  "Whittlesea (C)"
)

# get the naive matrices based on population sizes
lga_matrices_naive <- abs_lga_lookup %>%
  # temporary to get subset of LGAs for testing
  filter(state == "NSW" | state == "VIC") %>%
  # group_by(state) %>%
  # filter(row_number() <= 3) %>%
  # ungroup %>%
  # filter(lga %in% test_lgas) %>%
  #
  mutate(
    pop_mat = map(
      .x = lga,
      .f = function(x){
        abs_age_lga(lga_name = x)
        # weird that can't map this function directy to LGA
      }
    ),
    naive_matrix = map(
      .x = pop_mat,
      .f = extrapolate_polymod,
      age_breaks = age_limits_5y
    )
  ) %>%
  left_join(
    y = lga_hh_size_mean(),
    by = c("state", "lga")
  )

saveRDS(
  object = lga_matrices_naive,
  file = "outputs/lga_matrices_naive.Rds"
)

state_cols <- c("cornflowerblue", "darkblue")

# plot distribution of population by age class
lga_matrices_naive %>%
  dplyr::select(state, pop_mat) %>%
  unnest(pop_mat) %>%
  arrange(state) %>%
  ggplot(
    aes(
      x = lower.age.limit,
      y = population,
      fill = state
    )
  ) +
  geom_col() +
  facet_wrap(
    ~ lga,
    scales = "free_y",
  ) +
  theme_minimal() +
  scale_fill_manual(values = state_cols)

# lga_hh_size_mean() calculates the average household size in each LGA
# plot histogram of mean household size in lga by state
ggplot(
  data  = lga_hh_size_mean() %>%
    filter(state == "VIC" | state == "NSW")
) +
  geom_histogram(
    aes(
      mean_hh_size,
      fill = state
    ),
    binwidth = 0.1
  ) +
  facet_wrap(
    ~ state,
    scales = "free_y",
    ncol = 1
  ) +
  scale_fill_manual(values = state_cols)



# get corrected household contact matrix by mean
corrected_hh_matrices <- lga_matrices_naive %>%
  filter%$%
  mapply(
    FUN = correct_hh_matrix_by_mean,
    pop_mat = pop_mat,
    naive_matrix = naive_matrix,
    mean_hh_size = mean_hh_size,
    MoreArgs = list(
      age_limits = age_limits_5y
    ),
    SIMPLIFY = FALSE
  )


# update contact matrices with corrected household
contact_matrices <- lga_matrices_naive %>%
  mutate(
    home_corrected = corrected_hh_matrices,
    contact_matrices = map2(
      .x = naive_matrix,
      .y = home_corrected,
      .f = function(x, y){
        z <- x
        
        z$home <- y
        
        z$all <- with(
          z,
          home + school + work + other
        )
        
        z
      }
    )
  ) %>%
  filter(!is.na(mean_hh_size)) %>%
  dplyr::select(state, lga, contact_matrices)

saveRDS(
  contact_matrices,
  "outputs/lga_contact_matrices.RDS"
)

# convert to a next generation matrix

# construct a contact matrix for all of Australia
australia_contact_matrix <- conmat::abs_pop_age_lga_2020 %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  extrapolate_polymod(
    age_breaks = age_limits_5y
  )

# apply age-based susceptibility and infectiousness from Davies et al.

australia_ngm_unscaled <- apply_age_contribution(australia_contact_matrix$all)

m <- find_m(
  R_target = 3.6,
  transition_matrix = australia_ngm_unscaled
)

vax_scenarios <- readRDS("data/vacc_effect_by_age_scenario_19.RDS") %>%
  group_by(vacc_coverage, vacc_schoolkids) %>%
  nest()

lga_tp_reduction <- contact_matrices %>%
  mutate(
    ngm_unscaled = map(
      .x = contact_matrices,
      .f = function(x){
        apply_age_contribution(x$all)
      }
    ),
    ngm = map(
      .x = ngm_unscaled,
      .f = function(x, m) {
        x * m
      },
      m = m
    )
  ) %>%
  expand_grid(
    vax_scenarios
  ) %>%
  mutate(
    tp_multiplier = map2(
      .x = data,
      .y = ngm,
      .f = function(x, y){
        vacc_tp_reduction(
          x$vacc_effect,
          y
        )
      }
    ) %>%
      unlist,
    tp_percent_reduction = 100 * (1 - tp_multiplier)
  ) %>%
  arrange(
    state,
    lga,
    vacc_schoolkids,
    vacc_coverage
  ) %>%
  mutate(
    starting_tp = map(
      .x = ngm,
      .f = get_R
    ) %>%
      unlist,
    post_vacc_tp = starting_tp * tp_multiplier
  )

saveRDS(
  lga_tp_reduction,
  file = "outputs/lga_tp_reduction.RDS"
)