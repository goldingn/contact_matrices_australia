library(png)
library(tidyverse)

matrix <- png::readPNG("data/eyre_household_attack_rate_raw.png")[, , 1:3]
legend <- png::readPNG("data/eyre_legend_raw.png")[, , 1:3]

# convert matrix into tibble with age labels
n_contact_pixels <- dim(matrix)[1]
n_case_pixels <- dim(matrix)[2]
min_age <- 4.5
max_age <- 70.5
case_ages <- seq(min_age, max_age, length.out = n_case_pixels)
contact_ages <- seq(min_age, max_age, length.out = n_contact_pixels)
matrix_vals <- matrix %>%
  `dim<-`(c(n_contact_pixels * n_case_pixels, 3)) %>%
  as_tibble() %>%
  mutate(
    case_age = rep(case_ages, each = n_contact_pixels),
    contact_age = rep(rev(contact_ages), n_case_pixels),
  )

# convert legend into 3 channel lookup
legend_vals <- legend[75, , ] %>%
  as_tibble() %>%
  mutate(
    probability = seq(0.16, 0.8, length.out = n())
  )

# convert Euclidean lookup from matrix pixels to legends
library(fields)
d <- rdist(
  matrix_vals[, 1:3],
  legend_vals[, 1:3]
)
lookup <- apply(d, 1, which.min)

matrix_vals_prob <- matrix_vals %>%
  mutate(
    probability = legend_vals$probability[lookup]
  )

matrix_vals_prob %>%
  ggplot(
    aes(
      x = case_age,
      y = contact_age,
      fill = probability
    )
  ) +
  geom_tile() +
  scale_fill_viridis() +
  theme_bw()


# aggregate to 1y resolution
matrix_vals_prob_1y <- matrix_vals_prob %>%
  mutate(
    across(
      ends_with("age"),
      round
    )
  ) %>%
  group_by(
    case_age, contact_age
  ) %>%
  summarise(
    across(
      probability,
      mean
    ),
    .groups = "drop"
  )

# need extrapolate to all ages
matrix_vals_prob_1y_all <- matrix_vals_prob_1y %>%
  complete(
    case_age = 0:100,
    contact_age = 0:100,
    fill = list(probability = NA)
  )

d <- rdist(
  matrix_vals_prob_1y_all %>%
    select(
      ends_with("age")
    ),
  matrix_vals_prob_1y %>%
    select(
      ends_with("age")
    )
)

lookup <- apply(d, 1, which.min)

matrix_vals_prob_1y_all <- matrix_vals_prob_1y_all %>%
  mutate(
    probability = coalesce(probability, matrix_vals_prob_1y$probability[lookup])
  )

matrix_vals_prob_1y_all %>%
  ggplot(
    aes(
      x = case_age,
      y = contact_age,
      fill = probability
    )
  ) +
  geom_tile() +
  scale_fill_viridis() +
  theme_bw()

age_group_lookup <- tibble(
  age = 0:100
) %>%
  mutate(
    lower = pmin(5 * (age %/% 5), 80),
    upper = paste0("-", lower + 4),
    upper = if_else(upper == "-84", "+", upper),
    age_5y = paste0(lower, upper),
  ) %>%
  select(
    age,
    age_5y
  )
  
matrix_vals_prob_with_5y <- matrix_vals_prob_1y_all %>%
  left_join(
    age_group_lookup,
    by = c(case_age = "age")
  ) %>%
  rename(
    case_age_5y = age_5y
  ) %>%
  left_join(
    age_group_lookup,
    by = c(contact_age = "age")
  ) %>%
  rename(
    contact_age_5y = age_5y
  ) %>%
  relocate(
    ends_with("5y"),
    .before = "probability"
  ) %>%
  mutate(
    across(
      ends_with("age_5y"),
      ~ factor(.x,
               levels = str_sort(
                 unique(.x),
                 numeric = TRUE
               )
      )
    )
  ) %>%
  arrange(case_age, case_age_5y,
          contact_age, contact_age_5y)

write_csv(
  matrix_vals_prob_with_5y,
  file = "outputs/eyre_household_attack_rate.csv"
)
  
matrix_vals_prob_5y <- matrix_vals_prob_with_5y %>%
  group_by(
    case_age_5y, contact_age_5y
  ) %>%
  summarise(
    probability = mean(probability),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      ends_with("age_5y"),
      ~ factor(.x,
               levels = str_sort(
                 unique(.x),
                 numeric = TRUE
               )
      )
    )
  ) %>%
  arrange(case_age_5y, contact_age_5y)

matrix_vals_prob_5y %>%
  ggplot(
    aes(
      x = case_age_5y,
      y = contact_age_5y,
      fill = probability
    )
  ) +
  geom_tile() +
  scale_fill_viridis() +
  theme_bw() +
  theme(
    axis.text = element_text(angle = 45, hjust = 1)
  )
