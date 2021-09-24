#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_state_ngms_unscaled <- function(model, age_breaks) {

  # return a list of unscaled NGMs for Australian states, accounting for
  # population age distributions and household sizes
  tibble(
    state = unique(abs_pop_age_lga_2020$state)
  ) %>%
    rowwise() %>%
    mutate(
      household_size = get_mean_household_size(
        state = state
      ),
      population = list(
        abs_age_state(state)
      ),
      setting_matrices = list(
        predict_setting_contacts(
          contact_model = model,
          population = abs_age_state(state),
          age_breaks = age_breaks
        )
      )
    ) %>%
    mutate(
      setting_matrices = list(
        adjust_household_contact_matrix(
          setting_matrices = setting_matrices,
          household_size = household_size,
          population = population
        )
      ),
      contact_matrix = list(
        pluck(setting_matrices, "all")
      ),
      ngm_unscaled = list(
        apply_age_contribution(
          contact_matrix
        )
      )
    ) %>%
    select(
      state, ngm_unscaled
    ) %>%
    pivot_wider(
      names_from = state,
      values_from = ngm_unscaled
    ) %>%
    as.list() %>%
    lapply(`[[`, 1)
  
}
