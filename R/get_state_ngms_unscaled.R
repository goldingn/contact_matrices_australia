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
  transmission_matrices <- get_setting_transmission_matrices(
    age_breaks = age_breaks
  )
  
  tibble(
    state = unique(abs_pop_age_lga_2020$state)
  ) %>%
    rowwise() %>%
    mutate(
      per_capita_household_size = get_per_capita_household_size(
        state = state
      ),
      population = list(
        abs_age_state(state)
      ),
      setting_matrices = list(
        predict_setting_contacts(
          contact_model = model,
          population = abs_age_state(state),
          per_capita_household_size = per_capita_household_size,
          age_breaks = age_breaks
        )
      )
    ) %>%
    mutate(
      contact_matrices = list(
        setting_matrices[c("home", "school", "work", "other")]
      ),
      ngm_unscaled = list(
        all = get_unscaled_ngm(
          contact_matrices = contact_matrices,
          transmission_matrices = transmission_matrices
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
