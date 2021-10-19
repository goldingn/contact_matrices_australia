#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_australia_ngm_unscaled <- function(model, age_breaks) {

  # unscaled next generation amtrix for all Australia
  
  transmission_matrices <- get_setting_transmission_matrices(
    age_breaks = age_breaks
  )
  
  abs_pop_age_lga_2020 %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    mutate(
      country = "Australia"
    ) %>%
    nest(
      population = -country
    ) %>%
    rowwise() %>%
    mutate(
      per_capita_household_size = get_per_capita_household_size(),
      setting_matrices = list(
        predict_setting_contacts(
          contact_model = model,
          population = population,
          per_capita_household_size = per_capita_household_size,
          age_breaks = age_breaks
        )
      ),
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
    pull(ngm_unscaled) %>%
    `[[`(1)
  
  
}
