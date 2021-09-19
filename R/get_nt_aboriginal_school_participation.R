#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_nt_aboriginal_school_participation <- function() {

  nt_aboriginal_pop_fun <- get_nt_aboriginal_pop_function()
  
  # school participation among aboriginal population of in NT in 2020
  conmat::abs_education_state %>%
    filter(
      year == max(year),
      state == "NT",
      aboriginal_and_torres_strait_islander_status == "Aboriginal and Torres Strait Islander"
    ) %>%
    # join on NT aboriginal population
    select(
      age,
      school_population = n_full_and_part_time
    ) %>%
    complete(
      age = 0:100,
      fill = list(school_population = 0)
    ) %>%
    mutate(
      population = nt_aboriginal_pop_fun(age),
      school_fraction = if_else(
        population > 0,
        school_population / population,
        0
      )
    ) %>%
    select(
      age,
      school_fraction
    )

}
