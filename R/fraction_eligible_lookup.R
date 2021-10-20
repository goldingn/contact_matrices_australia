#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author geryan
#' @export
fraction_eligible_lookup <- function() {

  # table of fraction of each age class eligible for vaccination
  
  age_limits_5y <- c(seq(0, 80, by = 5), Inf)
  
  australia_pop_fun <- abs_state_age %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    conmat::get_age_population_function()
  
  tibble(
    age = 0:100
  ) %>%
    mutate(
      age_band_5y = cut(age, age_limits_5y, right = FALSE),
      population = australia_pop_fun(age),
      eligible = age >= 12
    ) %>%
    group_by(age_band_5y) %>%
    summarise(
      fraction_eligible = sum(population * eligible) / sum(population),
      .groups = "drop"
    )

}
