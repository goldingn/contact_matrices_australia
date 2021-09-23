#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lga
#' @return
#' @author Nick Golding
#' @export
population_weight <- function(lga, max_lower_age_bound = 80) {
  # given an lga name, get the age populations in intervals up to a maximum age
  # group
  abs_age_lga(lga) %>%
    mutate(
      lower.age.limit = pmin(lower.age.limit, max_lower_age_bound)
    ) %>%
    group_by(
      lower.age.limit
    ) %>%
    summarise(
      population = sum(population)
    ) %>%
    arrange(
      lower.age.limit
    )

}
