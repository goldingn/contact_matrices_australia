#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age
#' @return
#' @author geryan
#' @export
australia_pop_fun <- function(age) {

  abs_state_age %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    conmat::get_age_population_function()

}
