#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_nt_aboriginal_pop_function <- function(nt_lhd_aboriginal_pop = NULL) {
  
  if (is.null(nt_lhd_aboriginal_pop)) {
    nt_lhd_aboriginal_pop <- get_nt_lhd_aboriginal_pop()
  }

  nt_aboriginal_pop_fun <- nt_lhd_aboriginal_pop %>%
    group_by(
      lower.age.limit
    ) %>%
    summarise(
      across(
        population,
        sum
      )
    ) %>%
    get_age_population_function()

}
