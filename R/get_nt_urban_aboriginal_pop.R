#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_nt_urban_aboriginal_pop <- function() {

  # aboriginal population in urban alice springs
  population <- get_nt_lhd_aboriginal_pop() %>%
    filter(
      district == "Alice Springs Urban"
    ) %>%
    group_by(
      lower.age.limit
    ) %>%
    summarise(
      population = sum(population)
    )
  
}
