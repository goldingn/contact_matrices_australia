#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_nt_remote_aboriginal_pop <- function() {

  # aboriginal population in all remote districts
  population <- get_nt_lhd_aboriginal_pop() %>%
    filter(
      !(district %in% c("Alice Springs Urban", "Darwin/Casuarina"))
    ) %>%
    group_by(
      lower.age.limit
    ) %>%
    summarise(
      population = sum(population)
    )
}
