#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param matrices
#' @return
#' @author Nick Golding
#' @export
mean_home_contacts <- function(matrices) {

  # loop through LGA setting matrices, getting the population average number of
  # contacts in the home
  lgas <- names(matrices)
  contacts <- list()
  for (lga in lgas) {
    contacts[[lga]] <- weighted.mean(
      colSums(matrices[[lga]]$home),
      population_weight(lga)$population
    )
  }
  names(contacts) <- lgas
  
  contacts

}
