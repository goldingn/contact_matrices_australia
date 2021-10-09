#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @param nameme2
#' @param population
#' @return
#' @author Nick Golding
#' @export
mean_attack_rate <- function(transmission_probability_matrix,
                             contact_matrix,
                             population) {

  total_contacts <- colSums(contact_matrix)
  pop_weights <- population / sum(population)
  weights <- sweep(
    contact_matrix,
    2,
    pop_weights / total_contacts,
    FUN = "*"
  )
  sum(transmission_probability_matrix * weights)
  
}
