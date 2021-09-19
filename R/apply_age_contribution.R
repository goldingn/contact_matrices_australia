#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_matrix
#' @param asymp_rel_infectious
#' @return
#' @author Nick Golding
#' @export
apply_age_contribution <- function(matrix, asymp_rel_infectious = 0.5) {
  
  # get age-based relative susceptibility and infectiousness from Davies et al.
  age_contribution <- get_age_contribution(asymp_rel_infectious = asymp_rel_infectious)
  
  # adjust infectiousness on columns and susceptibility on rows
  matrix <- sweep(matrix, 2, age_contribution$rel_infectiousness, FUN = "*")
  matrix <- sweep(matrix, 1, age_contribution$rel_susceptibility, FUN = "*")
  
  matrix

}
