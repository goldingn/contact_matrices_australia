#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param R_target
#' @param transition_matrix
#' @param stable_age
#' @return
#' @author Nick Golding
#' @export
find_m <- function(R_target, transition_matrix, stable_age = NULL, max_m = 100) {

  # this function from STEPS
  #https://github.com/steps-dev/steps/blob/74c5359dd4470c4056cd799c53ef56d503ba69da/R/growth_transition_functions-class.R#L266
  #
  # compute the m that calibrates a next generation matrix to R0
  
  obj <- function (m, R_target, transition_matrix, stable_age = NULL) {
    new_transition_matrix <- m*transition_matrix
    R_current <- get_R(new_transition_matrix, stable_age = stable_age)
    (R_current - R_target) ^ 2
  } 
  
  out <- stats::optimise(f = obj,
                         interval = c(0, max_m),
                         R_target,
                         transition_matrix,
                         stable_age)
  out$minimum
  
  return(out$minimum)
}
