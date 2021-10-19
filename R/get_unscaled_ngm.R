#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_matrices
#' @param transmission_matrices
#' @return
#' @author geryan
#' @export
get_unscaled_ngm <- function(contact_matrices, transmission_matrices) {

  next_generation_matrices <- mapply(
    FUN = `*`,
    contact_matrices,
    transmission_matrices,
    SIMPLIFY = FALSE
  )
  
  ngm_overall <- Reduce("+", next_generation_matrices)

}
