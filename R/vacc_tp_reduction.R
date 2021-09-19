#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vaccination_effect
#' @param next_generation_matrix
#' @return
#' @author Nick Golding
#' @export
vacc_tp_reduction <- function(vaccination_effect, next_generation_matrix) {

  vc_next_gen_matrix <- sweep(
    next_generation_matrix,
    2,
    vaccination_effect,
    FUN = "*"
  )
  
  R0 <- get_R(next_generation_matrix)
  Rv <- get_R(vc_next_gen_matrix)
  overall <- Rv / R0

}
