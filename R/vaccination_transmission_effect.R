#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param age_coverage
#' @param efficacy_mean
#' @param next_generation_matrix
#' @return
#' @author geryan
#' @export
vaccination_transmission_effect <- function(
  age_coverage,
  efficacy_mean,
  next_generation_matrix
) {

  age_transmission_reduction <- 1 - age_coverage * efficacy_mean
  vc_next_gen_matrix <- sweep(
    next_generation_matrix,
    2,
    age_transmission_reduction,
    FUN = "*"
  )
  
  overall <- get_R(vc_next_gen_matrix) / get_R(next_generation_matrix)
  
  list(
    by_age = age_transmission_reduction,
    overall = overall
  )

}
