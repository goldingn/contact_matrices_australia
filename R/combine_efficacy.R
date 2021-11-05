#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param infection
#' @param transmission
#' @return
#' @author geryan
#' @export
combine_efficacy <- function(
  infection,
  transmission
) {

  1 - ((1 - infection) * (1 - transmission)) 

}
