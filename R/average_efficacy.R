#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param efficacy_az_1_dose
#' @param efficacy_az_2_dose
#' @param efficacy_pf_1_dose
#' @param efficacy_pf_2_dose
#' @param proportion_az_1_dose
#' @param proportion_az_2_dose
#' @param proportion_pf_1_dose
#' @param proportion_pf_2_dose
#' @return
#' @author geryan
#' @export
average_efficacy <- function(
  efficacy_az_1_dose = combine_efficacy(0.46, 0.02),
  efficacy_az_2_dose = combine_efficacy(0.67, 0.36),
  efficacy_pf_1_dose = combine_efficacy(0.57, 0.13),
  efficacy_pf_2_dose = combine_efficacy(0.8, 0.65),
  proportion_az_1_dose,
  proportion_az_2_dose,
  proportion_pf_1_dose,
  proportion_pf_2_dose
) {

  
  efficacy_mean <- proportion_pf_2_dose * efficacy_pf_2_dose +
    proportion_pf_1_dose * efficacy_pf_1_dose +
    proportion_az_2_dose * efficacy_az_2_dose +
    proportion_az_1_dose * efficacy_az_1_dose 
  
  efficacy_mean

}
