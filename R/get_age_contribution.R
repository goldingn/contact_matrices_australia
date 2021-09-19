#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param asymp_rel_infectious
#' @return
#' @author Nick Golding
#' @export
get_age_contribution <- function(asymp_rel_infectious = 0.5) {
  
  age_disaggregation <- tibble::tribble(
    ~age_group_10y, ~age_group_5y,
    "0_9", "0-4",
    "0_9", "5-9",
    "10_19", "10-14",
    "10_19", "15-19",
    "20_29", "20-24",
    "20_29", "25-29",
    "30_39", "30-34", 
    "30_39", "35-39",
    "40_49", "40-44",
    "40_49", "45-49",
    "50_59", "50-54",
    "50_59", "55-59",
    "60_69", "60-64",
    "60_69", "65-69",
    "70+", "70-74",
    "70+", "75-79",
    "70+", "80+"
  )
  
  age_data_davies <- read_csv(
    "data/susceptibility_clinical_fraction_age_Davies.csv",
    col_types = cols(
      age_group = col_character(),
      rel_susceptibility_mean = col_double(),
      rel_susceptibility_median = col_double(),
      clinical_fraction_mean = col_double(),
      clinical_fraction_median = col_double()
    )
  )
  
  # calculate relative infectiousness (using relative differences in clinical
  # fraction by age and assumed relative infectiousness of asymptomatics) and
  # susceptibility by age
  age_contribution <- age_disaggregation %>%
    left_join(
      age_data_davies,
      by = c("age_group_10y" = "age_group")
    ) %>%
    mutate(
      rel_infectiousness = clinical_fraction_mean +
        asymp_rel_infectious * (1 - clinical_fraction_mean),
      rel_infectiousness = rel_infectiousness /
        max(rel_infectiousness),
      rel_susceptibility = rel_susceptibility_mean /
        max(rel_susceptibility_mean),
    ) %>%
    select(
      age_group_5y,
      rel_infectiousness,
      rel_susceptibility
    )

}
