#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lgas
#' @return
#' @author Nick Golding
#' @export
mean_household_sizes <- function(lgas) {
  
  # get average household sizes *per person* from ABS - assuming a max of 8
  # people per households. Note - I tried computing the mean size of the
  # households larger than 7, by comparing with LGA populations, but they were
  # improbably enormous, probably because some of the population lives in
  # facilities, not households.

  abs_household_lga %>%
    filter(
      year == max(year),
      lga %in% lgas,
      n_persons_usually_resident != "total"
    ) %>%
    group_by(
      lga
    ) %>%
    mutate(
      # household size as a number, assuming all people in households 8+ are
      # exactly 8
      size = readr::parse_number(n_persons_usually_resident),
      # number of *people* in a household of that size
      n_people = n_households * size,
      # as a fraction of the population
      fraction = n_people / sum(n_people)
    ) %>%
    summarise(
      mean_household_size = sum(size * fraction)
    ) 

}
