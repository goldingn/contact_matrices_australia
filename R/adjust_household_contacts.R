#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
adjust_household_contacts <- function(matrices) {

  # given a named list, where the names are LGAs and each element is a list of 4
  # setting-specific synthetic contact matrices (including 'home'), adjust the
  # number of household contacts to match the average household size in that LGA
  # from ABS
  
  lgas <- names(matrices)
  
  # get ratios between Polymod-predicted and ABS numbers of household contacts
  ratios <- mean_home_contacts(matrices) %>%
    bind_rows(.id = "lga") %>%
    pivot_longer(
      cols = everything(),
      names_to = "lga",
      values_to = "home_contacts"
    ) %>%
    left_join(
      mean_household_sizes(lgas),
      by = "lga"
    ) %>%
    mutate(
      # get the expected average number of homme contacts (take number of other
      # people in the house)
      expected_home_contacts = (mean_household_size - 1),
      ratio = expected_home_contacts / home_contacts
    ) %>%
    select(
      lga, ratio
    ) %>%
    pivot_wider(
      names_from = lga,
      values_from = ratio
    )
  
  settings <- matrices %>%
    lapply(names) %>%
    unlist() %>%
    unique() %>%
    setdiff("all")
  
  for (lga in lgas) {
    # apply correction
    matrices[[lga]]$home <- matrices[[lga]]$home * ratios[[lga]]
    # recompute the 'all' matrices
    matrices[[lga]]$all <- Reduce('+', matrices[[lga]][settings])
  }
  
  matrices

}
