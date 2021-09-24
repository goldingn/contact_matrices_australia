#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param setting_matrices
#' @param household_size
#' @return
#' @author Nick Golding
#' @export
adjust_household_contact_matrix <- function(setting_matrices,
                                            household_size,
                                            population,
                                            household_contact_rate = 1) {
    
  # given a list of 4 setting-specific synthetic contact matrices (including
  # 'home'), and a mean household size, adjust the number of household
  # contacts to match the average household size in that LGA from ABS
  
  # get population weights on each column of the matrix
  max_age <- setting_matrices %>%
    pluck("all") %>%
    colnames() %>%
    str_remove("[0-9]{2}\\)$") %>%
    readr::parse_number() %>%
    max()
  population_weight <- population %>%
    mutate(
      lower = pmin(lower.age.limit, max_age)
    ) %>%
    group_by(lower) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      weight = population / sum(population)
    ) %>%
    arrange(lower) %>%
    pull(weight)
  
  # get ratio between household matrix average household contacts and that
  # expected from the average household size
  expected_home_contacts <- household_contact_rate * (household_size - 1)
  matrix_home_contacts <- weighted.mean(
    colSums(setting_matrices$home),
    population_weight
  )
  ratio <- expected_home_contacts / matrix_home_contacts

  # adjust home matrix and recompute all matrix
  settings <- setdiff(names(setting_matrices), "all")
  setting_matrices$home <- setting_matrices$home * ratio
  setting_matrices$all <- Reduce('+', setting_matrices[settings])

  setting_matrices

}
