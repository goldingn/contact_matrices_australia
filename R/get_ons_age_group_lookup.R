#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_ons_age_group_lookup <- function() {

  tibble(
    age = 0:100
  ) %>%
    mutate(
      age_group = case_when(
        age >= 2 & age < 12 ~ "2-11",
        age >= 12 & age < 16 ~ "12-15",
        age >= 16 & age < 25 ~ "16-24",
        age >= 25 & age < 35 ~ "25-34",
        age >= 35 & age < 50 ~ "35-49",
        age >= 50 & age < 70 ~ "50-69",
        age >= 70 & age < 85 ~ "70-85",
        TRUE ~ NA_character_
      ),
      age_group = factor(
        age_group, 
        levels = str_sort(
          unique(age_group),
          numeric = TRUE
        )
      )
    )
  

}
