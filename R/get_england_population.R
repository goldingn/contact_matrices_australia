#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_england_population <- function() {
  
  # return the mid-2020 population of England by integer age
  read_excel(
    "data/ukpopestimatesmid2020on2021geography.xls",
    sheet = "MYE2 - Persons",
    skip = 7
  ) %>%
    filter(
      Geography == "Country",
      Name == "ENGLAND"
    ) %>%
    select(
      -Code,
      -Name,
      -Geography,
      -`All ages`
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "age",
      values_to = "population"
    ) %>%
    mutate(
      age = case_when(
        age == "90+" ~"90",
        TRUE ~ age
      ),
      age = as.integer(age)
    )  
}
