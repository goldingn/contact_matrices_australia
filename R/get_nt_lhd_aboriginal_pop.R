#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_nt_lhd_aboriginal_pop <- function(file = "data/Health District Population.xlsx") {

  # get aboriginal populations in NT health districts
  nt_lhd_pop <- read_excel(file)  %>%
    clean_names() %>%
    mutate(
      lower.age.limit = parse_number(age_group)
    ) %>%
    filter(
      year == max(year),
      indigenous_status == "Aboriginal"
    ) %>%
    group_by(
      lower.age.limit, district
    ) %>%
    summarise(
      across(
        population,
        sum
      ),
      .groups = "drop"
    )
  
}
