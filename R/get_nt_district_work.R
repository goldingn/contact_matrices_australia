#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_nt_district_work <- function() {

  conmat::abs_employ_age_lga %>%
    filter(
      year == max(year),
      state == "NT",
      age_group != "total"
    ) %>%
    mutate(
      district = case_when(
        lga %in% c("Central Desert (R)", "MacDonnell (R)") ~ "Alice Springs Rural",
        lga == "Alice Springs (T)" ~ "Alice Springs Urban",
        lga == "Barkly (R)" ~ "Barkly",
        lga == "Darwin (C)" ~ "Darwin/Casuarina",
        lga == "East Arnhem (R)" ~ "East Arnhem",
        TRUE ~ NA_character_
      ),
      .before = lga
    ) %>%
    filter(
      !is.na(district)
    ) %>%
    group_by(
      district, age_group
    ) %>%
    summarise(
      across(
        starts_with("total"),
        sum
      ),
      .groups = "drop"
    ) %>%
    # compute the fraction of the population (not the labour force) in employment
    mutate(
      work_fraction = if_else(
        total > 0,
        total_employed / total,
        0
      )
    ) %>%
    select(
      district,
      age_group,
      work_fraction
    ) %>%
    # disaggregate to integer ages
    group_by(
      district
    ) %>%
    group_modify(
      ~ {
        lower_ages <- parse_number(as.character(.x$age_group))
        interp <- spline(
          x = lower_ages + 5,
          y = .x$work_fraction,
          xout = seq(min(lower_ages), max(lower_ages), by = 1)
        )
        tibble(
          age = interp$x,
          work_fraction = pmax(0, interp$y)
        )
      }
    ) %>%
    complete(
      district,
      age = 0:100,
      fill = list(work_fraction = 0)
    )
  
}
