#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_england_vaccination_coverage <- function() {
  
  # get vaccination coverage by age in England as at the date before vaccination
  # of 12-15 year olds
  
  # load england population by single year in mid 2020
  england_pop <- get_england_population()
  
  # lookup between integer ages and available age groups
  age_lookup <- get_age_group_lookup(
    age_breaks = c(0, 16, 18, seq(25, 80, by = 5), Inf)
  )
  
  # load england vaccination coverage
  england_vaccination_raw <- read_excel(
    "data/COVID-19-daily-announced-vaccinations-19-September-2021.xlsx",
    sheet = "Vaccinations by LTLA and Age ",
    skip = 12,
    n_max = 2
  )
  
  col_names <- bind_cols(
    top = colnames(england_vaccination_raw),
    bottom = as.character(england_vaccination_raw[1, ])
  ) %>%
    mutate(
      top = case_when(
        grepl("^\\.\\.\\.", top) ~ NA_character_,
        top == "1st dose6,7,8" ~ "dose 1",
        top == "2nd dose6,7,8" ~ "dose 2",
        TRUE ~ top
      ),
      bottom = case_when(
        bottom == "Under 18" ~ "16-17",
        bottom == "NA" ~ NA_character_,
        TRUE ~ bottom
      )
    ) %>%
    fill(
      top
    ) %>%
    mutate(
      top = case_when(
        is.na(bottom) ~ NA_character_,
        TRUE ~ top
      ),
      across(
        c(top, bottom),
        ~replace_na(.x, "")
      ),
      combined = paste(top, bottom, sep = ":"),
      combined = str_replace(combined, " ", "_")
    )
  
  england_vaccination <- england_vaccination_raw %>%
    filter(
      row_number() > 1
    ) %>%
    set_names(
      col_names$combined
    ) %>%
    select(-`:`) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("dose", "age"),
      names_sep = ":",
      values_to = "count"
    ) %>%
    mutate(
      count = parse_integer(count)
    )
  
  # get age group coverages
  england_vaccination_coverage <- england_pop %>%
    left_join(
      age_lookup,
      by = "age"
    ) %>%
    filter(
      !is.na(age_group)
    ) %>%
    group_by(
      age_group
    ) %>%
    summarise(
      across(
        population,
        sum
      )
    ) %>%
    right_join(
      england_vaccination,
      by = c("age_group" = "age")
    ) %>%
    mutate(
      coverage = count / population
    ) %>%
    select(
      -population,
      -count
    ) %>%
    pivot_wider(
      names_from = dose,
      values_from = coverage
    )
  
  # assign percentage coverages to single ages
  england_vaccination_coverage_1y <- age_lookup %>%
    left_join(
      england_vaccination_coverage,
      by = "age_group"
    ) %>%
    mutate(
      across(
        starts_with("dose"),
        ~replace_na(.x, 0)
      )
    ) %>%
    select(
      -age_group
    )
  
  england_vaccination_coverage_1y
  
}
