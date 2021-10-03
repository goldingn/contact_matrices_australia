#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_england_infections <- function() {

  # get a timeseries of infections in England by integer age and date, from ONS prevalence surveys and population estimates
  
  # load england population by single year in mid 2020
  england_pop <- get_england_population()
  
  # load england prevalence estimates by age
  england_prevalence <- read_excel(
    "data/covid19infectionsurveydatasets20211001england.xlsx",
    sheet = "1i",
    skip = 4,
  ) %>%
    filter(
      row_number() > 1
    ) %>%
    mutate(
      Date = excel_numeric_to_date(as.numeric(Date))
    ) %>%
    filter(
      !is.na(Date)
    ) %>%
    select(
      Date,
      starts_with("Age")
    ) %>%
    pivot_longer(
      cols = starts_with("Age"),
      names_to = "age",
      values_to = "prevalence",
      names_prefix = "Age "
    ) %>%
    mutate(
      prevalence = readr::parse_double(prevalence) / 100,
      age = readr::parse_integer(age)
    ) %>%
    rename(
      date = Date
    )
  
  england_infections <- england_prevalence %>%
    left_join(
      england_pop,
      by = "age"
    ) %>%
    mutate(
      infections = population * prevalence
    ) %>%
    # filter to between end of schooll holidays and start of vaccination of
    # 12-15s
    filter(
      date > as.Date("2021-09-01"),
      date < as.Date("2021-09-20")
    )
  
  england_infections
  
}
