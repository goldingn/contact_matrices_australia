#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_england_infections <- function() {

  # get a distribution of infections in England by age group use broad ONS
  # categories instead of single ages since the smoothing may hide kinks which would mess up our analysis
  
  # data from October 1 report corresponds to September 22
  england_prevalence <- read_excel(
    "data/covid19infectionsurveydatasets20211001england.xlsx",
    sheet = "1g",
    skip = 4,
    n_max = 7
  ) %>%
    mutate(
      age_group = case_when(
        `Grouped age` == "Age 2 to School Year 6" ~ "2-11",
        `Grouped age` == "School Year 7 to School Year 11" ~ "12-15",
        `Grouped age` == "School Year 12 to Age 24" ~ "16-24",
        `Grouped age` == "Age 25 to Age 34" ~ "25-34",
        `Grouped age` == "Age 35 to Age 49" ~ "35-49",
        `Grouped age` == "Age 50 to Age 69" ~ "50-69",
        `Grouped age` == "Age 70+" ~ "70-85"
      ),
      age_group = factor(
        age_group, 
        levels = unique(age_group)
      ),
      prevalence = `Modelled % testing positive for COVID-19` / 100
    ) %>%
    select(
      age_group,
      prevalence
    )
  
  # convert to infections
  england_infections <- get_ons_age_group_lookup() %>%
    filter(
      !is.na(age_group)
    ) %>%
    left_join(
      get_england_population(),
      by = "age"
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
    left_join(
      england_prevalence,
      by = "age_group"
    ) %>%
    mutate(
      infections = population * prevalence
    ) 
  
  england_infections
  
}
