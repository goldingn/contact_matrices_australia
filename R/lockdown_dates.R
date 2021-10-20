#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author geryan
#' @export
lockdown_dates <- function() {

  dates <- seq.Date(
    from = as.Date("2020-04-05"),
    to   = as.Date("2021-10-17"),
    by = 1
  )
  
  vic <- tribble(
    ~state,       ~date, ~lockdown,
    "VIC", "2020-04-05",         1,
    "VIC", "2020-05-13",         0,
    "VIC", "2020-07-08",         1,
    "VIC", "2020-10-26",         0,
    "VIC", "2021-02-13",         1,
    "VIC", "2021-02-18",         0,
    "VIC", "2021-05-28",         1,
    "VIC", "2021-06-11",         0,
    "VIC", "2021-07-16",         1,
    "VIC", "2021-07-28",         0,
    "VIC", "2021-08-06",         1,
  ) %>%
    mutate(date = as.Date(date))
  
  nsw <- tribble(
    ~state,       ~date, ~lockdown,
    "NSW", "2020-04-05",         1,
    "NSW", "2020-05-15",         0,
    "NSW", "2021-06-25",         1,
    "NSW", "2021-10-11",         0
  ) %>%
    mutate(date = as.Date(date))
  
  bind_rows(
    full_join(
      vic,
      tibble(date = dates)
    ) %>%
      arrange(date) %>%
      fill(everything(), .direction = "down"),
    full_join(
      nsw,
      tibble(date = dates)
    ) %>%
      arrange(date) %>%
      fill(everything(), .direction = "down")
  )
  

}
