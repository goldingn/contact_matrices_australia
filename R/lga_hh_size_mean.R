#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author geryan
#' @export
lga_hh_size_mean <- function() {

  abs_household_lga %>% 
    filter(n_persons_usually_resident != "total") %>%
    mutate(
      n_persons_usually_resident = ifelse(
        n_persons_usually_resident == "8+",
        "8",
        n_persons_usually_resident
      ) %>%
        as.numeric,
      n_residents = n_persons_usually_resident * n_households,
      hh_size_weighting = n_residents * n_persons_usually_resident
    ) %>%
    group_by(state, lga) %>%
    summarise(
      mean_hh_size = sum(hh_size_weighting) / sum(n_residents),
      mean_hh_size = ifelse(
        is.nan(mean_hh_size),
        0,
        mean_hh_size
      )
    ) %>%
    ungroup

}
