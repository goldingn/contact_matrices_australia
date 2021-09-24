#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param population
#' @param naive_matrix
#' @param mean_hh_size
#' @param age_limits
#' @return
#' @author geryan
#' @export
correct_hh_matrix_by_mean <- function(
  pop_mat,
  naive_matrix,
  mean_hh_size,
  age_limits = c(seq(0, 80, by = 5), Inf)
) {

  age_pop <- pop_mat %>%
    mutate(
      age_group = cut(
        pmax(0.1, lower.age.limit),
        age_limits,
        right = FALSE
      )
    )
  
  age_sizes <- naive_matrix$home %>%
    colSums() + 1
  
  df_age_sizes <- tibble(
    age_group = names(age_sizes),
    hh_size = age_sizes
  )
  
  naive_hh_size <- age_pop %>%
    left_join(
      df_age_sizes,
      by = "age_group"
    ) %>%
    dplyr::select(age_group, population, hh_size) %>%
    mutate(
      hh_size_weighting = population * hh_size
    ) %>%
    summarise(size = sum(hh_size_weighting) / sum(population)) %>%
    pull(size)
  
  hh_correction_factor <- mean_hh_size / naive_hh_size
  
  
  naive_matrix$home * hh_correction_factor

}
